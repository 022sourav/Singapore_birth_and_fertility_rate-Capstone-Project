options(stringsAsFactors = FALSE, warn = 1)
set.seed(20260921)
t_start <- Sys.time()

# ---- 0. Setup ----------------------------------------------------------------
need <- c("forecast", "tseries", "ggplot2", "patchwork", "scales")
ulib <- Sys.getenv("R_LIBS_USER")                       # per-user library (no admin rights needed)
if (nzchar(ulib)) { dir.create(ulib, recursive = TRUE, showWarnings = FALSE); .libPaths(c(ulib, .libPaths())) }
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) install.packages(miss, lib = .libPaths()[1], repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(forecast); library(tseries); library(ggplot2); library(patchwork); library(scales)
})

cmd <- commandArgs(trailingOnly = FALSE)
f   <- sub("^--file=", "", cmd[grep("^--file=", cmd)])
base_dir <- if (length(f)) dirname(normalizePath(f)) else getwd()
data_file <- file.path(base_dir, "..", "dataset", "SARIMA_dataset.csv")
if (!file.exists(data_file)) data_file <- file.path(base_dir, "SARIMA_dataset.csv")
stopifnot(file.exists(data_file))
fig_dir <- file.path(base_dir, "output", "figures")
tab_dir <- file.path(base_dir, "output", "tables")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

# Palette: neutral ink for actual data; validated categorical slots for series.
INK <- "#1a1a1a"; MUTED <- "#6b6b6b"; GRID <- "#e6e6e3"
C_BLUE <- "#2a78d6"; C_ORANGE <- "#eb6834"; C_AQUA <- "#1baf7a"; C_YELLOW <- "#eda100"

theme_rep <- function(base = 10) {
  theme_minimal(base_size = base) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3),
          axis.text = element_text(colour = MUTED), axis.title = element_text(colour = MUTED),
          plot.title = element_text(face = "bold", colour = INK, size = base + 1),
          plot.subtitle = element_text(colour = MUTED),
          legend.position = "top", legend.title = element_blank(),
          strip.text = element_text(face = "bold", colour = INK, hjust = 0),
          plot.background = element_rect(fill = "white", colour = NA))
}
save_fig <- function(p, name, w = 8, h = 4.2) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 200, bg = "white")
}
save_tab <- function(d, name) write.csv(d, file.path(tab_dir, paste0(name, ".csv")), row.names = FALSE)
r2 <- function(x, k = 2) round(x, k)
key <- list(); addkey <- function(k, v) key[[k]] <<- v

# ---- 1. Data -----------------------------------------------------------------
raw <- read.csv(data_file, check.names = FALSE, stringsAsFactors = FALSE)
lbl <- names(raw)[-1]
dates <- as.Date(sprintf("%s-%02d-01", substr(lbl, 1, 4), match(substr(lbl, 5, 7), month.abb)))
ord <- order(dates); dates <- dates[ord]
M <- t(as.matrix(raw[, -1]))[ord, , drop = FALSE]; storage.mode(M) <- "numeric"; rownames(M) <- NULL
# Row order in the file: 5 rows (all sexes), 5 rows (male), 5 rows (female)
colnames(M) <- c("Total", "Malays", "Chinese", "Indians", "Others",
                 "Male", "Male_Malays", "Male_Chinese", "Male_Indians", "Male_Others",
                 "Female", "Female_Malays", "Female_Chinese", "Female_Indians", "Female_Others")
n   <- nrow(M)
yr  <- as.integer(format(dates, "%Y")); mo <- as.integer(format(dates, "%m"))
days_in_month <- function(d) as.integer(format(as.Date(format(d + 32, "%Y-%m-01")) - 1, "%d"))
dim_all <- days_in_month(dates)
dragon_years <- seq(1964, 2048, by = 12)                 # Year of the Dragon
make_X <- function(d) {                                   # calendar & Dragon regressors
  y_ <- as.integer(format(d, "%Y"))
  cbind(dragon = as.numeric(y_ %in% dragon_years),
        post   = as.numeric((y_ - 1) %in% dragon_years),
        ldays  = log(days_in_month(d)))
}
X_all <- make_X(dates)
fut_dates <- seq(as.Date("2025-04-01"), by = "month", length.out = 24)
X_fut <- make_X(fut_dates)

y_all <- ts(M[, "Total"], start = c(1960, 1), frequency = 12)
idx_of <- function(Y, m) which(yr == Y & mo == m)
test_origin <- idx_of(2023, 3)                            # last training month
stopifnot(n - test_origin == 24)

# 1.1 data-quality table
eth <- rowSums(M[, c("Malays", "Chinese", "Indians", "Others")])
sexsum <- M[, "Male"] + M[, "Female"]
dq <- data.frame(
  Check = c("Observations (months)", "First month", "Last month", "Missing values", "Zero counts",
            "Gaps in monthly sequence", "Months where Total != sum of 4 ethnic groups",
            "Largest |Total - sum of ethnic groups| (births)",
            "Months where Total != Male + Female", "Largest |Total - (Male + Female)| (births)",
            "Months where Male != sum of male ethnic rows", "Months where Female != sum of female ethnic rows"),
  Result = c(n, format(min(dates)), format(max(dates)), sum(is.na(M)), sum(M == 0, na.rm = TRUE),
             sum(as.integer(diff(dates)) > 31), sum(M[, "Total"] != eth), max(abs(M[, "Total"] - eth)),
             sum(M[, "Total"] != sexsum), max(abs(M[, "Total"] - sexsum)),
             sum(M[, "Male"] != rowSums(M[, c("Male_Malays", "Male_Chinese", "Male_Indians", "Male_Others")])),
             sum(M[, "Female"] != rowSums(M[, c("Female_Malays", "Female_Chinese", "Female_Indians", "Female_Others")]))))
save_tab(dq, "t01_data_quality")
addkey("n_obs", n); addkey("n_mismatch_eth", sum(M[, "Total"] != eth)); addkey("max_mismatch", max(abs(M[, "Total"] - eth)))

# 1.2 descriptive summary by era
era <- cut(yr, c(1959, 1979, 1999, 2025), labels = c("1960-1979", "1980-1999", "2000-2025"))
ds <- do.call(rbind, lapply(levels(era), function(e) {
  v <- M[era == e, "Total"]; data.frame(Era = e, Months = length(v), Mean = round(mean(v)), Min = min(v), Max = max(v), SD = round(sd(v)))
}))
ds$PeakYear <- sapply(levels(era), function(e) { k <- which(era == e); yr[k][which.max(M[k, "Total"])] })
ann_tot <- tapply(M[, "Total"], yr, sum); ann_n <- tapply(M[, "Total"], yr, length)
ann_complete <- ann_tot[ann_n == 12]
addkey("peak_annual_year", as.integer(names(ann_complete)[which.max(ann_complete)])); addkey("peak_annual_value", max(ann_complete))
addkey("last_complete_year", as.integer(tail(names(ann_complete), 1))); addkey("last_complete_value", tail(ann_complete, 1))
save_tab(ds, "t02_era_summary")

# ---- 2. Exploratory analysis ---------------------------------------------------
# Fig 1: time plot
df1 <- data.frame(date = dates, births = M[, "Total"])
df1$ma12 <- as.numeric(stats::filter(df1$births, rep(1 / 12, 12), sides = 1))
dr <- data.frame(xmin = as.Date(paste0(dragon_years[dragon_years <= 2024], "-01-01")),
                 xmax = as.Date(paste0(dragon_years[dragon_years <= 2024], "-12-31")))
p1 <- ggplot(df1, aes(date)) +
  geom_rect(data = dr, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = C_ORANGE, alpha = 0.16) +
  geom_line(aes(y = births, colour = "Monthly births"), linewidth = 0.3) +
  geom_line(aes(y = ma12, colour = "12-month moving average"), linewidth = 0.9, na.rm = TRUE) +
  scale_colour_manual(values = c("Monthly births" = MUTED, "12-month moving average" = C_BLUE)) +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  labs(title = "Monthly live births in Singapore, 1960-2025", subtitle = "Shaded bands mark Year-of-the-Dragon calendar years", x = NULL, y = "Births per month") +
  theme_rep()
save_fig(p1, "fig01_timeseries")

# Seasonal indices from STL (periodic seasonal) on log scale
seas_idx <- function(v, d, from, to, per_day = TRUE) {
  k <- which(yr >= from & yr <= to)
  x <- log(v[k] / if (per_day) days_in_month(d[k]) else 1)
  s <- stl(ts(x, start = c(yr[k][1], 1), frequency = 12), s.window = "periodic", robust = TRUE)$time.series[, "seasonal"]
  exp(as.numeric(s[1:12]))
}
si_raw <- seas_idx(M[, "Total"], dates, 2000, 2024, per_day = FALSE)
si_day <- seas_idx(M[, "Total"], dates, 2000, 2024, per_day = TRUE)
eras <- list("1960-1979" = c(1960, 1979), "1980-1999" = c(1980, 1999), "2000-2024" = c(2000, 2024))
si_era <- sapply(eras, function(e) seas_idx(M[, "Total"], dates, e[1], e[2], per_day = TRUE))
sidx <- data.frame(Month = month.abb, Raw_index = r2(si_raw, 3), PerDay_index = r2(si_day, 3), round(si_era, 3), check.names = FALSE)
save_tab(sidx, "t03_seasonal_index")
addkey("peak_month", month.abb[which.max(si_day)]); addkey("trough_month", month.abb[which.min(si_day)])
addkey("peak_idx", max(si_day)); addkey("trough_idx", min(si_day))
addkey("raw_trough_idx", min(si_raw)); addkey("raw_peak_idx", max(si_raw)); addkey("raw_trough_month", month.abb[which.min(si_raw)])
addkey("swing_pct_raw", (max(si_raw) / min(si_raw) - 1) * 100); addkey("swing_pct_day", (max(si_day) / min(si_day) - 1) * 100)

d2a <- rbind(data.frame(Month = 1:12, Index = si_raw, Type = "Raw monthly counts"),
             data.frame(Month = 1:12, Index = si_day, Type = "Births per day (calendar-adjusted)"))
p2a <- ggplot(d2a, aes(Month, Index, colour = Type)) + geom_hline(yintercept = 1, colour = GRID) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_colour_manual(values = c(C_BLUE, C_ORANGE)) +
  labs(title = "Seasonal index, 2000-2024", subtitle = "Ratio to the yearly level (STL, log scale)", x = NULL, y = "Seasonal index") + theme_rep(9)
d2b <- do.call(rbind, lapply(names(eras), function(e) data.frame(Month = 1:12, Index = si_era[, e], Era = e)))
p2b <- ggplot(d2b, aes(Month, Index, colour = Era)) + geom_hline(yintercept = 1, colour = GRID) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_colour_manual(values = c(C_BLUE, C_ORANGE, C_AQUA)) +
  labs(title = "Has the seasonal shape changed?", subtitle = "Calendar-adjusted seasonal index by era", x = NULL, y = NULL) + theme_rep(9)
save_fig(p2a + p2b, "fig02_seasonality", 9, 3.8)

# Fig 3: STL decomposition of log births
stl_all <- stl(log(y_all), s.window = "periodic", robust = TRUE)
comp <- as.data.frame(stl_all$time.series); comp$data <- as.numeric(log(y_all)); comp$date <- dates
d3 <- rbind(data.frame(date = comp$date, v = comp$data, part = "Data (log)"),
            data.frame(date = comp$date, v = comp$trend, part = "Trend"),
            data.frame(date = comp$date, v = comp$seasonal, part = "Seasonal"),
            data.frame(date = comp$date, v = comp$remainder, part = "Remainder"))
d3$part <- factor(d3$part, levels = c("Data (log)", "Trend", "Seasonal", "Remainder"))
p3 <- ggplot(d3, aes(date, v)) + geom_line(colour = C_BLUE, linewidth = 0.4) +
  facet_grid(part ~ ., scales = "free_y") +
  labs(title = "STL decomposition of log monthly births", x = NULL, y = NULL) + theme_rep(9)
save_fig(p3, "fig03_stl", 8, 5.6)
vr <- var(comp$remainder); Fs <- max(0, 1 - vr / var(comp$seasonal + comp$remainder)); Ft <- max(0, 1 - vr / var(comp$trend + comp$remainder))
addkey("Fs_total", Fs); addkey("Ft_total", Ft)

# ---- 3. Stationarity & differencing ---------------------------------------------
ly <- log(y_all)
suppressWarnings({
  st_row <- function(lbl, x) {
    k <- kpss.test(x, null = "Level"); a <- adf.test(x)
    data.frame(Series = lbl, KPSS_stat = r2(unname(k$statistic), 3), KPSS_p = k$p.value,
               ADF_stat = r2(unname(a$statistic), 3), ADF_p = a$p.value, ADF_lags = unname(a$parameter))
  }
  stat_tab <- rbind(st_row("log births (level)", ly),
                    st_row("seasonal difference (D=1)", diff(ly, 12)),
                    st_row("first difference (d=1)", diff(ly)),
                    st_row("d=1 and D=1", diff(diff(ly, 12))))
})
stat_tab$nsdiffs_OCSB <- c(nsdiffs(ly, test = "ocsb"), rep(NA, 3))
stat_tab$nsdiffs_seas <- c(nsdiffs(ly, test = "seas"), rep(NA, 3))
stat_tab$ndiffs_KPSS  <- c(ndiffs(ly, test = "kpss"), ndiffs(diff(ly, 12), test = "kpss"), NA, ndiffs(diff(diff(ly, 12)), test = "kpss"))
save_tab(stat_tab, "t04_stationarity")
addkey("nsdiffs_seas", nsdiffs(ly, test = "seas")); addkey("nsdiffs_ocsb", nsdiffs(ly, test = "ocsb"))

d4 <- rbind(data.frame(date = dates, v = as.numeric(ly), p = "Log births"),
            data.frame(date = dates[-(1:12)], v = as.numeric(diff(ly, 12)), p = "Seasonal difference (lag 12)"),
            data.frame(date = dates[-(1:13)], v = as.numeric(diff(diff(ly, 12))), p = "Seasonal + first difference"))
d4$p <- factor(d4$p, levels = unique(d4$p))
p4 <- ggplot(d4, aes(date, v)) + geom_line(colour = C_BLUE, linewidth = 0.35) + facet_grid(p ~ ., scales = "free_y") +
  labs(title = "Differencing removes the seasonal pattern and the trend", x = NULL, y = NULL) + theme_rep(9)
save_fig(p4, "fig04_differencing", 8, 5.2)

dd <- diff(diff(ly, 12))
pa <- ggAcf(dd, lag.max = 48) + labs(title = "ACF of the differenced log series", x = "Lag (months)", y = NULL) + theme_rep(9)
pp <- ggPacf(dd, lag.max = 48) + labs(title = "PACF of the differenced log series", x = "Lag (months)", y = NULL) + theme_rep(9)
save_fig(pa / pp, "fig05_acf_pacf", 8, 5.4)

# ---- 4. Forecaster library -------------------------------------------------------
pack <- function(f, fit = NULL) list(mean = as.numeric(f$mean), lo80 = as.numeric(f$lower[, 1]), hi80 = as.numeric(f$upper[, 1]),
                                     lo95 = as.numeric(f$lower[, 2]), hi95 = as.numeric(f$upper[, 2]), fit = fit)
fc_sarima <- function(order, sorder, xcols = NULL, lambda = 0, drift = FALSE) {
  force(order); force(sorder); force(xcols); force(lambda); force(drift)
  function(y, Xtr, Xf, h) {
    a <- list(y = y, order = order, seasonal = list(order = sorder, period = 12), lambda = lambda, include.drift = drift)
    if (!is.null(xcols)) a$xreg <- Xtr[, xcols, drop = FALSE]
    fit <- do.call(Arima, a)
    f <- if (is.null(xcols)) forecast(fit, h = h, level = c(80, 95)) else forecast(fit, h = h, xreg = Xf[, xcols, drop = FALSE], level = c(80, 95))
    pack(f, fit)
  }
}
fc_auto      <- function(y, Xtr, Xf, h) { fit <- auto.arima(y, lambda = 0); pack(forecast(fit, h = h, level = c(80, 95)), fit) }
fc_auto_nons <- function(y, Xtr, Xf, h) { fit <- auto.arima(y, seasonal = FALSE, lambda = 0); pack(forecast(fit, h = h, level = c(80, 95)), fit) }
fc_ets       <- function(y, Xtr, Xf, h) { fit <- ets(y, lambda = 0); pack(forecast(fit, h = h, level = c(80, 95)), fit) }
fc_snaive    <- function(y, Xtr, Xf, h) pack(snaive(y, h = h, level = c(80, 95)))

# Rolling-origin / hold-out engine: returns one row per (origin, horizon)
run_cv <- function(v, fc, origins, h, start_year = 1960, Xmat = X_all) {
  i0 <- which(yr == start_year)[1]
  rows <- lapply(origins, function(o) {
    tr <- i0:o; fu <- (o + 1):(o + h)
    ytr <- ts(as.numeric(v[tr]), start = c(yr[i0], mo[i0]), frequency = 12)
    res <- tryCatch(fc(ytr, Xmat[tr, , drop = FALSE], Xmat[fu, , drop = FALSE], h), error = function(e) { message("  fit failed at origin ", dates[o], ": ", conditionMessage(e)); NULL })
    if (is.null(res)) return(NULL)
    data.frame(origin = dates[o], h = seq_len(h), date = dates[fu], actual = as.numeric(v[fu]), fc = res$mean,
               lo80 = res$lo80, hi80 = res$hi80, lo95 = res$lo95, hi95 = res$hi95, scale = mean(abs(diff(ytr, 12))))
  })
  do.call(rbind, rows)
}
metrics <- function(d) {
  e <- d$actual - d$fc
  data.frame(RMSE = sqrt(mean(e^2)), MAE = mean(abs(e)), MAPE = mean(abs(e) / d$actual) * 100, MASE = mean(abs(e) / d$scale),
             ME = mean(e), Cov80 = mean(d$actual >= d$lo80 & d$actual <= d$hi80) * 100,
             Cov95 = mean(d$actual >= d$lo95 & d$actual <= d$hi95) * 100, n = nrow(d))
}
ord_str <- function(fit) { a <- fit$arma; sprintf("SARIMA(%d,%d,%d)(%d,%d,%d)[%d]", a[1], a[6], a[2], a[3], a[7], a[4], a[5]) }

# ---- 5. Candidate models & rolling-origin cross-validation ------------------------
spec <- function(name, fc, family, start = 1960) list(name = name, fc = fc, family = family, start = start)
stage1 <- list(
  spec("SARIMA(0,1,1)(0,1,1) log",             fc_sarima(c(0, 1, 1), c(0, 1, 1)), "SARIMA"),
  spec("SARIMA(1,1,1)(0,1,1) log",             fc_sarima(c(1, 1, 1), c(0, 1, 1)), "SARIMA"),
  spec("SARIMA(0,1,2)(0,1,1) log",             fc_sarima(c(0, 1, 2), c(0, 1, 1)), "SARIMA"),
  spec("SARIMA(1,1,0)(0,1,1) log",             fc_sarima(c(1, 1, 0), c(0, 1, 1)), "SARIMA"),
  spec("SARIMA(2,1,1)(0,1,1) log",             fc_sarima(c(2, 1, 1), c(0, 1, 1)), "SARIMA"),
  spec("SARIMA(0,1,1)(1,1,1) log",             fc_sarima(c(0, 1, 1), c(1, 1, 1)), "SARIMA"),
  spec("SARIMA(0,1,1)(0,1,2) log",             fc_sarima(c(0, 1, 1), c(0, 1, 2)), "SARIMA"),
  spec("SARIMA(1,0,0)(0,1,1) + drift log",     fc_sarima(c(1, 0, 0), c(0, 1, 1), drift = TRUE), "SARIMA"),
  spec("SARIMA(1,0,1)(0,1,1) + drift log",     fc_sarima(c(1, 0, 1), c(0, 1, 1), drift = TRUE), "SARIMA"),
  spec("SARIMA(0,1,1)(0,1,1) raw scale",       fc_sarima(c(0, 1, 1), c(0, 1, 1), lambda = NULL), "SARIMA"),
  spec("SARIMA(0,1,1)(0,1,1) + days",          fc_sarima(c(0, 1, 1), c(0, 1, 1), xcols = "ldays"), "SARIMA+X"),
  spec("SARIMA(0,1,1)(0,1,1) + days + Dragon", fc_sarima(c(0, 1, 1), c(0, 1, 1), xcols = c("ldays", "dragon", "post")), "SARIMA+X"),
  spec("auto.arima (log)",                     fc_auto, "SARIMA"),
  spec("Seasonal naive",                       fc_snaive, "Benchmark"),
  spec("ETS (log)",                            fc_ets, "Benchmark"),
  spec("Non-seasonal ARIMA (log)",             fc_auto_nons, "Benchmark"))
cv_origins <- sapply(2010:2022, function(Y) idx_of(Y, 3))
cat("\n[CV stage 1] ", length(stage1), " models x ", length(cv_origins), " origins (h = 12)\n", sep = "")
run_specs <- function(specs, origins, h) {
  out <- lapply(specs, function(s) {
    t0 <- Sys.time(); d <- run_cv(as.numeric(y_all), s$fc, origins, h, s$start)
    cat(sprintf("  %-46s start %d  %5.1fs\n", s$name, s$start, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    if (is.null(d)) return(NULL)
    d$model <- s$name; d$family <- s$family; d$start <- s$start; d
  })
  do.call(rbind, out)
}
cv1 <- run_specs(stage1, cv_origins, 12)

# Stage 2: training-window sensitivity for the two best SARIMA-family structures
cv_tab1 <- do.call(rbind, lapply(split(cv1, cv1$model), function(d) cbind(model = d$model[1], family = d$family[1], start = d$start[1], metrics(d))))
cv_tab1 <- cv_tab1[order(cv_tab1$RMSE), ]
top3 <- head(cv_tab1$model[cv_tab1$family != "Benchmark" & cv_tab1$model != "auto.arima (log)"], 3)
s2_names <- unique(c(top3, "SARIMA(0,1,1)(0,1,1) log", "SARIMA(0,1,1)(0,1,1) + days"))   # + classic airline model and its calendar-adjusted version
stage2 <- list()
for (s in stage1) if (s$name %in% s2_names) for (st in c(1980, 2000)) stage2[[length(stage2) + 1]] <- spec(s$name, s$fc, s$family, st)
cat("\n[CV stage 2] window sensitivity for: ", paste(s2_names, collapse = " ; "), "\n", sep = "")
cv2 <- run_specs(stage2, cv_origins, 12)
cv_all <- rbind(cv1, cv2)
cv_tab <- do.call(rbind, lapply(split(cv_all, paste(cv_all$model, cv_all$start)), function(d) cbind(model = d$model[1], family = d$family[1], start = d$start[1], metrics(d))))
cv_tab <- cv_tab[order(cv_tab$RMSE), ]; rownames(cv_tab) <- NULL

# Full-training AICc for the SARIMA-family models (fit through Mar 2023)
aicc_tab <- do.call(rbind, lapply(c(stage1, stage2), function(s) {
  if (s$family == "Benchmark") return(NULL)
  i0 <- which(yr == s$start)[1]; tr <- i0:test_origin
  ytr <- ts(as.numeric(y_all[tr]), start = c(yr[i0], mo[i0]), frequency = 12)
  r <- tryCatch(s$fc(ytr, X_all[tr, , drop = FALSE], X_all[test_origin + 1:24, , drop = FALSE], 24), error = function(e) NULL)
  if (is.null(r)) return(NULL)
  rr <- as.numeric(residuals(r$fit))[-(1:13)]; kk <- sum(r$fit$arma[1:4])        # drop the d + D*s = 13 start-up residuals
  lbp <- function(L) Box.test(rr, lag = L, type = "Ljung-Box", fitdf = kk)$p.value
  data.frame(model = s$name, start = s$start, order = ord_str(r$fit), AICc = r$fit$aicc, loglik = r$fit$loglik, sigma2 = r$fit$sigma2,
             LB12_p = lbp(12), LB24_p = lbp(24))
}))
cv_tab <- merge(cv_tab, aicc_tab[, c("model", "start", "LB24_p")], by = c("model", "start"), all.x = TRUE)
cv_tab <- cv_tab[order(cv_tab$RMSE), ]; rownames(cv_tab) <- NULL

# Selection rule: lowest cross-validated RMSE among SARIMA-family models whose training residuals pass a
# Ljung-Box adequacy screen (lag 24, p >= 0.01). The unscreened winner is kept for transparency.
ADEQ <- 0.01
fam_ok <- cv_tab$family != "Benchmark"
unscreened <- cv_tab[fam_ok, ][1, ]
cand_ok <- fam_ok & !is.na(cv_tab$LB24_p) & cv_tab$LB24_p >= ADEQ
if (!any(cand_ok)) { message("No candidate passes the residual screen - falling back to lowest CV RMSE"); cand_ok <- fam_ok }
best_row <- cv_tab[cand_ok, ][1, ]
cat("
Best by CV RMSE alone:", unscreened$model, "| start", unscreened$start, "| LB24 p =", signif(unscreened$LB24_p, 3), "
")
addkey("unscreened_name", unscreened$model); addkey("unscreened_start", unscreened$start); addkey("unscreened_rmse", unscreened$RMSE); addkey("unscreened_lb24", unscreened$LB24_p)
addkey("n_pass_screen", sum(cand_ok & fam_ok)); addkey("n_sarima_cands", sum(fam_ok))
best_name <- best_row$model; best_start <- best_row$start
all_specs <- c(stage1, stage2)
best_spec <- all_specs[[which(sapply(all_specs, function(s) s$name == best_name & s$start == best_start))[1]]]
cat("\n>>> Selected by cross-validation:", best_name, "| training start", best_start, "| CV RMSE", r2(best_row$RMSE, 1), "\n")
addkey("best_name", best_name); addkey("best_start", best_start); addkey("best_cv_rmse", best_row$RMSE); addkey("best_cv_mape", best_row$MAPE); addkey("best_lb24_train", best_row$LB24_p)
bench_cv <- cv_tab[cv_tab$family == "Benchmark", ]
addkey("snaive_cv_rmse", bench_cv$RMSE[bench_cv$model == "Seasonal naive"]); addkey("ets_cv_rmse", bench_cv$RMSE[bench_cv$model == "ETS (log)"])
addkey("nonseas_cv_rmse", bench_cv$RMSE[bench_cv$model == "Non-seasonal ARIMA (log)"])
cv_out <- cv_tab; cv_out[, c("RMSE", "MAE", "MAPE", "MASE", "ME", "Cov80", "Cov95")] <- lapply(cv_out[, c("RMSE", "MAE", "MAPE", "MASE", "ME", "Cov80", "Cov95")], r2, 2)
cv_out$Residual_screen <- ifelse(is.na(cv_out$LB24_p), "", ifelse(cv_out$LB24_p >= ADEQ, "pass", "fail")); cv_out$LB24_p <- signif(cv_out$LB24_p, 3)
cv_out$Selected <- ifelse(cv_out$model == best_name & cv_out$start == best_start, "yes", "")
save_tab(cv_out, "t05_cv_summary")
aicc_out <- aicc_tab; aicc_out[, c("AICc", "loglik", "sigma2")] <- lapply(aicc_out[, c("AICc", "loglik", "sigma2")], r2, 4); aicc_out[, c("LB12_p", "LB24_p")] <- lapply(aicc_out[, c("LB12_p", "LB24_p")], signif, 3); save_tab(aicc_out, "t06_aicc_training")

# Fig 6: CV RMSE ranking
d6 <- cv_tab; d6$label <- ifelse(d6$start == 1960, d6$model, paste0(d6$model, " [from ", d6$start, "]"))
d6$label <- factor(d6$label, levels = rev(d6$label)); d6$fam <- ifelse(d6$family == "Benchmark", "Benchmark", ifelse(!is.na(d6$LB24_p) & d6$LB24_p >= ADEQ, "SARIMA family - residuals pass", "SARIMA family - residuals fail"))
p6 <- ggplot(d6, aes(RMSE, label, fill = fam)) + geom_col(width = 0.65) +
  geom_text(aes(label = comma(round(RMSE, 0))), hjust = -0.15, size = 3, colour = INK) +
  scale_fill_manual(values = c("SARIMA family - residuals pass" = C_BLUE, "SARIMA family - residuals fail" = "#a9c7ef", "Benchmark" = "#9a9a96")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12)), labels = comma) +
  labs(title = "Cross-validated accuracy (12-month horizon, 13 yearly origins, 2010-2022)", subtitle = "Root mean squared error in births per month - lower is better", x = "RMSE (births)", y = NULL) +
  theme_rep(9) + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = GRID, linewidth = 0.3))
save_fig(p6, "fig06_cv_ranking", 8.5, 6.4)

# Fig 7: RMSE by forecast horizon
pick <- c(best_name, "Seasonal naive", "ETS (log)", "Non-seasonal ARIMA (log)")
cvh <- cv_all[(cv_all$model %in% pick) & ((cv_all$model == best_name & cv_all$start == best_start) | cv_all$model != best_name), ]
hz <- do.call(rbind, lapply(split(cvh, list(cvh$model, cvh$h)), function(d) data.frame(model = d$model[1], h = d$h[1], RMSE = sqrt(mean((d$actual - d$fc)^2)))))
save_tab(transform(hz, RMSE = r2(RMSE, 1))[order(hz$model, hz$h), ], "t07_cv_by_horizon")
hz$model <- factor(hz$model, levels = pick)
p7 <- ggplot(hz, aes(h, RMSE, colour = model)) + geom_line(linewidth = 0.9) + geom_point(size = 1.8) +
  scale_colour_manual(values = setNames(c(C_BLUE, C_ORANGE, C_AQUA, "#6b6b6b"), pick)) +
  scale_x_continuous(breaks = 1:12) + scale_y_continuous(labels = comma, limits = c(0, NA)) +
  labs(title = "Forecast error by horizon (cross-validation)", subtitle = "Average error at each forecast horizon across the 13 cross-validation origins", x = "Months ahead", y = "RMSE (births)") + theme_rep(9)
save_fig(p7, "fig07_cv_horizon", 8, 4)

# ---- 6. Hold-out test (Apr 2023 - Mar 2025), scored once --------------------------
cat("\n[Hold-out test] ", length(all_specs), " model variants, h = 24\n", sep = "")
test_all <- run_specs(all_specs, test_origin, 24)
test_tab <- do.call(rbind, lapply(split(test_all, paste(test_all$model, test_all$start)), function(d) cbind(model = d$model[1], family = d$family[1], start = d$start[1], metrics(d))))
test_tab <- test_tab[order(test_tab$RMSE), ]; rownames(test_tab) <- NULL
test_tab$Selected <- ifelse(test_tab$model == best_name & test_tab$start == best_start, "yes", "")
tt <- test_tab; tt[, c("RMSE", "MAE", "MAPE", "MASE", "ME", "Cov80", "Cov95")] <- lapply(tt[, c("RMSE", "MAE", "MAPE", "MASE", "ME", "Cov80", "Cov95")], r2, 2)
save_tab(tt, "t08_test_accuracy")
sel_test <- test_tab[test_tab$Selected == "yes", ]
addkey("test_rmse", sel_test$RMSE); addkey("test_mape", sel_test$MAPE); addkey("test_mase", sel_test$MASE); addkey("test_cov95", sel_test$Cov95); addkey("test_cov80", sel_test$Cov80)
for (b in c("Seasonal naive", "ETS (log)", "Non-seasonal ARIMA (log)")) {
  r <- test_tab[test_tab$model == b, ]; nm <- c("Seasonal naive" = "snaive", "ETS (log)" = "ets", "Non-seasonal ARIMA (log)" = "nonseas")[b]
  addkey(paste0("test_rmse_", nm), r$RMSE); addkey(paste0("test_mape_", nm), r$MAPE)
}
# H1 vs H2: yearly split of test error for selected model
d_sel_test <- test_all[test_all$model == best_name & test_all$start == best_start, ]
d_sel_test$yr_block <- ifelse(d_sel_test$date <= as.Date("2024-03-01"), "Apr 2023 - Mar 2024", "Apr 2024 - Mar 2025")
yb <- do.call(rbind, lapply(split(d_sel_test, d_sel_test$yr_block), function(d) cbind(Block = d$yr_block[1], metrics(d)[, c("RMSE", "MAPE", "ME")])))
yb[, -1] <- lapply(yb[, -1], r2, 2); save_tab(yb, "t09_test_by_year")
d_sel_test$err_pct <- (d_sel_test$actual / d_sel_test$fc - 1) * 100
save_tab(transform(d_sel_test[, c("date", "actual", "fc", "lo95", "hi95", "err_pct")], fc = round(fc), lo95 = round(lo95), hi95 = round(hi95), err_pct = r2(err_pct, 1)), "t10_test_monthly")
addkey("test_first_block_mape", yb$MAPE[1]); addkey("test_second_block_mape", yb$MAPE[2])

# Fig 8: hold-out forecast vs actual
hist_k <- which(dates >= as.Date("2020-04-01") & dates <= as.Date("2023-03-01"))
mk <- function(m, s) test_all[test_all$model == m & test_all$start == s, ]
dsn <- mk("Seasonal naive", 1960); dets <- mk("ETS (log)", 1960)
d8_line <- rbind(data.frame(date = d_sel_test$date, v = d_sel_test$fc, s = "Selected SARIMA"),
                 data.frame(date = dsn$date, v = dsn$fc, s = "Seasonal naive"), data.frame(date = dets$date, v = dets$fc, s = "ETS (log)"))
d8_line$s <- factor(d8_line$s, levels = c("Selected SARIMA", "Seasonal naive", "ETS (log)"))
d8_act <- data.frame(date = dates[c(hist_k, test_origin + 1:24)], v = M[c(hist_k, test_origin + 1:24), "Total"])
p8 <- ggplot() +
  geom_ribbon(data = d_sel_test, aes(date, ymin = lo95, ymax = hi95), fill = C_BLUE, alpha = 0.12) +
  geom_ribbon(data = d_sel_test, aes(date, ymin = lo80, ymax = hi80), fill = C_BLUE, alpha = 0.20) +
  geom_line(data = d8_act, aes(date, v), colour = INK, linewidth = 0.6) +
  geom_line(data = d8_line, aes(date, v, colour = s), linewidth = 0.9) +
  geom_vline(xintercept = as.Date("2023-03-16"), linetype = "dashed", colour = MUTED) +
  scale_colour_manual(values = c("Selected SARIMA" = C_BLUE, "Seasonal naive" = C_ORANGE, "ETS (log)" = C_AQUA)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Hold-out test: April 2023 - March 2025", subtitle = "Black = actual births; shaded = 80% and 95% prediction intervals of the selected model", x = NULL, y = "Births per month") + theme_rep(9)
save_fig(p8, "fig08_test_forecast", 8.5, 4.2)

# ---- 7. Final model: diagnostics, coefficients, forecasts -------------------------
i0 <- which(yr == best_start)[1]; win <- i0:n
y_win <- ts(as.numeric(y_all[win]), start = c(yr[i0], mo[i0]), frequency = 12)
final <- best_spec$fc(y_win, X_all[win, , drop = FALSE], X_fut, 24)
fit <- final$fit
final_order <- ord_str(fit); addkey("final_order", final_order)
cat("\nFinal model (all data):", final_order, "| AICc", r2(fit$aicc, 1), "\n"); print(round(coef(fit), 4)); cat("sigma^2 =", signif(fit$sigma2, 4), " loglik =", r2(fit$loglik, 1), "\n")
se <- sqrt(diag(fit$var.coef)); cf <- coef(fit)
coef_tab <- data.frame(Term = names(cf), Estimate = r2(cf, 4), SE = r2(se, 4), z = r2(cf / se, 2), p = signif(2 * pnorm(-abs(cf / se)), 3))
save_tab(coef_tab, "t11_model_coefficients")
addkey("theta1", cf["ma1"]); addkey("Theta1", if ("sma1" %in% names(cf)) cf["sma1"] else NA)
addkey("final_aicc", fit$aicc); addkey("final_n", length(y_win)); addkey("final_sigma", sqrt(fit$sigma2))

# Diagnostics on innovation residuals (drop the d + D*s = 13 start-up values)
res <- as.numeric(residuals(fit))[-(1:13)]; rz <- (res - mean(res)) / sd(res)
kfit <- sum(fit$arma[1:4])
lb <- do.call(rbind, lapply(c(12, 24, 36), function(L) { b <- Box.test(res, lag = L, type = "Ljung-Box", fitdf = kfit); data.frame(Test = paste0("Ljung-Box, lag ", L), Statistic = r2(unname(b$statistic), 2), df = L - kfit, p_value = signif(b$p.value, 3)) }))
sw <- shapiro.test(res); jb <- suppressWarnings(jarque.bera.test(res))
diag_tab <- rbind(lb, data.frame(Test = "Shapiro-Wilk (normality)", Statistic = r2(unname(sw$statistic), 4), df = NA, p_value = signif(sw$p.value, 3)),
                  data.frame(Test = "Jarque-Bera (normality)", Statistic = r2(unname(jb$statistic), 2), df = 2, p_value = signif(jb$p.value, 3)),
                  data.frame(Test = "Ljung-Box on squared residuals, lag 12", Statistic = r2(unname(Box.test(res^2, lag = 12, type = "Ljung-Box")$statistic), 2), df = 12, p_value = signif(Box.test(res^2, lag = 12, type = "Ljung-Box")$p.value, 3)))
save_tab(diag_tab, "t12_residual_diagnostics")
for (i in 1:3) addkey(paste0("lb", c(12, 24, 36)[i]), lb$p_value[i]); addkey("shapiro_p", sw$p.value); addkey("jb_p", jb$p.value)
res_dates <- dates[win][-(1:13)]
out_idx <- which(abs(rz) > 3)
out_tab <- data.frame(Month = format(res_dates[out_idx], "%Y-%m"), Std_residual = r2(rz[out_idx], 2), Births = M[win, "Total"][-(1:13)][out_idx])
save_tab(out_tab, "t13_residual_outliers"); addkey("n_outliers", length(out_idx))
acf_res <- acf(res, lag.max = 36, plot = FALSE)$acf[-1]; addkey("n_acf_out", sum(abs(acf_res) > 1.96 / sqrt(length(res))))

r1 <- ggplot(data.frame(d = res_dates, z = rz), aes(d, z)) + geom_hline(yintercept = c(-3, 3), linetype = "dashed", colour = MUTED) + geom_hline(yintercept = 0, colour = GRID) +
  geom_line(colour = C_BLUE, linewidth = 0.3) + labs(title = "Standardised residuals", x = NULL, y = NULL) + theme_rep(9)
r2p <- ggAcf(res, lag.max = 36) + labs(title = "Residual ACF", x = "Lag (months)", y = NULL) + theme_rep(9)
r3 <- ggplot(data.frame(z = rz), aes(z)) + geom_histogram(aes(y = after_stat(density)), bins = 40, fill = C_BLUE, alpha = 0.7, colour = "white") +
  stat_function(fun = dnorm, colour = INK, linewidth = 0.6) + labs(title = "Residual distribution", x = NULL, y = NULL) + theme_rep(9)
r4 <- ggplot(data.frame(z = rz), aes(sample = z)) + stat_qq(colour = C_BLUE, size = 0.8) + stat_qq_line(colour = INK) + labs(title = "Normal Q-Q plot", x = "Theoretical", y = "Sample") + theme_rep(9)
save_fig((r1 | r2p) / (r3 | r4), "fig09_residuals", 8.5, 6)

# 24-month forecast
fc_tab <- data.frame(Month = format(fut_dates, "%Y-%m"), Forecast = round(final$mean), Lo80 = round(final$lo80), Hi80 = round(final$hi80), Lo95 = round(final$lo95), Hi95 = round(final$hi95))
save_tab(fc_tab, "t14_forecast_monthly")
h_k <- which(dates >= as.Date("2017-01-01"))
d10h <- data.frame(date = dates[h_k], v = M[h_k, "Total"])
d10f <- data.frame(date = fut_dates, fc = final$mean, lo80 = final$lo80, hi80 = final$hi80, lo95 = final$lo95, hi95 = final$hi95)
p10 <- ggplot() + geom_ribbon(data = d10f, aes(date, ymin = lo95, ymax = hi95), fill = C_BLUE, alpha = 0.12) +
  geom_ribbon(data = d10f, aes(date, ymin = lo80, ymax = hi80), fill = C_BLUE, alpha = 0.20) +
  geom_line(data = d10h, aes(date, v), colour = INK, linewidth = 0.6) + geom_line(data = d10f, aes(date, fc), colour = C_BLUE, linewidth = 1) +
  scale_y_continuous(labels = comma) +
  labs(title = paste0("Forecast of monthly births, April 2025 - March 2027 (", final_order, ")"), subtitle = "Black = actual; blue = forecast with 80% and 95% prediction intervals", x = NULL, y = "Births per month") + theme_rep(9)
save_fig(p10, "fig10_forecast", 8.5, 4.2)

# Annual totals with simulation-based intervals
uses_x <- grepl("\\+ days", best_name)
xc <- if (grepl("Dragon", best_name)) c("ldays", "dragon", "post") else if (uses_x) "ldays" else NULL
sims <- replicate(3000, as.numeric(if (is.null(xc)) simulate(fit, nsim = 24, future = TRUE) else simulate(fit, nsim = 24, future = TRUE, xreg = X_fut[, xc, drop = FALSE])))
if (median(sims) < 100) sims <- exp(sims)                    # (safety: back-transform if simulate() returned log scale)
cat("Simulation check: median simulated month =", round(median(sims)), "vs forecast median =", round(median(final$mean)), "\n")
q1_2025 <- sum(M[yr == 2025, "Total"])
tot25 <- q1_2025 + colSums(sims[1:9, ]); tot26 <- colSums(sims[10:21, ])
qs <- function(x) c(mean = mean(x), p2.5 = quantile(x, .025), p10 = quantile(x, .10), p90 = quantile(x, .90), p97.5 = quantile(x, .975))
ann_fc <- data.frame(Year = c(2025, 2026), Basis = c("Jan-Mar actual + Apr-Dec forecast", "12 forecast months"), rbind(qs(tot25), qs(tot26)), row.names = NULL)
names(ann_fc)[3:7] <- c("Mean", "Lo95", "Lo80", "Hi80", "Hi95"); ann_fc[, 3:7] <- round(ann_fc[, 3:7])
save_tab(ann_fc, "t15_forecast_annual")
addkey("q1_2025", q1_2025); addkey("tot25", ann_fc$Mean[1]); addkey("tot25_lo", ann_fc$Lo95[1]); addkey("tot25_hi", ann_fc$Hi95[1])
addkey("tot26", ann_fc$Mean[2]); addkey("tot26_lo", ann_fc$Lo95[2]); addkey("tot26_hi", ann_fc$Hi95[2])
addkey("q1_2024", sum(M[yr == 2024 & mo <= 3, "Total"]))
ann_hist <- data.frame(Year = as.integer(names(ann_complete)), v = as.numeric(ann_complete)); ann_hist <- ann_hist[ann_hist$Year >= 2005, ]
ann_f <- data.frame(Year = c(2025, 2026), v = ann_fc$Mean, lo = ann_fc$Lo95, hi = ann_fc$Hi95)
p11 <- ggplot() + geom_col(data = ann_hist, aes(Year, v), fill = "#9a9a96", width = 0.7) +
  geom_pointrange(data = ann_f, aes(Year, v, ymin = lo, ymax = hi), colour = C_BLUE, linewidth = 0.9, size = 0.6) +
  geom_text(data = ann_f, aes(Year, hi, label = comma(round(v))), vjust = -0.6, size = 3, colour = INK) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) + scale_x_continuous(breaks = seq(2005, 2026, 3)) +
  labs(title = "Annual births: history and SARIMA-implied 2025 and 2026", subtitle = "Grey = actual complete years; blue = forecast mean with 95% interval (2025 includes actual Jan-Mar)", x = NULL, y = "Births per year") + theme_rep(9)
save_fig(p11, "fig11_annual", 8.5, 4)

# ---- 8. SARIMA-based analysis by segment -----------------------------------------------
# Every segment is modelled with the SAME structure as the selected total model (same differencing, same
# training window), so the segments are comparable and can be added up. auto.arima, ETS and seasonal naive
# are shown for comparison. Accuracy is scored on the same 24-month hold-out.
segs <- c("Malays", "Chinese", "Indians", "Others", "Male", "Female")
SAME <- "Same SARIMA as total (log)"
cat("\n[Segments] same structure / auto.arima / ETS / seasonal naive on ", length(segs), " series\n", sep = "")
seg_start <- best_start
seg_test <- list(); seg_models <- list(); seg_fc <- list()
for (s in segs) {
  v <- M[, s]
  a0 <- run_cv(v, best_spec$fc, test_origin, 24, seg_start); a0$model <- SAME
  a  <- run_cv(v, fc_auto, test_origin, 24, seg_start);      a$model <- "Auto SARIMA (log)"
  b  <- run_cv(v, fc_ets, test_origin, 24, seg_start);       b$model <- "ETS (log)"
  c_ <- run_cv(v, fc_snaive, test_origin, 24, seg_start);    c_$model <- "Seasonal naive"
  d <- rbind(a0, a, b, c_); d$segment <- s; seg_test[[s]] <- d
  i0s <- which(yr == seg_start)[1]
  yfull <- ts(as.numeric(v[i0s:n]), start = c(yr[i0s], mo[i0s]), frequency = 12)
  fs <- best_spec$fc(yfull, X_all[i0s:n, , drop = FALSE], X_fut, 24)          # refit on all data, forecast Apr 2025 - Mar 2027
  fa <- auto.arima(yfull, lambda = 0)
  cf_s <- coef(fs$fit); se_s <- sqrt(diag(fs$fit$var.coef))
  seg_models[[s]] <- data.frame(Segment = s, Same_structure = ord_str(fs$fit),
                                Coefficients = paste(sprintf("%s = %.3f (%.3f)", names(cf_s), cf_s, se_s), collapse = "; "),
                                Auto_SARIMA_order = ord_str(fa))
  seg_fc[[s]] <- data.frame(segment = s, date = fut_dates, fc = fs$mean, lo95 = fs$lo95, hi95 = fs$hi95)
  cat("  ", s, ":", ord_str(fs$fit), "| auto.arima chose", ord_str(fa), "\n")
}
seg_test_all <- do.call(rbind, seg_test)
seg_acc <- do.call(rbind, lapply(split(seg_test_all, list(seg_test_all$segment, seg_test_all$model)), function(d) cbind(Segment = d$segment[1], Model = d$model[1], metrics(d)[, c("RMSE", "MAPE", "MASE", "ME", "Cov95")])))
seg_acc <- seg_acc[order(match(seg_acc$Segment, segs), seg_acc$MAPE), ]; rownames(seg_acc) <- NULL
seg_acc[, c("RMSE", "MAPE", "MASE", "ME", "Cov95")] <- lapply(seg_acc[, c("RMSE", "MAPE", "MASE", "ME", "Cov95")], r2, 2)
save_tab(seg_acc, "t16_segment_test_accuracy"); save_tab(do.call(rbind, seg_models), "t17_segment_models")


# Seasonal features per segment (2000-2024, calendar-adjusted, STL on log)
feat <- do.call(rbind, lapply(c("Total", segs), function(s) {
  k <- which(yr >= 2000 & yr <= 2024); x <- log(M[k, s] / dim_all[k])
  st <- stl(ts(x, start = c(2000, 1), frequency = 12), s.window = "periodic", robust = TRUE)$time.series
  si <- exp(as.numeric(st[1:12, "seasonal"])); rem <- st[, "remainder"]
  data.frame(Segment = s, Seasonal_strength = r2(max(0, 1 - var(rem) / var(st[, "seasonal"] + rem)), 3),
             Trend_strength = r2(max(0, 1 - var(rem) / var(st[, "trend"] + rem)), 3),
             Peak_month = month.abb[which.max(si)], Trough_month = month.abb[which.min(si)], Peak_to_trough_pct = r2((max(si) / min(si) - 1) * 100, 1),
             Mean_monthly_2024 = round(mean(M[yr == 2024, s])))
}))
save_tab(feat, "t18_segment_seasonal_features")
si_seg <- do.call(rbind, lapply(c("Malays", "Chinese", "Indians", "Others"), function(s) data.frame(Month = 1:12, Index = seas_idx(M[, s], dates, 2000, 2024, TRUE), Group = s)))
p13 <- ggplot(si_seg, aes(Month, Index, colour = Group)) + geom_hline(yintercept = 1, colour = GRID) + geom_line(linewidth = 0.9) + geom_point(size = 1.8) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) + scale_colour_manual(values = c(Malays = C_BLUE, Chinese = C_ORANGE, Indians = C_AQUA, Others = C_YELLOW)) +
  labs(title = "Seasonal shape by ethnic group, 2000-2024", subtitle = "Calendar-adjusted seasonal index (births per day relative to the yearly level)", x = NULL, y = "Seasonal index") + theme_rep(9)
save_fig(p13, "fig13_segment_seasonality", 8, 3.9)

# Fig 12: hold-out forecasts for each segment (same-structure SARIMA vs actual)
d12 <- do.call(rbind, lapply(segs, function(s) {
  a <- seg_test_all[seg_test_all$segment == s & seg_test_all$model == SAME, ]
  hk <- which(dates >= as.Date("2021-04-01") & dates <= as.Date("2025-03-01"))
  rbind(data.frame(segment = s, date = dates[hk], v = M[hk, s], kind = "Actual"), data.frame(segment = s, date = a$date, v = a$fc, kind = "SARIMA forecast"))
}))
d12$segment <- factor(d12$segment, levels = segs)
p12 <- ggplot(d12, aes(date, v, colour = kind)) + geom_line(linewidth = 0.6) + facet_wrap(~segment, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = c(Actual = INK, "SARIMA forecast" = C_BLUE)) + scale_y_continuous(labels = comma) +
  geom_vline(xintercept = as.Date("2023-03-16"), linetype = "dashed", colour = MUTED) +
  labs(title = "Hold-out forecasts by segment (Apr 2023 - Mar 2025)", subtitle = "Model fitted to data up to March 2023; dashed line marks the forecast origin", x = NULL, y = "Births per month") + theme_rep(9)
save_fig(p12, "fig12_segment_holdout", 8.5, 6.2)

# Bottom-up versus direct forecasting of the total
ths <- function(nm, model) seg_test_all[seg_test_all$segment == nm & seg_test_all$model == model, "fc"]
eth4 <- c("Malays", "Chinese", "Indians", "Others")
bu_eth <- Reduce(`+`, lapply(eth4, ths, model = SAME)); bu_sex <- ths("Male", SAME) + ths("Female", SAME)
bu_eth_auto <- Reduce(`+`, lapply(eth4, ths, model = "Auto SARIMA (log)"))
act <- d_sel_test$actual
bu <- function(f) data.frame(RMSE = sqrt(mean((act - f)^2)), MAPE = mean(abs(act - f) / act) * 100, ME = mean(act - f))
bu_tab <- rbind(cbind(Approach = "Direct: selected SARIMA on the total", bu(d_sel_test$fc)),
                cbind(Approach = "Bottom-up: sum of 4 ethnic-group SARIMAs (same structure)", bu(bu_eth)),
                cbind(Approach = "Bottom-up: Male + Female SARIMAs (same structure)", bu(bu_sex)),
                cbind(Approach = "Bottom-up: sum of 4 ethnic-group auto.arima models", bu(bu_eth_auto)),
                cbind(Approach = "Average of direct and ethnic bottom-up (same structure)", bu((d_sel_test$fc + bu_eth) / 2)))
bu_tab[, -1] <- lapply(bu_tab[, -1], r2, 2); save_tab(bu_tab, "t19_bottom_up_vs_direct")
addkey("bu_eth_rmse", bu(bu_eth)$RMSE); addkey("bu_eth_mape", bu(bu_eth)$MAPE); addkey("bu_sex_rmse", bu(bu_sex)$RMSE); addkey("bu_sex_mape", bu(bu_sex)$MAPE); addkey("bu_avg_rmse", bu((d_sel_test$fc + bu_eth) / 2)$RMSE); addkey("bu_avg_mape", bu((d_sel_test$fc + bu_eth) / 2)$MAPE); addkey("direct_rmse", bu(d_sel_test$fc)$RMSE); addkey("direct_mape", bu(d_sel_test$fc)$MAPE)

# Forecast composition: annual shares and 2026 forecasts by segment
sf <- do.call(rbind, seg_fc); save_tab(transform(sf, fc = round(fc), lo95 = round(lo95), hi95 = round(hi95)), "t20_segment_forecast_monthly")
seg26 <- do.call(rbind, lapply(segs, function(s) { d <- seg_fc[[s]]; data.frame(Segment = s, Births_2025_AprDec = round(sum(d$fc[1:9])), Births_2026 = round(sum(d$fc[10:21])),
        Actual_2024 = sum(M[yr == 2024, s]), Change_2026_vs_2024_pct = r2((sum(d$fc[10:21]) / sum(M[yr == 2024, s]) - 1) * 100, 1)) }))
save_tab(seg26, "t21_segment_forecast_annual")
share <- do.call(rbind, lapply(c("Malays", "Chinese", "Indians", "Others"), function(s) {
  a <- tapply(M[, s], yr, sum)[as.character(2000:2024)] / tapply(rowSums(M[, c("Malays", "Chinese", "Indians", "Others")]), yr, sum)[as.character(2000:2024)]
  f26 <- sum(seg_fc[[s]]$fc[10:21]) / sum(sapply(c("Malays", "Chinese", "Indians", "Others"), function(g) sum(seg_fc[[g]]$fc[10:21])))
  rbind(data.frame(Group = s, Year = 2000:2024, Share = as.numeric(a) * 100, Kind = "Actual"), data.frame(Group = s, Year = 2026, Share = f26 * 100, Kind = "Forecast"))
}))
save_tab(transform(share, Share = r2(Share, 2)), "t22_ethnic_shares")
p15 <- ggplot(share, aes(Year, Share, colour = Group)) + geom_line(data = subset(share, Kind == "Actual"), linewidth = 0.9) +
  geom_point(data = subset(share, Kind == "Forecast"), size = 2.6) +
  scale_colour_manual(values = c(Malays = C_BLUE, Chinese = C_ORANGE, Indians = C_AQUA, Others = C_YELLOW)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Ethnic composition of live births", subtitle = "Lines = actual annual share; dots = share implied by the 2026 segment forecasts", x = NULL, y = "Share of live births") + theme_rep(9)
save_fig(p15, "fig15_ethnic_shares", 8, 3.9)

# Sex ratio at birth (male per female)
sr_dec <- do.call(rbind, lapply(seq(1960, 2020, 10), function(d0) { k <- yr >= d0 & yr <= min(d0 + 9, 2024); data.frame(Decade = paste0(d0, "s"), Male_per_female = r2(sum(M[k, "Male"]) / sum(M[k, "Female"]), 4)) }))
sr_dec <- rbind(sr_dec, data.frame(Decade = "2026 forecast", Male_per_female = r2(sum(seg_fc[["Male"]]$fc[10:21]) / sum(seg_fc[["Female"]]$fc[10:21]), 4)))
save_tab(sr_dec, "t23_sex_ratio")

# ---- 9. Dragon-year effects (regression with SARIMA errors) -------------------------------
dragon_effect <- function(v, s, start_year = 1960) {
  i0 <- which(yr == start_year)[1]; idx <- i0:n
  ev <- dragon_years[dragon_years >= start_year & dragon_years <= 2024]
  Xd <- sapply(ev, function(Y) as.numeric(yr[idx] == Y)); colnames(Xd) <- paste0("d", ev); Xd <- cbind(Xd, ldays = log(dim_all[idx]))
  fitd <- do.call(Arima, list(y = ts(v[idx], start = c(yr[i0], mo[i0]), frequency = 12), order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12), lambda = 0, xreg = Xd))
  b <- coef(fitd); se_ <- sqrt(diag(fitd$var.coef)); nm <- paste0("d", ev)
  data.frame(Group = s, Dragon_year = ev, Effect_pct = (exp(b[nm]) - 1) * 100, Lo95 = (exp(b[nm] - 1.96 * se_[nm]) - 1) * 100, Hi95 = (exp(b[nm] + 1.96 * se_[nm]) - 1) * 100,
             p = 2 * pnorm(-abs(b[nm] / se_[nm])), ldays_coef = unname(b["ldays"]), row.names = NULL)
}
dg <- rbind(dragon_effect(M[, "Total"], "Total"), do.call(rbind, lapply(c("Malays", "Chinese", "Indians", "Others"), function(s) dragon_effect(M[, s], s))))
dg_out <- dg; dg_out[, c("Effect_pct", "Lo95", "Hi95", "ldays_coef")] <- lapply(dg_out[, c("Effect_pct", "Lo95", "Hi95", "ldays_coef")], r2, 2); dg_out$p <- signif(dg_out$p, 3)
save_tab(dg_out, "t24_dragon_effects")
dgt <- dg[dg$Group == "Total", ]; for (i in seq_len(nrow(dgt))) addkey(paste0("dragon_total_", dgt$Dragon_year[i]), dgt$Effect_pct[i])
dgc <- dg[dg$Group == "Chinese", ]; for (i in seq_len(nrow(dgc))) addkey(paste0("dragon_chinese_", dgc$Dragon_year[i]), dgc$Effect_pct[i])
addkey("ldays_coef_total", dgt$ldays_coef[1])
dg$Group <- factor(dg$Group, levels = c("Total", "Malays", "Chinese", "Indians", "Others"))
p14 <- ggplot(dg, aes(factor(Dragon_year), Effect_pct)) + geom_hline(yintercept = 0, colour = MUTED) +
  geom_errorbar(aes(ymin = Lo95, ymax = Hi95), width = 0.2, colour = C_BLUE, linewidth = 0.5) + geom_point(colour = C_BLUE, size = 2.2) +
  facet_wrap(~Group, ncol = 3) +
  labs(title = "Estimated Dragon-year effect on monthly births", subtitle = "Percentage difference vs. the SARIMA-expected level, with 95% confidence intervals", x = "Dragon year", y = "Effect (%)") + theme_rep(9)
save_fig(p14, "fig14_dragon_effects", 8.5, 5)

# ---- 10. Cross-check against the annual file & wrap-up -----------------------------------------
ann_file <- file.path(dirname(data_file), "Singapore_Births_Fertility_Long_1960_2025_new.csv")
if (file.exists(ann_file)) {
  a <- read.csv(ann_file); a <- a[a$DataSeries == "Total Live-Births", ]
  cmp <- merge(data.frame(Year = as.integer(names(ann_tot)), Monthly_sum = as.numeric(ann_tot), Months = as.integer(ann_n)), a[, c("Year", "Value")], by = "Year")
  names(cmp)[4] <- "Annual_file"; cmp$Diff <- cmp$Monthly_sum - cmp$Annual_file
  addkey("annual_match_complete_years", all(cmp$Diff[cmp$Months == 12] == 0)); addkey("annual_2025", cmp$Annual_file[cmp$Year == 2025])
  save_tab(cmp[cmp$Year >= 2019, ], "t25_annual_crosscheck")
  need25 <- cmp$Annual_file[cmp$Year == 2025] - q1_2025; base25 <- sum(M[yr == 2024 & mo >= 4, "Total"])
  addkey("aprdec_needed_yoy_pct", (need25 / base25 - 1) * 100)
}
kv <- data.frame(key = names(key), value = sapply(key, function(x) if (is.numeric(x)) as.character(signif(x, 8)) else as.character(x)), row.names = NULL)
save_tab(kv, "key_numbers")
cat("\nDone in", round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), "minutes. Outputs in", file.path(base_dir, "output"), "\n")
writeLines(capture.output(sessionInfo()), file.path(base_dir, "output", "sessionInfo.txt"))
