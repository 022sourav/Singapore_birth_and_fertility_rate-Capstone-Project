# ============================================================
# PROJECT: Modelling Singapore Birth and Fertility Rates
# Team: PG-S2-51
#
# Data Cleaning and Exploratory Data Analysis
#
# Data period: 1960 - 2025
# Training period: 1960 - 2012
# Testing period: 2013 - 2025
# ============================================================



# LOAD PACKAGES
#install.packages("janitor")

library(tidyverse)
library(janitor)



# DATA CLEANING
# ============================================================


# LOAD ORIGINAL DATA
# ============================================================

raw_data <- read.csv("BirthsAndFertilityRatesAnnual.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)


# Look at the original data

head(raw_data)

str(raw_data)

names(raw_data)


# TRANSPOSE THE DATA
# ============================================================

# The original data has:
# - Indicators in rows
# - Years in columns
#
# We want:
# - Years in rows
# - Indicators in columns


data_transposed <- as.data.frame(
  t(raw_data[, -1])
)


# Use the DataSeries values as column names

colnames(data_transposed) <- raw_data$DataSeries


# Create the Year column

data_transposed$Year <- rownames(data_transposed)


# Remove row names

rownames(data_transposed) <- NULL


# Put Year as the first column

data <- data_transposed %>%
  select(
    Year,
    everything()
  )

# If X before the year, remove it

data <- data %>%
  mutate(
    Year = str_remove(Year, "^X"),
    Year = as.numeric(Year)
  )

#  CLEAN COLUMN NAMES
# ============================================================

# Make column names easier to use in R

names(data) <- make_clean_names(
  names(data)
)

data <- data %>%
  rename(
    age_15_19 = x15_19_years,
    age_20_24 = x20_24_years,
    age_25_29 = x25_29_years,
    age_30_34 = x30_34_years,
    age_35_39 = x35_39_years,
    age_40_44 = x40_44_years,
    age_45_49 = x45_49_years
  )

# Check the new column names

names(data)


# CONVERT DATA VALUES TO NUMERIC
# ============================================================

# Convert all columns except Year to numeric

data <- data %>%
  mutate(
    across(
      -year,
      as.numeric
    )
  )


# SORT DATA BY YEAR
# ============================================================

data <- data %>%
  arrange(year)


# CHECK CLEANED DATA
# ============================================================

head(data)

tail(data)

str(data)

summary(data)




# CHECK DATA RANGE
# ============================================================

min_year <- min(data$year, na.rm = TRUE)

max_year <- max(data$year, na.rm = TRUE)


print(
  paste("First year:", min_year)
)

print(
  paste("Last year:", max_year)
)


# Expected:
# First year: 1960
# Last year: 2025


# CHECK MISSING VALUES
# ============================================================

missing_values <- data %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  )


print(missing_values)


# CHECK FOR DUPLICATE YEARS
# ============================================================

data %>%
  count(year) %>%
  filter(n > 1)


# CREATE TRAINING DATA
# ============================================================

train_data <- data %>%
  filter(
    year >= 1960,
    year <= 2012
  )


# CREATE TESTING DATA
# ============================================================

test_data <- data %>%
  filter(
    year >= 2013,
    year <= 2025
  )


# Check dimensions

dim(train_data)

dim(test_data)


# CREATE OUTPUT FOLDERS
# ============================================================

if (!dir.exists("Output")) {
  dir.create("Output")
}


if (!dir.exists("Plots")) {
  dir.create("Plots")
}


# SAVE CLEANED DATA
# ============================================================

write_csv(
  data,
  "Output/Singapore_Birth_Fertility_Cleaned.csv"
)


write_csv(
  train_data,
  "Output/Training_Data_1960_2012.csv"
)


write_csv(
  test_data,
  "Output/Testing_Data_2013_2025.csv"
)




# ============================================================
# EXPLORATORY DATA ANALYSIS
# ============================================================


# ============================================================
# TOTAL FERTILITY RATE
# ============================================================

ggplot(
  data,
  aes(
    x = year,
    y = total_fertility_rate_tfr
  )
) +
  geom_line(
    linewidth = 1.2,
    colour = "steelblue"
  ) +
  geom_point(
    size = 1.5,
    colour = "steelblue"
  ) +
  labs(
    title = "Singapore Total Fertility Rate",
    subtitle = "1960 to 2025",
    x = "Year",
    y = "Total Fertility Rate"
  ) +
  theme_minimal()

# ============================================================
# TOTAL LIVE-BIRTHS
# ============================================================

ggplot(
  data,
  aes(
    x = year,
    y = total_live_births
  )
) +
  geom_line(
    linewidth = 1.2,
    colour = "forestgreen"
  ) +
  geom_point(
    size = 1.5,
    colour = "forestgreen"
  ) +
  labs(
    title = "Total Live-Births in Singapore",
    subtitle = "1960 to 2025",
    x = "Year",
    y = "Total Live-Births"
  ) +
  theme_minimal()

# ============================================================
# CRUDE BIRTH RATE
# ============================================================

ggplot(
  data,
  aes(
    x = year,
    y = crude_birth_rate
  )
) +
  geom_line(
    linewidth = 1.2,
    colour = "darkorange"
  ) +
  geom_point(
    size = 1.5,
    colour = "darkorange"
  ) +
  labs(
    title = "Singapore Crude Birth Rate",
    subtitle = "1960 to 2025",
    x = "Year",
    y = "Birth Rate per 1,000 Population"
  ) +
  theme_minimal()


# ============================================================
# AGE-SPECIFIC FERTILITY RATES
# ============================================================

age_data <- data %>%
  select(
    year,
    age_15_19,
    age_20_24,
    age_25_29,
    age_30_34,
    age_35_39,
    age_40_44,
    age_45_49
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "Age_Group",
    values_to = "Fertility_Rate"
  )


ggplot(
  age_data,
  aes(
    x = year,
    y = Fertility_Rate,
    group = Age_Group,
    colour = Age_Group
  )
) +
  geom_line(
    linewidth = 1
  ) +
  scale_colour_brewer(
    palette = "Dark2",
    labels = c(
      age_15_19 = "15–19 years",
      age_20_24 = "20–24 years",
      age_25_29 = "25–29 years",
      age_30_34 = "30–34 years",
      age_35_39 = "35–39 years",
      age_40_44 = "40–44 years",
      age_45_49 = "45–49 years"
    )
  ) +
  labs(
    title = "Age-Specific Fertility Rates",
    subtitle = "Singapore, 1960 to 2025",
    x = "Year",
    y = "Fertility Rate",
    colour = "Age Group"
  ) +
  theme_minimal()

# ============================================================
# AGE-SPECIFIC FERTILITY: 1960 VS 2025
# ============================================================

age_comparison <- age_data %>%
  filter(
    year %in% c(1960, 2025)
  )


ggplot(
  age_comparison,
  aes(
    x = Age_Group,
    y = Fertility_Rate,
    group = factor(year),
    colour = factor(year)
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  scale_colour_manual(
    values = c(
      "1960" = "steelblue",
      "2025" = "darkorange"
    )
  ) +
  labs(
    title = "Age-Specific Fertility: 1960 vs 2025",
    subtitle = "Comparison of fertility patterns across age groups",
    x = "Age Group",
    y = "Fertility Rate",
    colour = "Year"
  ) +
  theme_minimal()


# ============================================================
# ETHNIC GROUP FERTILITY
# ============================================================

ethnic_data <- data %>%
  select(
    year,
    chinese,
    malays,
    indians
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "Ethnic_Group",
    values_to = "Fertility_Rate"
  )


ggplot(
  ethnic_data,
  aes(
    x = year,
    y = Fertility_Rate,
    group = Ethnic_Group,
    colour = Ethnic_Group
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  scale_colour_manual(
    values = c(
      "chinese" = "steelblue",
      "malays" = "darkorange",
      "indians" = "forestgreen"
    ),
    labels = c(
      chinese = "Chinese",
      malays = "Malay",
      indians = "Indian"
    )
  ) +
  labs(
    title = "Fertility Rates by Ethnic Group",
    subtitle = "Singapore, 1960 to 2025",
    x = "Year",
    y = "Fertility Rate",
    colour = "Ethnic Group"
  ) +
  theme_minimal()

# ============================================================
# GROSS AND NET REPRODUCTION RATE
# ============================================================

reproduction_data <- data %>%
  select(
    year,
    gross_reproduction_rate,
    net_reproduction_rate
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "Rate_Type",
    values_to = "Rate"
  )


ggplot(
  reproduction_data,
  aes(
    x = year,
    y = Rate,
    group = Rate_Type,
    colour = Rate_Type
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  scale_colour_manual(
    values = c(
      "gross_reproduction_rate" = "purple",
      "net_reproduction_rate" = "red"
    ),
    labels = c(
      gross_reproduction_rate = "Gross Reproduction Rate",
      net_reproduction_rate = "Net Reproduction Rate"
    )
  ) +
  labs(
    title = "Gross and Net Reproduction Rates",
    subtitle = "Singapore, 1960 to 2025",
    x = "Year",
    y = "Reproduction Rate",
    colour = "Rate Type"
  ) +
  theme_minimal()


# ============================================================
# MAIN INDICATORS
# ============================================================

main_data <- data %>%
  select(
    year,
    total_fertility_rate_tfr,
    total_live_births
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "Indicator",
    values_to = "Value"
  )


ggplot(
  main_data,
  aes(
    x = year,
    y = Value,
    colour = Indicator
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  facet_wrap(
    ~ Indicator,
    scales = "free_y",
    labeller = as_labeller(
      c(
        total_fertility_rate_tfr = "Total Fertility Rate",
        total_live_births = "Total Live-Births"
      )
    )
  ) +
  scale_colour_manual(
    values = c(
      "total_fertility_rate_tfr" = "steelblue",
      "total_live_births" = "forestgreen"
    )
  ) +
  labs(
    title = "Main Fertility Indicators",
    subtitle = "Singapore, 1960 to 2025",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )


# ============================================================
# TFR START AND END VALUES
# ============================================================

tfr_start_end <- data %>%
  filter(
    year %in% c(1960, 2025)
  ) %>%
  select(
    year,
    total_fertility_rate_tfr
  )


print(tfr_start_end)


# ============================================================
# LIVE-BIRTHS START AND END VALUES
# ============================================================

birth_start_end <- data %>%
  filter(
    year %in% c(1960, 2025)
  ) %>%
  select(
    year,
    total_live_births
  )


print(birth_start_end)


# ============================================================
# TFR PERCENTAGE CHANGE
# ============================================================

tfr_1960 <- data %>%
  filter(year == 1960) %>%
  pull(total_fertility_rate_tfr)


tfr_2025 <- data %>%
  filter(year == 2025) %>%
  pull(total_fertility_rate_tfr)


tfr_change <- (
  (tfr_2025 - tfr_1960) /
    tfr_1960
) * 100


print(
  paste(
    "Percentage change in TFR:",
    round(tfr_change, 2),
    "%"
  )
)


# ============================================================
# CORRELATION ANALYSIS
# ============================================================

# Select all numeric columns except Year

numeric_data <- data %>%
  select(
    -year
  )


correlation_matrix <- cor(
  numeric_data,
  use = "pairwise.complete.obs"
)


print(
  round(correlation_matrix, 2)
)


# Save correlation matrix

write.csv(
  correlation_matrix,
  "Output/Correlation_Matrix.csv"
)

### ##############################################ARIMA ---- HK

# Convert wide format to long format
births_long <- data |>
  pivot_longer(
    cols = -year,
    names_to = "DataSeries",
    values_to = "Value"
  ) |>
  mutate(
    DataSeries = recode(
      DataSeries,
      "total_fertility_rate_tfr" = "Total Fertility Rate (TFR)",
      "age_15_19" = "15 - 19 Years",
      "age_20_24" = "20 - 24 Years",
      "age_25_29" = "25 - 29 Years",
      "age_30_34" = "30 - 34 Years",
      "age_35_39" = "35 - 39 Years",
      "age_40_44" = "40 - 44 Years",
      "age_45_49" = "45 - 49 Years",
      "chinese" = "Chinese",
      "malays" = "Malays",
      "indians" = "Indians",
      "gross_reproduction_rate" = "Gross Reproduction Rate",
      "net_reproduction_rate" = "Net Reproduction Rate",
      "crude_birth_rate" = "Crude Birth Rate",
      "total_live_births" = "Total Live-Births",
      "resident_live_births" = "Resident Live-Births",
      "citizen_live_births" = "Citizen Live-Births"
    ),
    Year = as.integer(year),
    Value = as.numeric(Value)
  ) |>
  select(Year, DataSeries, Value) |>
  arrange(Year, DataSeries)

# Check the result
print(dim(births_long))  # Should be 1122 rows and 3 columns
head(births_long)

# Save in the same folder as the R script
write.csv(
  births_long,
  "Singapore_Births_Fertility_Long_1960_2025_new.csv",
  row.names = FALSE,
  na = ""
)
# ---- 5. Select Total Fertility Rate ----------------------------------------

tfr_data <- births_long |>
  filter(
    DataSeries == "Total Fertility Rate (TFR)",
    !is.na(Value)
  ) |>
  arrange(Year)

if (nrow(tfr_data) == 0) {
  stop("Total Fertility Rate (TFR) was not found.")
}

cat("\nTFR data:\n")
print(tfr_data)

# ---- 6. Divide training and testing data -----------------------------------

train_data <- tfr_data |>
  filter(
    Year >= 1960,
    Year <= 2012
  )

test_data <- tfr_data |>
  filter(
    Year >= 2013,
    Year <= 2025
  )

cat("\nTraining observations:", nrow(train_data), "\n")
cat("Testing observations:", nrow(test_data), "\n")

if (nrow(train_data) != 53) {
  warning(
    paste(
      "Expected 53 training observations but found",
      nrow(train_data)
    )
  )
}

if (nrow(test_data) != 13) {
  warning(
    paste(
      "Expected 13 testing observations but found",
      nrow(test_data)
    )
  )
}

# ---- 7. Create annual time-series objects ----------------------------------

train_ts <- ts(
  train_data$Value,
  start = 1960,
  end = 2012,
  frequency = 1
)

test_ts <- ts(
  test_data$Value,
  start = 2013,
  end = 2025,
  frequency = 1
)

# frequency = 1 because observations are annual.
# Therefore, within-year seasonality is not applicable.

# ============================================================================
# BOX-JENKINS STAGE 1: MODEL IDENTIFICATION
# ============================================================================


# ---- 8. Plot the training time series --------------------------------------

p_training <- ggplot(
  train_data,
  aes(x = Year, y = Value)
) +
  geom_line(
    colour = "#156082",
    linewidth = 1
  ) +
  geom_point(
    colour = "#156082",
    size = 1.8
  ) +
  geom_hline(
    yintercept = 2.1,
    linetype = "dashed",
    colour = "#C00000"
  ) +
  annotate(
    "text",
    x = 1962,
    y = 2.18,
    label = "Replacement level = 2.1",
    hjust = 0,
    colour = "#C00000",
    size = 3.5
  ) +
  labs(
    title = "TFR training data",
    subtitle = "Singapore, 1960-2012",
    x = "Year",
    y = "Children per woman",
    caption = "Training period used to estimate the ARIMA model"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

model_dir <- file.path(getwd(), "Box_Jenkins_ARIMA_TFR_M1")

dir.create(
  model_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

plot_dir <- file.path(getwd(), "Box_Jenkins_ARIMA_TFR_M1/plot")

dir.create(
  plot_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

ggsave(
  filename = file.path(
    plot_dir,
    "01_TFR_training_series.png"
  ),
  plot = p_training,
  width = 9,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

# ---- 9. Test stationarity --------------------------------------------------

# KPSS test:
# Null hypothesis = the series is stationary
kpss_result <- tseries::kpss.test(
  train_ts,
  null = "Level"
)

# ADF test:
# Null hypothesis = the series has a unit root/non-stationarity
adf_result <- tseries::adf.test(
  train_ts,
  alternative = "stationary"
)

cat("\nKPSS test for original TFR:\n")
print(kpss_result)

cat("\nADF test for original TFR:\n")
print(adf_result)

# ---- 10. Determine differencing order --------------------------------------

d_required_kpss <- forecast::ndiffs(
  train_ts,
  test = "kpss"
)

d_required_adf <- forecast::ndiffs(
  train_ts,
  test = "adf"
)

cat("\nDifferencing suggested by KPSS:", d_required_kpss, "\n")
cat("Differencing suggested by ADF:", d_required_adf, "\n")

# KPSS and ADF disagree for this short annual series.  Use first differencing
# for the diagnostic plots, then compare d = 1 and d = 2 candidate models
# using test-period accuracy and residual diagnostics below.
d_required <- 1

cat("Differencing order used for diagnostic plots, d =", d_required, "\n")
# ---- 11. Create differenced series -----------------------------------------

if (d_required > 0) {
  
  differenced_ts <- diff(
    train_ts,
    differences = d_required
  )
  
} else {
  
  differenced_ts <- train_ts
  
}

# ---- 12. Plot differenced series -------------------------------------------

differenced_years <- as.numeric(
  time(differenced_ts)
)

differenced_df <- tibble(
  Year = differenced_years,
  Difference = as.numeric(differenced_ts)
)

p_difference <- ggplot(
  differenced_df,
  aes(x = Year, y = Difference)
) +
  geom_hline(
    yintercept = 0,
    colour = "grey45"
  ) +
  geom_line(
    colour = "#7030A0",
    linewidth = 0.9
  ) +
  geom_point(
    colour = "#7030A0",
    size = 1.5
  ) +
  labs(
    title = paste(
      "Differenced TFR series: d =",
      d_required
    ),
    subtitle = "Differencing is used to obtain a stationary series",
    x = "Year",
    y = "Differenced TFR"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

ggsave(
  filename = file.path(
    plot_dir,
    "02_TFR_differenced_series.png"
  ),
  plot = p_difference,
  width = 9,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

# ---- 13. ACF of the original series ----------------------------------------

p_acf_original <- forecast::ggAcf(
  train_ts,
  lag.max = 15
) +
  labs(
    title = "ACF of original TFR",
    subtitle = "Used to examine autocorrelation and non-stationarity"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

ggsave(
  filename = file.path(
    plot_dir,
    "03_ACF_original_TFR.png"
  ),
  plot = p_acf_original,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# ---- 14. PACF of the original series ---------------------------------------

p_pacf_original <- forecast::ggPacf(
  train_ts,
  lag.max = 15
) +
  labs(
    title = "PACF of original TFR",
    subtitle = "Used to examine possible autoregressive terms"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

ggsave(
  filename = file.path(
    plot_dir,
    "04_PACF_original_TFR.png"
  ),
  plot = p_pacf_original,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# ---- 15. ACF of the differenced series -------------------------------------

p_acf_difference <- forecast::ggAcf(
  differenced_ts,
  lag.max = 15
) +
  labs(
    title = "ACF of differenced TFR",
    subtitle = paste("Differencing order: d =", d_required)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

ggsave(
  filename = file.path(
    plot_dir,
    "05_ACF_differenced_TFR.png"
  ),
  plot = p_acf_difference,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# ---- 16. PACF of the differenced series ------------------------------------

p_pacf_difference <- forecast::ggPacf(
  differenced_ts,
  lag.max = 15
) +
  labs(
    title = "PACF of differenced TFR",
    subtitle = paste("Differencing order: d =", d_required)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

ggsave(
  filename = file.path(
    plot_dir,
    "06_PACF_differenced_TFR.png"
  ),
  plot = p_pacf_difference,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# ---- 17. Fit and compare candidate ARIMA models ----------------------------

# The original automatic search selected ARIMA(0,2,1), but it searched only
# d = 2 models.  The d = 1 candidates below are included to check possible
# over-differencing and to compare genuine test-period forecasting performance.
candidate_specs <- list(
  "ARIMA(0,1,1)" = list(
    order = c(0, 1, 1), drift = FALSE
  ),
  "ARIMA(0,1,1) with drift" = list(
    order = c(0, 1, 1), drift = TRUE
  ),
  "ARIMA(1,1,0)" = list(
    order = c(1, 1, 0), drift = FALSE
  ),
  "ARIMA(1,1,0) with drift" = list(
    order = c(1, 1, 0), drift = TRUE
  ),
  "ARIMA(1,1,1)" = list(
    order = c(1, 1, 1), drift = FALSE
  ),
  "ARIMA(1,1,1) with drift" = list(
    order = c(1, 1, 1), drift = TRUE
  ),
  "ARIMA(0,2,1)" = list(
    order = c(0, 2, 1), drift = FALSE
  )
)

candidate_models <- purrr::map(
  candidate_specs,
  ~ forecast::Arima(
    train_ts,
    order = .x$order,
    include.drift = .x$drift,
    method = "ML"
  )
)

candidate_forecasts <- purrr::map(
  candidate_models,
  ~ forecast::forecast(
    .x,
    h = length(test_ts),
    level = c(80, 95)
  )
)

candidate_comparison <- purrr::imap_dfr(
  candidate_models,
  function(model, candidate_name) {
    candidate_order <- forecast::arimaorder(model)
    candidate_forecast <- candidate_forecasts[[candidate_name]]
    actual_values <- as.numeric(test_ts)
    forecast_values <- as.numeric(candidate_forecast$mean)
    errors <- actual_values - forecast_values
    fitted_parameters <- candidate_order["p"] + candidate_order["q"]
    candidate_lag <- min(
      max(10, fitted_parameters + 3),
      length(residuals(model)) - 1
    )
    candidate_ljung <- Box.test(
      residuals(model),
      lag = candidate_lag,
      type = "Ljung-Box",
      fitdf = fitted_parameters
    )

    tibble(
      Model = candidate_name,
      p = unname(candidate_order["p"]),
      d = unname(candidate_order["d"]),
      q = unname(candidate_order["q"]),
      Drift = "drift" %in% names(coef(model)),
      AIC = AIC(model),
      AICc = model$aicc,
      BIC = BIC(model),
      ME = mean(errors, na.rm = TRUE),
      MAE = mean(abs(errors), na.rm = TRUE),
      RMSE = sqrt(mean(errors^2, na.rm = TRUE)),
      MAPE = mean(abs(errors / actual_values), na.rm = TRUE) * 100,
      LjungBoxLag = candidate_lag,
      LjungBoxPValue = candidate_ljung$p.value,
      WhiteNoiseResiduals = candidate_ljung$p.value > 0.05
    )
  }
) |>
  arrange(RMSE, MAE, AICc)

cat("\nCandidate ARIMA model comparison:\n")
print(candidate_comparison)

# Select the lowest-test-RMSE model among candidates whose residuals are
# consistent with white noise.  If none passes Ljung-Box, use lowest RMSE and
# print a warning so the result is not silently treated as adequate.
eligible_candidates <- candidate_comparison |>
  filter(WhiteNoiseResiduals)

if (nrow(eligible_candidates) > 0) {
  selected_model_name <- eligible_candidates$Model[1]
} else {
  selected_model_name <- candidate_comparison$Model[1]
  warning(
    "No candidate passed the Ljung-Box check; selected lowest-RMSE model."
  )
}

arima_model <- candidate_models[[selected_model_name]]
model_name <- selected_model_name
selected_order <- forecast::arimaorder(arima_model)
p_order <- unname(selected_order["p"])
d_order <- unname(selected_order["d"])
q_order <- unname(selected_order["q"])

write_csv(
  candidate_comparison,
  file.path(
    model_dir,
    "TFR_ARIMA_candidate_model_comparison.csv"
  )
)

cat("\nSelected ARIMA model based on test RMSE and residual checks:\n")
print(arima_model)

cat("\nFull model summary:\n")
print(summary(arima_model))

# ---- 19. Save model coefficients -------------------------------------------

coefficient_table <- tibble(
  Parameter = names(coef(arima_model)),
  Estimate = as.numeric(coef(arima_model)),
  StandardError = sqrt(
    diag(arima_model$var.coef)
  )
) |>
  mutate(
    ZStatistic = Estimate / StandardError,
    PValue = 2 * pnorm(
      abs(ZStatistic),
      lower.tail = FALSE
    )
  )

write_csv(
  coefficient_table,
  file.path(
    model_dir,
    "ARIMA_model_coefficients.csv"
  )
)

# ============================================================================
# BOX-JENKINS STAGE 3: DIAGNOSTIC CHECKING
# ============================================================================


# ---- 20. Extract residuals -------------------------------------------------

model_residuals <- residuals(
  arima_model
)

residual_df <- tibble(
  Year = as.numeric(time(model_residuals)),
  Residual = as.numeric(model_residuals)
)

# ---- 21. Residual time plot ------------------------------------------------

p_residuals <- ggplot(
  residual_df,
  aes(x = Year, y = Residual)
) +
  geom_hline(
    yintercept = 0,
    colour = "grey40"
  ) +
  geom_line(
    colour = "#C55A11",
    linewidth = 0.8
  ) +
  geom_point(
    colour = "#C55A11",
    size = 1.4
  ) +
  labs(
    title = paste("Residuals from", model_name),
    subtitle = "Residuals should fluctuate randomly around zero",
    x = "Year",
    y = "Residual"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

ggsave(
  filename = file.path(
    plot_dir,
    "07_ARIMA_residuals.png"
  ),
  plot = p_residuals,
  width = 9,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

# ---- 22. Residual ACF -------------------------------------------------------

p_residual_acf <- forecast::ggAcf(
  model_residuals,
  lag.max = 15
) +
  labs(
    title = paste("Residual ACF:", model_name),
    subtitle = "Significant spikes may indicate unexplained autocorrelation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

ggsave(
  filename = file.path(
    plot_dir,
    "08_ARIMA_residual_ACF.png"
  ),
  plot = p_residual_acf,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# ---- 23. Residual histogram ------------------------------------------------

p_residual_histogram <- ggplot(
  residual_df,
  aes(x = Residual)
) +
  geom_histogram(
    bins = 10,
    fill = "#5B9BD5",
    colour = "white"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "#C00000"
  ) +
  labs(
    title = paste("Residual distribution:", model_name),
    x = "Residual",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

ggsave(
  filename = file.path(
    plot_dir,
    "09_ARIMA_residual_histogram.png"
  ),
  plot = p_residual_histogram,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# ---- 24. Ljung-Box test ----------------------------------------------------

number_parameters <- p_order + q_order

ljung_lag <- min(
  max(10, number_parameters + 3),
  length(model_residuals) - 1
)

ljung_box_result <- Box.test(
  model_residuals,
  lag = ljung_lag,
  type = "Ljung-Box",
  fitdf = number_parameters
)

cat("\nLjung-Box residual test:\n")
print(ljung_box_result)

if (ljung_box_result$p.value > 0.05) {
  
  residual_conclusion <- paste(
    "The Ljung-Box p-value is",
    round(ljung_box_result$p.value, 4),
    "> 0.05. There is no strong evidence of residual",
    "autocorrelation. The residuals are consistent with white noise."
  )
  
} else {
  
  residual_conclusion <- paste(
    "The Ljung-Box p-value is",
    round(ljung_box_result$p.value, 4),
    "<= 0.05. Significant residual autocorrelation remains.",
    "The model may require further refinement."
  )
  
}

cat("\n", residual_conclusion, "\n")

# ---- 25. Standard diagnostic display ---------------------------------------

png(
  filename = file.path(
    plot_dir,
    "10_complete_residual_diagnostics.png"
  ),
  width = 2400,
  height = 1800,
  res = 300,
  bg = "white"
)

forecast::checkresiduals(
  arima_model
)

dev.off()

# ============================================================================
# BOX-JENKINS STAGE 4: FORECASTING
# ============================================================================


# ---- 26. Forecast 2013-2025 ------------------------------------------------

forecast_horizon <- nrow(test_data)

tfr_forecast <- forecast::forecast(
  arima_model,
  h = forecast_horizon,
  level = c(80, 95)
)

cat("\nForecast results:\n")
print(tfr_forecast)

# ---- 27. Create forecast table ---------------------------------------------

forecast_table <- tibble(
  Year = test_data$Year,
  Actual = test_data$Value,
  Forecast = as.numeric(tfr_forecast$mean),
  Lower80 = as.numeric(
    tfr_forecast$lower[, "80%"]
  ),
  Upper80 = as.numeric(
    tfr_forecast$upper[, "80%"]
  ),
  Lower95 = as.numeric(
    tfr_forecast$lower[, "95%"]
  ),
  Upper95 = as.numeric(
    tfr_forecast$upper[, "95%"]
  )
) |>
  mutate(
    Error = Actual - Forecast,
    AbsoluteError = abs(Error),
    SquaredError = Error^2,
    PercentageError = if_else(
      Actual != 0,
      100 * Error / Actual,
      NA_real_
    ),
    AbsolutePercentageError = abs(
      PercentageError
    )
  )

print(forecast_table)

# ---- 28. Calculate forecast accuracy ---------------------------------------

mae_value <- mean(
  forecast_table$AbsoluteError,
  na.rm = TRUE
)

rmse_value <- sqrt(
  mean(
    forecast_table$SquaredError,
    na.rm = TRUE
  )
)

mape_value <- mean(
  forecast_table$AbsolutePercentageError,
  na.rm = TRUE
)

accuracy_table <- tibble(
  DataSeries = "Total Fertility Rate (TFR)",
  Model = model_name,
  TrainStart = 1960,
  TrainEnd = 2012,
  TestStart = 2013,
  TestEnd = 2025,
  TrainingObservations = nrow(train_data),
  TestingObservations = nrow(test_data),
  AIC = AIC(arima_model),
  AICc = arima_model$aicc,
  BIC = BIC(arima_model),
  MAE = mae_value,
  RMSE = rmse_value,
  MAPE = mape_value,
  LjungBoxLag = ljung_lag,
  LjungBoxPValue = ljung_box_result$p.value,
  ResidualConclusion = residual_conclusion
)

cat("\nModel accuracy:\n")
print(accuracy_table)

# ---- 29. Forecast versus actual plot ---------------------------------------

p_forecast <- ggplot() +
  
  # 95% interval
  geom_ribbon(
    data = forecast_table,
    aes(
      x = Year,
      ymin = Lower95,
      ymax = Upper95
    ),
    fill = "#BDD7EE",
    alpha = 0.55
  ) +
  
  # 80% interval
  geom_ribbon(
    data = forecast_table,
    aes(
      x = Year,
      ymin = Lower80,
      ymax = Upper80
    ),
    fill = "#5B9BD5",
    alpha = 0.45
  ) +
  
  # Training values
  geom_line(
    data = train_data,
    aes(
      x = Year,
      y = Value,
      colour = "Training data"
    ),
    linewidth = 0.9
  ) +
  
  # Actual testing values
  geom_line(
    data = forecast_table,
    aes(
      x = Year,
      y = Actual,
      colour = "Actual test data"
    ),
    linewidth = 1
  ) +
  
  geom_point(
    data = forecast_table,
    aes(
      x = Year,
      y = Actual,
      colour = "Actual test data"
    ),
    size = 2
  ) +
  
  # Forecast values
  geom_line(
    data = forecast_table,
    aes(
      x = Year,
      y = Forecast,
      colour = "ARIMA forecast"
    ),
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  geom_point(
    data = forecast_table,
    aes(
      x = Year,
      y = Forecast,
      colour = "ARIMA forecast"
    ),
    size = 2
  ) +
  
  # Training/test boundary
  geom_vline(
    xintercept = 2012.5,
    linetype = "dotted",
    linewidth = 0.8,
    colour = "grey30"
  ) +
  
  annotate(
    "text",
    x = 2012,
    y = max(train_data$Value),
    label = "Train | Test",
    hjust = 1,
    vjust = -0.5,
    size = 3.5
  ) +
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Training data" = "#1F4E78",
      "Actual test data" = "#008000",
      "ARIMA forecast" = "#C00000"
    )
  ) +
  
  labs(
    title = paste(
      "TFR forecast using",
      model_name
    ),
    subtitle = "Training: 1960-2012 | Testing: 2013-2025",
    x = "Year",
    y = "Children per woman",
    caption = paste(
      "Dark shading: 80% prediction interval |",
      "Light shading: 95% prediction interval"
    )
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title.position = "plot",
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

ggsave(
  filename = file.path(
    plot_dir,
    "11_TFR_ARIMA_forecast_vs_actual.png"
  ),
  plot = p_forecast,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


# ---- 30. Test-period-only comparison ---------------------------------------

p_test_comparison <- forecast_table |>
  select(
    Year,
    Actual,
    Forecast
  ) |>
  pivot_longer(
    cols = c(Actual, Forecast),
    names_to = "Series",
    values_to = "Value"
  ) |>
  ggplot(
    aes(
      x = Year,
      y = Value,
      colour = Series
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  scale_colour_manual(
    values = c(
      "Actual" = "#008000",
      "Forecast" = "#C00000"
    )
  ) +
  labs(
    title = "Actual versus forecast TFR",
    subtitle = paste(
      "Testing period: 2013-2025 | Selected model:",
      model_name
    ),
    x = "Year",
    y = "Children per woman",
    colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

ggsave(
  filename = file.path(
    plot_dir,
    "12_TFR_actual_vs_forecast_2013_2025.png"
  ),
  plot = p_test_comparison,
  width = 9,
  height = 5.5,
  dpi = 300,
  bg = "white"
)


# ---- 30A. Compare every candidate forecast with actual TFR -----------------

candidate_plot_data <- purrr::imap_dfr(
  candidate_forecasts,
  function(candidate_forecast, candidate_name) {
    tibble(
      Year = test_data$Year,
      Model = candidate_name,
      Forecast = as.numeric(candidate_forecast$mean)
    )
  }
)

p_candidate_comparison <- ggplot() +
  geom_line(
    data = candidate_plot_data,
    aes(x = Year, y = Forecast, colour = Model),
    linewidth = 0.9
  ) +
  geom_point(
    data = candidate_plot_data,
    aes(x = Year, y = Forecast, colour = Model),
    size = 1.6
  ) +
  geom_line(
    data = test_data,
    aes(x = Year, y = Value, linetype = "Actual"),
    colour = "black",
    linewidth = 1.2
  ) +
  geom_point(
    data = test_data,
    aes(x = Year, y = Value),
    colour = "black",
    size = 2
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("Actual" = "solid")
  ) +
  labs(
    title = "Candidate ARIMA forecasts versus actual TFR",
    subtitle = "Testing period: 2013-2025",
    x = "Year",
    y = "Children per woman",
    colour = "Candidate model"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = file.path(
    plot_dir,
    "13_TFR_candidate_models_vs_actual.png"
  ),
  plot = p_candidate_comparison,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


# ---- 31. Save all output tables --------------------------------------------

write_csv(
  train_data,
  file.path(
    model_dir,
    "TFR_training_data_1960_2012.csv"
  )
)

write_csv(
  test_data,
  file.path(
    model_dir,
    "TFR_test_data_2013_2025.csv"
  )
)

write_csv(
  forecast_table,
  file.path(
    model_dir,
    "TFR_ARIMA_forecasts_2013_2025.csv"
  )
)

write_csv(
  accuracy_table,
  file.path(
    model_dir,
    "TFR_ARIMA_accuracy.csv"
  )
)


# ---- 32. Save stationarity results -----------------------------------------

stationarity_table <- tibble(
  Test = c(
    "KPSS",
    "ADF"
  ),
  NullHypothesis = c(
    "Series is level stationary",
    "Series has a unit root"
  ),
  Statistic = c(
    unname(kpss_result$statistic),
    unname(adf_result$statistic)
  ),
  PValue = c(
    kpss_result$p.value,
    adf_result$p.value
  )
)

write_csv(
  stationarity_table,
  file.path(
    model_dir,
    "TFR_stationarity_tests.csv"
  )
)


# ---- 33. Save diagnostic results -------------------------------------------

diagnostic_table <- tibble(
  Model = model_name,
  ResidualMean = mean(
    model_residuals,
    na.rm = TRUE
  ),
  ResidualSD = sd(
    model_residuals,
    na.rm = TRUE
  ),
  LjungBoxStatistic = unname(
    ljung_box_result$statistic
  ),
  LjungBoxLag = ljung_lag,
  LjungBoxPValue = ljung_box_result$p.value,
  Conclusion = residual_conclusion
)

write_csv(
  diagnostic_table,
  file.path(
    model_dir,
    "TFR_ARIMA_residual_diagnostics.csv"
  )
)


# ---- 34. Save fitted model as RDS -------------------------------------------

saveRDS(
  arima_model,
  file.path(
    model_dir,
    "TFR_ARIMA_fitted_model.rds"
  )
)


# ---- 35. Create a text model summary ---------------------------------------

sink(
  file.path(
    model_dir,
    "TFR_ARIMA_model_summary.txt"
  )
)

cat("BOX-JENKINS ARIMA ANALYSIS\n")
cat("==========================\n\n")

cat("Data series: Total Fertility Rate (TFR)\n")
cat("Training period: 1960-2012\n")
cat("Testing period: 2013-2025\n")
cat("Frequency: Annual\n")
cat("Seasonal model: No\n\n")

cat("Selected model:", model_name, "\n\n")

cat("MODEL SUMMARY\n")
cat("-------------\n")
print(summary(arima_model))

cat("\nSTATIONARITY TESTS\n")
cat("------------------\n")
print(stationarity_table)

cat("\nMODEL ACCURACY\n")
cat("--------------\n")
print(accuracy_table)

cat("\nCANDIDATE MODEL COMPARISON\n")
cat("--------------------------\n")
print(candidate_comparison)

cat("\nRESIDUAL DIAGNOSTICS\n")
cat("--------------------\n")
print(diagnostic_table)

cat("\nFORECASTS\n")
cat("---------\n")
print(forecast_table)

sink()


# ---- 36. Display final results ---------------------------------------------

cat("\n============================================================\n")
cat("BOX-JENKINS ARIMA ANALYSIS COMPLETED\n")
cat("============================================================\n")

cat("\nSelected model:", model_name, "\n")
cat("\nCandidate comparison ranked by test RMSE:\n")
print(candidate_comparison)
cat("MAE:", round(mae_value, 4), "\n")
cat("RMSE:", round(rmse_value, 4), "\n")
cat("MAPE:", round(mape_value, 2), "%\n")
cat(
  "Ljung-Box p-value:",
  round(ljung_box_result$p.value, 4),
  "\n"
)

cat("\n", residual_conclusion, "\n")

cat("\nAll outputs were saved in:\n")
cat(normalizePath(model_dir), "\n")

cat("\nPlots were saved in:\n")
cat(normalizePath(plot_dir), "\n")


# ---- 37. Open output folder in Windows -------------------------------------

if (.Platform$OS.type == "windows") {
  shell.exec(
    normalizePath(model_dir)
  )
}

############ - TLB #######################################################################################


# ---- 4. Select Total Live Births ------------------------------------------
# The flexible pattern accepts labels such as "Total Live-Births" and
# "Total Live Births".
tlb_data <- births_long |>
  filter(
    stringr::str_detect(
      stringr::str_to_lower(DataSeries),
      "^total\\s+live[- ]?births?$"
    ),
    !is.na(Value)
  ) |>
  arrange(Year) |>
  distinct(Year, .keep_all = TRUE)

if (nrow(tlb_data) == 0) {
  available_series <- sort(unique(births_long$DataSeries))
  stop(
    paste0(
      "Total Live-Births was not found. Available DataSeries values are:\n",
      paste(available_series, collapse = "\n")
    )
  )
}

if (anyDuplicated(tlb_data$Year) > 0) {
  stop("Duplicate years remain in the Total Live Births series.")
}

cat("\nTotal Live Births data:\n")
print(tlb_data)

# ---- 5. Divide training and testing data ----------------------------------
train_data <- tlb_data |>
  filter(Year >= 1960, Year <= 2012)

test_data <- tlb_data |>
  filter(Year >= 2013, Year <= 2025)

if (nrow(train_data) == 0 || nrow(test_data) == 0) {
  stop("Training or testing data are missing after the year split.")
}

if (anyNA(train_data$Value) || anyNA(test_data$Value)) {
  stop("Missing values exist in the selected TLB training or testing period.")
}

cat("\nTraining observations:", nrow(train_data), "\n")
cat("Testing observations:", nrow(test_data), "\n")

# ---- 6. Create annual time-series objects ---------------------------------
train_ts <- ts(
  train_data$Value,
  start = min(train_data$Year),
  frequency = 1
)

test_ts <- ts(
  test_data$Value,
  start = min(test_data$Year),
  frequency = 1
)

# frequency = 1 because observations are annual.
# Therefore, within-year seasonality is not applicable.

# ============================================================================
# BOX-JENKINS STAGE 1: MODEL IDENTIFICATION
# ============================================================================

# ---- 7. Plot the training series ------------------------------------------
p_training <- ggplot(train_data, aes(x = Year, y = Value)) +
  geom_line(colour = "#156082", linewidth = 1) +
  geom_point(colour = "#156082", size = 1.8) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total Live Births training data",
    subtitle = "Singapore, 1960-2012",
    x = "Year",
    y = "Number of live births",
    caption = "Training period used to estimate the ARIMA model"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

ggsave(
  file.path(plot_dir, "01_TLB_training_series.png"),
  p_training, width = 9, height = 5.5, dpi = 300, bg = "white"
)

# ---- 8. Stationarity tests and differencing orders ------------------------
kpss_result <- tseries::kpss.test(train_ts, null = "Level")
adf_result <- tseries::adf.test(train_ts, alternative = "stationary")

kpss_d <- forecast::ndiffs(train_ts, test = "kpss")
adf_d <- forecast::ndiffs(train_ts, test = "adf")
pp_d <- forecast::ndiffs(train_ts, test = "pp")

# Use d = 1 for diagnostic plots when at least one test recommends
# differencing. Candidate models below compare both d = 1 and d = 2.
diagnostic_d <- ifelse(max(kpss_d, adf_d, pp_d) >= 1, 1, 0)

cat("\nKPSS test for original TLB:\n")
print(kpss_result)
cat("\nADF test for original TLB:\n")
print(adf_result)
cat("\nSuggested differencing orders:\n")
print(tibble(KPSS = kpss_d, ADF = adf_d, PP = pp_d))

# ---- 9. Differenced series -------------------------------------------------
if (diagnostic_d == 0) {
  differenced_ts <- train_ts
  difference_label <- "No differencing required for diagnostic plot"
} else {
  differenced_ts <- diff(train_ts, differences = diagnostic_d)
  difference_label <- paste("Differencing order: d =", diagnostic_d)
}

differenced_data <- tibble(
  Year = as.integer(time(differenced_ts)),
  Value = as.numeric(differenced_ts)
)

p_difference <- ggplot(differenced_data, aes(Year, Value)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_line(colour = "#7030A0", linewidth = 1) +
  geom_point(colour = "#7030A0", size = 1.7) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = paste0("Differenced Total Live Births series: d = ", diagnostic_d),
    subtitle = difference_label,
    x = "Year",
    y = "Change in live births"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(plot_dir, "02_TLB_differenced_series.png"),
  p_difference, width = 9, height = 5.5, dpi = 300, bg = "white"
)

# ---- 10. ACF and PACF helper ----------------------------------------------
save_correlation_plot <- function(series, type, title, subtitle, filename) {
  correlation <- if (type == "acf") {
    forecast::Acf(series, plot = FALSE, lag.max = 15)
  } else {
    forecast::Pacf(series, plot = FALSE, lag.max = 15)
  }
  
  plot_data <- tibble(
    Lag = as.numeric(correlation$lag),
    Correlation = as.numeric(correlation$acf)
  ) |>
    filter(if (type == "acf") Lag > 0 else TRUE)
  
  bound <- qnorm(0.975) / sqrt(length(series))
  y_label <- ifelse(type == "acf", "ACF", "PACF")
  
  p <- ggplot(plot_data, aes(Lag, Correlation)) +
    geom_hline(yintercept = 0, colour = "black") +
    geom_segment(aes(xend = Lag, y = 0, yend = Correlation), linewidth = 0.6) +
    geom_hline(yintercept = c(-bound, bound), colour = "blue", linetype = "dashed") +
    scale_x_continuous(breaks = 1:15) +
    labs(title = title, subtitle = subtitle, x = "Lag", y = y_label) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
  
  ggsave(
    file.path(plot_dir, filename), p,
    width = 8, height = 5, dpi = 300, bg = "white"
  )
  
  p
}

p_acf_original <- save_correlation_plot(
  train_ts, "acf", "ACF of original Total Live Births",
  "Used to examine autocorrelation and non-stationarity",
  "03_ACF_original_TLB.png"
)

p_pacf_original <- save_correlation_plot(
  train_ts, "pacf", "PACF of original Total Live Births",
  "Used to examine possible autoregressive terms",
  "04_PACF_original_TLB.png"
)

p_acf_difference <- save_correlation_plot(
  differenced_ts, "acf", "ACF of differenced Total Live Births",
  difference_label, "05_ACF_differenced_TLB.png"
)

p_pacf_difference <- save_correlation_plot(
  differenced_ts, "pacf", "PACF of differenced Total Live Births",
  difference_label, "06_PACF_differenced_TLB.png"
)

# ============================================================================
# BOX-JENKINS STAGE 2: ESTIMATION AND MODEL SELECTION
# ============================================================================

# ---- 11. Candidate specifications -----------------------------------------
# Drift is allowed only when d = 1. The best TLB model is selected separately
# from the TFR model.
candidate_specs <- list(
  "ARIMA(0,1,0)" = list(order = c(0, 1, 0), drift = FALSE),
  "ARIMA(0,1,0) with drift" = list(order = c(0, 1, 0), drift = TRUE),
  "ARIMA(0,1,1)" = list(order = c(0, 1, 1), drift = FALSE),
  "ARIMA(0,1,1) with drift" = list(order = c(0, 1, 1), drift = TRUE),
  "ARIMA(1,1,0)" = list(order = c(1, 1, 0), drift = FALSE),
  "ARIMA(1,1,0) with drift" = list(order = c(1, 1, 0), drift = TRUE),
  "ARIMA(1,1,1)" = list(order = c(1, 1, 1), drift = FALSE),
  "ARIMA(1,1,1) with drift" = list(order = c(1, 1, 1), drift = TRUE),
  "ARIMA(2,1,0)" = list(order = c(2, 1, 0), drift = FALSE),
  "ARIMA(0,1,2)" = list(order = c(0, 1, 2), drift = FALSE),
  "ARIMA(0,2,1)" = list(order = c(0, 2, 1), drift = FALSE)
)

fit_candidate <- function(specification) {
  tryCatch(
    forecast::Arima(
      train_ts,
      order = specification$order,
      include.drift = specification$drift,
      method = "ML"
    ),
    error = function(e) NULL
  )
}

candidate_models <- purrr::map(candidate_specs, fit_candidate)
candidate_models <- candidate_models[!purrr::map_lgl(candidate_models, is.null)]

# Add the exhaustive automatic model as another independently estimated
# candidate. It may select a different order for TLB.
auto_model <- forecast::auto.arima(
  train_ts,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE,
  allowdrift = TRUE,
  allowmean = TRUE,
  ic = "aicc"
)

auto_order <- forecast::arimaorder(auto_model)
auto_has_drift <- "drift" %in% names(coef(auto_model))
auto_name <- paste0(
  "Auto ARIMA(", auto_order["p"], ",", auto_order["d"], ",",
  auto_order["q"], ")", ifelse(auto_has_drift, " with drift", "")
)

candidate_models[[auto_name]] <- auto_model

# Remove exact duplicate model names/orders so comparison is easy to read.
model_signature <- purrr::map_chr(candidate_models, function(model) {
  ord <- forecast::arimaorder(model)
  paste(ord["p"], ord["d"], ord["q"], "drift" %in% names(coef(model)))
})
candidate_models <- candidate_models[!duplicated(model_signature)]

# ---- 12. Forecast every candidate and calculate accuracy ------------------
candidate_forecasts <- purrr::map(
  candidate_models,
  ~ forecast::forecast(.x, h = length(test_ts), level = c(80, 95))
)

candidate_comparison <- purrr::imap_dfr(
  candidate_models,
  function(model, candidate_name) {
    model_order <- forecast::arimaorder(model)
    model_forecast <- candidate_forecasts[[candidate_name]]
    actual <- as.numeric(test_ts)
    predicted <- as.numeric(model_forecast$mean)
    errors <- actual - predicted
    fitdf <- unname(model_order["p"] + model_order["q"])
    test_lag <- min(max(10, fitdf + 3), length(residuals(model)) - 1)
    ljung <- Box.test(
      residuals(model), lag = test_lag,
      type = "Ljung-Box", fitdf = fitdf
    )
    
    tibble(
      Model = candidate_name,
      p = unname(model_order["p"]),
      d = unname(model_order["d"]),
      q = unname(model_order["q"]),
      Drift = "drift" %in% names(coef(model)),
      AIC = AIC(model),
      AICc = model$aicc,
      BIC = BIC(model),
      ME = mean(errors),
      MAE = mean(abs(errors)),
      RMSE = sqrt(mean(errors^2)),
      MAPE = mean(abs(errors / actual)) * 100,
      LjungBoxLag = test_lag,
      LjungBoxPValue = ljung$p.value,
      WhiteNoiseResiduals = ljung$p.value > 0.05
    )
  }
) |>
  arrange(RMSE, MAE, AICc)

cat("\nCandidate TLB ARIMA model comparison:\n")
print(candidate_comparison)

# Select lowest test RMSE among models passing the residual test.
eligible_candidates <- candidate_comparison |>
  filter(WhiteNoiseResiduals)

if (nrow(eligible_candidates) > 0) {
  selected_model_name <- eligible_candidates$Model[1]
} else {
  selected_model_name <- candidate_comparison$Model[1]
  warning("No candidate passed Ljung-Box; lowest-RMSE model was selected.")
}

arima_model <- candidate_models[[selected_model_name]]
tfr_forecast <- NULL  # Prevent accidental reuse of an old TFR object.
tlb_forecast <- candidate_forecasts[[selected_model_name]]
model_name <- selected_model_name
selected_order <- forecast::arimaorder(arima_model)
p_order <- unname(selected_order["p"])
d_order <- unname(selected_order["d"])
q_order <- unname(selected_order["q"])
has_drift <- "drift" %in% names(coef(arima_model))

cat("\nSelected TLB model:\n")
print(arima_model)
print(summary(arima_model))

# ---- 13. Coefficients ------------------------------------------------------
coefficient_estimates <- as.numeric(coef(arima_model))
coefficient_se <- sqrt(diag(arima_model$var.coef))

coefficient_table <- tibble(
  Parameter = names(coef(arima_model)),
  Estimate = coefficient_estimates,
  StandardError = coefficient_se,
  ZStatistic = coefficient_estimates / coefficient_se,
  PValue = 2 * pnorm(-abs(ZStatistic))
)

# ============================================================================
# BOX-JENKINS STAGE 3: RESIDUAL DIAGNOSTICS
# ============================================================================

# ---- 14. Residual statistics and Ljung-Box test ----------------------------
model_residuals <- as.numeric(residuals(arima_model))
residual_years <- as.integer(time(residuals(arima_model)))

residual_data <- tibble(
  Year = residual_years,
  Residual = model_residuals
)

fitdf <- p_order + q_order
ljung_lag <- min(max(10, fitdf + 3), length(model_residuals) - 1)
ljung_box_result <- Box.test(
  model_residuals,
  lag = ljung_lag,
  type = "Ljung-Box",
  fitdf = fitdf
)

residual_conclusion <- if (ljung_box_result$p.value > 0.05) {
  paste0(
    "The Ljung-Box p-value is ",
    round(ljung_box_result$p.value, 4),
    " > 0.05. There is no strong evidence of residual autocorrelation. ",
    "The residuals are consistent with white noise."
  )
} else {
  paste0(
    "The Ljung-Box p-value is ",
    round(ljung_box_result$p.value, 4),
    " <= 0.05. Residual autocorrelation may remain."
  )
}

cat("\n", residual_conclusion, "\n", sep = "")

model_label <- paste0(
  "ARIMA(", p_order, ",", d_order, ",", q_order, ")",
  ifelse(has_drift, " with drift", "")
)

# ---- 15. Residual plots ----------------------------------------------------
p_residuals <- ggplot(residual_data, aes(Year, Residual)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_line(colour = "#C65911", linewidth = 1) +
  geom_point(colour = "#C65911", size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = paste("Residuals from", model_label),
    subtitle = "Residuals should fluctuate randomly around zero",
    x = "Year", y = "Residual"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(plot_dir, "07_TLB_ARIMA_residuals.png"),
  p_residuals, width = 9, height = 5.5, dpi = 300, bg = "white"
)

residual_acf <- forecast::Acf(
  model_residuals, plot = FALSE,
  lag.max = min(15, length(model_residuals) - 1)
)

residual_acf_data <- tibble(
  Lag = as.numeric(residual_acf$lag),
  ACF = as.numeric(residual_acf$acf)
) |>
  filter(Lag > 0)

acf_bound <- qnorm(0.975) / sqrt(length(model_residuals))

p_residual_acf <- ggplot(residual_acf_data, aes(Lag, ACF)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_segment(aes(xend = Lag, y = 0, yend = ACF), linewidth = 0.6) +
  geom_hline(
    yintercept = c(-acf_bound, acf_bound),
    colour = "blue", linetype = "dashed"
  ) +
  scale_x_continuous(breaks = seq_len(max(residual_acf_data$Lag))) +
  labs(
    title = paste("Residual ACF:", model_label),
    subtitle = "Significant spikes may indicate unexplained autocorrelation",
    x = "Lag", y = "ACF"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(plot_dir, "08_TLB_ARIMA_residual_ACF.png"),
  p_residual_acf, width = 8, height = 5, dpi = 300, bg = "white"
)

p_residual_histogram <- ggplot(residual_data, aes(Residual)) +
  geom_histogram(bins = 10, fill = "#5B9BD5", colour = "white") +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = paste("Residual distribution:", model_label),
    x = "Residual", y = "Frequency"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(plot_dir, "09_TLB_ARIMA_residual_histogram.png"),
  p_residual_histogram, width = 8, height = 5, dpi = 300, bg = "white"
)

p_complete_diagnostics <- (
  p_residuals / (p_residual_acf | p_residual_histogram)
) + patchwork::plot_layout(heights = c(1, 1))

ggsave(
  file.path(plot_dir, "10_TLB_complete_residual_diagnostics.png"),
  p_complete_diagnostics, width = 11, height = 8, dpi = 300, bg = "white"
)

# ============================================================================
# BOX-JENKINS STAGE 4: FORECAST AND EVALUATION
# ============================================================================

# ---- 16. Forecast table and error measures --------------------------------
forecast_table <- tibble(
  Year = test_data$Year,
  Actual = as.numeric(test_ts),
  Forecast = as.numeric(tlb_forecast$mean),
  Lower80 = as.numeric(tlb_forecast$lower[, "80%"]),
  Upper80 = as.numeric(tlb_forecast$upper[, "80%"]),
  Lower95 = as.numeric(tlb_forecast$lower[, "95%"]),
  Upper95 = as.numeric(tlb_forecast$upper[, "95%"])
) |>
  mutate(
    Error = Actual - Forecast,
    AbsoluteError = abs(Error),
    SquaredError = Error^2,
    PercentageError = if_else(Actual != 0, 100 * Error / Actual, NA_real_),
    AbsolutePercentageError = abs(PercentageError)
  )

mae_value <- mean(forecast_table$AbsoluteError, na.rm = TRUE)
rmse_value <- sqrt(mean(forecast_table$SquaredError, na.rm = TRUE))
mape_value <- mean(forecast_table$AbsolutePercentageError, na.rm = TRUE)
me_value <- mean(forecast_table$Error, na.rm = TRUE)

accuracy_table <- tibble(
  DataSeries = "Total Live-Births",
  Model = model_name,
  TrainStart = min(train_data$Year),
  TrainEnd = max(train_data$Year),
  TestStart = min(test_data$Year),
  TestEnd = max(test_data$Year),
  TrainingObservations = nrow(train_data),
  TestingObservations = nrow(test_data),
  AIC = AIC(arima_model),
  AICc = arima_model$aicc,
  BIC = BIC(arima_model),
  ME = me_value,
  MAE = mae_value,
  RMSE = rmse_value,
  MAPE = mape_value,
  LjungBoxLag = ljung_lag,
  LjungBoxPValue = ljung_box_result$p.value,
  ResidualConclusion = residual_conclusion
)

cat("\nTLB forecast results:\n")
print(forecast_table)
cat("\nTLB accuracy results:\n")
print(accuracy_table)

# ---- 17. Full forecast-versus-actual plot ---------------------------------
history_data <- bind_rows(
  train_data |> transmute(Year, Value, Period = "Training data"),
  test_data |> transmute(Year, Value, Period = "Actual test data")
)

p_forecast <- ggplot() +
  geom_ribbon(
    data = forecast_table,
    aes(Year, ymin = Lower95, ymax = Upper95),
    fill = "#BDD7EE", alpha = 0.55
  ) +
  geom_ribbon(
    data = forecast_table,
    aes(Year, ymin = Lower80, ymax = Upper80),
    fill = "#5B9BD5", alpha = 0.55
  ) +
  geom_line(
    data = train_data, aes(Year, Value, colour = "Training data"),
    linewidth = 1
  ) +
  geom_line(
    data = test_data, aes(Year, Value, colour = "Actual test data"),
    linewidth = 1
  ) +
  geom_point(
    data = test_data, aes(Year, Value, colour = "Actual test data"),
    size = 2
  ) +
  geom_line(
    data = forecast_table, aes(Year, Forecast, colour = "ARIMA forecast"),
    linewidth = 1
  ) +
  geom_point(
    data = forecast_table, aes(Year, Forecast, colour = "ARIMA forecast"),
    size = 2
  ) +
  geom_vline(xintercept = 2012.5, linetype = "dotted", linewidth = 0.8) +
  scale_colour_manual(values = c(
    "Training data" = "#1F4E79",
    "Actual test data" = "green4",
    "ARIMA forecast" = "#C00000"
  )) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = paste("Total Live Births forecast using", model_label),
    subtitle = "Training: 1960-2012 | Testing: 2013-2025",
    x = "Year", y = "Number of live births", colour = NULL,
    caption = "Dark shading: 80% prediction interval | Light shading: 95% prediction interval"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 1)
  )

ggsave(
  file.path(plot_dir, "11_TLB_ARIMA_forecast_vs_actual.png"),
  p_forecast, width = 11, height = 7, dpi = 300, bg = "white"
)

# ---- 18. Test-period actual-versus-forecast plot ---------------------------
p_test_comparison <- ggplot() +
  geom_line(
    data = forecast_table, aes(Year, Actual, colour = "Actual"),
    linewidth = 1
  ) +
  geom_point(
    data = forecast_table, aes(Year, Actual, colour = "Actual"), size = 2
  ) +
  geom_line(
    data = forecast_table, aes(Year, Forecast, colour = "Forecast"),
    linewidth = 1
  ) +
  geom_point(
    data = forecast_table, aes(Year, Forecast, colour = "Forecast"), size = 2
  ) +
  scale_colour_manual(values = c("Actual" = "green4", "Forecast" = "#C00000")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Actual versus forecast Total Live Births",
    subtitle = paste("Testing period: 2013-2025 | Selected model:", model_label),
    x = "Year", y = "Number of live births", colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave(
  file.path(plot_dir, "12_TLB_actual_vs_forecast_2013_2025.png"),
  p_test_comparison, width = 9, height = 5.5, dpi = 300, bg = "white"
)

# ---- 19. Plot all candidate forecasts -------------------------------------
candidate_plot_data <- purrr::imap_dfr(
  candidate_forecasts,
  ~ tibble(
    Year = test_data$Year,
    Model = .y,
    Forecast = as.numeric(.x$mean)
  )
)

p_candidate_comparison <- ggplot() +
  geom_line(
    data = candidate_plot_data,
    aes(Year, Forecast, colour = Model), linewidth = 0.8
  ) +
  geom_point(
    data = candidate_plot_data,
    aes(Year, Forecast, colour = Model), size = 1.3
  ) +
  geom_line(
    data = forecast_table,
    aes(Year, Actual, colour = "Actual"), linewidth = 1.1
  ) +
  geom_point(
    data = forecast_table,
    aes(Year, Actual, colour = "Actual"), size = 2
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Candidate ARIMA forecasts versus actual Total Live Births",
    subtitle = "Testing period: 2013-2025",
    x = "Year", y = "Number of live births", colour = "Candidate model"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave(
  file.path(plot_dir, "13_TLB_candidate_models_vs_actual.png"),
  p_candidate_comparison, width = 11, height = 7, dpi = 300, bg = "white"
)

# ---- 20. Save all output tables and the fitted model -----------------------
write_csv(train_data, file.path(model_dir, "TLB_training_data_1960_2012.csv"))
write_csv(test_data, file.path(model_dir, "TLB_test_data_2013_2025.csv"))
write_csv(
  forecast_table,
  file.path(model_dir, "TLB_ARIMA_forecasts_2013_2025.csv")
)
write_csv(accuracy_table, file.path(model_dir, "TLB_ARIMA_accuracy.csv"))
write_csv(
  candidate_comparison,
  file.path(model_dir, "TLB_ARIMA_candidate_model_comparison.csv")
)
write_csv(
  coefficient_table,
  file.path(model_dir, "TLB_ARIMA_model_coefficients.csv")
)

stationarity_table <- tibble(
  Test = c("KPSS", "ADF"),
  NullHypothesis = c("Series is level stationary", "Series has a unit root"),
  Statistic = c(unname(kpss_result$statistic), unname(adf_result$statistic)),
  PValue = c(kpss_result$p.value, adf_result$p.value),
  SuggestedDifferenceKPSS = c(kpss_d, NA_integer_),
  SuggestedDifferenceADF = c(NA_integer_, adf_d)
)

write_csv(
  stationarity_table,
  file.path(model_dir, "TLB_stationarity_tests.csv")
)

residual_diagnostics <- tibble(
  Model = model_name,
  ResidualMean = mean(model_residuals, na.rm = TRUE),
  ResidualSD = sd(model_residuals, na.rm = TRUE),
  LjungBoxStatistic = unname(ljung_box_result$statistic),
  LjungBoxLag = ljung_lag,
  LjungBoxPValue = ljung_box_result$p.value,
  Conclusion = residual_conclusion
)

write_csv(
  residual_diagnostics,
  file.path(model_dir, "TLB_ARIMA_residual_diagnostics.csv")
)

saveRDS(arima_model, file.path(model_dir, "TLB_ARIMA_fitted_model.rds"))

summary_file <- file.path(model_dir, "TLB_ARIMA_model_summary.txt")
sink(summary_file)
cat("TOTAL LIVE BIRTHS ARIMA MODEL SUMMARY\n")
cat("=====================================\n\n")
cat("Input file:", input_file, "\n")
cat("Selected model:", model_name, "\n")
cat("Model label:", model_label, "\n\n")
print(summary(arima_model))
cat("\nForecast accuracy:\n")
print(accuracy_table)
cat("\nResidual conclusion:\n", residual_conclusion, "\n")
sink()

# ---- 21. Final console summary --------------------------------------------
cat("\n============================================================\n")
cat("TOTAL LIVE BIRTHS ARIMA ANALYSIS COMPLETED\n")
cat("============================================================\n")
cat("Selected model:", model_name, "\n")
cat("MAE:", round(mae_value, 2), "births\n")
cat("RMSE:", round(rmse_value, 2), "births\n")
cat("MAPE:", round(mape_value, 2), "%\n")
cat("Ljung-Box p-value:", round(ljung_box_result$p.value, 4), "\n")
cat("All outputs were saved in:\n", normalizePath(model_dir), "\n")

# Open the output directory on Windows when running interactively.
if (interactive() && .Platform$OS.type == "windows") {
  shell.exec(normalizePath(model_dir))
}

######################## LOG APPLIED #######################

# ================================================================
# LOGARITHMIC ARIMA MODEL FOR TOTAL FERTILITY RATE
# Model: ARIMA(1,1,1) with drift
# Training: 1960-2012
# Testing:  2013-2025
# ================================================================

# 1. Load packages ------------------------------------------------

library(tidyverse)
library(forecast)
library(tseries)
library(patchwork)


# 2. Create output folder -----------------------------------------

output_dir <- "Box_Jenkins_ARIMA_TFR_Log"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# 3. Read data -----------------------------------------------------

# Change the filename if necessary
file_name <- "Singapore_Births_Fertility_Long_1960_2025_new.csv"

birth_data <- read.csv(
  file_name,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

names(birth_data) <- trimws(names(birth_data))


# 4. Select TFR ----------------------------------------------------

tfr_data <- births_long %>%
  filter(
    DataSeries %in% c(
      "Total Fertility Rate (TFR)",
      "Total Fertility Rate",
      "TFR"
    )
  ) %>%
  transmute(
    Year = as.integer(Year),
    TFR = as.numeric(Value)
  ) %>%
  filter(
    !is.na(Year),
    !is.na(TFR)
  ) %>%
  arrange(Year)

if (nrow(tfr_data) == 0) {
  stop("TFR data were not found. Check the DataSeries name.")
}

if (any(tfr_data$TFR <= 0)) {
  stop("Log transformation requires all TFR values to be above zero.")
}


# 5. Training and testing data ------------------------------------

train_data <- tfr_data %>%
  filter(Year >= 1960, Year <= 2012)

test_data <- tfr_data %>%
  filter(Year >= 2013, Year <= 2025)

train_tfr <- ts(
  train_data$TFR,
  start = 1960,
  frequency = 1
)

test_tfr <- ts(
  test_data$TFR,
  start = 2013,
  frequency = 1
)

forecast_horizon <- length(test_tfr)


# 6. Apply logarithmic transformation -----------------------------

log_train_tfr <- log(train_tfr)


# 7. Plot the log-transformed series -------------------------------

log_training_df <- train_data %>%
  mutate(
    LogTFR = log(TFR)
  )

log_series_plot <- ggplot(
  log_training_df,
  aes(x = Year, y = LogTFR)
) +
  geom_line(
    colour = "#7A2FA3",
    linewidth = 1
  ) +
  geom_point(
    colour = "#7A2FA3",
    size = 2
  ) +
  labs(
    title = "Log-transformed TFR training series",
    subtitle = "Singapore, 1960-2012",
    x = "Year",
    y = "Log TFR"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = file.path(
    output_dir,
    "01_log_TFR_training_series.png"
  ),
  plot = log_series_plot,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


# 8. Stationarity tests -------------------------------------------

log_adf_test <- adf.test(log_train_tfr)
log_kpss_test <- kpss.test(log_train_tfr)

differenced_log_tfr <- diff(log_train_tfr)

differenced_adf_test <- adf.test(differenced_log_tfr)
differenced_kpss_test <- kpss.test(differenced_log_tfr)

stationarity_results <- data.frame(
  Series = c(
    "Log TFR",
    "Log TFR",
    "Differenced log TFR",
    "Differenced log TFR"
  ),
  Test = c(
    "ADF",
    "KPSS",
    "ADF",
    "KPSS"
  ),
  Statistic = c(
    as.numeric(log_adf_test$statistic),
    as.numeric(log_kpss_test$statistic),
    as.numeric(differenced_adf_test$statistic),
    as.numeric(differenced_kpss_test$statistic)
  ),
  PValue = c(
    log_adf_test$p.value,
    log_kpss_test$p.value,
    differenced_adf_test$p.value,
    differenced_kpss_test$p.value
  )
)

write.csv(
  stationarity_results,
  file.path(
    output_dir,
    "TFR_log_stationarity_tests.csv"
  ),
  row.names = FALSE
)


# 9. Plot differenced log TFR -------------------------------------

differenced_data <- data.frame(
  Year = train_data$Year[-1],
  DifferencedLogTFR = as.numeric(differenced_log_tfr)
)

differenced_plot <- ggplot(
  differenced_data,
  aes(x = Year, y = DifferencedLogTFR)
) +
  geom_hline(
    yintercept = 0,
    colour = "grey40"
  ) +
  geom_line(
    colour = "#7030A0",
    linewidth = 1
  ) +
  geom_point(
    colour = "#7030A0",
    size = 2
  ) +
  labs(
    title = "Differenced log TFR series",
    subtitle = "Differencing order: d = 1",
    x = "Year",
    y = "Change in log TFR"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = file.path(
    output_dir,
    "02_differenced_log_TFR.png"
  ),
  plot = differenced_plot,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


# 10. ACF and PACF -------------------------------------------------

png(
  filename = file.path(
    output_dir,
    "03_ACF_log_TFR.png"
  ),
  width = 2400,
  height = 1600,
  res = 250,
  bg = "white"
)

Acf(
  log_train_tfr,
  lag.max = 15,
  main = "ACF of log-transformed TFR"
)

dev.off()


png(
  filename = file.path(
    output_dir,
    "04_PACF_log_TFR.png"
  ),
  width = 2400,
  height = 1600,
  res = 250,
  bg = "white"
)

Pacf(
  log_train_tfr,
  lag.max = 15,
  main = "PACF of log-transformed TFR"
)

dev.off()


png(
  filename = file.path(
    output_dir,
    "05_ACF_differenced_log_TFR.png"
  ),
  width = 2400,
  height = 1600,
  res = 250,
  bg = "white"
)

Acf(
  differenced_log_tfr,
  lag.max = 15,
  main = "ACF of differenced log TFR"
)

dev.off()


png(
  filename = file.path(
    output_dir,
    "06_PACF_differenced_log_TFR.png"
  ),
  width = 2400,
  height = 1600,
  res = 250,
  bg = "white"
)

Pacf(
  differenced_log_tfr,
  lag.max = 15,
  main = "PACF of differenced log TFR"
)

dev.off()


# 11. Fit ARIMA model to log TFR ----------------------------------

log_model <- Arima(
  log_train_tfr,
  order = c(1, 1, 1),
  include.drift = TRUE,
  method = "ML"
)

cat("\nLogarithmic ARIMA model:\n")
print(summary(log_model))


# 12. Forecast log TFR --------------------------------------------

log_forecast <- forecast(
  log_model,
  h = forecast_horizon,
  level = c(80, 95)
)


# 13. Convert forecasts back to original TFR scale ----------------

# exp() reverses the logarithmic transformation.

forecast_values <- exp(
  as.numeric(log_forecast$mean)
)

lower_80 <- exp(
  as.numeric(log_forecast$lower[, "80%"])
)

upper_80 <- exp(
  as.numeric(log_forecast$upper[, "80%"])
)

lower_95 <- exp(
  as.numeric(log_forecast$lower[, "95%"])
)

upper_95 <- exp(
  as.numeric(log_forecast$upper[, "95%"])
)


# 14. Create forecast table ---------------------------------------

forecast_results <- data.frame(
  Year = test_data$Year,
  Actual = as.numeric(test_tfr),
  Forecast = forecast_values,
  Lower80 = lower_80,
  Upper80 = upper_80,
  Lower95 = lower_95,
  Upper95 = upper_95
) %>%
  mutate(
    Error = Actual - Forecast,
    AbsoluteError = abs(Error),
    SquaredError = Error^2,
    AbsolutePercentageError =
      abs(Error / Actual) * 100
  )

write.csv(
  forecast_results,
  file.path(
    output_dir,
    "TFR_log_ARIMA_forecasts_2013_2025.csv"
  ),
  row.names = FALSE
)


# 15. Calculate test accuracy -------------------------------------

accuracy_results <- data.frame(
  Model = "Log ARIMA(1,1,1) with drift",
  MAE = mean(forecast_results$AbsoluteError),
  RMSE = sqrt(
    mean(forecast_results$SquaredError)
  ),
  MAPE = mean(
    forecast_results$AbsolutePercentageError
  )
)

cat("\nTest accuracy:\n")
print(accuracy_results)

write.csv(
  accuracy_results,
  file.path(
    output_dir,
    "TFR_log_ARIMA_accuracy.csv"
  ),
  row.names = FALSE
)


# 16. Residual diagnostics ----------------------------------------

model_residuals <- residuals(log_model)

ljung_box_test <- Box.test(
  model_residuals,
  lag = 10,
  type = "Ljung-Box",
  fitdf = 2
)

residual_results <- data.frame(
  Model = "Log ARIMA(1,1,1) with drift",
  ResidualMean = mean(
    model_residuals,
    na.rm = TRUE
  ),
  ResidualSD = sd(
    model_residuals,
    na.rm = TRUE
  ),
  LjungBoxStatistic = as.numeric(
    ljung_box_test$statistic
  ),
  LjungBoxLag = 10,
  LjungBoxPValue = ljung_box_test$p.value,
  Conclusion = ifelse(
    ljung_box_test$p.value > 0.05,
    "No strong evidence of residual autocorrelation",
    "Possible residual autocorrelation remains"
  )
)

write.csv(
  residual_results,
  file.path(
    output_dir,
    "TFR_log_ARIMA_residual_diagnostics.csv"
  ),
  row.names = FALSE
)


# 17. Save residual diagnostic graph ------------------------------

png(
  filename = file.path(
    output_dir,
    "07_log_ARIMA_residual_diagnostics.png"
  ),
  width = 2400,
  height = 1800,
  res = 250,
  bg = "white"
)

checkresiduals(log_model)

dev.off()


# 18. Forecast graph ----------------------------------------------

forecast_plot <- ggplot(
  forecast_results,
  aes(x = Year)
) +
  geom_ribbon(
    aes(
      ymin = Lower95,
      ymax = Upper95
    ),
    fill = "#9CC4E4",
    alpha = 0.35
  ) +
  geom_ribbon(
    aes(
      ymin = Lower80,
      ymax = Upper80
    ),
    fill = "#4A90C2",
    alpha = 0.45
  ) +
  geom_line(
    aes(
      y = Actual,
      colour = "Actual TFR"
    ),
    linewidth = 1.2
  ) +
  geom_point(
    aes(
      y = Actual,
      colour = "Actual TFR"
    ),
    size = 2.8
  ) +
  geom_line(
    aes(
      y = Forecast,
      colour = "Log ARIMA forecast"
    ),
    linewidth = 1.2
  ) +
  geom_point(
    aes(
      y = Forecast,
      colour = "Log ARIMA forecast"
    ),
    size = 2.8
  ) +
  scale_colour_manual(
    values = c(
      "Actual TFR" = "darkgreen",
      "Log ARIMA forecast" = "#CC0000"
    )
  ) +
  scale_x_continuous(
    breaks = seq(2013, 2025, by = 2)
  ) +
  labs(
    title = "TFR forecast using logarithmic ARIMA",
    subtitle = paste(
      "ARIMA(1,1,1) with drift |",
      "Training: 1960-2012 | Testing: 2013-2025"
    ),
    x = "Year",
    y = "Children per woman",
    colour = NULL,
    caption = paste(
      "Dark shading: 80% prediction interval |",
      "Light shading: 95% prediction interval"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(
    output_dir,
    "08_log_ARIMA_forecast_vs_actual.png"
  ),
  plot = forecast_plot,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "white"
)


# 19. Actual versus forecast graph without intervals ---------------

actual_forecast_long <- forecast_results %>%
  select(
    Year,
    Actual,
    Forecast
  ) %>%
  pivot_longer(
    cols = c(Actual, Forecast),
    names_to = "Series",
    values_to = "TFR"
  )

actual_forecast_plot <- ggplot(
  actual_forecast_long,
  aes(
    x = Year,
    y = TFR,
    colour = Series
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_colour_manual(
    values = c(
      Actual = "darkgreen",
      Forecast = "#CC0000"
    )
  ) +
  scale_x_continuous(
    breaks = seq(2013, 2025, by = 2)
  ) +
  labs(
    title = "Actual versus logarithmic ARIMA forecast TFR",
    subtitle = "Testing period: 2013-2025",
    x = "Year",
    y = "Children per woman",
    colour = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(
    output_dir,
    "09_actual_vs_log_ARIMA_forecast.png"
  ),
  plot = actual_forecast_plot,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "white"
)


# 20. Save model coefficients -------------------------------------

coefficient_results <- data.frame(
  Parameter = names(coef(log_model)),
  Estimate = as.numeric(coef(log_model))
)

write.csv(
  coefficient_results,
  file.path(
    output_dir,
    "TFR_log_ARIMA_coefficients.csv"
  ),
  row.names = FALSE
)


# 21. Save model summary ------------------------------------------

capture.output(
  summary(log_model),
  file = file.path(
    output_dir,
    "TFR_log_ARIMA_model_summary.txt"
  )
)


# 22. Save fitted model -------------------------------------------

saveRDS(
  log_model,
  file.path(
    output_dir,
    "TFR_log_ARIMA_fitted_model.rds"
  )
)


# 23. Completion message ------------------------------------------

cat("\nLogarithmic TFR ARIMA analysis completed.\n")
cat("Output folder:\n")
cat(normalizePath(output_dir), "\n")


################ TLBLOG

tlb_log_output_dir <- "Box_Jenkins_ARIMA_TLB_Log_M1"
if (!dir.exists(tlb_log_output_dir)) {
  dir.create(tlb_log_output_dir, recursive = TRUE)
}


# ------------------------------------------------
# 3. Extract Total Live Births
# The flexible match accepts labels such as:
# "Total Live Births", "Total Live-Births", or "TLB".
# ------------------------------------------------
tlb_rows <- births_long %>%
  mutate(
    Year = as.integer(Year),
    Value = as.numeric(Value),
    series_clean = tolower(trimws(as.character(DataSeries)))
  ) %>%
  filter(
    grepl("total.*live.*birth", series_clean) |
      series_clean %in% c("tlb", "total births", "live births")
  ) %>%
  select(Year, TLB = Value) %>%
  filter(!is.na(Year), !is.na(TLB)) %>%
  arrange(Year) %>%
  distinct(Year, .keep_all = TRUE)

if (nrow(tlb_rows) == 0) {
  stop("Total Live Births could not be identified in the DataSeries column.")
}

if (any(tlb_rows$TLB <= 0)) {
  stop("All TLB values must be greater than zero before applying log().")
}

train_tlb_df <- tlb_rows %>% filter(Year >= 1960, Year <= 2012)
test_tlb_df  <- tlb_rows %>% filter(Year >= 2013, Year <= 2025)

if (nrow(train_tlb_df) == 0 || nrow(test_tlb_df) == 0) {
  stop("Training or testing observations are missing for the specified years.")
}

train_tlb <- ts(train_tlb_df$TLB,
                start = min(train_tlb_df$Year), frequency = 1)
test_tlb <- ts(test_tlb_df$TLB,
               start = min(test_tlb_df$Year), frequency = 1)
log_train_tlb <- log(train_tlb)
differenced_log_tlb <- diff(log_train_tlb, differences = 1)

write_csv(train_tlb_df,
          file.path(tlb_log_output_dir, "TLB_log_training_data_1960_2012.csv"))
write_csv(test_tlb_df,
          file.path(tlb_log_output_dir, "TLB_log_test_data_2013_2025.csv"))

# Common graph theme
report_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 15),
    legend.position = "bottom"
  )

# ------------------------------------------------
# 4. Log-transformed training series
# ------------------------------------------------
log_train_plot_df <- data.frame(
  Year = train_tlb_df$Year,
  Log_TLB = as.numeric(log_train_tlb)
)

p1 <- ggplot(log_train_plot_df, aes(Year, Log_TLB)) +
  geom_line(colour = "#7030A0", linewidth = 1) +
  geom_point(colour = "#7030A0", size = 2.4) +
  labs(
    title = "Log-transformed Total Live Births training series",
    subtitle = "Singapore, 1960-2012",
    x = "Year", y = "Log Total Live Births"
  ) + report_theme

ggsave(file.path(tlb_log_output_dir, "01_log_TLB_training_series.png"),
       p1, width = 11, height = 7, dpi = 300, bg = "white")

# ------------------------------------------------
# 5. Differenced log series
# ------------------------------------------------
diff_plot_df <- data.frame(
  Year = train_tlb_df$Year[-1],
  Difference = as.numeric(differenced_log_tlb)
)

p2 <- ggplot(diff_plot_df, aes(Year, Difference)) +
  geom_hline(yintercept = 0, colour = "grey35") +
  geom_line(colour = "#7030A0", linewidth = 1) +
  geom_point(colour = "#7030A0", size = 2.4) +
  labs(
    title = "Differenced log Total Live Births series",
    subtitle = "Differencing order: d = 1",
    x = "Year", y = "Change in log Total Live Births"
  ) + report_theme

ggsave(file.path(tlb_log_output_dir, "02_differenced_log_TLB.png"),
       p2, width = 11, height = 7, dpi = 300, bg = "white")

# ------------------------------------------------
# 6. ACF and PACF graphs
# ------------------------------------------------
save_corr_plot <- function(filename, plot_expression) {
  png(file.path(tlb_log_output_dir, filename),
      width = 3300, height = 2100, res = 300, bg = "white")
  plot_expression()
  dev.off()
}

save_corr_plot("03_ACF_log_TLB.png", function() {
  acf(log_train_tlb, lag.max = 15,
      main = "ACF of log-transformed Total Live Births")
})

save_corr_plot("04_PACF_log_TLB.png", function() {
  pacf(log_train_tlb, lag.max = 15,
       main = "PACF of log-transformed Total Live Births")
})

save_corr_plot("05_ACF_differenced_log_TLB.png", function() {
  acf(differenced_log_tlb, lag.max = 15,
      main = "ACF of differenced log Total Live Births")
})

save_corr_plot("06_PACF_differenced_log_TLB.png", function() {
  pacf(differenced_log_tlb, lag.max = 15,
       main = "PACF of differenced log Total Live Births")
})

# ------------------------------------------------
# 7. ADF and KPSS stationarity tests
# ------------------------------------------------
log_adf_test <- adf.test(log_train_tlb)
log_kpss_test <- kpss.test(log_train_tlb)
diff_log_adf_test <- adf.test(differenced_log_tlb)
diff_log_kpss_test <- kpss.test(differenced_log_tlb)

stationarity_results <- data.frame(
  Series = c("Log TLB", "Log TLB",
             "Differenced log TLB", "Differenced log TLB"),
  Test = c("ADF", "KPSS", "ADF", "KPSS"),
  Statistic = c(
    unname(log_adf_test$statistic),
    unname(log_kpss_test$statistic),
    unname(diff_log_adf_test$statistic),
    unname(diff_log_kpss_test$statistic)
  ),
  P_value = c(
    log_adf_test$p.value,
    log_kpss_test$p.value,
    diff_log_adf_test$p.value,
    diff_log_kpss_test$p.value
  )
)

write_csv(stationarity_results,
          file.path(tlb_log_output_dir, "TLB_log_stationarity_tests.csv"))

# ------------------------------------------------
# 8. Fit logarithmic ARIMA(1,1,1) with drift
# ------------------------------------------------
log_tlb_model <- Arima(
  log_train_tlb,
  order = c(1, 1, 1),
  include.drift = TRUE,
  method = "ML"
)

saveRDS(log_tlb_model,
        file.path(tlb_log_output_dir, "TLB_log_ARIMA_fitted_model.rds"))

capture.output(
  summary(log_tlb_model),
  file = file.path(tlb_log_output_dir, "TLB_log_ARIMA_model_summary.txt")
)

# Coefficients, standard errors, z statistics and approximate p-values
coefficient_estimates <- coef(log_tlb_model)
coefficient_se <- sqrt(diag(vcov(log_tlb_model)))
coefficient_table <- data.frame(
  Term = names(coefficient_estimates),
  Estimate = as.numeric(coefficient_estimates),
  Standard_Error = as.numeric(coefficient_se),
  Z_value = as.numeric(coefficient_estimates / coefficient_se),
  P_value = 2 * pnorm(-abs(coefficient_estimates / coefficient_se))
)

write_csv(coefficient_table,
          file.path(tlb_log_output_dir, "TLB_log_ARIMA_coefficients.csv"))

# Convert log drift into approximate annual percentage change
drift_value <- if ("drift" %in% names(coefficient_estimates)) {
  unname(coefficient_estimates["drift"])
} else {
  NA_real_
}

drift_interpretation <- data.frame(
  Log_drift = drift_value,
  Approximate_annual_percentage_change = 100 * (exp(drift_value) - 1)
)

write_csv(drift_interpretation,
          file.path(tlb_log_output_dir, "TLB_log_ARIMA_drift_interpretation.csv"))

# ------------------------------------------------
# 9. Residual diagnostics
# ------------------------------------------------
log_tlb_residuals <- residuals(log_tlb_model)
lb_lag <- min(10, floor(length(na.omit(log_tlb_residuals)) / 5))
lb_test <- Box.test(
  na.omit(log_tlb_residuals),
  lag = lb_lag,
  type = "Ljung-Box",
  fitdf = 2
)

residual_diagnostics <- data.frame(
  Model = "Log ARIMA(1,1,1) with drift",
  Residual_mean = mean(log_tlb_residuals, na.rm = TRUE),
  Residual_SD = sd(log_tlb_residuals, na.rm = TRUE),
  Ljung_Box_statistic = unname(lb_test$statistic),
  Ljung_Box_lag = lb_lag,
  Ljung_Box_df = unname(lb_test$parameter),
  Ljung_Box_p_value = lb_test$p.value
)

write_csv(residual_diagnostics,
          file.path(tlb_log_output_dir, "TLB_log_ARIMA_residual_diagnostics.csv"))

png(file.path(tlb_log_output_dir, "07_log_TLB_ARIMA_residual_diagnostics.png"),
    width = 3300, height = 2400, res = 300, bg = "white")
checkresiduals(log_tlb_model, lag = 15)
dev.off()

# ------------------------------------------------
# 10. Forecast on log scale and back-transform to birth counts
# ------------------------------------------------
horizon <- nrow(test_tlb_df)
log_tlb_forecast <- forecast(log_tlb_model, h = horizon, level = c(80, 95))

log_mean <- as.numeric(log_tlb_forecast$mean)
log_lower_80 <- as.numeric(log_tlb_forecast$lower[, "80%"])
log_upper_80 <- as.numeric(log_tlb_forecast$upper[, "80%"])
log_lower_95 <- as.numeric(log_tlb_forecast$lower[, "95%"])
log_upper_95 <- as.numeric(log_tlb_forecast$upper[, "95%"])

# Forecast standard errors inferred from the 95% interval.
# exp(log_mean + 0.5 * variance) gives a bias-adjusted mean forecast.
forecast_se <- (log_upper_95 - log_mean) / qnorm(0.975)
forecast_births <- exp(log_mean + 0.5 * forecast_se^2)

forecast_results <- data.frame(
  Year = test_tlb_df$Year,
  Actual = test_tlb_df$TLB,
  Forecast = forecast_births,
  Lower_80 = exp(log_lower_80),
  Upper_80 = exp(log_upper_80),
  Lower_95 = exp(log_lower_95),
  Upper_95 = exp(log_upper_95),
  Error = test_tlb_df$TLB - forecast_births,
  Absolute_Error = abs(test_tlb_df$TLB - forecast_births),
  Percentage_Error = 100 * (test_tlb_df$TLB - forecast_births) /
    test_tlb_df$TLB
)

write_csv(forecast_results,
          file.path(tlb_log_output_dir,
                    "TLB_log_ARIMA_forecasts_2013_2025.csv"))

# Accuracy on the original number-of-births scale
mae_value <- mean(abs(forecast_results$Error))
rmse_value <- sqrt(mean(forecast_results$Error^2))
mape_value <- mean(abs(forecast_results$Percentage_Error))

accuracy_results <- data.frame(
  Model = "Log ARIMA(1,1,1) with drift",
  MAE = mae_value,
  RMSE = rmse_value,
  MAPE = mape_value
)

write_csv(accuracy_results,
          file.path(tlb_log_output_dir, "TLB_log_ARIMA_accuracy.csv"))

# ------------------------------------------------
# 11. Forecast graph with 80% and 95% intervals
# ------------------------------------------------
p8 <- ggplot(forecast_results, aes(x = Year)) +
  geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95),
              fill = "#BDD7EE", alpha = 0.55) +
  geom_ribbon(aes(ymin = Lower_80, ymax = Upper_80),
              fill = "#5B9BD5", alpha = 0.55) +
  geom_line(aes(y = Actual, colour = "Actual TLB"), linewidth = 1.1) +
  geom_point(aes(y = Actual, colour = "Actual TLB"), size = 2.8) +
  geom_line(aes(y = Forecast, colour = "Log ARIMA forecast"), linewidth = 1.1) +
  geom_point(aes(y = Forecast, colour = "Log ARIMA forecast"), size = 2.8) +
  scale_colour_manual(values = c("Actual TLB" = "darkgreen",
                                 "Log ARIMA forecast" = "#CC0000")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total Live Births forecast using logarithmic ARIMA",
    subtitle = paste0("ARIMA(1,1,1) with drift | Training: 1960-2012 | ",
                      "Testing: 2013-2025"),
    x = "Year", y = "Number of live births", colour = NULL,
    caption = "Dark shading: 80% prediction interval | Light shading: 95% prediction interval"
  ) + report_theme

ggsave(file.path(tlb_log_output_dir, "08_log_TLB_ARIMA_forecast_vs_actual.png"),
       p8, width = 12, height = 7.5, dpi = 300, bg = "white")

# ------------------------------------------------
# 12. Clear actual-versus-forecast graph
# ------------------------------------------------
comparison_long <- forecast_results %>%
  select(Year, Actual, Forecast) %>%
  pivot_longer(cols = c(Actual, Forecast),
               names_to = "Series", values_to = "Births")

p9 <- ggplot(comparison_long,
             aes(Year, Births, colour = Series, group = Series)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("Actual" = "darkgreen",
                                 "Forecast" = "#CC0000")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Actual versus logarithmic ARIMA forecast: Total Live Births",
    subtitle = "Testing period: 2013-2025",
    x = "Year", y = "Number of live births", colour = NULL
  ) + report_theme

ggsave(file.path(tlb_log_output_dir, "09_actual_vs_log_TLB_ARIMA_forecast.png"),
       p9, width = 11, height = 7, dpi = 300, bg = "white")

# ------------------------------------------------
# 13. Print main results in the R console
# ------------------------------------------------
cat("\nLOGARITHMIC TLB ARIMA ANALYSIS COMPLETED\n")
cat("Output folder:", tlb_log_output_dir, "\n\n")
print(summary(log_tlb_model))
cat("\nDrift interpretation:\n")
print(drift_interpretation)
cat("\nResidual diagnostics:\n")
print(residual_diagnostics)
cat("\nTest accuracy:\n")
print(accuracy_results)
