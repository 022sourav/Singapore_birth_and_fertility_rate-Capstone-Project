# ================================================================
# Singapore Births and Fertility: Base Forecasting Models
# Models: Naive, Drift, and Holt's Trend
# Training period: 1960-2012 | Test period: 2013-2025
# Outcomes: Total Fertility Rate and Total Live-Births
# ================================================================
#install.packages("patchwork")
# 1. PACKAGES ----------------------------------------------------
required_packages <- c("tidyverse", "forecast", "patchwork")
new_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(new_packages) > 0) install.packages(new_packages)
#install.packages("dplyr")  # Run only if dplyr is not installed
library(dplyr)
library(readr)
library(stringr)
library(forecast)
library(tidyverse)
library(forecast)
#library(patchwork)

# 2. SETTINGS ----------------------------------------------------
output_dir <- file.path(getwd(), "base_model_outputs")

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)
# Keep this R file and CSV in the same folder, or change file_path.
file_path <- "Singapore_Births_Fertility_Long_1960_2025.csv"
train_end <- 2012
test_start <- 2013
test_end <- 2025
output_dir <- "base_model_outputs"
dir.create(output_dir, showWarnings = FALSE)

# 3. IMPORT AND CHECK DATA ---------------------------------------
births <- readr::read_csv(
  file_path,
  show_col_types = FALSE
) |>
  dplyr::transmute(
    Year = as.integer(Year),
    DataSeries = stringr::str_squish(DataSeries),
    Value = as.numeric(Value)
  ) |>
  dplyr::arrange(DataSeries, Year)

required_columns <- c("Year", "DataSeries", "Value")
if (!all(required_columns %in% names(births))) {
  stop("The CSV must contain Year, DataSeries, and Value columns.")
}

duplicate_rows <- births |>
  count(Year, DataSeries) |>
  filter(n > 1)

if (nrow(duplicate_rows) > 0) {
  stop("Duplicate Year-DataSeries combinations were found. Check the data first.")
}

target_series <- c("Total Fertility Rate (TFR)", "Total Live-Births")
missing_series <- setdiff(target_series, unique(births$DataSeries))
if (length(missing_series) > 0) {
  stop(paste("Series not found:", paste(missing_series, collapse = ", ")))
}

cat("Rows:", nrow(births), "\n")
cat("Years:", min(births$Year, na.rm = TRUE), "to",
    max(births$Year, na.rm = TRUE), "\n")
cat("Missing Value entries:", sum(is.na(births$Value)), "\n")

# 4. ACCURACY FUNCTION ------------------------------------------
calculate_accuracy <- function(actual, predicted, model_name) {
  valid <- !is.na(actual) & !is.na(predicted)
  actual <- actual[valid]
  predicted <- predicted[valid]

  tibble(
    Model = model_name,
    MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2)),
    MAPE = if (any(actual == 0)) {
      NA_real_
    } else {
      mean(abs((actual - predicted) / actual)) * 100
    }
  )
}

# 5. MODEL FUNCTION ---------------------------------------------
run_base_models <- function(full_data, series_name, file_prefix) {

  cat("\n================================================\n")
  cat("Analysing:", series_name, "\n")
  cat("================================================\n")

  series_data <- full_data |>
    filter(DataSeries == series_name, !is.na(Value)) |>
    arrange(Year)

  train_data <- series_data |> filter(Year <= train_end)
  test_data <- series_data |> filter(Year >= test_start, Year <= test_end)

  expected_test_years <- test_start:test_end
  if (!identical(test_data$Year, expected_test_years)) {
    stop(paste("The test period is incomplete for", series_name))
  }

  train_ts <- ts(
    train_data$Value,
    start = min(train_data$Year),
    frequency = 1
  )

  h <- nrow(test_data)

  # Fit and forecast the three base models.
  naive_forecast <- naive(train_ts, h = h)
  drift_forecast <- rwf(train_ts, h = h, drift = TRUE)
  holt_forecast <- holt(train_ts, h = h)

  forecast_table <- tibble(
    Year = test_data$Year,
    Actual = test_data$Value,
    Naive = as.numeric(naive_forecast$mean),
    Drift = as.numeric(drift_forecast$mean),
    Holt = as.numeric(holt_forecast$mean)
  )

  accuracy_table <- bind_rows(
    calculate_accuracy(forecast_table$Actual, forecast_table$Naive, "Naive"),
    calculate_accuracy(forecast_table$Actual, forecast_table$Drift, "Drift"),
    calculate_accuracy(forecast_table$Actual, forecast_table$Holt, "Holt's trend")
  ) |>
    arrange(RMSE) |>
    mutate(across(c(MAE, RMSE, MAPE), ~ round(.x, 3)))

  cat("\nForecast values:\n")
  print(forecast_table, n = Inf)
  cat("\nAccuracy results (lower is better):\n")
  print(accuracy_table, n = Inf)
  cat("\nBest model based on RMSE:", accuracy_table$Model[1], "\n")

  # Forecast comparison plot.
  plot_data <- forecast_table |>
    pivot_longer(
      cols = c(Actual, Naive, Drift, Holt),
      names_to = "Model",
      values_to = "Value"
    )

  forecast_plot <- ggplot(plot_data,
                          aes(Year, Value, colour = Model, linetype = Model)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_colour_manual(values = c(
      "Actual" = "black",
      "Naive" = "#E63946",
      "Drift" = "#457B9D",
      "Holt" = "#2A9D8F"
    )) +
    scale_x_continuous(breaks = test_start:test_end) +
    labs(
      title = paste("Base-model forecasts:", series_name),
      subtitle = "Forecasts from 2013 to 2025 using training data through 2012",
      x = "Year", y = "Value", colour = NULL, linetype = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold")
    )

  # Full history with train/test separation.
  history_plot <- ggplot(series_data, aes(Year, Value)) +
    annotate("rect", xmin = test_start - 0.5, xmax = test_end + 0.5,
             ymin = -Inf, ymax = Inf, fill = "#E76F51", alpha = 0.10) +
    geom_line(colour = "#264653", linewidth = 0.9) +
    geom_vline(xintercept = train_end + 0.5, linetype = "dashed") +
    labs(
      title = paste("Training and test periods:", series_name),
      subtitle = "Training: 1960-2012 | Test: 2013-2025",
      x = "Year", y = "Value"
    ) +
    theme_minimal(base_size = 12)

  # Residual tests: p-value > 0.05 supports approximately random residuals.
  residual_tests <- bind_rows(
    tibble(
      Model = "Naive",
      Ljung_Box_p_value = Box.test(
        naive_forecast$residuals,
        lag = min(10, floor(length(naive_forecast$residuals) / 5)),
        type = "Ljung-Box"
      )$p.value
    ),
    tibble(
      Model = "Drift",
      Ljung_Box_p_value = Box.test(
        drift_forecast$residuals,
        lag = min(10, floor(length(drift_forecast$residuals) / 5)),
        type = "Ljung-Box"
      )$p.value
    ),
    tibble(
      Model = "Holt's trend",
      Ljung_Box_p_value = Box.test(
        residuals(holt_forecast$model),
        lag = min(10, floor(length(residuals(holt_forecast$model)) / 5)),
        type = "Ljung-Box"
      )$p.value
    )
  ) |>
    mutate(
      Ljung_Box_p_value = round(Ljung_Box_p_value, 4),
      Interpretation = if_else(
        Ljung_Box_p_value > 0.05,
        "No strong residual autocorrelation detected",
        "Residual autocorrelation may remain"
      )
    )

  cat("\nLjung-Box residual tests:\n")
  print(residual_tests, n = Inf)

  # Save every result for this outcome.
  write_csv(forecast_table,
            file.path(output_dir, paste0(file_prefix, "_forecasts.csv")))
  write_csv(accuracy_table,
            file.path(output_dir, paste0(file_prefix, "_accuracy.csv")))
  write_csv(residual_tests,
            file.path(output_dir, paste0(file_prefix, "_residual_tests.csv")))

  ggsave(
    file.path(output_dir, paste0(file_prefix, "_forecast_comparison.png")),
    forecast_plot, width = 11, height = 7, dpi = 300,bg = "white"
  )
  ggsave(
    file.path(output_dir, paste0(file_prefix, "_train_test_periods.png")),
    history_plot, width = 11, height = 6, dpi = 300,bg = "white"
  )

  list(
    series = series_data,
    training = train_data,
    testing = test_data,
    forecasts = forecast_table,
    accuracy = accuracy_table,
    residual_tests = residual_tests,
    forecast_plot = forecast_plot,
    history_plot = history_plot,
    fitted_models = list(
      naive = naive_forecast,
      drift = drift_forecast,
      holt = holt_forecast
    )
  )
}

# 6. RUN MODELS FOR BOTH OUTCOMES -------------------------------
tfr_results <- run_base_models(
  full_data = births,
  series_name = "Total Fertility Rate (TFR)",
  file_prefix = "TFR"
)

birth_results <- run_base_models(
  full_data = births,
  series_name = "Total Live-Births",
  file_prefix = "Total_Live_Births"
)

# 7. COMBINED ACCURACY TABLE ------------------------------------
combined_accuracy <- bind_rows(
  tfr_results$accuracy |> mutate(Outcome = "Total Fertility Rate"),
  birth_results$accuracy |> mutate(Outcome = "Total Live-Births")
) |>
  select(Outcome, everything())

print(combined_accuracy, n = Inf)
write_csv(combined_accuracy,
          file.path(output_dir, "combined_base_model_accuracy.csv"))

# Compare accuracy visually. Values are separated by outcome because the two
# outcomes use different measurement scales.
accuracy_plot <- combined_accuracy |>
  ggplot(aes(Model, RMSE, fill = Model)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Outcome, scales = "free_y") +
  labs(
    title = "Base-model test accuracy",
    subtitle = "Lower RMSE means a more accurate forecast",
    x = NULL, y = "RMSE"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    plot.title = element_text(face = "bold")
  )

print(accuracy_plot)
ggsave(file.path(output_dir, "combined_accuracy_comparison.png"),
       accuracy_plot, width = 10, height = 6, dpi = 300,bg = "white")

# 8. DISPLAY THE MAIN OUTPUTS -----------------------------------
tfr_results$accuracy
birth_results$accuracy
tfr_results$residual_tests
birth_results$residual_tests
tfr_results$forecast_plot
birth_results$forecast_plot

cat("\nAnalysis complete.\n")
cat("All tables and charts were saved in:", normalizePath(output_dir), "\n")
cat("Use the model with lower test errors, supported by residual diagnostics.\n")
