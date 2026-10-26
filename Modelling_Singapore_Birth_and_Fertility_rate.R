
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

library(tidyverse)
library(janitor)
library(skimr)
library(readr)
library(tseries)
library(forecast)


# DATA CLEANING
# ============================================================


# LOAD ORIGINAL DATA
# ============================================================

raw_data <- read.csv("BirthsAndFertilityRatesAnnual.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE,
  na.strings = c("","NA","N/A","na")
)


# Look at the original data

head(raw_data)

names(raw_data)

str(raw_data)



# CLEAN THE DATA
# ============================================================

# Remove extra spaces from the variable names

raw_data$DataSeries <- str_trim(raw_data$DataSeries)


# CHANGE DATA INTO LONG FORMAT
# ============================================================

# The original data has years as columns.
# We change it so that each row contains:
# DataSeries, Year and Value


data <- raw_data %>%
  
  # Make all year columns character first
  mutate(across(-DataSeries,as.character)
  ) %>%
  
  # Change from wide format to long format
  pivot_longer(
    cols = -DataSeries,
    names_to = "year",
    values_to = "value"
  ) %>%
  
  # Remove X from the year
  mutate(
    year = str_remove(year, "^X"),
    year = as.numeric(year),
    
    # Convert values to numeric
    value = str_trim(value),
    value = na_if(value, "na"),
    value = as.numeric(value)
  ) %>%
  
  # Rename DataSeries
  rename(
    variable = DataSeries
  ) %>%
  
  # Fix variable names
  mutate(
    variable = recode(
      variable,
      "Total Fertility Rate (TFR)" = "Total_Fertility_Rate_TFR",
      "15 - 19 Years" = "15_19_Years",
      "20 - 24 Years" = "20_24_Years",
      "25 - 29 Years" = "25_29_Years",
      "30 - 34 Years" = "30_34_Years",
      "35 - 39 Years" = "35_39_Years",
      "40 - 44 Years" = "40_44_Years",
      "45 - 49 Years" = "45_49_Years",
      "Chinese" = "Chinese",
      "Malays" = "Malays",
      "Indians" = "Indians",
      "Gross Reproduction Rate" = "Gross_Reproduction_Rate",
      "Net Reproduction Rate" = "Net_Reproduction_Rate",
      "Crude Birth Rate" = "Crude_Birth_Rate",
      "Total Live-Births" = "Total_Live_Births",
      "Resident Live-Births" = "Resident_Live_Births",
      "Citizen Live-Births" = "Citizen_Live_Births"
    )
  ) %>%
  
  # Sort by year and variable
  arrange(year,variable)


# CHECK THE LONG DATA
# ============================================================

head(data, 20)

tail(data, 20)

str(data)


# Check years

range(
  data$year,
  na.rm = TRUE
)


# Check all variables

unique(data$variable)


# CHECK MISSING VALUES
# ============================================================

missing_values <- data %>%
  group_by(variable) %>%
  summarise(
    missing = sum(is.na(value)),
    .groups = "drop"
  )


print(missing_values)


# CHECK DUPLICATES
# ============================================================

data %>%
  count(year,variable) %>%
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
head(train_data)
dim(train_data)

head(test_data)
dim(test_data)


# CREATE OUTPUT FOLDERS
# ============================================================

if (!dir.exists("Output")) {
  dir.create("Output")
}

if (!dir.exists("Output/Data")) {
  dir.create("Output/Data")
}

if (!dir.exists("Output/EDA")) {
  dir.create("Output/EDA")
}

if (!dir.exists("Output/EDA/Graphs")) {
  dir.create("Output/EDA/Graphs", recursive = TRUE)
}

# SAVE CLEANED DATA
# ============================================================

write.csv(
  data,
  "Output/Data/Cleaned_Singapore_Birth_Fertility_Data.csv",
  row.names = FALSE
)

write.csv(
  train_data,
  "Output/Data/Training_Data_1960_2012.csv",
  row.names = FALSE
)

write.csv(
  test_data,
  "Output/Data/Testing_Data_2013_2025.csv",
  row.names = FALSE
)

# CREATE DATA DICTIONARY
# ============================================================

data_dictionary <- tibble(
  variable = unique(data$variable),
  description = c(
    "Total Fertility Rate",
    "Age-specific fertility rate for ages 15-19",
    "Age-specific fertility rate for ages 20-24",
    "Age-specific fertility rate for ages 25-29",
    "Age-specific fertility rate for ages 30-34",
    "Age-specific fertility rate for ages 35-39",
    "Age-specific fertility rate for ages 40-44",
    "Age-specific fertility rate for ages 45-49",
    "Fertility rate for Chinese population",
    "Fertility rate for Malay population",
    "Fertility rate for Indian population",
    "Gross Reproduction Rate",
    "Net Reproduction Rate",
    "Crude Birth Rate",
    "Total number of live-births",
    "Resident number of live-births",
    "Citizen number of live-births"
  )
)


write.csv(
  data_dictionary,
  "Output/Data/Data_Dictionary.csv",
  row.names = FALSE
)



# ============================================================
# EXPLORATORY DATA ANALYSIS
# ============================================================

# BASIC SUMMARY
# ============================================================

data %>%
  group_by(variable) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    minimum = min(value, na.rm = TRUE),
    maximum = max(value, na.rm = TRUE),
    .groups = "drop"
  )


# TOTAL FERTILITY RATE
# ============================================================
tfr_data <- data %>%
  filter(
    variable == "Total_Fertility_Rate_TFR"
  )


ggplot(
  tfr_data,
  aes(
    x = year,
    y = value
  )
) +
  geom_line(
    colour = "steelblue",
    linewidth = 1
  ) +
  labs(
    title = "Singapore Total Fertility Rate",
    subtitle = "1960 to 2025",
    x = "Year",
    y = "Total Fertility Rate"
  ) +
  theme_minimal()
ggsave(
  "Output/EDA/Graphs/01_Total_Fertility_Rate.png",
  width = 10,
  height = 6,
  dpi = 300
)

# TOTAL LIVE-BIRTHS
# ============================================================

birth_data <- data %>%
  filter(
    variable == "Total_Live_Births"
  )


ggplot(
  birth_data,
  aes(
    x = year,
    y = value
  )
) +
  geom_line(
    colour = "forestgreen",
    linewidth = 1
  ) +
  labs(
    title = "Total Live-Births in Singapore",
    subtitle = "1960 to 2025",
    x = "Year",
    y = "Total Live-Births"
  ) +
  theme_minimal()

ggsave(
  "Output/EDA/Graphs/02_Total_Live_Births.png",
  width = 10,
  height = 6,
  dpi = 300
)


# CRUDE BIRTH RATE
# ============================================================

crude_birth_data <- data %>%
  filter(
    variable == "Crude_Birth_Rate"
  )


ggplot(
  crude_birth_data,
  aes(
    x = year,
    y = value
  )
) +
  geom_line(
    colour = "darkorange",
    linewidth = 1
  ) +
  labs(
    title = "Singapore Crude Birth Rate",
    subtitle = "1960 to 2025",
    x = "Year",
    y = "Crude Birth Rate"
  ) +
  theme_minimal()
ggsave(
  "Output/EDA/Graphs/03_Crude_Birth_Rate.png",
  width = 10,
  height = 6,
  dpi = 300
)

# AGE-SPECIFIC FERTILITY RATES
# ============================================================

age_data <- data %>%
  filter(
    variable %in% c(
      "15_19_Years",
      "20_24_Years",
      "25_29_Years",
      "30_34_Years",
      "35_39_Years",
      "40_44_Years",
      "45_49_Years"
    )
  )


ggplot(
  age_data,
  aes(
    x = year,
    y = value,
    colour = variable
  )
) +
  geom_line(
    linewidth = 1
  ) +
  labs(
    title = "Age-Specific Fertility Rates",
    subtitle = "Singapore, 1960 to 2025",
    x = "Year",
    y = "Fertility Rate",
    colour = "Age Group"
  ) +
  theme_minimal()
ggsave(
  "Output/EDA/Graphs/04_Age-Specific Fertility Rates.png",
  width = 10,
  height = 6,
  dpi = 300
)


# AGE-SPECIFIC FERTILITY RATES - 1960 AND 2025
# ============================================================

age_comparison <- age_data %>%
  filter(
    year %in% c(1960, 2025)
  )


ggplot(
  age_comparison,
  aes(
    x = variable,
    y = value,
    fill = variable
  )
) +
  geom_col() +
  facet_wrap(
    ~year
  ) +
  labs(
    title = "Age-Specific Fertility Rates: 1960 vs 2025",
    x = "Age Group",
    y = "Fertility Rate"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

ggsave(
  "Output/EDA/Graphs/05_Age-Specific Fertility Rates Comparison.png",
  width = 10,
  height = 6,
  dpi = 300
)

# ETHNIC GROUP FERTILITY
# ============================================================

ethnic_data <- data %>%
  filter(
    variable %in% c(
      "Chinese",
      "Malays",
      "Indians"
    )
  )


ggplot(
  ethnic_data,
  aes(
    x = year,
    y = value,
    colour = variable
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  labs(
    title = "Fertility Rates by Ethnic Group",
    subtitle = "Singapore, 1960 to 2025",
    x = "Year",
    y = "Fertility Rate",
    colour = "Ethnic Group"
  ) +
  theme_minimal()

ggsave(
  "Output/EDA/Graphs/06_Ethinc_Group_Fertility.png",
  width = 10,
  height = 6,
  dpi = 300
)


# GROSS AND NET REPRODUCTION RATE
# ============================================================

reproduction_data <- data %>%
  filter(
    variable %in% c(
      "Gross_Reproduction_Rate",
      "Net_Reproduction_Rate"
    )
  )


ggplot(
  reproduction_data,
  aes(
    x = year,
    y = value,
    colour = variable
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  labs(
    title = "Gross and Net Reproduction Rates",
    subtitle = "Singapore, 1960 to 2025",
    x = "Year",
    y = "Rate",
    colour = "Variable"
  ) +
  theme_minimal()

ggsave(
  "Output/EDA/Graphs/07_Gross_Net_Reproduction_Rates.png",
  width = 10,
  height = 6,
  dpi = 300
)

# MAIN FERTILITY INDICATORS
# ============================================================

main_data <- data %>%
  filter(
    variable %in% c(
      "Total_Fertility_Rate_TFR",
      "Total_Live_Births"
    )
  )


ggplot(
  main_data,
  aes(
    x = year,
    y = value,
    colour = variable
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  facet_wrap(
    ~variable,
    scales = "free_y"
  )+
  labs(
    title = "Main Fertility Indicators in Singapore",
    subtitle = "1960 to 2025",
    x = "Year",
    y = "Value",
    colour = "Indicator"
  ) +
  theme_minimal()

  ggsave(
    "Output/EDA/Graphs/08_Main_Indicators.png",
    width = 10,
    height = 6,
    dpi = 300
  )  



# TFR- TRAINING AND TESTING PERIOD
# ============================================================

tfr_train <- data %>%
  filter(
    variable == "Total_Fertility_Rate_TFR",
    year <= 2012
  )


tfr_test <- data %>%
  filter(
    variable == "Total_Fertility_Rate_TFR",
    year >= 2013
  )
tfr_test

ggplot() +
  
  geom_line(
    data = tfr_train,
    aes(
      x = year,
      y = value
    ),
    colour = "blue",
    linewidth = 1
  ) +
  
  geom_line(
    data = tfr_test,
    aes(
      x = year,
      y = value
    ),
    colour = "red",
    linewidth = 1
  ) +
  
  labs(
    title = "TFR Training and Testing Period",
    subtitle = "Training: 1960-2012 | Testing: 2013-2025",
    x = "Year",
    y = "Total Fertility Rate"
  ) +
  
  theme_minimal()

ggsave(
  "Output/EDA/Graphs/09_TFR_Training_Testing.png",
  width = 10,
  height = 6,
  dpi = 300
) 


# TFR 1960 AND 2025
# ============================================================

tfr_start_end <- tfr_data %>%
  filter(
    year %in% c(1960, 2025)
  )


print(tfr_start_end)


# TOTAL LIVE-BIRTHS 1960 AND 2025
# ============================================================

birth_start_end <- birth_data %>%
  filter(
    year %in% c(1960, 2025)
  )


print(birth_start_end)


# PERCENTAGE CHANGE IN TFR
# ============================================================

tfr_1960 <- tfr_data %>%
  filter(year == 1960) %>%
  pull(value)


tfr_2025 <- tfr_data %>%
  filter(year == 2025) %>%
  pull(value)


tfr_change <- (
  (tfr_2025 - tfr_1960) /
    tfr_1960
) * 100


print(
  paste(
    "TFR percentage change:",
    round(tfr_change, 2),
    "%"
  )
)

# PERCENTAGE CHANGE IN TOTAL LIVE-BIRTHS
# ============================================================

tlb_data <- data %>%
  filter(
    variable == "Total_Live_Births"
  )


tlb_1960 <- tlb_data %>%
  filter(year == 1960) %>%
  pull(value)


tlb_2025 <- tlb_data %>%
  filter(year == 2025) %>%
  pull(value)


tlb_change <- (
  (tlb_2025 - tlb_1960) /
    tlb_1960
) * 100


print(
  paste(
    "Total Live-Births percentage change:",
    round(tlb_change, 2),
    "%"
  )
)


# HIGHEST FERTILITY AGE GROUP IN 1960
# ============================================================

age_1960 <- age_data %>%
  filter(year == 1960) %>%
  arrange(desc(value)
  )


print(age_1960)


# HIGHEST FERTILITY AGE GROUP IN 2025
# ============================================================

age_2025 <- age_data %>%
  filter(year == 2025) %>%
  arrange(desc(value)
  )


print(age_2025)



# HIGHEST ETHNIC FERTILITY IN 2025
# ============================================================

ethnic_2025 <- ethnic_data %>%
  filter(year == 2025) %>%
  arrange(desc(value)
  )


print(ethnic_2025)


# CORRELATION ANALYSIS
# ============================================================

correlation_data <- data %>%
  select(
    year,
    variable,
    value
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  )

correlation_matrix <- correlation_data %>%
  select(
    Total_Fertility_Rate_TFR,
    Total_Live_Births,
    Crude_Birth_Rate,
    Gross_Reproduction_Rate,
    Net_Reproduction_Rate
  ) %>%
  cor(
    use = "pairwise.complete.obs"
  )

print(correlation_matrix)



# SAVE SOME EDA RESULTS
# ============================================================

write.csv(
  tfr_data,
  "Output/EDA/TFR_Data.csv",
  row.names = FALSE
)

write.csv(
  tlb_data,
  "Output/EDA/Total_Live_Births_Data.csv",
  row.names = FALSE
)

write.csv(
  age_data,
  "Output/EDA/Age_Specific_Fertility_Data.csv",
  row.names = FALSE
)

write.csv(
  ethnic_data,
  "Output/EDA/Ethnic_Fertility_Data.csv",
  row.names = FALSE
)

write.csv(
  reproduction_data,
  "Output/EDA/Reproduction_Rate_Data.csv",
  row.names = FALSE
)

write.csv(
  correlation_matrix,
  "Output/EDA/Correlation_Matrix.csv"
)


##############################################################
#ARIMA MODEL - TFR

tfr_train <- train_data %>%
  filter(variable == "Total_Fertility_Rate_TFR") %>%
  arrange(year)

tfr_test <- test_data %>%
  filter(variable == "Total_Fertility_Rate_TFR") %>%
  arrange(year)

tfr_ts <- ts(
  tfr_train$value,
  start = 1960,
  frequency = 1
)
tfr_ts

#Plot Original TFR

plot(tfr_ts,
     main = "Singapore Total Fertility Rate",
     xlab = "Year",
     ylab = "Total Fertility Rate")

# First Difference of TFR

tfr_diff <- diff(tfr_ts)
tfr_diff

plot(tfr_diff,
     main = "First Difference of Singapore TFR",
     xlab = "Year",
     ylab = "Differenced TFR")

# ACF and PACF of differenced TFR

acf(tfr_diff, lag.max = 40, main= "ACF of differenced TFR")

pacf(tfr_diff, lag.max = 40, main = "PACF of differenced TFR" )

# Second difference of TFR

tfr_diff2 <- diff(tfr_ts, differences = 2)
tfr_diff2

plot(tfr_diff2,
     main = "Second Difference of Singapore TFR",
     xlab = "Year",
     ylab = "Second Differenced TFR")

# ACF and PACF for second differenced TFR

acf(tfr_diff2, lag.max = 40, main = "ACF of second differenced TFR")

pacf(tfr_diff2, lag.max = 40, main = "PACF of second differenced TFR")


#########  TFR ARIMA ############################
# Full dataset tfr

tfr <- data[data$variable == "Total_Fertility_Rate_TFR", ]
tfr <- tfr[order(tfr$year), ]

#Training dataset

train_rows_tfr <- tfr[tfr$year >= 1960 & tfr$year <= 2012, ]
train_rows_tfr <- train_rows_tfr[order(train_rows_tfr$year), ]

# Create the training time series

tfr_training <- ts(train_rows_tfr$value, start = 1960, end = 2012, frequency = 1)
tfr_training



# Test dataset

# Select and sort the test years

test_rows <- tfr[tfr$year >= 2013 & tfr$year <= 2025, ]
test_rows <- test_rows[order(test_rows$year), ]

# Create the test time series

tfr_test <- ts(test_rows$value, start = 2013, end = 2025, frequency = 1)
tfr_test

# KPSS TEST
#plot(tfr_training)
kpss.test(diff(tfr_training))

kpss.test(diff(diff(tfr_training)))

#plot(diff(diff(tfr_training)))
acf(diff(diff(tfr_training)),lag=40)
pacf(diff(diff(tfr_training)),lag=40)

########## RUN mODEL ###############################
candidate_tfr <- arima((tfr_training), order = c(12,2,3))
candidate_tfr

########### FORCAST #########################################
fc <- predict(candidate_tfr, n.ahead = length(tfr_test))$pred
train_fitted <- tfr_training - residuals(candidate_tfr)

# Training accuracy
accuracy(train_fitted, tfr_training)[, c("RMSE", "MAE", "MAPE")]

accuracy(fc, tfr_test)[, c("RMSE", "MAE", "MAPE")]

# FORCAST Result
#       Model  Set       AIC      RMSE       MAE      MAPE
# 3 ARIMA(12,2,3) Test -62.57198 0.1216015 0.09286404  9.011205
# 4 ARIMA(12,2,4) Test -60.98709 0.1331871 0.09849370  9.637660
# 10,2,3       RMSE        MAE       MAPE 
#            0.2667480  0.2131881 20.9759165 
#AIC--54.69
#ARIMA(10,2,4) Test -57.48213 0.2619891 0.2057930 20.36590
#ARIMA(11,2,3) Test -52.90772 0.2569323 0.2049421 20.174292

# ACF PACF
old_par <- par(mfrow = c(1, 2))
acf(residuals(candidate_tfr), lag.max = 40, main = "ACF-c(12,2,3)")
pacf(residuals(candidate_tfr), lag.max = 40, main = "PACF-c(12,2,3)")
par(old_par)


############################ PLOT PRDECTION TFR ARIMA ###################
# Models to compare
orders <- list(
  c(12, 2, 3),
  c(12, 2, 4),
  c(10, 2, 4)
)

selected_names <- sapply(
  orders,
  function(x) paste0("ARIMA(", paste(x, collapse = ","), ")")
)

# Fit TFR models without log
selected_models <- setNames(
  lapply(orders, function(x) {
    arima(
      tfr_training,
      order = x,
      method = "ML",
      optim.control = list(maxit = 2000)
    )
  }),
  selected_names
)

# Training fitted values
train_predictions <- lapply(selected_models, function(model) {
  tfr_training - residuals(model)
})

# Test predictions
test_predictions <- lapply(selected_models, function(model) {
  predict(model, n.ahead = length(tfr_test))$pred
})

# Colours and line types
colours <- c("blue", "red", "darkgreen")
line_types <- 1:3

# Horizontal axes
use_years <- is.ts(tfr_training) && is.ts(tfr_test)

if (use_years) {
  train_years <- as.numeric(time(tfr_training))
  test_years <- as.numeric(time(tfr_test))
} else {
  train_years <- seq_along(tfr_training)
  test_years <- length(tfr_training) + seq_along(tfr_test)
}

# Plotting function
plot_predictions <- function(actual, years, predictions, title, set) {
  
  actual <- as.numeric(actual)
  
  model_labels <- sapply(seq_along(selected_models), function(i) {
    
    predicted <- as.numeric(predictions[[i]])
    
    valid <- is.finite(actual) &
      is.finite(predicted) &
      actual != 0
    
    mape <- mean(
      abs((actual[valid] - predicted[valid]) / actual[valid])
    ) * 100
    
    sprintf(
      "%s | AIC: %.2f | %s MAPE: %.2f%%",
      selected_names[i],
      AIC(selected_models[[i]]),
      set,
      mape
    )
  })
  
  plot(
    years, actual,
    type = "l",
    col = "black",
    lwd = 3,
    ylim = range(c(actual, unlist(predictions)), finite = TRUE),
    xlab = if (use_years) "Year" else "Observation",
    ylab = "Total fertility rate",
    main = title
  )
  
  for (i in seq_along(predictions)) {
    lines(
      years,
      as.numeric(predictions[[i]]),
      col = colours[i],
      lty = line_types[i],
      lwd = 2
    )
  }
  
  legend(
    "bottomleft",
    legend = c("Actual", model_labels),
    col = c("black", colours),
    lty = c(1, line_types),
    lwd = c(3, 2, 2, 2),
    cex = 0.55,
    bty = "n"
  )
}

# Training and test graphs on one screen
old_par <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

plot_predictions(
  tfr_training, train_years, train_predictions,
  "Training: Actual vs Fitted", "Train"
)

plot_predictions(
  tfr_test, test_years, test_predictions,
  "Test: Actual vs Prediction", "Test"
)

par(old_par)

######################### ARIMA MODEL - TLB ##################################


tlb <- data[data$variable == "Total_Live_Births", ]
tlb <- tlb[order(tlb$year), ]

# Training Dataset

train_rows_tlb <- tlb[tlb$year >= 1960 & tlb$year <= 2012, ]
train_rows_tlb <- train_rows_tlb[order(train_rows_tlb$year), ]

# Create the training time series

tlb_training <- ts(train_rows_tlb$value, start = 1960, end = 2012, frequency = 1)
tlb_training

# TEST DATASET

# Select and sort the test years

test_rows <- tlb[tlb$year >= 2013 & tlb$year <= 2025, ]
test_rows <- test_rows[order(test_rows$year), ]

# Create the test time series

tlb_test <- ts(test_rows$value, start = 2013, end = 2025, frequency = 1)
tlb_test




#plot(log(tlb_training))
kpss.test(diff(log(tlb_training)))

#plot(diff(log(tlb_training)))

acf(diff(log(tlb_training)),lag=40)
pacf(diff(log(tlb_training)),lag=40)



#######  RUN MODEL ##########################
candidate_tlb <- arima(  log(tlb_training),  order = c(12, 1, 3))
candidate_tlb

# Test accuracy on the original scale
predicted_births <- exp(predict(candidate_tlb, n.ahead = length(tlb_test))$pred)
accuracy(predicted_births, tlb_test)[, c("RMSE", "MAE", "MAPE")]

# Residual diagnostics
old_par_tlb <- par(mfrow = c(1, 2))
acf(residuals(candidate_tlb), lag.max = 40, main = "ACF-c(12, 1, 3)")
pacf(residuals(candidate_tlb), lag.max = 40, main = "PACF-c(12, 1, 3)")
par(old_par_tlb)

#### prediction graph

# Training fitted values and test predictions
train_pred <- exp(log(tlb_training) - residuals(candidate_tlb))
test_pred <- exp(predict(candidate_tlb, n.ahead = length(tlb_test))$pred)

######### PRDDICTION RESUL

#           Model  Set       AIC      RMSE      MAE     MAPE

# 4 ARIMA(13,1,1) Test -141.4462 10476.842 8969.293 25.21881
# 3 ARIMA(13,1,2) Test -142.4665 10203.632 8715.483 24.53455
# 1 ARIMA(13,1,3) Test -145.4682  9299.696 7881.560 22.18053
# 2 ARIMA(13,1,4) Test -144.77  9937.09618 8295.83  23.43728

# 4 ARIMA(12,1,3) Test -139.6921 5654.236 4331.727 12.50054 -------------
# 4 ARIMA(12,1,4) Test -144.77  9937.09618 8295.83691   23.43728
#ARIMA(11,1,4) AIC -  -136.58
# RMSE        MAE       MAPE 
# 8677.98763 7475.43243   21.01787
#ARIMA(14,1,3) Test -143.6268  9458.965 8027.458 22.58280
################## PLOT PREDICTION################################

# Keep only your passing models here
orders_tlb <- list(
  c(13, 1, 4),
  c(12, 1, 3),
  c(13, 1, 3)
)

# Model names
selected_names_tlb <- sapply(
  orders_tlb,
  function(x) paste0("ARIMA(", paste(x, collapse = ","), ")")
)

# Fit log models using the default arima() method
selected_models_tlb <- setNames(
  lapply(orders_tlb, function(x) {
    arima(
      log(tlb_training),
      order = x,
      method = "ML",
      optim.control = list(maxit = 2000)
    )
  }),
  selected_names_tlb
)

# Training fitted values on the original birth-count scale
train_predictions_tlb <- lapply(selected_models_tlb, function(model) {
  exp(log(tlb_training) - residuals(model))
})

# Test predictions on the original birth-count scale
test_predictions_tlb <- lapply(selected_models_tlb, function(model) {
  exp(predict(model, n.ahead = length(tlb_test))$pred)
})

# Model names
model_names_tlb <- names(selected_models_tlb)

# Function to create separate training and test labels
make_labels_tlb <- function(actual, predictions, set_name) {
  
  sapply(seq_along(selected_models_tlb), function(i) {
    
    actual_values <- as.numeric(actual)
    predicted_values <- as.numeric(predictions[[i]])
    
    valid <- is.finite(actual_values) &
      is.finite(predicted_values) &
      actual_values != 0
    
    mape <- if (any(valid)) {
      mean(
        abs(
          (actual_values[valid] - predicted_values[valid]) /
            actual_values[valid]
        )
      ) * 100
    } else {
      NA_real_
    }
    
    sprintf(
      "%s | AIC: %.2f | %s MAPE: %.2f%%",
      model_names_tlb[i],
      AIC(selected_models_tlb[[i]]),
      set_name,
      mape
    )
  })
}

# Separate labels
train_labels_tlb <- make_labels_tlb(
  tlb_training,
  train_predictions_tlb,
  "Train"
)

test_labels_tlb <- make_labels_tlb(
  tlb_test,
  test_predictions_tlb,
  "Test"
)

# Colours and line types
colours_tlb <- rep(
  c(
    "blue", "red", "darkgreen", "purple",
    "orange", "brown", "deeppink", "cyan4"
  ),
  length.out = length(selected_models_tlb)
)

line_types_tlb <- rep(
  1:6,
  length.out = length(selected_models_tlb)
)

# Horizontal axes
use_years_tlb <- is.ts(tlb_training) && is.ts(tlb_test)

if (use_years_tlb) {
  train_years_tlb <- as.numeric(time(tlb_training))
  test_years_tlb <- as.numeric(time(tlb_test))
} else {
  train_years_tlb <- seq_along(tlb_training)
  test_years_tlb <- length(tlb_training) + seq_along(tlb_test)
}

# Function to draw each graph
plot_predictions_tlb <- function(
  actual, years, predictions, title, labels) {
  
  plot(
    years,
    as.numeric(actual),
    type = "l",
    col = "black",
    lwd = 3,
    ylim = range(
      c(as.numeric(actual), unlist(predictions)),
      finite = TRUE
    ),
    xlab = if (use_years_tlb) "Year" else "Observation",
    ylab = "Total live births",
    main = title
  )
  
  for (i in seq_along(predictions)) {
    lines(
      years,
      as.numeric(predictions[[i]]),
      col = colours_tlb[i],
      lty = line_types_tlb[i],
      lwd = 2
    )
  }
  
  legend(
    "bottomleft",
    legend = c("Actual", labels),
    col = c("black", colours_tlb),
    lty = c(1, line_types_tlb),
    lwd = c(3, rep(2, length(predictions))),
    cex = 0.55,
    bty = "n"
  )
}

# Show training and test graphs side by side
old_par_tlb <- par(no.readonly = TRUE)

par(
  mfrow = c(1, 2),
  mar = c(4, 4, 3, 1)
)

plot_predictions_tlb(
  tlb_training,
  train_years_tlb,
  train_predictions_tlb,
  "Training: Actual vs Fitted",
  train_labels_tlb
)

plot_predictions_tlb(
  tlb_test,
  test_years_tlb,
  test_predictions_tlb,
  "Test: Actual vs Prediction",
  test_labels_tlb
)

# Restore previous plotting settings
par(old_par_tlb)



################### HK SEASONAL TFR ####################################
x <- diff(tfr_training, 12)
kpss.test(x)
plot(x)
acf(x)
pacf(x)


y <- diff(diff(tfr_training), 12)
kpss.test(y)
plot(y)
acf(y, lag.max = 45)
pacf(y, lag.max = 45)

#############RUN MODEL ############################################################################################
candidate_tfr_s <- arima( tfr_training,  order = c(5, 1, 3),  seasonal = list(order = c(0, 1, 0),period = 12 ))
candidate_tfr_s

fc <- predict(candidate_tfr_s, n.ahead = length(tfr_test))$pred
train_fitted <- tfr_training - residuals(candidate_tfr_s)
# Training accuracy
accuracy(train_fitted, tfr_training)[, c("RMSE", "MAE", "MAPE")]
#accuracy(fc, tfr_test)["Test set", ]
accuracy(fc, tfr_test)[, c("RMSE", "MAE", "MAPE")]

########## ACF PCF #################
old_par_s_tfr <- par(mfrow = c(1, 2))
acf(residuals(candidate_tfr_s), lag.max = 40, main = "ACF")
pacf(residuals(candidate_tfr_s), lag.max = 40, main = "PACF")
par(old_par_s_tfr)


############################ PLOT PRDECTION TFR SARIMA ###################
# Models to compare
orders_s_tfr <- list(
  c(5, 1, 0),
  c(5, 1, 2),
  c(5, 1, 3)
)

# Seasonal settings
seasonal_order <- c(0, 1, 0)
seasonal_period <- 12

selected_names_s_tfr <- sapply(
  orders_s_tfr,
  function(x) paste0(
    "SARIMA(", paste(x, collapse = ","), ")",
    "(", paste(seasonal_order, collapse = ","), ")",
    "[", seasonal_period, "]"
  )
)

# Fit SARIMA models without log
selected_models_s_tfr <- setNames(
  lapply(orders_s_tfr, function(x) {
    arima(
      tfr_training,
      order = x,
      seasonal = list(
        order = seasonal_order,
        period = seasonal_period
      ),
      method = "ML",
      optim.control = list(maxit = 2000)
    )
  }),
  selected_names_s_tfr
)

# Check convergence: 0 means successful convergence
print(sapply(selected_models_s_tfr, function(model) model$code))

# Training fitted values
train_predictions_s_tfr <- lapply(selected_models_s_tfr, function(model) {
  tfr_training - residuals(model)
})

# Test predictions using predict()
test_predictions_s_tfr <- lapply(selected_models_s_tfr, function(model) {
  predict(model, n.ahead = length(tfr_test))$pred
})

# Colours and line types
colours_s_tfr <- c("blue", "red", "darkgreen")
line_types <- 1:3

# Horizontal axes
use_years_s_tfr <- is.ts(tfr_training) && is.ts(tfr_test)

if (use_years_s_tfr) {
  train_years <- as.numeric(time(tfr_training))
  test_years <- as.numeric(time(tfr_test))
} else {
  train_years <- seq_along(tfr_training)
  test_years <- length(tfr_training) + seq_along(tfr_test)
}

# Plotting function
plot_predictions_s_tfr <- function(actual, years, predictions, title, set) {
  
  actual <- as.numeric(actual)
  
  model_labels_s_tfr <- sapply(seq_along(selected_models_s_tfr), function(i) {
    
    predicted <- as.numeric(predictions[[i]])
    
    valid <- is.finite(actual) &
      is.finite(predicted) &
      actual != 0
    
    mape <- mean(
      abs((actual[valid] - predicted[valid]) / actual[valid])
    ) * 100
    
    sprintf(
      "%s | AIC: %.2f | %s MAPE: %.2f%%",
      selected_names_s_tfr[i],
      AIC(selected_models_s_tfr[[i]]),
      set,
      mape
    )
  })
  
  plot(
    years, actual,
    type = "l",
    col = "black",
    lwd = 3,
    ylim = range(c(actual, unlist(predictions)), finite = TRUE),
    xlab = if (use_years_s_tfr) "Year" else "Observation",
    ylab = "Total fertility rate",
    main = title
  )
  
  for (i in seq_along(predictions)) {
    lines(
      years,
      as.numeric(predictions[[i]]),
      col = colours_s_tfr[i],
      lty = line_types[i],
      lwd = 2
    )
  }
  
  legend(
    "bottomleft",
    legend = c("Actual", model_labels_s_tfr),
    col = c("black", colours_s_tfr),
    lty = c(1, line_types),
    lwd = c(3, 2, 2, 2),
    cex = 0.5,
    bty = "n"
  )
}

# Training and test graphs on one screen
old_par_s_tfr <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

plot_predictions_s_tfr(
  tfr_training, train_years, train_predictions_s_tfr,
  "Training: Actual vs Fitted", "Train"
)

plot_predictions_s_tfr(
  tfr_test, test_years, test_predictions_s_tfr,
  "Test: Actual vs Prediction", "Test"
)

par(old_par_s_tfr)

############################# HK SARIMA TLB ###########################

x_tlb <- diff(tlb_training, 12)
kpss.test(x_tlb)
plot(x_tlb)
acf(x_tlb, lag=45)
pacf(x_tlb, lag=45)


y_tlb <- diff(diff(tlb_training), 12)
kpss.test(y_tlb)
plot(y_tlb)
acf(y_tlb)
pacf(y_tlb)



# RUN MODEL
candidate_tlb_s <- arima(  log(tlb_training),  order = c(5, 0, 0),  seasonal = list(order = c(0, 1, 1), period = 12))
candidate_tlb_s

# Predict and convert back to birth counts
predicted_births_tlb <- exp(  predict(candidate_tlb_s, n.ahead = length(tlb_test))$pred)

# Test accuracy on the original scale
accuracy(predicted_births_tlb, tlb_test)[, c("RMSE", "MAE", "MAPE")]

# Residual ACF and PACF on one screen
res <- na.omit(residuals(candidate_tlb_s))

########### ACF PACF ###############
old_par_s_tlb <- par(mfrow = c(1, 2))
acf(  res, lag.max = 40,   main = "ACF-c(5, 0, 0),c(0, 1, 1)")
pacf(  res, lag.max = 40,  main = "PACF-c(5, 0, 0),c(0, 1, 1)")
par(old_par_s_tlb)

# RUN MODEL
candidate_tlb_s_1 <- arima(  log(tlb_training),  order = c(5, 0, 0),  seasonal = list(order = c(1, 1, 0), period = 12))
candidate_tlb_s_1

# Predict and convert back to birth counts
predicted_births_tlb_1 <- exp(  predict(candidate_tlb_s_1, n.ahead = length(tlb_test))$pred)

# Test accuracy on the original scale
accuracy(predicted_births_tlb_1, tlb_test)[, c("RMSE", "MAE", "MAPE")]

# Residual ACF and PACF on one screen
res <- na.omit(residuals(candidate_tlb_s_1))

########### ACF PACF ###############
old_par_s_tlb <- par(mfrow = c(1, 2))
acf(  res, lag.max = 40,   main = "ACF-c(5, 0, 0),c(1, 1, 0)")
pacf(  res, lag.max = 40,  main = "PACF-c(5, 0, 0),c(1, 1, 0)")
par(old_par_s_tlb)

################ PLOT #########################################

# Models to compare
orders_s_tlb <- list(
  c(5, 0, 0),
  c(5, 0, 1),
  c(5, 0, 2),
  c(5, 0, 0),
  c(5, 0, 1),
  c(5, 0, 2)
  
)

# Seasonal settings
seasonal_orders_s_tlb <- list(
  c(0, 1, 1),
  c(0, 1, 1),
  c(0, 1, 1),
  c(1, 1, 0),
  c(1, 1, 0),
  c(1, 1, 0)
  
)
seasonal_period <- 12

selected_names_s_tlb <- sapply(
  seq_along(orders_s_tlb),
  function(i) {
    paste0(
      "SARIMA(",
      paste(orders_s_tlb[[i]], collapse = ","),
      ")(",
      paste(seasonal_orders_s_tlb[[i]], collapse = ","),
      ")[",
      seasonal_period,
      "]"
    )
  }
)

# Fit log SARIMA models
selected_models_s_tlb <- setNames(
  lapply(seq_along(orders_s_tlb), function(i) {
    arima(
      log(tlb_training),
      order = orders_s_tlb[[i]],
      seasonal = list(
        order = seasonal_orders_s_tlb[[i]],
        period = seasonal_period
      ),
      method = "ML",
      optim.control = list(maxit = 2000)
    )
  }),
  selected_names_s_tlb
)

# Check convergence: 0 means successful convergence
print(sapply(selected_models_s_tlb, function(model) model$code))

# Training fitted values: back to birth counts
train_predictions_s_tlb <- lapply(selected_models_s_tlb, function(model) {
  exp(log(tlb_training) - residuals(model))
})

# Test predictions: back to birth counts
test_predictions_s_tlb <- lapply(selected_models_s_tlb, function(model) {
  exp(
    predict(model, n.ahead = length(tlb_test))$pred
  )
})

# Colours and line types
colours_s_tlb <- c(
  "blue",
  "red",
  "darkgreen",
  "brown",
  "purple",
  "orange"
)

line_types <- c(1, 2, 3, 1, 2, 3)

# Horizontal axes
use_years_s_tlb <- is.ts(tlb_training) && is.ts(tlb_test)

if (use_years_s_tlb) {
  train_years <- as.numeric(time(tlb_training))
  test_years <- as.numeric(time(tlb_test))
} else {
  train_years <- seq_along(tlb_training)
  test_years <- length(tlb_training) + seq_along(tlb_test)
}

# Plotting function
plot_predictions_s_tlb <- function(actual, years, predictions, title, set) {
  
  actual <- as.numeric(actual)
  
  model_labels_s_tlb <- sapply(seq_along(selected_models_s_tlb), function(i) {
    
    predicted <- as.numeric(predictions[[i]])
    
    valid <- is.finite(actual) &
      is.finite(predicted) &
      actual != 0
    
    mape <- mean(
      abs((actual[valid] - predicted[valid]) / actual[valid])
    ) * 100
    
    sprintf(
      "%s | AIC: %.2f | %s MAPE: %.2f%%",
      selected_names_s_tlb[i],
      AIC(selected_models_s_tlb[[i]]),
      set,
      mape
    )
  })
  
  plot(
    years, actual,
    type = "l",
    col = "black",
    lwd = 3,
    ylim = range(c(actual, unlist(predictions)), finite = TRUE),
    xlab = if (use_years_s_tlb) "Year" else "Observation",
    ylab = "Total live births",
    main = title
  )
  
  for (i in seq_along(predictions)) {
    lines(
      years,
      as.numeric(predictions[[i]]),
      col = colours_s_tlb[i],
      lty = line_types[i],
      lwd = 2
    )
  }
  
  legend(
    "bottomleft",
    legend = c("Actual", model_labels_s_tlb),
    col = c("black", colours_s_tlb),
    lty = c(1, line_types),
    lwd = c(3, rep(2, length(selected_models_s_tlb))),
    cex = 0.45,
    bty = "n"
  )
}

# Training and test graphs on one screen
old_par_s_tlb <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

plot_predictions_s_tlb(
  tlb_training, train_years, train_predictions_s_tlb,
  "Training: Actual vs Fitted", "Train"
)

plot_predictions_s_tlb(
  tlb_test, test_years, test_predictions_s_tlb,
  "Test: Actual vs Prediction", "Test"
)

par(old_par_s_tlb)

################# THIS FOR MANUAL ERROR CALCULATIOn USE IF REQUIRE DONT DELETE #################

# actual <- as.numeric(tfr_test)
# predicted <- as.numeric(fc)
# errors <- actual - predicted
# 
# c(
#   RMSE = sqrt(mean(errors^2)),
#   MAE = mean(abs(errors)),
#   MAPE = mean(abs(errors / actual)) * 100
# )
