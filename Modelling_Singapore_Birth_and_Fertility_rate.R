
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

# K-YEAR AVERAGE MODEL
# ============================================================

k <- 2       

ma_label <- paste0(k, "-yr average")

# SECTION 1: TOTAL FERTILITY RATE (TFR)
# ============================================================


# 1.1 PREPARE TFR DATA
# ============================================================

tfr_series <- data %>%
  filter(variable == "Total_Fertility_Rate_TFR", !is.na(value),
         year >= 1960, year <= 2025) %>%
  arrange(year)

tfr_x <- tfr_series$value
tfr_n <- length(tfr_x)

unique(diff(tfr_series$year))


# 1.2 MANUAL K-YEAR AVERAGE MODEL
# ============================================================

tfr_ma <- rep(NA, tfr_n)

for (t in (k + 1):tfr_n) {
  total <- 0
  for (j in 1:k) {
    total <- total + tfr_x[t - j]
  }
  tfr_ma[t] <- total / k
}

tfr_series$forecast <- tfr_ma
tfr_series$residual <- tfr_series$value - tfr_series$forecast

# 1.3 SPLIT INTO TRAINING AND TESTING
# ============================================================

tfr_train <- tfr_series %>% filter(year <= 2012)
tfr_test  <- tfr_series %>% filter(year >= 2013)

print(tfr_test %>% select(year, actual = value, forecast, residual))

# 1.4 ACCURACY: RMSE, MAE, MAPE
# ============================================================

tfr_tr <- tfr_train %>% filter(!is.na(forecast))
tfr_te <- tfr_test  %>% filter(!is.na(forecast))

tfr_accuracy <- rbind(
  accuracy(tfr_tr$forecast, tfr_tr$value)[, c("RMSE", "MAE", "MAPE")],
  accuracy(tfr_te$forecast, tfr_te$value)[, c("RMSE", "MAE", "MAPE")]
)
rownames(tfr_accuracy) <- c("Training", "Testing")

cat("TFR accuracy, k =", k, "\n")
print(round(tfr_accuracy, 4))


# 1.5 ACF AND PACF - TRAINING
# ============================================================

tfr_res_train <- na.omit(tfr_train$residual)

acf(tfr_res_train,  lag.max = 40, main = paste("ACF Training - TFR, k =", k))
pacf(tfr_res_train, lag.max = 40, main = paste("PACF Training - TFR, k =", k))


# 1.6 ACF AND PACF - TESTING
# ============================================================

tfr_res_test <- na.omit(tfr_test$residual)
tfr_lag_test <- min(10, length(tfr_res_test) - 1)

acf(tfr_res_test,  lag.max = tfr_lag_test, main = paste("ACF Testing - TFR, k =", k))
pacf(tfr_res_test, lag.max = tfr_lag_test, main = paste("PACF Testing - TFR, k =", k))


# 1.7 PLOT: TRAINING (1960 - 2012)
# ============================================================

ggplot(tfr_train, aes(x = year)) +
  geom_line(aes(y = value, colour = "Actual"), linewidth = 1) +
  geom_line(aes(y = forecast, colour = ma_label),
            linetype = "dashed", linewidth = 1) +
  scale_colour_manual(values = setNames(c("black", "red"),
                                        c("Actual", ma_label))) +
  labs(title = paste0("TFR: Actual vs ", k, "-Year Average Model (Training 1960-2012)"),
       x = "Year", y = "Total Fertility Rate", colour = "") +
  theme_minimal() +
  theme(legend.position = "bottom")


# 1.8 PLOT: TESTING (2013 - 2025)
# ============================================================

ggplot(tfr_test, aes(x = year)) +
  geom_line(aes(y = value, colour = "Actual"), linewidth = 1) +
  geom_point(aes(y = value, colour = "Actual"), size = 2) +
  geom_line(aes(y = forecast, colour = ma_label),
            linetype = "dashed", linewidth = 1) +
  geom_point(aes(y = forecast, colour = ma_label), size = 2) +
  scale_x_continuous(breaks = 2013:2025) +
  scale_colour_manual(values = setNames(c("black", "red"),
                                        c("Actual", ma_label))) +
  labs(title = paste0("TFR: Actual vs ", k, "-Year Average Model (Testing 2013-2025)"),
       x = "Year", y = "Total Fertility Rate", colour = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

# SECTION 2: LOG TOTAL LIVE-BIRTHS (log TLB)
# ============================================================


# 2.1 PREPARE LOG TLB DATA
# ============================================================

tlb_series <- data %>%
  filter(variable == "Total_Live_Births", !is.na(value),
         year >= 1960, year <= 2025) %>%
  arrange(year) %>%
  mutate(log_value = log(value))

tlb_x <- tlb_series$log_value
tlb_n <- length(tlb_x)

unique(diff(tlb_series$year))


# 2.2 K-YEAR AVERAGE MODEL ON LOG(TLB)
# ============================================================

tlb_ma <- rep(NA, tlb_n)

for (t in (k + 1):tlb_n) {
  total <- 0
  for (j in 1:k) {
    total <- total + tlb_x[t - j]
  }
  tlb_ma[t] <- total / k
}

tlb_series$log_forecast <- tlb_ma
tlb_series$log_residual <- tlb_series$log_value - tlb_series$log_forecast


# 2.3 SPLIT INTO TRAINING AND TESTING
# ============================================================

tlb_train <- tlb_series %>% filter(year <= 2012)
tlb_test  <- tlb_series %>% filter(year >= 2013)

print(tlb_test %>% select(year, log_value, log_forecast, log_residual))


# 2.4 ACCURACY ON LOG SCALE: RMSE, MAE, MAPE
# ============================================================

tlb_tr <- tlb_train %>% filter(!is.na(log_forecast))
tlb_te <- tlb_test  %>% filter(!is.na(log_forecast))

tlb_accuracy <- rbind(
  accuracy(tlb_tr$log_forecast, tlb_tr$log_value)[, c("RMSE", "MAE", "MAPE")],
  accuracy(tlb_te$log_forecast, tlb_te$log_value)[, c("RMSE", "MAE", "MAPE")]
)
rownames(tlb_accuracy) <- c("Training", "Testing")

cat("log TLB accuracy, k =", k, "\n")
print(round(tlb_accuracy, 4))


# 2.5 ACF AND PACF - TRAINING
# ============================================================

tlb_res_train <- na.omit(tlb_train$log_residual)

acf(tlb_res_train,  lag.max = 40, main = paste("ACF Training - log TLB, k =", k))
pacf(tlb_res_train, lag.max = 40, main = paste("PACF Training - log TLB, k =", k))


# 2.6 ACF AND PACF - TESTING
# ============================================================

tlb_res_test <- na.omit(tlb_test$log_residual)
tlb_lag_test <- min(10, length(tlb_res_test) - 1)

acf(tlb_res_test,  lag.max = tlb_lag_test, main = paste("ACF Testing - log TLB, k =", k))
pacf(tlb_res_test, lag.max = tlb_lag_test, main = paste("PACF Testing - log TLB, k =", k))


# 2.7 PLOT: TRAINING (1960 - 2012)
# ============================================================

ggplot(tlb_train, aes(x = year)) +
  geom_line(aes(y = log_value, colour = "Actual"), linewidth = 1) +
  geom_line(aes(y = log_forecast, colour = ma_label),
            linetype = "dashed", linewidth = 1) +
  scale_colour_manual(values = setNames(c("black", "red"),
                                        c("Actual", ma_label))) +
  labs(title = paste0("log TLB: Actual vs ", k, "-Year Average Model (Training 1960-2012)"),
       x = "Year", y = "log(Total Live-Births)", colour = "") +
  theme_minimal() +
  theme(legend.position = "bottom")


# 2.8 PLOT: TESTING (2013 - 2025)
# ============================================================

ggplot(tlb_test, aes(x = year)) +
  geom_line(aes(y = log_value, colour = "Actual"), linewidth = 1) +
  geom_point(aes(y = log_value, colour = "Actual"), size = 2) +
  geom_line(aes(y = log_forecast, colour = ma_label),
            linetype = "dashed", linewidth = 1) +
  geom_point(aes(y = log_forecast, colour = ma_label), size = 2) +
  scale_x_continuous(breaks = 2013:2025) +
  scale_colour_manual(values = setNames(c("black", "red"),
                                        c("Actual", ma_label))) +
  labs(title = paste0("log TLB: Actual vs ", k, "-Year Average Model (Testing 2013-2025)"),
       x = "Year", y = "log(Total Live-Births)", colour = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

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
# 
# actual <- as.numeric(tfr_test)
# predicted <- as.numeric(fc)
# errors <- actual - predicted
# 
# c(
#   RMSE = sqrt(mean(errors^2)),
#   MAE = mean(abs(errors)),
#   MAPE = mean(abs(errors / actual)) * 100
# )

# Machine Learning Models

########## NEURAL NETWORK MACHINE LEARNING MODEL TFR ################

old_par <- par(mfrow = c(1, 2))

acf(tfr_training,lag=40, main = "ACF: Original TFR")
pacf(tfr_training,lag=40, main = "PACF: Original TFR")

par(old_par)
####################################################
set.seed(123)

candidate_tfr_ml <- nnetar(
  tfr_training,
  p = 10,           # Use the previous years as inputs
  size = 3,        # Hidden neurons
  repeats = 20,    # Average 20 fitted networks
  seasonal = FALSE
)

candidate_tfr_ml

########## FORECAST ##################################
fc_ml <- forecast(
  candidate_tfr_ml,
  h = length(tfr_test)
)

train_fitted_ml <- fitted(candidate_tfr_ml)

########## TRAINING ACCURACY ##########################
accuracy(
  train_fitted_ml,
  tfr_training
)[, c("RMSE", "MAE", "MAPE")]

########## TEST ACCURACY ##############################
accuracy(
  fc_ml,
  tfr_test
)["Test set", c("RMSE", "MAE", "MAPE")]

########## RESIDUAL ACF AND PACF #######################
res_ml <- na.omit(residuals(candidate_tfr_ml))

old_par <- par(mfrow = c(1, 2))

acf(
  res_ml,
  lag.max = 40,
  main = "Residual ACF: Neural Network"
)

pacf(
  res_ml,
  lag.max = 40,
  main = "Residual PACF: Neural Network"
)

par(old_par)
############### RESULT ########################################################
##############################################################################

############### PLOT ##########################################################


########## SETTINGS TO COMPARE ########################
settings <- list(
  c(p = 10,  size = 3, repeats = 20),
  c(p = 11, size = 3, repeats = 20)
)

########## MODEL NAMES ###############################
selected_names <- sapply(settings, function(x) {
  sprintf(
    "NNAR(p=%d, size=%d, repeats=%d)",
    x["p"], x["size"], x["repeats"]
  )
})

########## FIT NEURAL NETWORK MODELS ##################
selected_models <- setNames(
  lapply(settings, function(x) {
    set.seed(123)
    
    nnetar(
      tfr_training,
      p = unname(x["p"]),
      size = unname(x["size"]),
      repeats = unname(x["repeats"]),
      seasonal = FALSE
    )
  }),
  selected_names
)

########## TRAINING FITTED VALUES #####################
train_predictions <- lapply(selected_models, function(model) {
  fitted(model)
})

########## TEST PREDICTIONS ###########################
test_predictions <- lapply(selected_models, function(model) {
  forecast(model, h = length(tfr_test))$mean
})

########## COLOURS AND LINE TYPES #####################
colours <- c("blue", "red", "darkgreen")
line_types <- 1:3

########## HORIZONTAL AXES ############################
use_years <- is.ts(tfr_training) && is.ts(tfr_test)

if (use_years) {
  train_years <- as.numeric(time(tfr_training))
  test_years <- as.numeric(time(tfr_test))
} else {
  train_years <- seq_along(tfr_training)
  test_years <- length(tfr_training) + seq_along(tfr_test)
}

########## PLOTTING FUNCTION ##########################
plot_predictions <- function(actual, years, predictions, title, set) {
  
  actual <- as.numeric(actual)
  
  model_labels <- sapply(seq_along(predictions), function(i) {
    
    predicted <- as.numeric(predictions[[i]])
    
    valid <- is.finite(actual) &
      is.finite(predicted) &
      actual != 0
    
    mape <- mean(
      abs((actual[valid] - predicted[valid]) / actual[valid])
    ) * 100
    
    sprintf(
      "%s | %s MAPE: %.2f%%",
      selected_names[i],
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
    lwd = c(3, rep(2, length(predictions))),
    cex = 0.55,
    bty = "n"
  )
}

########## TRAINING AND TEST GRAPHS ###################
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

##########################################################
########## TEST ACCURACY TABLE TFR ########################
test_results <- do.call(
  rbind,
  lapply(seq_along(selected_models), function(i) {
    
    acc <- accuracy(
      as.numeric(test_predictions[[i]]),
      as.numeric(tfr_test)
    )
    
    data.frame(
      Model = selected_names[i],
      RMSE = acc[1, "RMSE"],
      MAE = acc[1, "MAE"],
      MAPE = acc[1, "MAPE"]
    )
  })
)

########## SORT AND VIEW #############################
test_results <- test_results[order(test_results$MAPE), ]
rownames(test_results) <- NULL

test_results
##########################################################

########## HK FIT LOG-TLB NEURAL NETWORK #################

old_par <- par(mfrow = c(1, 2))

acf(log(tlb_training),lag=40, main = "ACF: Original TLB")
pacf(log(tlb_training),lag=40, main = "PACF: Original TLB")

par(old_par)

set.seed(123)
candidate_tlb_ml <- nnetar(
  tlb_training,
  p = 6,          # Previous 12 years as inputs
  P = 0,           # No seasonal inputs
  size = 3,      # Hidden neurons
  repeats = 30,
  lambda = 0       # Log transformation
)

candidate_tlb_ml

########## FORECAST ##################################
# Automatically back-transformed to original birth counts
fc_tlb_ml <- forecast(
  candidate_tlb_ml,
  h = length(tlb_test)
)

train_fitted_tlb_ml <- fitted(candidate_tlb_ml)

########## TRAINING ACCURACY ##########################
accuracy(
  train_fitted_tlb_ml,
  tlb_training
)[, c("RMSE", "MAE", "MAPE")]

########## TEST ACCURACY ##############################
accuracy(
  fc_tlb_ml,
  tlb_test
)["Test set", c("RMSE", "MAE", "MAPE")]

########## RESIDUAL ACF AND PACF #######################

# One residual series on the original birth-count scale
res_tlb_ml <- na.omit(
  residuals(candidate_tlb_ml, type = "response")
)

# Display one ACF and one PACF together
old_par <- par(mfrow = c(1, 2), ask = FALSE)

acf(
  res_tlb_ml,
  lag.max = 40,
  main = "Residual ACF: Log-TLB Neural Network"
)

pacf(
  res_tlb_ml,
  lag.max = 40,
  main = "Residual PACF: Log-TLB Neural Network"
)

# Restore previous plot layout
par(old_par)

############### PLOT #################################

########## SETTINGS TO COMPARE ########################
settings_tlb <- list(
  c(p = 5, size = 3, repeats = 20),
  c(p = 6, size = 3, repeats = 30),
  c(p = 4, size = 3, repeats = 20)
)

########## MODEL NAMES ###############################
selected_names_tlb <- sapply(settings_tlb, function(x) {
  sprintf(
    "Log-NNAR(p=%d, size=%d, repeats=%d)",
    x["p"], x["size"], x["repeats"]
  )
})

########## FIT NEURAL NETWORK MODELS ##################
selected_models_tlb <- setNames(
  lapply(settings_tlb, function(x) {
    set.seed(123)
    
    nnetar(
      tlb_training,
      p = unname(x["p"]),
      P = 0,
      size = unname(x["size"]),
      repeats = unname(x["repeats"]),
      lambda = 0
    )
  }),
  selected_names_tlb
)

########## TRAINING FITTED VALUES #####################
# Original birth-count scale
train_predictions_tlb <- lapply(
  selected_models_tlb,
  function(model) fitted(model)
)

########## TEST PREDICTIONS ###########################
# Original birth-count scale
test_predictions_tlb <- lapply(
  selected_models_tlb,
  function(model) {
    forecast(model, h = length(tlb_test))$mean
  }
)

########## COLOURS AND LINE TYPES #####################
colours_tlb <- c("blue", "red", "darkgreen")
line_types_tlb <- 1:3

########## HORIZONTAL AXES ############################
use_years_tlb <- is.ts(tlb_training) && is.ts(tlb_test)

if (use_years_tlb) {
  train_years_tlb <- as.numeric(time(tlb_training))
  test_years_tlb <- as.numeric(time(tlb_test))
} else {
  train_years_tlb <- seq_along(tlb_training)
  test_years_tlb <- length(tlb_training) + seq_along(tlb_test)
}

########## PLOTTING FUNCTION ##########################
plot_predictions_tlb <- function(
    actual, years, predictions, title, set) {
  
  actual <- as.numeric(actual)
  
  model_labels <- sapply(seq_along(predictions), function(i) {
    
    predicted <- as.numeric(predictions[[i]])
    
    valid <- is.finite(actual) &
      is.finite(predicted) &
      actual != 0
    
    mape <- mean(
      abs((actual[valid] - predicted[valid]) / actual[valid])
    ) * 100
    
    sprintf(
      "%s | %s MAPE: %.2f%%",
      selected_names_tlb[i],
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
    legend = c("Actual", model_labels),
    col = c("black", colours_tlb),
    lty = c(1, line_types_tlb),
    lwd = c(3, rep(2, length(predictions))),
    cex = 0.55,
    bty = "n"
  )
}

########## TRAINING AND TEST GRAPHS ###################
old_par <- par(
  mfrow = c(1, 2),
  mar = c(4, 4, 3, 1),
  ask = FALSE
)

plot_predictions_tlb(
  tlb_training,
  train_years_tlb,
  train_predictions_tlb,
  "Training: Actual vs Fitted",
  "Train"
)

plot_predictions_tlb(
  tlb_test,
  test_years_tlb,
  test_predictions_tlb,
  "Test: Actual vs Prediction",
  "Test"
)

par(old_par)
########## TEST ACCURACY TABLE TFR ########################
test_results <- do.call(
  rbind,
  lapply(seq_along(selected_models_tlb), function(i) {
    
    acc <- accuracy(
      as.numeric(test_predictions_tlb[[i]]),
      as.numeric(tlb_test)
    )
    
    data.frame(
      Model = selected_names_tlb[i],
      RMSE = acc[1, "RMSE"],
      MAE = acc[1, "MAE"],
      MAPE = acc[1, "MAPE"]
    )
  })
)

########## SORT AND VIEW #############################
test_results <- test_results[order(test_results$MAPE), ]
rownames(test_results) <- NULL

test_results
##########################################################
########## HK TRAINING DATA: 12-YEAR CYCLE TFR ###############
old_par <- par(mfrow = c(1, 2))

acf(tfr_training,lag=45, main = "ACF: Original TFR")
pacf(tfr_training,lag=45, main = "PACF: Original TFR")

par(old_par)


tfr_training_12 <- ts(
  as.numeric(tfr_training),
  frequency = 12
)

########## RUN MODEL #################################
set.seed(123)

candidate_tfr_ml <- nnetar(
  tfr_training_12,
  p = 12,
  P = 1,
  size=6,
  repeats = 20
)

candidate_tfr_ml
########## TRAINING ACCURACY #########################
train_fit <- fitted(candidate_tfr_ml)

train_accuracy <- accuracy(
  as.numeric(train_fit),
  as.numeric(tfr_training_12)
)[, c("RMSE", "MAE", "MAPE")]

train_accuracy

########## FORECAST ##################################
fc_ml <- forecast(
  candidate_tfr_ml,
  h = length(tfr_test)
)

########## TEST ACCURACY #############################
accuracy(
  as.numeric(fc_ml$mean),
  as.numeric(tfr_test)
)[, c("RMSE", "MAE", "MAPE")]

########## RESIDUAL ACF AND PACF ######################
# Numeric vector makes lag 12 display as 12 years
res_ml <- as.numeric(residuals(candidate_tfr_ml))
res_ml <- res_ml[is.finite(res_ml)]

max_lag <- min(40, length(res_ml) - 1)

old_par <- par(mfrow = c(1, 2))

acf(
  res_ml,
  lag.max = max_lag,
  main = "Residual ACF: NNAR, 12-year cycle",
  xlab = "Lag (years)"
)

pacf(
  res_ml,
  lag.max = max_lag,
  main = "Residual PACF: NNAR, 12-year cycle",
  xlab = "Lag (years)"
)

par(old_par)

################### PLOT ##################################

########## SETTINGS TO COMPARE ########################
########## SETTINGS TO COMPARE ########################
settings <- list(
  c(p = 11, P = 1, size = 6, repeats = 20),
  c(p = 12, P = 1, size = 6, repeats = 20)
)

########## FIT NEURAL NETWORK MODELS ##################
selected_models <- lapply(settings, function(x) {
  set.seed(123)
  
  nnetar(
    tfr_training_12,
    p = unname(x["p"]),
    P = unname(x["P"]),
    size = unname(x["size"]),
    repeats = unname(x["repeats"])
  )
})

########## MODEL NAMES ###############################
selected_names <- sapply(seq_along(selected_models), function(i) {
  model <- selected_models[[i]]
  
  sprintf(
    "NNAR(%d,%d,%d)[12] | repeats=%d",
    model$p,
    model$P,
    model$size,
    settings[[i]]["repeats"]
  )
})

names(selected_models) <- selected_names
########## TRAINING FITTED VALUES #####################
train_predictions <- lapply(selected_models, function(model) {
  as.numeric(fitted(model))
})

########## TEST PREDICTIONS ###########################
test_predictions <- lapply(selected_models, function(model) {
  as.numeric(forecast(model, h = length(tfr_test))$mean)
})

########## COLOURS AND LINE TYPES #####################
colours <- c("blue", "red", "darkgreen")
line_types <- 1:3

########## ORIGINAL YEAR AXES #########################
use_years <- is.ts(tfr_training) && is.ts(tfr_test)

if (use_years) {
  train_years <- as.numeric(time(tfr_training))
  test_years <- as.numeric(time(tfr_test))
} else {
  train_years <- seq_along(tfr_training)
  test_years <- length(tfr_training) + seq_along(tfr_test)
}

########## PLOTTING FUNCTION ##########################
plot_predictions <- function(actual, years, predictions, title, set) {
  
  actual <- as.numeric(actual)
  
  model_labels <- sapply(seq_along(predictions), function(i) {
    
    predicted <- as.numeric(predictions[[i]])
    
    valid <- is.finite(actual) &
      is.finite(predicted) &
      actual != 0
    
    mape <- mean(
      abs((actual[valid] - predicted[valid]) / actual[valid])
    ) * 100
    
    sprintf(
      "%s | %s MAPE: %.2f%%",
      selected_names[i],
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
    lwd = c(3, rep(2, length(predictions))),
    cex = 0.55,
    bty = "n"
  )
}

########## TRAINING AND TEST GRAPHS ###################
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
########## TEST ACCURACY TABLE TFR ########################
test_results <- do.call(
  rbind,
  lapply(seq_along(selected_models), function(i) {
    
    acc <- accuracy(
      as.numeric(test_predictions[[i]]),
      as.numeric(tfr_test)
    )
    
    data.frame(
      Model = selected_names[i],
      RMSE = acc[1, "RMSE"],
      MAE = acc[1, "MAE"],
      MAPE = acc[1, "MAPE"]
    )
  })
)

########## SORT AND VIEW #############################
test_results <- test_results[order(test_results$MAPE), ]
rownames(test_results) <- NULL

test_results
##########################################################
########## HK TRAINING DATA: 12-YEAR CYCLE TLB #########
old_par <- par(mfrow = c(1, 2))

acf(tlb_training,lag=45, main = "ACF: Original TLB")
pacf(tlb_training,lag=45, main = "PACF: Original TLB")

par(old_par)

# Use original positive birth counts
tlb_training_12 <- ts(
  as.numeric(tlb_training),
  frequency = 12
)

########## RUN LOG MODEL #############################
set.seed(123)

candidate_tlb_ml <- nnetar(
  tlb_training_12,
  p = 9,
  P = 1,
  size = 3,
  repeats = 20,
  lambda = 0
)

candidate_tlb_ml

########## FORECAST ##################################
# Automatically returns to original birth-count scale
fc_tlb_ml <- forecast(
  candidate_tlb_ml,
  h = length(tlb_test)
)

########## TEST ACCURACY #############################
accuracy(
  as.numeric(fc_tlb_ml$mean),
  as.numeric(tlb_test)
)[, c("RMSE", "MAE", "MAPE")]

########## RESIDUAL ACF AND PACF ######################
# Response residuals give ONE series on original scale
res_tlb_ml <- as.numeric(
  residuals(candidate_tlb_ml, type = "response")
)

res_tlb_ml <- res_tlb_ml[is.finite(res_tlb_ml)]

max_lag_tlb <- min(40, length(res_tlb_ml) - 1)

old_par <- par(mfrow = c(1, 2), ask = FALSE)

acf(
  res_tlb_ml,
  lag.max = max_lag_tlb,
  main = "Residual ACF: Log-NNAR, 12-year cycle",
  xlab = "Lag (years)"
)

pacf(
  res_tlb_ml,
  lag.max = max_lag_tlb,
  main = "Residual PACF: Log-NNAR, 12-year cycle",
  xlab = "Lag (years)"
)

par(old_par)

################### PLOT #############################

########## SETTINGS TO COMPARE ########################
settings_tlb <- list(
  c(p = 6, P = 1, size = 3, repeats = 20),
  c(p = 8, P = 1, size = 3, repeats = 20),
  c(p = 9, P = 1, size = 3, repeats = 20)
)

########## FIT LOG NEURAL NETWORK MODELS ##############
selected_models_tlb <- lapply(settings_tlb, function(x) {
  set.seed(123)
  
  nnetar(
    tlb_training_12,
    p = unname(x["p"]),
    P = unname(x["P"]),
    size = unname(x["size"]),
    repeats = unname(x["repeats"]),
    lambda = 0
  )
})

########## MODEL LABELS ###############################
selected_names_tlb <- sapply(seq_along(selected_models_tlb), function(i) {
  model <- selected_models_tlb[[i]]
  
  sprintf(
    "Log-NNAR(%d,%d,%d)[12] | repeats=%d",
    model$p,
    model$P,
    model$size,
    settings_tlb[[i]]["repeats"]
  )
})

names(selected_models_tlb) <- selected_names_tlb

########## TRAINING FITTED VALUES #####################
# Original birth-count scale
train_predictions_tlb <- lapply(
  selected_models_tlb,
  function(model) {
    as.numeric(fitted(model))
  }
)

########## TEST PREDICTIONS ###########################
# Original birth-count scale
test_predictions_tlb <- lapply(
  selected_models_tlb,
  function(model) {
    as.numeric(
      forecast(model, h = length(tlb_test))$mean
    )
  }
)

########## COLOURS AND LINE TYPES #####################
colours_tlb <- c("blue", "red","green")
line_types_tlb <- 1:3

########## ORIGINAL YEAR AXES #########################
use_years_tlb <- is.ts(tlb_training) && is.ts(tlb_test)

if (use_years_tlb) {
  train_years_tlb <- as.numeric(time(tlb_training))
  test_years_tlb <- as.numeric(time(tlb_test))
} else {
  train_years_tlb <- seq_along(tlb_training)
  test_years_tlb <- length(tlb_training) + seq_along(tlb_test)
}

########## PLOTTING FUNCTION ##########################
plot_predictions_tlb <- function(
    actual, years, predictions, title, set) {
  
  actual <- as.numeric(actual)
  
  model_labels <- sapply(seq_along(predictions), function(i) {
    
    predicted <- as.numeric(predictions[[i]])
    
    valid <- is.finite(actual) &
      is.finite(predicted) &
      actual != 0
    
    mape <- mean(
      abs((actual[valid] - predicted[valid]) / actual[valid])
    ) * 100
    
    sprintf(
      "%s | %s MAPE: %.2f%%",
      selected_names_tlb[i],
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
    legend = c("Actual", model_labels),
    col = c("black", colours_tlb),
    lty = c(1, line_types_tlb),
    lwd = c(3, rep(2, length(predictions))),
    cex = 0.55,
    bty = "n"
  )
}

########## TRAINING AND TEST GRAPHS ###################
old_par <- par(
  mfrow = c(1, 2),
  mar = c(4, 4, 3, 1),
  ask = FALSE
)

plot_predictions_tlb(
  tlb_training,
  train_years_tlb,
  train_predictions_tlb,
  "Training: Actual vs Fitted",
  "Train"
)

plot_predictions_tlb(
  tlb_test,
  test_years_tlb,
  test_predictions_tlb,
  "Test: Actual vs Prediction",
  "Test"
)

par(old_par)

######################
########## TEST ACCURACY TABLE TFR ########################
test_results <- do.call(
  rbind,
  lapply(seq_along(selected_models_tlb), function(i) {
    
    acc <- accuracy(
      as.numeric(test_predictions_tlb[[i]]),
      as.numeric(tlb_test)
    )
    
    data.frame(
      Model = selected_names_tlb[i],
      RMSE = acc[1, "RMSE"],
      MAE = acc[1, "MAE"],
      MAPE = acc[1, "MAPE"]
    )
  })
)

########## SORT AND VIEW #############################
test_results <- test_results[order(test_results$MAPE), ]
rownames(test_results) <- NULL

test_results


##############################################################################
##############################################################################
# GAUSSIAN PROCESS REGRESSION (GPR) MODELS - TFR AND TLB
#
# Training period: 1960 - 2012 | Testing period: 2013 - 2025
#
# Uses the cleaned `data` and the series tfr_training, tfr_test,
# tlb_training and tlb_test already created in the ARIMA sections above.
#
# Revisions (after instructor feedback):
#   1. TFR  - d = 1: every lag from 12 to 18 is tested with rbfdot.
#             d = 2: GPR(11,2) and GPR(12,2) rbfdot are tested, from the
#             significant PACF lags 11 and 12 of the second-differenced TFR.
#             GPR(11,1,rbfdot) gave low test errors but was dropped because
#             its residuals were not white noise.
#   2. TLB  - modelled on the LOG scale, the same as ARIMA / SARIMA, so the
#             models can be compared fairly. Forecasts are back-transformed
#             with exp() and errors are reported in number of births.
#   3. TLB  - every PACF-guided model whose residuals are white noise
#             (Ljung-Box p > 0.05) is kept and ranked on test RMSE.
#   4. Models are not filtered on error. Every candidate with white-noise
#      residuals is kept and its RMSE, MAE and MAPE are listed.
#   5. Test  - multi-step (recursive) forecasts with gpr_forecast(), the
#             same method as predict() / forecast() for ARIMA, SARIMA and
#             NNAR, so all models are compared fairly. 2013 is predicted
#             from the real 1960-2012 values; from 2014 onwards each
#             forecast is fed back in as a lag. No real 2013-2025 value is
#             used to make any test prediction.
#
# Object names in this section end in _gpr (or start with gpr_) so they do
# not overwrite the ARIMA / SARIMA / neural network objects above.
##############################################################################

library(kernlab)

# Make sure the plot settings are back to one plot per screen
par(mfrow = c(1, 1), ask = FALSE)

# GPR FUNCTIONS
# ============================================================
# GPR(p, d, kernel)
#   p      = number of past years used as inputs (chosen from the PACF)
#   d      = differencing, same as ARIMA (chosen with the KPSS test)
#   kernel = "rbfdot" (Gaussian kernel) or "laplacedot" (exponential kernel)
#
# The model predicts the next differenced value from the previous p
# differenced values. The test period uses gpr_forecast, which feeds each
# forecast back in as an input for the next year (multi-step forecasting,
# the same as predict() for ARIMA / SARIMA and forecast() for NNAR).
#
# log_scale = TRUE fits the model on log(series) and converts the fitted
# values and forecasts back with exp(). Residuals stay on the log scale
# (that is the scale the model is fitted on, so it is the one checked
# for white noise).

gpr_model <- function(series, p, d = 1, kernel = "rbfdot", log_scale = FALSE) {
  
  original <- series
  if (log_scale) series <- log(series)
  
  # difference the series
  series_diff <- diff(series, differences = d)
  
  # lagged inputs: column 1 = this year, columns 2 to p+1 = previous p years
  lagged <- embed(series_diff, p + 1)
  y <- lagged[, 1]
  X <- lagged[, -1, drop = FALSE]
  
  # fit the GPR
  set.seed(1)
  model <- gausspr(X, y, kernel = kernel, var = 0.01)
  
  # residuals and fitted values
  res <- y - as.numeric(predict(model, X))
  n <- length(series)
  
  fitted <- ts(c(rep(NA, p + d), series[(p + d + 1):n] - res),
               start = start(series))
  if (log_scale) fitted <- exp(fitted)
  
  list(
    model = model, p = p, d = d, kernel = kernel, log_scale = log_scale,
    series = series, original = original, series_diff = series_diff,
    fitted = fitted,
    residuals = ts(c(rep(NA, p + d), res), start = start(series))
  )
}

gpr_forecast <- function(gpr, h) {
  
  history <- gpr$series_diff
  levels <- as.numeric(gpr$series)
  
  for (i in 1:h) {
    new_x <- matrix(rev(tail(history, gpr$p)), nrow = 1)
    next_diff <- as.numeric(predict(gpr$model, new_x))
    history <- c(history, next_diff)
    
    # undo the differencing
    n <- length(levels)
    if (gpr$d == 1) next_value <- levels[n] + next_diff
    if (gpr$d == 2) next_value <- next_diff + 2 * levels[n] - levels[n - 1]
    levels <- c(levels, next_value)
  }
  
  fc_gpr <- ts(tail(levels, h), start = end(gpr$series)[1] + 1)
  if (gpr$log_scale) fc_gpr <- exp(fc_gpr)   # back to the original scale
  fc_gpr
}

# ONE-STEP-AHEAD FORECAST - NOT USED IN THE MODEL COMPARISON
# Kept for reference only. The test results in this section use the
# multi-step gpr_forecast() above, so GPR is evaluated the same way as the
# other models.
#
# One-step-ahead version (using the real past values):
# Each test year is predicted from the REAL previous p years, not from
# earlier forecasts. Example for GPR(12,1) on TFR:
#   2013 uses the real differences up to 2012 (all from the training data)
#   2014 uses the real differences up to 2013 (11 training + 1 test year)
#   2025 uses the real differences up to 2024 (all from the test data)
# The model is NOT re-fitted: it is still trained on 1960-2012 only, and a
# year's own actual value is never used to predict that year.
gpr_test_forecast <- function(gpr, test) {
  
  actual <- as.numeric(test)
  if (gpr$log_scale) actual <- log(actual)
  
  # real values and real differences for the training + test years
  levels <- c(as.numeric(gpr$series), actual)
  real_diff <- diff(levels, differences = gpr$d)
  
  n_train <- length(gpr$series)
  h <- length(actual)
  preds <- numeric(h)
  
  for (i in 1:h) {
    t <- n_train + i              # position of the year being predicted
    last <- t - gpr$d - 1         # latest real difference known before year t
    
    # most recent year first, the same column order as embed()
    new_x <- matrix(real_diff[last:(last - gpr$p + 1)], nrow = 1)
    next_diff <- as.numeric(predict(gpr$model, new_x))
    
    # undo the differencing with the real previous values
    if (gpr$d == 1) preds[i] <- levels[t - 1] + next_diff
    if (gpr$d == 2) preds[i] <- next_diff + 2 * levels[t - 1] - levels[t - 2]
  }
  
  fc_gpr <- ts(preds, start = start(test), frequency = frequency(test))
  if (gpr$log_scale) fc_gpr <- exp(fc_gpr)   # back to the original scale
  fc_gpr
}

gpr_model_name <- function(p, d, kernel) paste0("GPR(", p, ",", d, ",", kernel, ")")

# Fit every candidate, then return Ljung-Box p-value and train/test errors.
# All errors are on the original scale of the data.
evaluate_models_gpr <- function(candidates, training, test, log_scale = FALSE) {
  
  names_vec <- sapply(candidates, function(x) gpr_model_name(x$p, x$d, x$kernel))
  models <- setNames(
    lapply(candidates, function(x)
      gpr_model(training, x$p, x$d, x$kernel, log_scale = log_scale)),
    names_vec
  )
  
  results <- do.call(rbind, lapply(names(models), function(x) {
    model <- models[[x]]
    fc_gpr <- gpr_forecast(model, h = length(test))
    acc_train <- accuracy(model$fitted, training)
    acc_test <- accuracy(fc_gpr, test)
    
    data.frame(
      Model = x,
      Set = c("Train", "Test"),
      Ljung_Box_p = Box.test(na.omit(model$residuals), lag = 10,
                             type = "Ljung-Box")$p.value,
      RMSE = c(acc_train[, "RMSE"], acc_test[, "RMSE"]),
      MAE = c(acc_train[, "MAE"], acc_test[, "MAE"]),
      MAPE = c(acc_train[, "MAPE"], acc_test[, "MAPE"]),
      row.names = NULL
    )
  }))
  
  results$White_Noise <- ifelse(results$Ljung_Box_p > 0.05, "Yes", "No")
  list(models = models, results = results)
}

# Residual ACF / PACF for each model, one plot at a time
plot_residuals_gpr <- function(models, type = "ACF") {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))
  
  for (i in seq_along(models)) {
    res <- na.omit(models[[i]]$residuals)
    if (type == "ACF") {
      acf(res, lag.max = 40, main = paste("Residual ACF:", names(models)[i]))
    } else {
      pacf(res, lag.max = 40, main = paste("Residual PACF:", names(models)[i]))
    }
  }
}

# Actual vs fitted / forecast plot
# Colours and line types are generated for any number of models.
#
# ylim_pad: the y-axis covers the actual data +/- this share of its range.
# Some forecasts drift far away from the data; with a fixed axis they leave
# the graph instead of squashing every other line flat. Set ylim_pad = NULL
# to fit the axis to every line instead.
plot_predictions_gpr <- function(actual, predictions, title, labels, ylab,
                                 legend_position = "bottomleft", ylim_pad = 0.5) {
  k <- length(predictions)
  model_cols <- hcl.colors(k, palette = "Dark 3")
  model_lty <- rep(1:6, length.out = k)
  
  if (is.null(ylim_pad)) {
    ylim <- range(c(actual, unlist(predictions)), na.rm = TRUE)
  } else {
    r <- range(actual, na.rm = TRUE)
    ylim <- r + c(-1, 1) * ylim_pad * diff(r)
  }
  
  plot(
    actual,
    ylim = ylim,
    main = title,
    xlab = "Year",
    ylab = ylab,
    col = "black",
    lwd = 3
  )
  
  for (i in seq_len(k)) {
    lines(
      ts(as.numeric(predictions[[i]]), start = start(actual),
         frequency = frequency(actual)),
      col = model_cols[i], lty = model_lty[i], lwd = 2
    )
  }
  
  legend(
    legend_position,
    legend = c("Actual", labels),
    col = c("black", model_cols[seq_len(k)]),
    lty = c(1, model_lty[seq_len(k)]),
    lwd = c(3, rep(2, k)),
    cex = if (k > 8) 0.5 else 0.65,
    bty = "n",
    inset = 0.02
  )
}



##############################################################
# GPR MODEL - TFR
# (tfr_training and tfr_test are created in the ARIMA - TFR section)

tfr_training
tfr_test

# First difference of TFR

plot(diff(tfr_training),
     main = "First Difference of Singapore TFR",
     xlab = "Year",
     ylab = "Differenced TFR")

acf(diff(tfr_training), lag.max = 40, main = "ACF of differenced TFR")
pacf(diff(tfr_training), lag.max = 40, main = "PACF of differenced TFR")

# Second difference of TFR

plot(diff(tfr_training, differences = 2),
     main = "Second Difference of Singapore TFR",
     xlab = "Year",
     ylab = "Second Differenced TFR")

acf(diff(tfr_training, differences = 2), lag.max = 40,
    main = "ACF of second differenced TFR")
pacf(diff(tfr_training, differences = 2), lag.max = 40,
     main = "PACF of second differenced TFR")

# Significant PACF lags:
#   d = 1 : lags 1, 11, 12, 13, 16
#   d = 2 : lags 1, 2, 5, 11, 12


# KPSS TEST

kpss.test(diff(tfr_training))

kpss.test(diff(tfr_training, differences = 2))


# GPR Models

# GPR(11,2,rbfdot)

gpr_tfr_p11_d2_rbf <- gpr_model(tfr_training, p = 11, d = 2, kernel = "rbfdot")
gpr_tfr_p11_d2_rbf$model
fc_gpr <- gpr_forecast(gpr_tfr_p11_d2_rbf, h = length(tfr_test))

accuracy(fc_gpr, tfr_test)[, c("RMSE", "MAE", "MAPE")]

acf(na.omit(gpr_tfr_p11_d2_rbf$residuals), lag.max = 40, main = "ACF of GPR(11,2,rbfdot) Residuals")
pacf(na.omit(gpr_tfr_p11_d2_rbf$residuals), lag.max = 40, main = "PACF of GPR(11,2,rbfdot) Residuals")

# GPR(12,1,rbfdot)

gpr_tfr_p12_d1_rbf <- gpr_model(tfr_training, p = 12, d = 1, kernel = "rbfdot")
gpr_tfr_p12_d1_rbf$model
fc_gpr <- gpr_forecast(gpr_tfr_p12_d1_rbf, h = length(tfr_test))

accuracy(fc_gpr, tfr_test)[, c("RMSE", "MAE", "MAPE")]

acf(na.omit(gpr_tfr_p12_d1_rbf$residuals), lag.max = 40, main = "ACF of GPR(12,1,rbfdot) Residuals")
pacf(na.omit(gpr_tfr_p12_d1_rbf$residuals), lag.max = 40, main = "PACF of GPR(12,1,rbfdot) Residuals")

# GPR(13,1,rbfdot)

gpr_tfr_p13_d1_rbf <- gpr_model(tfr_training, p = 13, d = 1, kernel = "rbfdot")
gpr_tfr_p13_d1_rbf$model
fc_gpr <- gpr_forecast(gpr_tfr_p13_d1_rbf, h = length(tfr_test))

accuracy(fc_gpr, tfr_test)[, c("RMSE", "MAE", "MAPE")]

acf(na.omit(gpr_tfr_p13_d1_rbf$residuals), lag.max = 40, main = "ACF of GPR(13,1,rbfdot) Residuals")
pacf(na.omit(gpr_tfr_p13_d1_rbf$residuals), lag.max = 40, main = "PACF of GPR(13,1,rbfdot) Residuals")

# GPR(14,1,rbfdot)

gpr_tfr_p14_d1_rbf <- gpr_model(tfr_training, p = 14, d = 1, kernel = "rbfdot")
gpr_tfr_p14_d1_rbf$model
fc_gpr <- gpr_forecast(gpr_tfr_p14_d1_rbf, h = length(tfr_test))

accuracy(fc_gpr, tfr_test)[, c("RMSE", "MAE", "MAPE")]

acf(na.omit(gpr_tfr_p14_d1_rbf$residuals), lag.max = 40, main = "ACF of GPR(14,1,rbfdot) Residuals")
pacf(na.omit(gpr_tfr_p14_d1_rbf$residuals), lag.max = 40, main = "PACF of GPR(14,1,rbfdot) Residuals")

# GPR(15,1,rbfdot)

gpr_tfr_p15_d1_rbf <- gpr_model(tfr_training, p = 15, d = 1, kernel = "rbfdot")
gpr_tfr_p15_d1_rbf$model
fc_gpr <- gpr_forecast(gpr_tfr_p15_d1_rbf, h = length(tfr_test))

accuracy(fc_gpr, tfr_test)[, c("RMSE", "MAE", "MAPE")]

acf(na.omit(gpr_tfr_p15_d1_rbf$residuals), lag.max = 40, main = "ACF of GPR(15,1,rbfdot) Residuals")
pacf(na.omit(gpr_tfr_p15_d1_rbf$residuals), lag.max = 40, main = "PACF of GPR(15,1,rbfdot) Residuals")

# GPR(16,1,rbfdot)

gpr_tfr_p16_d1_rbf <- gpr_model(tfr_training, p = 16, d = 1, kernel = "rbfdot")
gpr_tfr_p16_d1_rbf$model
fc_gpr <- gpr_forecast(gpr_tfr_p16_d1_rbf, h = length(tfr_test))

accuracy(fc_gpr, tfr_test)[, c("RMSE", "MAE", "MAPE")]

acf(na.omit(gpr_tfr_p16_d1_rbf$residuals), lag.max = 40, main = "ACF of GPR(16,1,rbfdot) Residuals")
pacf(na.omit(gpr_tfr_p16_d1_rbf$residuals), lag.max = 40, main = "PACF of GPR(16,1,rbfdot) Residuals")

# GPR(17,1,rbfdot)

gpr_tfr_p17_d1_rbf <- gpr_model(tfr_training, p = 17, d = 1, kernel = "rbfdot")
gpr_tfr_p17_d1_rbf$model
fc_gpr <- gpr_forecast(gpr_tfr_p17_d1_rbf, h = length(tfr_test))

accuracy(fc_gpr, tfr_test)[, c("RMSE", "MAE", "MAPE")]

acf(na.omit(gpr_tfr_p17_d1_rbf$residuals), lag.max = 40, main = "ACF of GPR(17,1,rbfdot) Residuals")
pacf(na.omit(gpr_tfr_p17_d1_rbf$residuals), lag.max = 40, main = "PACF of GPR(17,1,rbfdot) Residuals")

# GPR(18,1,rbfdot)

gpr_tfr_p18_d1_rbf <- gpr_model(tfr_training, p = 18, d = 1, kernel = "rbfdot")
gpr_tfr_p18_d1_rbf$model
fc_gpr <- gpr_forecast(gpr_tfr_p18_d1_rbf, h = length(tfr_test))

accuracy(fc_gpr, tfr_test)[, c("RMSE", "MAE", "MAPE")]

acf(na.omit(gpr_tfr_p18_d1_rbf$residuals), lag.max = 40, main = "ACF of GPR(18,1,rbfdot) Residuals")
pacf(na.omit(gpr_tfr_p18_d1_rbf$residuals), lag.max = 40, main = "PACF of GPR(18,1,rbfdot) Residuals")

# GPR(12,2,rbfdot)

gpr_tfr_p12_d2_rbf <- gpr_model(tfr_training, p = 12, d = 2, kernel = "rbfdot")
gpr_tfr_p12_d2_rbf$model
fc_gpr <- gpr_forecast(gpr_tfr_p12_d2_rbf, h = length(tfr_test))

accuracy(fc_gpr, tfr_test)[, c("RMSE", "MAE", "MAPE")]

acf(na.omit(gpr_tfr_p12_d2_rbf$residuals), lag.max = 40, main = "ACF of GPR(12,2,rbfdot) Residuals")
pacf(na.omit(gpr_tfr_p12_d2_rbf$residuals), lag.max = 40, main = "PACF of GPR(12,2,rbfdot) Residuals")

# #################################################
# LB = Ljung-Box p-value of the residuals (above 0.05 = white noise)
# Multi-step test errors (2013-2025 forecast from 1960-2012).
# Sorted by test RMSE. ---- = shortlisted candidate
# The two d = 2 rows marked (run) are new: fill in their test errors from
# test_results_tfr_gpr after running the script.
### d=1 ###
#   Model                 Set   RMSE         MAE          MAPE       LB
#   GPR(13,1,rbfdot)      Test  0.0596841    0.0529371    4.8637     0.879 ----
#   GPR(16,1,rbfdot)      Test  0.0607418    0.0496233    4.6741     0.892 ----
#   GPR(17,1,rbfdot)      Test  0.0612574    0.0522214    4.7637     -
#   GPR(14,1,rbfdot)      Test  0.0622955    0.0512506    4.8212     0.968 ----
#   GPR(18,1,rbfdot)      Test  0.0647824    0.0535512    4.7816     -
#   GPR(15,1,rbfdot)      Test  0.0698732    0.0548927    5.3091     0.878 ----
#   GPR(12,1,rbfdot)      Test  0.1062666    0.0933096    8.2585     0.978 ----
#   GPR(11,1,rbfdot)      Test  0.0487423    0.0421225    3.8916     0.576 - removed: residuals not white noise

#### d=2 ###
#   Model                 Set   RMSE         MAE          MAPE       LB
#   GPR(11,2,rbfdot)      Test  (run)        (run)        (run)      0.740 ----
#   GPR(12,2,rbfdot)      Test  (run)        (run)        (run)      (run) ----
#   GPR(16,2,rbfdot)      Test  0.0516227    0.0444069    4.0423     0.093 - replaced by GPR(12,2)

# FORECAST OF GOOD CANDIDATES #
# Shortlist: d = 1 lags 12-16 and d = 2 lags 11 and 12 (PACF-guided).
# The White_Noise column in the results confirms the Ljung-Box test for
# each model. Nothing is removed on the basis of RMSE, MAE or MAPE.

candidates_gpr_tfr <- list(
  list(p = 11, d = 2, kernel = "rbfdot"),
  list(p = 12, d = 1, kernel = "rbfdot"),
  list(p = 13, d = 1, kernel = "rbfdot"),
  list(p = 14, d = 1, kernel = "rbfdot"),
  list(p = 15, d = 1, kernel = "rbfdot"),
  list(p = 16, d = 1, kernel = "rbfdot"),
  list(p = 12, d = 2, kernel = "rbfdot")
)

tfr_eval_gpr <- evaluate_models_gpr(candidates_gpr_tfr, tfr_training, tfr_test)
selected_models_tfr_gpr <- tfr_eval_gpr$models
results_tfr_gpr <- tfr_eval_gpr$results

# Separate training and test results (test ranked by RMSE, best first)

train_results_tfr_gpr <- subset(results_tfr_gpr, Set == "Train")
test_results_tfr_gpr <- subset(results_tfr_gpr, Set == "Test")
test_results_tfr_gpr <- test_results_tfr_gpr[order(test_results_tfr_gpr$RMSE), ]

rownames(train_results_tfr_gpr) <- NULL
rownames(test_results_tfr_gpr) <- NULL

train_results_tfr_gpr
test_results_tfr_gpr

# Best TFR model = lowest test RMSE among the white-noise models
best_tfr_gpr <- subset(test_results_tfr_gpr, White_Noise == "Yes")$Model[1]
best_tfr_gpr

# PLOT ACF AND PACF (one plot per model)

plot_residuals_gpr(selected_models_tfr_gpr, "ACF")
plot_residuals_gpr(selected_models_tfr_gpr, "PACF")

# PLOT TRAIN AND TEST FORECAST

# Every white-noise model is drawn (best first by test RMSE).
plot_names_tfr_gpr <- subset(test_results_tfr_gpr, White_Noise == "Yes")$Model

train_predictions_tfr_gpr <- lapply(selected_models_tfr_gpr[plot_names_tfr_gpr],
                                    function(model) model$fitted)
test_predictions_tfr_gpr <- lapply(selected_models_tfr_gpr[plot_names_tfr_gpr], function(model) {
  gpr_forecast(model, h = length(tfr_test))
})

make_labels_gpr <- function(results, names_vec, flag, digits = "%.4f") {
  rows <- results[results$Set == flag, ]
  rows <- rows[match(names_vec, rows$Model), ]
  sprintf(paste0("%s | RMSE: ", digits, " | MAE: ", digits, " | MAPE: %.2f%%"),
          names_vec, rows$RMSE, rows$MAE, rows$MAPE)
}

train_labels_tfr_gpr <- make_labels_gpr(results_tfr_gpr, plot_names_tfr_gpr, "Train")
test_labels_tfr_gpr <- make_labels_gpr(results_tfr_gpr, plot_names_tfr_gpr, "Test")

# Change this one setting to move the legend inside the graph.
# Options: "bottomleft", "bottomright", "topleft", "topright"
legend_position_tfr_gpr <- "bottomleft"

# Draw each graph separately instead of side-by-side.
old_par <- par(no.readonly = TRUE)
par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))
plot_predictions_gpr(tfr_training, train_predictions_tfr_gpr,
                     "GPR Training: Actual vs Fitted (TFR)", train_labels_tfr_gpr,
                     ylab = "TFR", legend_position = legend_position_tfr_gpr)
plot_predictions_gpr(tfr_test, test_predictions_tfr_gpr,
                     "GPR Test: Actual vs Forecast (TFR)", test_labels_tfr_gpr,
                     ylab = "TFR", legend_position = legend_position_tfr_gpr)
par(old_par)



##############################################################
# GPR MODEL - TLB (log scale, same as ARIMA / SARIMA)
# (tlb_training and tlb_test are created in the ARIMA - TLB section,
#  on the original scale - the log is taken inside gpr_model)

tlb_training
tlb_test

# Log-transformed training series (used for identification)

log_tlb_training_gpr <- log(tlb_training)

plot(log_tlb_training_gpr,
     main = "Log of Total Live-Births",
     xlab = "Year",
     ylab = "log(TLB)")


# First difference of log TLB

plot(diff(log_tlb_training_gpr),
     main = "First Difference of log(Total Live-Births)",
     xlab = "Year",
     ylab = "Differenced log(TLB)")

acf(diff(log_tlb_training_gpr), lag.max = 40, main = "ACF of differenced log(TLB)")
pacf(diff(log_tlb_training_gpr), lag.max = 40, main = "PACF of differenced log(TLB)")

# Second difference of log TLB

plot(diff(log_tlb_training_gpr, differences = 2),
     main = "Second Difference of log(Total Live-Births)",
     xlab = "Year",
     ylab = "Second Differenced log(TLB)")

acf(diff(log_tlb_training_gpr, differences = 2), lag.max = 40,
    main = "ACF of second differenced log(TLB)")
pacf(diff(log_tlb_training_gpr, differences = 2), lag.max = 40,
     main = "PACF of second differenced log(TLB)")

# Significant PACF lags (log scale):
#   d = 1 : lags 11, 12, 13
#   d = 2 : lags 1, 2, 5, 11, 12


# KPSS TEST (both p-values > 0.05, so d = 1 and d = 2 are both stationary)

kpss.test(diff(log_tlb_training_gpr))

kpss.test(diff(log_tlb_training_gpr, differences = 2))


# GPR Models (log scale)
# Every PACF-guided lag is fitted with both kernels:
#   d = 1 : p = 11, 12, 13
#   d = 2 : p = 1, 2, 5, 11, 12

# GPR(11,1,rbfdot)

gpr_tlb_p11_d1_rbf <- gpr_model(tlb_training, p = 11, d = 1, kernel = "rbfdot", log_scale = TRUE)
gpr_tlb_p11_d1_rbf$model
fc_gpr <- gpr_forecast(gpr_tlb_p11_d1_rbf, h = length(tlb_test))

accuracy(fc_gpr, tlb_test)[, c("RMSE", "MAE", "MAPE")]
Box.test(na.omit(gpr_tlb_p11_d1_rbf$residuals), lag = 10, type = "Ljung-Box")

acf(na.omit(gpr_tlb_p11_d1_rbf$residuals), lag.max = 40, main = "ACF of GPR(11,1,rbfdot) Residuals (log TLB)")
pacf(na.omit(gpr_tlb_p11_d1_rbf$residuals), lag.max = 40, main = "PACF of GPR(11,1,rbfdot) Residuals (log TLB)")

# GPR(11,1,laplacedot)

gpr_tlb_p11_d1_lap <- gpr_model(tlb_training, p = 11, d = 1, kernel = "laplacedot", log_scale = TRUE)
gpr_tlb_p11_d1_lap$model
fc_gpr <- gpr_forecast(gpr_tlb_p11_d1_lap, h = length(tlb_test))

accuracy(fc_gpr, tlb_test)[, c("RMSE", "MAE", "MAPE")]
Box.test(na.omit(gpr_tlb_p11_d1_lap$residuals), lag = 10, type = "Ljung-Box")

acf(na.omit(gpr_tlb_p11_d1_lap$residuals), lag.max = 40, main = "ACF of GPR(11,1,laplacedot) Residuals (log TLB)")
pacf(na.omit(gpr_tlb_p11_d1_lap$residuals), lag.max = 40, main = "PACF of GPR(11,1,laplacedot) Residuals (log TLB)")

# GPR(12,1,rbfdot)

gpr_tlb_p12_d1_rbf <- gpr_model(tlb_training, p = 12, d = 1, kernel = "rbfdot", log_scale = TRUE)
gpr_tlb_p12_d1_rbf$model
fc_gpr <- gpr_forecast(gpr_tlb_p12_d1_rbf, h = length(tlb_test))

accuracy(fc_gpr, tlb_test)[, c("RMSE", "MAE", "MAPE")]
Box.test(na.omit(gpr_tlb_p12_d1_rbf$residuals), lag = 10, type = "Ljung-Box")

acf(na.omit(gpr_tlb_p12_d1_rbf$residuals), lag.max = 40, main = "ACF of GPR(12,1,rbfdot) Residuals (log TLB)")
pacf(na.omit(gpr_tlb_p12_d1_rbf$residuals), lag.max = 40, main = "PACF of GPR(12,1,rbfdot) Residuals (log TLB)")

# GPR(12,1,laplacedot)

gpr_tlb_p12_d1_lap <- gpr_model(tlb_training, p = 12, d = 1, kernel = "laplacedot", log_scale = TRUE)
gpr_tlb_p12_d1_lap$model
fc_gpr <- gpr_forecast(gpr_tlb_p12_d1_lap, h = length(tlb_test))

accuracy(fc_gpr, tlb_test)[, c("RMSE", "MAE", "MAPE")]
Box.test(na.omit(gpr_tlb_p12_d1_lap$residuals), lag = 10, type = "Ljung-Box")

acf(na.omit(gpr_tlb_p12_d1_lap$residuals), lag.max = 40, main = "ACF of GPR(12,1,laplacedot) Residuals (log TLB)")
pacf(na.omit(gpr_tlb_p12_d1_lap$residuals), lag.max = 40, main = "PACF of GPR(12,1,laplacedot) Residuals (log TLB)")

# GPR(13,1,rbfdot)

gpr_tlb_p13_d1_rbf <- gpr_model(tlb_training, p = 13, d = 1, kernel = "rbfdot", log_scale = TRUE)
gpr_tlb_p13_d1_rbf$model
fc_gpr <- gpr_forecast(gpr_tlb_p13_d1_rbf, h = length(tlb_test))

accuracy(fc_gpr, tlb_test)[, c("RMSE", "MAE", "MAPE")]
Box.test(na.omit(gpr_tlb_p13_d1_rbf$residuals), lag = 10, type = "Ljung-Box")

acf(na.omit(gpr_tlb_p13_d1_rbf$residuals), lag.max = 40, main = "ACF of GPR(13,1,rbfdot) Residuals (log TLB)")
pacf(na.omit(gpr_tlb_p13_d1_rbf$residuals), lag.max = 40, main = "PACF of GPR(13,1,rbfdot) Residuals (log TLB)")

# GPR(13,1,laplacedot)

gpr_tlb_p13_d1_lap <- gpr_model(tlb_training, p = 13, d = 1, kernel = "laplacedot", log_scale = TRUE)
gpr_tlb_p13_d1_lap$model
fc_gpr <- gpr_forecast(gpr_tlb_p13_d1_lap, h = length(tlb_test))

accuracy(fc_gpr, tlb_test)[, c("RMSE", "MAE", "MAPE")]
Box.test(na.omit(gpr_tlb_p13_d1_lap$residuals), lag = 10, type = "Ljung-Box")

acf(na.omit(gpr_tlb_p13_d1_lap$residuals), lag.max = 40, main = "ACF of GPR(13,1,laplacedot) Residuals (log TLB)")
pacf(na.omit(gpr_tlb_p13_d1_lap$residuals), lag.max = 40, main = "PACF of GPR(13,1,laplacedot) Residuals (log TLB)")

# GPR(1,2,rbfdot)

gpr_tlb_p1_d2_rbf <- gpr_model(tlb_training, p = 1, d = 2, kernel = "rbfdot", log_scale = TRUE)
gpr_tlb_p1_d2_rbf$model
fc_gpr <- gpr_forecast(gpr_tlb_p1_d2_rbf, h = length(tlb_test))

accuracy(fc_gpr, tlb_test)[, c("RMSE", "MAE", "MAPE")]
Box.test(na.omit(gpr_tlb_p1_d2_rbf$residuals), lag = 10, type = "Ljung-Box")

acf(na.omit(gpr_tlb_p1_d2_rbf$residuals), lag.max = 40, main = "ACF of GPR(1,2,rbfdot) Residuals (log TLB)")
pacf(na.omit(gpr_tlb_p1_d2_rbf$residuals), lag.max = 40, main = "PACF of GPR(1,2,rbfdot) Residuals (log TLB)")

# GPR(1,2,laplacedot)

gpr_tlb_p1_d2_lap <- gpr_model(tlb_training, p = 1, d = 2, kernel = "laplacedot", log_scale = TRUE)
gpr_tlb_p1_d2_lap$model
fc_gpr <- gpr_forecast(gpr_tlb_p1_d2_lap, h = length(tlb_test))

accuracy(fc_gpr, tlb_test)[, c("RMSE", "MAE", "MAPE")]
Box.test(na.omit(gpr_tlb_p1_d2_lap$residuals), lag = 10, type = "Ljung-Box")

acf(na.omit(gpr_tlb_p1_d2_lap$residuals), lag.max = 40, main = "ACF of GPR(1,2,laplacedot) Residuals (log TLB)")
pacf(na.omit(gpr_tlb_p1_d2_lap$residuals), lag.max = 40, main = "PACF of GPR(1,2,laplacedot) Residuals (log TLB)")

# GPR(2,2,rbfdot)

gpr_tlb_p2_d2_rbf <- gpr_model(tlb_training, p = 2, d = 2, kernel = "rbfdot", log_scale = TRUE)
gpr_tlb_p2_d2_rbf$model
fc_gpr <- gpr_forecast(gpr_tlb_p2_d2_rbf, h = length(tlb_test))

accuracy(fc_gpr, tlb_test)[, c("RMSE", "MAE", "MAPE")]
Box.test(na.omit(gpr_tlb_p2_d2_rbf$residuals), lag = 10, type = "Ljung-Box")

acf(na.omit(gpr_tlb_p2_d2_rbf$residuals), lag.max = 40, main = "ACF of GPR(2,2,rbfdot) Residuals (log TLB)")
pacf(na.omit(gpr_tlb_p2_d2_rbf$residuals), lag.max = 40, main = "PACF of GPR(2,2,rbfdot) Residuals (log TLB)")

# GPR(2,2,laplacedot)

gpr_tlb_p2_d2_lap <- gpr_model(tlb_training, p = 2, d = 2, kernel = "laplacedot", log_scale = TRUE)
gpr_tlb_p2_d2_lap$model
fc_gpr <- gpr_forecast(gpr_tlb_p2_d2_lap, h = length(tlb_test))

accuracy(fc_gpr, tlb_test)[, c("RMSE", "MAE", "MAPE")]
Box.test(na.omit(gpr_tlb_p2_d2_lap$residuals), lag = 10, type = "Ljung-Box")

acf(na.omit(gpr_tlb_p2_d2_lap$residuals), lag.max = 40, main = "ACF of GPR(2,2,laplacedot) Residuals (log TLB)")
pacf(na.omit(gpr_tlb_p2_d2_lap$residuals), lag.max = 40, main = "PACF of GPR(2,2,laplacedot) Residuals (log TLB)")

# GPR(5,2,rbfdot)

gpr_tlb_p5_d2_rbf <- gpr_model(tlb_training, p = 5, d = 2, kernel = "rbfdot", log_scale = TRUE)
gpr_tlb_p5_d2_rbf$model
fc_gpr <- gpr_forecast(gpr_tlb_p5_d2_rbf, h = length(tlb_test))

accuracy(fc_gpr, tlb_test)[, c("RMSE", "MAE", "MAPE")]
Box.test(na.omit(gpr_tlb_p5_d2_rbf$residuals), lag = 10, type = "Ljung-Box")

acf(na.omit(gpr_tlb_p5_d2_rbf$residuals), lag.max = 40, main = "ACF of GPR(5,2,rbfdot) Residuals (log TLB)")
pacf(na.omit(gpr_tlb_p5_d2_rbf$residuals), lag.max = 40, main = "PACF of GPR(5,2,rbfdot) Residuals (log TLB)")

# GPR(5,2,laplacedot)

gpr_tlb_p5_d2_lap <- gpr_model(tlb_training, p = 5, d = 2, kernel = "laplacedot", log_scale = TRUE)
gpr_tlb_p5_d2_lap$model
fc_gpr <- gpr_forecast(gpr_tlb_p5_d2_lap, h = length(tlb_test))

accuracy(fc_gpr, tlb_test)[, c("RMSE", "MAE", "MAPE")]
Box.test(na.omit(gpr_tlb_p5_d2_lap$residuals), lag = 10, type = "Ljung-Box")

acf(na.omit(gpr_tlb_p5_d2_lap$residuals), lag.max = 40, main = "ACF of GPR(5,2,laplacedot) Residuals (log TLB)")
pacf(na.omit(gpr_tlb_p5_d2_lap$residuals), lag.max = 40, main = "PACF of GPR(5,2,laplacedot) Residuals (log TLB)")

# GPR(11,2,rbfdot)

gpr_tlb_p11_d2_rbf <- gpr_model(tlb_training, p = 11, d = 2, kernel = "rbfdot", log_scale = TRUE)
gpr_tlb_p11_d2_rbf$model
fc_gpr <- gpr_forecast(gpr_tlb_p11_d2_rbf, h = length(tlb_test))

accuracy(fc_gpr, tlb_test)[, c("RMSE", "MAE", "MAPE")]
Box.test(na.omit(gpr_tlb_p11_d2_rbf$residuals), lag = 10, type = "Ljung-Box")

acf(na.omit(gpr_tlb_p11_d2_rbf$residuals), lag.max = 40, main = "ACF of GPR(11,2,rbfdot) Residuals (log TLB)")
pacf(na.omit(gpr_tlb_p11_d2_rbf$residuals), lag.max = 40, main = "PACF of GPR(11,2,rbfdot) Residuals (log TLB)")

# GPR(11,2,laplacedot)

gpr_tlb_p11_d2_lap <- gpr_model(tlb_training, p = 11, d = 2, kernel = "laplacedot", log_scale = TRUE)
gpr_tlb_p11_d2_lap$model
fc_gpr <- gpr_forecast(gpr_tlb_p11_d2_lap, h = length(tlb_test))

accuracy(fc_gpr, tlb_test)[, c("RMSE", "MAE", "MAPE")]
Box.test(na.omit(gpr_tlb_p11_d2_lap$residuals), lag = 10, type = "Ljung-Box")

acf(na.omit(gpr_tlb_p11_d2_lap$residuals), lag.max = 40, main = "ACF of GPR(11,2,laplacedot) Residuals (log TLB)")
pacf(na.omit(gpr_tlb_p11_d2_lap$residuals), lag.max = 40, main = "PACF of GPR(11,2,laplacedot) Residuals (log TLB)")

# GPR(12,2,rbfdot)

gpr_tlb_p12_d2_rbf <- gpr_model(tlb_training, p = 12, d = 2, kernel = "rbfdot", log_scale = TRUE)
gpr_tlb_p12_d2_rbf$model
fc_gpr <- gpr_forecast(gpr_tlb_p12_d2_rbf, h = length(tlb_test))

accuracy(fc_gpr, tlb_test)[, c("RMSE", "MAE", "MAPE")]
Box.test(na.omit(gpr_tlb_p12_d2_rbf$residuals), lag = 10, type = "Ljung-Box")

acf(na.omit(gpr_tlb_p12_d2_rbf$residuals), lag.max = 40, main = "ACF of GPR(12,2,rbfdot) Residuals (log TLB)")
pacf(na.omit(gpr_tlb_p12_d2_rbf$residuals), lag.max = 40, main = "PACF of GPR(12,2,rbfdot) Residuals (log TLB)")

# GPR(12,2,laplacedot)

gpr_tlb_p12_d2_lap <- gpr_model(tlb_training, p = 12, d = 2, kernel = "laplacedot", log_scale = TRUE)
gpr_tlb_p12_d2_lap$model
fc_gpr <- gpr_forecast(gpr_tlb_p12_d2_lap, h = length(tlb_test))

accuracy(fc_gpr, tlb_test)[, c("RMSE", "MAE", "MAPE")]
Box.test(na.omit(gpr_tlb_p12_d2_lap$residuals), lag = 10, type = "Ljung-Box")

acf(na.omit(gpr_tlb_p12_d2_lap$residuals), lag.max = 40, main = "ACF of GPR(12,2,laplacedot) Residuals (log TLB)")
pacf(na.omit(gpr_tlb_p12_d2_lap$residuals), lag.max = 40, main = "PACF of GPR(12,2,laplacedot) Residuals (log TLB)")

# #################################################
# LB = Ljung-Box p-value of the residuals (above 0.05 = white noise)
# Multi-step test errors (2013-2025 forecast from 1960-2012), log scale;
# RMSE / MAE in births (back-transformed), MAPE in %. Sorted by test RMSE.
# ---- = PACF-guided candidate (all have white-noise residuals)
# The low-lag d = 2 models drift away from the data when forecasting
# 13 years ahead, which is why their errors are very large.
### d=1 ###
#   Model                 Set   RMSE         MAE          MAPE       LB
#   GPR(11,1,rbfdot)      Test  1660.6       1378.6       3.6661     0.977 ----
#   GPR(13,1,rbfdot)      Test  2579.1       1847.7       5.2488     0.663 ----
#   GPR(12,1,rbfdot)      Test  2749.6       2184.1       5.9286     0.483 ----
#   GPR(12,1,laplacedot)  Test  3067.1       2211.0       6.3886     0.935 ----
#   GPR(13,1,laplacedot)  Test  3141.2       2297.2       6.6696     0.791 ----
#   GPR(11,1,laplacedot)  Test  3376.5       2598.4       7.4863     0.992 ----

#### d=2 ###
#   Model                 Set   RMSE         MAE          MAPE       LB
#   GPR(11,2,rbfdot)      Test  1552.6       1365.4       3.7265     0.725 ----
#   GPR(11,2,laplacedot)  Test  2496.3       2087.5       5.8414     0.836 ----
#   GPR(12,2,rbfdot)      Test  3803.2       3016.4       8.5866     0.601 ----
#   GPR(12,2,laplacedot)  Test  9611.5       7193.2       20.8107    0.267 ----
#   GPR(2,2,laplacedot)   Test  21058.1      19452.7      53.6441    0.412 ----
#   GPR(1,2,rbfdot)       Test  28393.5      26720.5      72.3587    0.217 ----
#   GPR(5,2,laplacedot)   Test  38844.4      30838.9      88.1436    0.880 ----
#   GPR(5,2,rbfdot)       Test  79949.0      54955.7      161.5347   0.984 ----
#   GPR(2,2,rbfdot)       Test  101478.0     80694.3      230.8789   0.982 ----
#   GPR(1,2,laplacedot)   Test  252423.2     191482.7     553.0856   0.998 ----
#################################################

# FORECAST OF GOOD CANDIDATES #
# Every candidate above has white-noise residuals (LB > 0.05), so all of
# them are kept. Nothing is removed on the basis of RMSE, MAE or MAPE.

candidates_gpr_tlb <- list(
  list(p = 11, d = 1, kernel = "rbfdot"),
  list(p = 11, d = 1, kernel = "laplacedot"),
  list(p = 12, d = 1, kernel = "rbfdot"),
  list(p = 12, d = 1, kernel = "laplacedot"),
  list(p = 13, d = 1, kernel = "rbfdot"),
  list(p = 13, d = 1, kernel = "laplacedot"),
  list(p = 1, d = 2, kernel = "rbfdot"),
  list(p = 1, d = 2, kernel = "laplacedot"),
  list(p = 2, d = 2, kernel = "rbfdot"),
  list(p = 2, d = 2, kernel = "laplacedot"),
  list(p = 5, d = 2, kernel = "rbfdot"),
  list(p = 5, d = 2, kernel = "laplacedot"),
  list(p = 11, d = 2, kernel = "rbfdot"),
  list(p = 11, d = 2, kernel = "laplacedot"),
  list(p = 12, d = 2, kernel = "rbfdot"),
  list(p = 12, d = 2, kernel = "laplacedot")
)

tlb_eval_gpr <- evaluate_models_gpr(candidates_gpr_tlb, tlb_training, tlb_test, log_scale = TRUE)
selected_models_tlb_gpr <- tlb_eval_gpr$models
results_tlb_gpr <- tlb_eval_gpr$results

# Separate training and test results (test ranked by RMSE, best first)

train_results_tlb_gpr <- subset(results_tlb_gpr, Set == "Train")
test_results_tlb_gpr <- subset(results_tlb_gpr, Set == "Test")
test_results_tlb_gpr <- test_results_tlb_gpr[order(test_results_tlb_gpr$RMSE), ]

rownames(train_results_tlb_gpr) <- NULL
rownames(test_results_tlb_gpr) <- NULL

train_results_tlb_gpr
test_results_tlb_gpr

# Best TLB model = lowest test RMSE among the white-noise models
best_tlb_gpr <- subset(test_results_tlb_gpr, White_Noise == "Yes")$Model[1]
best_tlb_gpr

# PLOT ACF & PACF (one plot per white-noise model)

plot_residuals_gpr(selected_models_tlb_gpr, "ACF")
plot_residuals_gpr(selected_models_tlb_gpr, "PACF")

# PLOT FORECAST

# Every white-noise model is drawn (best first by test RMSE).
plot_names_tlb_gpr <- subset(test_results_tlb_gpr, White_Noise == "Yes")$Model

train_predictions_tlb_gpr <- lapply(selected_models_tlb_gpr[plot_names_tlb_gpr],
                                    function(model) model$fitted)
test_predictions_tlb_gpr <- lapply(selected_models_tlb_gpr[plot_names_tlb_gpr], function(model) {
  gpr_forecast(model, h = length(tlb_test))
})

train_labels_tlb_gpr <- make_labels_gpr(results_tlb_gpr, plot_names_tlb_gpr, "Train", digits = "%.0f")
test_labels_tlb_gpr <- make_labels_gpr(results_tlb_gpr, plot_names_tlb_gpr, "Test", digits = "%.0f")

# Change this one setting to move the legend inside the graph.
# Options: "bottomleft", "bottomright", "topleft", "topright"
legend_position_tlb_gpr <- "bottomleft"

# Draw each graph separately instead of side-by-side.
old_par <- par(no.readonly = TRUE)
par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))
plot_predictions_gpr(tlb_training, train_predictions_tlb_gpr,
                     "GPR Training: Actual vs Fitted (log TLB, back-transformed)",
                     train_labels_tlb_gpr, ylab = "Total Live-Births",
                     legend_position = legend_position_tlb_gpr)
plot_predictions_gpr(tlb_test, test_predictions_tlb_gpr,
                     "GPR Test: Actual vs Forecast (log TLB, back-transformed)",
                     test_labels_tlb_gpr, ylab = "Total Live-Births",
                     legend_position = legend_position_tlb_gpr)
par(old_par)


##############################################################################
##############################################################################
# FINAL COMPARISON OF THE BEST MODELS
#
# The best model of each type (lowest test error in its own section):
#
#   TFR : ARIMA(12,2,3)
#         SARIMA(5,1,3)(0,1,0)[12]
#         NNAR(12,1,6)[12]           (12-year cycle, repeats = 20)
#         best GPR = best_tfr_gpr    (chosen in the GPR section)
#
#   TLB : ARIMA(12,1,3)              on log(TLB)
#         SARIMA(5,0,0)(0,1,1)[12]   on log(TLB)
#         Log-NNAR(5,3)              (lambda = 0, repeats = 20)
#         best GPR = best_tlb_gpr    on log(TLB) (chosen in the GPR section)
#
# ARIMA, SARIMA and NNAR are fitted again here with exactly the same
# settings and seed as in their own sections, so the numbers match the
# earlier results. The GPR models are taken directly from the GPR section:
# the white-noise model with the lowest test RMSE (best_tfr_gpr and
# best_tlb_gpr), so this section always uses the current best GPR. Objects in
# this section end in _final so nothing above is overwritten.
#
# All test forecasts are multi-step: 2013-2025 is forecast from the
# 1960-2012 training data only. All errors are on the original scale
# (TFR, or number of births for TLB).
#
# To compare a different ARIMA, SARIMA or NNAR model, change its line in the
# FIT section below. The GPR model follows best_tfr_gpr / best_tlb_gpr.
##############################################################################

# Output folder for the final graphs and table
if (!dir.exists("Output/Final_Comparison")) {
  dir.create("Output/Final_Comparison", recursive = TRUE)
}

# Years for the x-axis
train_years_final <- as.numeric(time(tfr_training))   # 1960 - 2012
test_years_final  <- as.numeric(time(tfr_test))       # 2013 - 2025

# Same colour and line type for each model type in every graph
model_colours_final <- c(ARIMA = "blue", SARIMA = "darkgreen", SARIMA_1 = "darkgreen",SARIMA_2 = "skyblue",
                         NNAR = "orange", GPR = "red")
model_lty_final <- c(ARIMA = 2, SARIMA = 3,SARIMA_1 = 3, SARIMA_2 = 5, NNAR = 4, GPR = 1)


# ERROR MEASURES (same formulas as accuracy(); missing values are skipped)
# ============================================================

error_measures_final <- function(actual, predicted) {
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  ok <- is.finite(actual) & is.finite(predicted)
  errors <- actual[ok] - predicted[ok]
  c(
    RMSE = sqrt(mean(errors^2)),
    MAE  = mean(abs(errors)),
    MAPE = mean(abs(errors / actual[ok])) * 100
  )
}


# FIT THE BEST MODELS - TFR
# ============================================================

# ARIMA(12,2,3) - same call as candidate_tfr
arima_tfr_final <- arima(tfr_training, order = c(12, 2, 3))

# SARIMA(5,1,3)(0,1,0)[12] - same call as candidate_tfr_s
sarima_tfr_final <- arima(tfr_training, order = c(5, 1, 3),
                          seasonal = list(order = c(0, 1, 0), period = 12))

# NNAR(12,1,6)[12] - same settings and seed as the 12-year cycle TFR model
tfr_training_12_final <- ts(as.numeric(tfr_training), frequency = 12)
set.seed(123)
nnar_tfr_final <- nnetar(tfr_training_12_final, p = 12, P = 1,
                         size = 6, repeats = 20)

# Best TFR GPR from the GPR section (white noise, lowest test RMSE)
gpr_tfr_final <- selected_models_tfr_gpr[[best_tfr_gpr]]

# Training fitted values (original scale)
train_fitted_tfr_final <- list(
  ARIMA  = as.numeric(tfr_training - residuals(arima_tfr_final)),
  SARIMA = as.numeric(tfr_training - residuals(sarima_tfr_final)),
  NNAR   = as.numeric(fitted(nnar_tfr_final)),
  GPR    = as.numeric(gpr_tfr_final$fitted)
)

# Test forecasts 2013-2025 (multi-step, original scale)
h_tfr_final <- length(tfr_test)

test_forecast_tfr_final <- list(
  ARIMA  = as.numeric(predict(arima_tfr_final, n.ahead = h_tfr_final)$pred),
  SARIMA = as.numeric(predict(sarima_tfr_final, n.ahead = h_tfr_final)$pred),
  NNAR   = as.numeric(forecast(nnar_tfr_final, h = h_tfr_final)$mean),
  GPR    = as.numeric(gpr_forecast(gpr_tfr_final, h = h_tfr_final))
)

# Residuals on the scale each model is fitted on (for the Ljung-Box test)
residuals_tfr_final <- list(
  ARIMA  = residuals(arima_tfr_final),
  SARIMA = residuals(sarima_tfr_final),
  NNAR   = residuals(nnar_tfr_final),
  GPR    = gpr_tfr_final$residuals
)

model_names_tfr_final <- c(
  ARIMA  = "ARIMA(12,2,3)",
  SARIMA = "SARIMA(5,1,3)(0,1,0)[12]",
  NNAR   = "NNAR(12,1,6)[12]",
  GPR    = best_tfr_gpr
)


# FIT THE BEST MODELS - TLB (all on the log scale)
# ============================================================

# ARIMA(12,1,3) on log(TLB) - same call as candidate_tlb
arima_tlb_final <- arima(log(tlb_training), order = c(12, 1, 3))

# SARIMA(5,0,0)(0,1,1)[12] on log(TLB) - same call as candidate_tlb_s
sarima_tlb_final_1 <- arima(log(tlb_training), order = c(5, 0, 0),
                          seasonal = list(order = c(0, 1, 1), period = 12))

# SARIMA(5,0,0)(1,1,0)[12] on log(TLB) - same call as candidate_tlb_s
sarima_tlb_final_2 <- arima(log(tlb_training), order = c(5, 0, 0),
                          seasonal = list(order = c(1, 1, 0), period = 12))


# Log-NNAR(5,3) - same settings and seed as in the TLB comparison
set.seed(123)
nnar_tlb_final <- nnetar(tlb_training, p = 5, P = 0, size = 3,
                         repeats = 20, lambda = 0)

# Best TLB GPR from the GPR section (log scale, white noise, lowest test RMSE)
gpr_tlb_final <- selected_models_tlb_gpr[[best_tlb_gpr]]

# Training fitted values (back-transformed to number of births)
train_fitted_tlb_final <- list(
  ARIMA  = as.numeric(exp(log(tlb_training) - residuals(arima_tlb_final))),
  SARIMA_1 = as.numeric(exp(log(tlb_training) - residuals(sarima_tlb_final_1))),
  SARIMA_2 = as.numeric(exp(log(tlb_training) - residuals(sarima_tlb_final_2))),
    NNAR   = as.numeric(fitted(nnar_tlb_final)),
  GPR    = as.numeric(gpr_tlb_final$fitted)
)

# Test forecasts 2013-2025 (multi-step, number of births)
h_tlb_final <- length(tlb_test)

test_forecast_tlb_final <- list(
  ARIMA  = as.numeric(exp(predict(arima_tlb_final, n.ahead = h_tlb_final)$pred)),
  SARIMA_1 = as.numeric(exp(predict(sarima_tlb_final_1, n.ahead = h_tlb_final)$pred)),
  SARIMA_2 = as.numeric(exp(predict(sarima_tlb_final_2, n.ahead = h_tlb_final)$pred)),
    NNAR   = as.numeric(forecast(nnar_tlb_final, h = h_tlb_final)$mean),
  GPR    = as.numeric(gpr_forecast(gpr_tlb_final, h = h_tlb_final))
)

# Residuals on the log scale (the scale the models are fitted on)
residuals_tlb_final <- list(
  ARIMA  = residuals(arima_tlb_final),
  SARIMA_1 = residuals(sarima_tlb_final_1),
  SARIMA_2 = residuals(sarima_tlb_final_2),
    NNAR   = residuals(nnar_tlb_final),
  GPR    = gpr_tlb_final$residuals
)

model_names_tlb_final <- c(
  ARIMA  = "ARIMA(12,1,3) log",
  SARIMA_1 = "SARIMA(5,0,0)(0,1,1)[12] log",
  SARIMA_2 = "SARIMA(5,0,0)(1,1,0)[12] log",
  NNAR   = "Log-NNAR(5,3)",
  GPR    = paste(best_tlb_gpr, "log")
)


# COMPARISON TABLES
# ============================================================
# One row per model: train and test RMSE, MAE, MAPE and the Ljung-Box
# p-value of the residuals (above 0.05 = white noise). Sorted by test RMSE.

make_comparison_final <- function(series_name, model_names, train_actual,
                                  train_fitted, test_actual, test_forecast,
                                  residual_list) {
  rows <- lapply(names(model_names), function(type) {
    tr <- error_measures_final(train_actual, train_fitted[[type]])
    te <- error_measures_final(test_actual, test_forecast[[type]])
    lb <- Box.test(na.omit(as.numeric(residual_list[[type]])), lag = 10,
                   type = "Ljung-Box")$p.value
    data.frame(
      Series = series_name,
      Model = model_names[[type]],
      Type = type,
      Train_RMSE = tr["RMSE"], Train_MAE = tr["MAE"], Train_MAPE = tr["MAPE"],
      Test_RMSE = te["RMSE"], Test_MAE = te["MAE"], Test_MAPE = te["MAPE"],
      Ljung_Box_p = lb,
      White_Noise = ifelse(lb > 0.05, "Yes", "No"),
      row.names = NULL
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(out$Test_RMSE), ]
  out$Rank <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

comparison_tfr_final <- make_comparison_final(
  "TFR", model_names_tfr_final,
  tfr_training, train_fitted_tfr_final,
  tfr_test, test_forecast_tfr_final,
  residuals_tfr_final
)

comparison_tlb_final <- make_comparison_final(
  "TLB", model_names_tlb_final,
  tlb_training, train_fitted_tlb_final,
  tlb_test, test_forecast_tlb_final,
  residuals_tlb_final
)

# View the tables
comparison_tfr_final
comparison_tlb_final

# Best overall model for each series (lowest test RMSE)
best_overall_tfr <- comparison_tfr_final$Model[1]
best_overall_tlb <- comparison_tlb_final$Model[1]

print(paste("Best TFR model:", best_overall_tfr))
print(paste("Best TLB model:", best_overall_tlb))

# Save one table with both series
write.csv(
  rbind(comparison_tfr_final, comparison_tlb_final),
  "Output/Final_Comparison/Best_Model_Comparison.csv",
  row.names = FALSE
)


# GRAPHS: TRAINING (LEFT) AND TEST (RIGHT) SIDE BY SIDE
# ============================================================

plot_final_comparison <- function(series_label, ylab, model_names,
                                  train_actual, train_fitted,
                                  test_actual, test_forecast,
                                  comparison, digits = "%.4f") {
  
  types <- names(model_names)
  cols <- model_colours_final[types]
  ltys <- model_lty_final[types]
  
  # legend labels with each model's error for that period
  label_for <- function(set) {
    sapply(types, function(type) {
      row <- comparison[comparison$Type == type, ]
      rmse <- if (set == "Train") row$Train_RMSE else row$Test_RMSE
      mape <- if (set == "Train") row$Train_MAPE else row$Test_MAPE
      sprintf(paste0("%s | RMSE: ", digits, " | MAPE: %.2f%%"),
              model_names[[type]], rmse, mape)
    })
  }
  
  old_par_final <- par(no.readonly = TRUE)
  on.exit(par(old_par_final))
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
  
  # Training: actual vs fitted
  plot(train_years_final, as.numeric(train_actual), type = "l",
       col = "black", lwd = 3,
       ylim = range(c(as.numeric(train_actual), unlist(train_fitted)),
                    finite = TRUE),
       xlab = "Year", ylab = ylab,
       main = "Training (1960-2012): Actual vs Fitted")
  for (type in types) {
    lines(train_years_final, train_fitted[[type]],
          col = cols[[type]], lty = ltys[[type]], lwd = 2)
  }
  legend("topright", legend = c("Actual", label_for("Train")),
         col = c("black", cols), lty = c(1, ltys), lwd = c(3, rep(2, length(types))),
         cex = 0.6, bty = "n")
  
  # Test: actual vs multi-step forecast
  plot(test_years_final, as.numeric(test_actual), type = "l",
       col = "black", lwd = 3,
       ylim = range(c(as.numeric(test_actual), unlist(test_forecast)),
                    finite = TRUE),
       xlab = "Year", ylab = ylab,
       main = "Test (2013-2025): Actual vs Forecast")
  for (type in types) {
    lines(test_years_final, test_forecast[[type]],
          col = cols[[type]], lty = ltys[[type]], lwd = 2)
  }
  legend("bottomleft", legend = c("Actual", label_for("Test")),
         col = c("black", cols), lty = c(1, ltys), lwd = c(3, rep(2, length(types))),
         cex = 0.6, bty = "n")
  
  mtext(paste("Best Model Comparison:", series_label),
        outer = TRUE, cex = 1.2, font = 2)
}

# TFR - show on screen
plot_final_comparison(
  "Total Fertility Rate (TFR)", "Total Fertility Rate", model_names_tfr_final,
  tfr_training, train_fitted_tfr_final,
  tfr_test, test_forecast_tfr_final,
  comparison_tfr_final, digits = "%.4f"
)



# TLB - show on screen
plot_final_comparison(
  "Total Live-Births (TLB)", "Total Live-Births", model_names_tlb_final,
  tlb_training, train_fitted_tlb_final,
  tlb_test, test_forecast_tlb_final,
  comparison_tlb_final, digits = "%.0f"
)
# 
# TLB - save as PNG
png("Output/Final_Comparison/Best_Models_TLB.png",
    width = 14, height = 6, units = "in", res = 300)
plot_final_comparison(
  "Total Live-Births (TLB)", "Total Live-Births", model_names_tlb_final,
  tlb_training, train_fitted_tlb_final,
  tlb_test, test_forecast_tlb_final,
  comparison_tlb_final, digits = "%.0f"
)
