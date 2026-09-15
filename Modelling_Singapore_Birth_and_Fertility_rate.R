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

ggplot(
  tfr_start_end,
  aes(
    x = factor(year),
    y = value
  )
) +
  geom_col() +
  labs(
    title = "Total Fertility Rate: 1960 vs 2025",
    x = "Year",
    y = "Total Fertility Rate"
  ) +
  theme_minimal()


ggsave(
  "Output/EDA/Graphs/10_TFR_1960_vs_2025.png",
  width = 10,
  height = 6,
  dpi = 300
) 

# TOTAL LIVE-BIRTHS 1960 AND 2025
# ============================================================

birth_start_end <- birth_data %>%
  filter(
    year %in% c(1960, 2025)
  )


print(birth_start_end)

ggplot(
  birth_start_end,
  aes(
    x = factor(year),
    y = value
  )
) +
  geom_col() +
  labs(
    title = "Total Live-Births: 1960 vs 2025",
    x = "Year",
    y = "Total Live-Births"
  ) +
  theme_minimal()

ggsave(
  "Output/EDA/Graphs/11_Total_Live_Births_1960_vs_2025.png",
  width = 10,
  height = 6,
  dpi = 300
) 

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



# TIME-SERIES DIAGNOSTICS
# ============================================================

# Create training data for Total Fertility Rate
tfr_train <- train_data %>%
  filter(variable == "Total_Fertility_Rate_TFR") %>%
  arrange(year)

# Create training data for Total Live-Births
birth_train <- train_data %>%
  filter(variable == "Total_Live_Births") %>%
  arrange(year)



# Create time-series objects
# ============================================================

tfr_ts <- ts(
  tfr_train$value,
  start = 1960,
  frequency = 1
)

birth_ts <- ts(
  birth_train$value,
  start = 1960,
  frequency = 1
)


# Plot TFR time series
# ============================================================

plot(
  tfr_ts,
  main = "Total Fertility Rate Time Series",
  xlab = "Year",
  ylab = "Total Fertility Rate",
  type = "o"
)


# Plot Total Live-Births time series
# ============================================================

plot(
  birth_ts,
  main = "Total Live-Births Time Series",
  xlab = "Year",
  ylab = "Total Live-Births",
  type = "o"
)


# Check stationarity using Augmented Dickey-Fuller test
# ============================================================

adf_tfr <- adf.test(tfr_ts)

adf_birth <- adf.test(birth_ts)

adf_tfr
adf_birth


# Check autocorrelation
# ============================================================

acf(
  tfr_ts,
  main = "ACF of Total Fertility Rate"
)

acf(
  birth_ts,
  main = "ACF of Total Live-Births"
)


# Check partial autocorrelation
# ============================================================

pacf(
  tfr_ts,
  main = "PACF of Total Fertility Rate"
)

pacf(
  birth_ts,
  main = "PACF of Total Live-Births"
)


# Check whether differencing is needed
# ============================================================

tfr_diff <- diff(tfr_ts)

birth_diff <- diff(birth_ts)


# Plot differenced TFR
plot(
  tfr_diff,
  main = "Differenced Total Fertility Rate",
  xlab = "Year",
  ylab = "Differenced TFR",
  type = "o"
)


# Plot differenced Total Live-Births
plot(
  birth_diff,
  main = "Differenced Total Live-Births",
  xlab = "Year",
  ylab = "Differenced Total Live-Births",
  type = "o"
)



# ACF and PACF after differencing
# ============================================================

acf(
  tfr_diff,
  main = "ACF of Differenced TFR"
)

pacf(
  tfr_diff,
  main = "PACF of Differenced TFR"
)

acf(
  birth_diff,
  main = "ACF of Differenced Total Live-Births"
)

pacf(
  birth_diff,
  main = "PACF of Differenced Total Live-Births"
)



# Ljung-Box test for autocorrelation
# ============================================================

Box.test(
  tfr_ts,
  lag = 10,
  type = "Ljung-Box"
)

Box.test(
  birth_ts,
  lag = 10,
  type = "Ljung-Box"
)

