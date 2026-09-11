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


