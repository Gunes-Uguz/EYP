library(WDI)
library(dplyr)
library(tidyr)
library(randomForest)
library(openxlsx)
library(readxl)

indicators <- c(
  gdp_per_capita_ppp = "NY.GDP.PCAP.PP.KD",
  gdp_growth = "NY.GDP.MKTP.KD.ZG",
  gdp_per_capita_growth = "NY.GDP.PCAP.KD.ZG"
)

years <- 2018:2023

coe_countries <- c(  
  "AL", "AM", "AT", "AZ", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
  "GE", "DE", "GR", "HU", "IS", "IE", "IT", "KZ", "XK", "LV", "LI", "LT", "LU",
  "MT", "MD", "MC", "ME", "NL", "MK", "NO", "PL", "PT", "RO", "SM", "RS", "SK",
  "SI", "ES", "SE", "CH", "TR", "UA", "GB"
)

# Download data from World Bank
df <- WDI(
  country = coe_countries,
  indicator = indicators,
  start = min(years),
  end = max(years),
  extra = FALSE,
  cache = NULL
)

df <- df %>%
  rename(
    gdp_pc_ppp = gdp_per_capita_ppp,
    gdp_pc_growth = gdp_per_capita_growth
  ) %>%
  select(iso2c, country, year, gdp_pc_ppp, gdp_growth, gdp_pc_growth)

# Lagged features for forecasting
df <- df %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    lag1_growth = lag(gdp_pc_growth, 1),
    lag2_growth = lag(gdp_pc_growth, 2)
  ) %>%
  ungroup()

# Training data 2021–2023 to predict 2027 later
train_data <- df %>%
  filter(year %in% 2021:2023, !is.na(gdp_pc_growth), !is.na(lag1_growth), !is.na(lag2_growth))

# Average country growth & GDP for clustering
df_summary <- df %>%
  group_by(country, iso2c) %>%
  summarise(
    avg_gdp = mean(gdp_pc_ppp, na.rm = TRUE),
    .groups = "drop"
  )

# Clustering into 7 tiers
set.seed(123)
clust_data <- df_summary %>% filter(!is.na(avg_gdp))
km <- kmeans(clust_data$avg_gdp, centers = 7)

df_summary <- clust_data %>%
  mutate(tier = as.numeric(factor(km$cluster, levels = order(tapply(avg_gdp, km$cluster, mean)))))

# Train a model for each tier
df_train <- train_data %>%
  left_join(df_summary %>% select(country, tier), by = "country")

models <- df_train %>%
  group_by(tier) %>%
  group_map(~ randomForest(gdp_pc_growth ~ lag1_growth + lag2_growth, data = .x), .keep = TRUE)

# Forecast 2027 GDP per capita using predicted growth
df_2023 <- df %>%
  filter(year == 2023) %>%
  select(iso2c, country, gdp_pc_ppp, lag1_growth = gdp_pc_growth)

df_2022 <- df %>%
  filter(year == 2022) %>%
  select(iso2c, lag2_growth = gdp_pc_growth)

df_pred_input <- df_2023 %>%
  left_join(df_2022, by = "iso2c") %>%
  left_join(df_summary %>% select(iso2c, tier), by = "iso2c") %>%
  filter(!is.na(lag1_growth), !is.na(lag2_growth), !is.na(gdp_pc_ppp), !is.na(tier))

df_pred_input <- df_pred_input %>%
  rowwise() %>%
  mutate(
    predicted_growth = predict(models[[tier]], newdata = cur_data()),
    gdp_2027 = gdp_pc_ppp * (1 + predicted_growth/100)^4
  ) %>%
  ungroup()

# Merge predicted GDP with historical data
df_wide <- df %>%
  filter(year %in% 2018:2023) %>%
  select(country, iso2c, year, gdp_pc_ppp) %>%
  pivot_wider(names_from = year, names_prefix = "gdp_per_capita_ppp_", values_from = gdp_pc_ppp) %>%
  left_join(df_pred_input %>% select(iso2c, gdp_2027, predicted_growth, tier), by = "iso2c")

# Export to Excel
write.xlsx(df_wide, "CoE_GDP_Predictions_2018_2027.xlsx", asTable = TRUE)