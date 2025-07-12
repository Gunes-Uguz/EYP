library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

# Reshape wide data to long format for plotting
df_long <- df_wide %>%
  pivot_longer(
    cols = starts_with("gdp_per_capita_ppp_"),
    names_to = "year",
    values_to = "gdp_pc_ppp"
  ) %>%
  mutate(
    year = as.numeric(gsub("gdp_per_capita_ppp_", "", year)),
    data_type = "Historical"
  )

# Add 2027 predictions to the long dataset
df_viz <- df_long %>%
  bind_rows(
    df_wide %>%
      select(country, iso2c, gdp_2027, tier) %>%
      rename(gdp_pc_ppp = gdp_2027) %>%
      mutate(year = 2027, data_type = "Predicted")
  )


# Create base ggplot
p <- ggplot(df_viz, aes(x = year, y = gdp_pc_ppp, group = country, 
                       text = paste("Country:", country, 
                                   "\nTier:", tier,
                                   "\nGDP per capita:", round(gdp_pc_ppp, 2),
                                   "\nType:", data_type))) +
  geom_line(aes(color = as.factor(tier))) +
  geom_point(aes(shape = data_type)) +
  scale_shape_manual(values = c("Historical" = 16, "Predicted" = 17)) +
  labs(
    title = "GDP per Capita Trends with 2027 Predictions",
    x = "Year",
    y = "GDP per Capita (PPP)",
    color = "Tier"
  ) +
  theme_minimal()

# Convert to interactive plot
interactive_plot <- ggplotly(p, tooltip = "text")
interactive_plot

print(interactive_plot)