library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)

df_wide <- read_excel("CoE_GDP_Predictions_2018_2027.xlsx")

# Load map and filter for CoE countries
coe_countries <- c(  
  "AL", "AM", "AT", "AZ", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
  "GE", "DE", "GR", "HU", "IS", "IE", "IT", "XK", "LV", "LI", "LT", "LU",
  "MT", "MD", "MC", "ME", "NL", "MK", "NO", "PL", "PT", "RO", "SM", "RS", "SK",
  "SI", "ES", "SE", "CH", "TR", "UA", "GB"
)

world <- ne_countries(scale = "medium", returnclass = "sf")
coe_map <- world %>% filter(iso_a2 %in% coe_countries)

# Merge GDP data
map_data <- coe_map %>%
  left_join(df_wide, by = c("iso_a2" = "iso2c")) %>%
  mutate(
    label = paste0(name, "<br>GDP 2027: $", format(round(gdp_2027, 0), big.mark = ",")),
    tier = factor(tier)
  )

# Base ggplot
p <- ggplot(map_data) +
  geom_sf(aes(fill = tier, text = label), color = "white") +
  geom_sf_text(aes(label = iso_a2), size = 3, color = "black") +
  scale_fill_viridis_d(name = "Tier", option = "C", na.value = "gray90") +
  labs(
    title = "Council of Europe: Predicted GDP per Capita in 2027",
    subtitle = "Click or hover for country details"
  ) +
  theme_minimal()

interactive_map <- ggplotly(p, tooltip = "text")
interactive_map
print(interactive_map)
