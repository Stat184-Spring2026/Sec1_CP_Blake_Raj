# Gini Index Choropleth Script by Raj
# Purpose: Use Maryland county ACS 5-year data to map where income inequality
# increased or decreased between 2019 and 2022.

## Load packages:
library(tidycensus)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(tigris)
library(sf)

options(tigris_use_cache = TRUE)

## Set the Census API key
census_api_key("6377fd44a462d589653004510dcef417f2f8aabb", overwrite = TRUE, install = TRUE)

## Set variables
gini_vars <- c(
  gini_index = "B19083_001"
)

## Get Gini Index data for 2019
md_gini_2019 <- get_acs(
  geography = "county",
  state = "MD",
  variables = gini_vars,
  year = 2019,
  survey = "acs5",
  output = "wide"
) |>
  mutate(year = 2019)

## Get Gini Index data for 2022
md_gini_2022 <- get_acs(
  geography = "county",
  state = "MD",
  variables = gini_vars,
  year = 2022,
  survey = "acs5",
  output = "wide"
) |>
  mutate(year = 2022)

## Bind and tidy Gini data
md_gini_clean <- bind_rows(md_gini_2019, md_gini_2022) |>
  transmute(
    GEOID,
    County = NAME,
    year,
    gini_index = gini_indexE
  )

## Calculate county-level Gini Index change
md_gini_change <- md_gini_clean |>
  pivot_wider(
    names_from = year,
    values_from = gini_index,
    names_prefix = "gini_"
  ) |>
  mutate(
    gini_change = gini_2022 - gini_2019
  )

## Get Maryland county map boundaries
md_counties <- counties(state = "MD", cb = TRUE, year = 2022) |>
  st_as_sf()

## Join Gini change data to county map boundaries
md_gini_change_map <- md_counties |>
  left_join(md_gini_change, by = "GEOID")

## Create Gini Index change choropleth map
gini_map <- ggplot(md_gini_change_map) +
  geom_sf(aes(fill = gini_change), color = "gray50", linewidth = 0.2) +
  scale_fill_gradient2(
    low = "#4575B4",
    mid = "#F7F7F7",
    high = "#D73027",
    midpoint = 0,
    labels = label_number(accuracy = 0.001)
  ) +
  labs(
    title = "Change in Income Inequality by Maryland County",
    subtitle = "Gini Index change, ACS 5-year estimates, 2019 to 2022",
    fill = "Gini Index\nchange",
    caption = "Source: U.S. Census Bureau ACS 5-year estimates via tidycensus"
  ) +
  
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "left",
    plot.caption = element_text(
      hjust = 0,
      size = 9,
      color = "gray40",
      margin = margin(t = 10))
  )

# Narrative:
# The Gini Index map shows that income inequality changed unevenly across Maryland
# counties from 2019 to 2022. Most counties show only very small changes, suggesting
# that income inequality was relatively stable overall. Taken together, the statewide
# pattern appears to show a slight net reduction in income inequality, but the change
# is not evenly distributed across counties.

gini_map