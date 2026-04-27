library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)
# tigris is for retrieving county borders, SF is simple figures 

# This API key isn't sensitive.... I think. ^_^
census_api_key("6377fd44a462d589653004510dcef417f2f8aabb", overwrite = TRUE, install = TRUE)


state_abbr <- "MD"
state_name <- "Maryland"
years <- 2019:2022
filter_counties <- TRUE # Select to choose only counties around St. Mary's. FALSE to choose all

selected_counties <- c(
  "St. Mary's County, Maryland",
  "Calvert County, Maryland",
  "Charles County, Maryland",
  "Prince George's County, Maryland",
  "Anne Arundel County, Maryland"
)

# Create a DF of all the counties, for our selected years, using get_acs()
pop_all_years <- data.frame()
for (y in years) {
  one_year <- get_acs(
    geography = "county",
    variables = c(population = "B01003_001"),
    state = state_abbr,
    year = y,
    survey = "acs5"
  ) |>
    
    mutate(year = y)
  pop_all_years <- bind_rows(pop_all_years, one_year)
}

# Optionally filter counties
pop_clean <- pop_all_years |>
  filter(
    if (filter_counties) {
      NAME %in% selected_counties
    } else {
      TRUE
    }
    
  ) |>
  transmute(
    year,
    county = NAME,
    population = estimate
  ) |>
  
  arrange(county, year)

# Calculate percent changes, earliest and latest population values
pop_pct_change <- pop_clean |>
  group_by(county) |>
  arrange(year, .by_group = TRUE) |>
  summarise(
    earliest_year = first(year),
    latest_year = last(year),
    earliest_population = first(population),
    latest_population = last(population),
    population_change = latest_population - earliest_population,
    percent_change = ((latest_population - earliest_population) / earliest_population) * 100,
    
    .groups = "drop"
  ) |>
  
  arrange(desc(percent_change))

# Retrieve the county borders
md_counties <- counties(
  state = state_abbr,
  year = 2022,
  cb = TRUE,
  class = "sf"
)


# Adjust the county names. Using paste0 to avoid the case of Baltimore being dropped because it doesnt have county in its name 
pop_pct_change_map <- pop_pct_change |>
  mutate(
    county_name = county |>
      str_remove(paste0(" County, ", state_name)) |>
      str_remove(paste0(", ", state_name))
  )

# Keep only counties we want to visualize
map_data <- md_counties |>
  inner_join(
    pop_pct_change_map,
    by = c("NAME" = "county_name")
  )

# Plot choropleth using ggplot
ggplot(map_data) +
  geom_sf(aes(fill = percent_change), color = "white", linewidth = 0.4) +
  geom_sf_text(aes(label = NAME), size = 3) +
  
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "lightblue",
    midpoint = 0,
    name = "% Change"
  ) +
  
  labs(
    title = "Population Change by County in Maryland",
    subtitle = "ACS 5-year estimates, 2019 to 2022",
    caption = "Source: U.S. Census Bureau ACS 5-year estimates"
  ) +
  theme_void()
