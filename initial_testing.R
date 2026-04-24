# install.packages("tidycensus")

# Load libraries for getting data
library(tidycensus)
library(tidyverse)

# Use the API key given to me from The United States Census Bureau 
# "https://www.census.gov/data/developers.html" to recieve census data capabilities
# If this is the first time running, keep "install" parameter, otherwise use "overwrite" parameter
census_api_key("6377fd44a462d589653004510dcef417f2f8aabb", overwrite = TRUE, install = TRUE)

# Load all variables that can be used to make list of possible data
v2010 <- load_variables(2010, "acs1")
View(v2010)

### List of important variables for data set:
#'
#' Total population: B01003_001
#' Median age: B01002_001
#' Median household income: B19013_001
#' Per capita income: B19301_001
#' Population below poverty: B17001_002
#' Labor force: B23025_003
#' Unemployed: B23025_005
#' Median home value: B25077_001
#' Median gross rent: B25064_001
#' Total housing units: B25002_001
#' Vacant housing units: B25002_003
#' White population: B02001_002
#' Black population: B02001_003
#' Asian population: B02001_005
#' Hispanic/Latino population: B03003_003
#' Total (health insurance): B27001_001
#'

# Vector of all tidycensus variables
census_vars <- c(
  total_population = "B01003_001",
  median_age = "B01002_001",
  median_household_income = "B19013_001",
  per_capita_income = "B19301_001",
  poverty_population = "B17001_002",
  labor_force = "B23025_003",
  unemployed = "B23025_005",
  median_home_value = "B25077_001",
  median_gross_rent = "B25064_001",
  total_housing_units = "B25002_001",
  vacant_housing_units = "B25002_003",
  white_population = "B02001_002",
  black_population = "B02001_003",
  asian_population = "B02001_005",
  hispanic_latino_population = "B03003_003",
  total_health_insurance_population = "B27001_001"
)

### Examples of getting raw data with specific variables from US Census Bureau
# One variable data set
raw_1_var <- get_acs(
  geography = "county", 
  variables = c(population = "B01003_001"),
  state = "PA",
  year = 2022
)

# View raw data
View(raw_1_var)

# More than one variable data set
raw_more_var <- get_acs(
  geography = "county",
  variables = c(
    # set the name of the variable in the vector to be what the code represents
    # each code is obtained from the load_variables() function call and searching through the data
    population = "B01003_001",
    income = "B19013_001"
  ),
  state = "PA",
  year = 2022
)

# Data set containing all variables
US_Census_Raw <- get_acs(
  geography = "county",
  variables = census_vars,
  state = "PA",
  year = 2022
)

### When making the raw data there will be 5 columns:
#' GEOID: A unique geographic identifier assigned by the Census Bureau (ex. 42 is Pennsylvania, and 42027 is Centre County, PA)
#' NAME: A human-readable label for the geography (ex. county, state)
#' variable: The Census variable code we requested and (will show up with names if you used a vector)
#' estimate: The estimate value of the variable (ex. estimate population value)
#' moe: This is the margin of error in for ACS, based on a 90% confidence interval (thus true value = estimate +- moe)
#' 
#' This is in long format and can thus be wrangled for better statistical use.

# Wrangle the data a bit:
US_Census_Tidy <- US_Census_Raw |>
  pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = variable,
    values_from = c(estimate, moe)
  )
