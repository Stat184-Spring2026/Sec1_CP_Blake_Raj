# Data Wrangling, Visualization, and Analysis Script by Blake

## Load packages:
library(tidycensus)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(scales)

options(tigris_use_cache = TRUE)

## Set the Census API key
census_api_key("6377fd44a462d589653004510dcef417f2f8aabb", overwrite = TRUE, install = TRUE)

## Set variables for census data filtering:
#' We want the following variables:
#' - B01003_001  # total_population
#' - B01002_001  # median_age
#' - B01001      # age_structure (65+)
#' - B27001_001  # insurance
#' - C18108      # disability
#' - B23025_003  # labor force
#' - B23025_005  # unemployed
#' - B19013_001  # income
#' - B25014      # crowding
#' - B25070      # rent burden
#' - B08301      # commute type
#' - B03003_003  # Hispanic
#' - B02001_003  # Black
#' - B28002      # internet
#' 
#' Use the following to load a table of all variables and their codes:
#' v2010 <- load_variables(2010, "acs5") # the year does not matter
#' View(v2010)

## Set variables
vars <- c(
  population = "B01003_001",
  median_income = "B19013_001",
  employed = "B23025_004"
)

## Get data for each year



## Bind DF's



## Tidy DF



## Create Data Table



## Create Data Visualization



## (Optional) Data Analysis/Narrative Text



