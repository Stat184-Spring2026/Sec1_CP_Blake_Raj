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
#' Use the following to load a table of all variables and their codes:
#' v2010 <- load_variables(2010, "acs5") # the year does not matter
#' View(v2010)

## Set variables
vars <- c(
  # Population
  total_population = "B01003_001",
  # Households
  total_households = "B11001_001",
  # Renter-occupied households with income
  renter_occupied_households = "B25070_001",
  # Income
  median_income = "B19013_001",
  # Employment
  labor_force = "B23025_003",
  total_workers = "B08301_001",
  unemployed = "B23025_005",
  # Remote work (work from home)
  work_from_home = "B08301_021",
  # Internet access (household has any internet subscription)
  internet_access = "B28002_002",
  # Rent burden (30%+ of income, sum these)
  rent_30_34 = "B25070_007",
  rent_35_39 = "B25070_008",
  rent_40_49 = "B25070_009",
  rent_50_plus = "B25070_010",
  # Age 65+ (we will sum these)
  male_65_66 = "B01001_020",
  male_67_69 = "B01001_021",
  male_70_74 = "B01001_022",
  male_75_79 = "B01001_023",
  male_80_84 = "B01001_024",
  male_85_plus = "B01001_025",
  female_65_66 = "B01001_044",
  female_67_69 = "B01001_045",
  female_70_74 = "B01001_046",
  female_75_79 = "B01001_047",
  female_80_84 = "B01001_048",
  female_85_plus = "B01001_049"
)

## Get data for each year
md_2019 <- get_acs(
  geography = "county",
  state = "MD",
  variables = vars,
  year = 2019,
  output = "wide"
) |>
  mutate(year = 2019)

md_2022 <- get_acs(
  geography = "county",
  state = "MD",
  variables = vars,
  year = 2022,
  output = "wide"
) |>
  mutate(year = 2022)

## Bind DF's
md_all <- bind_rows(md_2019, md_2022)

## Tidy DF
md_clean <- md_all |>
  mutate(
    age_65_plus = male_65_66E + male_67_69E + male_70_74E + male_75_79E + 
      male_80_84E + male_85_plusE + female_65_66E + female_67_69E + female_70_74E + 
      female_75_79E + female_80_84E + female_85_plusE,
    rent_30_plus = rent_30_34E + rent_35_39E + rent_40_49E + rent_50_plusE
  ) |>
  select(
    County = NAME,
    year,
    total_population = total_populationE,
    total_households = total_householdsE,
    total_renter_occ_households = renter_occupied_householdsE,
    median_income = median_incomeE,
    labor_force = labor_forceE,
    total_workers_commute = total_workersE,
    unemployed = unemployedE,
    work_from_home = work_from_homeE,
    household_internet_access = internet_accessE,
    rent_30_plus,
    age_65_plus
  ) |>
  mutate(
    County = str_remove(County, ",\\s*.*$"),
    # Unemployment rate
    unemployment_rate = unemployed / labor_force,
    # % Working from home (proxy)
    pct_wfh = work_from_home / total_workers_commute,
    # Internet access rate
    pct_internet = household_internet_access / total_households,
    # Rent burden (30%+)
    rent_burden = rent_30_plus / total_renter_occ_households,
    # % Age 65+
    pct_65_plus = age_65_plus / total_population
  ) |>
  arrange(County, year)

## Create Data Table


## Create Data Visualization


## (Optional) Data Analysis/Narrative Text


