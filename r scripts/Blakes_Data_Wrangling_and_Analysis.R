# Data Wrangling, Visualization, and Analysis Script by Blake

## Load packages:
library(tidycensus)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(scales)
library(esquisse)

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
md_2019_table <- md_clean |>
  mutate(
    unemployment_rate = percent(unemployment_rate, accuracy = 0.01),
    pct_wfh = percent(pct_wfh, accuracy = 0.01),
    pct_internet = percent(pct_internet, accuracy = 0.01),
    rent_burden = percent(rent_burden, accuracy = 0.01),
    pct_65_plus = percent(pct_65_plus, accuracy = 0.01),
    median_income = dollar(median_income)
  ) |>
  filter(
    year == 2019
  ) |>
  select(
    "Maryland County" = County,
    "Median Income" = median_income,
    "% Working From Home" = pct_wfh,
    "% Households with Internet" = pct_internet,
    "30%+ Renters Burden" = rent_burden,
    "% Population 65+" = pct_65_plus,
    "% Unemployed" = unemployment_rate
  ) |>
  kable(
  caption = "Maryland County Socioeconomic Indicators (2019)"
  ) |>
  kable_classic(
    lightable_options = "striped"
  )

md_2019_table

md_2022_table <- md_clean |>
  mutate(
    unemployment_rate = percent(unemployment_rate, accuracy = 0.01),
    pct_wfh = percent(pct_wfh, accuracy = 0.01),
    pct_internet = percent(pct_internet, accuracy = 0.01),
    rent_burden = percent(rent_burden, accuracy = 0.01),
    pct_65_plus = percent(pct_65_plus, accuracy = 0.01),
    median_income = dollar(median_income)
  ) |>
  filter(
    year == 2022
  ) |>
  select(
    "Maryland County" = County,
    "Median Income" = median_income,
    "% Working From Home" = pct_wfh,
    "% Households with Internet" = pct_internet,
    "30%+ Renters Burden" = rent_burden,
    "% Population 65+" = pct_65_plus,
    "% Unemployed" = unemployment_rate
  ) |>
  kable(
    caption = "Maryland County Socioeconomic Indicators (2022)"
  ) |>
  kable_classic(
    lightable_options = "striped"
  )

md_2022_table

## Create Data Visualization
md_visualization <- md_clean |>
ggplot(
  mapping = aes(
    x = pct_internet,
    y = pct_wfh,
    size = pct_65_plus,
    color = median_income
    )
  ) +
  geom_point(alpha = 1) +
  facet_wrap(~year) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Remote Work, Connectivity, Income, and Age Across Maryland Counties",
    subtitle = "US Census Data from 2019-2022",
    alt = "Scatter plot showing remote work trends, internet access, income, 
    and aging in Maryland counties for 2019 and 2022.",
    x = "% Internet Access",
    y = "% Working From Home",
    size = "% Age 65+",
    color = "Median Income"
  ) +
  theme_bw()

# Long Description:
#' The image is a comparative scatter plot divided into two panels, presenting 
#' data from 2019 and 2022 on remote work, internet access, income, and aging 
#' across Maryland counties. Each panel displays county data points represented 
#' as circles, plotted according to "% Internet Access" on the x-axis and 
#' "% Working From Home" on the y-axis. The size of each circle indicates the 
#' percentage of the population aged 65 or older, with larger circles 
#' representing higher percentages. Color gradients from dark to light blue 
#' depict median income levels, ranging from 40,000 to 140,000. In 2019, data 
#' points concentrate at lower working-from-home and internet access percentages, 
#' while in 2022, the distribution shifts upwards, indicating an increase in 
#' remote work and internet access. (5/5/2026)

md_visualization

ggsave("test.png")

## (Optional) Data Analysis/Narrative Text


