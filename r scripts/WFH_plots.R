# Work From Home Visualization Script by Raj
# Purpose: Use Maryland county ACS 5-year data to show how the average
# work-from-home rate changed from 2019 through 2022.

## Load packages:
library(tidycensus)
library(dplyr)
library(ggplot2)
library(scales)
library(purrr)
library(tibble)

## Set the Census API key
census_api_key("6377fd44a462d589653004510dcef417f2f8aabb", overwrite = TRUE, install = TRUE)

## Set variables
wfh_vars <- c(
  total_workers = "B08301_001",
  work_from_home = "B08301_021"
)

## Get work-from-home data for each year
years <- 2019:2022

md_wfh_all <- map_dfr(years, \(yr) {
  get_acs(
    geography = "county",
    state = "MD",
    variables = wfh_vars,
    year = yr,
    survey = "acs5",
    output = "wide"
  ) |>
    mutate(year = yr)
})

## Tidy work-from-home data
md_wfh_clean <- md_wfh_all |>
  transmute(
    GEOID,
    County = NAME,
    year,
    total_workers = total_workersE,
    work_from_home = work_from_homeE,
    pct_wfh = work_from_home / total_workers
  )

## Calculate average work-from-home percentage across Maryland counties
wfh_avg <- md_wfh_clean |>
  group_by(year) |>
  summarise(
    avg_pct_wfh = mean(pct_wfh, na.rm = TRUE),
    .groups = "drop"
  )

## Add 2018 and 2023 to the axis without plotting values for those years
wfh_avg_plot_data <- tibble(
  year = 2018:2023
) |>
  left_join(wfh_avg, by = "year")

## Create work-from-home line plot
wfh_plot <- wfh_avg_plot_data |>
  ggplot(aes(x = year, y = avg_pct_wfh)) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(size = 3, na.rm = TRUE) +
  scale_x_continuous(
    breaks = 2019:2022,
    limits = c(2019, 2022)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, NA)
  ) +
  labs(
    subtitle = "Average percent of workers working from home, ACS 5-year estimates",
    x = "Year",
    y = "% Working from home",
    caption = "Source: U.S. Census Bureau ACS 5-year estimates via tidycensus"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(hjust = 0.5),
    panel.border = element_rect(color = "gray60", fill = NA, linewidth = 0.6)
  )

# Narrative:
# The work-from-home visualization shows a clear and consistent increase in the
# average share of Maryland county workers working from home from 2019 through 2022.
# This shows that remote work became much more common during this period,
# reflecting major world-wide changes consistent with the social distancing requirements.
wfh_plot