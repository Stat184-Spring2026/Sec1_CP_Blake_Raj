# Purpose: Create a viewable table of all the different concepts available to us, to help us brainstorm for ideas. 

library(tidycensus)
library(tidyverse)
library(readr)
library(dplyr)

# Use the API key given to me from The United States Census Bureau 
# "https://www.census.gov/data/developers.html" to recieve census data capabilities
# If this is the first time running, keep "install" parameter, otherwise use "overwrite" parameter
census_api_key("6377fd44a462d589653004510dcef417f2f8aabb", overwrite = TRUE, install = TRUE)

# Load all the variables from the ACS, both 1-year and 5-year, as well as the Decennial census -> Dataframe
all_concepts <- bind_rows(
  load_variables(2022, "acs1", cache = TRUE) |> mutate(source = "ACS 1-year"),
  load_variables(2022, "acs5", cache = TRUE) |> mutate(source = "ACS 5-year"),
  load_variables(2020, "pl", cache = TRUE) |> mutate(source = "Decennial 2020 PL")
) |>
  # Group them to hide the extra subcategories
  group_by(source, concept) |>
  summarise(
    num_variables = n(),
    .groups = "drop"
  ) |>
  arrange(source, concept)

# Open them for viewing
View(all_concepts)


############
# OPTIONAL: Use this helper to find exact subcategories and names for variables, AFTER selecting a concept from earlier

# Pick the ACS year and survey you want to search
year <- 2022
survey <- "acs5"

# Put the concept/table name you want here
target_concept <- "Age by Disability Status (White Alone)"

# Load all ACS variable metadata
acs_vars <- load_variables(year, survey, cache = TRUE)

# Filter to one concept and show variable IDs with their labels
concept_labels <- acs_vars |>
  filter(concept == target_concept) |>
  select(
    name,      # Variable ID, like B08006_001
    concept,    # Parent table/concept
    label,     # Human-readable subcategory
  ) |>
  arrange(name)

# Open in RStudio table viewer
View(concept_labels)