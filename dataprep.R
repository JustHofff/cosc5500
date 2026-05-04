# Clean up environ
rm(list = ls())

# Import libraries
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(httr)
library(readr)

# Create data folders
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)


### SCORING FUNCTION ###

scale01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
}


### ACS SECTION ###

census_api_key(Sys.getenv("CENSUS_API_KEY"), install = FALSE)

acs_vars <- c(
  median_home_value = "B25077_001",
  median_gross_rent = "B25064_001",
  median_hh_income = "B19013_001",
  total_population = "B01003_001"
)

# Home financial data
acs_raw <- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = acs_vars,
  year = 2023,
  survey = "acs5",
  output = "wide"
)

# Population growth rate data
acs_pop_2022 <- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = c(pop_2022 = "B01003_001"),
  year = 2022,
  survey = "acs5",
  output = "wide"
) %>%
  select(GEOID, pop_2022 = pop_2022E)

acs_clean <- acs_raw %>%
  select(GEOID, NAME, ends_with("E")) %>%
  rename_with(~str_remove(., "E$"), ends_with("E")) %>%
  rename(NAME = NAM) %>%
  filter(total_population > 50000) %>%
  filter(!is.na(median_gross_rent), !is.na(median_hh_income)) %>%
  mutate(
    NAME = str_remove(NAME, ", Metro Area$") %>%
      str_remove(", Micro Area$"),
    rent_to_income = (median_gross_rent * 12) / median_hh_income,
    size_category = factor(case_when(
      total_population >= 2e6 ~ "Major",
      total_population >= 5e5 ~ "Large",
      total_population >= 1.5e5 ~ "Medium",
      TRUE ~ "Small"
    ), levels = c("Small", "Medium", "Large", "Major"))
  ) %>%
  left_join(acs_pop_2022, by = "GEOID") %>%
  mutate(
    pop_growth_pct = (total_population - pop_2022) / pop_2022 * 100
  )

# Sanity checks
# nrow(acs_clean)
# acs_clean %>% count(size_category)
# summary(acs_clean$pop_growth_pct)

saveRDS(acs_clean, "data/raw/acs_clean.rds")


### CBSA SECTION ###

options(tigris_use_cache = TRUE)

# Outlines for metro areas data
cbsa_shapes <- core_based_statistical_areas(cb = TRUE, year = 2021) %>%
  st_transform(4326)

metros_sf <- cbsa_shapes %>%
  select(GEOID, geometry) %>%
  inner_join(st_drop_geometry(acs_clean), by = "GEOID")

saveRDS(metros_sf, "data/raw/metros_sf.rds")


### BLS SECTION ###

# BLS URL: "https://www.bls.gov/web/metro/ssamatab1.txt"

# Unemployment data
bls_raw <- read_fwf(
  "data/raw/ssamatab1.txt",
  fwf_widths(
    c(15, 7, 12, 65, 4, 6, 14, 18, 14, 14),
    c("laus_code", "state_fips", "area_fips", "area_title",
      "year", "month", "labor_force", "employment",
      "unemployment", "unemployment_rate")
  ),
  skip = 5,
  col_types = "cccciicccd"
)

bls_clean <- bls_raw %>%
  mutate(
    area_fips = str_trim(area_fips),
    area_title = str_trim(area_title),
    labor_force = as.numeric(str_remove_all(labor_force, ",")),
    employment = as.numeric(str_remove_all(employment, ",")),
    unemployment = as.numeric(str_remove_all(unemployment, ","))
  ) %>%
  group_by(area_fips) %>%
  slice_max(order_by = year * 100 + month, n = 12) %>%
  summarize(
    unemployment_rate = mean(unemployment_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(area_fips = str_pad(area_fips, width = 5, pad = "0")) %>%
  filter(
    !is.na(unemployment_rate),
    !is.nan(unemployment_rate),
    str_detect(area_fips, "^\\d{5}$")
  )

metros_sf <- readRDS("data/raw/metros_sf.rds") %>%
  left_join(bls_clean, by = c("GEOID" = "area_fips"))

saveRDS(metros_sf, "data/raw/metros_sf.rds")


### FEMA SECTION ###

# Hazard risk data
fema_raw <- read_csv("data/raw/NRI_Table_Counties.csv") %>%
  select(STCOFIPS, RISK_SCORE) %>%
  mutate(GEOID_county = str_pad(as.character(STCOFIPS), width = 5, pad = "0"))

# Maps FIPS to CBSA area codes
county_xwalk <- read_csv("data/raw/cbsa2fipsxw_2023.csv", col_types = cols(.default = "c")) %>%
  mutate(
    GEOID_county = paste0(
      str_pad(fipsstatecode, 2, pad = "0"),
      str_pad(fipscountycode, 3, pad = "0")
    ),
    CBSAFP = str_pad(cbsacode, 5, pad = "0")
  ) %>%
  select(GEOID_county, CBSAFP) %>%
  filter(!is.na(CBSAFP), CBSAFP != "NA")

fema_cbsa <- fema_raw %>%
  left_join(county_xwalk, by = "GEOID_county") %>%
  filter(!is.na(CBSAFP), !is.na(RISK_SCORE)) %>%
  group_by(CBSAFP) %>%
  summarise(avg_risk = mean(RISK_SCORE, na.rm = TRUE), .groups = "drop")

cat("FEMA coverage:", nrow(fema_cbsa), "CBSAs matched\n")


### COMBINING SECTION ###

# Combines all data and calculates scores
master_metros <- readRDS("data/raw/metros_sf.rds") %>%
  left_join(fema_cbsa, by = c("GEOID" = "CBSAFP")) %>%
  mutate(
    housing_score = ((100 - scale01(rent_to_income)) + (100 - scale01(median_home_value))) / 2,
    job_score = 100 - scale01(unemployment_rate),
    fema_score = 100 - scale01(avg_risk)
  ) %>%
  mutate(
    NAME = str_remove(NAME, " Metro Area$") %>%
      str_remove(" Micro Area$")
  )

median_job_score <- median(master_metros$job_score, na.rm = TRUE)
median_fema_score <- median(master_metros$fema_score, na.rm = TRUE)

master_metros <- master_metros %>%
  mutate(
    job_score = if_else(is.na(job_score), median_job_score, job_score),
    fema_score = if_else(is.na(fema_score), median_fema_score, fema_score)
  )

# Sanity checks
cat("Final metro count:", nrow(master_metros), "\n")
cat("Score NAs — housing:", sum(is.na(master_metros$housing_score)),
    "| job:", sum(is.na(master_metros$job_score)),
    "| fema:", sum(is.na(master_metros$fema_score)), "\n")

master_metros %>%
  st_drop_geometry() %>%
  select(NAME, housing_score, job_score, fema_score) %>%
  arrange(desc(housing_score)) %>%
  head(10)

saveRDS(master_metros, "data/master_metros.rds")