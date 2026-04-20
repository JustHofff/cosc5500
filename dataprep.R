### ACS SECTION ###
library(tidycensus)
library(tidyverse)

census_api_key(Sys.getenv("CENSUS_API_KEY"), install = FALSE)

# Fetch ACS data
acs_vars <- c(
  median_home_value = "B25077_001",
  median_gross_rent = "B25064_001",
  median_hh_income  = "B19013_001",
  total_population  = "B01003_001"
)

acs_raw <- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = acs_vars,
  year      = 2022,
  survey    = "acs5",
  output    = "wide"
)

# Clean up ACS data
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
    size_category  = factor(case_when(
      total_population >= 2e6   ~ "Major",
      total_population >= 5e5   ~ "Large",
      total_population >= 1.5e5 ~ "Medium",
      TRUE                      ~ "Small"
    ), levels = c("Small", "Medium", "Large", "Major"))
  )

# Sanity checks
#nrow(acs_clean)
#acs_clean %>% count(size_category)

# Save checkpoint
saveRDS(acs_clean, "data/raw/acs_clean.rds")


### CBSA SECTION ###
library(tigris)
library(sf)

options(tigris_use_cache = TRUE)

# Fetch CBSA data
cbsa_shapes <- core_based_statistical_areas(cb = TRUE, year = 2021) %>%
  st_transform(4326)

# Join shapes to the ACS data
metros_sf <- cbsa_shapes %>%
  select(GEOID, geometry) %>%
  inner_join(st_drop_geometry(acs_clean), by = "GEOID")

# Sanity checks
#nrow(metros_sf)
#plot(st_geometry(metros_sf))

# Save checkpoint
saveRDS(metros_sf, "data/raw/metros_sf.rds")


### BLS SECTION ###
library(httr)
library(readr)

# BLS file of metro unemployment rates
bls_url <- "https://www.bls.gov/web/metro/ssamatab1.txt"

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

# Sanity Checks
#glimpse(bls_raw)
#head(bls_raw)

bls_clean <- bls_raw %>%
  mutate(
    # Trim whitespace from all character columns
    area_fips         = str_trim(area_fips),
    area_title        = str_trim(area_title),
    # Remove commas from numeric columns and convert
    labor_force       = as.numeric(str_remove_all(labor_force, ",")),
    employment        = as.numeric(str_remove_all(employment, ",")),
    unemployment      = as.numeric(str_remove_all(unemployment, ","))
  ) %>%
  # Keep only the most recent 12 months and average them
  group_by(area_fips) %>%
  slice_max(order_by = year * 100 + month, n = 12) %>%
  summarize(
    unemployment_rate = mean(unemployment_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Pad area_fips to 5 digits to match the GEOID format
  mutate(area_fips = str_pad(area_fips, width = 5, pad = "0"))

# Sanity Checks
#nrow(bls_clean)
#head(bls_clean)
#bls_clean %>% filter(is.na(unemployment_rate))

# Check big cities
bls_clean %>%
  filter(area_fips %in% c("35620", "31080", "16980")) %>% 
  select(area_fips, unemployment_rate)

bls_clean <- bls_clean %>%
  filter(
    !is.na(unemployment_rate),
    !is.nan(unemployment_rate),
    str_detect(area_fips, "^\\d{5}$")  # keep only valid 5-digit codes
  )

# Join metros with bls data
metros_sf <- readRDS("data/raw/metros_sf.rds") %>%
  left_join(bls_clean, by = c("GEOID" = "area_fips"))

# Check how many metros got a match
metros_sf %>%
  st_drop_geometry() %>%
  summarize(
    total        = n(),
    has_bls_data = sum(!is.na(unemployment_rate)),
    missing_bls  = sum(is.na(unemployment_rate))
  )

# Check major metros have data
metros_sf %>%
  st_drop_geometry() %>%
  filter(size_category == "Major") %>%
  select(NAME, unemployment_rate) %>%
  arrange(NAME)

saveRDS(metros_sf, "data/raw/metros_sf.rds")


### COMBINING SECTION ###
# Scoring function
scale01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
}

master_metros <- metros_sf %>%
  mutate(
    # Affordability — lower rent-to-income ratio is better
    afford_score      = 100 - scale01(rent_to_income),
    # Job market — lower unemployment is better
    job_score         = 100 - scale01(unemployment_rate),
    # Home value — lower is better
    homevalue_score   = 100 - scale01(median_home_value)
  )

# Remove Metro & Micro in area names
master_metros <- master_metros %>%
  mutate(NAME = str_remove(NAME, " Metro Area$") %>%
           str_remove(" Micro Area$"))

# Check the scores
master_metros %>%
  st_drop_geometry() %>%
  select(NAME, afford_score, job_score, homevalue_score) %>%
  arrange(desc(afford_score)) %>%
  head(10)

saveRDS(master_metros, "data/master_metros.rds")
