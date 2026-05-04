# Waypoint - Find Your Next City

An interactive R Shiny app that helps you decide where to live by scoring and exploring US metro areas based on what matters most to you.

**Live app:** [jhoff.shinyapps.io/waypoint](https://jhoff.shinyapps.io/waypoint/)

## Features

- **Explore** - Browse 583 US metros on an interactive map, weighted by your priorities
- **Find My City** - Answer a short questionnaire and get personalized metro recommendations
- **Compare** - Select up to 3 cities and compare them side by side with a score chart

## Data Sources

| Source | What It Provides |
|--------|-----------------|
| ACS 2023 5-year (Census) | Median rent, income, home value, population |
| BLS LAUS | Metro unemployment rates |
| FEMA National Risk Index | Natural hazard risk by county |

## Scoring

Each metro is scored across three dimensions (0-100 scale):

- **Housing Affordability** - Based on rent-to-income ratio and median home value
- **Job Market** - Based on unemployment rate
- **Natural Hazard Safety** - Based on FEMA risk index (inverted — higher = safer)

Scores are weighted by your slider settings and combined into a composite score.

## Running Locally

**Requirements:** R, and the following packages:

```r
install.packages(c("shiny", "leaflet", "tidyverse", "sf", "tidycensus",
                   "tigris", "httr", "readr", "plotly"))
```

**Before running data_prep.R**, add your Census API key to `.Renviron`:

```
CENSUS_API_KEY=your_key_here
```

Get a free key at [api.census.gov/data/key_signup.html](https://api.census.gov/data/key_signup.html)

You will also need to manually download:
- BLS LAUS file (`ssamatab1.txt`) from [bls.gov/web/metro/ssamatab1.txt](https://www.bls.gov/web/metro/ssamatab1.txt) - save to `data/raw/`
- FEMA NRI county file (`NRI_Table_Counties.csv`) from [hazards.fema.gov/nri/data-resources](https://hazards.fema.gov/nri/data-resources) - save to `data/raw/`
- CBSA to FIPS file (`cbsa2fipsxw_2023.csv`) from [nber.org/research/data](https://www.nber.org/research/data/census-core-based-statistical-area-cbsa-federal-information-processing-series-fips-county-crosswalk) - save to `data/raw/`

Then run `data_prep.R` once to build `data/master_metros.rds`, and launch the app with:

```r
shiny::runApp()
```