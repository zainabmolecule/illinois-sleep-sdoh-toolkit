# ── Script #1: ACS 5-Year Download ─────────────────────────────────────────
# Source:    U.S. Census Bureau, American Community Survey 5-year estimates
# API:       api.census.gov via tidycensus package
# Geography: County level, Illinois (state = 'IL')
# Year:      2024 (most recent 5-year ACS, 2020–2024)
# Citation:  U.S. Census Bureau. (2026). ACS 5-year estimates 2020-2024.
#            https://data.census.gov/
# ────────────────────────────────────────────────────────────────────────────

library(tidycensus)
library(tidyverse)
library(janitor)

# ── Define variables to pull ────────────────────────────────────────────────
# Variable codes: browse with load_variables(2024, 'acs5') in RStudio
acs_vars <- c(
  # Domain 1 — Economic Stability
  total_pop          = "B17001_001",  # total population for poverty denom
  poverty_count      = "B17001_002",  # persons below poverty line
  median_hh_income   = "B19013_001",  # median household income ($)
  unemployed         = "B23025_005",  # unemployed persons in labor force
  labor_force        = "B23025_002",  # total labor force
  snap_hh            = "B22010_002",  # households receiving SNAP
  total_hh           = "B22010_001",  # total households (SNAP denom)
  rent_burden_30plus = "B25070_007",  # gross rent 30–34.9% of income
  rent_burden_35plus = "B25070_008",  # gross rent 35–39.9% of income
  rent_burden_40plus = "B25070_009",  # gross rent 40–49.9% of income
  rent_burden_50plus = "B25070_010",  # gross rent 50%+ of income
  rent_denom         = "B25070_001",  # renter-occupied units (denom)
  
  # Domain 2 — Education Access & Quality
  edu_total_25plus   = "B15003_001",  # total pop 25+ (education denom)
  no_hs_diploma      = "B15003_002",  # less than 9th grade
  hs_graduate        = "B15003_017",  # HS graduate or equivalent
  bachelors          = "B15003_022",  # bachelor's degree
  graduate           = "B15003_023",  # master's degree
  professional       = "B15003_024",  # professional school degree
  doctorate          = "B15003_025",  # doctorate degree
  
  # Domain 3 — cross-domain variable (health insurance)
  uninsured_pct = "DP03_0099P"  # % of residents without health insurance
  # excludes prisoners and nursing home residents
)

# ── Download from Census API ─────────────────────────────────────────────────
acs_raw <- get_acs(
  geography = "county",
  variables = acs_vars,
  state     = "IL",          # Change to 'all' for national data
  year      = 2024,
  survey    = "acs5",
  output    = "wide"          # one row per county, all vars as columns
)

# ── Clean and compute derived rates ─────────────────────────────────────────
acs_clean <- acs_raw %>%
  clean_names() %>%
  rename(county_name = name) %>%
  mutate(
    # Standardize FIPS: always 5-digit character string with leading zeros
    fips = str_pad(geoid, width = 5, side = "left", pad = "0"),
    
    # Compute rates from raw counts
    poverty_rate     = round(poverty_count_e / total_pop_e * 100, 2),
    unemployment_rate= round(unemployed_e   / labor_force_e * 100, 2),
    snap_rate        = round(snap_hh_e       / total_hh_e   * 100, 2),
    
    # Rent cost burden: share of renters paying 30%+ of income on rent
    rent_burden_rate = round(
      (rent_burden_30plus_e + rent_burden_35plus_e +
         rent_burden_40plus_e + rent_burden_50plus_e) / rent_denom_e * 100, 2),
    
    # Education rates (% of adults 25+)
    pct_no_hs = round(no_hs_diploma_e / edu_total_25plus_e * 100, 2),
    pct_hs_grad = round(hs_graduate_e   / edu_total_25plus_e * 100, 2),
    pct_bachelor_plus = round(
      (bachelors_e + graduate_e + professional_e + doctorate_e) /
        edu_total_25plus_e * 100, 2),
    
    # Uninsured rate 
    uninsured_rate = uninsured_pct_e   # already a percentage from DP03_0099P
  ) %>%
  select(fips, county_name, poverty_rate, median_hh_income_e,
         unemployment_rate, snap_rate, rent_burden_rate,
         pct_no_hs, pct_hs_grad, pct_bachelor_plus, uninsured_rate) %>%
  rename(median_hh_income = median_hh_income_e)

# ── Inspect the output ───────────────────────────────────────────────────────
glimpse(acs_clean)
summary(acs_clean)

# ── Save to data/clean/ ──────────────────────────────────────────────────────
write_csv(acs_clean, "data/clean/01_acs_clean.csv")
cat("Script #1 complete. Rows:", nrow(acs_clean), "\n")

