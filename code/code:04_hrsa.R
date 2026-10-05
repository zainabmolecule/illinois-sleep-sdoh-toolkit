# ── Script #4: HRSA HPSA Shortage Areas from AHRF ─────────────────────────────
# Source:  Health Resources & Services Administration
#          Area Health Resources Files (AHRF) 2024-2025
# URL:     https://data.hrsa.gov/data/download
# Steps:   1. Click "Area Health Resources Files"
#          2. Under County, download: AHRF 2024-2025 County CSV
#          3. Unzip and save AHRF2025.csv as: data/raw/AHRF2025.csv
# Citation: HRSA, Bureau of Health Workforce. (2025). Area Health Resources
#           Files 2024-2025. https://data.hrsa.gov/data/download
# Note:    AHRF is county-level only. Cannot be disaggregated to tract or ZIP.
#          HPSA variables use designation codes:
#          0 = No shortage designation
#          1 = Part of county is designated (partial shortage)
#          2 = Whole county is designated (full shortage)
# ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(janitor)

# ── Load AHRF county file ─────────────────────────────────────────────────────
ahrf_raw <- read_csv("data/raw/AHRF2025.csv")

# ── Inspect column names ──────────────────────────────────────────────────────
names(ahrf_raw) %>% sort()

# ── Filter to Illinois and select HPSA variables ──────────────────────────────
hpsa_clean <- ahrf_raw %>%
  clean_names() %>%
  filter(st_name_abbrev == "IL") %>%   # Change "IL" for other states
  mutate(
    fips = str_pad(as.character(fips_st_cnty), 5, "left", "0")
  ) %>%
  select(
    fips,
    county_name         = cnty_name,
    # 2025 HPSA designation codes (0=none, 1=partial, 2=full)
    hpsa_prim_care      = hpsa_prim_care_25,   # Primary care shortage
    hpsa_dental         = hpsa_dent_25,         # Dental shortage
    hpsa_mental_health  = hpsa_mentl_hlth_25    # Mental health shortage
  ) %>%
  mutate(
    # Convert to numeric
    across(c(hpsa_prim_care, hpsa_dental, hpsa_mental_health), as.numeric),
    # Create binary shortage indicator (1 = any shortage, 0 = none)
    hpsa_any_shortage   = as.integer(
      hpsa_prim_care > 0 | hpsa_dental > 0 | hpsa_mental_health > 0),
    # Create primary care binary (used in logistic regression)
    hpsa_prim_care_designated = as.integer(hpsa_prim_care > 0)
  )

# ── Check distribution ────────────────────────────────────────────────────────
cat("HPSA rows (IL counties):", nrow(hpsa_clean), "\n")

# Primary care shortage distribution
cat("\nPrimary care HPSA designation (0=none, 1=partial, 2=full):\n")
table(hpsa_clean$hpsa_prim_care)

cat("\nDental HPSA designation:\n")
table(hpsa_clean$hpsa_dental)

cat("\nMental health HPSA designation:\n")
table(hpsa_clean$hpsa_mental_health)

cat("\nCounties with ANY shortage designation:", 
    sum(hpsa_clean$hpsa_any_shortage), "\n")

# ── Save ──────────────────────────────────────────────────────────────────────
write_csv(hpsa_clean, "data/clean/04_hpsa_clean.csv")
cat("Script #4 complete.\n")
