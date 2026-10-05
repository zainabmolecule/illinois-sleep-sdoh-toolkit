# ── Script #5: USDA Food Access + NCES CCD + County Health Rankings ──────
# Download before running:
# USDA: ers.usda.gov/data-products/food-access-research-atlas/download-the-data
#       Save as: data/raw/FoodAccessResearchAtlasData.xlsx
#       Note: Most current data year is 2019.
# CHR&R: countyhealthrankings.org/health-data/methodology-and-sources/data-documentation
#        Save as: data/raw/2025CHR_CSV_Analytic_Data.csv
# NCES CCD: nces.ed.gov/ccd/files.asp(District file, most recent year)
#   Download 1: Nonfiscal > School > 2024-2025 > LUNCH PROGRAM ELIGIBILITY
#               Unzip and save CSV as: data/raw/ccd_lunch.csv
#   Download 2: Nonfiscal > District > 2024-2025 > DIRECTORY
#               Unzip and save CSV as: data/raw/ccd_lea_directory.csv
#   Note: Free/reduced lunch counts not available for all states.
#         Illinois does not report these. 46 other states do.
#   Note: ZIP-to-county crosswalk may assign some districts to multiple
#         counties. Enrollment counts are used as descriptive context only.
# ────────────────────────────────────────────────

library(tidyverse)
library(readxl)
library(janitor)

# ══ PART A: USDA Food Access Research Atlas ══════════════════════
usda_raw <- read_excel(
  "data/raw/FoodAccessResearchAtlasData.xlsx",
  sheet = "Food Access Research Atlas"
)

# ── Check column names before cleaning ─────────────────────────
usda_raw %>% clean_names() %>% names() %>%
  .[grep("lila|lapop|lahunv|tract|state", .)] %>%
  head(20)


# ── Then run the cleaning code ──────────────────────────────────────────────────
usda_county <- usda_raw %>%
  clean_names() %>%
  filter(state == "Illinois") %>%
  mutate(
    fips = str_pad(as.character(census_tract), 11, "left", "0") %>%
      str_sub(1, 5),
    lapophalfshare = as.numeric(lapophalfshare),
    lahunv1share   = as.numeric(lahunv1share)
  ) %>%
  group_by(fips) %>%
  summarise(
    pct_lila_tracts       = round(mean(lila_tracts_1and10, na.rm=TRUE) * 100, 2),
    mean_dist_supermarket = round(mean(lapophalfshare,      na.rm=TRUE), 4),
    pct_no_vehicle_far    = round(mean(lahunv1share,        na.rm=TRUE) * 100, 2)
  ) %>%
  ungroup()

cat("USDA county rows:", nrow(usda_county), "\n")
write_csv(usda_county, "data/clean/05a_usda_food_access_clean.csv")

# ══ PART B: County Health Rankings 2025 ════════════════════════
chr_raw <- read_csv(
  "data/raw/2025CHR_CSV_Analytic_Data.csv",
  skip = 1
)
chr_clean <- chr_raw %>%
  clean_names() %>%
  filter(state == "IL",
         fipscode != "17000") %>%       # remove state summary row
  mutate(
    fips = str_pad(as.character(fipscode), 5, "left", "0")
  ) %>%
  select(
    fips,
    premature_death     = v001_rawvalue,
    poor_health_days    = v036_rawvalue,
    poor_mental_days    = v042_rawvalue,
    social_associations = v140_rawvalue,
    voter_turnout       = v153_rawvalue,
    adult_smoking       = v009_rawvalue,
    adult_obesity       = v011_rawvalue,
    food_insecurity     = v139_rawvalue
  ) %>%
  mutate(
    across(everything(), as.numeric),
    # Convert proportions to percentages
    voter_turnout   = voter_turnout   * 100,
    adult_smoking   = adult_smoking   * 100,
    adult_obesity   = adult_obesity   * 100,
    food_insecurity = food_insecurity * 100
  )

# Note: Column codes change with each annual CHR&R release.
# Run names(chr_raw) and check the CHR codebook to verify codes.
cat("CHR&R county rows:", nrow(chr_clean), "\n")
write_csv(chr_clean, "data/clean/05b_chr_clean.csv")

# ══ PART C: NCES CCD Lunch Program Eligibility ════════════════════

# ── Set your state FIPS here ──────────────────────────────
STATE_FIPS <- "17"   # Illinois = 17. Change for other states.

# ── Download Census TIGER ZIP-to-county crosswalk ──────────────────
tiger_url <- "https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_county20_natl.txt"

zip_county_xwalk <- read.delim(tiger_url, sep = "|") %>%
  clean_names() %>%
  select(geoid_zcta5_20, geoid_county_20) %>%
  rename(zip = geoid_zcta5_20, fips = geoid_county_20) %>%
  mutate(
    zip  = as.character(zip),
    fips = as.character(fips)
  ) %>%
  filter(str_sub(fips, 1, 2) == STATE_FIPS) %>%
  distinct()

cat("ZIP-county pairs downloaded:", nrow(zip_county_xwalk), "\n")

# ── Load district directory — use ZIP to get county FIPS ─────────────
dir_raw <- read.csv("data/raw/ccd_lea_directory.csv",
                    stringsAsFactors = FALSE) %>%
  clean_names() %>%
  filter(as.character(fipst) == STATE_FIPS) %>%
  mutate(
    leaid = as.character(leaid),
    zip   = str_pad(as.character(mzip), 5, "left", "0")
  )

# Valid IL county FIPS — Illinois uses odd numbers only (17001 to 17203)
valid_il_fips <- sprintf("17%03d", seq(1, 203, by = 2))

# Join ZIP crosswalk — allow many-to-many for full county coverage
ccd_dir <- dir_raw %>%
  left_join(zip_county_xwalk, by = "zip",
            relationship = "many-to-many") %>%
  filter(!is.na(fips), fips %in% valid_il_fips) %>%
  select(leaid, fips) %>%
  distinct()

cat("Districts with valid county FIPS:", nrow(ccd_dir), "\n")
cat("Unique counties:", n_distinct(ccd_dir$fips), "\n")

# ── Load lunch file ───────────────────────────────────
ccd_lunch <- read.csv("data/raw/ccd_lunch.csv",
                      stringsAsFactors = FALSE) %>%
  clean_names() %>%
  filter(as.character(fipst) == STATE_FIPS)

cat("School lunch rows:", nrow(ccd_lunch), "\n")

# ── Check if your state reports free lunch counts ─────────────────
lunch_check <- ccd_lunch %>%
  filter(lunch_program == "Free lunch qualified",
         total_indicator == "Category Set A",
         !is.na(student_count)) %>%
  nrow()

cat("Schools with free lunch counts:", lunch_check, "\n")

if (lunch_check == 0) {
  cat("NOTE: Your state does not report free lunch counts in this file.\n")
  cat("Only total enrollment and district count will be available.\n")
  cat("Food insecurity is captured via CHR&R food_insecurity variable.\n")
} else {
  cat("Free lunch data available. Full aggregation will proceed.\n")
}

# ── Extract total enrollment ──────────────────────────────
total_enroll <- ccd_lunch %>%
  filter(data_group == "Free and Reduced-price Lunch Table",
         lunch_program == "No Category Codes",
         total_indicator == "Education Unit Total") %>%
  select(ncessch, leaid, student_count) %>%
  rename(total_students = student_count) %>%
  mutate(
    total_students = as.numeric(total_students),
    leaid          = as.character(leaid)
  )

# ── Extract free lunch counts ─────────────────────────────
free_lunch <- ccd_lunch %>%
  filter(data_group == "Free and Reduced-price Lunch Table",
         lunch_program == "Free lunch qualified",
         total_indicator == "Category Set A") %>%
  select(ncessch, student_count) %>%
  rename(free_lunch_count = student_count) %>%
  mutate(free_lunch_count = as.numeric(free_lunch_count))

# ── Extract reduced lunch counts ───────────────────────────
reduced_lunch <- ccd_lunch %>%
  filter(data_group == "Free and Reduced-price Lunch Table",
         lunch_program == "Reduced-price lunch qualified",
         total_indicator == "Category Set A") %>%
  select(ncessch, student_count) %>%
  rename(reduced_lunch_count = student_count) %>%
  mutate(reduced_lunch_count = as.numeric(reduced_lunch_count))

# ── Join at school level ────────────────────────────────
school_lunch <- total_enroll %>%
  left_join(free_lunch,    by = "ncessch") %>%
  left_join(reduced_lunch, by = "ncessch")

# ── Join district directory to get county FIPS ───────────────────
school_county <- school_lunch %>%
  mutate(leaid = as.character(leaid)) %>%
  left_join(ccd_dir, by = "leaid",
            relationship = "many-to-many")

cat("Schools with county FIPS:", sum(!is.na(school_county$fips)), "\n")
cat("Unique counties in schools:", n_distinct(school_county$fips, na.rm=TRUE), "\n")

# ── Load valid county FIPS from ACS for filtering ──────────────────
acs_fips <- read_csv("data/clean/01_acs_clean.csv",
                     col_types = cols(fips = col_character())) %>%
  pull(fips)

# ── Aggregate to county level ──────────────────────────────
ccd_county <- school_county %>%
  filter(!is.na(fips), fips %in% acs_fips) %>%
  group_by(fips) %>%
  summarise(
    n_districts      = n_distinct(leaid),
    total_enrollment = sum(total_students,      na.rm = TRUE),
    free_lunch       = sum(free_lunch_count,     na.rm = TRUE),
    reduced_lunch    = sum(reduced_lunch_count,  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    pct_free_lunch    = ifelse(lunch_check > 0,
                               round(free_lunch / total_enrollment * 100, 2),
                               NA_real_),
    pct_reduced_lunch = ifelse(lunch_check > 0,
                               round(reduced_lunch / total_enrollment * 100, 2),
                               NA_real_)
  ) %>%
  select(fips, n_districts, total_enrollment, pct_free_lunch, pct_reduced_lunch)

cat("NCES CCD county rows:", nrow(ccd_county), "\n")
glimpse(ccd_county)
write_csv(ccd_county, "data/clean/05c_nces_ccd_clean.csv")
cat("Script #5 complete. Rows:", nrow(ccd_county), "\n")
