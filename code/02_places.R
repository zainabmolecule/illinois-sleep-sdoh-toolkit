# ── Script #2: CDC PLACES 2025 County Data ──────────────────────
# Source:    Centers for Disease Control and Prevention
# Endpoint:  https://data.cdc.gov/resource/swc5-untb.csv
# Geography: County level — Illinois default. See end of script for tract/ZCTA
# Citation:  CDC. (2025). PLACES: Local data for better health, county data
#            2025 release. https://data.cdc.gov/resource/swc5-untb
# ────────────────────────────────────────────────

library(tidyverse)
library(janitor)

# ── Download PLACES data — Illinois counties only ──────────────────
places_raw <- read.csv(
  "https://data.cdc.gov/resource/swc5-untb.csv?StateAbbr=IL&$limit=10000",
  stringsAsFactors = FALSE
)

# ── Check the download ──────────────────────────────────
cat("Total rows downloaded:", nrow(places_raw), "\n")
names(places_raw)
unique(places_raw$measureid) %>% sort()

# ── Check county coverage per measure ────────────────────────────────────────
places_raw %>%
  filter(datavaluetypeid == "CrdPrv") %>%
  group_by(measureid) %>%
  summarise(n_counties = n()) %>%
  arrange(n_counties) %>%
  View()

# ── Optional: Browse available measures ────────────────────────
# Run these lines to explore what measures are available before choosing
# Browse all measures with descriptions via searchable table in RStudio
places_raw %>%
  select(measureid, measure, category) %>%
  distinct() %>%
  arrange(category, measureid) %>%
  View()

# Search by keyword — change "diabetes" to any topic you are interested in
places_raw %>%
  select(measureid, measure, category) %>%
  distinct() %>%
  filter(str_detect(tolower(measure), "sleep"))

# ── Select key measures and pivot to wide format ───────────────────
measures_keep <- c(
  "DIABETES",
  "OBESITY",
  "BPHIGH",
  "DEPRESSION",
  "COPD",
  "CASTHMA",
  "CSMOKING",
  "LPA",
  "COLON_SCREEN",
  "MAMMOUSE",
  "ACCESS2",
  "SLEEP"
)

places_wide <- places_raw %>%
  filter(measureid %in% measures_keep) %>%
  filter(datavaluetypeid == "CrdPrv") %>%
  select(locationid, locationname, measureid, data_value) %>%
  mutate(
    fips       = str_pad(locationid, width = 5, side = "left", pad = "0"),
    data_value = as.numeric(data_value)
  ) %>%
  pivot_wider(
    id_cols     = c(fips, locationname),
    names_from  = measureid,
    values_from = data_value
  ) %>%
  clean_names() %>%
  rename(
    diabetes_pct          = diabetes,
    obesity_pct           = obesity,
    hypertension_pct      = bphigh,
    depression_pct        = depression,
    copd_pct              = copd,
    asthma_pct            = casthma,
    smoking_pct           = csmoking,
    inactivity_pct        = lpa,
    colorectal_screen_pct = colon_screen,
    mammography_pct       = mammouse,
    uninsured_places = access2,
    short_sleep_pct = sleep
  )

cat("Counties in PLACES data:", nrow(places_wide), "\n")
glimpse(places_wide)
write_csv(places_wide, "data/clean/02_places_clean.csv")
cat("Script #2 complete. Rows:", nrow(places_wide), "\n")
