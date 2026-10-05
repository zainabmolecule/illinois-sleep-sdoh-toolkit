# ── Script #3: SVI 2022 & EJI 2024 ─────────────────────────────────────────
# SVI Source:  ATSDR. (2023). SVI 2022. 
#              https://www.atsdr.cdc.gov/place-health/php/svi/index.html
# EJI Source:  CDC/ATSDR. (2024). Environmental Justice Index (EJI).
#              https://www.atsdr.cdc.gov/place-health/php/eji/index.html
# ────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(janitor)

# ══ PART A: Social Vulnerability Index (SVI) ════════════════════════════════

svi_raw <- read_csv("data/raw/SVI2022_US_county.csv")

svi_clean <- svi_raw %>%
  clean_names() %>%
  filter(st_abbr == "IL") %>%     # Change 'IL' to filter a different state
  mutate(
    # Pad FIPS to 5 digits — critical for merging
    fips = str_pad(as.character(fips), width = 5, side = "left", pad = "0"),
    
    # SVI flag: -999 means no data — replace with NA
    across(starts_with("rpl_"), ~ ifelse(. == -999, NA, .))
  ) %>%
  select(
    fips,
    county_name  = county,
    svi_overall  = rpl_themes,   # Overall SVI percentile (0-1; higher=more vulnerable)
    svi_socioeco = rpl_theme1,   # Theme 1: Socioeconomic status
    svi_hh_char  = rpl_theme2,   # Theme 2: Household characteristics
    svi_minority = rpl_theme3,   # Theme 3: Racial/ethnic minority & language
    svi_housing  = rpl_theme4,   # Theme 4: Housing type & transportation
    # Key component variables
    pct_poverty  = ep_pov150,    # % persons below 150% poverty line
    pct_unemp_svi= ep_unemp,     # % civilian unemployed
    pct_uninsured_svi = ep_uninsur  # % uninsured
  )

cat("SVI rows (IL counties):", nrow(svi_clean), "\n")
write_csv(svi_clean, "data/clean/03a_svi_clean.csv")

# ══ PART B: Environmental Justice Index (EJI) ═══════════════════════════════
# EJI is tract-level — aggregate to county by averaging

eji_raw <- read_csv("data/raw/EJI_2024_US.csv")

eji_county <- eji_raw %>%
  clean_names() %>%
  mutate(
    fips       = str_sub(geoid, 1, 5),
    state_abbr = stateabbr
  ) %>%
  filter(state_abbr == "IL") %>%
  mutate(across(c(rpl_eji, rpl_ebm, rpl_svm, rpl_hvm),
                ~ ifelse(. == -999, NA, .))) %>%
  group_by(fips) %>%
  summarise(
    eji_overall    = round(mean(rpl_eji, na.rm=TRUE), 4),  # Overall EJI
    eji_env_burden = round(mean(rpl_ebm, na.rm=TRUE), 4),  # Environmental Burden
    eji_social_vuln= round(mean(rpl_svm, na.rm=TRUE), 4),  # Social Vulnerability
    eji_health_vuln= round(mean(rpl_hvm, na.rm=TRUE), 4),  # Health Vulnerability
    n_tracts       = n()
  ) %>%
  ungroup()

cat("EJI county rows (IL):", nrow(eji_county), "\n")
write_csv(eji_county, "data/clean/03b_eji_county_clean.csv")
cat("Script #3 complete.\n")
