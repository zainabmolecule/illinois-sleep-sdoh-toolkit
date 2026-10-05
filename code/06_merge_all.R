# ── Script #6: Merge All Datasets — Master County CSV ───────────────────────
# Input:  data/clean/01 through 05 CSV files
# Output: data/master/sdoh_il_master.csv
# ────────────────────────────────────────────────────────────────────────────

library(tidyverse)

# ── Load all clean files ─────────────────────────────────────────────────────
acs      <- read_csv("data/clean/01_acs_clean.csv")
places   <- read_csv("data/clean/02_places_clean.csv")
svi      <- read_csv("data/clean/03a_svi_clean.csv")
eji      <- read_csv("data/clean/03b_eji_county_clean.csv")
hpsa     <- read_csv("data/clean/04_hpsa_clean.csv")
usda     <- read_csv("data/clean/05a_usda_food_access_clean.csv")
chr      <- read_csv("data/clean/05b_chr_clean.csv")
nces <- read_csv("data/clean/05c_nces_ccd_clean.csv")

# ── Standardize all FIPS columns as character ─────────────────────────────────
acs   <- acs   %>% mutate(fips = as.character(fips))
places <- places %>% mutate(fips = as.character(fips))
svi   <- svi   %>% mutate(fips = as.character(fips))
eji   <- eji   %>% mutate(fips = as.character(fips))
hpsa  <- hpsa  %>% mutate(fips = as.character(fips))
usda  <- usda  %>% mutate(fips = as.character(fips))
chr   <- chr   %>% mutate(fips = as.character(fips))
nces  <- nces  %>% mutate(fips = as.character(fips))

# ── Merge all on FIPS (left join — keep all ACS counties as base) ────────────
# Check row counts before and after each join to detect FIPS mismatches
cat("ACS base rows:", nrow(acs), "\n")

master <- acs %>%
  left_join(places %>% select(-locationname), by = "fips") %>%
  { cat("After PLACES:", nrow(.), "\n"); . } %>%
  left_join(svi    %>% select(-county_name),  by = "fips") %>%
  { cat("After SVI:",    nrow(.), "\n"); . } %>%
  left_join(eji,                              by = "fips") %>%
  { cat("After EJI:",    nrow(.), "\n"); . } %>%
  left_join(hpsa   %>% select(-county_name),  by = "fips") %>%
  { cat("After HPSA:",   nrow(.), "\n"); . } %>%
  left_join(usda,                             by = "fips") %>%
  { cat("After USDA:",   nrow(.), "\n"); . } %>%
  left_join(chr,                              by = "fips") %>%
  { cat("After CHR:",    nrow(.), "\n"); . } %>%
  left_join(nces,                             by = "fips") %>%
  { cat("After NCES:",   nrow(.), "\n"); . }

# ── Verify: row count should equal number of IL counties (102) ──────────────────
stopifnot(nrow(master) == nrow(acs))   # stops script if rows changed

# ── Quick data quality check ────────────────────────────────────────────────────
cat("\nMissing values per column:\n")
colSums(is.na(master)) %>% sort(decreasing = TRUE) %>% head(15) %>% print()

# ── Set output file label ────────────────────────────────
# Change "il" to your state abbreviation if using a different state
STATE_LABEL <- "il"   # e.g. "ca" for California, "tx" for Texas

# ── Save master file ───────────────────────────────────
master_filename <- paste0("data/master/sdoh_", STATE_LABEL, "_master.csv")
tableau_filename <- paste0("output/tableau/sdoh_", STATE_LABEL, "_tableau.csv")

write_csv(master, master_filename)
write_csv(master, tableau_filename)

cat("\nScript #6 complete.\n")
cat("Master file saved:", master_filename, "\n")
cat("Tableau file saved:", tableau_filename, "\n")
cat("Rows:", nrow(master), "| Columns:", ncol(master), "\n")
