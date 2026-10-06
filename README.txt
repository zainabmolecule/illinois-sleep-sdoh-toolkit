=== SDOH TOOLKIT — PROJECT README ===

Author:           [Zainab Yaseen]
Institution:      [University of Illinois Springfield]
Date completed:    2026-10-03 
Research question:[Which combination of county-level social determinants of health across the five Healthy People 2030 domains is most strongly associated with insufficient sleep prevalence among Illinois counties?How are insufficient sleep and its strongest SDOH correlates, geographically distributed across Illinois counties? ]

--- Analysis configuration ---
State:             il 
Geography:        County level
Outcome variable:  short_sleep_pct 
Predictors:        poverty_rate, unemployment_rate, pct_bachelor_plus, uninsured_rate, pct_lila_tracts, social_associations 
Analysis label:    sleep_sdoh_model 

--- Script run order ---
01_acs.R → 02_places.R → 03_svi_eji.R → 04_hrsa.R
→ 05_usda_nces_chr.R → 06_merge_all.R → 07_analysis.R

--- Data download dates ---
ACS 2020-2024: 2026-09-22
CDC PLACES 2025: 2026-09-27
SVI 2022: 2026-09-27
EJI 2024: 2026-09-27
HRSA AHRF 2024-25: 2026-09-27
USDA Atlas 2019: 2026-10-03
CHR&R 2025: 2026-10-03
NCES CCD 2024-25: 2026-09-27

--- Output files ---
Master CSV:    data/master/sdoh_ il _master.csv
Tableau CSV:   output/tableau/sdoh_ il _tableau.csv
Descriptive:   output/tables/01_descriptive_ sleep_sdoh_model .csv
LM results:    output/tables/03_lm_results_ sleep_sdoh_model .csv
Logit results: output/tables/04_logit_OR_ sleep_sdoh_model .csv
Correlation:   output/figures/02_correlation_ sleep_sdoh_model .png
Diagnostics:   output/figures/03_lm_diagnostics_ sleep_sdoh_model .png

--- GitHub repository ---
Repository URL: [your GitHub URL]
Visibility:     Public

--- R environment ---
R version: 4 . 6.1 
OS: Darwin 25.4.0 

--- Package versions ---
tidycensus : 1.8.1 
tidyverse : 2.0.0 
haven : 2.5.5 
readxl : 1.5.0.1 
janitor : 2.2.1 
skimr : 2.2.2 
corrplot : 0.95 
car : 3.1.5 
broom : 1.0.13 
lmtest : 0.9.40 
ResourceSelection : 0.3.6 
sandwich : 3.1.3 
devtools : 2.5.2 
--- Tableau Public ---

Interactive dashboard and story:
https://public.tableau.com/views/Illinois-Sleep-SDOH-Draft-1/Story1?:language=en-US&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link 
--- Project Links ---

GitHub repository:
https://github.com/zainabmolecule/illinois-sleep-sdoh-toolkit 

Tableau Public dashboard and story:
https://public.tableau.com/views/Illinois-Sleep-SDOH-Draft-1/Story1?:language=en-US&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link 
