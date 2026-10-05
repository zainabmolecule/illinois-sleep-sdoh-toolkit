# ════════════════════════════════════════════════════════════════════════════
# Script #7: Statistical Analyses
# ════════════════════════════════════════════════════════════════════════════
# EDIT THIS BLOCK ONLY — change your outcome and predictor variables here.
# Everything else in the script uses these names automatically.
# ════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(skimr)
library(corrplot)
library(car)
library(broom)
library(lmtest)   # Breusch-Pagan test for homoscedasticity
library(ResourceSelection)  # Hosmer-Lemeshow test for logistic regression

# ── Load master dataset ────────────────────────────────────────────────────
STATE_LABEL <- "il"   # must match what you set in Script 06
master <- read_csv(paste0("data/master/sdoh_", STATE_LABEL, "_master.csv"))

# ════════════════════════════════════════════════════════════════════════════
# VARIABLE CONFIGURATION — EDIT THIS SECTION TO CHANGE YOUR ANALYSIS
# ════════════════════════════════════════════════════════════════════════════

# ── Step 1: Choose your OUTCOME variable (Y) ────────────────────────────────
# Must be a continuous variable for linear regression.
# It will be dichotomized automatically for logistic regression.
#
# Available outcome options (change the value in quotes below):
#   "diabetes_pct"        — Diagnosed diabetes prevalence (%)
#   "depression_pct"      — Depression prevalence (%)
#   "obesity_pct"         — Adult obesity prevalence (%)
#   "hypertension_pct"    — High blood pressure prevalence (%)
#   "smoking_pct"         — Current smoking prevalence (%)
#   "inactivity_pct"      — Physical inactivity prevalence (%)
#   "premature_death"     — Premature death rate (YPLL per 100k)
#   "poor_mental_days"    — Avg mentally unhealthy days/month
#   "poor_health_days"    — Avg physically unhealthy days/month
#
OUTCOME_VAR <- "short_sleep_pct"

# ── Step 2: Choose your PREDICTOR variables (X) ──────────────────────────────
# List 3-7 variables. More than 7 is too many for n=102 counties.
#
# Available predictor options:
#  -- Domain 1: Economic Stability --
#   "poverty_rate"        — % persons below poverty line
#   "unemployment_rate"   — % civilian labor force unemployed
#   "median_hh_income"    — median household income ($)
#   "snap_rate"           — % households receiving SNAP
#   "rent_burden_rate"    — % renters paying 30%+ income on rent
#
#  -- Domain 2: Education --
#   "pct_no_hs"           — % adults 25+ with less than HS diploma
#   "pct_bachelor_plus"   — % adults 25+ with bachelor's degree or higher
#   "pct_free_lunch"      — % students eligible for free lunch (NCES CCD)
#   "total_enrollment"    — total student enrollment in county (NCES CCD)
#
#  -- Domain 3: Health Care Access --
#   "uninsured_rate"            — % population uninsured (ACS)
#   "hpsa_prim_care"            — primary care shortage (0=none, 1=partial, 2=full)
#   "hpsa_dental"               — dental shortage (0=none, 1=partial, 2=full)
#   "hpsa_mental_health"       — mental health shortage (0=none, 1=partial, 2=full)
#   "hpsa_any_shortage"         — any shortage designation (1=yes, 0=no)
#   "hpsa_prim_care_designated" — primary care shortage binary (1=yes, 0=no)
#
#  -- Domain 4: Built Environment --
#   "pct_lila_tracts"     — % tracts that are low-income & low-access food
#   "eji_overall"         — EJI overall environmental burden percentile
#   "eji_env_burden"      — EJI environmental burden percentile
#
#  -- Domain 5: Social & Community Context --
#   "svi_overall"         — SVI overall vulnerability percentile (0-1)
#   "svi_socioeco"        — SVI Theme 1: socioeconomic status
#   "svi_minority"        — SVI Theme 3: minority status & language (as defined by #                                        CDC/ATSDR SVI documentation)
#   "social_associations" — membership organizations per 100k
#   "voter_turnout"       — % voting-age pop who voted
#
PREDICTOR_VARS <- c(
  "poverty_rate",
  "unemployment_rate",
  "pct_bachelor_plus",
  "uninsured_rate",
  "pct_lila_tracts",
  "social_associations"
)

# ── Step 3 (optional): Add a label for your output files ─────────────────────
ANALYSIS_LABEL <- "sleep_sdoh_model"  # <── CHANGE TO DESCRIBE YOUR MODEL

# ── Build analysis dataset from your chosen variables ─────────────────────────
all_vars   <- c("fips", "county_name", OUTCOME_VAR, PREDICTOR_VARS)
analysis_df <- master %>%
  select(all_of(all_vars)) %>%
  drop_na()

cat("\n── Analysis configuration ──────────────────────────────\n")
cat("Outcome:    ", OUTCOME_VAR, "\n")
cat("Predictors: ", paste(PREDICTOR_VARS, collapse=", "), "\n")
cat("Sample size:", nrow(analysis_df), "observations (after removing missing)\n")
cat("Counties dropped due to missing:", nrow(master) - nrow(analysis_df), "\n")
# ══ PART C: Descriptive Statistics ══════════════════════════════════════════
# Uses OUTCOME_VAR and PREDICTOR_VARS defined in the configuration block above.

# Full skim of all key variables
skim_output <- analysis_df %>%
  select(all_of(c(OUTCOME_VAR, PREDICTOR_VARS))) %>%
  skim()
print(skim_output)

# ── Formatted summary table ───────────────────────────────────────────────────
desc_stats <- analysis_df %>%
  select(all_of(c(OUTCOME_VAR, PREDICTOR_VARS))) %>%
  pivot_longer(everything(), names_to="variable", values_to="value") %>%
  group_by(variable) %>%
  summarise(
    n       = sum(!is.na(value)),
    missing = sum(is.na(value)),
    mean    = round(mean(value,   na.rm=TRUE), 2),
    sd      = round(sd(value,     na.rm=TRUE), 2),
    min     = round(min(value,    na.rm=TRUE), 2),
    median  = round(median(value, na.rm=TRUE), 2),
    max     = round(max(value,    na.rm=TRUE), 2)
  )

print(desc_stats)
write_csv(desc_stats,
          paste0("output/tables/01_descriptive_", ANALYSIS_LABEL, ".csv"))
# ══ PART D: Correlation Matrix ═══════════════════════════════

cor_data <- analysis_df %>%
  select(all_of(c(OUTCOME_VAR, PREDICTOR_VARS))) %>%
  drop_na()

cor_matrix <- cor(cor_data, method="pearson")

# ── Plot ─────────────────────────────────────────────────────────────────────
corrplot(
  cor_matrix,
  method      = "color",
  type        = "upper",
  order       = "hclust",
  addCoef.col = "black",
  tl.col      = "black",
  tl.srt      = 45,
  number.cex  = 0.75,
  title       = paste("Correlation Matrix —", OUTCOME_VAR),
  mar         = c(0,0,2,0)
)

# Save plot
png(paste0("output/figures/02_correlation_", ANALYSIS_LABEL, ".png"),
    width=1400, height=1200, res=150)
corrplot(cor_matrix, method="color", type="upper", order="hclust",
         addCoef.col="black", tl.col="black", tl.srt=45, number.cex=0.75)
dev.off()

# ── Flag high inter-predictor correlations ────────────────────────────────────
pred_cor <- cor(analysis_df %>% select(all_of(PREDICTOR_VARS)), method="pearson")
high_pairs <- which(abs(pred_cor) > 0.70 & upper.tri(pred_cor), arr.ind=TRUE)

if (nrow(high_pairs) > 0) {
  cat("\nWARNING — High correlations (r > 0.70) between predictors:\n")
  for (i in seq_len(nrow(high_pairs))) {
    r1 <- rownames(pred_cor)[high_pairs[i,1]]
    r2 <- colnames(pred_cor)[high_pairs[i,2]]
    r  <- round(pred_cor[high_pairs[i,1], high_pairs[i,2]], 2)
    cat(" ", r1, "vs", r2, ":", r, "\n")
  }
  cat("ACTION: Consider removing one variable from each highly correlated pair.\n")
} else {
  cat("\nNo high inter-predictor correlations (all r < 0.70). Proceed.\n")
}
# ══ PART E: Multiple Linear Regression + Assumption Checks ════════════

# ── Unadjusted models — one per predictor ──────────────────────
# Standard epidemiological practice: run one unadjusted model per predictor
# Each gives a crude regression coefficient (unadjusted β)
cat("\n── Unadjusted linear models (crude β — one predictor each) ──\n")

unadj_lm_results <- map_dfr(PREDICTOR_VARS, function(var) {
  formula_unadj <- as.formula(paste(OUTCOME_VAR, "~", var))
  model_unadj   <- lm(formula_unadj, data = analysis_df)
  tidy(model_unadj, conf.int = TRUE) %>%
    filter(term == var) %>%
    mutate(model = "Unadjusted", predictor = var,
           across(where(is.numeric), ~round(., 4)))
})

print(unadj_lm_results %>%
        select(predictor, estimate, conf.low, conf.high, p.value))

# ── Fully adjusted model — all predictors together ────────────────
cat("\n── Fully adjusted model (all predictors) ──\n")
formula_adj <- as.formula(paste(OUTCOME_VAR, "~",
                                paste(PREDICTOR_VARS, collapse=" + ")))
model_adj   <- lm(formula_adj, data = analysis_df)
summary(model_adj)

# ════════════════════════════════════════════════════════════════════════════
# ASSUMPTION CHECKS — MULTIPLE LINEAR REGRESSION
# ════════════════════════════════════════════════════════════════════════════

cat("\n══ ASSUMPTION CHECKS: MULTIPLE LINEAR REGRESSION ══\n")

# ── Assumption 1 & 3 & 5: Diagnostic plots ───────────────────────────────────
# Four plots: (1) Residuals vs Fitted, (2) Normal Q-Q,
#             (3) Scale-Location,       (4) Residuals vs Leverage
png(paste0("output/figures/03_lm_diagnostics_", ANALYSIS_LABEL, ".png"),
    width=1600, height=1400, res=150)
par(mfrow=c(2,2))
plot(model_adj, main=paste("LM Diagnostics —", OUTCOME_VAR))
par(mfrow=c(1,1))
dev.off()
cat("Diagnostic plots saved to output/figures/\n")
cat("Interpret: (1) Residuals vs Fitted — no pattern = linearity + homoscedasticity OK\n")
cat("           (2) Q-Q plot — points on line = normality OK\n")
cat("           (3) Scale-Location — flat red line = homoscedasticity OK\n")
cat("           (4) Cook's Distance — no points > dashed line = no influential outliers\n")

# ── Assumption 3: Breusch-Pagan test for homoscedasticity ────────────────────
cat("\n── Assumption 3: Homoscedasticity (Breusch-Pagan test) ──\n")
bp_test <- bptest(model_adj)
print(bp_test)
if (bp_test$p.value < 0.05) {
  cat("RESULT: p <0.05 — heteroscedasticity detected (unequal variance).\n")
  cat("ACTION: Use heteroscedasticity-consistent (robust) standard errors.\n")
  cat("        Run: library(sandwich); coeftest(model_adj, vcov=vcovHC(model_adj))\n")
} else {
  cat("RESULT: p >", round(bp_test$p.value,3), "— homoscedasticity assumption met.\n")
}

# ── Assumption 4: Shapiro-Wilk test for normality of residuals ───────────────
cat("\n── Assumption 4: Normality of residuals (Shapiro-Wilk test) ──\n")
sw_test <- shapiro.test(residuals(model_adj))
print(sw_test)
if (sw_test$p.value < 0.05) {
  cat("RESULT: p <0.05 — residuals may not be normally distributed.\n")
  cat("NOTE:   With n=102, linear regression is generally robust to mild\n")
  cat("        non-normality. Check the Q-Q plot visually.\n")
  cat("        If Q-Q plot shows heavy skew, consider log-transforming the outcome.\n")
} else {
  cat("RESULT: p >", round(sw_test$p.value,3), "— normality assumption met.\n")
}

# ── Assumption 5: Cook's Distance — influential observations ─────────────────
cat("\n── Assumption 5: Influential observations (Cook's Distance) ──\n")
cooks_d    <- cooks.distance(model_adj)
cutoff     <- 4 / nrow(analysis_df)   # common threshold: 4/n
influential <- which(cooks_d > cutoff)
cat("Cook's D threshold (4/n):", round(cutoff, 4), "\n")
if (length(influential) > 0) {
  cat("Influential observations (Cook's D >", round(cutoff,4), "):\n")
  print(analysis_df[influential, c("fips","county_name", OUTCOME_VAR)])
  cat("ACTION: Run sensitivity analyses – see Follow-up 2 below.\n")
} else {
  cat("No influential outliers detected.\n")
}

# ── Multicollinearity: Variance Inflation Factor (VIF) ───────────────────────
cat("\n── Multicollinearity check: Variance Inflation Factor (VIF) ──\n")
cat("VIF < 5 = acceptable | VIF 5-10 = moderate concern | VIF > 10 = serious problem\n")
vif_vals <- vif(model_adj)
print(round(vif_vals, 2))
if (any(vif_vals > 5)) {
  cat("WARNING: VIF > 5 detected. Consider removing correlated predictors.\n")
  cat("High VIF variables:", names(which(vif_vals > 5)), "\n")
} else {
  cat("VIF OK — no multicollinearity problem detected.\n")
}

# ── Model results table ───────────────────────────────────────────────────────
cat("\n── Final regression results ──\n")
results_lm <- tidy(model_adj, conf.int=TRUE) %>%
  mutate(across(where(is.numeric), ~round(.,4)))
print(results_lm)

fit_lm <- glance(model_adj)
cat("R² =", round(fit_lm$r.squared,3),
    " | Adj. R² =", round(fit_lm$adj.r.squared,3),
    " | F-stat p =", round(fit_lm$p.value,4), "\n")

write_csv(results_lm,
          paste0("output/tables/03_lm_results_", ANALYSIS_LABEL, ".csv"))
# ════════════════════════════════════════════════
# FOLLOW-UP ANALYSES BASED ON ASSUMPTION CHECKS
# ════════════════════════════════════════════════

library(sandwich)  # robust standard errors

# ── Follow-up 1: Robust standard errors (required if Breusch-Pagan p < 0.05) ──
if (bp_test$p.value < 0.05) {
  cat("\n── Robust standard errors (heteroscedasticity correction) ──\n")
  library(sandwich)
  robust_results <- coeftest(model_adj, vcov = vcovHC(model_adj))
  print(robust_results)
  cat("NOTE: Use these standard errors and p-values for reporting.\n")
  cat("      Coefficients are identical — only standard errors change.\n")
}

# ── Follow-up 2: Sensitivity analysis (remove influential observations) ─────
if (length(influential) > 0) {
  cat("\n── Sensitivity analysis (removing influential observations) ──\n")
  
  # Use the influential observations flagged by Cook's Distance above
  influential_fips <- analysis_df$fips[influential]
  cat("Removing observations with FIPS:", influential_fips, "\n")
  
  analysis_df_trim <- analysis_df %>%
    filter(!fips %in% influential_fips)
  
  model_trim <- lm(formula_adj, data = analysis_df_trim)
  
  cat("Trimmed model (n =", nrow(analysis_df_trim), "):\n")
  summary(model_trim)
  
  cat("\nComparison: key coefficients:\n")
  cat("                    Full model    Trimmed model\n")
  for (v in PREDICTOR_VARS) {
    full_coef  <- round(coef(model_adj)[v], 3)
    trim_coef  <- round(coef(model_trim)[v], 3)
    cat(sprintf("%-20s %10s    %10s\n", v, full_coef, trim_coef))
  }
  cat("\nIf coefficients are similar, results are robust to influential observations.\n")
}
# ══ PART F: Logistic Regression & Assumption Checks ═════════════════════════
library(ResourceSelection)  # Hosmer-Lemeshow test

# ── Create binary outcome at the state median ─────────────────────────────────
state_median <- median(analysis_df[[OUTCOME_VAR]], na.rm=TRUE)
binary_outcome <- paste0(OUTCOME_VAR, "_high")

logistic_df <- analysis_df %>%
  mutate(
    !!binary_outcome := as.integer(.data[[OUTCOME_VAR]] > state_median)
  )

n_events   <- sum(logistic_df[[binary_outcome]] == 1, na.rm=TRUE)
n_nonevent <- sum(logistic_df[[binary_outcome]] == 0, na.rm=TRUE)

cat("\nBinary outcome:", binary_outcome, "\n")
cat("  Threshold (state median):", round(state_median, 2),
    "[", OUTCOME_VAR, "]\n")
cat("  High (", binary_outcome, "= 1):", n_events, "observations\n")
cat("  Low  (", binary_outcome, "= 0):", n_nonevent, "observations\n")

# ── Assumption 5 check: events-per-variable ───────────────────────────────────
cat("\n── Assumption 5: Sample size adequacy ──\n")
epv <- n_events / length(PREDICTOR_VARS)
cat("Events per predictor variable (EPV):", round(epv,1), "\n")
if (epv < 10) {
  cat("WARNING: EPV <10. Consider reducing predictor count to",
      floor(n_events/10), "or fewer.\n")
} else {
  cat("EPV OK (>=10). Sample size is adequate.\n")
}

# ── Unadjusted models — one per predictor ────────────────────────────────────
# Standard epidemiological practice: run one unadjusted model per predictor
# Each gives a crude odds ratio (unadjusted OR)
cat("\n── Unadjusted logistic models (crude OR — one predictor each) ──\n")

unadj_results <- map_dfr(PREDICTOR_VARS, function(var) {
  formula_unadj <- as.formula(paste(binary_outcome, "~", var))
  model_unadj   <- glm(formula_unadj, data = logistic_df,
                       family = binomial(link = "logit"))
  tidy(model_unadj, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term == var) %>%
    mutate(model = "Unadjusted", predictor = var,
           across(where(is.numeric), ~round(., 4)))
})

print(unadj_results %>%
        select(predictor, estimate, conf.low, conf.high, p.value))

# ── Fully adjusted logistic model — all predictors together ──────────────────
cat("\n── Fully adjusted logistic model (all predictors) ──\n")
formula_logit <- as.formula(
  paste(binary_outcome, "~", paste(PREDICTOR_VARS, collapse=" + "))
)
logit_model <- glm(formula_logit, data=logistic_df,
                   family=binomial(link="logit"))
summary(logit_model)

# ════════════════════════════════════════════════════════════════════════════
# ASSUMPTION CHECKS — LOGISTIC REGRESSION
# ════════════════════════════════════════════════════════════════════════════

cat("\n══ ASSUMPTION CHECKS: LOGISTIC REGRESSION ══\n")

# ── Assumption 3: VIF — multicollinearity ─────────────────────────────────────
cat("\n── Assumption 3: Multicollinearity (VIF) ──\n")
vif_logit <- vif(logit_model)
print(round(vif_logit, 2))
if (any(vif_logit > 5)) {
  cat("WARNING: VIF > 5 — multicollinearity present. Remove correlated predictors.\n")
} else {
  cat("VIF OK — no multicollinearity problem.\n")
}

# ── Assumption 4: Complete separation check ──────────────────────────────────
cat("\n── Assumption 4: Complete separation check ──\n")
# If any predictor perfectly predicts the outcome, glm() will warn about
# 'fitted probabilities numerically 0 or 1'.
# Also check for very large coefficients (> 10 in log-odds scale).
coef_check <- coef(logit_model)[-1]   # exclude intercept
if (any(abs(coef_check) > 10, na.rm=TRUE)) {
  cat("WARNING: Very large coefficients detected — possible complete separation.\n")
  cat("Variables with large coefficients:", names(which(abs(coef_check)>10)), "\n")
  cat("ACTION: Check frequency table for that predictor by outcome category.\n")
} else {
  cat("No evidence of complete separation (all log-odds coefficients < 10).\n")
}

# ── Assumption 6: Hosmer-Lemeshow goodness-of-fit test ───────────────────────
cat("\n── Assumption 6: Hosmer-Lemeshow goodness-of-fit test ──\n")
cat("Null hypothesis: model fits the data adequately (p > 0.05 = good fit)\n")
hl_test <- hoslem.test(
  x = logistic_df[[binary_outcome]],
  y = fitted(logit_model),
  g = 10   # 10 groups (default)
)
print(hl_test)
if (hl_test$p.value < 0.05) {
  cat("RESULT: p <0.05 — model may not fit well. Consider revising predictors.\n")
} else {
  cat("RESULT: p =", round(hl_test$p.value,3), "— model fits the data adequately.\n")
}

# ── Model fit statistics ─────────────────────────────────────────────────────
cat("\n── Model fit statistics ──\n")
cat("AIC:", round(AIC(logit_model),2), "\n")
cat("Null deviance:",     round(logit_model$null.deviance,2), "\n")
cat("Residual deviance:", round(logit_model$deviance,2), "\n")
mcfadden <- 1 - (logit_model$deviance / logit_model$null.deviance)
cat("McFadden pseudo-R²:", round(mcfadden,3), "\n")
cat("(0.2-0.4 = good fit for logistic models)\n")

# ── Odds Ratios (exponentiated coefficients) ──────────────────────────────────
cat("\n── Odds Ratios with 95% Confidence Intervals ──\n")
results_logit <- tidy(logit_model, conf.int=TRUE, exponentiate=TRUE) %>%
  mutate(across(where(is.numeric), ~round(.,4)))
print(results_logit)

# ── Classification performance ────────────────────────────────────────────────
cat("\n── Classification performance (threshold = 0.5) ──\n")
predicted_class <- as.integer(fitted(logit_model) >= 0.5)
actual_class    <- logistic_df[[binary_outcome]]
conf_matrix     <- table(Predicted=predicted_class, Actual=actual_class)
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", round(accuracy*100,1), "% \n")

# ── Save results ─────────────────────────────────────────────────────────────
write_csv(results_logit,
          paste0("output/tables/04_logit_OR_", ANALYSIS_LABEL, ".csv"))
cat("Script #7 complete.\n")



