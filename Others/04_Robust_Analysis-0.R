# ==============================================================================
# Project: Global Health Efficiency - Quantile Trajectories & ML
# Script: 04_Robust_Analysis
# Objective: Robustness Checks (Time Windows, Outliers, Model Comparison)
# ==============================================================================

# 1. SETUP ---------------------------------------------------------------------
set.seed(2026)

base_path <- "C:/Documents/Global Health Efficiency"
proc_path <- file.path(base_path, "data/processed")
out_path  <- file.path(base_path, "outputs")

dir.create(out_path, showWarnings = FALSE)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, data.table,
  quantreg, ranger
)

# 2. LOAD DATA -----------------------------------------------------------------
message(">>> Loading ML feature matrix...")

df <- read_csv(file.path(proc_path, "ml_feature_matrix.csv")) %>%
  arrange(iso3, year)

# Identify target
target_var <- if ("dea_score" %in% names(df)) {
  "dea_score"
} else if ("sfa_score" %in% names(df)) {
  "sfa_score"
} else {
  "life_exp"
}

# Remove ID columns for modeling
df_model <- df %>%
  select(-iso3, -year, -any_of(c("region", "income", "country"))) %>%
  filter(!is.na(.data[[target_var]]))

# 3. DEFINE ROBUSTNESS SCENARIOS -----------------------------------------------
message(">>> Defining robustness scenarios...")

scenarios <- list(
  full_sample = df,
  post_2000   = df %>% filter(year >= 2000),
  pre_2015    = df %>% filter(year <= 2015)
)

# 4. OUTLIER DETECTION ---------------------------------------------------------
message(">>> Detecting outlier countries...")

country_means <- df %>%
  group_by(iso3) %>%
  summarise(target_mean = mean(.data[[target_var]], na.rm = TRUE))

threshold <- quantile(country_means$target_mean, probs = c(0.01, 0.99), na.rm = TRUE)

outliers <- country_means %>%
  filter(target_mean < threshold[1] | target_mean > threshold[2]) %>%
  pull(iso3)

df_no_outliers <- df %>% filter(!iso3 %in% outliers)

scenarios$outliers_removed <- df_no_outliers

# 5. MODEL FUNCTIONS -----------------------------------------------------------
compute_metrics <- function(truth, pred) {
  mae <- mean(abs(truth - pred), na.rm = TRUE)
  r2  <- 1 - sum((truth - pred)^2) / sum((truth - mean(truth))^2)
  
  tibble(MAE = mae, R2 = r2)
}

# 6. LOOP OVER SCENARIOS -------------------------------------------------------
message(">>> Running robustness models...")

results <- list()

taus <- c(0.1, 0.5, 0.9)

for (sc_name in names(scenarios)) {
  
  df_s <- scenarios[[sc_name]] %>%
    select(-iso3, -year, -any_of(c("region", "income", "country"))) %>%
    filter(!is.na(.data[[target_var]]))
  
  X <- df_s %>% select(-all_of(target_var))
  y <- df_s[[target_var]]
  
  # ---------------- QRF (via ranger) ----------------
  qrf_model <- ranger(
    formula = as.formula(paste(target_var, "~ .")),
    data = df_s,
    num.trees = 300,
    quantreg = TRUE
  )
  
  for (tau in taus) {
    
    qrf_pred <- predict(qrf_model, data = df_s, type = "quantiles",
                        quantiles = tau)$predictions
    
    qrf_metrics <- compute_metrics(y, qrf_pred) %>%
      mutate(
        model = "QRF",
        tau = tau,
        scenario = sc_name
      )
    
    results[[length(results) + 1]] <- qrf_metrics
  }
  
  # ---------------- Linear Quantile Regression ----------------
  for (tau in taus) {
    
    rq_model <- rq(
      formula = as.formula(paste(target_var, "~ .")),
      data = df_s,
      tau = tau
    )
    
    rq_pred <- predict(rq_model, newdata = df_s)
    
    rq_metrics <- compute_metrics(y, rq_pred) %>%
      mutate(
        model = "LinearQR",
        tau = tau,
        scenario = sc_name
      )
    
    results[[length(results) + 1]] <- rq_metrics
  }
}

# 7. AGGREGATE RESULTS ---------------------------------------------------------
robust_results <- bind_rows(results)

write_csv(robust_results, file.path(out_path, "robustness_results.csv"))

# 8. SCENARIO DOCUMENTATION ----------------------------------------------------
scenario_log <- tibble(
  scenario = names(scenarios),
  description = c(
    "Full sample baseline",
    "Post-2000 subsample",
    "Pre-2015 subsample",
    "Outliers removed (1%-99%)"
  )
)

write_csv(scenario_log, file.path(out_path, "robustness_scenarios_log.csv"))

# 9. SUMMARY TABLE -------------------------------------------------------------
summary_table <- robust_results %>%
  group_by(model, tau, scenario) %>%
  summarise(
    MAE_mean = mean(MAE, na.rm = TRUE),
    R2_mean  = mean(R2, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(summary_table, file.path(out_path, "robustness_summary.csv"))

message("---------------------------------------------------------------")
message(">>> SUCCESS: Robustness Analysis Complete")
message(">>> Outputs saved in /outputs/")
message(">>> Ready for appendix & reviewer validation")
message("---------------------------------------------------------------")