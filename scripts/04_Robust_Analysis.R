# ==============================================================================
# Project: Global Health Efficiency - Quantile Trajectories & ML
# Script: 04_Robust_Analysis
# Objective: Robustness Checks (Time Windows, Outliers, OOS Validation)
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
  quantreg, ranger, caret
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

# ------------------------------------------------------------------------------
# CLEAN DESIGN MATRIX (FIX MULTICOLLINEARITY)
clean_design_matrix <- function(df, target_var) {
  
  df_num <- df %>% select(where(is.numeric))
  
  y <- df_num[[target_var]]
  X <- df_num %>% select(-all_of(target_var))
  
  # Remove near-zero variance
  nzv <- nearZeroVar(X)
  if (length(nzv) > 0) {
    X <- X[, -nzv, drop = FALSE]
  }
  
  # Remove high correlation
  if (ncol(X) > 1) {
    corr <- cor(X, use = "pairwise.complete.obs")
    high_corr <- findCorrelation(corr, cutoff = 0.95)
    if (length(high_corr) > 0) {
      X <- X[, -high_corr, drop = FALSE]
    }
  }
  
  df_clean <- bind_cols(y = y, X)
  names(df_clean)[1] <- target_var
  
  return(df_clean)
}

# 3. ROBUSTNESS SCENARIOS -----------------------------------------------------
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

threshold <- quantile(country_means$target_mean, c(0.01, 0.99), na.rm = TRUE)

outliers <- country_means %>%
  filter(target_mean < threshold[1] | target_mean > threshold[2]) %>%
  pull(iso3)

scenarios$outliers_removed <- df %>% filter(!iso3 %in% outliers)

# 5. METRICS -------------------------------------------------------------------
compute_metrics <- function(truth, pred) {
  mae <- mean(abs(truth - pred), na.rm = TRUE)
  r2  <- 1 - sum((truth - pred)^2) / sum((truth - mean(truth))^2)
  
  tibble(MAE = mae, R2 = r2)
}

# 6. TRAIN/TEST ROBUSTNESS LOOP -----------------------------------------------
message(">>> Running robustness models (OOS evaluation)...")

results <- list()
taus <- c(0.1, 0.5, 0.9)

for (sc_name in names(scenarios)) {
  
  message(">>> Scenario: ", sc_name)
  
  df_raw <- scenarios[[sc_name]] %>%
    select(-iso3, -year, -any_of(c("region", "income", "country"))) %>%
    filter(!is.na(.data[[target_var]]))
  
  df_s <- clean_design_matrix(df_raw, target_var)
  
  # ---------------- TRAIN / TEST SPLIT ----------------
  set.seed(2026)
  idx <- createDataPartition(df_s[[target_var]], p = 0.8, list = FALSE)
  
  train <- df_s[idx, ]
  test  <- df_s[-idx, ]
  
  y_test <- test[[target_var]]
  
  # ===================== QRF ==========================
  qrf_model <- ranger(
    formula = as.formula(paste(target_var, "~ .")),
    data = train,
    num.trees = 300,
    quantreg = TRUE
  )
  
  for (tau in taus) {
    
    qrf_pred <- predict(
      qrf_model,
      data = test,
      type = "quantiles",
      quantiles = tau
    )$predictions
    
    results[[length(results) + 1]] <- compute_metrics(y_test, qrf_pred) %>%
      mutate(model = "QRF", tau = tau, scenario = sc_name)
  }
  
  # ================= LINEAR QR ========================
  for (tau in taus) {
    
    rq_metrics <- tryCatch({
      
      rq_model <- rq(
        formula = as.formula(paste(target_var, "~ .")),
        data = train,
        tau = tau,
        method = "fn"
      )
      
      rq_pred <- predict(rq_model, newdata = test)
      
      compute_metrics(y_test, rq_pred) %>%
        mutate(model = "LinearQR", tau = tau, scenario = sc_name)
      
    }, error = function(e) {
      
      message(">>> QR failed | scenario=", sc_name, " tau=", tau)
      
      tibble(
        MAE = NA_real_,
        R2 = NA_real_,
        model = "LinearQR",
        tau = tau,
        scenario = sc_name
      )
    })
    
    results[[length(results) + 1]] <- rq_metrics
  }
}

# 7. EXPORT RESULTS ------------------------------------------------------------
robust_results <- bind_rows(results)

write_csv(robust_results, file.path(out_path, "robustness_results.csv"))

summary_table <- robust_results %>%
  group_by(model, tau, scenario) %>%
  summarise(
    MAE_mean = mean(MAE, na.rm = TRUE),
    R2_mean  = mean(R2, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(summary_table, file.path(out_path, "robustness_summary.csv"))

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

# 8. FINAL MESSAGE -------------------------------------------------------------
message("---------------------------------------------------------------")
message(">>> SUCCESS: Robustness Analysis Complete (OOS validated)")
message(">>> Outputs saved in /outputs/")
message(">>> Ready for publication figures + Appendix tables")
message("---------------------------------------------------------------")