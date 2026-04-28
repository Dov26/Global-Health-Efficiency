# ==============================================================================
# Project: Global Health Efficiency - Quantile Trajectories & ML
# Script: 03_Econometric_Analysis
# Objective: ML Modeling + Econometric Interpretation (SHAP, Performance)
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
  ranger, xgboost,
  rsample, yardstick,
  fastshap
)

# 2. LOAD DATA -----------------------------------------------------------------
message(">>> Loading ML feature matrix...")

df <- read_csv(file.path(proc_path, "ml_feature_matrix.csv"))

# Ensure ordering (critical for time series)
df <- df %>% arrange(iso3, year)

# Identify target
target_var <- if ("dea_score" %in% names(df)) {
  "dea_score"
} else if ("sfa_score" %in% names(df)) {
  "sfa_score"
} else {
  "life_exp"
}

# Remove ID columns
df_model <- df %>%
  select(-iso3, -year, -any_of(c("region", "income", "country")))

# Remove rows with NA target
df_model <- df_model %>% filter(!is.na(.data[[target_var]]))

# 3. TIME SERIES CROSS VALIDATION ----------------------------------------------
message(">>> Creating TimeSeries splits...")

ts_splits <- rolling_origin(
  df_model,
  initial = floor(0.7 * nrow(df_model)),
  assess  = floor(0.2 * nrow(df_model)),
  cumulative = TRUE
)

# 4. MODEL FUNCTIONS -----------------------------------------------------------

compute_metrics <- function(truth, pred) {
  mae <- mean(abs(truth - pred), na.rm = TRUE)
  r2  <- 1 - sum((truth - pred)^2) / sum((truth - mean(truth))^2)
  
  tibble(MAE = mae, R2 = r2)
}

# 5. TRAINING LOOP -------------------------------------------------------------
message(">>> Training models (RF & XGBoost)...")

results <- list()
residuals_all <- list()

for (i in seq_along(ts_splits$splits)) {
  
  split <- ts_splits$splits[[i]]
  
  train_data <- analysis(split)
  test_data  <- assessment(split)
  
  y_train <- train_data[[target_var]]
  y_test  <- test_data[[target_var]]
  
  X_train <- train_data %>% select(-all_of(target_var))
  X_test  <- test_data %>% select(-all_of(target_var))
  
  # ---------------- RANDOM FOREST ----------------
  rf_model <- ranger(
    formula = as.formula(paste(target_var, "~ .")),
    data = train_data,
    num.trees = 300,
    importance = "impurity"
  )
  
  rf_pred <- predict(rf_model, data = test_data)$predictions
  
  rf_metrics <- compute_metrics(y_test, rf_pred) %>%
    mutate(model = "RandomForest", split = i)
  
  # Residual variance
  rf_resid_var <- var(y_test - rf_pred, na.rm = TRUE)
  
  # ---------------- XGBOOST ----------------
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  dtest  <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
  
  xgb_model <- xgboost(
    data = dtrain,
    nrounds = 200,
    objective = "reg:squarederror",
    verbose = 0
  )
  
  xgb_pred <- predict(xgb_model, dtest)
  
  xgb_metrics <- compute_metrics(y_test, xgb_pred) %>%
    mutate(model = "XGBoost", split = i)
  
  xgb_resid_var <- var(y_test - xgb_pred, na.rm = TRUE)
  
  # Store results
  results[[i]] <- bind_rows(rf_metrics, xgb_metrics)
  
  residuals_all[[i]] <- tibble(
    split = i,
    rf_residual_var = rf_resid_var,
    xgb_residual_var = xgb_resid_var
  )
}

# 6. AGGREGATE RESULTS ---------------------------------------------------------
results_df <- bind_rows(results)
residuals_df <- bind_rows(residuals_all)

write_csv(results_df, file.path(out_path, "model_performance.csv"))
write_csv(residuals_df, file.path(out_path, "residual_variance.csv"))

# 7. FINAL MODEL TRAINING (FULL DATA) ------------------------------------------
message(">>> Training final models on full dataset...")

X <- df_model %>% select(-all_of(target_var))
y <- df_model[[target_var]]

# RF final
rf_final <- ranger(
  formula = as.formula(paste(target_var, "~ .")),
  data = df_model,
  num.trees = 500,
  importance = "impurity"
)

rf_importance <- as.data.frame(rf_final$variable.importance) %>%
  rownames_to_column("variable") %>%
  rename(importance = 2)

# XGB final
dtrain_full <- xgb.DMatrix(data = as.matrix(X), label = y)

xgb_final <- xgboost(
  data = dtrain_full,
  nrounds = 300,
  objective = "reg:squarederror",
  verbose = 0
)

xgb_importance <- xgb.importance(model = xgb_final)

write_csv(rf_importance, file.path(out_path, "rf_importance.csv"))
write_csv(xgb_importance, file.path(out_path, "xgb_importance.csv"))

# 8. SHAP VALUES (INTERPRETABILITY) --------------------------------------------
message(">>> Computing SHAP values (XGBoost)...")

shap_values <- fastshap::explain(
  object = xgb_final,
  X = as.data.frame(X),
  pred_wrapper = function(object, newdata) {
    predict(object, xgb.DMatrix(as.matrix(newdata)))
  },
  nsim = 50
)

shap_df <- as.data.frame(shap_values)

write_csv(shap_df, file.path(out_path, "shap_values.csv"))

# 9. GLOBAL SUMMARY ------------------------------------------------------------
summary_results <- results_df %>%
  group_by(model) %>%
  summarise(
    MAE_mean = mean(MAE, na.rm = TRUE),
    R2_mean  = mean(R2, na.rm = TRUE)
  )

write_csv(summary_results, file.path(out_path, "summary_performance.csv"))

message("---------------------------------------------------------------")
message(">>> SUCCESS: Econometric ML Analysis Complete")
message(">>> Outputs saved in /outputs/")
message(">>> Ready for interpretation & paper writing")
message("---------------------------------------------------------------")