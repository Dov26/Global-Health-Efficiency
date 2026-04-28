# ==============================================================================
# Project: Global Health Efficiency - Quantile Trajectories & ML
# Script: 02_Bis_Feature_Engineering
# Objective: Derived variables, Lags (Endogeneity) & ML Normalization
# Path: C:/Documents/Global Health Efficiency/scripts
# ==============================================================================

# 1. SETUP & LOADING -----------------------------------------------------------
set.seed(2026)

base_path <- "C:/Documents/Global Health Efficiency"
proc_path <- file.path(base_path, "data/processed")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, recipes, zoo, rlang, countrycode)

master_file <- file.path(proc_path, "master_database.csv")

if (!file.exists(master_file)) {
  stop("CRITICAL ERROR: master_database.csv not found. Please run Script 02 first.")
}

df_raw_master <- read_csv(master_file)

# 2. IDENTIFIER RESCUE (ISO3) --------------------------------------------------
message(">>> Phase: Rescuing country identifiers...")

potential_names <- c("iso3", "iso3c", "SpatialLocationCode", "country", "code")

found_col <- names(df_raw_master)[tolower(names(df_raw_master)) %in% tolower(potential_names)][1]

if (is.na(found_col)) {
  found_col <- names(df_raw_master)[1]
  message(">>> Warning: No standard ID name found. Using first column: ", found_col)
}

df_master <- df_raw_master %>% 
  rename(iso3 = !!sym(found_col)) %>%
  mutate(
    iso3 = countrycode(
      iso3,
      origin = "country.name",
      destination = "iso3c",
      warn = FALSE,
      nomatch = iso3
    )
  ) %>%
  filter(!is.na(iso3))

# 3. PREDICTOR HARMONIZATION ---------------------------------------------------
message(">>> Phase: Harmonizing variable names...")

safe_rename <- function(df, pattern, new_name) {
  cols <- names(df)[str_detect(names(df), regex(pattern, ignore_case = TRUE))]
  
  if (length(cols) > 0) {
    df <- df %>% rename(!!new_name := all_of(cols[1]))
  }
  
  return(df)
}

df_clean <- df_master %>%
  safe_rename("year|period|time", "year") %>%
  safe_rename("gdp", "gdp_pc") %>%
  safe_rename("urban", "urban_pop") %>%
  safe_rename("water", "water_acc") %>%
  safe_rename("health|ghed|che|expenditure", "health_exp") %>%
  safe_rename("life|whosis", "life_exp")

# ------------------------------------------------------------------------------
# 3B. ENSURE REQUIRED VARIABLES EXIST ------------------------------------------
message(">>> Phase: Checking required variables...")

required_vars <- c("year", "gdp_pc", "urban_pop", "water_acc", "health_exp")

missing_vars <- setdiff(required_vars, names(df_clean))

if (length(missing_vars) > 0) {
  message(">>> Warning: Missing variables detected: ", paste(missing_vars, collapse = ", "))
  
  for (v in missing_vars) {
    df_clean[[v]] <- NA_real_
  }
}

# 4. FEATURE ENGINEERING & LAGS ------------------------------------------------
message(">>> Phase: Creating derived variables and Lags...")

df_features <- df_clean %>%
  group_by(iso3) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    # Interaction variable
    urban_water_idx = if (all(c("urban_pop", "water_acc") %in% names(.))) {
      (urban_pop / 100) * (water_acc / 100)
    } else NA_real_,
    
    # Growth rate (safe)
    health_exp_growth = if ("health_exp" %in% names(.)) {
      (health_exp - lag(health_exp)) / lag(health_exp)
    } else NA_real_,
    
    # Lags
    across(
      any_of(c("gdp_pc", "health_exp", "dea_score", "sfa_score")),
      list(lag1 = ~lag(., 1)),
      .names = "{.col}_lag1"
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(gdp_pc_lag1))

# 5. NORMALIZATION PREP FOR MACHINE LEARNING -----------------------------------
message(">>> Phase: ML Pre-processing (Normalization & Transformation)...")

target_var <- case_when(
  "dea_score" %in% names(df_features) ~ "dea_score",
  "sfa_score" %in% names(df_features) ~ "sfa_score",
  TRUE ~ "life_exp"
)

rec_obj <- recipe(as.formula(paste(target_var, "~ .")), data = df_features) %>%
  update_role(iso3, year, any_of(c("region", "income", "country")), new_role = "id") %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())

df_final_ml <- prep(rec_obj) %>% bake(new_data = NULL)

# 6. EXPORT --------------------------------------------------------------------
write_csv(df_final_ml, file.path(proc_path, "ml_feature_matrix.csv"))

message("---------------------------------------------------------------")
message(">>> SUCCESS: Feature Engineering Complete.")
message(">>> Generated: ml_feature_matrix.csv in /data/processed/")
message(">>> Ready for Script 03 (Clustering & Trajectories).")
message("---------------------------------------------------------------")