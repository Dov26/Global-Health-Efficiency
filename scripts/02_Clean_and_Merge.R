# ==============================================================================
# Project: Global Health Efficiency
# Script: 02_Clean_and_Merge
# Objective: Ultra-Robust Cleaning, Imputation, and Efficiency Modeling
# Path: C:/Documents/Global Health Efficiency/scripts
# ==============================================================================

# 1. SETUP & PATH LOGIC --------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, countrycode, missForest, zoo, Benchmarking, frontier)

base_path <- "C:/Documents/Global Health Efficiency"
raw_path  <- file.path(base_path, "data/raw")
proc_path <- file.path(base_path, "data/processed")

if (!dir.exists(proc_path)) dir.create(proc_path, recursive = TRUE)

# 2. DATA LOADING & ADAPTIVE CLEANING ------------------------------------------
message(">>> Loading and Harmonizing Data...")

df_wdi <- read_csv(file.path(raw_path, "wb_wdi_raw.csv"))
df_who <- read_csv(file.path(raw_path, "who_gho_raw.csv"))

# ROBUST WHO CLEANING: 
# Instead of strict filtering, we pick the most 'representative' row for each country-year-indicator
df_who_clean <- df_who %>%
  rename(iso3 = SpatialDim, year = TimeDim) %>%
  # Handle cases where NumericValue might be missing but 'Value' exists
  mutate(NumericValue = coalesce(NumericValue, as.numeric(Value))) %>%
  filter(!is.na(NumericValue)) %>%
  # Sort so that 'Both Sexes' (BTSX) or 'Total' comes first if available
  mutate(is_total = ifelse(str_detect(tolower(coalesce(Dim1, "")), "btsx|total|all"), 1, 0)) %>%
  arrange(iso3, year, IndicatorCode, desc(is_total)) %>%
  # Keep only the best record for each indicator per year
  group_by(iso3, year, IndicatorCode) %>%
  slice(1) %>% 
  ungroup() %>%
  select(iso3, year, IndicatorCode, NumericValue) %>%
  pivot_wider(names_from = IndicatorCode, values_from = NumericValue)

# DYNAMIC RENAMING
mapping <- c(
  "health_exp_pc"     = "GHED_CHE_PC_PPP_SHA2011",
  "life_expectancy"   = "WHOSIS_000001",
  "physicians_density" = "HRH_26"
)

# Rename only what exists
existing_cols <- colnames(df_who_clean)
rename_list <- mapping[mapping %in% existing_cols]
df_who_clean <- df_who_clean %>% rename(any_of(rename_list))

# Clean WDI
df_wdi_clean <- df_wdi %>%
  mutate(iso3 = countrycode(iso3c, origin = "iso3c", destination = "iso3c")) %>%
  filter(!is.na(iso3)) %>%
  select(iso3, year, gdp_pc, edu_exp, water_acc, urban_pop)

# Merge
df_merged <- full_join(df_wdi_clean, df_who_clean, by = c("iso3", "year")) %>%
  filter(year >= 2000 & year <= 2025)

# 3. MISSING DATA HANDLING (Interpolation + MissForest) ------------------------
message(">>> Handling Missing Data...")

# Time-series interpolation
df_interp <- df_merged %>%
  group_by(iso3) %>%
  arrange(year) %>%
  mutate(across(where(is.numeric), ~na.approx(., na.rm = FALSE))) %>%
  ungroup()

# MissForest for remaining gaps
df_to_imp <- df_interp %>% select(-iso3, -year)
set.seed(2026)
# Note: missForest requires data.frame
imp_res <- missForest(as.data.frame(df_to_imp))
df_final <- as_tibble(imp_res$ximp) %>% 
  mutate(iso3 = df_interp$iso3, year = df_interp$year)

# 4. EFFICIENCY (DEA & SFA) ----------------------------------------------------
message(">>> Computing Efficiency Scores...")

compute_eff <- function(d) {
  # 1. Identify which expected columns actually exist
  potential_inputs <- c("gdp_pc", "health_exp_pc", "edu_exp")
  available_inputs <- potential_inputs[potential_inputs %in% colnames(d)]
  
  # 2. Check if we have the minimum requirements (Life Expectancy + at least 1 input)
  if (!"life_expectancy" %in% colnames(d) || length(available_inputs) == 0) {
    return(d %>% mutate(dea_score = NA_real_, sfa_score = NA_real_))
  }
  
  # 3. Clean and prepare data (Handle zeros/NAs)
  d_clean <- d %>% 
    mutate(across(all_of(c("life_expectancy", available_inputs)), 
                  ~ifelse(. <= 0 | is.na(.), 0.01, .)))
  
  x <- as.matrix(d_clean %>% select(all_of(available_inputs)))
  y <- as.matrix(d_clean %>% select(life_expectancy))
  
  # 4. DEA Calculation
  dea_res <- dea(x, y, RTS = "vrs", ORIENTATION = "in")
  
  # 5. SFA Calculation (with dynamic formula based on available inputs)
  sfa_formula <- as.formula(paste("log(life_expectancy) ~", 
                                  paste(paste0("log(", available_inputs, ")"), collapse = " + ")))
  
  # Try-Catch for SFA to prevent crash on non-convergence
  sfa_score <- tryCatch({
    sfa_mod <- sfa(sfa_formula, data = d_clean)
    as.vector(efficiencies(sfa_mod))
  }, error = function(e) NA_real_)
  
  d %>% mutate(
    dea_score = as.vector(dea_res$eff),
    sfa_score = if(length(sfa_score) == nrow(d)) sfa_score else NA_real_
  )
}

# Apply the function per year
df_master <- df_final %>% 
  group_by(year) %>% 
  group_modify(~compute_eff(.x)) %>% 
  ungroup()

# 5. EXPORT --------------------------------------------------------------------
# Final check: did we get health_exp_pc? If not, we should know.
if(!"health_exp_pc" %in% colnames(df_master)) {
  warning("NOTE: 'health_exp_pc' was not found in the source data. Scores were calculated using available inputs.")
}

write_csv(df_master, file.path(proc_path, "master_database.csv"))

message("---------------------------------------------------------------")
message(">>> SUCCESS: master_database.csv created.")
message(">>> Rows: ", nrow(df_master), " | Columns: ", ncol(df_master))
message("---------------------------------------------------------------")