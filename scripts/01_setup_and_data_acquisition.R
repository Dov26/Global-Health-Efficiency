# ==============================================================================
# Project: Global Health Efficiency - Quantile Trajectories & ML
# Script: 01_setup_and_data_acquisition
# Objective: Robust API Data Extraction & Reproducibility Setup
# Path: C:/Documents/Global Health Efficiency/scripts
# ==============================================================================

# 1. GLOBAL SETTINGS & REPRODUCIBILITY -----------------------------------------
set.seed(2026) 
options(scipen = 999)

# DEFINE ABSOLUTE PATHS (Ensures consistency across all scripts)
base_path      <- "C:/Documents/Global Health Efficiency"
raw_data_path  <- file.path(base_path, "data/raw")

# Create directory if it does not exist
if (!dir.exists(raw_data_path)) {
  dir.create(raw_data_path, recursive = TRUE)
  message(">>> Created project directory at: ", raw_data_path)
}

# Load essential libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, # Data manipulation
  WDI,       # World Bank API
  httr,      # HTTP requests for WHO API
  jsonlite,  # JSON parsing
  readxl     # Reading Excel files
)

# 2. WORLD BANK (WDI) DATA EXTRACTION ------------------------------------------
wb_indicators <- c(
  "gdp_pc"    = "NY.GDP.PCAP.PP.KD", # GDP per capita, PPP (constant 2017)
  "edu_exp"   = "SE.XPD.TOTL.GD.ZS", # Govt expenditure on education (% of GDP)
  "water_acc" = "SH.H2O.BASW.ZS",    # Access to basic drinking water (%)
  "urban_pop" = "SP.URB.TOTL.IN.ZS"  # Urban population (% of total)
)

message(">>> Extracting World Bank (WDI) Data...")
df_wdi <- WDI(
  indicator = wb_indicators,
  start     = 2000,
  end       = 2025,
  extra     = TRUE 
)

# SAVE WDI DATA
write_csv(df_wdi, file.path(raw_data_path, "wb_wdi_raw.csv"))

# 3. WHO (GHO) DATA EXTRACTION - DIRECT ODATA METHOD ---------------------------
fetch_gho_data <- function(indicator_code) {
  url <- paste0("https://ghoapi.azureedge.net/api/", indicator_code)
  response <- GET(url)
  
  if (status_code(response) == 200) {
    content_raw <- content(response, "text", encoding = "UTF-8")
    json_data   <- fromJSON(content_raw)
    return(as_tibble(json_data$value))
  } else {
    stop(paste("Failed to fetch WHO indicator:", indicator_code))
  }
}

message(">>> Extracting WHO (GHO) Data via OData API...")
who_codes <- c("GHED_CHE_PC_PPP_SHA2011", "WHOSIS_000001", "HRH_26")

df_who_final <- map_df(who_codes, function(code) {
  message(paste("Processing WHO Code:", code))
  fetch_gho_data(code) %>% mutate(IndicatorCode = code)
})

# SAVE WHO DATA
write_csv(df_who_final, file.path(raw_data_path, "who_gho_raw.csv"))

# 4. FINAL SUCCESS CHECK & VALIDATION -----------------------------------------
# This ensures Script 02 won't fail due to missing files
required_outputs <- c("wb_wdi_raw.csv", "who_gho_raw.csv")
missing_outputs <- required_outputs[!file.exists(file.path(raw_data_path, required_outputs))]

if (length(missing_outputs) == 0) {
  message("---------------------------------------------------------------")
  message(">>> SUCCESS: Script 01 completed and files are verified.")
  message(">>> Target Directory: ", raw_data_path)
  message(">>> You can now safely run Script 02_Clean_and_Merge.R")
  message("---------------------------------------------------------------")
} else {
  stop(paste("CRITICAL ERROR: The following files failed to save:", 
             paste(missing_outputs, collapse = ", ")))
}

# 5. LOGGING FOR AUDIT TRAIL --------------------------------------------------
log_entry <- tibble(
  extraction_date = Sys.time(),
  r_version       = R.version.string,
  target_path     = raw_data_path
)
write_csv(log_entry, file.path(raw_data_path, "extraction_metadata.csv"))