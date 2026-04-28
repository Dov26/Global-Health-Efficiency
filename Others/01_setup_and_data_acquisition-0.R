# ==============================================================================
# Project: Global Health Efficiency - Quantile Trajectories & ML
# Script: 01_setup_and_data_acquisition
# Objective: Robust API Data Extraction & Reproducibility Setup
# Note: Replaces deprecated 'whoapi' with direct OData GHO API calls.
# ==============================================================================

# 1. GLOBAL SETTINGS & REPRODUCIBILITY -----------------------------------------
set.seed(2026) 
options(scipen = 999)

# Define Project Path (Adjust if necessary)
base_path <- "C:/Documents/Global-Health-Efficiency"
raw_data_path <- file.path(base_path, "data/raw")

# Create directory if it does not exist
if (!dir.exists(raw_data_path)) dir.create(raw_data_path, recursive = TRUE)

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
# Selection of core socio-economic indicators
wb_indicators <- c(
  "gdp_pc"     = "NY.GDP.PCAP.PP.KD", # GDP per capita, PPP (constant 2017)
  "edu_exp"    = "SE.XPD.TOTL.GD.ZS", # Govt expenditure on education (% of GDP)
  "water_acc"  = "SH.H2O.BASW.ZS",    # Access to basic drinking water (%)
  "urban_pop"  = "SP.URB.TOTL.IN.ZS"  # Urban population (% of total)
)

message(">>> Extracting World Bank (WDI) Data...")
df_wdi <- WDI(
  indicator = wb_indicators,
  start     = 2000,
  end       = 2025,
  extra     = TRUE # Captures region, income level, and ISO3 codes
)

write_csv(df_wdi, file.path(raw_data_path, "wb_wdi_raw.csv"))

# 3. WHO (GHO) DATA EXTRACTION - DIRECT ODATA METHOD ---------------------------
# Using the official WHO GHO OData API for maximum stability.

fetch_gho_data <- function(indicator_code) {
  # Endpoint for WHO Global Health Observatory
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

# Targeted WHO Indicators:
# GHED_CHE_PC_PPP_SHA2011: Current Health Expenditure per Capita (PPP)
# WHOSIS_000001: Life Expectancy at Birth (Total)
# HRH_26: Medical doctors (per 10,000 population)

message(">>> Extracting WHO (GHO) Data via OData API...")
who_codes <- c("GHED_CHE_PC_PPP_SHA2011", "WHOSIS_000001", "HRH_26")

df_who_final <- map_df(who_codes, function(code) {
  message(paste("Processing WHO Code:", code))
  fetch_gho_data(code) %>% mutate(IndicatorCode = code)
})

write_csv(df_who_final, file.path(raw_data_path, "who_gho_raw.csv"))

# 4. MANUAL DATASET VERIFICATION -----------------------------------------------
# These sources often require portal-specific downloads or account access.
# Verify their presence in /data/raw before moving to Script 02.

required_manual_files <- c(
  "ihme_hale_results.csv",  # From IHME GBD Results Tool
  "wgi_governance.xlsx",    # From World Bank Worldwide Governance Indicators
  "itu_digital_dev.csv",    # From ITU ICT Indicators
  "unesco_science_edu.csv", # From UNESCO UIS Database
  "cpi_transparency.xlsx"   # From Transparency International
)

missing_files <- required_manual_files[!file.exists(file.path(raw_data_path, required_manual_files))]

if (length(missing_files) > 0) {
  warning("CRITICAL: The following files are missing in /data/raw/ and must be downloaded manually:\n", 
          paste("- ", missing_files, collapse = "\n"))
} else {
  message(">>> Success: All API and manual datasets are verified and present.")
}

# 5. LOGGING FOR REPRODUCIBILITY -----------------------------------------------
log_entry <- tibble(
  extraction_date = Sys.time(),
  r_version       = R.version.string,
  platform        = R.version$platform
)

write_csv(log_entry, file.path(raw_data_path, "extraction_metadata.csv"))
message(">>> Step 01: Data Acquisition Phase Complete.")