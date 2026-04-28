# ==============================================================================
# Project: Global Health Efficiency - Quantile Trajectories & ML
# Script: 07_Run_Pipeline
# Objective: Orchestration complète et reproductibilité du workflow
# ==============================================================================

# 1. INITIALIZATION ------------------------------------------------------------
t0 <- Sys.time()
message(">>> STARTING PIPELINE: ", t0)

# Chemins de base
base_path <- "C:/Documents/Global Health Efficiency"
script_path <- file.path(base_path, "scripts") # Ajuste si tes scripts sont ailleurs

# 2. ENVIRONMENT CHECK ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tictoc)

# 3. PIPELINE EXECUTION --------------------------------------------------------
# Liste des scripts à exécuter dans l'ordre logique
pipeline_scripts <- c(
  "01_setup_and_data_acquisition.R",
  "02_Bis_Feature_Engineering.R",
  "02_Clean_and_Merge",
  "03_Econometric_Analysis.R",
  "04_Robust_Analysis.R",
  "05_Evidence_and_Visualization_of_Results.R",
  "06_Interactive_Dashboard.R"
)

# Fonction pour exécuter un script avec gestion d'erreurs
run_step <- function(script_name) {
  full_path <- file.path(script_path, script_name)
  
  if (file.exists(full_path)) {
    message("\n", paste0(rep("-", 50), collapse = ""))
    message(">>> EXECUTING: ", script_name)
    tic(script_name)
    
    tryCatch({
      source(full_path, local = FALSE, echo = FALSE)
      message(">>> SUCCESS: ", script_name)
    }, error = function(e) {
      message(">>> ERROR in ", script_name, ": ", e$message)
      stop("Pipeline halted due to error in script: ", script_name)
    })
    
    toc()
  } else {
    warning(">>> SKIP: File not found - ", script_name)
  }
}

# 4. RUN ALL -------------------------------------------------------------------
walk(pipeline_scripts, run_step)

# 5. WRAP-UP -------------------------------------------------------------------
t1 <- Sys.time()
duration <- difftime(t1, t0, units = "mins")

message("\n", paste0(rep("=", 60), collapse = ""))
message(">>> PIPELINE COMPLETED SUCCESSFULLY")
message(">>> Total Duration: ", round(duration, 2), " minutes")
message(">>> Results available in /outputs/ and /outputs/figures/")
message(paste0(rep("=", 60), collapse = ""))