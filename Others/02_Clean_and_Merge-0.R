# ==============================================================================
# Project: Global Health Efficiency - Quantile Trajectories & ML
# Script: 02_Clean_and_Merge
# Objective: ISO Harmonization, ML Imputation, Efficiency Computing (DEA/SFA)
# ==============================================================================

# 1. SETUP ---------------------------------------------------------------------
set.seed(2026)
pacman::p_load(tidyverse, countrycode, missForest, Benchmarking, zoo, rlang)

base_path <- "C:/Documents/Global-Health-Efficiency"
df_wdi_raw <- read_csv(file.path(base_path, "data/raw/wb_wdi_raw.csv"))
df_who_raw <- read_csv(file.path(base_path, "data/raw/who_gho_raw.csv"))

# Fonction de renommage préventive
smart_rename <- function(df, target, patterns) {
  cols <- names(df)
  m <- cols[str_detect(cols, paste0("(?i)", paste(patterns, collapse = "|")))][1]
  if (!is.na(m)) df <- rename(df, !!target := !!sym(m))
  return(df)
}

# 2. PRÉPARATION DES DONNÉES ---------------------------------------------------
df_who <- df_who_raw %>%
  smart_rename("c_code", c("Spatial", "Country", "Location")) %>%
  smart_rename("year", c("Year", "Period", "Time")) %>%
  mutate(iso3 = countrycode(c_code, "iso3c", "iso3c", warn=F), year = as.numeric(year)) %>%
  filter(!is.na(iso3), !is.na(year)) %>%
  pivot_wider(id_cols = c(iso3, year), names_from = IndicatorCode, values_from = NumericValue, values_fn = mean) %>%
  rename(health_exp_pc = any_of("GHED_CHE_PC_PPP_SHA2011"),
         life_exp = any_of("WHOSIS_000001"), physicians = any_of("HRH_26"))

df_wdi <- df_wdi_raw %>%
  smart_rename("iso3", c("iso3c", "iso3")) %>%
  smart_rename("year", c("year")) %>%
  rename(gdp_pc = any_of("NY.GDP.PCAP.PP.KD"), urban_pop = any_of("SP.URB.TOTL.IN.ZS"), water_acc = any_of("SH.H2O.BASW.ZS"))

df_master <- full_join(df_wdi, df_who, by = c("iso3", "year"))

# 3. IMPUTATION SÉCURISÉE (MÉTHODE RECONSTRUCTION) -----------------------------
message(">>> Imputation MissForest en cours...")

# On sauvegarde l'ordre et les noms originaux
original_names <- names(df_master %>% select(where(is.numeric)))
meta_info <- df_master %>% select(iso3, year)

# Nettoyage numérique simple avant MissForest
numeric_clean <- df_master %>% 
  select(where(is.numeric)) %>% 
  as.data.frame()

# L'algorithme MissForest
mf_output <- missForest(numeric_clean)
df_imputed <- as_tibble(mf_output$ximp)

# RÉASSEMBLAGE : On force la création d'un nouvel objet propre
# Cela évite les erreurs de select() car on définit les colonnes manuellement
df_final_ready <- tibble(
  iso3 = meta_info$iso3,
  year = meta_info$year
) %>% bind_cols(df_imputed)

# 4. CALCUL DEA ----------------------------------------------------------------
message(">>> Calcul de l'efficience (DEA)...")

req_cols <- c("health_exp_pc", "physicians", "life_exp")
for (c in req_cols) { if (!c %in% names(df_final_ready)) df_final_ready[[c]] <- NA_real_ }

compute_dea <- function(d) {
  # On vérifie que year est bien présent dans le chunk d
  if (!"year" %in% names(d)) return(NULL)
  if (nrow(d) < 5 || any(colSums(!is.na(d[, req_cols])) == 0)) return(d %>% mutate(dea_score = NA_real_))
  
  X <- as.matrix(d %>% select(health_exp_pc, physicians))
  Y <- as.matrix(d %>% select(life_exp))
  
  score <- try(as.vector(dea(X, Y, RTS="crs", ORIENTATION="in")), silent = TRUE)
  if(inherits(score, "try-error")) return(d %>% mutate(dea_score = NA_real_))
  return(d %>% mutate(dea_score = score))
}

df_master_final <- df_final_ready %>%
  group_split(year) %>%
  map_dfr(compute_dea)

# 5. EXPORTATION FINALE --------------------------------------------------------
# Vérification ultime de l'existence de iso3 avant l'export
if ("iso3" %in% names(df_master_final)) {
  df_master_final %>%
    select(iso3, year, everything()) %>%
    arrange(iso3, year) %>%
    write_csv(file.path(base_path, "master_database.csv"))
  message(">>> SUCCÈS : master_database.csv créé avec iso3 et year.")
} else {
  stop("ERREUR : La colonne iso3 a été perdue lors du calcul DEA.")
}