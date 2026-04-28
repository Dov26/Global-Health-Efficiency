# ==============================================================================
# Project: Global Health Efficiency - Quantile Trajectories & ML
# Script: 02_Bis_Feature_Engineering
# Objective: Creation of derived variables, Lags (Endogeneity) & Normalization
# ==============================================================================

# 1. SETUP & LOADING -----------------------------------------------------------
set.seed(2026)
base_path <- "C:/Documents/Global-Health-Efficiency"
# Chargement brut pour inspection
df_raw_master <- read_csv(file.path(base_path, "master_database.csv"))

pacman::p_load(tidyverse, recipes, zoo, rlang, countrycode)

# 2. SAUVETAGE DE L'IDENTIFIANT PAYS (iso3) ------------------------------------
message(">>> Phase de sauvetage de l'identifiant pays...")

# On cherche n'importe quelle colonne qui pourrait être l'ID pays
potential_names <- c("iso3", "iso3c", "SpatialLocationCode", "country", "code")
found_col <- names(df_raw_master)[tolower(names(df_raw_master)) %in% tolower(potential_names)][1]

if (is.na(found_col)) {
  # Si rien n'est trouvé par nom, on prend la 1ère colonne (standard d'exportation)
  found_col <- names(df_raw_master)[1]
  message(">>> Alerte : Aucun nom standard trouvé. Utilisation de la 1ère colonne : ", found_col)
}

df_master <- df_raw_master %>% 
  rename(iso3 = !!sym(found_col)) %>%
  # On s'assure que c'est bien de l'ISO3 (convertit "Afghanistan" -> "AFG")
  mutate(iso3 = countrycode(iso3, origin = "country.name", destination = "iso3c", 
                            warn = FALSE, nomatch = iso3)) %>%
  filter(!is.na(iso3))

# 3. HARMONISATION DES PRÉDICTEURS ---------------------------------------------
# On utilise des regex pour attraper les colonnes même si elles ont des suffixes
df_clean <- df_master %>%
  rename_with(~ "year", matches("year|period|time", ignore.case = TRUE)) %>%
  rename_with(~ "gdp_pc", matches("gdp", ignore.case = TRUE)) %>%
  rename_with(~ "urban_pop", matches("urban", ignore.case = TRUE)) %>%
  rename_with(~ "water_acc", matches("water", ignore.case = TRUE)) %>%
  rename_with(~ "health_exp", matches("health|ghed", ignore.case = TRUE)) %>%
  rename_with(~ "life_exp", matches("life|whosis", ignore.case = TRUE))

# 4. FEATURE ENGINEERING & LAGS (ENDOGÉNÉITÉ) ----------------------------------
message(">>> Création des variables dérivées et calcul des retards (Lags)...")

df_features <- df_clean %>%
  group_by(iso3) %>%
  arrange(year) %>%
  mutate(
    # Variable d'interaction : Impact de l'urbanisation sur l'accès aux services
    urban_water_idx = (urban_pop/100) * (water_acc/100),
    
    # Lags de 1 an pour les variables économiques (Évite la corrélation instantanée)
    across(any_of(c("gdp_pc", "health_exp", "dea_score")), 
           list(lag1 = ~lag(., 1)), .names = "{.col}_lag1")
  ) %>%
  ungroup() %>%
  # On élimine la première année de chaque pays (car lag1 sera NA)
  filter(!is.na(gdp_pc_lag1))

# 5. NORMALISATION PRÊTE POUR LE ML --------------------------------------------
# On définit la cible : dea_score si présent, sinon life_exp
target_var <- if("dea_score" %in% names(df_features)) "dea_score" else "life_exp"

rec_obj <- recipe(as.formula(paste(target_var, "~ .")), data = df_features) %>%
  # On protège les identifiants pour qu'ils ne soient pas normalisés
  update_role(iso3, year, any_of(c("region", "income")), new_role = "id") %>%
  step_normalize(all_numeric_predictors()) %>%
  step_YeoJohnson(all_numeric_predictors())

df_final_ml <- prep(rec_obj) %>% bake(new_data = NULL)

# 6. EXPORTATION ---------------------------------------------------------------
write_csv(df_final_ml, file.path(base_path, "ml_feature_matrix.csv"))
message(">>> Étape terminée. Fichier généré : ml_feature_matrix.csv")