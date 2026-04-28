# ==============================================================================
# Project: Global Health Efficiency - Quantile Trajectories & ML
# Script: 05_Evidence_and_Visualization_of_Results
# Objective: Visualization, Structural Breaks & Evidence Synthesis
# ==============================================================================

# 1. SETUP ---------------------------------------------------------------------
set.seed(2026)

base_path <- "C:/Documents/Global Health Efficiency"
proc_path <- file.path(base_path, "data/processed")
out_path  <- file.path(base_path, "outputs")
fig_path  <- file.path(base_path, "outputs/figures")

dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)

# Gestion robuste des packages
if (!require("pacman")) install.packages("pacman")

# Installation explicite des dépendances rnaturalearth pour éviter les prompts interactifs
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata")
if (!require("rnaturalearthhires")) {
  install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev")
}

pacman::p_load(
  tidyverse, data.table,
  ggplot2, sf, rnaturalearth, rnaturalearthdata,
  cluster, factoextra,
  corrplot, strucchange,
  boot
)

# 2. LOAD DATA -----------------------------------------------------------------
# Vérification de l'existence du fichier avant chargement
if (!file.exists(file.path(proc_path, "ml_feature_matrix.csv"))) {
  stop("Le fichier ml_feature_matrix.csv est introuvable dans : ", proc_path)
}

df <- read_csv(file.path(proc_path, "ml_feature_matrix.csv")) %>%
  arrange(iso3, year)

target_var <- if ("dea_score" %in% names(df)) {
  "dea_score"
} else if ("sfa_score" %in% names(df)) {
  "sfa_score"
} else {
  "life_exp"
}

# ==============================================================================
# 3. GLOBAL EFFICIENCY MAP (FIXED SAFE VERSION)
# ==============================================================================
message(">>> Creating global map...")

world <- ne_countries(scale = "medium", returnclass = "sf")

df_map <- df %>%
  group_by(iso3) %>%
  summarise(efficiency = mean(.data[[target_var]], na.rm = TRUE))

map_data <- world %>%
  left_join(df_map, by = c("iso_a3" = "iso3"))

p_map <- ggplot(map_data) +
  geom_sf(aes(fill = efficiency)) +
  scale_fill_viridis_c(na.value = "grey80", option = "C") +
  labs(title = paste("Global Mean Efficiency:", target_var),
       fill = "Score") +
  theme_minimal()

ggsave(file.path(fig_path, "global_efficiency_map.png"), p_map, width = 10, height = 6)

# ==============================================================================
# 4. TEMPORAL TRAJECTORY
# ==============================================================================
message(">>> Creating trajectory plot...")

df_ts <- df %>%
  group_by(year) %>%
  summarise(mean_eff = mean(.data[[target_var]], na.rm = TRUE))

p_traj <- ggplot(df_ts, aes(year, mean_eff)) +
  geom_line(color = "steelblue", size = 1) +
  geom_smooth(method = "loess", se = TRUE, fill = "lightblue", alpha = 0.3) +
  labs(title = "Temporal Trajectory of Efficiency", y = "Mean Efficiency") +
  theme_minimal()

ggsave(file.path(fig_path, "temporal_trajectory.png"), p_traj)

# ==============================================================================
# 5. BOXPLOT
# ==============================================================================
message(">>> Creating boxplot...")

p_box <- ggplot(df, aes(x = factor(year), y = .data[[target_var]])) +
  geom_boxplot(outlier.alpha = 0.3, fill = "lightgrey") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Year", y = target_var)

ggsave(file.path(fig_path, "efficiency_boxplot.png"), p_box)

# ==============================================================================
# 6. CLUSTERING (SAFE)
# ==============================================================================
message(">>> Running clustering...")

df_cluster <- df %>%
  select(where(is.numeric)) %>%
  select(-year) %>%
  drop_na()

if (nrow(df_cluster) > 10) {
  k <- 4
  kmeans_res <- kmeans(scale(df_cluster), centers = k, nstart = 25)
  p_cluster <- fviz_cluster(kmeans_res, data = scale(df_cluster), geom = "point")
  ggsave(file.path(fig_path, "clusters.png"), p_cluster)
}

# ==============================================================================
# 7. CORRELATION HEATMAP
# ==============================================================================
message(">>> Correlation heatmap...")

corr_mat <- cor(df_cluster, use = "complete.obs")

png(file.path(fig_path, "correlation_heatmap.png"), width = 800, height = 800)
corrplot(corr_mat, method = "color", tl.cex = 0.8, addCoef.col = "black", number.cex = 0.6)
dev.off()

# ==============================================================================
# 8. STRUCTURAL BREAK ANALYSIS (ROBUST)
# ==============================================================================
message(">>> Structural break analysis...")

# Utilisation explicite de mean_eff au lieu de y
break_point_idx <- floor(nrow(df_ts) / 2)
break_year <- df_ts$year[break_point_idx]

# Correction ici : on utilise mean_eff
chow_model <- lm(mean_eff ~ year, data = df_ts)

# Pour sctest, on s'assure que le point est un entier valide
chow_test <- sctest(chow_model, type = "Chow", point = break_point_idx)

# Bai-Perron
bp_model <- breakpoints(mean_eff ~ year, data = df_ts)

write_csv(as.data.frame(bp_model$breakpoints),
          file.path(out_path, "breakpoints.csv"))

# ==============================================================================
# 9. BOOTSTRAP CI
# ==============================================================================
message(">>> Bootstrap confidence intervals...")

boot_fn <- function(data, indices) {
  d <- data[indices, ]
  mean(d[[target_var]], na.rm = TRUE)
}

boot_res <- boot(df, statistic = boot_fn, R = 200)
boot_ci <- boot.ci(boot_res, type = "perc")

capture.output(print(boot_ci),
               file = file.path(out_path, "bootstrap_ci.txt"))

# ==============================================================================
# 10. QUANTILE TREATMENT EFFECTS (SIMPLIFIED)
# ==============================================================================
message(">>> Quantile treatment effects...")

df_q <- df %>%
  mutate(group = ntile(.data[[target_var]], 2))

qte <- df_q %>%
  group_by(group) %>%
  summarise(mean_eff = mean(.data[[target_var]], na.rm = TRUE),
            n = n())

write_csv(qte, file.path(out_path, "quantile_treatment_effects.csv"))

# ==============================================================================
# 11. CLUSTER ROBUSTNESS
# ==============================================================================
message(">>> Cluster robustness...")

cluster_sizes <- tibble()

for (k in 2:6) {
  km <- kmeans(scale(df_cluster), centers = k, nstart = 20)
  cluster_sizes <- bind_rows(
    cluster_sizes,
    tibble(k = k, tot_withinss = km$tot.withinss)
  )
}

write_csv(cluster_sizes, file.path(out_path, "cluster_robustness.csv"))

# ==============================================================================
# 12. SHAP STABILITY (SAFE LOAD)
# ==============================================================================
message(">>> SHAP stability check...")

shap_file <- file.path(out_path, "shap_values.csv")

if (file.exists(shap_file)) {
  shap <- read_csv(shap_file)
  shap_var <- apply(shap, 2, var, na.rm = TRUE)
  
  shap_stability <- tibble(
    variable = names(shap_var),
    variance = shap_var
  ) %>% arrange(desc(variance))
  
  write_csv(shap_stability, file.path(out_path, "shap_stability.csv"))
}

# ==============================================================================
# 13. DASHBOARD DATA
# ==============================================================================
message(">>> Building dashboard dataset...")

dashboard_data <- df %>%
  group_by(year) %>%
  summarise(
    mean_eff = mean(.data[[target_var]], na.rm = TRUE),
    sd_eff   = sd(.data[[target_var]], na.rm = TRUE)
  )

write_csv(dashboard_data, file.path(out_path, "dashboard_data.csv"))
write_csv(df_ts, file.path(out_path, "time_series_efficiency.csv"))

# ==============================================================================
# FINAL MESSAGE
# ==============================================================================
message("---------------------------------------------------------------")
message(">>> SUCCESS: Evidence & Visualization Complete")
message(">>> Figures saved in /outputs/figures/")
message(">>> Ready for dashboard + manuscript figures")
message("---------------------------------------------------------------")