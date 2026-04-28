# ==============================================================================
# Project: Global Health Efficiency - Quantile Trajectories & ML
# Script: Master Orchestrator (Quarto Integrated Version)
# Objective: Full Delivery via Quarto (HTML, PDF, Figures, README, Dashboard)
# Author: Romuald Guédé, PhD
# ==============================================================================

rm(list = ls()); gc()

# --- 1. PATH RESOLUTION ---
path1 <- "C:/Documents/Global-Health-Efficiency"
path2 <- "C:/Documents/Global Health Efficiency"

base_path <- if (file.exists(path1)) path1 else if (file.exists(path2)) path2 else
  stop("CRITICAL: No valid project directory found.")

setwd(base_path)
cat(">>> Base path:", base_path, "\n")

# --- 2. PACKAGES ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, quarto, knitr, tinytex,
  plotly, shinydashboard, factoextra,
  hrbrthemes, RColorBrewer
)

# Vérification de l'installation de Quarto
if(!quarto::quarto_binary_sitrep()) stop("Quarto CLI n'est pas détecté par R.")

# --- 3. DIRECTORIES ---
dirs <- c("reports", "outputs", "outputs/figures", "docs")
for (d in dirs) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# --- 4. DATA ---
csv_path <- file.path("data/processed/ml_feature_matrix.csv")
if (!file.exists(csv_path)) stop("Dataset missing")

df <- readr::read_csv(csv_path, show_col_types = FALSE)

# --- 5. FIGURE GÉNÉRATION (Inchangé) ---
fig_path <- file.path("outputs/figures")
comparison_list <- c("CAN","ALB","USA","FRA")
df_comp <- df %>% filter(iso3 %in% comparison_list) %>% drop_na(dea_score)

p <- ggplot(df_comp, aes(year, dea_score, color = iso3)) +
  geom_vline(xintercept = 2020, linetype = "dotted", color = "darkred") +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Comparative Efficiency Trajectories", y = "DEA Efficiency")

ggsave(file.path(fig_path, "comparative_trajectories.png"), p, width = 11, height = 7)

# --- 6. QUARTO CONFIGURATION (_quarto.yml) ---
# On crée le fichier de configuration pour centraliser le design
q_yml <- "
project:
  type: website
  output-dir: ../outputs

format:
  html:
    theme: cosmo
    toc: true
    code-fold: true
  pdf:
    documentclass: report
    toc: true
"
writeLines(q_yml, "_quarto.yml")

# --- 7. QUARTO REPORT GENERATION (.qmd) ---
qmd_path <- file.path("reports", "Final_Analysis.qmd")

qmd_content <- c(
  "---",
  "title: \"Global Health Efficiency: Evidence, Analysis and Policy\"",
  "subtitle: \"Transition from RMarkdown to Quarto\"",
  "author: \"Romuald Guédé, PhD\"",
  "date: last-modified",
  "---",
  
  "## Abstract",
  "Global health systems exhibit strong non-linear efficiency dynamics post-2020.",
  
  "## Empirical Evidence",
  "- Translog frontier supported (LR = 378.92)",
  "- QRF performance (R² = 0.912)",
  
  "## Comparative Trajectories",
  "![Trajectories](../outputs/figures/comparative_trajectories.png)",
  
  "## Policy Recommendations",
  "### Q10–Q25 (Low efficiency)",
  "- Strengthen governance and regulatory quality.",
  
  "### Q75–Q90 (Frontier)",
  "- Prioritize innovation ecosystems and R&D."
)

writeLines(qmd_content, qmd_path)

# --- 8. RENDER QUARTO REPORTS ---
cat(">>> Rendering Quarto reports...\n")

# Rendu HTML
tryCatch({
  quarto::quarto_render(qmd_path, output_format = "html")
  cat("SUCCESS: Quarto HTML generated in /outputs\n")
}, error = function(e) cat("QUARTO HTML ERROR:", e$message, "\n"))

# Rendu PDF
tryCatch({
  quarto::quarto_render(qmd_path, output_format = "pdf")
  cat("SUCCESS: Quarto PDF generated in /outputs\n")
}, error = function(e) cat("QUARTO PDF ERROR:", e$message, "\n"))

# --- 9. SHINY APP (Actualisée pour les chemins Quarto) ---
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Health Efficiency Quarto"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Dashboard", tabName = "dash", icon = icon("chart-line")),
      shinydashboard::menuItem("Voir Rapport HTML", icon = icon("file-code"), 
                               href = "outputs/Final_Analysis.html"),
      shinydashboard::menuItem("Télécharger PDF", icon = icon("file-pdf"), 
                               href = "outputs/Final_Analysis.pdf")
    )
  ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "dash", plotly::plotlyOutput("plot"))
    )
  )
)

server <- function(input, output, session) {
  # Pour servir les fichiers statiques dans Shiny si lancé localement
  addResourcePath("outputs", file.path(base_path, "outputs"))
  
  output$plot <- plotly::renderPlotly({
    plotly::ggplotly(ggplot(df, aes(year, dea_score)) + geom_line(alpha=0.3, aes(group=iso3)))
  })
}

shiny::shinyApp(ui, server)