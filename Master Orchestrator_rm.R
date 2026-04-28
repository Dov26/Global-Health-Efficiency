# ==============================================================================
# Project: Global Health Efficiency - Quantile Trajectories & ML
# Script: Master Orchestrator (rmarkdown, Evidence, Analysis, Policy Integrated)
# Objective: Full Delivery (HTML, PDF, Figures, README, Dashboard)
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
  tidyverse, rmarkdown, knitr, tinytex,
  plotly, shinydashboard, factoextra,
  hrbrthemes, RColorBrewer
)

# --- 3. DIRECTORIES ---
dirs <- c("reports", "outputs", "outputs/figures", "docs")
for (d in dirs) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# --- 4. DATA ---
csv_path <- file.path("data/processed/ml_feature_matrix.csv")
if (!file.exists(csv_path)) stop("Dataset missing")

df <- readr::read_csv(csv_path, show_col_types = FALSE)

# --- 5. SCRIPT 06 (COMPARATIVE FIGURE) ---
fig_path <- file.path("outputs/figures")

comparison_list <- c("CAN","ALB","USA","FRA")

df_comp <- df %>%
  filter(iso3 %in% comparison_list) %>%
  select(iso3, year, dea_score) %>%
  drop_na()

p <- ggplot(df_comp, aes(year, dea_score, color = iso3)) +
  geom_vline(xintercept = 2020, linetype = "dotted", color = "darkred") +
  geom_line(size = 1.2) +
  geom_point() +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Comparative Efficiency Trajectories (2000–2025)",
    subtitle = "Great Convergence and Structural Realignment",
    x = "Year", y = "DEA Efficiency", color = "ISO3"
  )

fig_file <- file.path(fig_path, "comparative_trajectories.png")
ggsave(fig_file, p, width = 11, height = 7, dpi = 300)

cat(">>> Figure generated\n")

# --- 6. README (SCIENTIFIC) ---
readme_path <- file.path("docs", "README.md")

writeLines(c(
  "# Global Health Efficiency Analysis",
  "",
  "## Key Findings",
  "- Translog frontier validated (LR = 378.92, p < 0.001)",
  "- Great Convergence by 2025 (Mean DEA ≈ 0.9999)",
  "- QRF superior performance (R² = 0.912; MAE = 0.038)",
  "",
  "## Structural Insights",
  "- Q10: governance, infrastructure constraints",
  "- Q90: education, innovation drivers",
  "",
  "## Policy Insight",
  "Efficiency is process-driven, not spending-driven.",
  "",
  paste("Generated:", Sys.time())
), readme_path)

# --- 7. RMarkdown (FULL ANALYSIS + POLICY) ---
rmd_path <- file.path("reports", "Final_Analysis.Rmd")

rmd_content <- c(
  "---",
  "title: \"Global Health Efficiency: Evidence, Analysis and Policy\"",
  "author: \"Romuald Guédé, PhD\"",
  "date: \"`r Sys.Date()`\"",
  "output:",
  "  html_document: {toc: true}",
  "  pdf_document: {toc: true}",
  "---",
  
  "# Abstract",
  "Global health systems exhibit strong non-linear efficiency dynamics with a structural break post-2020.",
  
  "# Empirical Evidence",
  "- Translog frontier strongly supported (LR = 378.92, p < 0.001)",
  "- DEA convergence toward frontier (Mean ≈ 0.9999)",
  "- QRF outperforms linear models (R² = 0.912)",
  
  "# Comparative Trajectories",
  "```{r, echo=FALSE}",
  "knitr::include_graphics('outputs/figures/comparative_trajectories.png')",
  "```",
  
  "# Analysis",
  "## Quantile Heterogeneity",
  "- Lower quantiles (Q10): governance, infrastructure bottlenecks",
  "- Upper quantiles (Q90): education expenditure, innovation",
  "",
  "## Great Convergence (2025)",
  "Global efficiency disparities collapse due to digitalization and standardized protocols.",
  "",
  "## Spending Paradox",
  "High spending correlates with wealth but negatively with efficiency.",
  
  "# Policy Recommendations",
  "## Tiered Strategy",
  "",
  "### 1. Q10–Q25 (Low efficiency)",
  "- Strengthen governance",
  "- Improve regulatory quality",
  "",
  "### 2. Q25–Q75 (Transition)",
  "- Invest in digital health systems",
  "- Enable leapfrogging",
  "",
  "### 3. Q75–Q90 (Frontier)",
  "- Prioritize education and R&D",
  "- Strengthen innovation ecosystems",
  "",
  "## Structural Shift",
  "Move from resource-based to process-based health systems.",
  
  "# Conclusion",
  "The 2025 convergence marks a paradigm shift toward process-driven efficiency systems."
)

writeLines(rmd_content, rmd_path)

# --- 8. RENDER REPORTS (ROBUST) ---
cat(">>> Rendering reports...\n")

output_dir <- file.path(base_path, "outputs")
dir.create(output_dir, showWarnings = FALSE)

# HTML
tryCatch({
  rmarkdown::render(rmd_path, "html_document", output_dir = output_dir, quiet = FALSE)
  cat("SUCCESS: HTML\n")
}, error = function(e) cat("HTML ERROR:", e$message, "\n"))

# PDF
tryCatch({
  rmarkdown::render(rmd_path, "pdf_document", output_dir = output_dir, quiet = FALSE)
  cat("SUCCESS: PDF\n")
}, error = function(e) {
  cat("PDF ERROR:", e$message, "\n")
  try(tinytex::install_tinytex(), silent = TRUE)
})

# --- 9. VALIDATION ---
print(list.files("outputs"))

# --- 10. SHINY APP ---
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Health Efficiency"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Dashboard", tabName = "dash"),
      shinydashboard::menuItem("README", tabName = "readme"),
      shinydashboard::menuItem("HTML", href = "outputs/Final_Analysis.html"),
      shinydashboard::menuItem("PDF", href = "outputs/Final_Analysis.pdf")
    )
  ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "dash",
                              plotly::plotlyOutput("plot")
      ),
      shinydashboard::tabItem(tabName = "readme",
                              verbatimTextOutput("readme")
      )
    )
  )
)

server <- function(input, output, session) {
  output$plot <- plotly::renderPlotly({
    plotly::ggplotly(
      ggplot(df, aes(year, dea_score)) + geom_line()
    )
  })
  
  output$readme <- renderText({
    paste(readLines(readme_path), collapse = "\n")
  })
}

shiny::shinyApp(ui, server)