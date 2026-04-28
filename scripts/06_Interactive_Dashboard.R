# ==============================================================================
# Project: Global Health Efficiency
# Script: 06_Interactive_Dashboard
# Objective: Shiny App for Results Exploration
# ==============================================================================

library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(factoextra)
library(shinythemes)

# 1. DATA LOADING --------------------------------------------------------------
# Verified Path: Using spaces as per your successful log
base_path <- "C:/Documents/Global Health Efficiency"
csv_path <- file.path(base_path, "data/processed/ml_feature_matrix.csv")

if (!file.exists(csv_path)) {
  stop(paste0("Check path: ", csv_path))
}

df <- read_csv(csv_path)

# Determine Target Variable
target_var <- if ("dea_score" %in% names(df)) "dea_score" else "life_exp"

# Prepare clustering data (Numeric only, exclude time for cross-sectional view)
df_cluster_clean <- df %>%
  select(where(is.numeric)) %>%
  drop_na()

# 2. UI ------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Global Health Efficiency Dashboard 📊"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Interactive exploration of efficiency trajectories and ML clusters."),
      
      selectInput("country", "Select Country (ISO3):", 
                  choices = sort(unique(df$iso3)), selected = "CAN"),
      
      hr(),
      
      sliderInput("k_clusters", "K-Means Clusters:", 
                  min = 2, max = 6, value = 4),
      
      actionButton("update", "Run Cluster Analysis", icon = icon("play"), 
                   class = "btn-success"),
      
      br(), br(),
      helpText("Project: Global Health Efficiency - 2026")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Efficiency Trends", 
                 plotlyOutput("time_plot"),
                 plotlyOutput("dist_plot")),
        
        tabPanel("Cluster Analysis", 
                 plotlyOutput("cluster_plot")),
        
        tabPanel("Data Explorer", 
                 DTOutput("raw_data"))
      )
    )
  )
)

# 3. SERVER --------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- TRENDS ---
  output$time_plot <- renderPlotly({
    req(input$country)
    
    p <- df %>%
      dplyr::filter(iso3 == input$country) %>%
      ggplot(aes(x = year, y = .data[[target_var]])) +
      geom_line(color = "#2c3e50", linewidth = 1) +
      geom_point(color = "#e74c3c", size = 2) +
      theme_minimal() +
      labs(title = paste("Efficiency Trajectory:", input$country),
           y = "Efficiency Score", x = "Year")
    
    ggplotly(p)
  })
  
  output$dist_plot <- renderPlotly({
    p <- ggplot(df, aes(x = .data[[target_var]])) +
      geom_density(fill = "#3498db", alpha = 0.5) +
      theme_minimal() +
      labs(title = "Global Efficiency Distribution",
           x = "Score", y = "Density")
    
    ggplotly(p)
  })
  
  # --- CLUSTERING ---
  output$cluster_plot <- renderPlotly({
    # CRITICAL: req() prevents the 'sample.int' error on startup
    req(input$k_clusters)
    
    # Wait for action button to avoid lag
    input$update
    
    isolate({
      # Scale data excluding the year column
      data_for_km <- df_cluster_clean %>% select(-year)
      scaled_data <- scale(data_for_km)
      
      set.seed(2026)
      km_res <- kmeans(scaled_data, centers = as.numeric(input$k_clusters), nstart = 25)
      
      p <- fviz_cluster(km_res, data = scaled_data, 
                        geom = "point", 
                        ellipse.type = "t", # 95% confidence ellipse
                        palette = "Set2",
                        main = paste("Clustering Results (k =", input$k_clusters, ")")) +
        theme_minimal()
      
      ggplotly(p)
    })
  })
  
  # --- DATA TABLE ---
  output$raw_data <- renderDT({
    datatable(df, options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE, filter = 'top')
  })
}

# 4. RUN -----------------------------------------------------------------------
shinyApp(ui = ui, server = server)