source("Code/utils_pipeline.R")

check_required_packages(c("dplyr", "ggplot2", "plotly", "readr", "shiny"))

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(readr)
  library(shiny)
})

train_path <- project_path("Output", "app", "train_data_for_app.rds")
metrics_path <- project_path("Output", "metrics", "assessment_metrics.csv")
preds_path <- project_path("Output", "metrics", "assessment_predictions.csv")
importance_path <- project_path("Output", "metrics", "variable_importance.csv")

if (!file.exists(train_path) || !file.exists(metrics_path) ||
    !file.exists(preds_path) || !file.exists(importance_path)) {
  stop(
    "Run Code/01_build_model_data.R and Code/02_train_models.R before launching the app.",
    call. = FALSE
  )
}

train_data <- readr::read_rds(train_path)
assessment_metrics <- readr::read_csv(metrics_path, show_col_types = FALSE)
assessment_predictions <- readr::read_csv(preds_path, show_col_types = FALSE)
variable_importance <- readr::read_csv(importance_path, show_col_types = FALSE)
champion_model <- readLines(project_path("Output", "models", "champion_model.txt"), warn = FALSE)

soil_choices <- c("soilph", "om_pct", "soilk_ppm", "soilp_ppm")
weather_choices <- c("annual_prcp", "annual_gdd", "growing_prcp", "growing_tmax")

ui <- fluidPage(
  titlePanel("Corn Yield Final Project Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "site_filter",
        "Site",
        choices = c("All", sort(unique(as.character(train_data$site)))),
        selected = "All"
      ),
      selectInput(
        "soil_var",
        "Soil variable",
        choices = soil_choices,
        selected = "soilph"
      ),
      selectInput(
        "weather_var",
        "Weather variable",
        choices = weather_choices,
        selected = "growing_prcp"
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Yield EDA",
          plotlyOutput("yield_hist", height = "350px"),
          plotlyOutput("yield_by_site", height = "350px")
        ),
        tabPanel(
          "Predictors",
          plotlyOutput("soil_plot", height = "350px"),
          plotlyOutput("weather_plot", height = "350px")
        ),
        tabPanel(
          "Model Results",
          tableOutput("metric_table"),
          plotlyOutput("pred_obs_plot", height = "400px"),
          plotlyOutput("vip_plot", height = "400px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_train <- reactive({
    if (identical(input$site_filter, "All")) {
      train_data
    } else {
      dplyr::filter(train_data, as.character(site) == input$site_filter)
    }
  })
  
  output$yield_hist <- renderPlotly({
    p <- filtered_train() %>%
      ggplot(aes(x = yield_mg_ha)) +
      geom_histogram(bins = 35, fill = "#2f6b2f", color = "white") +
      labs(x = "Yield (Mg/ha)", y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$yield_by_site <- renderPlotly({
    p <- filtered_train() %>%
      ggplot(aes(x = site, y = yield_mg_ha)) +
      geom_boxplot(fill = "#cfd8c3") +
      labs(x = "Site", y = "Yield (Mg/ha)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p)
  })
  
  output$soil_plot <- renderPlotly({
    p <- filtered_train() %>%
      ggplot(aes(x = .data[[input$soil_var]], y = yield_mg_ha, color = site)) +
      geom_point(alpha = 0.35) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(x = input$soil_var, y = "Yield (Mg/ha)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$weather_plot <- renderPlotly({
    p <- filtered_train() %>%
      ggplot(aes(x = .data[[input$weather_var]], y = yield_mg_ha, color = site)) +
      geom_point(alpha = 0.35) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(x = input$weather_var, y = "Yield (Mg/ha)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$metric_table <- renderTable({
    assessment_metrics %>%
      tidyr::pivot_wider(names_from = .metric, values_from = .estimate) %>%
      dplyr::arrange(rmse)
  })
  
  output$pred_obs_plot <- renderPlotly({
    p <- assessment_predictions %>%
      ggplot(aes(x = yield_mg_ha, y = .pred, color = model)) +
      geom_point(alpha = 0.4) +
      geom_abline(slope = 1, intercept = 0, linetype = 2) +
      labs(
        x = "Observed yield (Mg/ha)",
        y = "Predicted yield (Mg/ha)",
        title = "Predicted vs Observed on Internal Test Set"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$vip_plot <- renderPlotly({
    p <- variable_importance %>%
      dplyr::filter(model == champion_model) %>%
      ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
      geom_col(fill = "#2f6b2f") +
      coord_flip() +
      labs(
        x = NULL,
        y = "Importance",
        title = paste("Top Variables for", champion_model)
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)