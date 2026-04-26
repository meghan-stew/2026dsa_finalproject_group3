library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(readr)

find_file <- function(file_name) {
  possible_paths <- c(
    file_name,
    file.path("Data", file_name),
    file.path("data", file_name),
    file.path("Output", file_name)
  )
  
  existing <- possible_paths[file.exists(possible_paths)]
  
  if (length(existing) == 0) {
    stop(
      paste0("Cannot find ", file_name, ". Put it in your project folder or Data folder."),
      call. = FALSE
    )
  }
  
  existing[1]
}

find_image <- function(file_names) {
  possible_paths <- c(
    file_names,
    file.path("www", file_names),
    file.path("Output", file_names),
    file.path("output", file_names),
    file.path("Figures", file_names),
    file.path("figures", file_names)
  )
  
  existing <- possible_paths[file.exists(possible_paths)]
  
  if (length(existing) == 0) {
    return(NA_character_)
  }
  
  normalizePath(existing[1], winslash = "/", mustWork = TRUE)
}

corn_path <- find_file("final_data.csv")

corn <- read_csv(corn_path, show_col_types = FALSE)

corn <- corn %>%
  select(-starts_with("..."))

required_cols <- c("yield_mg_ha", "year", "site", "hybrid")
missing_cols <- setdiff(required_cols, names(corn))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "These columns are missing from final_data.csv: ",
      paste(missing_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}

corn <- corn %>%
  mutate(
    year = as.factor(as.character(year)),
    site = as.factor(site),
    hybrid = as.factor(hybrid)
  )

all_sites <- sort(unique(as.character(corn$site)))
all_years <- sort(unique(as.character(corn$year)))
all_hybrids <- sort(unique(as.character(corn$hybrid)))

hybrid_counts <- corn %>%
  count(hybrid, sort = TRUE)

top_hybrids <- hybrid_counts %>%
  slice_head(n = min(100, nrow(hybrid_counts))) %>%
  pull(hybrid) %>%
  as.character()

yield_min <- floor(min(corn$yield_mg_ha, na.rm = TRUE))
yield_max <- ceiling(max(corn$yield_mg_ha, na.rm = TRUE))

if (!is.finite(yield_min)) {
  yield_min <- 0
}

if (!is.finite(yield_max)) {
  yield_max <- 25
}

if (yield_min == yield_max) {
  yield_max <- yield_min + 1
}

vip_path <- find_image(c(
  "vip_test_corn.jpg",
  "vip_test_corn.jpeg",
  "vip_test_corn.png",
  "vip_test_corn.JPG",
  "vip_test_corn.PNG"
))

perf_path <- find_image(c(
  "model_perf_test_data.jpg",
  "model_perf_test_data.jpeg",
  "model_perf_test_data.png",
  "model_perf_test_data.JPG",
  "model_perf_test_data.PNG"
))

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: 'Segoe UI', Arial, sans-serif; background:#f4f6f9; }
      .navbar-default { background-color:#1a3a5c; border-color:#1a3a5c; }
      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a { color:#ffffff !important; }
      .navbar-default .navbar-nav > .active > a { background-color:#2c6fad !important; }
      .well { background:#fff; border:1px solid #dee2e6; border-radius:8px; }
      .plot-caption { font-size:11px; color:#6c757d; margin-top:6px; line-height:1.5; }
      .img-panel { background:white; padding:16px; border-radius:8px;
                   border:1px solid #dee2e6; box-shadow:0 2px 6px rgba(0,0,0,.07); }
      .sec-hdr { background:#1a3a5c; color:white; padding:8px 14px;
                 border-radius:6px; margin-bottom:14px;
                 font-size:15px; font-weight:600; }
    "))
  ),
  
  navbarPage(
    title = "Corn Yield Dashboard",
    id = "main_nav",
    
    tabPanel(
      "Yield by Year",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(class = "sec-hdr", "Filters"),
          
          checkboxGroupInput(
            "yr_years",
            label = "Select Years:",
            choices = all_years,
            selected = all_years
          ),
          
          selectInput(
            "yr_site",
            label = "Filter by Site:",
            choices = c("All Sites" = "ALL", all_sites),
            selected = "ALL"
          ),
          
          sliderInput(
            "yr_ylim",
            label = "Yield Axis Range (Mg/ha):",
            min = yield_min,
            max = yield_max,
            value = c(yield_min, yield_max),
            step = 0.5
          ),
          
          checkboxInput("yr_notch", "Show notched boxes", value = FALSE),
          checkboxInput("yr_points", "Overlay jitter points", value = FALSE)
        ),
        
        mainPanel(
          width = 9,
          div(class = "sec-hdr", "Corn Yield Distribution by Crop Year"),
          plotlyOutput("yield_year_plot", height = "520px"),
          div(
            class = "plot-caption",
            "Boxplot of observed corn yield by crop year. Red diamonds show yearly means."
          )
        )
      )
    ),
    
    tabPanel(
      "Hybrids by Site",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(class = "sec-hdr", "Hybrid Explorer"),
          
          selectizeInput(
            "hy_hybrids",
            label = "Select Hybrids:",
            choices = all_hybrids,
            selected = top_hybrids[1:min(5, length(top_hybrids))],
            multiple = TRUE,
            options = list(
              maxItems = 12,
              placeholder = "Search hybrids..."
            )
          ),
          
          selectizeInput(
            "hy_sites",
            label = "Select Sites:",
            choices = all_sites,
            selected = all_sites[1:min(10, length(all_sites))],
            multiple = TRUE,
            options = list(
              placeholder = "Pick sites..."
            )
          ),
          
          sliderInput(
            "hy_ylim",
            label = "Yield Axis Range (Mg/ha):",
            min = yield_min,
            max = yield_max,
            value = c(yield_min, yield_max),
            step = 0.5
          ),
          
          selectInput(
            "hy_geom",
            label = "Chart type:",
            choices = c(
              "Boxplot" = "box",
              "Violin" = "violin",
              "Dot / Strip" = "strip"
            ),
            selected = "box"
          )
        ),
        
        mainPanel(
          width = 9,
          div(class = "sec-hdr", "Hybrid Yield Performance by Site"),
          plotlyOutput("hybrid_site_plot", height = "540px"),
          div(
            class = "plot-caption",
            "Yield distributions for selected hybrids across selected trial sites."
          )
        )
      )
    ),
    
    tabPanel(
      "Variable Importance",
      fluidRow(
        column(
          12,
          div(class = "sec-hdr", "XGBoost Variable Importance"),
          div(
            class = "img-panel",
            uiOutput("vip_message"),
            imageOutput("vip_image", height = "700px")
          )
        )
      )
    ),
    
    tabPanel(
      "Model Performance",
      fluidRow(
        column(
          12,
          div(class = "sec-hdr", "Predicted vs Observed Yield"),
          div(
            class = "img-panel",
            uiOutput("perf_message"),
            imageOutput("perf_image", height = "700px")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  yr_data <- reactive({
    req(input$yr_years)
    
    d <- corn %>%
      filter(as.character(year) %in% input$yr_years)
    
    if (!identical(input$yr_site, "ALL")) {
      d <- d %>%
        filter(as.character(site) == input$yr_site)
    }
    
    d
  })
  
  output$yield_year_plot <- renderPlotly({
    d <- yr_data()
    req(nrow(d) > 0)
    
    means_df <- d %>%
      group_by(year) %>%
      summarise(mean_yield = mean(yield_mg_ha, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(d, aes(x = year, y = yield_mg_ha, fill = year))
    
    if (isTRUE(input$yr_points)) {
      p <- p +
        geom_jitter(
          alpha = 0.10,
          size = 0.6,
          width = 0.25,
          show.legend = FALSE
        )
    }
    
    p <- p +
      geom_boxplot(
        notch = isTRUE(input$yr_notch),
        outlier.size = 0.7,
        outlier.alpha = 0.25,
        alpha = 0.82,
        show.legend = FALSE
      ) +
      geom_point(
        data = means_df,
        aes(x = year, y = mean_yield),
        shape = 23,
        size = 3.5,
        fill = "firebrick",
        color = "white",
        inherit.aes = FALSE
      ) +
      coord_cartesian(ylim = input$yr_ylim) +
      labs(
        x = "Crop Year",
        y = "Yield (Mg/ha)",
        title = "Corn Grain Yield by Year"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", color = "#1a3a5c"),
        axis.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank()
      )
    
    ggplotly(p)
  })
  
  hy_data <- reactive({
    req(input$hy_hybrids)
    req(input$hy_sites)
    
    corn %>%
      filter(
        as.character(hybrid) %in% input$hy_hybrids,
        as.character(site) %in% input$hy_sites
      )
  })
  
  output$hybrid_site_plot <- renderPlotly({
    d <- hy_data()
    req(nrow(d) > 0)
    
    p <- ggplot(
      d,
      aes(
        x = site,
        y = yield_mg_ha,
        fill = hybrid,
        color = hybrid,
        text = paste0(
          "Hybrid: ", hybrid,
          "\nSite: ", site,
          "\nYear: ", year,
          "\nYield: ", round(yield_mg_ha, 2), " Mg/ha"
        )
      )
    )
    
    if (input$hy_geom == "box") {
      p <- p +
        geom_boxplot(
          position = position_dodge(width = 0.85),
          alpha = 0.70,
          outlier.size = 0.6,
          outlier.alpha = 0.15
        )
    }
    
    if (input$hy_geom == "violin") {
      p <- p +
        geom_violin(
          position = position_dodge(width = 0.90),
          alpha = 0.60,
          scale = "width"
        )
    }
    
    if (input$hy_geom == "strip") {
      p <- p +
        geom_jitter(
          position = position_jitterdodge(
            jitter.width = 0.15,
            dodge.width = 0.75
          ),
          size = 1.4,
          alpha = 0.55
        )
    }
    
    p <- p +
      coord_cartesian(ylim = input$hy_ylim) +
      labs(
        x = "Trial Site",
        y = "Yield (Mg/ha)",
        title = "Hybrid Yield by Site",
        fill = "Hybrid",
        color = "Hybrid"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        plot.title = element_text(face = "bold", color = "#1a3a5c"),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 8)
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$vip_message <- renderUI({
    if (is.na(vip_path)) {
      tags$div(
        tags$h4("Variable importance image not found."),
        tags$p("The app searched for vip_test_corn.jpg, vip_test_corn.jpeg, and vip_test_corn.png in the project folder, www folder, Output folder, and Figures folder.")
      )
    } else {
      tags$div(
        class = "plot-caption",
        style = "text-align:center; margin-bottom:10px;",
        paste("Showing image from:", vip_path)
      )
    }
  })
  
  output$vip_image <- renderImage({
    req(!is.na(vip_path))
    list(
      src = vip_path,
      contentType = "image/jpeg",
      width = "100%",
      alt = "Variable importance plot"
    )
  }, deleteFile = FALSE)
  
  output$perf_message <- renderUI({
    if (is.na(perf_path)) {
      tags$div(
        tags$h4("Model performance image not found."),
        tags$p("The app searched for model_perf_test_data.jpg, model_perf_test_data.jpeg, and model_perf_test_data.png in the project folder, www folder, Output folder, and Figures folder.")
      )
    } else {
      tags$div(
        class = "plot-caption",
        style = "text-align:center; margin-bottom:10px;",
        paste("Showing image from:", perf_path)
      )
    }
  })
  
  output$perf_image <- renderImage({
    req(!is.na(perf_path))
    list(
      src = perf_path,
      contentType = "image/jpeg",
      width = "100%",
      alt = "Model performance plot"
    )
  }, deleteFile = FALSE)
}

shinyApp(ui = ui, server = server)