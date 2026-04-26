# ============================================================
# Corn Yield XGBoost Dashboard — Shiny App
# Four tabs:
#   1. Yield by Year (boxplot, interactive)
#   2. Hybrids by Site (interactive selector + slider)
#   3. Variable Importance (still image — vip_test_corn.jpg)
#   4. Model Performance (still image — model_perf_test_data.jpg)
#       with labelled R² line and RMSE line annotations
# ============================================================
#install.packages("base64enc")
library(base64enc)
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(readr)

# ── Base64-encode images so they render in any environment ──
encode_image <- function(path) {
  paste0(
    "data:image/png;base64,",
    base64enc::base64encode(path)
  )
}

vip_src  <- encode_image("vip_test_corn.png")
perf_src <- encode_image("model_perf_test_data.png")

# ── Load data ──────────────────────────────────────────────
corn <- read_csv("final_data.csv", show_col_types = FALSE) %>%
  mutate(year = as.factor(as.character(year)))

all_sites  <- sort(unique(corn$site))
all_years  <- sort(unique(as.character(corn$year)))

# Top 100 hybrids by frequency for the searchable selector
top_hybrids <- corn %>%
  count(hybrid, sort = TRUE) %>%
  slice_head(n = 100) %>%
  pull(hybrid)

# Year colour palette
year_pal <- c(
  "2014" = "#2166ac", "2015" = "#4dac26", "2016" = "#d01c8b",
  "2017" = "#f1a340", "2018" = "#7fbf7b", "2019" = "#af8dc3",
  "2020" = "#e08214", "2021" = "#01665e", "2022" = "#8c510a",
  "2023" = "#c51b7d"
)

# ── UI ─────────────────────────────────────────────────────
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
      .legend-box { display:inline-flex; align-items:center; gap:8px;
                    margin-right:18px; font-size:12px; }
      .legend-line { display:inline-block; width:36px; height:3px; }
    "))
  ),
  
  navbarPage(
    title = "🌽  Corn Yield — XGBoost Dashboard",
    id    = "main_nav",
    
    # ════════════════════════════════════════════════════════
    # TAB 1 · Yield by Year
    # ════════════════════════════════════════════════════════
    tabPanel("Yield by Year",
             sidebarLayout(
               sidebarPanel(width = 3,
                            div(class = "sec-hdr", "📅 Filters"),
                            
                            checkboxGroupInput("yr_years",
                                               label    = "Select Years:",
                                               choices  = all_years,
                                               selected = all_years
                            ),
                            hr(),
                            selectInput("yr_site",
                                        label    = "Filter by Site (optional):",
                                        choices  = c("All Sites" = "ALL", all_sites),
                                        selected = "ALL"
                            ),
                            hr(),
                            sliderInput("yr_ylim",
                                        label = "Yield Axis Range (Mg/ha):",
                                        min = 0, max = 25, value = c(0, 22), step = 0.5
                            ),
                            hr(),
                            checkboxInput("yr_notch",  "Show notched boxes",    value = FALSE),
                            checkboxInput("yr_points", "Overlay jitter points", value = FALSE)
               ),
               
               mainPanel(width = 9,
                         div(class = "sec-hdr",
                             "Corn Yield Distribution by Crop Year"),
                         plotlyOutput("yield_year_plot", height = "520px"),
                         div(class = "plot-caption",
                             "Boxplot of observed grain yield (Mg/ha) per crop year (2014–2023).
             Red diamonds = group means. Hover bars for median / IQR.
             Use year checkboxes and site selector to filter the dataset.")
               )
             )
    ), # end Tab 1
    
    # ════════════════════════════════════════════════════════
    # TAB 2 · Hybrids by Site
    # ════════════════════════════════════════════════════════
    tabPanel("Hybrids by Site",
             sidebarLayout(
               sidebarPanel(width = 3,
                            div(class = "sec-hdr", "🌿 Hybrid Explorer"),
                            
                            selectizeInput("hy_hybrids",
                                           label    = "Select Hybrids (up to 12):",
                                           choices  = top_hybrids,
                                           selected = top_hybrids[1:5],
                                           multiple = TRUE,
                                           options  = list(
                                             maxItems    = 12,
                                             placeholder = "Search all 5,022 hybrids…",
                                             # allow typing any hybrid name, not only top 100
                                             create = FALSE
                                           )
                            ),
                            helpText("Type to search. Up to 12 hybrids at a time."),
                            hr(),
                            
                            # ── interactive site selector ──
                            selectizeInput("hy_sites",
                                           label    = "Select Sites to Display:",
                                           choices  = all_sites,
                                           selected = all_sites[1:10],
                                           multiple = TRUE,
                                           options  = list(placeholder = "Pick sites…")
                            ),
                            hr(),
                            
                            # ── yield axis slider ──
                            sliderInput("hy_ylim",
                                        label = "Yield Axis Range (Mg/ha):",
                                        min = 0, max = 25, value = c(0, 22), step = 0.5
                            ),
                            hr(),
                            
                            selectInput("hy_geom",
                                        label   = "Chart type:",
                                        choices = c(
                                          "Boxplot"     = "box",
                                          "Violin"      = "violin",
                                          "Dot / Strip" = "strip"
                                        ),
                                        selected = "box"
                            )
               ),
               
               mainPanel(width = 9,
                         div(class = "sec-hdr", "Hybrid Yield Performance by Site"),
                         plotlyOutput("hybrid_site_plot", height = "540px"),
                         div(class = "plot-caption",
                             "Yield distributions for selected hybrids across trial sites.
             Colours differentiate hybrids. Hover to see individual values.
             Use the Hybrid selector above to search across all 5,022 unique hybrids in the dataset.")
               )
             )
    ), # end Tab 2
    
    # ════════════════════════════════════════════════════════
    # TAB 3 · Variable Importance (still image)
    # ════════════════════════════════════════════════════════
    tabPanel("Variable Importance",
             fluidRow(
               column(12,
                      div(class = "sec-hdr",
                          "XGBoost Variable Importance (VIP) — Held-out Test Set"),
                      div(class = "img-panel",
                          tags$img(
                            src   = vip_src,           # <-- Base64 string, no file path needed
                            style = "max-width:100%; height:auto; display:block; margin:auto;",
                            alt   = "Variable Importance Plot — XGBoost corn yield model"
                          ),
                          div(class = "plot-caption", style = "text-align:center; margin-top:10px;",
                              "Top predictors ranked by XGBoost variable importance (Gain).
           Mean solar radiation (mean_srad), grain moisture, and days to harvest
           are the dominant features. Site and year indicator variables appear further down the ranking.")
                      )
               )
             )
    ),# end Tab 3
    
    # ════════════════════════════════════════════════════════
    # TAB 4 · Model Performance — Predicted vs. Observed
    # ════════════════════════════════════════════════════════
    tabPanel("Model Performance",
             fluidRow(
               column(12,
                      div(class = "sec-hdr",
                          "Predicted vs. Observed Yield — model_perf_test_data"),
                      div(class = "img-panel",
                          
                          div(style = "margin-bottom:10px;",
                              span(class = "legend-box",
                                   tags$span(style = "display:inline-block; width:36px; border-top:3px dashed red; margin-right:6px;"),
                                   tags$b("1:1 Reference line"),
                                   tags$span(style = "color:#555; font-size:11px;",
                                             " — geom_abline(slope=1, intercept=0): perfect prediction line (red dashed).
                Points on this line = Predicted equals Observed exactly.")
                              ),
                              tags$br(),
                              span(class = "legend-box",
                                   tags$span(style = "display:inline-block; width:36px; border-top:3px solid #2166ac; margin-right:6px;"),
                                   tags$b("OLS Regression line"),
                                   tags$span(style = "color:#555; font-size:11px;",
                                             " — geom_smooth(method='lm'): fitted linear regression of Predicted ~ Observed
                (blue solid). Deviation from red line = systematic bias.")
                              ),
                              tags$br(),
                              tags$div(style = "font-size:11px; color:#555; margin-top:6px;",
                                       tags$b("R²"), " — Proportion of variance in observed yield explained by predictions (closer to 1.0 = better).",
                                       tags$br(),
                                       tags$b("RMSE"), " — Root Mean Squared Error (Mg/ha): average prediction error magnitude."
                              )
                          ),
                          
                          tags$img(
                            src   = perf_src,          # <-- Base64 string, no file path needed
                            style = "max-width:100%; height:auto; display:block; margin:auto;",
                            alt   = "Predicted vs Observed yield — R² and RMSE annotated scatter plot"
                          ),
                          
                          div(class = "plot-caption", style = "text-align:center; margin-top:10px;",
                              "X-axis: Observed Yield (mg/ha) | Y-axis: Predicted Yield (mg/ha) | Both axes: 0–30.")
                      )
               )
             )
    ) # end Tab 4
    
  ) # end navbarPage
)

# ── Server ──────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Tab 1: filtered reactive ─────────────────────────────
  yr_data <- reactive({
    req(input$yr_years)
    d <- corn %>%
      filter(as.character(year) %in% input$yr_years)
    if (!identical(input$yr_site, "ALL"))
      d <- d %>% filter(site == input$yr_site)
    d
  })
  
  output$yield_year_plot <- renderPlotly({
    d <- yr_data()
    req(nrow(d) > 0)
    
    # group means for diamond overlay
    means_df <- d %>%
      group_by(year) %>%
      summarise(m = mean(yield_mg_ha, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(d, aes(x = year, y = yield_mg_ha, fill = year))
    
    if (isTRUE(input$yr_points))
      p <- p + geom_jitter(
        aes(text = paste0("Hybrid: ", hybrid, "\nYield: ", round(yield_mg_ha, 2))),
        alpha = 0.08, size = 0.5, width = 0.25,
        colour = "#333333", show.legend = FALSE
      )
    
    p <- p +
      geom_boxplot(
        notch         = isTRUE(input$yr_notch),
        outlier.size  = 0.7,
        outlier.alpha = 0.25,
        colour        = "grey25",
        alpha         = 0.82
      ) +
      # red diamonds = group means
      geom_point(
        data     = means_df,
        aes(y = m, text = paste0("Mean: ", round(m, 2), " Mg/ha")),
        shape    = 23,
        size     = 3.5,
        fill     = "firebrick",
        colour   = "white",
        stroke   = 0.8,
        inherit.aes = TRUE
      ) +
      scale_fill_manual(values = year_pal, guide = "none") +
      scale_y_continuous(
        limits  = input$yr_ylim,
        expand  = expansion(mult = .02)
      ) +
      labs(
        x     = "Crop Year",
        y     = "Yield (Mg/ha)",
        title = "Corn Grain Yield by Year"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title          = element_text(face = "bold", colour = "#1a3a5c", size = 15),
        axis.title          = element_text(face = "bold"),
        panel.grid.major.x  = element_blank()
      )
    
    ggplotly(p, tooltip = c("x", "y", "text")) %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
  })
  
  # ── Tab 2: filtered reactive ─────────────────────────────
  hy_data <- reactive({
    req(input$hy_hybrids, length(input$hy_hybrids) > 0)
    req(input$hy_sites,   length(input$hy_sites)   > 0)
    corn %>%
      filter(hybrid %in% input$hy_hybrids,
             site   %in% input$hy_sites)
  })
  
  output$hybrid_site_plot <- renderPlotly({
    d <- hy_data()
    req(nrow(d) > 0)
    
    p <- ggplot(d, aes(x = site, y = yield_mg_ha,
                       fill   = hybrid,
                       colour = hybrid,
                       text   = paste0("Hybrid: ", hybrid,
                                       "\nSite: ", site,
                                       "\nYield: ", round(yield_mg_ha, 2), " Mg/ha")))
    
    if (input$hy_geom == "box") {
      p <- p + geom_boxplot(
        position      = position_dodge(width = 0.85),
        alpha         = 0.70,
        outlier.size  = 0.6,
        outlier.alpha = 0.15
      )
    } else if (input$hy_geom == "violin") {
      p <- p + geom_violin(
        position = position_dodge(width = 0.9),
        alpha    = 0.60,
        scale    = "width",
        draw_quantiles = c(0.25, 0.5, 0.75)
      )
    } else {
      p <- p + geom_jitter(
        position = position_jitterdodge(
          jitter.width = 0.15,
          dodge.width  = 0.75
        ),
        size  = 1.4,
        alpha = 0.55
      )
    }
    
    p <- p +
      scale_y_continuous(
        limits = input$hy_ylim,
        expand = expansion(mult = .02)
      ) +
      labs(
        x      = "Trial Site",
        y      = "Yield (Mg/ha)",
        title  = "Hybrid Yield by Site",
        fill   = "Hybrid",
        colour = "Hybrid"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x     = element_text(angle = 45, hjust = 1, size = 9),
        plot.title      = element_text(face = "bold", colour = "#1a3a5c", size = 15),
        axis.title      = element_text(face = "bold"),
        legend.position = "bottom",
        legend.text     = element_text(size = 8)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.3))
  })
}

# ── Run ─────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)