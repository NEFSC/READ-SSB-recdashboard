library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)

# reticulate::install_miniconda()
# reticulate::conda_install("r-reticulate", "python-kaleido")

# Generate sample recreational fishing data
set.seed(123)
generate_fish_data <- function() {
  species <- c("Atlantic Cod", "Haddock", "Summer Flounder", "Black Sea Bass", "Scup", "Bluefish")
  modes <- c("Shore", "Private/Rental Boat", "Party/Charter Boat")
  years <- 2020:2023
  waves <- 1:6
  
  data <- expand.grid(
    species      = species,
    mode         = modes,
    year         = years,
    wave         = waves,
    length_cm    = seq(20, 80, by = 5)
  )
  
  data$catch_count <- rpois(nrow(data), lambda = sample(10:50, nrow(data), replace = TRUE))
  data$weight_kg   <- round(data$length_cm * 0.015 + rnorm(nrow(data), 0, 0.3), 2)
  data$cpue        <- round(runif(nrow(data), 0.1, 2.5), 2)
  
  data %>%
    tidyr::pivot_longer(
      cols      = c(catch_count, weight_kg, cpue),
      names_to  = "metric",
      values_to = "value"
    ) %>%
    mutate(
      data_version = "v1.0",
      units = dplyr::case_when(
        metric == "catch_count" ~ "number of fish",
        metric == "weight_kg"   ~ "kg",
        metric == "cpue"        ~ "fish per trip"
      )
    ) %>%
    select(species, mode, data_version, year, wave, metric, value, units)
}

fish_data <- generate_fish_data()

ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bg = "#F1F2F3",        # --light-gray
    fg = "#323C46",        # --whale-gray
    primary = "#0085CA",   # --noaa-sea
    secondary = "#5EB6D9", # --light-sea
    base_font = font_google("Open Sans")
  ),
  
  # Combined NOAA Banner + Nav Bar
  div(
    style = "
    background-color: #003087;
    border-bottom: 4px solid #0085CA;
    margin: 0;
    padding: 5px 30px;
    display: flex;
    flex-direction: column;
    box-sizing: border-box;
  ",
    
    # --- Top Row: Logo + Title + Buttons ---
    div(
      style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
      
      # Left: Logo + Titles
      div(
        style = "display: flex; align-items: center; gap: 12px;",
        img(
          src = "https://www.fisheries.noaa.gov/themes/custom/noaa_components/images/fisheries_header_logo_jul2019.png",
          height = "50px",
          style = "display: block; vertical-align: top; margin: 0; padding: 0;"
        ),
        div(
          h3("Recreational Fisheries Dashboard",
             style = "
             color: white;
             margin: 0;
             padding: 0;
             font-weight: 700;
             font-size: 18px;
             line-height: 1;
             vertical-align: top;
           "
          ),
          div("Northeast & Mid-Atlantic Region",
              style = "
              color: #C6E6F0;
              font-size: 11px;
              margin: 0;
              padding: 0;
              line-height: 1;
              vertical-align: top;
            "
          )
        )
      ),
      
      # Right: Download Buttons
      div(
        style = "display: flex; gap: 10px; flex-wrap: wrap;",
        downloadButton("download_data", "Download Data",
                       style = "background-color: #0085CA; border: none; color: white; font-size: 13px; border-radius: 3px; padding: 5px 10px;"),
        downloadButton("download_plot", "Download Plot",
                       style = "background-color: transparent; border: 1.5px solid #0085CA; color: white; font-size: 13px; border-radius: 3px; padding: 5px 10px;")
      )
    ),
    
    # --- Bottom Row: Nav Bar ---
    div(
      style = "
      display: flex;
      gap: 5px;
      align-items: center;
      margin-top: 5px;
      height: 35px;
    ",
      
      div("Overview",
          style = "
          color: white;
          font-size: 12.5px;
          font-weight: 600;
          padding: 5px 15px;
          border-bottom: 3px solid #0085CA;
          cursor: pointer;
          margin: 0;
          line-height: 1;
        "
      ),
      
      div("Documentation",
          style = "
          color: white;
          font-size: 12.5px;
          font-weight: 600;
          padding: 5px 15px;
          border-bottom: 3px solid transparent;
          cursor: pointer;
          margin: 0;
          line-height: 1;
        "
      )
    )
  ),
  
 
  # Main content
  layout_sidebar(
    sidebar = sidebar(
      #title = "Filter Options",
      width = 280,
      style = "background-color: #ffffff; border-right: 1px solid #CBCFD1;",
      
      # Stock section
      div(
        div(
          style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
          "Stock"
        ),
        selectInput(
          "species",
          NULL,
          choices = c("Atlantic Cod", "Haddock", "Summer Flounder", "Black Sea Bass", "Scup", "Bluefish"),
          selected = "Atlantic Cod"
        )
      ),
      
      # Data Metric section
      div(
        style = "margin-top: 15px;",
        div(
          style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
          "Data Metric"
        ),
        selectInput(
          "data_metric",
          NULL,
          choices = c("Catch-at-Length" = "length",
                      "CPUE" = "cpue",
                      "Average Weight" = "weight"),
          selected = "length"
        )
      ),
      
      # Fishing Mode section
      div(
        style = "margin-top: 15px;",
        div(
          style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
          "Fishing Mode"
        ),
        checkboxGroupInput(
          "mode",
          NULL,
          choices = c("Shore", "Private/Rental Boat", "Party/Charter Boat"),
          selected = c("Shore", "Private/Rental Boat", "Party/Charter Boat")
        )
      ),
      
      # Time Interval section
      div(
        style = "margin-top: 15px;",
        div(
          style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
          "Time Interval"
        ),
        radioButtons(
          "time_interval",
          NULL,
          choices = c("Annual" = "annual", "By Wave (2-month periods)" = "wave"),
          selected = "annual"
        ),
        conditionalPanel(
          condition = "input.time_interval == 'annual'",
          checkboxGroupInput(
            "years",
            "Select Years:",
            choices = 2020:2023,
            selected = 2020:2023
          )
        ),
        conditionalPanel(
          condition = "input.time_interval == 'wave'",
          selectInput(
            "year_wave",
            "Select Year:",
            choices = 2020:2023,
            selected = 2023
          ),
          checkboxGroupInput(
            "waves",
            "Select Waves:",
            choices = setNames(1:6, paste("Wave", 1:6, c("(Jan-Feb)", "(Mar-Apr)", "(May-Jun)", "(Jul-Aug)", "(Sep-Oct)", "(Nov-Dec)"))),
            selected = 1:6
          )
        )
      )
    ),
    
    layout_columns(
      col_widths = c(12, 12),
      
      card(
        style = "border: 1px solid #CBCFD1; border-radius: 3px;",
        card_header(
          textOutput("plot_title"),
          style = "background-color: #003087; color: white; font-weight: 700; font-size: 13px;"
        ),
        plotlyOutput("main_plot", height = "500px")
      ),
      
      card(
        style = "border: 1px solid #CBCFD1; border-radius: 3px;",
        card_header(
          "Data Summary",
          style = "background-color: #003087; color: white; font-weight: 700; font-size: 13px;"
        ),
        tableOutput("summary_table")
      )
    )
  )
)

#The server needs to be updated to work with the long format data — filtering on metric instead of using column names directly, and extracting length_cm from the wave column where needed. Here's the updated server:
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$species, input$mode)
    
    data <- fish_data %>%
      filter(species == input$species, mode %in% input$mode)
    
    if (input$time_interval == "annual") {
      req(input$years)
      data <- data %>%
        filter(year %in% as.numeric(input$years))
    } else {
      req(input$year_wave, input$waves)
      data <- data %>%
        filter(year == as.numeric(input$year_wave), wave %in% as.numeric(input$waves))
    }
    
    data
  })
  
  # Plot title
  output$plot_title <- renderText({
    metric_label <- switch(input$data_metric,
                           "length" = "Catch-at-Length",
                           "cpue"   = "CPUE (fish per trip)",
                           "weight" = "Average Weight (kg)"
    )
    paste(input$species, "-", metric_label)
  })
  
  plot_obj <- reactive({
    req(filtered_data())
    
    library(ggplot2)
    
    time_var   <- if (input$time_interval == "annual") "year" else "wave"
    time_label <- if (input$time_interval == "annual") "Year" else "Wave"
    
    metric_name <- switch(input$data_metric,
                          "length" = "catch_count",
                          "cpue"   = "cpue",
                          "weight" = "weight_kg"
    )
    
    x_label <- switch(input$data_metric,
                      "length" = "Total Catch Count",
                      "cpue"   = "CPUE (fish per trip)",
                      "weight" = "Average Weight (kg)"
    )
    
    plot_data <- filtered_data() %>%
      dplyr::filter(metric == metric_name)
    
    max_time <- max(plot_data[[time_var]], na.rm = TRUE)
    
    max_data <- plot_data %>%
      dplyr::filter(.data[[time_var]] == max_time)
    
    max_mean <- mean(max_data$value, na.rm = TRUE)
    
    g <- ggplot(max_data, aes(x = value)) +
      geom_histogram(aes(y = ..density..), bins = 30,
                     fill = "#5EB6D9", color = "#0085CA") +
      geom_density(aes(y = after_stat(density)),
                   color = "red", fill = NA, size = 0.5) +
      geom_vline(xintercept = max_mean, linetype = "dashed", size = 1) +
      labs(
        x = x_label,
        y = "Density",
        title = paste("Distribution for", x_label, time_label, max_time)
      ) +
      theme_minimal()
    
    ggplotly(g)
  })
  
  
  
  
  
  
  # Single main plot driven by data_metric
  output$main_plot <- renderPlotly({
    plot_obj()
  })
  
  # Summary table
  output$summary_table <- renderTable({
    
    metric_name <- switch(input$data_metric,
                          "length" = "catch_count",
                          "cpue"   = "cpue",
                          "weight" = "weight_kg"
    )
    time_var <- if (input$time_interval == "annual") "year" else "wave"
    
    filtered_data() %>%
      filter(metric == metric_name) %>%
      group_by(mode, .data[[time_var]]) %>%
      summarise(
        Median  = round(median(value, na.rm = TRUE), 2),
        Q25     = round(quantile(value, 0.25, na.rm = TRUE), 2),
        Q75     = round(quantile(value, 0.75, na.rm = TRUE), 2),
        N       = n(),
        .groups = "drop"
      ) %>%
      rename(!!time_var := .data[[time_var]])
  })
  
  # Download data
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("fishing_data_", gsub(" ", "_", input$species), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Download plot
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("fishing_plot_", gsub(" ", "_", input$species), "_", Sys.Date(), ".png")
    },
    content = function(file) {
      plotly::save_image(plot_obj(), file)
    }
  )
}

shinyApp(ui, server)