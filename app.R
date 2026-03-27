library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)

# Generate sample recreational fishing data
set.seed(123)
generate_fish_data <- function() {
  species <- c("Atlantic Cod", "Haddock", "Summer Flounder", "Black Sea Bass", "Scup", "Bluefish")
  modes <- c("Shore", "Private/Rental Boat", "Party/Charter Boat")
  years <- 2020:2023
  waves <- 1:6
  
  data <- expand.grid(
    species = species,
    mode = modes,
    year = years,
    wave = waves,
    length_cm = seq(20, 80, by = 5)
  )
  
  data$catch_count <- rpois(nrow(data), lambda = sample(10:50, nrow(data), replace = TRUE))
  data$weight_kg <- round(data$length_cm * 0.015 + rnorm(nrow(data), 0, 0.3), 2)
  data$cpue <- round(runif(nrow(data), 0.1, 2.5), 2)
  
  data
}

fish_data <- generate_fish_data()

ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#003087",
    primary = "#0085CA",
    secondary = "#41B6E6",
    base_font = font_google("Open Sans")
  ),
  
  # NOAA Banner
  div(
    style = "background-color: #003087; padding: 15px 30px; margin-bottom: 0px; display: flex; align-items: center; justify-content: space-between;",
    
    div(
      style = "display: flex; align-items: center;",
      img(
        src = "https://www.fisheries.noaa.gov/themes/custom/noaa_components/images/fisheries_header_logo_jul2019.png",
        height = "60px",
        style = "margin-right: 20px;"
      ),
      h3("Recreational Fisheries Dashboard", 
         style = "color: white; margin: 0; font-weight: 600;")
    ),
    
    div(
      style = "display: flex; gap: 10px; flex-wrap: wrap;",
      downloadButton("download_data", "Download Data", 
                     style = "background-color: #0085CA; border: none; color: white; font-size: 13px;"),
      downloadButton("download_plot", "Download Plot", 
                     style = "background-color: #0085CA; border: none; color: white; font-size: 13px;")
    )
  ),
  
  # Main content
  layout_sidebar(
    sidebar = sidebar(
      title = "Filter Options",
      width = 280,
      
      # Stock section with blue header
      div(
        div(
          style = "background-color: #0085CA; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600;",
          "Stock"
        ),
        selectInput(
          "species",
          NULL,
          choices = c("Atlantic Cod", "Haddock", "Summer Flounder", "Black Sea Bass", "Scup", "Bluefish"),
          selected = "Atlantic Cod"
        )
      ),
      
      # Data Metric section with blue header
      div(
        style = "margin-top: 15px;",
        div(
          style = "background-color: #0085CA; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600;",
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
      
      checkboxGroupInput(
        "mode",
        "Fishing Mode:",
        choices = c("Shore", "Private/Rental Boat", "Party/Charter Boat"),
        selected = c("Shore", "Private/Rental Boat", "Party/Charter Boat")
      ),
      
      radioButtons(
        "time_interval",
        "Time Interval:",
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
    ),
    
    layout_columns(
      col_widths = c(12, 12),
      
      card(
        card_header(textOutput("plot_title")),
        plotlyOutput("main_plot", height = "500px")
      ),
      
      card(
        card_header("Data Summary"),
        tableOutput("summary_table")
      )
    )
  )
)


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
  
  # Catch-at-Length plot
  length_plot_reactive <- reactive({
    data <- filtered_data()
    
    if (input$time_interval == "annual") {
      plot_data <- data %>%
        group_by(length_cm, year) %>%
        summarise(total_catch = sum(catch_count), .groups = "drop")
      
      p <- plot_ly(plot_data, x = ~length_cm, y = ~total_catch, color = ~factor(year),
                   type = "bar", colors = "Blues") %>%
        layout(
          xaxis = list(title = "Length (cm)"),
          yaxis = list(title = "Total Catch Count"),
          barmode = "group",
          legend = list(title = list(text = "Year"))
        )
    } else {
      plot_data <- data %>%
        group_by(length_cm, wave) %>%
        summarise(total_catch = sum(catch_count), .groups = "drop") %>%
        mutate(wave_label = paste("Wave", wave))
      
      p <- plot_ly(plot_data, x = ~length_cm, y = ~total_catch, color = ~wave_label,
                   type = "bar", colors = "Blues") %>%
        layout(
          xaxis = list(title = "Length (cm)"),
          yaxis = list(title = "Total Catch Count"),
          barmode = "group",
          legend = list(title = list(text = "Wave"))
        )
    }
    
    p
  })
  
  output$length_plot <- renderPlotly({
    length_plot_reactive()
  })
  
  # CPUE plot
  cpue_plot_reactive <- reactive({
    data <- filtered_data()
    
    if (input$time_interval == "annual") {
      plot_data <- data %>%
        group_by(year, mode) %>%
        summarise(avg_cpue = mean(cpue, na.rm = TRUE), .groups = "drop")
      
      p <- plot_ly(plot_data, x = ~year, y = ~avg_cpue, color = ~mode,
                   type = "scatter", mode = "lines+markers", colors = "Set2") %>%
        layout(
          xaxis = list(title = "Year"),
          yaxis = list(title = "Average CPUE (fish per trip)"),
          legend = list(title = list(text = "Mode"))
        )
    } else {
      plot_data <- data %>%
        group_by(wave, mode) %>%
        summarise(avg_cpue = mean(cpue, na.rm = TRUE), .groups = "drop")
      
      p <- plot_ly(plot_data, x = ~wave, y = ~avg_cpue, color = ~mode,
                   type = "scatter", mode = "lines+markers", colors = "Set2") %>%
        layout(
          xaxis = list(title = "Wave", tickvals = 1:6),
          yaxis = list(title = "Average CPUE (fish per trip)"),
          legend = list(title = list(text = "Mode"))
        )
    }
    
    p
  })
  
  output$cpue_plot <- renderPlotly({
    cpue_plot_reactive()
  })
  
  # Weight plot
  weight_plot_reactive <- reactive({
    data <- filtered_data()
    
    if (input$time_interval == "annual") {
      plot_data <- data %>%
        group_by(year) %>%
        summarise(avg_weight = mean(weight_kg, na.rm = TRUE), .groups = "drop")
      
      p <- plot_ly(plot_data, x = ~year, y = ~avg_weight, type = "bar",
                   marker = list(color = "#0085CA")) %>%
        layout(
          xaxis = list(title = "Year"),
          yaxis = list(title = "Average Weight (kg)")
        )
    } else {
      plot_data <- data %>%
        group_by(wave) %>%
        summarise(avg_weight = mean(weight_kg, na.rm = TRUE), .groups = "drop")
      
      p <- plot_ly(plot_data, x = ~wave, y = ~avg_weight, type = "bar",
                   marker = list(color = "#0085CA")) %>%
        layout(
          xaxis = list(title = "Wave", tickvals = 1:6),
          yaxis = list(title = "Average Weight (kg)")
        )
    }
    
    p
  })
  
  output$weight_plot <- renderPlotly({
    weight_plot_reactive()
  })
  
  # Download handlers
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("fishing_data_", gsub(" ", "_", input$species), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)