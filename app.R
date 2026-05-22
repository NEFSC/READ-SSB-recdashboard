library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(123)
generate_fish_data <- function() {
  species <- c("Atlantic Cod", "Haddock", "Summer Flounder", "Black Sea Bass", "Scup", "Bluefish")
  modes <- c("Shore", "Private/Rental Boat", "Party/Charter Boat")
  years <- 2020:2023
  waves <- 1:6
  
  data <- expand.grid(
    species   = species,
    mode      = modes,
    year      = years,
    wave      = waves,
    length_cm = seq(20, 80, by = 5)
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

# ── Load NAA RDS files ────────────────────────────────────────────────────────
# Helper: pivot wide age columns to long
pivot_naa_long <- function(df) {
  age_cols <- grep("^age\\d+$", names(df), value = TRUE)
  df %>%
    tidyr::pivot_longer(cols = all_of(age_cols),
                        names_to  = "age",
                        values_to = "naa") %>%
    mutate(age = as.integer(sub("age", "", age)))
}
# Helper: parse the metric field to deal with NAA

parse_metric_naa <- function(df) {
  df %>%
    mutate(age = as.integer(str_split_i(metric,pattern=" of Age " ,-1)),
    metric_parsed = str_split_i(metric,pattern=" of Age " ,-2)     
    )
}



naa_data <- list(
  cod_historical  = parse_metric_naa(readRDS(here::here("data", "main", "WGOM_Cod_historical_NAA_2026-05-21.Rds"))),
  cod_projected   = parse_metric_naa(readRDS(here::here("data", "main", "WGOM_Cod_projected_NAA_2026-05-21.Rds"))),
  haddock_historical = parse_metric_naa(readRDS(here::here("data", "main", "GOM_Haddock_historical_NAA_2026-05-21.Rds"))),
  haddock_projected  = parse_metric_naa(readRDS(here::here("data", "main", "GOM_Haddock_projected_NAA_2026-05-21.Rds")))
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bg = "#F1F2F3",
    fg = "#323C46",
    primary = "#0085CA",
    secondary = "#5EB6D9",
    base_font = font_google("Open Sans")
  ),
  
  shinyjs::useShinyjs(),
  
  tags$input(id = "current_tab", type = "hidden", value = "overview"),
  
  # Banner + Nav Bar
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
    
    div(
      style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
      
      div(
        style = "display: flex; align-items: center; gap: 12px;",
        img(
          src = "https://www.fisheries.noaa.gov/themes/custom/noaa_components/images/fisheries_header_logo_jul2019.png",
          height = "50px",
          style = "display: block; vertical-align: top; margin: 0; padding: 0;"
        ),
        div(
          h3("Recreational Fisheries Dashboard - PROTOTYPE - Fake Data",
             style = "color: white; margin: 0; padding: 0; font-weight: 700; font-size: 18px; line-height: 1;"),
          div("Northeast & Mid-Atlantic Region",
              style = "color: #C6E6F0; font-size: 11px; margin: 0; padding: 0; line-height: 1;")
        )
      ),
      
      conditionalPanel(
        condition = "input.current_tab == 'overview'",
        div(
          style = "display: flex; gap: 10px; flex-wrap: wrap;",
          downloadButton("download_data", "Download Data",
                         style = "background-color: #0085CA; border: none; color: white; font-size: 13px; border-radius: 3px; padding: 5px 10px;"),
          downloadButton("download_plot", "Download Plot",
                         style = "background-color: transparent; border: 1.5px solid #0085CA; color: white; font-size: 13px; border-radius: 3px; padding: 5px 10px;")
        )
      )
    ),
    
    div(
      style = "display: flex; gap: 5px; align-items: center; margin-top: 5px; height: 35px;",
      
      actionLink("nav_overview", "Overview",
                 style = "color: white; font-size: 12.5px; font-weight: 600; padding: 5px 15px; cursor: pointer; margin: 0; line-height: 1; text-decoration: none;",
                 class = "nav-link active-nav"),
      
      actionLink("nav_documentation", "Documentation",
                 style = "color: white; font-size: 12.5px; font-weight: 600; padding: 5px 15px; cursor: pointer; margin: 0; line-height: 1; text-decoration: none;",
                 class = "nav-link")
    )
  ),
  
  tags$style(HTML("
    .nav-link {
      border-bottom: 3px solid transparent;
      transition: border-bottom 0.3s;
    }
    .active-nav {
      border-bottom: 3px solid #0085CA !important;
    }
  ")),
  
  div(id = "overview_panel",
      layout_sidebar(
        sidebar = sidebar(
          width = 280,
          style = "background-color: #ffffff; border-right: 1px solid #CBCFD1;",
          
          # Stock selector — always visible; choices narrow to Cod/Haddock when NAA selected
          div(id = "stock_selector",
              div(style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
                  "Stock"),
              selectInput("species", NULL,
                          choices = c("Atlantic Cod", "Haddock", "Summer Flounder",
                                      "Black Sea Bass", "Scup", "Bluefish"),
                          selected = "Atlantic Cod")
          ),
          
          div(
            style = "margin-top: 15px;",
            div(style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
                "Data Metric"),
            selectInput("data_metric", NULL,
                        choices = c(
                          "Catch-at-Length"  = "length",
                          "CPUE"             = "cpue",
                          "Average Weight"   = "weight",
                          "Numbers-at-Age"   = "naa"
                        ),
                        selected = "length")
          ),
          
          # Fishing Mode — hidden when NAA is selected
          div(id = "fishing_mode_control",
              div(
                style = "margin-top: 15px;",
                div(style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
                    "Fishing Mode"),
                checkboxGroupInput("mode", NULL,
                                   choices  = c("Shore", "Private/Rental Boat", "Party/Charter Boat"),
                                   selected = c("Shore", "Private/Rental Boat", "Party/Charter Boat"))
              )
          ),
          
          conditionalPanel(
            condition = "input.species == 'Summer Flounder' || input.species == 'Black Sea Bass' || input.species == 'Scup'",
            div(
              style = "margin-top: 15px;",
              div(style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
                  "State"),
              checkboxGroupInput("state", NULL,
                                 choices  = c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC"),
                                 selected = c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC"))
            )
          ),
          
          # Time Interval section — label always shown; contents swap
          div(id = "time_interval_control",
              div(
                style = "margin-top: 15px;",
                div(style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
                    "Time Interval"),
                
                # Standard options (Annual / By Wave) — shown when metric != naa
                div(id = "time_standard",
                    radioButtons("time_interval", NULL,
                                 choices  = c("Annual" = "annual", "By Wave (2-month periods)" = "wave"),
                                 selected = "annual"),
                    conditionalPanel(
                      condition = "input.time_interval == 'annual'",
                      checkboxGroupInput("years", "Select Years:", choices = 2020:2023, selected = 2020:2023)
                    ),
                    conditionalPanel(
                      condition = "input.time_interval == 'wave'",
                      selectInput("year_wave", "Select Year:", choices = 2020:2023, selected = 2023),
                      checkboxGroupInput("waves", "Select Waves:",
                                         choices  = setNames(1:6, paste("Wave", 1:6, c("(Jan-Feb)", "(Mar-Apr)", "(May-Jun)", "(Jul-Aug)", "(Sep-Oct)", "(Nov-Dec)"))),
                                         selected = 1:6)
                    )
                ),
                
                # Historical / Projected — shown only when metric == naa; starts hidden
                shinyjs::hidden(
                  div(id = "time_naa",
                      radioButtons("naa_period", NULL,
                                   choices  = c("Historical" = "historical", "Projected" = "projected"),
                                   selected = "historical")
                  )
                )
              )
          )
        ),
        
        layout_columns(
          col_widths = c(12, 12),
          card(
            style = "border: 1px solid #CBCFD1; border-radius: 3px;",
            card_header(textOutput("plot_title"),
                        style = "background-color: #003087; color: white; font-weight: 700; font-size: 13px;"),
            plotlyOutput("main_plot", height = "500px")
          ),
          card(
            style = "border: 1px solid #CBCFD1; border-radius: 3px;",
            card_header("Data Summary",
                        style = "background-color: #003087; color: white; font-weight: 700; font-size: 13px;"),
            tableOutput("summary_table")
          )
        )
      )
  ),
  
  shinyjs::hidden(
    div(id = "documentation_panel",
        style = "padding: 30px;",
        card(
          style = "border: 1px solid #CBCFD1; border-radius: 3px;",
          card_header("Documentation",
                      style = "background-color: #003087; color: white; font-weight: 700; font-size: 15px;"),
          card_body(
            style = "padding: 0;",
            div(
              style = "display: flex; height: 100%;",
              
              div(
                style = "
                width: 240px;
                min-width: 240px;
                background-color: #f8f9fa;
                border-right: 1px solid #CBCFD1;
                padding: 20px 15px;
              ",
                div(
                  style = "background-color: #003087; color: white; padding: 8px 12px; margin: -20px -15px 15px -15px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
                  "Data Metric"
                ),
                ## Add documentation link here
                selectInput(
                  "doc_metric", NULL,
                  choices  = c("Catch-at-Length" = "length_doc", "Cod Numbers-at-age" = "naa_cod_doc", 
                               "Haddock Numbers-at-age" = "naa_haddock_doc", 
                               "Directed Trips and Catch" = "trips_catch_cod_haddock_doc"),
                  selected = c("length_doc", "naa_cod_doc", "naa_haddock_doc", "trips_catch_cod_haddock_doc"),
                  width    = "100%"
                ),
              ),
              
              div(
                style = "flex: 1; padding: 25px 30px; overflow-y: auto;",
                uiOutput("documentation_content")
              )
            )
          )
        )
    )
  )
)


# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  shiny::addResourcePath("docs", here::here("docs"))
  
  # ── React to metric switching ──────────────────────────────────────────────
  observeEvent(input$data_metric, {
    if (input$data_metric == "naa") {
      # Narrow species choices to only those with NAA data
      updateSelectInput(session, "species",
                        choices  = c("Atlantic Cod", "Haddock"),
                        selected = if (input$species %in% c("Atlantic Cod", "Haddock")) input$species else "Atlantic Cod")
      shinyjs::hide("fishing_mode_control")
      shinyjs::hide("time_standard")
      shinyjs::show("time_naa")
    } else {
      updateSelectInput(session, "species",
                        choices  = c("Atlantic Cod", "Haddock", "Summer Flounder",
                                     "Black Sea Bass", "Scup", "Bluefish"),
                        selected = input$species)
      shinyjs::show("fishing_mode_control")
      shinyjs::show("time_standard")
      shinyjs::hide("time_naa")
    }
  })
  
  # ── Standard survey data ───────────────────────────────────────────────────
  filtered_data <- reactive({
    req(input$data_metric != "naa", input$species, input$mode)
    
    data <- fish_data %>%
      filter(species == input$species, mode %in% input$mode)
    
    if (input$time_interval == "annual") {
      req(input$years)
      data <- data %>% filter(year %in% as.numeric(input$years))
    } else {
      req(input$year_wave, input$waves)
      data <- data %>% filter(year == as.numeric(input$year_wave), wave %in% as.numeric(input$waves))
    }
    data
  })
  
  stock_abbrev <- reactive({
    switch(input$species,
           "Atlantic Cod" = "WGOM",
           "Haddock"      = "GOM"
    )
  })
  # ── NAA data selector ─────────────────────────────────────────────────────
  filtered_naa <- reactive({
    req(input$data_metric == "naa", input$species, input$naa_period)
    
    key <- if (input$species == "Atlantic Cod") {
      if (input$naa_period == "historical") "cod_historical" else "cod_projected"
    } else if(input$species == "Haddock") {
      if (input$naa_period == "historical") "haddock_historical" else "haddock_projected"
    } 
    naa_data[[key]]
  })
  
  # ── Plot title ────────────────────────────────────────────────────────────
  output$plot_title <- renderText({
    if (input$data_metric == "naa") {
      req(input$species, input$naa_period)
      period_label <- ifelse(input$naa_period == "historical", "Historical", "Projected")
      paste(paste0( stock_abbrev()), input$species, "\u2014 Numbers-at-Age,", period_label)
    } else {
      metric_label <- switch(input$data_metric,
                             "length" = "Catch-at-Length",
                             "cpue"   = "CPUE (fish per trip)",
                             "weight" = "Average Weight (kg)")
      paste(input$species, "-", metric_label)
    }
  })
  
  # ── Main plot ─────────────────────────────────────────────────────────────
  plot_obj <- reactive({
    
    if (input$data_metric == "naa") {
      req(filtered_naa())
      df <- filtered_naa()
      # Assemble y axis label
      yaxis_label<-glue("{df$metric_parsed[1]} at Age ({df$units[1]})") 
      if (input$naa_period == "historical") {
        # Historical: one line per year, age on x-axis
        n_years     <- length(unique(tail(sort(unique(df$year)), 5)))
        year_colors <- colorRampPalette(c("#C6E6F0", "#0085CA", "#003087"))(n_years)
        
        plot_data <- df %>% mutate(year = factor(year, levels = tail(sort(unique(df$year)), 5))) %>%
          filter(year %in% tail(sort(unique(df$year)), 5))
        
        g <- ggplot(plot_data, aes(x = age, y = value, color = year, group = year)) +
          geom_line(linewidth = 0.7, alpha = 0.8) +
          geom_point(size = 1.5, alpha = 0.8) +
          scale_color_manual(values = year_colors, name = "Year") +
          scale_x_continuous(breaks = sort(unique(df$age)),
                             labels = paste0("Age ", sort(unique(df$age)))) +
          scale_y_continuous(labels = scales::comma) +
          labs(x = "Age", y = yaxis_label) +
          theme_minimal(base_size = 12) +
          theme(legend.position = "right",
                axis.text.x = element_text(angle = 45, hjust = 1))
        
      } else {
       
        plot_data <- df %>%
          mutate(age = factor(paste0("Age ", age),
                              levels = paste0("Age ", sort(unique(df$age)))))
        
        g <- ggplot(plot_data, aes(x = age, y = value)) +
          theme_minimal(base_size = 12) +
          geom_boxplot(fill = "#5EB6D9", color = "#003087", outlier.fill = "#5EB6D9",
                       outlier.alpha = 0.1, outlier.color = "transparent") +
          scale_y_continuous(labels = scales::comma) +
          labs(x = "Age", y = yaxis_label,
               caption = "Boxes show distribution across 500 replicates") +
          
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      
      
    } else {
      req(filtered_data())
      
      time_var   <- if (input$time_interval == "annual") "year" else "wave"
      time_label <- if (input$time_interval == "annual") "Year" else "Wave"
      
      metric_name <- switch(input$data_metric,
                            "length" = "catch_count",
                            "cpue"   = "cpue",
                            "weight" = "weight_kg")
      
      x_label <- switch(input$data_metric,
                        "length" = "Total Catch Count",
                        "cpue"   = "CPUE (fish per trip)",
                        "weight" = "Average Weight (kg)")
      
      plot_data <- filtered_data() %>% dplyr::filter(metric == metric_name)
      max_time  <- max(plot_data[[time_var]], na.rm = TRUE)
      max_data  <- plot_data %>% dplyr::filter(.data[[time_var]] == max_time)
      max_mean  <- mean(max_data$value, na.rm = TRUE)
      
      g <- ggplot(max_data, aes(x = value)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = "#5EB6D9", color = "#0085CA") +
        geom_density(color = "red", fill = NA, linewidth = 0.5) +
        geom_vline(xintercept = max_mean, linetype = "dashed", linewidth = 1) +
        labs(x = x_label, y = "Density",
             title = paste("Distribution for", x_label, time_label, max_time)) +
        theme_minimal()
      
      ggplotly(g)
    }
  })
  
  output$main_plot <- renderPlotly({ plot_obj() })
  
  # ── Summary table ─────────────────────────────────────────────────────────
  output$summary_table <- renderTable({
    if (input$data_metric == "naa") {
      req(filtered_naa())
      df <- filtered_naa()
      
      if (input$naa_period == "historical") {
        df %>%
          group_by(Year = as.integer(year)) %>%
          summarise(
            
            `Age 1`      = scales::comma(round(sum(value[age == 1], na.rm = TRUE), 0)),
            `Age 2`      = scales::comma(round(sum(value[age == 2], na.rm = TRUE), 0)),
            `Age 3`      = scales::comma(round(sum(value[age == 3], na.rm = TRUE), 0)),
            `Age 4`      = scales::comma(round(sum(value[age == 4], na.rm = TRUE), 0)),
            `Age 5`      = scales::comma(round(sum(value[age == 5], na.rm = TRUE), 0)),
            `Age 6`      = scales::comma(round(sum(value[age == 6], na.rm = TRUE), 0)),
            `Age 7`      = scales::comma(round(sum(value[age == 7], na.rm = TRUE), 0)),
            `Age 8`      = scales::comma(round(sum(value[age == 8], na.rm = TRUE), 0)),
            `Age 9`      = scales::comma(round(sum(value[age == 9], na.rm = TRUE), 0)),
            .groups = "drop"
          ) %>% 
          arrange(-Year)
      } else {
        df %>%
          group_by(Year = as.integer(year), age) %>%
          summarise(median_naa = median(value, na.rm = TRUE), .groups = "drop") %>%
          group_by(Year) %>%
          summarise(
            
            `Median Age 1`     = scales::comma(round(sum(median_naa[age == 1]), 0)),
            `Median Age 2`     = scales::comma(round(sum(median_naa[age == 2]), 0)),
            `Median Age 3`     = scales::comma(round(sum(median_naa[age == 3]), 0)),
            `Median Age 4`     = scales::comma(round(sum(median_naa[age == 4]), 0)),
            `Median Age 5`     = scales::comma(round(sum(median_naa[age == 5]), 0)),
            `Median Age 6`     = scales::comma(round(sum(median_naa[age == 6]), 0)),
            `Median Age 7`     = scales::comma(round(sum(median_naa[age == 7]), 0)),
            `Median Age 8`     = scales::comma(round(sum(median_naa[age == 8]), 0)),
            `Median Age 9`     = scales::comma(round(sum(median_naa[age == 9]), 0)),
            .groups = "drop"
          )
      }
    } else {
      metric_name <- switch(input$data_metric,
                            "length" = "catch_count",
                            "cpue"   = "cpue",
                            "weight" = "weight_kg")
      time_var <- if (input$time_interval == "annual") "year" else "wave"
      
      filtered_data() %>%
        filter(metric == metric_name) %>%
        group_by(mode, .data[[time_var]]) %>%
        summarise(Median = round(median(value, na.rm = TRUE), 2),
                  Q25    = round(quantile(value, 0.25, na.rm = TRUE), 2),
                  Q75    = round(quantile(value, 0.75, na.rm = TRUE), 2),
                  N      = n(), .groups = "drop") %>%
        rename(!!time_var := .data[[time_var]])
    }
  })
  
  # ── Downloads ─────────────────────────────────────────────────────────────
  output$download_data <- downloadHandler(
    filename = function() paste0("data_", gsub(" ", "_", input$species), "_", Sys.Date(), ".csv"),
    content  = function(file) {
      if (input$data_metric == "naa") {
        write.csv(filtered_naa(), file, row.names = FALSE)
      } else {
        write.csv(filtered_data(), file, row.names = FALSE)
      }
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() paste0("plot_", gsub(" ", "_", input$species), "_", Sys.Date(), ".png"),
    content  = function(file) plotly::save_image(plot_obj(), file)
  )
  
  # ── Nav observers ─────────────────────────────────────────────────────────
  observeEvent(input$nav_overview, {
    shinyjs::show("overview_panel")
    shinyjs::hide("documentation_panel")
    shinyjs::runjs("
      Shiny.setInputValue('current_tab', 'overview');
      document.querySelectorAll('.nav-link').forEach(el => el.classList.remove('active-nav'));
      document.getElementById('nav_overview').classList.add('active-nav');
      window.dispatchEvent(new Event('resize'));
    ")
  })
  
  observeEvent(input$nav_documentation, {
    shinyjs::hide("overview_panel")
    shinyjs::show("documentation_panel")
    shinyjs::runjs("
      Shiny.setInputValue('current_tab', 'documentation');
      document.querySelectorAll('.nav-link').forEach(el => el.classList.remove('active-nav'));
      document.getElementById('nav_documentation').classList.add('active-nav');
    ")
  })
  
  output$documentation_content <- renderUI({
    doc_path <- switch(input$doc_metric,
                       "length_doc" = "docs/catch-at-length.html", 
                       "naa_cod_doc" = "docs/NAA_cod.html", 
                       "naa_hadddock_doc" = "docs/NAA_haddock.html",
                       "trips_catch_cod_haddock_doc" = "docs/trips_catch_cod_haddock.html")
    tags$iframe(
      src      = doc_path,
      style    = "width: 100%; height: 800px; border: none;",
      seamless = NA
    )
  })
}

shinyApp(ui, server)
