library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

set.seed(123)
# ── Load trip_catch CSV ───────────────────────────────────────────────────────
trip_catch_raw <- tryCatch(
  read.csv(here::here("data/main/trip_catch.csv"), stringsAsFactors = FALSE),
  error = function(e) read.csv("data/main/trip_catch.csv", stringsAsFactors = FALSE)
)

trips_data <- trip_catch_raw %>%
  filter(metric == "directed trips") %>%
  mutate(wave = as.integer(wave), year = as.integer(year),
         mode = dplyr::case_when(
           mode %in% c("charter", "headboat") ~ "for_hire",
           TRUE ~ mode
         )) %>%
  group_by(across(-value)) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

catch_data <- trip_catch_raw %>%
  filter(metric %in% c("harvest", "catch", "discards")) %>%
  mutate(wave = as.integer(wave), year = as.integer(year),
         common = tools::toTitleCase(common),
         mode = dplyr::case_when(
           mode %in% c("charter", "headboat") ~ "for_hire",
           TRUE ~ mode
         )) %>%
  group_by(across(-value)) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

# ── Load NAA RDS files ────────────────────────────────────────────────────────
pivot_naa_long <- function(df) {
  age_cols <- grep("^age\\d+$", names(df), value = TRUE)
  df %>%
    tidyr::pivot_longer(cols = all_of(age_cols),
                        names_to  = "age",
                        values_to = "naa") %>%
    mutate(age = as.integer(sub("age", "", age)))
}

parse_metric_naa <- function(df) {
  df %>%
    mutate(age = as.integer(str_split_i(metric, pattern = " of Age ", -1)),
           metric_parsed = str_split_i(metric, pattern = " of Age ", -2))
}

naa_data <- list(
  cod_historical     = parse_metric_naa(readRDS(here::here("data", "main", "WGOM_Cod_historical_NAA_2026-05-21.Rds"))),
  cod_projected      = parse_metric_naa(readRDS(here::here("data", "main", "WGOM_Cod_projected_NAA_2026-05-21.Rds"))),
  haddock_historical = parse_metric_naa(readRDS(here::here("data", "main", "GOM_Haddock_historical_NAA_2026-05-21.Rds"))),
  haddock_projected  = parse_metric_naa(readRDS(here::here("data", "main", "GOM_Haddock_projected_NAA_2026-05-21.Rds")))
)

# ── Colour palettes ───────────────────────────────────────────────────────────
mode_colors_tc <- c("for_hire" = "#003087",
                    "private"  = "#5EB6D9",
                    "shore"    = "#C6E6F0")

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_fillable(
  theme = bs_theme(
    version   = 5,
    bg        = "#F1F2F3",
    fg        = "#323C46",
    primary   = "#0085CA",
    secondary = "#5EB6D9",
    base_font = font_google("Open Sans")
  ),
  
  shinyjs::useShinyjs(),
  
  tags$input(id = "current_tab", type = "hidden", value = "overview"),
  
  # ── Banner + Nav ────────────────────────────────────────────────────────────
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
          src    = "https://www.fisheries.noaa.gov/themes/custom/noaa_components/images/fisheries_header_logo_jul2019.png",
          height = "50px",
          style  = "display: block; vertical-align: top; margin: 0; padding: 0;"
        ),
        div(
          h3("Recreational Fisheries DST Dashboard",
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
  
  # ── Overview panel ──────────────────────────────────────────────────────────
  div(id = "overview_panel",
      layout_sidebar(
        sidebar = sidebar(
          width = 280,
          style = "background-color: #ffffff; border-right: 1px solid #CBCFD1;",
          
          # Stock selector — hidden for trips/catch metrics
          div(id = "stock_selector",
              div(style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
                  "Stock"),
              selectInput("species", NULL,
                          choices  = c("Atlantic Cod", "Haddock", "Summer Flounder",
                                       "Black Sea Bass", "Scup", "Bluefish"),
                          selected = "Atlantic Cod")
          ),
          
          # Data Metric — now includes Trips and Catch
          div(
            style = "margin-top: 15px;",
            div(style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
                "Data Metric"),
            selectInput("data_metric", NULL,
                        choices = c(
                          "Numbers-at-Age"   = "naa",
                          "Total Trips"      = "trips",
                          "Catch"            = "catch_tc"
                        ),
                        selected = "length")
          ),
          
          # Fishing Mode — for standard fish data metrics
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
          
          # Fishing Mode — for trips/catch CSV metrics
          div(id = "tc_mode_control",
              div(
                style = "margin-top: 15px;",
                div(style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
                    "Fishing Mode"),
                checkboxGroupInput("tc_mode", NULL,
                                   choices  = c("For Hire" = "for_hire", "Private" = "private", "Shore" = "shore"),
                                   selected = c("for_hire", "private", "shore"))
              )
          ),
          
          # Catch metric sub-selector — only shown when data_metric == "catch_tc"
          div(id = "catch_metric_control",
              div(
                style = "margin-top: 15px;",
                div(style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
                    "Catch Type"),
                selectInput("tc_catch_type", NULL,
                            choices  = c("Harvest" = "harvest", "Discards" = "discards", "Total Catch" = "catch"),
                            selected = "harvest")
              )
          ),
          
          # Fishery selector — only for trips/catch metrics
          div(id = "fishery_control",
              div(
                style = "margin-top: 15px;",
                div(style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
                    "Fishery"),
                selectInput("tc_fishery", NULL,
                            choices  = sort(unique(trips_data$fishery)),
                            selected = sort(unique(trips_data$fishery))[1])
              )
          ),
          
          # State selector — mid-atlantic species only
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
          
          # Time Interval
          div(id = "time_interval_control",
              div(
                style = "margin-top: 15px;",
                div(style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
                    "Time Interval"),
                
                # Standard + trips/catch time options
                div(id = "time_standard",
                    radioButtons("time_interval", NULL,
                                 choices  = c("Annual" = "annual", "By Wave (2-month periods)" = "wave"),
                                 selected = "annual"),
                    conditionalPanel(
                      condition = "input.time_interval == 'annual'",
                      uiOutput("year_selector_ui")
                    ),
                    conditionalPanel(
                      condition = "input.time_interval == 'wave'",
                      uiOutput("year_wave_ui"),
                      uiOutput("wave_selector_ui")
                    )
                ),
                
                # NAA period options
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
  
  # ── Documentation panel ─────────────────────────────────────────────────────
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
                selectInput(
                  "doc_metric", NULL,
                  choices  = c("Catch-at-Length"          = "length_doc",
                               "Cod Numbers-at-age"       = "naa_cod_doc",
                               "Haddock Numbers-at-age"   = "naa_haddock_doc",
                               "Directed Trips and Catch" = "trips_catch_cod_haddock_doc"),
                  selected = "length_doc",
                  width    = "100%"
                )
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
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

server <- function(input, output, session) {
  
  shiny::addResourcePath("docs", here::here("docs"))
  
  # ── Sidebar visibility logic ───────────────────────────────────────────────
  observeEvent(input$data_metric, {
    is_naa     <- input$data_metric == "naa"
    is_tc      <- input$data_metric %in% c("trips", "catch_tc")
    is_catch   <- input$data_metric == "catch_tc"
    is_standard <- !is_naa && !is_tc
    
    # Stock selector — hide for trips only; show for catch_tc (limited to cod/haddock)
    if (input$data_metric == "trips") shinyjs::hide("stock_selector") else shinyjs::show("stock_selector")
    
    # Fishing mode controls — mutually exclusive sets
    if (is_tc) {
      shinyjs::hide("fishing_mode_control")
      shinyjs::show("tc_mode_control")
    } else {
      shinyjs::show("fishing_mode_control")
      shinyjs::hide("tc_mode_control")
    }
    
    # Catch type sub-selector — no longer needed (always shows all metrics in chart)
    shinyjs::hide("catch_metric_control")
    
    # Fishery selector — only for trips (catch shows all species across fisheries)
    if (input$data_metric == "trips") {
      shinyjs::show("fishery_control")
      updateSelectInput(session, "tc_fishery",
                        selected = grep("Groundfish|groundfish|NE Groundfish", 
                                        sort(unique(trips_data$fishery)), 
                                        value = TRUE, ignore.case = TRUE)[1] %||%
                          sort(unique(trips_data$fishery))[1])
    } else {
      shinyjs::hide("fishery_control")
    }
    
    # Time interval: NAA vs standard/tc
    if (is_naa) {
      shinyjs::hide("time_standard")
      shinyjs::show("time_naa")
    } else {
      shinyjs::show("time_standard")
      shinyjs::hide("time_naa")
    }
    
    # Species choices — NAA and catch_tc limited to cod/haddock; standard gets all
    if (is_naa || is_catch) {
      updateSelectInput(session, "species",
                        choices  = c("Atlantic Cod", "Haddock"),
                        selected = if (input$species %in% c("Atlantic Cod", "Haddock")) input$species else "Atlantic Cod")
    } else if (is_standard) {
      updateSelectInput(session, "species",
                        choices  = c("Atlantic Cod", "Haddock", "Summer Flounder",
                                     "Black Sea Bass", "Scup", "Bluefish"),
                        selected = input$species)
    }
  })
  
  # ── Dynamic year/wave selectors (shared by standard + trips/catch) ──────────
  # Derive available years from whichever dataset is active
  active_years <- reactive({
    if (input$data_metric %in% c("trips")) {
      sort(unique(trips_data$year))
    } else if (input$data_metric == "catch_tc") {
      sort(unique(catch_data$year))
    } else {
      2020:2023
    }
  })
  
  output$year_selector_ui <- renderUI({
    yrs <- active_years()
    checkboxGroupInput("years", "Select Years:", choices = yrs, selected = yrs)
  })
  
  output$year_wave_ui <- renderUI({
    yrs <- active_years()
    selectInput("year_wave", "Select Year:", choices = yrs, selected = max(yrs))
  })
  
  output$wave_selector_ui <- renderUI({
    if (input$data_metric %in% c("trips", "catch_tc")) {
      # Determine available waves for selected year
      df <- if (input$data_metric == "trips") trips_data else catch_data
      req(input$year_wave)
      avail_waves <- sort(unique(df$wave[df$year == as.integer(input$year_wave)]))
      wave_names  <- setNames(avail_waves, paste("Wave", avail_waves))
      checkboxGroupInput("waves", "Select Waves:", choices = wave_names, selected = avail_waves)
    } else {
      checkboxGroupInput("waves", "Select Waves:",
                         choices  = setNames(1:6, paste("Wave", 1:6,
                                                        c("(Jan-Feb)", "(Mar-Apr)", "(May-Jun)",
                                                          "(Jul-Aug)", "(Sep-Oct)", "(Nov-Dec)"))),
                         selected = 1:6)
    }
  })
  
  # ── NAA reactive ───────────────────────────────────────────────────────────
  stock_abbrev <- reactive({
    switch(input$species, "Atlantic Cod" = "WGOM", "Haddock" = "GOM")
  })
  
  filtered_naa <- reactive({
    req(input$data_metric == "naa", input$species, input$naa_period)
    key <- paste0(if (input$species == "Atlantic Cod") "cod" else "haddock",
                  "_", input$naa_period)
    naa_data[[key]]
  })
  
  # ── Trips reactive ─────────────────────────────────────────────────────────
  filtered_trips <- reactive({
    req(input$data_metric == "trips", input$tc_mode, input$tc_fishery)
    df <- trips_data %>%
      filter(fishery %in% input$tc_fishery, mode %in% input$tc_mode)
    
    if (input$time_interval == "annual") {
      req(input$years)
      df <- df %>% filter(year %in% as.numeric(input$years))
    } else {
      req(input$year_wave, input$waves)
      df <- df %>% filter(year == as.numeric(input$year_wave), wave %in% as.numeric(input$waves))
    }
    df
  })
  
  # ── Catch (CSV) reactive ───────────────────────────────────────────────────
  filtered_catch_tc <- reactive({
    req(input$data_metric == "catch_tc", input$tc_mode, input$species)
    
    # Map species input to the common name used in catch_data
    species_map <- c("Atlantic Cod" = "Atlanticcod", "Haddock" = "Haddock")
    selected_common <- species_map[[input$species]]
    
    df <- catch_data %>%
      filter(mode %in% input$tc_mode, common == selected_common)
    if (input$time_interval == "annual") {
      req(input$years)
      df <- df %>% filter(year %in% as.numeric(input$years))
    } else {
      req(input$year_wave, input$waves)
      df <- df %>% filter(year == as.numeric(input$year_wave), wave %in% as.numeric(input$waves))
    }
    df
  })
  
  # ── Plot title ─────────────────────────────────────────────────────────────
  output$plot_title <- renderText({
    switch(input$data_metric,
           "naa" = {
             req(input$species, input$naa_period)
             paste(stock_abbrev(), input$species, "\u2014 Numbers-at-Age,",
                   ifelse(input$naa_period == "historical", "Historical", "Projected"))
           },
           "trips" = paste("Directed Trips \u2014", input$tc_fishery),
           "catch_tc" = {
             req(input$species)
             paste(input$species, "\u2014 Catch")
           },
           {
             metric_label <- switch(input$data_metric,
                                    "length" = "Catch-at-Length",
                                    "cpue"   = "CPUE (fish per trip)",
                                    "weight" = "Average Weight (kg)")
             paste(input$species, "-", metric_label)
           }
    )
  })
  
  # ── Main plot ──────────────────────────────────────────────────────────────
  plot_obj <- reactive({
    
    # ── NAA ──
    if (input$data_metric == "naa") {
      req(filtered_naa())
      df         <- filtered_naa()
      yaxis_label <- glue::glue("{df$metric_parsed[1]} at Age ({df$units[1]})")
      
      if (input$naa_period == "historical") {
        n_years     <- length(unique(tail(sort(unique(df$year)), 5)))
        year_colors <- colorRampPalette(c("#C6E6F0", "#0085CA", "#003087"))(n_years)
        plot_data   <- df %>%
          mutate(year = factor(year, levels = tail(sort(unique(df$year)), 5))) %>%
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
          theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))
        
      } else {
        plot_data <- df %>%
          mutate(age = factor(paste0("Age ", age), levels = paste0("Age ", sort(unique(df$age)))))
        
        g <- ggplot(plot_data, aes(x = age, y = value)) +
          geom_boxplot(fill = "#5EB6D9", color = "#003087",
                       outlier.fill = "#5EB6D9", outlier.alpha = 0.1, outlier.color = "transparent") +
          scale_y_continuous(labels = scales::comma) +
          labs(x = "Age", y = yaxis_label, caption = "Boxes show distribution across 500 replicates") +
          theme_minimal(base_size = 12) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      return(ggplotly(g))
      
      # ── Trips ──
    } else if (input$data_metric == "trips") {
      req(filtered_trips())
      df    <- filtered_trips()
      grp   <- if (input$time_interval == "annual") "year" else "wave"
      y_lab <- if (nrow(df) > 0) df$units[1] else "number of trips"
      
      plot_data <- df %>%
        group_by(mode, x_val = !!sym(grp)) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(x_label = if (grp == "wave") paste0("Wave ", x_val) else as.character(x_val),
               x_label = factor(x_label, levels = unique(x_label[order(x_val)])),
               mode    = factor(mode, levels = c("for_hire", "private", "shore"),
                                labels = c("For Hire", "Private", "Shore")))
      
      g <- ggplot(plot_data,
                  aes(x = x_label, y = total, fill = mode,
                      text = paste0("Mode: ", mode,
                                    "<br>", tools::toTitleCase(grp), ": ", x_label,
                                    "<br>Value: ", scales::comma(round(total, 0))))) +
        geom_col(position = "dodge", width = 0.7) +
        scale_fill_manual(values = c("For Hire" = "#003087", "Private" = "#5EB6D9", "Shore" = "#C6E6F0"),
                          name = "Mode") +
        scale_y_continuous(labels = scales::comma) +
        labs(x = tools::toTitleCase(grp), y = y_lab) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "right")
      
      return(ggplotly(g, tooltip = "text"))
      
      # ── Catch (CSV) ──
    } else if (input$data_metric == "catch_tc") {
      req(filtered_catch_tc())
      df    <- filtered_catch_tc()
      grp   <- if (input$time_interval == "annual") "year" else "wave"
      y_lab <- if (nrow(df) > 0) df$units[1] else "number of fish"
      
      mode_label <- function(m) dplyr::case_when(
        m == "for_hire" ~ "For Hire", m == "private" ~ "Private",
        m == "shore" ~ "Shore", TRUE ~ tools::toTitleCase(m)
      )
      
      # Stacked bars: harvest + discards summed across selected modes
      bars <- df %>%
        filter(metric %in% c("harvest", "discards")) %>%
        group_by(x_val = .data[[grp]], metric) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(x_label = if (grp == "wave") paste0("Wave ", x_val) else as.character(x_val),
               x_label = factor(x_label, levels = unique(x_label[order(x_val)])),
               metric  = factor(tools::toTitleCase(metric),
                                levels = c("Harvest", "Discards")))
      
      # Points: total catch summed across selected modes
      pts <- df %>%
        filter(metric == "catch") %>%
        group_by(x_val = .data[[grp]]) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(x_label = if (grp == "wave") paste0("Wave ", x_val) else as.character(x_val),
               x_label = factor(x_label, levels = levels(bars$x_label)))
      
      g <- ggplot() +
        geom_col(data = bars,
                 aes(x = x_label, y = total, fill = metric,
                     text = paste0(metric, "<br>",
                                   tools::toTitleCase(grp), ": ", x_label,
                                   "<br>Value: ", scales::comma(round(total, 0)))),
                 position = "stack", width = 0.65) +
        geom_point(data = pts,
                   aes(x = x_label, y = total,
                       text = paste0("Total Catch<br>",
                                     tools::toTitleCase(grp), ": ", x_label,
                                     "<br>Value: ", scales::comma(round(total, 0)))),
                   shape = 21, size = 3.5, fill = "white", color = "#323C46", stroke = 1.2) +
        scale_fill_manual(values = c("Harvest" = "#003087", "Discards" = "#5EB6D9"),
                          name = NULL) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = tools::toTitleCase(grp), y = y_lab,
             caption = "Bars = Harvest + Discards; Points = Total Catch") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "right")
      
      return(ggplotly(g, tooltip = "text"))
    } 
  })
  
  output$main_plot <- renderPlotly({ plot_obj() })
  
  # ── Summary table ──────────────────────────────────────────────────────────
  output$summary_table <- renderTable({
    
    if (input$data_metric == "naa") {
      req(filtered_naa())
      df <- filtered_naa()
      if (input$naa_period == "historical") {
        df %>%
          group_by(Year = as.integer(year)) %>%
          summarise(across(c(), ~ NULL),
                    `Age 1` = scales::comma(round(sum(value[age == 1], na.rm = TRUE), 0)),
                    `Age 2` = scales::comma(round(sum(value[age == 2], na.rm = TRUE), 0)),
                    `Age 3` = scales::comma(round(sum(value[age == 3], na.rm = TRUE), 0)),
                    `Age 4` = scales::comma(round(sum(value[age == 4], na.rm = TRUE), 0)),
                    `Age 5` = scales::comma(round(sum(value[age == 5], na.rm = TRUE), 0)),
                    `Age 6` = scales::comma(round(sum(value[age == 6], na.rm = TRUE), 0)),
                    `Age 7` = scales::comma(round(sum(value[age == 7], na.rm = TRUE), 0)),
                    `Age 8` = scales::comma(round(sum(value[age == 8], na.rm = TRUE), 0)),
                    `Age 9` = scales::comma(round(sum(value[age == 9], na.rm = TRUE), 0)),
                    .groups = "drop") %>%
          arrange(-Year)
      } else {
        df %>%
          group_by(Year = as.integer(year), age) %>%
          summarise(median_naa = median(value, na.rm = TRUE), .groups = "drop") %>%
          group_by(Year) %>%
          summarise(
            `Median Age 1` = scales::comma(round(sum(median_naa[age == 1]), 0)),
            `Median Age 2` = scales::comma(round(sum(median_naa[age == 2]), 0)),
            `Median Age 3` = scales::comma(round(sum(median_naa[age == 3]), 0)),
            `Median Age 4` = scales::comma(round(sum(median_naa[age == 4]), 0)),
            `Median Age 5` = scales::comma(round(sum(median_naa[age == 5]), 0)),
            `Median Age 6` = scales::comma(round(sum(median_naa[age == 6]), 0)),
            `Median Age 7` = scales::comma(round(sum(median_naa[age == 7]), 0)),
            `Median Age 8` = scales::comma(round(sum(median_naa[age == 8]), 0)),
            `Median Age 9` = scales::comma(round(sum(median_naa[age == 9]), 0)),
            .groups = "drop")
      }
      
    } else if (input$data_metric == "trips") {
      req(filtered_trips())
      filtered_trips() %>%
        mutate(Mode = dplyr::case_when(
          mode == "for_hire" ~ "For Hire",
          mode == "private"  ~ "Private",
          mode == "shore"    ~ "Shore",
          TRUE ~ tools::toTitleCase(mode)
        )) %>%
        group_by(Year = year, Wave = wave, Mode) %>%
        summarise(Total = scales::comma(round(sum(value, na.rm = TRUE), 0)),
                  .groups = "drop") %>%
        arrange(Year, Wave, Mode)
      
    } else if (input$data_metric == "catch_tc") {
      req(filtered_catch_tc())
      filtered_catch_tc() %>%
        mutate(Mode = dplyr::case_when(
          mode == "for_hire" ~ "For Hire",
          mode == "private"  ~ "Private",
          mode == "shore"    ~ "Shore",
          TRUE ~ tools::toTitleCase(mode)
        )) %>%
        group_by(Year = year, Wave = wave, Mode) %>%
        summarise(
          Harvest      = scales::comma(round(sum(value[metric == "harvest"],  na.rm = TRUE), 0)),
          Discards     = scales::comma(round(sum(value[metric == "discards"], na.rm = TRUE), 0)),
          `Total Catch` = scales::comma(round(sum(value[metric == "catch"],   na.rm = TRUE), 0)),
          .groups = "drop"
        ) %>%
        arrange(Year, Wave, Mode)
      
    } else {
      metric_name <- switch(input$data_metric,
                            "length" = "catch_count", "cpue" = "cpue", "weight" = "weight_kg")
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
  
  # ── Downloads ──────────────────────────────────────────────────────────────
  output$download_data <- downloadHandler(
    filename = function() paste0("data_", gsub(" ", "_", input$data_metric), "_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- switch(input$data_metric,
                   "naa"      = filtered_naa(),
                   "trips"    = filtered_trips(),
                   "catch_tc" = filtered_catch_tc(),
                   filtered_data())
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() paste0("plot_", gsub(" ", "_", input$data_metric), "_", Sys.Date(), ".png"),
    content  = function(file) plotly::save_image(plot_obj(), file)
  )
  
  # ── Nav observers ──────────────────────────────────────────────────────────
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
                       "length_doc"                  = "docs/catch-at-length.html",
                       "naa_cod_doc"                 = "docs/NAA_cod.html",
                       "naa_haddock_doc"             = "docs/NAA_haddock.html",
                       "trips_catch_cod_haddock_doc" = "docs/trips_catch_cod_haddock.html")
    tags$iframe(src = doc_path, style = "width: 100%; height: 800px; border: none;", seamless = NA)
  })
}

shinyApp(ui, server)
