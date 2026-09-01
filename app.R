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

# ── Load Trip/Catch RDS for Summer Flounder, Black Sea Bass & Scup ────────────
# NEW SPECIES: adds these three mid-Atlantic stocks (fishery = "SFSBSB") to the
# existing trips/catch metrics. Unlike the cod/haddock CSV, this source is
# state-level (has a "state" column), so it powers the State checkbox filter.
# Modes (charter/headboat/private/shore) are mapped to the same
# for_hire/private/shore vocabulary used everywhere else, and common names are
# title-cased to match the existing species_map convention
# ("Summerflounder"/"Blackseabass"/"Scup").
trip_catch_sfsbsb_raw <- tryCatch(
  readRDS(here::here("data", "main", "trip_catch_sfsbsb2026-08-25.Rds")),
  error = function(e) readRDS("data/main/trip_catch_sfsbsb2026-08-25.Rds")
)

trip_catch_sfsbsb <- trip_catch_sfsbsb_raw %>%
  dplyr::ungroup() %>%
  mutate(wave = as.integer(wave), year = as.integer(year),
         common = tools::toTitleCase(common),
         mode = dplyr::case_when(
           mode %in% c("charter", "headboat") ~ "for_hire",
           TRUE ~ mode
         ))

trips_data_sfsbsb <- trip_catch_sfsbsb %>%
  filter(metric == "directed trips") %>%
  group_by(across(-value)) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

catch_data_sfsbsb <- trip_catch_sfsbsb %>%
  filter(metric %in% c("harvest", "catch", "discards")) %>%
  group_by(across(-value)) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

# Align shared-column types before binding. The CSV-derived trips_data/catch_data
# never touch data_version, so it stays character from read.csv(), while the RDS
# gives data_version as class Date — bind_rows() errors on that mismatch. This
# coerces any shared column on the new side to match the old side's type, so it's
# robust to that (and any similar) mismatch without needing to know the CSV's
# exact schema.
align_types <- function(new_df, ref_df) {
  for (col in intersect(names(new_df), names(ref_df))) {
    ref_class <- class(ref_df[[col]])[1]
    if (!identical(class(new_df[[col]])[1], ref_class)) {
      new_df[[col]] <- switch(ref_class,
        "Date"      = as.Date(new_df[[col]]),
        "character" = as.character(new_df[[col]]),
        "numeric"   = as.numeric(new_df[[col]]),
        "double"    = as.double(new_df[[col]]),
        "integer"   = as.integer(new_df[[col]]),
        "factor"    = as.character(new_df[[col]]),
        new_df[[col]]
      )
    }
  }
  new_df
}

trips_data_sfsbsb <- align_types(trips_data_sfsbsb, trips_data)
catch_data_sfsbsb <- align_types(catch_data_sfsbsb, catch_data)

trips_data <- dplyr::bind_rows(trips_data, trips_data_sfsbsb)
catch_data <- dplyr::bind_rows(catch_data, catch_data_sfsbsb)

# Friendlier display label for the SFSBSB fishery code in the Fishery dropdown
# (underlying value passed to the server stays "SFSBSB").
fishery_display_labels <- function(fishery_values) {
  setNames(fishery_values,
           ifelse(fishery_values == "SFSBSB",
                  "Summer Flounder / Black Sea Bass / Scup",
                  fishery_values))
}

# Shared species -> common-name-key map for the catch/trips (MRIP) metrics,
# now covering cod/haddock (CSV) plus SF/BSB/Scup (state-level RDS above).
catch_tc_species_map <- c(
  "Atlantic Cod"     = "Atlanticcod",
  "Haddock"          = "Haddock",
  "Summer Flounder"  = "Summerflounder",
  "Black Sea Bass"   = "Blackseabass",
  "Scup"             = "Scup"
)

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
  # Handles both the older cod/haddock convention ("Numbers of Age 1") and the
  # newer species files ("2024 Numbers at Age 1" / "2026 Projected Numbers At Age 1")
  # — age is just the trailing number, and metric_parsed strips any leading year
  # and trailing "of/at Age N" (case-insensitive, since capitalization varies).
  df %>%
    mutate(
      age           = as.integer(str_extract(metric, "\\d+$")),
      metric_parsed = metric %>%
        str_remove("^\\d{4}\\s+") %>%
        str_remove(stringr::regex("\\s+(of|at)\\s+Age\\s*\\d+$", ignore_case = TRUE))
    )
}

naa_data <- list(
  cod_historical     = parse_metric_naa(readRDS(here::here("data", "main", "WGOM_Cod_historical_NAA_2026-06-16.Rds"))),
  cod_projected      = parse_metric_naa(readRDS(here::here("data", "main", "WGOM_Cod_projected_NAA_2026-06-16.Rds"))),
  haddock_historical = parse_metric_naa(readRDS(here::here("data", "main", "GOM_Haddock_historical_NAA_2026-06-16.Rds"))),
  haddock_projected  = parse_metric_naa(readRDS(here::here("data", "main", "GOM_Haddock_projected_NAA_2026-06-16.Rds"))),
  
  bsb_n_historical   = parse_metric_naa(readRDS(here::here("data", "main", "BlackSeaBassNorth_historicalNAA_2026_07_30.Rds"))),
  bsb_n_projected    = parse_metric_naa(readRDS(here::here("data", "main", "BlackSeaBassNorth_projectedNAA_2026_07_30.Rds"))),
  bsb_s_historical   = parse_metric_naa(readRDS(here::here("data", "main", "BlackSeaBassSouth_historicalNAA_2026_07_30.Rds"))),
  bsb_s_projected    = parse_metric_naa(readRDS(here::here("data", "main", "BlackSeaBassSouth_projectedNAA_2026_07_30.Rds"))), 
  
  scup_historical   = parse_metric_naa(readRDS(here::here("data", "main", "Scup_historicalNAA_2026_07_30.Rds"))),
  scup_projected    = parse_metric_naa(readRDS(here::here("data", "main", "Scup_projectedNAA_2026_07_30.Rds"))),
  sf_historical     = parse_metric_naa(readRDS(here::here("data", "main", "SummerFlounder_historicalNAA_2026_07_30.Rds"))),
  sf_projected      = parse_metric_naa(readRDS(here::here("data", "main", "SummerFlounder_projectedNAA_2026_07_30.Rds")))
)

# ── Load Catch-per-Trip RDS ───────────────────────────────────────────────────
# NEW METRIC: leaves all other metrics untouched. Modes are mapped to the same
# for_hire/private/shore vocabulary used by the trips/catch controls, and common
# names are title-cased to match the existing species_map ("Atlanticcod"/"Haddock").
cpt_raw <- tryCatch(
  readRDS(here::here("data", "main", "catch_per_trip_2026-06-15.Rds")),
  error = function(e) readRDS("data/main/catch_per_trip_2026-06-15.Rds")
)

cpt_data <- cpt_raw %>%
  mutate(
    year   = as.integer(year),
    wave   = as.integer(wave),
    month  = as.integer(month),
    common = tools::toTitleCase(common),
    mode   = dplyr::case_when(
      mode %in% c("fh", "charter", "headboat") ~ "for_hire",
      mode == "pr" ~ "private",
      mode == "sh" ~ "shore",
      TRUE ~ mode
    )
  )

# ── Load Catch-at-Length RDS ──────────────────────────────────────────────────
# NEW METRIC: metric column encodes "<season> <length>" (e.g. "summer 6.7"), so
# season and the numeric length (used for the x-axis) are parsed out here. The
# "units" column distinguishes projected vs. baseline (fitted/observed) values;
# common names are title-cased to match the existing species_map ("Atlanticcod"/"Haddock").
catch_len_raw <- tryCatch(
  readRDS(here::here("data", "main", "catch_at_len_2026-08-13.Rds")),
  error = function(e) readRDS("data/main/catch_at_len_2026-08-13.Rds")
)

catch_len_data <- catch_len_raw %>%
  mutate(
    common = tools::toTitleCase(common),
    season = stringr::str_extract(metric, "^[A-Za-z]+"),
    length = as.numeric(stringr::str_extract(metric, "[0-9.]+$"))
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
          
          # Data Metric — now includes Trips, Catch, and Catch per Trip
          div(
            style = "margin-top: 15px;",
            div(style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
                "Data Metric"),
            selectInput("data_metric", NULL,
                        choices = c(
                          "Numbers at Age - Stock Assessment"    = "naa",
                          "Directed Trips - MRIP"                = "trips",
                          "Catch - MRIP"                         = "catch_tc",
                          "Catch per trip - model intermediate"  = "cpt",
                          "Catch-at-Length - model intermediate" = "catch_len"
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
                                   choices  = c("Shore", "Private", "For Hire"),
                                   selected = c("Shore", "Private", "For Hire"))
              )
          ),
          
          # Fishing Mode — for trips/catch/catch-per-trip CSV metrics
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
          
          # Season — only shown when data_metric == "catch_len"
          div(id = "season_control",
              div(
                style = "margin-top: 15px;",
                div(style = "background-color: #003087; color: white; padding: 8px 12px; margin: -10px -10px 10px -10px; font-weight: 600; font-size: 11px; text-transform: uppercase; letter-spacing: 0.03em;",
                    "Season"),
                checkboxGroupInput("catch_len_season", NULL,
                                   choices  = c("Summer" = "summer", "Winter" = "winter"),
                                   selected = c("summer", "winter"))
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
                            choices  = fishery_display_labels(sort(unique(trips_data$fishery))),
                            selected = sort(unique(trips_data$fishery))[1])
              )
          ),
          
          # State selector — mid-atlantic species only (Catch: species-driven;
          # Trips: driven by the SFSBSB fishery selection instead, since Trips
          # has no species dimension of its own)
          conditionalPanel(
            condition = "(input.data_metric == 'catch_tc' && (input.species == 'Summer Flounder' || input.species == 'Black Sea Bass' || input.species == 'Scup')) || (input.data_metric == 'trips' && input.tc_fishery == 'SFSBSB')",
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
                ),
                
                # Catch-per-trip time options — explore by month (and by mode via the
                # Fishing Mode control above). Year + month pickers are data-driven.
                shinyjs::hidden(
                  div(id = "time_cpt",
                      uiOutput("cpt_year_ui"),
                      uiOutput("cpt_month_ui")
                  )
                )
              )
          ),
          
          # Download data — bottom of sidebar. Downloads exactly what's shown in the
          # table on the right, with the table's title used as the CSV filename.
          div(
            style = "margin-top: 20px; padding-top: 15px; border-top: 1px solid #CBCFD1;",
            downloadButton("download_data", "Download Data",
                           style = "background-color: #0085CA; border: none; color: white; font-size: 13px; border-radius: 3px; padding: 6px 12px; width: 100%;")
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
            # ── CHANGED: use separate table_title output instead of reusing plot_title ──
            card_header(textOutput("table_title"),
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
                  choices  = c("Cod Numbers at age"       = "naa_cod_doc",
                               "Haddock Numbers at age"   = "naa_haddock_doc",
                               "Black Sea Bass Numbers at age"   = "naa_bsb_doc",
                               "Summer Flounder Numbers at age"  = "naa_sf_doc",
                               "Scup Numbers at age"             = "naa_scup_doc",
                               "Groundfish Directed Trips and Catch" = "trips_catch_cod_haddock_doc", 
                               "SFBSBS Directed Trips and Catch" = "trips_catch_sfbsbs_doc", 
                               "Catch per trip"           = "cpt_cod_haddock_doc",
                               "Catch at length"          = "catch_at_len_doc"),
                  selected = "catch_at_len_doc",
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
    is_naa       <- input$data_metric == "naa"
    is_cpt       <- input$data_metric == "cpt"
    is_catch_len <- input$data_metric == "catch_len"
    # tc_mode_control (for_hire/private/shore) is shared by trips, catch, and cpt
    is_tc       <- input$data_metric %in% c("trips", "catch_tc", "cpt")
    # species limited to cod/haddock for catch, catch-per-trip, and catch-at-length
    is_catch    <- input$data_metric %in% c("catch_tc", "cpt", "catch_len")
    is_standard <- !is_naa && !is_tc && !is_catch_len
    
    # Stock selector — hide for trips only; show for catch_tc / cpt / catch_len (cod/haddock)
    if (input$data_metric == "trips") shinyjs::hide("stock_selector") else shinyjs::show("stock_selector")
    
    # Fishing mode controls — mutually exclusive sets (catch_len has no mode dimension, so hide both)
    if (is_tc) {
      shinyjs::hide("fishing_mode_control")
      shinyjs::show("tc_mode_control")
    } else if (is_naa || is_catch_len) {
      shinyjs::hide("fishing_mode_control")
      shinyjs::hide("tc_mode_control")
    } else {
      shinyjs::show("fishing_mode_control")
      shinyjs::hide("tc_mode_control")
    }
    
    # Season selector — only for catch_len
    if (is_catch_len) shinyjs::show("season_control") else shinyjs::hide("season_control")
    
    # Catch type sub-selector — no longer needed (always shows all metrics in chart)
    shinyjs::hide("catch_metric_control")
    
    # Fishery selector — only for trips (catch / cpt / catch_len show all species across fisheries)
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
    
    # Time interval: catch_len doesn't use it at all (baseline vs. projected panels are
    # fixed, and time is filtered via Season instead), so hide the whole control for it.
    if (is_catch_len) {
      shinyjs::hide("time_interval_control")
    } else {
      shinyjs::show("time_interval_control")
      if (is_naa) {
        shinyjs::hide("time_standard"); shinyjs::show("time_naa");  shinyjs::hide("time_cpt")
      } else if (is_cpt) {
        shinyjs::hide("time_standard"); shinyjs::hide("time_naa");  shinyjs::show("time_cpt")
      } else {
        shinyjs::show("time_standard"); shinyjs::hide("time_naa");  shinyjs::hide("time_cpt")
      }
    }
    
    # Species choices — NAA now covers all six assessed stocks; catch / catch-per-trip /
    # catch-at-length (MRIP- and model-derived metrics) stay limited to cod/haddock;
    # standard catch-at-length gets all species.
    naa_species_choices <- c("Atlantic Cod", "Haddock", "Black Sea Bass (North)",
                             "Black Sea Bass (South)", "Scup", "Summer Flounder")
    if (is_naa) {
      updateSelectInput(session, "species",
                        choices  = naa_species_choices,
                        selected = if (input$species %in% naa_species_choices) input$species else "Atlantic Cod")
    } else if (input$data_metric == "catch_tc") {
      # Catch (MRIP) now covers cod/haddock plus the three SFSBSB stocks
      catch_tc_choices <- names(catch_tc_species_map)
      updateSelectInput(session, "species",
                        choices  = catch_tc_choices,
                        selected = if (input$species %in% catch_tc_choices) input$species else "Atlantic Cod")
    } else if (is_cpt || is_catch_len) {
      # Catch-per-trip and Catch-at-Length (model-derived) remain cod/haddock only
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
  active_years <- reactive({
    if (input$data_metric == "trips") {
      req(input$tc_fishery)
      sort(unique(trips_data$year[trips_data$fishery == input$tc_fishery]))
    } else if (input$data_metric == "catch_tc") {
      req(input$species)
      selected_common <- catch_tc_species_map[[input$species]]
      sort(unique(catch_data$year[catch_data$common == selected_common]))
    } else {
      2020:2023
    }
  })
  
  output$year_selector_ui <- renderUI({
    yrs <- active_years()
    checkboxGroupInput("years", "Select Years:", choices = yrs, selected = yrs)
  })
  
  output$wave_selector_ui <- renderUI({
    if (input$data_metric %in% c("trips", "catch_tc")) {
      df <- if (input$data_metric == "trips") {
        req(input$tc_fishery)
        trips_data %>% filter(fishery == input$tc_fishery)
      } else {
        req(input$species)
        selected_common <- catch_tc_species_map[[input$species]]
        catch_data %>% filter(common == selected_common)
      }
      avail_waves <- sort(unique(df$wave))
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
  
  # ── Catch-per-trip year/month selectors (data-driven) ──────────────────────
  output$cpt_year_ui <- renderUI({
    yrs <- sort(unique(cpt_data$year))
    checkboxGroupInput("cpt_years", "Select Years:", choices = yrs, selected = yrs)
  })
  
  output$cpt_month_ui <- renderUI({
    mos <- sort(unique(cpt_data$month))
    checkboxGroupInput("cpt_months", "Select Months:",
                       choices  = setNames(mos, month.name[mos]),
                       selected = mos)
  })
  
  # ── NAA reactive ───────────────────────────────────────────────────────────
  naa_species_key_map <- c(
    "Atlantic Cod"           = "cod",
    "Haddock"                = "haddock",
    "Black Sea Bass (North)" = "bsb_n",
    "Black Sea Bass (South)" = "bsb_s",
    "Scup"                   = "scup",
    "Summer Flounder"        = "sf"
  )

  stock_abbrev <- reactive({
    switch(input$species,
           "Atlantic Cod"           = "WGOM",
           "Haddock"                = "GOM",
           "Black Sea Bass (North)" = "North",
           "Black Sea Bass (South)" = "South",
           "Scup"                   = "",
           "Summer Flounder"        = "")
  })
  
  filtered_naa <- reactive({
    req(input$data_metric == "naa", input$species, input$naa_period)
    key <- paste0(naa_species_key_map[[input$species]], "_", input$naa_period)
    naa_data[[key]]
  })
  
  # ── Trips reactive ─────────────────────────────────────────────────────────
  filtered_trips <- reactive({
    req(input$data_metric == "trips", input$tc_mode, input$tc_fishery)
    df <- trips_data %>%
      filter(fishery %in% input$tc_fishery, mode %in% input$tc_mode)
    
    # State filter only applies to the state-level SFSBSB fishery; other
    # fisheries have no state dimension (state is NA there).
    if (input$tc_fishery == "SFSBSB") {
      req(input$state)
      df <- df %>% filter(state %in% input$state)
    }
    
    if (input$time_interval == "annual") {
      req(input$years)
      df <- df %>% filter(year %in% as.numeric(input$years))
    } else {
      req(input$waves, input$years)
      df <- df %>% filter(year %in% as.numeric(input$years), wave %in% as.numeric(input$waves))
    }
    df
  })
  
  # ── Catch (CSV) reactive ───────────────────────────────────────────────────
  filtered_catch_tc <- reactive({
    req(input$data_metric == "catch_tc", input$tc_mode, input$species)
    
    selected_common <- catch_tc_species_map[[input$species]]
    
    df <- catch_data %>%
      filter(mode %in% input$tc_mode, common == selected_common)
    
    # State filter only applies to the state-level SFSBSB stocks; cod/haddock
    # rows have no state dimension (state is NA there).
    if (input$species %in% c("Summer Flounder", "Black Sea Bass", "Scup")) {
      req(input$state)
      df <- df %>% filter(state %in% input$state)
    }
    
    if (input$time_interval == "annual") {
      req(input$years)
      df <- df %>% filter(year %in% as.numeric(input$years))
    } else {
      req(input$waves, input$years)
      df <- df %>% filter(year %in% as.numeric(input$years), wave %in% as.numeric(input$waves))
    }
    df
  })
  
  # ── Catch-per-trip reactive ────────────────────────────────────────────────
  filtered_cpt <- reactive({
    req(input$data_metric == "cpt", input$tc_mode, input$species,
        input$cpt_years, input$cpt_months)
    
    species_map <- c("Atlantic Cod" = "Atlanticcod", "Haddock" = "Haddock")
    selected_common <- species_map[[input$species]]
    
    cpt_data %>%
      filter(common == selected_common,
             mode  %in% input$tc_mode,
             year  %in% as.numeric(input$cpt_years),
             month %in% as.numeric(input$cpt_months))
  })
  
  # ── Catch-at-Length reactive ───────────────────────────────────────────────
  filtered_catch_len <- reactive({
    req(input$data_metric == "catch_len", input$species, input$catch_len_season)
    
    species_map <- c("Atlantic Cod" = "Atlanticcod", "Haddock" = "Haddock")
    selected_common <- species_map[[input$species]]
    
    catch_len_data %>%
      filter(common == selected_common, season %in% input$catch_len_season)
  })
  
  # ── Plot title reactive (shared by both headers) ───────────────────────────
  # ── CHANGED: extracted into a reactive so both plot and table headers
  #             can reference the same logic without duplicating code ──────────
  plot_title_text <- reactive({
    switch(input$data_metric,
           "naa" = {
             req(input$species, input$naa_period)
             stringr::str_squish(
               paste(stock_abbrev(), input$species, "\u2014 Numbers-at-Age,",
                     ifelse(input$naa_period == "historical", "Historical", "Projected"))
             )
           },
           "trips" = paste("Directed Trips ", names(fishery_display_labels(input$tc_fishery))),
           "catch_tc" = {
             req(input$species)
             paste(input$species, " Catch")
           },
           "cpt" = {
             req(input$species)
             paste(input$species, "Catch per Trip")
           },
           "catch_len" = {
             req(input$species)
             paste(input$species, "Catch-at-Length (Projected vs. Baseline)")
           },
           {
             metric_label <- switch(input$data_metric,
                                    "length" = "Catch at Length",
                                    "cpue"   = "CPUE (fish per trip)",
                                    "weight" = "Average Weight (kg)")
             paste(input$species, "-", metric_label)
           }
    )
  })
  
  # ── CHANGED: both outputs draw from the same reactive ─────────────────────
  output$plot_title  <- renderText({ plot_title_text() })
  output$table_title <- renderText({ plot_title_text() })
  
  # ── Main plot ──────────────────────────────────────────────────────────────
  plot_obj <- reactive({
    
    # ── NAA ──
    if (input$data_metric == "naa") {
      req(filtered_naa())
      df          <- filtered_naa()
      yaxis_label <- glue::glue("{df$metric_parsed[1]} at Age ({df$units[1]})")
      
      # Multi-year historical time series (e.g. Cod/Haddock) get the line-per-year
      # view; a single historical assessment year or the projected replicates
      # (e.g. Scup, Summer Flounder, Black Sea Bass N/S) get a boxplot showing
      # the spread across replicates/estimates instead.
      use_line_view <- input$naa_period == "historical" &&
        (length(unique(df$year)) > 1 ||
           input$species %in% c("Black Sea Bass (North)", "Black Sea Bass (South)",
                                "Scup", "Summer Flounder"))
      
      if (use_line_view) {
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
        n_reps    <- df %>% dplyr::count(age) %>% dplyr::pull(n) %>% max()
        plot_data <- df %>%
          mutate(age = factor(paste0("Age ", age), levels = paste0("Age ", sort(unique(df$age)))))
        
        g <- ggplot(plot_data, aes(x = age, y = value)) +
          geom_boxplot(fill = "#5EB6D9", color = "#003087",
                       outlier.fill = "#5EB6D9", outlier.alpha = 0.1, outlier.color = "transparent") +
          scale_y_continuous(labels = scales::comma) +
          labs(x = "Age", y = yaxis_label,
               caption = paste0("Boxes show distribution across ", scales::comma(n_reps),
                                " replicate", ifelse(n_reps == 1, "", "s"))) +
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
        group_by(mode, x_val = !!sym(grp), year) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(x_label = if (grp == "wave") paste0("Wave ", x_val) else as.character(x_val),
               x_label = factor(x_label, levels = unique(x_label[order(x_val)])),
               year    = factor(year),
               mode    = factor(mode, levels = c("for_hire", "private", "shore"),
                                labels = c("For Hire", "Private", "Shore")))

      g <- ggplot(plot_data,
                  aes(x = x_label, y = total, fill = mode, alpha = year,
                      text = paste0("Mode: ", mode,
                                    "<br>", tools::toTitleCase(grp), ": ", x_label,
                                    "<br>Value: ", scales::comma(round(total, 0)))))  +
        geom_col(position = "dodge", width = 0.7) +
        scale_alpha_manual(values = setNames(
          seq(0.5, 1, length.out = length(unique(plot_data$year))),
          levels(plot_data$year)), name = "Year") +
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
      
      if (grp == "wave") {
        
        bars <- df %>%
          filter(metric %in% c("harvest", "discards")) %>%
          group_by(x_val = .data[[grp]], year, metric) %>%
          summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
          mutate(x_label = factor(paste0("Wave ", x_val),
                                  levels = unique(paste0("Wave ", sort(unique(x_val))))),
                 year    = factor(year),
                 metric  = factor(tools::toTitleCase(metric),
                                  levels = c("Harvest", "Discards"))) %>%
          arrange(x_label, year, metric) %>%
          group_by(x_label, year) %>%
          mutate(ymax = cumsum(total),
                 ymin = ymax - total) %>%
          ungroup()
        
        year_levels <- levels(bars$year)
        n_years     <- length(year_levels)
        bar_width   <- 0.35
        offsets     <- setNames(seq(-bar_width * (n_years - 1) / 2,
                                    bar_width * (n_years - 1) / 2,
                                    length.out = n_years),
                                year_levels)
        
        bars <- bars %>%
          mutate(x_num   = as.integer(x_label),
                 x_mid   = x_num + offsets[as.character(year)],
                 x_left  = x_mid - bar_width / 2,
                 x_right = x_mid + bar_width / 2)
        
        g <- ggplot() +
          geom_rect(data = bars,
                    aes(xmin = x_left, xmax = x_right, ymin = ymin, ymax = ymax,
                        fill = metric, alpha = year,
                        text = paste0(metric, "<br>Wave: ", x_label,
                                      "<br>Year: ", year,
                                      "<br>Value: ", scales::comma(round(total, 0))))) +
          scale_x_continuous(breaks = seq_along(levels(bars$x_label)),
                             labels = levels(bars$x_label)) +
          scale_fill_manual(values = c("Harvest" = "#003087", "Discards" = "#5EB6D9"),
                            name = NULL) +
          scale_alpha_manual(values = setNames(seq(1, 0.45, length.out = n_years), year_levels),
                             name = "Year") +
          scale_y_continuous(labels = scales::comma) +
          labs(x = "Wave", y = y_lab) +
          theme_minimal(base_size = 12) +
          theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "right")
        
      } else {
        
        bars <- df %>%
          filter(metric %in% c("harvest", "discards")) %>%
          group_by(x_val = .data[[grp]], metric) %>%
          summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
          mutate(x_label = if (grp == "wave") paste0("Wave ", x_val) else as.character(x_val),
                 x_label = factor(x_label, levels = unique(x_label[order(x_val)])),
                 metric  = factor(tools::toTitleCase(metric),
                                  levels = c("Harvest", "Discards")))
        
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
      }
      
      return(ggplotly(g, tooltip = "text"))
      
      # ── Catch per Trip ──
      # By month (x-axis) and by mode (colour). Point = median catch per trip;
      # error bar spans min–max. Multiple selected years are faceted.
    } else if (input$data_metric == "cpt") {
      
      req(filtered_cpt())
      df    <- filtered_cpt()
      y_lab <- if (nrow(df) > 0) paste0("catch per trip (", df$units[1], ")") else "catch per trip"
      
      plot_data <- df %>%
        mutate(metric = dplyr::recode(metric,
                                      "min catch per trip"    = "min",
                                      "median catch per trip" = "median",
                                      "max catch per trip"    = "max")) %>%
        tidyr::pivot_wider(names_from = metric, values_from = value) %>%
        mutate(
          mode = factor(mode, levels = c("for_hire", "private", "shore"),
                        labels = c("For Hire", "Private", "Shore")),
          year       = factor(year),
          date_order = as.Date(paste(year, month, "01"), "%Y %m %d"),
          month_year = format(date_order, "%b %Y"),
        )
      
      # Build complete grid of all mode x month-year combos
      all_combos <- tidyr::expand_grid(
        mode       = levels(plot_data$mode),
        date_order = seq(min(plot_data$date_order),
                         max(plot_data$date_order),
                         by = "month")
      ) %>%
        mutate(
          mode       = factor(mode, levels = levels(plot_data$mode)),
          year       = factor(format(date_order, "%Y")),
          month_year = format(date_order, "%b %Y")
        )
      
      plot_data <- all_combos %>%
        left_join(plot_data %>% select(mode, date_order, median, min, max),
                  by = c("mode", "date_order")) %>%
        mutate(month_year = factor(month_year,
                                   levels = unique(format(sort(unique(date_order)), "%b %Y"))))
      
      pd <- position_dodge(width = 0.5)
      
      g <- ggplot(plot_data,
                  aes(x = month_year, y = median, color = mode,
                      group = interaction(mode, year),
                      text = paste0("Mode: ", mode,
                                    "<br>Month/Year: ", month_year,
                                    "<br>Median: ", scales::comma(round(median, 2)),
                                    "<br>Min: ",    scales::comma(round(min, 2)),
                                    "<br>Max: ",    scales::comma(round(max, 2))))) +
        geom_errorbar(aes(ymin = min, ymax = max), width = 0.25, position = pd, alpha = 0.6,
                      na.rm = TRUE) +
        
        geom_point(position = pd, size = 2.2, na.rm = TRUE) +
        scale_color_manual(values = c("For Hire" = "#003087", "Private" = "#5EB6D9", "Shore" = "#C6E6F0"),
                           name = "Mode") +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Month / Year", y = y_lab,
             caption = "Points = median catch per trip; bars span min\u2013max") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "right")
      
      return(ggplotly(g, tooltip = "text"))
      
      # ── Catch-at-Length ──
      # One figure per season (faceted), with Baseline and Projected overlaid as
      # two colored lines on the same panel. Length (parsed from the metric
      # column) is the x-axis.
    } else if (input$data_metric == "catch_len") {
      
      req(filtered_catch_len())
      df <- filtered_catch_len()
      
      plot_data <- df %>%
        filter(units %in% c("projected fitted percent of catch",
                            "baseline fitted percent of catch")) %>%
        mutate(
          Panel  = dplyr::if_else(units == "projected fitted percent of catch",
                                  "Projected", "Baseline"),
          Panel  = factor(Panel, levels = c("Projected", "Baseline")),
          Season = factor(tools::toTitleCase(season), levels = c("Summer", "Winter"))
        ) %>%
        arrange(Season, Panel, length)
      
      g <- ggplot(plot_data,
                  aes(x = length, y = value, color = Panel, group = Panel,
                      text = paste0("Series: ", Panel,
                                    "<br>Season: ", Season,
                                    "<br>Length: ", length,
                                    "<br>Value: ", scales::comma(round(value, 5))))) +
        geom_line(linewidth = 0.8, alpha = 0.85) +
        geom_point(size = 1.2, alpha = 0.7) +
        facet_wrap(~ Season, ncol = 1) +
        scale_color_manual(values = c("Projected" = "#0085CA", "Baseline" = "#003087"), name = NULL) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Length (in)", y = "Percentage of Catch") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "right")
      
      return(ggplotly(g, tooltip = "text"))
    } 
  })
  
  output$main_plot <- renderPlotly({ plot_obj() })
  
  # ── Summary table ──────────────────────────────────────────────────────────
  summary_table_data <- reactive({
    
    if (input$data_metric == "naa") {
      req(filtered_naa())
      df <- filtered_naa()
      use_line_view <- input$naa_period == "historical" && length(unique(df$year)) > 1
      if (use_line_view) {
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
          Harvest       = scales::comma(round(sum(value[metric == "harvest"],  na.rm = TRUE), 0)),
          Discards      = scales::comma(round(sum(value[metric == "discards"], na.rm = TRUE), 0)),
          `Total Catch` = scales::comma(round(sum(value[metric == "catch"],    na.rm = TRUE), 0)),
          .groups = "drop"
        ) %>%
        arrange(Year, Wave, Mode)
      
    } else if (input$data_metric == "cpt") {
      req(filtered_cpt())
      filtered_cpt() %>%
        mutate(Mode = dplyr::case_when(
                 mode == "for_hire" ~ "For Hire",
                 mode == "private"  ~ "Private",
                 mode == "shore"    ~ "Shore",
                 TRUE ~ tools::toTitleCase(mode)),
               metric = dplyr::recode(metric,
                 "min catch per trip"    = "Min",
                 "median catch per trip" = "Median",
                 "max catch per trip"    = "Max")) %>%
        tidyr::pivot_wider(id_cols = c(year, month, Mode),
                           names_from = metric, values_from = value) %>%
        transmute(Year   = year,
                  Month  = factor(month.name[month], levels = month.name),
                  Mode,
                  Min    = round(Min, 2),
                  Median = round(Median, 2),
                  Max    = round(Max, 2)) %>%
        arrange(Year, Month, Mode) %>%
        mutate(Month = as.character(Month))
      
    } else if (input$data_metric == "catch_len") {
      req(filtered_catch_len())
      filtered_catch_len() %>%
        filter(units %in% c("projected fitted percent of catch",
                            "baseline fitted percent of catch")) %>%
        mutate(
          Panel  = dplyr::if_else(units == "projected fitted percent of catch",
                                  "Projected", "Baseline"),
          Season = tools::toTitleCase(season)
        ) %>%
        transmute(Panel, Season, Length = length,
                  `Proportion of Catch` = scales::comma(round(value, 5))) %>%
        arrange(Panel, Season, Length)
      
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
  
  output$summary_table <- renderTable({ summary_table_data() })
  
  # ── Downloads ──────────────────────────────────────────────────────────────
  # Downloads exactly what's shown in the summary table, named after the table's title.
  output$download_data <- downloadHandler(
    filename = function() {
      title_clean <- plot_title_text() %>%
        stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
        stringr::str_replace_all("^_+|_+$", "")
      paste0(title_clean, "_", Sys.Date(), ".csv")
    },
    content = function(file) write.csv(summary_table_data(), file, row.names = FALSE)
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
                       "catch_at_len_doc"            = "docs/catch_at_len_GF.html",
                       "naa_cod_doc"                 = "docs/NAA_cod.html",
                       "naa_haddock_doc"             = "docs/NAA_haddock.html",
                       "naa_bsb_doc"                 = "docs/NAA_blackseabass.html",
                       "naa_sf_doc"                  = "docs/NAA_summerflounder.html",
                       "naa_scup_doc"                = "docs/NAA_scup.html",
                       "trips_catch_cod_haddock_doc" = "docs/trips_catch_cod_haddock.html",
                       "trips_catch_sfbsbs_doc"      = "docs/trips_catch_sfsbsb.html",
                       "cpt_cod_haddock_doc"         = "docs/cpt_cod_haddock.html")

    tags$iframe(src = doc_path, style = "width: 100%; height: 800px; border: none;", seamless = NA)
  })
}

shinyApp(ui, server)
