################################################################################
# Script:       pull_NAA_dashboard_data.R
# Purpose:      Uses pull_dashboard_data() to get NAA data 
#
# Inputs:       Shared drive mounted at D:/Shared drives/NMFS NEC READ SSB
#
# Outputs:      .Rds files in data/main/. Named character vector of local
#               paths (dashboard_paths).
# Dependencies: R/pull_dashboard_data.R. Packages dplyr, glue,
#               here, lubridate, magrittr, purrr, stringr, tibble.
# Pipeline:     Data setup for the dashboard.
################################################################################

library(magrittr)

###################################################
### Project root and function
###################################################

# Declare the project root
here::i_am("stored_scripts/pull_NAA_dashboard_data.R")

# Load pull_dashboard_data() function
source(here::here("R", "pull_dashboard_data.R"))

###################################################
### Many files: one row per file
###################################################

# One row per file to pull. Column names match the function's arguments, so
# pmap passes each row as one call.
NAA_files_to_pull <- tibble::tribble(
  ~fishery,     ~management_cycle, ~final_folder,   ~file,
  "groundfish", 2027,              "input_data",    "WGOM_Cod_projected_NAA",
  "groundfish", 2027,              "input_data",    "WGOM_Cod_historical_NAA",
  "groundfish", 2027,              "input_data",    "GOM_Haddock_projected_NAA",
  "groundfish", 2027,              "input_data",    "GOM_Haddock_historical_NAA",
  "fluke",      2026,              "miscellaneous", "SummerFlounder_historicalNAA",
  "fluke",      2026,              "miscellaneous", "SummerFlounder_projectedNAA"
)

# One local path per row (NA where no file was found), named by stub
# NOTE: overwrite defaults to FALSE, so files already in data/main are skipped
NAA_file_paths <- NAA_files_to_pull %>%
  purrr::pmap_chr(pull_dashboard_data) %>%
  purrr::set_names(NAA_files_to_pull$file)

# Stubs with no file found
names(NAA_file_paths)[is.na(NAA_file_paths)]

###################################################
### Optional: read into memory
###################################################

# Named list of data frames; skips stubs that returned NA
dashboard_data <- NAA_file_paths %>%
  purrr::discard(is.na) %>%
   purrr::map(readRDS)
