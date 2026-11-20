################################################################################
# Script:       pull_NAA_dashboard_data.R
# Purpose:      Uses pull_dashboard_data() to get 2026 and 2027 NAA data,
#               then stacks the two cycles by stub and saves one file per
#               stub.
#
# Inputs:       Shared drive mounted at D:/Shared drives/NMFS NEC READ SSB
#
# Outputs:      Dated .Rds files in data/main/ (one per stub per cycle).
#               Stacked data/main/{stub}.Rds (one per stub).
#               Named character vector of stacked paths (data_stacked_paths).
# Dependencies: R/pull_dashboard_data.R. Packages dplyr, glue,
#               here, lubridate, magrittr, purrr, stringr, tibble.
# Pipeline:     Data setup for the dashboard.
################################################################################
library(here)
library(magrittr)
library(dplyr)
###################################################
### Project root and function
###################################################

# Declare the project root
here::i_am("stored_scripts/pull_NAA_dashboard_data.R")

# Load pull_dashboard_data() function
source(here::here("R", "pull_dashboard_data.R"))


dir.create(here("data", "main", "2026_management_cycle_data"), showWarnings = FALSE)
dir.create(here("data", "main", "2027_management_cycle_data"), showWarnings = FALSE)
dir.create(here("data", "main", "2028_management_cycle_data"), showWarnings = FALSE)

###################################################
# Get the NAA data from 2026
# One row per file to pull. Column names match the function's arguments, so
# pmap passes each row as one call.
files_to_pull_2026 <- tibble::tribble(
  ~fishery,     ~management_cycle, ~final_folder,   ~file,
  "groundfish", 2026,              "input_data",    "WGOM_Cod_projected_NAA",
  "groundfish", 2026,              "input_data",    "WGOM_Cod_historical_NAA",
  "groundfish", 2026,              "input_data",    "GOM_Haddock_projected_NAA",
  "groundfish", 2026,              "input_data",    "GOM_Haddock_historical_NAA"
)
# Pull the 2026 data 
file_paths_2026 <- files_to_pull_2026 %>%
  purrr::pmap_chr(pull_dashboard_data) %>%
  purrr::set_names(files_to_pull_2026$file)

# Stubs with no file found
names(file_paths_2026)[is.na(file_paths_2026)]


###################################################
# Get the NAA data from 2026. All you have to do is mutate 
# the value of the management_cycle column
files_to_pull_2027 <- files_to_pull_2026 %>%
  mutate(management_cycle=2027)

file_paths_2027 <- files_to_pull_2027 %>%
  purrr::pmap_chr(pull_dashboard_data) %>%
  purrr::set_names(files_to_pull_2027$file)

# Stubs with no file found
names(file_paths_2027)[is.na(file_paths_2027)]

data_2026 <- file_paths_2026 %>% 
  purrr::discard(is.na) %>%
  purrr::map(readRDS)

data_2027 <- file_paths_2027 %>% 
  purrr::discard(is.na) %>%
  purrr::map(readRDS)

stopifnot(length(data_2026)==length(data_2027))


###################################################
### Stack 2026 and 2027 NAA by stub and save
###################################################

# 4 stubs x 2 management cycles
expected_n_files <- length(files_to_pull_2026)+ length(files_to_pull_2027)

# Cycle labels taken from the request tables, as character, for comparing
# with the management_cycle column in each file
cycle_2026 <- as.character(unique(files_to_pull_2026$management_cycle))
cycle_2027 <- as.character(unique(files_to_pull_2027$management_cycle))

# Strip "_YYYY-MM-DD.Rds" so stubs and exact file names pair on the stub
to_stub <- function(x) {
  stringr::str_remove(x, "_\\d{4}-\\d{2}-\\d{2}\\.Rds$")
}

stubs_2026 <- to_stub(names(file_paths_2026))
stubs_2027 <- to_stub(names(file_paths_2027))

#### Checks on the file paths, before anything is stacked

# Every requested file was found in both cycles
missing_files <- c(
  names(file_paths_2026)[is.na(file_paths_2026)],
  names(file_paths_2027)[is.na(file_paths_2027)]
)
if (length(missing_files) > 0) {
  stop(
    "Not stacking. Files not found: ",
    paste(missing_files, collapse = ", "),
    call. = FALSE
  )
}

# Expected total number of files
n_files <- length(file_paths_2026) + length(file_paths_2027)
if (n_files != expected_n_files) {
  stop(
    glue::glue(
      "Not stacking. Expected {expected_n_files} files, found {n_files}."
    ),
    call. = FALSE
  )
}

# Each stub appears once per cycle, and both cycles have the same stubs
if (anyDuplicated(stubs_2026) > 0 || anyDuplicated(stubs_2027) > 0) {
  stop("Not stacking. A stub is requested more than once.", call. = FALSE)
}
if (!setequal(stubs_2026, stubs_2027)) {
  stop(
    "Not stacking. Stubs differ between cycles. Only in one cycle: ",
    paste(
      c(setdiff(stubs_2026, stubs_2027), setdiff(stubs_2027, stubs_2026)),
      collapse = ", "
    ),
    call. = FALSE
  )
}

#### Key the loaded data by stub, so pairing is by name and never by position
names(data_2026) <- to_stub(names(data_2026))
names(data_2027) <- to_stub(names(data_2027))

#### Stack and check every stub first. Nothing is saved unless all 4 pass,
#### so a failure can't leave a mix of new and old stacked files.

data_stacked <- list()

for (stub in stubs_2026) {
  df_2026 <- data_2026[[stub]]
  df_2027 <- data_2027[[stub]]

  # Same column names in the same order
  if (!setequal(names(df_2026), names(df_2027))) {
    stop(
      glue::glue("Not stacking {stub}. Column names differ between cycles."),
      call. = FALSE
    )
  }
  
  # Each file holds only its own cycle (catches crossed files)
  # NOTE: as.character() so the check works whether management_cycle is
  # stored as character or numeric
  if (!identical(unique(as.character(df_2026$management_cycle)), cycle_2026)) {
    stop(
      glue::glue(
        "Not stacking {stub}. The {cycle_2026} file has management_cycle ",
        "values other than {cycle_2026}."
      ),
      call. = FALSE
    )
  }
  if (!identical(unique(as.character(df_2027$management_cycle)), cycle_2027)) {
    stop(
      glue::glue(
        "Not stacking {stub}. The {cycle_2027} file has management_cycle ",
        "values other than {cycle_2027}."
      ),
      call. = FALSE
    )
  }

  # rbind() on data frames matches columns by name
  stacked <- rbind(df_2026, df_2027)

  # After stacking: every column has the same class as in both inputs.
  # rbind() silently coerces mismatched types (e.g., numeric + character
  # becomes character), so a changed class means the inputs differed.
  classes_stacked <- purrr::map(stacked, class)
  classes_2026 <- purrr::map(df_2026, class)
  classes_2027 <- purrr::map(df_2027, class)

  changed_columns <- names(classes_stacked)[
    !purrr::map2_lgl(classes_stacked, classes_2026, identical) |
      !purrr::map2_lgl(classes_stacked, classes_2027, identical)
  ]

  if (length(changed_columns) > 0) {
    stop(
      glue::glue(
        "Not saving {stub}. Column types differ between cycles: ",
        "{paste(changed_columns, collapse = ', ')}"
      ),
      call. = FALSE
    )
  }

  data_stacked[[stub]] <- stacked
}

#### All stubs passed: save each to data/main/{stub}.Rds (no date in name)

data_stacked_paths <- character(0)

for (stub in names(data_stacked)) {
  stacked_path <- here::here("data", "main", paste0(stub, ".Rds"))
  saveRDS(data_stacked[[stub]], stacked_path)
  message(glue::glue("Saved: {basename(stacked_path)}"))

  data_stacked_paths[stub] <- stacked_path
}

# One stacked file per stub
stopifnot(length(data_stacked_paths) == expected_n_files / 2)









# copied in the hardcoded file read and stack.
# #### GOM Haddock Hist
# GOM_Haddock_hist_NAA_2026 <- readRDS(here::here("data/main/GOM_Haddock_historical_NAA_2026-06-16.RDS"))
# GOM_Haddock_hist_NAA_2027 <- readRDS(here::here("data/main/GOM_Haddock_historical_NAA_2026-11-09.RDS"))
# 
# GOM_Haddock_hist_NAA_2026<- GOM_Haddock_hist_NAA_2026 %>%
#   dplyr::mutate(management_cycle = c("2026"))
# 
# GOM_Haddock_hist_NAA<- rbind(GOM_Haddock_hist_NAA_2026, GOM_Haddock_hist_NAA_2027)
# 
# saveRDS(GOM_Haddock_hist_NAA, here::here("data/main/GOM_Haddock_hist_NAA.RDS"))
# 
# #### GOM Haddock proj
# GOM_Haddock_proj_NAA_2026 <- readRDS(here::here("data/main/GOM_Haddock_projected_NAA_2026-06-16.RDS"))
# GOM_Haddock_proj_NAA_2027 <- readRDS(here::here("data/main/GOM_Haddock_projected_NAA_2026-11-09.RDS"))
# 
# GOM_Haddock_proj_NAA_2026<- GOM_Haddock_proj_NAA_2026 %>%
#   dplyr::mutate(management_cycle = c("2026"))
# 
# GOM_Haddock_proj_NAA<- rbind(GOM_Haddock_proj_NAA_2026, GOM_Haddock_proj_NAA_2027)
# 
# saveRDS(GOM_Haddock_proj_NAA, here::here("data/main/GOM_Haddock_proj_NAA.RDS"))
# 
# 
# 
# #### WGOM Cod Hist
# WGOM_Cod_hist_NAA_2026 <- readRDS(here::here("data/main/WGOM_Cod_historical_NAA_2026-06-16.RDS"))
# WGOM_Cod_hist_NAA_2027 <- readRDS(here::here("data/main/WGOM_Cod_historical_NAA_2026-11-09.RDS"))
# 
# WGOM_Cod_hist_NAA_2026<- WGOM_Cod_hist_NAA_2026 %>%
#   dplyr::mutate(management_cycle = c("2026"))
# 
# WGOM_Cod_hist_NAA<- rbind(WGOM_Cod_hist_NAA_2026, WGOM_Cod_hist_NAA_2027)
# 
# saveRDS(WGOM_Cod_hist_NAA, here::here("data/main/WGOM_Cod_hist_NAA.RDS"))
# 
# ### WGOM Haddock proj
# WGOM_Cod_proj_NAA_2026 <- readRDS(here::here("data/main/WGOM_Cod_projected_NAA_2026-06-16.RDS"))
# WGOM_Cod_proj_NAA_2027 <- readRDS(here::here("data/main/WGOM_Cod_projected_NAA_2026-11-09.RDS"))
# 
# WGOM_Cod_proj_NAA_2026<- WGOM_Cod_proj_NAA_2026 %>%
#   dplyr::mutate(management_cycle = c("2026"))
# 
# WGOM_Cod_proj_NAA<- rbind(WGOM_Cod_proj_NAA_2026, WGOM_Cod_proj_NAA_2027)
# 
# saveRDS(WGOM_Cod_proj_NAA, here::here("data/main/WGOM_Cod_proj_NAA.RDS"))





