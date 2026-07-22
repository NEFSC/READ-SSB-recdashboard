## This script reads in the WGOM Cod & Haddock catch at length Rds from Google Drive

# Load libraries
library(tidyverse)
library(haven)
library(glue)
library(googledrive)
library(here)

here::i_am("stored_scripts/pull_catch_len.R")

# Connect to Google Drive
# NOTE: Relies on cached credentials in .secrets. Will prompt interactive auth if missing or expired.
drive_auth(cache = here(".secrets"), email = TRUE)

# Find miscellaneous folder on google drive
miscellaneous_path <- file.path("socialsci", "RecreationalDST", "2027_management_cycle_data",
                                "groundfishRDM", "miscellaneous")

folder_info <- drive_get(
  path = miscellaneous_path,
  shared_drive = "NMFS NEC READ SSB"
)


# List files inside the miscellaneous folder, only the ones containing our target string to save API calls
folder_files <- drive_ls(
  path = as_id(folder_info$id),
  q = "name contains 'rdb_catch_at_length_'"
)


# Filter for our specific files, extract dates, and find the most recent one
latest_file <- folder_files %>%
  # Keep only files that match our naming pattern
  filter(str_detect(name, "^rdb_catch_at_length_\\d{4}-\\d{2}-\\d{2}\\.Rds$")) %>%
  # Extract the date string and convert to an actual Date object for accurate sorting
  mutate(
    parsed_date = as.Date(str_extract(name, "\\d{4}-\\d{2}-\\d{2}"))
  ) %>%
  # Sort descending by date so the newest is at the top, then keep only that row
  arrange(desc(parsed_date)) %>%
  slice(1)

# Safety check in case the folder is empty or files were renamed/moved
if (nrow(latest_file) == 0) {
  stop("No files matching 'rdb_catch_at_length_{YYYY-MM-DD}.Rds' were found in the folder.")
}

# Grab the ID of the most recent file
file_id <- latest_file$id
message(glue("Downloading the most recent file: {latest_file$name}"))

# Create path for a temporary file
# NOTE: tempfile handles cross-platform path for safe creation and garbage collection upon session end
temp_path <- tempfile(fileext = ".Rds")

# Download using the file ID we just dynamically found
drive_download(
  file = as_id(file_id),
  path = temp_path,
  overwrite = TRUE
)

# Read file into R environment
rdb_catch_at_len <- read_rds(temp_path)

# cleanup
if (file.exists(temp_path)) {
  file.remove(temp_path)
}


# get the data version from the downloaded file's date and save Rds to data folder
data_vintage <- as.character(latest_file$parsed_date)

write_rds(
  rdb_catch_at_len,
  file = here("data", "main", glue("catch_at_len_{data_vintage}.Rds"))
)



