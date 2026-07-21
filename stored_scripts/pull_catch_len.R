## This script reads in the WGOM Cod & Haddock catch at length Rds from Google Drive


#Load libraries
library(tidyverse)
library(haven)
library(glue)
library(googledrive)
library(here)

here::i_am("stored_scripts/pull_catch_len.R")

# date stamp of MRIP data pull; check miscellaneous folder on google and change file_date to the one in the Rds file name
file_date<-"2026-06-29" 
SimCPT_file_in<-glue("rdb_catch_at_length_{file_date}.Rds")  # input file


# Connect to Google Drive
# NOTE: Relies on cached credentials in .secrets. Will prompt interactive auth if missing or expired.
drive_auth(cache = here(".secrets"), email = TRUE)

# Find miscellaneous folder on google drive
miscellaneous_path <-file.path("socialsci","RecreationalDST","2027_management_cycle_data",
                               "groundfishRDM","miscellaneous")
folder_info <- drive_get(
  path = miscellaneous_path,
  shared_drive = "NMFS NEC READ SSB"
)
miscellaneous_path<-folder_info$id

# Find our file
miscellaneous_readin <-file.path("socialsci","RecreationalDST","2027_management_cycle_data",
                                 "groundfishRDM","miscellaneous",SimCPT_file_in)

file_id<-drive_get(path = miscellaneous_readin, shared_drive = "NMFS NEC READ SSB")$id


# Create path for a temporary file
# NOTE: tempfile handles cross-platform path for safe creation and garbage collection upon session end
temp_path <- tempfile()

# Download
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
#get a data version
data_vintage<-as.character(Sys.Date())

write_rds(
  rdb_catch_at_len,
  file=here("data","main",glue("catch_at_len_{data_vintage}.Rds")) )


## To download and save to data/main folder rather than using temporary file:

# save_file <- here("data","main",SimCPT_file_in)
# 
# drive_download(
#   file = as_id(file_id),
#   path = save_file,
#   overwrite = TRUE
# )
# 
# rdb_catch_at_len <- read_rds(save_file)


