###############################################################################
# Purpose:  A small R script to get data off google drive and put it in data/main  
# Inputs:   Google Drive files matching 2024 Assessment NAA patterns
# Outputs:  Downloaded .Rds files in data/main/ and optional workspace objects
# Requirements: Access to the shared google drive folder 
###############################################################################  

###################################################
### Code to pull NAA off google drive
###################################################

##############################
# Load Libraries #############
##############################
library(tidyverse)
library(glue)
library(googledrive)
library(here)
library(conflicted)
conflicts_prefer(dplyr::filter)


##############################
# Environment Setup ##########
##############################

# deal with directories
here::i_am("stored_scripts/pull_NAA.R")

#Set this to TRUE if you want to download the files and read them into memory. FALSE otherwise
readin<-FALSE

#####################################################################################################
# stubs of output save files
# no wildcards, because googledrive doesn't like that
CodProjectedNAA<-"WGOM_Cod_projected_NAA_from_2024Assessment"
CodHistoricalNAA<-"WGOM_Cod_historical_NAA_from_2024Assessment"

HaddockProjectedNAA<-glue("GOM_Haddock_projected_NAA_2024Assessment")
HaddockHistoricalNAA<-glue("GOM_Haddock_historical_NAA_2024Assessment")

files_to_get<-list(CodProjectedNAA,CodHistoricalNAA,HaddockProjectedNAA,HaddockHistoricalNAA)

#####################################################################################################
# Connect to Google Drive
drive_auth(cache = here(".secrets"), email = TRUE,  scopes = "https://www.googleapis.com/auth/drive")

# Find the folder on google drive
processed_data_path<-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","input_data")
folder_info <- drive_get(
  path = processed_data_path,
  shared_drive = "NMFS NEC READ SSB"
)
processed_data_path<-folder_info$id

# check that exactly one valid folder ID was returned
if(length(processed_data_path)>1){
  stop("more than 1 folder found.")
}
if(length(processed_data_path)<1){
  stop("no folders found")
}

#####################################################################################################

# It's a loop. Sorry.


for (my_file in files_to_get){
  
  # Search for files that match the pattern in myfile
  files_in_folder <- drive_ls(
    path = as_id(processed_data_path), 
    pattern = my_file
  )
  #################### Ensure you only get an Rds file#####
  search_pattern <- glue("{my_file}_.*\\.Rds")
  
  ############################################################
  # Identify most recent file based on date in filename #####
  ############################################################
  most_recent_file <- files_in_folder %>%
    # Filter only for files that match the full naming convention
    filter(str_detect(name, search_pattern)) %>%
    # Extract the date (looks for the YYYY-MM-DD format)
    mutate(file_date = str_extract(name, "\\d{4}-\\d{2}-\\d{2}")) %>%
    mutate(file_date = ymd(file_date)) %>%
    # Sort so the newest date is at the top
    arrange(desc(file_date)) %>%
    slice(1)
  
  # Throw some warnings
  
  # validate query results before attempting download
  if (nrow(most_recent_file) == 0) {
    warning(glue("No files matching the {search_pattern} were found in the specified folder. Nothing downloaded"))
  } else if (nrow(most_recent_file) >1) {
    warning(glue("More than 1 file matching the {search_pattern} were found in the specified folder. Nothing downloaded"))
  }else{
    # Get the file and save it
    drive_download(
      file = as_id(most_recent_file$id),
      path = here("data", "main", most_recent_file$name),
      overwrite = TRUE
    )
    print(glue("Successfully downloaded file {my_file}_{most_recent_file$file_date}.Rds" ))
  }
  
  # Load data into workspace if readin flag is active
  if(readin==TRUE){
    assign(my_file, readRDS(here("data", "main", most_recent_file$name)))
  }
}

##############################
# Cleanup workspace ##########
##############################
rm(folder_info, most_recent_file, files_to_get, files_in_folder)
