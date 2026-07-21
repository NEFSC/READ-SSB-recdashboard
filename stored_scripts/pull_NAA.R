###############################################################################
# Name: pull_NAA.R
# Inputs: Remote Google Drive directory containing GOM groundfish RDM files
# Outputs: Saved local .Rds files in data/main/ and optionally loaded R objects
# Dependencies: Valid Google Drive access credentials cached locally
# Description: Retrieves most recent Numbers-at-Age (NAA) data for GOM Cod 
#              and Haddock from Drive and downloads them locally.
###############################################################################  

###################################################
### Code to pull NAA off google drive



#Load Libraries
library(tidyverse)
library(glue)
library(googledrive)
library(here)
library(conflicted)
conflicts_prefer(dplyr::filter)


# deal with directories
# Declare script location to establish project root for relative paths
here::i_am("stored_scripts/pull_NAA.R")

# locations of NAA on google drive
groundfish_processed_data_path<-file.path("socialsci","RecreationalDST","2027_management_cycle_data","groundfishRDM","input_data")
sfsbsb_processed_data_path<-file.path("socialsci","RecreationalDST","2028_management_cycle_data","flukeRDM","miscellaneous")


#Set this to TRUE if you want to download the files and read them into memory. FALSE otherwise
# NOTE: Toggles whether downloaded files are immediately loaded into the global environment
readin<-TRUE

#####################################################################################################
# stubs of output save files
# no wildcards, because googledrive doesn't like that
# NOTE: These exact string stubs will be used for both regex matching and dynamic variable assignment

SFSBSB_files_to_get<-list("SummerFlounder_historicalNAA",
             "Scup_historicalNAA",
             "BlackSeaBassSouth_historicalNAA",
             "BlackSeaBassNorth_historicalNAA",
             "SummerFlounder_projectedNAA",
             "Scup_projectedNAA",
             "BlackSeaBassSouth_projectedNAA",
             "BlackSeaBassNorth_projectedNAA")
             

groundfish_files_to_get<-list("WGOM_Cod_projected_NAA",
                   "WGOM_Cod_historical_NAA",
                   "GOM_Haddock_projected_NAA",
                   "GOM_Haddock_historical_NAA")

files_to_get<-append(groundfish_files_to_get,SFSBSB_files_to_get)

#####################################################################################################
# Connect to Google Drive
# Authenticate using cached credentials located in the .secrets directory to bypass interactive prompts
drive_auth(cache = here(".secrets"), email = TRUE,  scopes = "https://www.googleapis.com/auth/drive")

# Find the folder on google drive
folder_info <- drive_get(
  path = groundfish_processed_data_path,
  shared_drive = "NMFS NEC READ SSB"
)
# Reassign processed_data_path to the specific Google Drive folder ID for targeted queries
groundfish_processed_data_path<-folder_info$id

if(length(groundfish_processed_data_path)>1){
  stop("more than 1 groundfish folder found.")
}
if(length(groundfish_processed_data_path)<1){
  stop("no groundfish folders found")
}


# Find the folder on google drive
folder_info <- drive_get(
  path = sfsbsb_processed_data_path,
  shared_drive = "NMFS NEC READ SSB"
)
# Reassign processed_data_path to the specific Google Drive folder ID for targeted queries
sfsbsb_processed_data_path<-folder_info$id

if(length(sfsbsb_processed_data_path)>1){
  stop("more than 1 folder for sfsbsb found.")
}
if(length(sfsbsb_processed_data_path)<1){
  stop("no sfsbsb folders found")
}



#####################################################################################################

# It's a loop. Sorry.

download_count<-0
for (my_file in files_to_get){
  
  if (my_file %in% SFSBSB_files_to_get){
    search_path<-sfsbsb_processed_data_path
  } else  if (my_file %in% groundfish_files_to_get){
    search_path<-groundfish_processed_data_path
  } else {
    stop("Error: the file you're looking for is not valid. This shouldn't happen")
  }
  # Search for files that match the pattern in myfile
  # NOTE: as_id prevents drive_ls from treating the ID string as a literal file path
  files_in_folder <- drive_ls(
    path = as_id(search_path), 
    pattern = my_file
  )
  #################### Ensure you only get an Rds file#####
  # Regex matches the filename stub, followed by an underscore, any characters, and specifically the .Rds extension
  search_pattern <- glue("{my_file}_.*\\.Rds")
  
  most_recent_file <- files_in_folder %>%
    # Filter only for files that match the full naming convention
    filter(str_detect(name, search_pattern)) %>%
    # Extract the date (looks for the YYYY-MM-DD format)
    # NOTE: str_extract only pulls the first match. If multiple date structures exist in the name, it grabs the first.
    mutate(file_date = str_extract(name, "\\d{4}-\\d{2}-\\d{2}")) %>%
    mutate(file_date = ymd(file_date)) %>%
    # Sort so the newest date is at the top
    arrange(desc(file_date)) %>%
    # Isolates the single most recent file based on the parsed date
    slice(1)
  
  # Throw some warnings
  
  if (nrow(most_recent_file) == 0) {
    warning(glue("No files matching the {search_pattern} were found in the specified folder. Nothing downloaded"))
  } else if (nrow(most_recent_file) >1) {
    # UNCERTAIN: This is dead code. Due to slice(1) in the pipeline above, nrow(most_recent_file) 
    # can never be greater than 1. 
    warning(glue("More than 1 file matching the {search_pattern} were found in the specified folder. Nothing downloaded"))
  }else{
    # Get the file and save it
    drive_download(
      file = as_id(most_recent_file$id),
      path = here("data", "main", most_recent_file$name),
      overwrite = TRUE
    )
    print(glue("Successfully downloaded file {my_file}_{most_recent_file$file_date}.Rds" ))
    download_count<-download_count+1
    }
  
  # read it in
  if(readin==TRUE){
    # Dynamically assign the loaded RDS to a variable in the global environment named after the my_file stub string
    assign(my_file, readRDS(here("data", "main", most_recent_file$name)))
  }

}

message("Files expected: " , length(files_to_get))
message("Files downloaded: " , download_count)


#cleanup
rm(folder_info, most_recent_file, files_to_get, files_in_folder)
