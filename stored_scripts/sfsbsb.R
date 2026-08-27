# Name: sfsbsb.R
# Inputs: mrip_pull{file_date}.Rds
# Outputs: sfsbsb_trips_catch
# Description: Builds a long-format data frame of directed trip counts and catch 
# (harvest, discards, total) for Mid-Atlantic Summer Flounder, Scup, and Black Sea   
# Bass sourced from MRIP trip-level microdata.
# Output object: 
# General strategy:
#  1. Read in data
#  2. Expand the survey data for trips and catch
#  3. De-duplicate trips
#  4. reshape and add descriptive columns


# load packages
library(dplyr)
library(readr)
library(mriptacklebox)
library(tidyverse)
library(here)
library(glue)
library(conflicted)
conflicted::conflicts_prefer(dplyr::filter) # resolve conflict with stats::filter
here::i_am("stored_scripts/sfsbsb.R")

 
#######################################################################  
###########OPTIONS
# intended to help with "function-izing"
#######################################################################  

yearlist<-c("2024", "2025") 
# 25 is MA, 23 is ME, 33 is NH, 9 is CT, 10 is DE, 24 is MD, 34 is NJ, 36 is NY, 44 is RI, 51 is VA, 37 is NC
statelist<- c("25", "09", "10", "24", "34", "36", "44", "51", "37")  
common_name1<-'SUMMER FLOUNDER'
common_name2<-'SCUP'
common_name3<-'BLACK SEA BASS'

fishery<-"SFSBSB"


#######################################################################  
######  Read in Data  ######
#######################################################################  
# To pull in most recent mrip file (to get this file, need to run get_mrip_oracle.R in flukeRDm repo)
folder <- "C:/Users/theresa.petesch/Documents/GitHub/flukeRDM/Data/2028_mgt_cycle/miscellaneous"
#folder<-file.path("GitHub","flukeRDM","Data","2028_mgt_cycle","miscellaneous")
vintage_string<-list.files(folder, pattern=glob2rx("mrip_pull*Rds"))
vintage_string<-gsub("mrip_pull","",vintage_string)
vintage_string<-gsub(".Rds","",vintage_string)
data_vintage<-max(vintage_string)
run_date<-as.Date(data_vintage)

# NOTE: glue and here are used to dynamically locate the file. Ensure the raw folder exists.
filename <- file.path(folder,glue("mrip_pull{data_vintage}.Rds"))
#filename <- here("GitHub","flukeRDM","Data","2028_mgt_cycle","miscellaneous",glue("mrip_pull{data_vintage}.Rds"))
mrip_pull <- read_rds(filename)  # a named list with elements: trip, catch, size, size_b2

# Load the elements in the list 
trip<-mrip_pull$trip
catch<-mrip_pull$catch
# convert column names and text to lowercase
names(trip) <- tolower(names(trip))
trip[] <- lapply(trip, function(x) if(is.character(x)) tolower(x) else x)
names(catch) <- tolower(names(catch))
catch[] <- lapply(catch, function(x) if(is.character(x)) tolower(x) else x)

#######################################################################  
######  Expand the survey data for trips and catch  ######
#######################################################################  

#### SF effort ####
# typ = c('PRIM1', 'A', 'B1', 'B2') captures directed trips where sf was 
# the primary target OR was caught (kept, unobserved dead, or released)
sf_effort <- mrip_effort(dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'INTSITE', 'CNTY',
                                  'STRAT_ID', 'PSU_ID', 'ID_CODE', 'LEADER'),
                          microdata = mrip_pull,
                          dir_trip = list(comname = common_name1,
                                          typ = c('PRIM1', 'A', 'B1', 'B2')))|>
  filter(ST %in% statelist) |>
  filter(YEAR %in% yearlist)

names(sf_effort) <- tolower(names(sf_effort))
sf_effort[] <- lapply(sf_effort, function(x) if(is.character(x)) tolower(x) else x)

#### SF Catch ####
# estimate_var=FALSE speeds up processing by skipping variance/SE/CV calculation
sf_catch <- mrip_catch(comname = common_name1, 
                        dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'STRAT_ID', 
                                'PSU_ID', 'ID_CODE', 'WP_INT'), 
                        microdata = mrip_pull, estimate_var = FALSE) 

## pull out estimates
sf_catch <- sf_catch$estimates%>%
  filter(ST %in% statelist) |>
  filter(YEAR %in% yearlist)

names(sf_catch) <- tolower(names(sf_catch))
sf_catch[] <- lapply(sf_catch, function(x) if(is.character(x)) tolower(x) else x)
sf_catch <- subset(sf_catch, select = -c(se, cv)) # se and cv only populated when estimate_var=TRUE


# Merge effort and catch
sf_effort$source <- "effort"
sf_catch$source <- "catch"
sf_effort_catch <- left_join(sf_effort, sf_catch, # left join: retains all effort rows; unmatched catch rows are NA
                              by = c("common", "year", "wave", "mode_fx", "st", 
                                     "strat_id", "psu_id", "id_code"))

## some trips without catch, keep them, will assign claim=0 down below
sf_effort_catch %>% count(source.x, source.y) # diagnostic: shows how many rows matched vs. effort-only

# Parse interview date from id_code; positions 6-13 = YYYYMMDD
sf_effort_catch$date <- substr(sf_effort_catch$id_code, 6, 13)
sf_effort_catch$month <- substr(sf_effort_catch$date, 5, 6)
sf_effort_catch$day <- substr(sf_effort_catch$date, 7, 8)
sf_effort_catch <- sf_effort_catch %>% filter(!(day %in% c("9x", "xx"))) # drop records with imputed/unknown interview day



#### Scup effort ####
sc_effort <- mrip_effort(dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'INTSITE', 'CNTY',
                                 'STRAT_ID', 'PSU_ID', 'ID_CODE', 'LEADER'),
                         microdata = mrip_pull,
                         dir_trip = list(comname = common_name2,
                                         typ = c('PRIM1', 'A', 'B1', 'B2')))|>
  filter(ST %in% statelist) |>
  filter(YEAR %in% yearlist)

names(sc_effort) <- tolower(names(sc_effort))
sc_effort[] <- lapply(sc_effort, function(x) if(is.character(x)) tolower(x) else x)

#### Scup Catch ####
sc_catch <- mrip_catch(comname = common_name2, 
                       dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'STRAT_ID', 
                               'PSU_ID', 'ID_CODE', 'WP_INT'), 
                       microdata = mrip_pull, estimate_var = FALSE) 

sc_catch <- sc_catch$estimates%>%
  filter(ST %in% statelist) |>
  filter(YEAR %in% yearlist)

names(sc_catch) <- tolower(names(sc_catch))
sc_catch[] <- lapply(sc_catch, function(x) if(is.character(x)) tolower(x) else x)
sc_catch <- subset(sc_catch, select = -c(se, cv)) # se and cv only populated when estimate_var=TRUE


# Merge effort and catch
sc_effort$source <- "effort"
sc_catch$source <- "catch"
sc_effort_catch <- left_join(sc_effort, sc_catch, # left join: retains all effort rows; unmatched catch rows are NA
                             by = c("common", "year", "wave", "mode_fx", "st", 
                                    "strat_id", "psu_id", "id_code"))

## some trips without catch, keep them, will assign claim=0 down below
sc_effort_catch %>% count(source.x, source.y) # diagnostic: shows how many rows matched vs. effort-only

# Parse interview date from id_code; same logic as above
sc_effort_catch$date <- substr(sc_effort_catch$id_code, 6, 13)
sc_effort_catch$month <- substr(sc_effort_catch$date, 5, 6)
sc_effort_catch$day <- substr(sc_effort_catch$date, 7, 8)
sc_effort_catch <- sc_effort_catch %>% filter(!(day %in% c("9x", "xx"))) # drop records with imputed/unknown interview day





#### BSB effort ####
bsb_effort <- mrip_effort(dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'INTSITE', 'CNTY', 
                                 'STRAT_ID', 'PSU_ID', 'ID_CODE', 'LEADER'),
                         microdata = mrip_pull,
                         dir_trip = list(comname = common_name3,
                                         typ = c('PRIM1', 'A', 'B1', 'B2')))|>
  filter(ST %in% statelist) |>
  filter(YEAR %in% yearlist)

names(bsb_effort) <- tolower(names(bsb_effort))
bsb_effort[] <- lapply(bsb_effort, function(x) if(is.character(x)) tolower(x) else x)

#### BSB Catch ####
bsb_catch <- mrip_catch(comname = common_name3, 
                       dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'STRAT_ID', 
                               'PSU_ID', 'ID_CODE', 'WP_INT'), 
                       microdata = mrip_pull, estimate_var = FALSE) 

bsb_catch <- bsb_catch$estimates%>%
  filter(ST %in% statelist) |>
  filter(YEAR %in% yearlist)

names(bsb_catch) <- tolower(names(bsb_catch))
bsb_catch[] <- lapply(bsb_catch, function(x) if(is.character(x)) tolower(x) else x)
bsb_catch <- subset(bsb_catch, select = -c(se, cv)) # se and cv only populated when estimate_var=TRUE


# Merge effort and catch
bsb_effort$source <- "effort"
bsb_catch$source <- "catch"
bsb_effort_catch <- left_join(bsb_effort, bsb_catch, # left join: retains all effort rows; unmatched catch rows are NA
                             by = c("common", "year", "wave", "mode_fx", "st", 
                                    "strat_id", "psu_id", "id_code"))

## some trips without catch, keep them, will assign claim=0 down below
bsb_effort_catch %>% count(source.x, source.y) # diagnostic: shows how many rows matched vs. effort-only

# Parse interview date from id_code; same logic as above
bsb_effort_catch$date <- substr(bsb_effort_catch$id_code, 6, 13)
bsb_effort_catch$month <- substr(bsb_effort_catch$date, 5, 6)
bsb_effort_catch$day <- substr(bsb_effort_catch$date, 7, 8)
bsb_effort_catch <- bsb_effort_catch %>% filter(!(day %in% c("9x", "xx"))) # drop records with imputed/unknown interview day



#######################################################################  
######  Stack  data  ######
#######################################################################  

### APPEND the three species ###
sfsbsb_all <- rbind(sf_effort_catch, sc_effort_catch, bsb_effort_catch)

# Recode numeric mode_fx to readable labels; mode_fx 1/2/3 = shore modes
sfsbsb_all <- sfsbsb_all %>%
  mutate(mode = case_when(
    mode_fx == 3|mode_fx==2|mode_fx==1 ~ "shore",
    mode_fx == 5 ~ "charter",
    mode_fx == 7 ~ "private",
    mode_fx == 4 ~ "headboat"
  ))

# Recode FIPS state codes to abbreviations
sfsbsb_all <- sfsbsb_all %>%
  mutate(state = case_when(
    st == "25" ~ "MA",
    st == "33" ~ "NH",
    st == "23" ~ "ME",
    st == "09"  ~ "CT",
    st == "10" ~ "DE",
    st == "24" ~ "MD",
    st == "34" ~ "NJ",
    st == "36" ~ "NY",
    st == "44" ~ "RI",
    st == "51" ~ "VA",
    st == "37" ~ "NC"
  ))

## Row count by state
sfsbsb_all %>% 
  count(state)

## Count rows south of Cape Hatteras (it's over 8k rows out of about 10k in NC that are south...)
sum(sfsbsb_all$state == "NC" & !(sfsbsb_all$cnty %in% c("015", "029", "041", "053", "055", "139", "143", "177", "187")), na.rm = TRUE)

## Drop NC trips south of Cape Hatteras
## Very few SF and Scup trips in southern NC, and BSB only managed in northern NC
sfsbsb_all <- sfsbsb_all %>%
  filter(!(state == "NC" & !(cnty %in% c("015", "029", "041", "053", "055", "139", "143", "177", "187"))))

sfsbsb_all %>%
  filter(state == "NC") %>%
  count(cnty)

sfsbsb_all <- rename(sfsbsb_all, dtrip = n_trip) # n_trip = estimated directed trips from mrip_effort()
#remove spaces in 'summer flounder', etc
sfsbsb_all$common <- gsub(" ", "", sfsbsb_all$common)

#For trips with no catch data, assign variable as claim and value=0 
# "claim" = angler reported targeting the species but had no catch
sfsbsb_all$value[is.na(sfsbsb_all$value)] <- 0
sfsbsb_all$variable[is.na(sfsbsb_all$variable)] <- "claim"




###### DIRECTED TRIPS ######
## Wide out the catch variables 
# Pivots long variable/value columns to wide; each catch category (claim, harvest, release, etc.) becomes a column
sfsbsb_all_w <- sfsbsb_all %>% spread(key = variable, value = value)


## Deal with Group catch
# Label trips based on species caught 
# NOTE: doing this before distinct() isn't strictly necessary but is safe.
trip_species_composition <- sfsbsb_all_w %>%
  group_by(id_code) %>%
  summarize(
    has_sf = any(common == "summerflounder"),
    has_sc = any(common == "scup"),
    has_bsb = any(common == "blackseabass")
  ) %>%
  ungroup()

trip_species_composition <- trip_species_composition %>%
  mutate(trip_category = case_when(
    has_sf & !has_sc & !has_bsb ~ "sf_only",
    !has_sf & has_sc & !has_bsb ~ "sc_only",
    !has_sf & !has_sc & has_bsb ~ "bsb_only",
    has_sf & has_sc & !has_bsb ~ "sfsc_only",
    has_sf & !has_sc & has_bsb ~ "sfbsb_only",
    has_sf & has_sc & has_bsb ~ "sfsbsb_all",
    !has_sf & has_sc & has_bsb  ~ "scbsb_only"
  ))

#merge in species composition
sfsbsb_all_w <- left_join(sfsbsb_all_w, trip_species_composition, by = c("id_code"))

sfsbsb_all_w %>%
  count(trip_category)

##################################################################################
## Drop duplicate trips
# distinct() keeps the first occurrence of an id_code.
# This prevents double-counting effort for trips that caught/targeted both species.
##################################################################################
sfsbsb_all_w <- sfsbsb_all_w %>%
  distinct(id_code, .keep_all = TRUE)


# Remove clutter in environment.  Keeping the workspace clean is good practice for memory. 
rm(sf_catch, sf_effort, sf_effort_catch, sc_catch, sc_effort, sc_effort_catch, 
   bsb_catch, bsb_effort, bsb_effort_catch, trip_species_composition)


## Other variables for our dataframe

# Use date from the Rds file read in at the top (WILL REMOVE QUOTATION MARKS SO IT FILLS IN THE DATE)
sfsbsb_all_w$data_version <- run_date

sfsbsb_all_w$metric <- "directed trips"
sfsbsb_all_w$units <- "number of trips"
sfsbsb_all_w$fishery <- fishery
sfsbsb_all_w$wave <- as.numeric(sfsbsb_all_w$wave)
sfsbsb_all_w$year <- as.numeric(sfsbsb_all_w$year)

# Sum directed trips across all trips in each stratum cell
sfsbsb_trips <- sfsbsb_all_w %>%
  group_by(fishery, state, mode, data_version, year, wave, metric, units) %>%
  summarise(value = sum(dtrip, na.rm = TRUE))

# Fill these in as NA for trips then reorder the columns
# Directed trips are not species-specific (mixed trips counted once); species fields set to NA
sfsbsb_trips$species_itis <- NA
sfsbsb_trips$common <- NA
sfsbsb_trips <- sfsbsb_trips %>% 
  select(fishery, common, species_itis, state, mode, data_version, year, wave, metric, value, units)




###### CATCH ###### 
## Wide out the catch variables 
# Second wide pivot from sfsbsb_all — does NOT deduplicate by id_code,
# so all rows survive for separate species-level catch totals
sfsbsb_all_w2 <- sfsbsb_all %>% spread(key = variable, value = value)

# Replace other missing catch variables with 0
sfsbsb_all_w2$harvest[is.na(sfsbsb_all_w2$harvest)] <- 0
sfsbsb_all_w2$release[is.na(sfsbsb_all_w2$release)] <- 0
sfsbsb_all_w2$landing[is.na(sfsbsb_all_w2$landing)] <- 0
sfsbsb_all_w2$tot_cat[is.na(sfsbsb_all_w2$tot_cat)] <- 0

# Label trips based on species they caught 
trip_species_composition <- sfsbsb_all_w %>%
  group_by(id_code) %>%
  summarize(
    has_sf = any(common == "summerflounder"),
    has_sc = any(common == "scup"),
    has_bsb = any(common == "blackseabass")
  ) %>%
  ungroup()

trip_species_composition <- trip_species_composition %>%
  mutate(trip_category = case_when(
    has_sf & !has_sc & !has_bsb ~ "sf_only",
    !has_sf & has_sc & !has_bsb ~ "sc_only",
    !has_sf & !has_sc & has_bsb ~ "bsb_only",
    has_sf & has_sc & !has_bsb ~ "sfsc_only",
    has_sf & !has_sc & has_bsb ~ "sfbsb_only",
    has_sf & has_sc & has_bsb ~ "sfsbsb_all",
    !has_sf & has_sc & has_bsb  ~ "scbsb_only"
  ))

#merge in species composition
sfsbsb_all_w2 <- left_join(sfsbsb_all_w2, trip_species_composition, by = c("id_code"))

sfsbsb_all_w2 %>%
  count(trip_category)



### Other variables for our dataframe
## grab tsn codes, create species_itis variable
# Extract the first non-NA ITIS TSN for each species from the raw trip microdata
subset_values <- trip$tsn1[trip$prim1_common == "summer flounder"]
tsn_sf <- subset_values[!is.na(subset_values)][1]
subset_values <- trip$tsn1[trip$prim1_common == "scup"]
tsn_sc <- subset_values[!is.na(subset_values)][1]
subset_values <- trip$tsn1[trip$prim1_common == "black sea bass"]
tsn_bsb <- subset_values[!is.na(subset_values)][1]

sfsbsb_all_w2 <- sfsbsb_all_w2 %>%
  mutate(species_itis = ifelse(common == "summerflounder", tsn_sf,
                               ifelse(common == "scup", tsn_sc,
                               ifelse(common == "blackseasbass", tsn_bsb, NA))))

sfsbsb_all_w2$species_itis <- as.numeric(sfsbsb_all_w2$species_itis)


# Use date from the Rds file read in at the top (REMOVE QUOTATION MARKS SO IT FILLS IN THE DATE)
sfsbsb_all_w2$data_version <- run_date

sfsbsb_all_w2$units <- "number of fish"
sfsbsb_all_w2$fishery <- fishery
sfsbsb_all_w2$wave <- as.numeric(sfsbsb_all_w2$wave)
sfsbsb_all_w2$year <- as.numeric(sfsbsb_all_w2$year)


# Sum catch components by species within each stratum cell
# harvest = kept + unobserved (A+B1); discards = released alive (B2); catch = tot_cat (harvest + discards)
# harvest is A+B1 although the mrip data calls it 'landing'. Below we rename landing as harvest.
# B1 'unobserved' is dead fish not available to the interviewer (ie, dead discards, fileted, given away)
sfsbsb_all_w2 <- sfsbsb_all_w2 %>%
  group_by(fishery, common, species_itis, state, mode, data_version, year, wave, units) %>%
  summarise(harvest = sum(landing, na.rm = TRUE),
            discards = sum(release, na.rm = TRUE),
            catch = sum(tot_cat, na.rm = TRUE))

#Long out the catch variables, make metric column,  reorder columns
# gather() pivots data back to long, creating separate rows for harvest/discards/catch estimates
sfsbsb_catch <- sfsbsb_all_w2 %>% 
  gather(key = "metric", value = "value", harvest, discards, catch, na.rm = T)

sfsbsb_catch <- sfsbsb_catch %>% 
  select(fishery, common, species_itis, state, mode, data_version, year, wave, metric, value, units)


#### Append trips and catch ####
# Final output: one long-format data frame combining directed trip counts and catch metrics
# metric values: "directed trips", "harvest", "discards", "catch"
# units vary by metric: "number of trips" (directed trips) vs "number of fish" (catch metrics)
sfsbsb_trips_catch <- rbind(sfsbsb_trips, sfsbsb_catch)

# Add in a column for source
sfsbsb_trips_catch <- sfsbsb_trips_catch %>%
  mutate(source = "MRIP")

# look at it.
sfsbsb_trips_catch %>% 
  ungroup() %>%
  group_by(metric, year, common) %>% 
  summarise(value=sum(value))


# checking state level harvest, they all look the numbers in lou's report for 2024 MRIP harvest
sfsbsb_trips_catch %>% 
  ungroup() %>%
  filter(state=="DE") %>% 
  filter(metric=="harvest") %>% 
  group_by(metric, year, common) %>% 
  summarise(value=sum(value))



##### Save file as Rds
output_folder <- file.path(here("data","main"))
SaveFile<-glue("trip_catch_sfsbsb{data_vintage}")
write_rds(sfsbsb_trips_catch, file=file.path(output_folder,glue("{SaveFile}.Rds")))


###### Push Rds to google drive ######

#Load libraries
library(haven)
library(googledrive)


# Connect to Google Drive
# NOTE: Relies on cached credentials in .secrets. Will prompt interactive auth if missing or expired.
drive_auth(cache = here(".secrets"), email = TRUE)

# Output folder on google drive
miscellaneous_path <-file.path("socialsci","RecreationalDST","2028_management_cycle_data",
                               "flukeRDM","miscellaneous")

folder_info <- drive_get(
  path = miscellaneous_path,
  shared_drive = "NMFS NEC READ SSB"
)
miscellaneous_path<-folder_info$id


## Push Rds to google drive
drive_upload(
  media = file.path(output_folder,glue("{SaveFile}.Rds")),
  path = as_id(miscellaneous_path),
  name = glue("{SaveFile}.Rds"),
  overwrite = TRUE
)



