# Name: groundfish_trips_catch.R
# Inputs: mrip_statistics_{file_date}.Rds, MRIP_COD_ALL_SITE_LIST.csv
# Outputs: rec_trips_catch
# Description: Builds a long-format data frame of directed trip counts and catch 
# (harvest, discards, total) for Atlantic Cod and Haddock in the Western Gulf 
# of Maine (WGOM), sourced from MRIP trip-level microdata.
# Output object: cod_haddock
# General strategy:
#  1. Read in data
#  2. Expand the survey data for trips and catch
#  3. De-duplicate trips
#  4. reshape and add descriptive columns


# load packages
library(dplyr)
library(readr)
library("mriptacklebox")
library(tidyverse)
library(here)
library(glue)
library(conflicted)
conflicted::conflicts_prefer(dplyr::filter) # resolve conflict with stats::filter
here::i_am("stored_scripts/groundfish_trips_catch.R")


#######################################################################  
###########OPTIONS
# intended to help with "function-izing"
#######################################################################  

yearlist<-c("2024", "2025") 
statelist<- c("25", "23", "33") # 25 is MA, 23 is ME, 33 is NH
common_name1<-'ATLANTIC COD'
common_name2<-'HADDOCK'

fishery<-"NE Groundfish"

file_date<-"2026-06-09" # date stamp of the MRIP data pull; used for file lookup and data_version field



run_date<-as.Date(file_date)

#######################################################################  
######  Read in Data  ######
#######################################################################  

# NOTE: glue and here are used to dynamically locate the file. Ensure the raw folder exists.
filename <- here("data","raw",glue("mrip_statistics_{file_date}.Rds"))
mrip_statistics <- read_rds(filename) # comes from get_mrip(), which returned a named list with elements: trip, catch, size, size_b2



# Load the elements in the list 
trip<-mrip_statistics$trip
catch<-mrip_statistics$catch
#size<-mrip_statistics$size
#size_b2<-mrip_statistics$size_b2

# convert column names and text to lowercase
names(trip) <- tolower(names(trip))
trip[] <- lapply(trip, function(x) if(is.character(x)) tolower(x) else x)
names(catch) <- tolower(names(catch))
catch[] <- lapply(catch, function(x) if(is.character(x)) tolower(x) else x)


#######################################################################  
######  Expand the survey data for trips  ######
#######################################################################  

#### Cod effort ####
# typ = c('PRIM1', 'A', 'B1', 'B2') captures directed trips where cod was 
# the primary target OR was caught (kept, unobserved dead, or released)
cod_effort <- mrip_effort(dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'INTSITE', 
                                  'STRAT_ID', 'PSU_ID', 'ID_CODE', 'LEADER'),
                      microdata = mrip_statistics,
                      dir_trip = list(comname = common_name1,
                                      typ = c('PRIM1', 'A', 'B1', 'B2')))|>
  filter(ST %in% statelist) |>
  filter(YEAR %in% yearlist)
                
names(cod_effort) <- tolower(names(cod_effort))
cod_effort[] <- lapply(cod_effort, function(x) if(is.character(x)) tolower(x) else x)
cod_effort <- subset(cod_effort, select = -c(dir_trip_typ, hrsf)) # drop: direction trip type flag and hours fished, not needed downstream

#######################################################################  
######  Expand the survey data for catch  ######
#######################################################################  

#### Cod Catch ####
# estimate_var=FALSE speeds up processing by skipping variance/SE/CV calculation
cod_catch <- mrip_catch(comname = common_name1, 
                        dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'STRAT_ID', 
                                'PSU_ID', 'ID_CODE', 'WP_INT'), 
                        microdata = mrip_statistics, estimate_var = FALSE) 

## pull out estimates
cod_catch <- cod_catch$estimates%>%
  filter(ST %in% statelist) |>
  filter(YEAR %in% yearlist)


names(cod_catch) <- tolower(names(cod_catch))
cod_catch[] <- lapply(cod_catch, function(x) if(is.character(x)) tolower(x) else x)
cod_catch <- subset(cod_catch, select = -c(se, cv)) # se and cv only populated when estimate_var=TRUE


# Merge effort and catch
cod_effort$source <- "effort"
cod_catch$source <- "catch"
cod_effort_catch <- left_join(cod_effort, cod_catch, # left join: retains all effort rows; unmatched catch rows are NA
                              by = c("common", "year", "wave", "mode_fx", "st", 
                                     "strat_id", "psu_id", "id_code"))

## some trips without catch, keep them, will assign claim=0 down below
cod_effort_catch %>% count(source.x, source.y) # diagnostic: shows how many rows matched vs. effort-only

# Parse interview date from id_code; positions 6-13 = YYYYMMDD
cod_effort_catch$date <- substr(cod_effort_catch$id_code, 6, 13)
cod_effort_catch$month <- substr(cod_effort_catch$date, 5, 6)
cod_effort_catch$day <- substr(cod_effort_catch$date, 7, 8)
cod_effort_catch <- cod_effort_catch %>% filter(!(day %in% c("9x", "xx"))) # drop records with imputed/unknown interview day

#### Haddock effort ####
hadd_effort <- mrip_effort(dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'INTSITE', 
                                  'STRAT_ID', 'PSU_ID', 'ID_CODE', 'LEADER'),
                      microdata = mrip_statistics,
                      dir_trip = list(comname = common_name2,
                                        typ = c('PRIM1', 'A', 'B1', 'B2')))|>
                      filter(ST %in% statelist) |>
                      filter(YEAR %in% yearlist)

names(hadd_effort) <- tolower(names(hadd_effort))
hadd_effort[] <- lapply(hadd_effort, function(x) if(is.character(x)) tolower(x) else x)
hadd_effort <- subset(hadd_effort, select = -c(dir_trip_typ, hrsf))


#### Haddock catch ####
hadd_catch <- mrip_catch(comname = common_name2, 
                        dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'STRAT_ID', 
                                'PSU_ID', 'ID_CODE', 'WP_INT'), 
                        microdata = mrip_statistics, estimate_var = FALSE)

hadd_catch <- hadd_catch$estimates  |>
  filter(ST %in% statelist) |>
  filter(YEAR %in% yearlist)

names(hadd_catch) <- tolower(names(hadd_catch))
hadd_catch[] <- lapply(hadd_catch, function(x) if(is.character(x)) tolower(x) else x)
hadd_catch <- subset(hadd_catch, select = -c(se, cv))


# Merge effort and catch
hadd_effort$source <- "effort"
hadd_catch$source <- "catch"
hadd_effort_catch <- left_join(hadd_effort, hadd_catch, 
                              by = c("common", "year", "wave", "mode_fx", "st", 
                                     "strat_id", "psu_id", "id_code"))

# Parse interview date from id_code; same logic as cod above
hadd_effort_catch$date <- substr(hadd_effort_catch$id_code, 6, 13)
hadd_effort_catch$month <- substr(hadd_effort_catch$date, 5, 6)
hadd_effort_catch$day <- substr(hadd_effort_catch$date, 7, 8)
hadd_effort_catch <- hadd_effort_catch %>% filter(!(day %in% c("9x", "xx")))


#######################################################################  
######  stack  data  ######
#######################################################################  
### APPEND cod and haddock ###
cod_hadd_all <- rbind(cod_effort_catch, hadd_effort_catch)

# Recode numeric mode_fx to readable labels; mode_fx 1/2/3 = shore modes
cod_hadd_all <- cod_hadd_all %>%
  mutate(mode = case_when(
    mode_fx == 3|mode_fx==2|mode_fx==1 ~ "shore",
    mode_fx == 5 ~ "charter",
    mode_fx == 7 ~ "private",
    mode_fx == 4 ~ "headboat"
  ))

# Recode FIPS state codes to abbreviations
cod_hadd_all <- cod_hadd_all %>%
  mutate(state = case_when(
    st == "25" ~ "MA",
    st == "33" ~ "NH",
    st == "23" ~ "ME"
  ))

cod_hadd_all <- rename(cod_hadd_all, dtrip = n_trip) # n_trip = estimated directed trips from mrip_effort()
#remove spaces in 'atlantic cod'
cod_hadd_all$common <- gsub(" ", "", cod_hadd_all$common)

#For trips with no catch data, assign variable as claim and value=0 
# "claim" = angler reported targeting the species but had no catch
cod_hadd_all$value[is.na(cod_hadd_all$value)] <- 0
cod_hadd_all$variable[is.na(cod_hadd_all$variable)] <- "claim"

#######################################################################  
######  Reshape, de-duplicate trips, handle group catch######
#######################################################################  

###### DIRECTED TRIPS ######
## Wide out the catch variables 
# Pivots long variable/value columns to wide; each catch category (claim, harvest, release, etc.) becomes a column
cod_hadd_all_w <- cod_hadd_all %>% spread(key = variable, value = value)


## Deal with Group catch
# Label trips based on species caught 
# NOTE: doing this before distinct() isn't strictly necessary but is safe.
trip_species_composition <- cod_hadd_all_w %>%
  group_by(id_code) %>%
  summarize(
    has_cod = any(common == "atlanticcod"),
    has_haddock = any(common == "haddock")
  ) %>%
  ungroup()

trip_species_composition <- trip_species_composition %>%
  mutate(trip_category = case_when(
    has_cod & !has_haddock ~ "cod_only",
    has_haddock & !has_cod ~ "hadd_only",
    has_cod & has_haddock  ~ "cod_and_hadd"
  ))

#merge in species composition
cod_hadd_all_w <- left_join(cod_hadd_all_w, trip_species_composition, by = c("id_code"))
##################################################################################
## Drop duplicate cod AND haddock trips
# distinct() keeps the first occurrence of an id_code.
# This prevents double-counting effort for trips that caught/targeted both species.
##################################################################################

cod_hadd_all_w <- cod_hadd_all_w %>%
  distinct(id_code, .keep_all = TRUE)


### Read in Cod Site List (stock and stat areas) ###
# Maps MRIP interview sites (intsite) to NMFS stat areas; used to identify WGOM trips
cod_site_list <- read.csv(here("data","raw","MRIP_COD_ALL_SITE_LIST.csv"))
names(cod_site_list) <- tolower(names(cod_site_list))
cod_site_list <- cod_site_list %>% filter(state %in% c("MA", "ME")) # NH handled separately by state rule below
cod_site_list <- subset(cod_site_list, select = c(state, intsite, nmfs_stock_area, nmfs_stat_area))

# Take 1st unique obs in the group 
# Sorting guarantees consistent retention during distinct() deduplication.
cod_site_list <- cod_site_list[order(cod_site_list$intsite, cod_site_list$nmfs_stock_area), ]
cod_site_list <- cod_site_list %>% distinct(nmfs_stock_area, intsite, nmfs_stat_area, state, .keep_all = TRUE)

## WGOM: 513 514 515 521 526 NH
cod_site_list <- cod_site_list %>%
  mutate(wgom = case_when(
    nmfs_stat_area %in% c(513, 514, 515 ,521,526)  ~ 1,
    TRUE ~ 0 # Catch-all for all other cases
  ))


## Merge cod sites in 
cod_hadd_all_w <- left_join(cod_hadd_all_w, cod_site_list, by = c("state", "intsite"))

# label NH trips as part of WGOM and fill in their stat area as "NH"
# NH sites are not in the site list so wgom would be NA without this override
cod_hadd_all_w <- cod_hadd_all_w %>%
  mutate(wgom = if_else(state == "NH", 1, wgom))
cod_hadd_all_w$nmfs_stat_area <- as.character(cod_hadd_all_w$nmfs_stat_area)
cod_hadd_all_w <- cod_hadd_all_w %>%
  mutate(nmfs_stat_area = if_else(state == "NH", "NH", nmfs_stat_area))

## keep if WGOM
cod_hadd_all_w <- cod_hadd_all_w %>% 
  filter(wgom == 1)


# Remove clutter in environment.  Keeping the workspace clean is good practice for memory, but these are commented out. 
#rm(cod_catch, cod_effort, cod_effort_catch, hadd_catch, hadd_effort, hadd_effort_catch,
#   cod_hadd_all, cod_site_list, trip_species_composition)


## Other variables for our dataframe

# Data_version is this when pulling MRIP and running this script on same day:
#cod_hadd_all_w$data_version <- Sys.Date()
# otherwise use date from the Rds file read in at the top 
cod_hadd_all_w$data_version <- run_date

cod_hadd_all_w$stock_abbrev <- "WGOM"
cod_hadd_all_w$metric <- "directed trips"
cod_hadd_all_w$units <- "number of trips"
cod_hadd_all_w$fishery <- fishery
cod_hadd_all_w$wave <- as.numeric(cod_hadd_all_w$wave)
cod_hadd_all_w$year <- as.numeric(cod_hadd_all_w$year)


# Sum directed trips across all trips in each stratum cell
cod_hadd_trips <- cod_hadd_all_w %>%
  group_by(fishery, stock_abbrev, state, mode, data_version, year, wave, metric, units) %>%
  summarise(value = sum(dtrip, na.rm = TRUE))

# Fill these in as NA for cod/haddock trips then reorder the columns
# Directed trips are not species-specific (mixed trips counted once); species fields set to NA
cod_hadd_trips$species_itis <- NA
cod_hadd_trips$common <- NA
cod_hadd_trips <- cod_hadd_trips %>% 
  select(fishery, common, species_itis, stock_abbrev, state, mode, data_version, year, wave, metric, value, units)




###### CATCH ###### 
## Wide out the catch variables 
# Second wide pivot from cod_hadd_all — does NOT deduplicate by id_code,
# so both cod and haddock rows survive for separate species-level catch totals
cod_hadd_all_w2 <- cod_hadd_all %>% spread(key = variable, value = value)

# Replace other missing catch variables with 0
cod_hadd_all_w2$harvest[is.na(cod_hadd_all_w2$harvest)] <- 0
cod_hadd_all_w2$release[is.na(cod_hadd_all_w2$release)] <- 0
cod_hadd_all_w2$landing[is.na(cod_hadd_all_w2$landing)] <- 0
cod_hadd_all_w2$tot_cat[is.na(cod_hadd_all_w2$tot_cat)] <- 0

# Label trips based on species they caught 
trip_species_composition <- cod_hadd_all_w2 %>%
  group_by(id_code) %>%
  summarize(
    has_cod = any(common == "atlanticcod"),
    has_haddock = any(common == "haddock")
  ) %>%
  ungroup()

trip_species_composition <- trip_species_composition %>%
  mutate(trip_category = case_when(
    has_cod & !has_haddock ~ "cod_only",
    has_haddock & !has_cod ~ "hadd_only",
    has_cod & has_haddock  ~ "cod_and_hadd"
  ))

cod_hadd_all_w2 <- left_join(cod_hadd_all_w2, trip_species_composition, by = c("id_code"))


### Read in cod sites (stock and stat areas)
# cod_site_list already built above; reused here for the catch data frame
cod_hadd_all_w2 <- left_join(cod_hadd_all_w2, cod_site_list, by = c("state", "intsite"))

# label NH trips as part of WGOM and fill in their stat area as "NH"
cod_hadd_all_w2 <- cod_hadd_all_w2 %>%
  mutate(wgom = if_else(state == "NH", 1, wgom))
cod_hadd_all_w2$nmfs_stat_area <- as.character(cod_hadd_all_w2$nmfs_stat_area)
cod_hadd_all_w2 <- cod_hadd_all_w2 %>%
  mutate(nmfs_stat_area = if_else(state == "NH", "NH", nmfs_stat_area))

## keep if WGOM
cod_hadd_all_w2 <- cod_hadd_all_w2 %>% 
  filter(wgom == 1)

# Remove clutter in environment
#rm(cod_catch, cod_effort, cod_effort_catch, hadd_catch, hadd_effort, hadd_effort_catch,
#   cod_hadd_all, cod_site_list, trip_species_composition)


### Other variables for our dataframe
## grab tsn codes, create species_itis variable
# Extract the first non-NA ITIS TSN for each species from the raw trip microdata
subset_values <- trip$tsn1[trip$prim1_common == "atlantic cod"]
tsn_cod <- subset_values[!is.na(subset_values)][1]
subset_values <- trip$tsn1[trip$prim1_common == "haddock"]
tsn_hadd <- subset_values[!is.na(subset_values)][1]

cod_hadd_all_w2 <- cod_hadd_all_w2 %>%
  mutate(species_itis = ifelse(common == "atlanticcod", tsn_cod,
                               ifelse(common == "haddock", tsn_hadd, NA)))

cod_hadd_all_w2$species_itis <- as.numeric(cod_hadd_all_w2$species_itis)

# Data_version is this when pulling MRIP and running this script on same day
#cod_hadd_all_w$data_version <- Sys.Date()
# Otherwise use date from the Rds file read in at the top 
cod_hadd_all_w2$data_version <- run_date

cod_hadd_all_w2$stock_abbrev <- "WGOM"
cod_hadd_all_w2$units <- "number of fish"
cod_hadd_all_w2$fishery <- fishery
cod_hadd_all_w2$wave <- as.numeric(cod_hadd_all_w2$wave)
cod_hadd_all_w2$year <- as.numeric(cod_hadd_all_w2$year)


# Sum catch components by species within each stratum cell
# harvest = kept + unobserved (A+B1); discards = released alive (B2); catch = tot_cat (harvest + discards)
# harvest is A+B1 although the mrip data calls it 'landing'. Below we rename landing as harvest.
# B1 'unobserved' is dead fish not available to the interviewer (ie, dead discards, fileted, given away)
cod_hadd_all_w2 <- cod_hadd_all_w2 %>%
  group_by(fishery, common, species_itis, stock_abbrev, state, mode, data_version, year, wave, units) %>%
  summarise(harvest = sum(landing, na.rm = TRUE),
            discards = sum(release, na.rm = TRUE),
            catch = sum(tot_cat, na.rm = TRUE))

#Long out the catch variables, make metric column,  reorder columns
# gather() pivots data back to long, creating separate rows for harvest/discards/catch estimates
cod_hadd_catch <- cod_hadd_all_w2 %>% 
  gather(key = "metric", value = "value", harvest, discards, catch, na.rm = T)

cod_hadd_catch <- cod_hadd_catch %>% 
  select(fishery, common, species_itis, stock_abbrev, state, mode, data_version, year, wave, metric, value, units)


#### Append trips and catch ####
# Final output: one long-format data frame combining directed trip counts and catch metrics
# metric values: "directed trips", "harvest", "discards", "catch"
# units vary by metric: "number of trips" (directed trips) vs "number of fish" (catch metrics)
rec_trips_catch <- rbind(cod_hadd_trips, cod_hadd_catch)

write.csv(rec_trips_catch, file = here("data/main/trip_catch.csv"))

# look at it.
rec_trips_catch %>% 
    ungroup() %>%
    group_by(metric, year, common) %>% 
    summarise(value=sum(value))


