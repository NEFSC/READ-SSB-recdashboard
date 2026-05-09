######  Directed trips for WGOM Cod and Haddock  ######

# load packages
library(dplyr)
library(readr)
library("mriptacklebox")
library(tidyverse)


# Run pull_mrip.R
# Or, if you've pulled the data recently, read in but check the date in the file name
filename <- "data/raw/mrip_statistics_2026-04-29.Rds"
mrip_statistics <- read_rds(filename)

# Extract the date if reading in a previous day's MRIP pull
file_date <- str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")


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



#### Cod effort ####
# set typ to pull trips where cod were stated as primary target OR were landed-A,
# unobserved-B1, or discarded-B2
cod_effort <- mrip_effort(dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'INTSITE', 
                                  'STRAT_ID', 'PSU_ID', 'ID_CODE', 'LEADER'),
                      microdata = mrip_statistics,
                      dir_trip = list(comname = 'ATLANTIC COD',
                                      typ = c('PRIM1', 'A', 'B1', 'B2')))|>
  dplyr::filter(ST %in% c("25", "23", "33") # 25 is MA, 23 is ME, 33 is NH
                & YEAR %in% c("2024", "2025"))

names(cod_effort) <- tolower(names(cod_effort))
cod_effort[] <- lapply(cod_effort, function(x) if(is.character(x)) tolower(x) else x)
cod_effort <- subset(cod_effort, select = -c(dir_trip_typ, hrsf))


#### Cod Catch ####
cod_catch <- mrip_catch(comname = 'ATLANTIC COD', 
                        dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'STRAT_ID', 
                                'PSU_ID', 'ID_CODE', 'WP_INT'), 
                        microdata = mrip_statistics, estimate_var = FALSE)

## pull out estimates
cod_catch <- cod_catch$estimates  |>
  dplyr::filter(ST %in% c("25", "23", "33") & YEAR %in% c("2024", "2025"))

names(cod_catch) <- tolower(names(cod_catch))
cod_catch[] <- lapply(cod_catch, function(x) if(is.character(x)) tolower(x) else x)
cod_catch <- subset(cod_catch, select = -c(se, cv))


# Merge effort and catch
cod_effort$source <- "effort"
cod_catch$source <- "catch"
cod_effort_catch <- left_join(cod_effort, cod_catch, 
                              by = c("common", "year", "wave", "mode_fx", "st", 
                                     "strat_id", "psu_id", "id_code"))

## some trips without catch, keep them, will assign claim=0 down below
cod_effort_catch %>% count(source.x, source.y)

cod_effort_catch$date <- substr(cod_effort_catch$id_code, 6, 13)
cod_effort_catch$month <- substr(cod_effort_catch$date, 5, 6)
cod_effort_catch$day <- substr(cod_effort_catch$date, 7, 8)
cod_effort_catch <- cod_effort_catch %>% filter(!(day %in% c("9x", "xx")))


#### Haddock effort ####
hadd_effort <- mrip_effort(dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'INTSITE', 
                                  'STRAT_ID', 'PSU_ID', 'ID_CODE', 'LEADER'),
                          microdata = mrip_statistics,
                          dir_trip = list(comname = 'HADDOCK',
                                          typ = c('PRIM1', 'A', 'B1', 'B2')))|>
  dplyr::filter(ST %in% c("25", "23", "33") # 25 is MA, 23 is ME, 33 is NH
                & YEAR %in% c("2024", "2025"))

names(hadd_effort) <- tolower(names(hadd_effort))
hadd_effort[] <- lapply(hadd_effort, function(x) if(is.character(x)) tolower(x) else x)
hadd_effort <- subset(hadd_effort, select = -c(dir_trip_typ, hrsf))


#### Haddock catch ####
hadd_catch <- mrip_catch(comname = 'HADDOCK', 
                        dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'STRAT_ID', 
                                'PSU_ID', 'ID_CODE', 'WP_INT'), 
                        microdata = mrip_statistics, estimate_var = FALSE)

hadd_catch <- hadd_catch$estimates  |>
  dplyr::filter(ST %in% c("25", "23", "33") & YEAR %in% c("2024", "2025"))

names(hadd_catch) <- tolower(names(hadd_catch))
hadd_catch[] <- lapply(hadd_catch, function(x) if(is.character(x)) tolower(x) else x)
hadd_catch <- subset(hadd_catch, select = -c(se, cv))


# Merge effort and catch
hadd_effort$source <- "effort"
hadd_catch$source <- "catch"
hadd_effort_catch <- left_join(hadd_effort, hadd_catch, 
                              by = c("common", "year", "wave", "mode_fx", "st", 
                                     "strat_id", "psu_id", "id_code"))

hadd_effort_catch$date <- substr(hadd_effort_catch$id_code, 6, 13)
hadd_effort_catch$month <- substr(hadd_effort_catch$date, 5, 6)
hadd_effort_catch$day <- substr(hadd_effort_catch$date, 7, 8)
hadd_effort_catch <- hadd_effort_catch %>% filter(!(day %in% c("9x", "xx")))



### APPEND cod and haddock ###
cod_hadd_all <- rbind(cod_effort_catch, hadd_effort_catch)

cod_hadd_all <- cod_hadd_all %>%
  mutate(mode = case_when(
    mode_fx == 3|mode_fx==2|mode_fx==1 ~ "shore",
    mode_fx == 5 ~ "charter",
    mode_fx == 7 ~ "private",
    mode_fx == 4 ~ "headboat"
  ))

cod_hadd_all <- cod_hadd_all %>%
  mutate(state = case_when(
    st == "25" ~ "MA",
    st == "33" ~ "NH",
    st == "23" ~ "ME"
  ))


#For trips with no catch data, assign variable as claim and value=0 
cod_hadd_all$value[is.na(cod_hadd_all$value)] <- 0
cod_hadd_all$variable[is.na(cod_hadd_all$variable)] <- "claim"



#### Wide out the catch variables 
cod_hadd_all_w <- cod_hadd_all %>% spread(key = variable, value = value)

cod_hadd_all_w <- rename(cod_hadd_all_w, dtrip = n_trip)
#remove spaces in 'atlantic cod'
cod_hadd_all_w$common <- gsub(" ", "", cod_hadd_all_w$common)


### Deal with Group catch
# Label trips based on species they caught (this code until drop duplicates isn't needed here)
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

## Drop duplicate cod AND haddock trips
cod_hadd_all_w <- cod_hadd_all_w %>%
  distinct(id_code, .keep_all = TRUE)



### Read in Cod Site List (stock and stat areas) ###
cod_site_list <- read.csv("data/raw/MRIP_COD_ALL_SITE_LIST.csv")
names(cod_site_list) <- tolower(names(cod_site_list))
cod_site_list <- cod_site_list %>% filter(state %in% c("MA", "ME"))
cod_site_list <- subset(cod_site_list, select = c(state, intsite, nmfs_stock_area, nmfs_stat_area))

# Take 1st unique obs in the group 
cod_site_list <- cod_site_list[order(cod_site_list$intsite, cod_site_list$nmfs_stock_area), ]
cod_site_list <- cod_site_list %>% distinct(nmfs_stock_area, intsite, nmfs_stat_area, state, .keep_all = TRUE)

## WGOM: 513 514 515 521 526 NH
cod_site_list <- cod_site_list %>%
  mutate(wgom = case_when(
    nmfs_stat_area == 513 | nmfs_stat_area == 514  ~ 1,
    nmfs_stat_area == 515 | nmfs_stat_area == 521  ~ 1,
    nmfs_stat_area == 526  ~ 1,
    TRUE ~ 0 # Catch-all for all other cases
  ))


## Merge cod sites in 
cod_hadd_all_w <- left_join(cod_hadd_all_w, cod_site_list, by = c("state", "intsite"))

# label NH trips as part of WGOM and fill in their stat area as "NH"
cod_hadd_all_w <- cod_hadd_all_w %>%
  mutate(wgom = if_else(state == "NH", 1, wgom))
cod_hadd_all_w$nmfs_stat_area <- as.character(cod_hadd_all_w$nmfs_stat_area)
cod_hadd_all_w <- cod_hadd_all_w %>%
  mutate(nmfs_stat_area = if_else(state == "NH", "NH", nmfs_stat_area))

## keep if WGOM
cod_hadd_all_w <- cod_hadd_all_w %>% 
  filter(wgom == 1)


# Remove clutter in environment
rm(cod_catch, cod_effort, cod_effort_catch, hadd_catch, hadd_effort, hadd_effort_catch,
   cod_hadd_all, cod_site_list, trip_species_composition)


### Other variables for our dataframe
## grab tsn codes, create species_itis variable
subset_values <- trip$tsn1[trip$prim1_common == "atlantic cod"]
tsn_cod <- subset_values[!is.na(subset_values)][1]
subset_values <- trip$tsn1[trip$prim1_common == "haddock"]
tsn_hadd <- subset_values[!is.na(subset_values)][1]

cod_hadd_all_w <- cod_hadd_all_w %>%
  mutate(species_itis = ifelse(common == "atlanticcod", tsn_cod,
                        ifelse(common == "haddock", tsn_hadd, NA)))

cod_hadd_all_w$species_itis <- as.numeric(cod_hadd_all_w$species_itis)

# Data_version is this when pulling MRIP and running this script on same day
#cod_hadd_all_w$data_version <- Sys.Date()
# otherwise use date from the Rds file read in at the top 
cod_hadd_all_w$data_version <- as.Date(file_date)

cod_hadd_all_w$stock_abbrev <- "WGOM"
cod_hadd_all_w$metric <- "directed trips"
cod_hadd_all_w$units <- "number of trips"
cod_hadd_all_w$fishery <- "NE Groundfish"
cod_hadd_all_w$wave <- as.numeric(cod_hadd_all_w$wave)
cod_hadd_all_w$year <- as.numeric(cod_hadd_all_w$year)


cod_hadd_trips <- cod_hadd_all_w %>%
  group_by(fishery, stock_abbrev, state, mode, data_version, year, wave, metric, units) %>%
  summarise(value = sum(dtrip, na.rm = TRUE))

# Fill these in as NA for cod/haddock trips then reorder the columns
cod_hadd_trips$species_itis <- NA
cod_hadd_trips$common <- NA
cod_hadd_trips <- cod_hadd_trips %>% 
  select(fishery, common, species_itis, stock_abbrev, state, mode, data_version, year, wave, metric, value, units)




##### cod_hadd_trips is now in our format, will append with catch and catch per trip
### Can stop here or see estimates by fishing year below



##Note: If we use the older pull from 4/10, the 2024 directed trips match lou's but
## 2025 does not because he was using older data when we ran his script to get yearly_mrip_stats.dta 
# The 4/29 pull must have had a tiny update to 2024 trips compared to the 4/10 pull
#mrip_statistics <- readRDS("~/GitHub/mrip_statistics_2026-04-10.Rds")


# FY variables
cod_hadd_trips1 <- cod_hadd_trips %>%
  mutate(fy2024 = case_when(
    year == 2024 & wave >= 3 ~ 1,
    year == 2025 & wave == 2 ~ 1,
    TRUE ~ 0 
  ))

cod_hadd_trips1 <- cod_hadd_trips1 %>%
  mutate(fy2025_imp = case_when(
    year == 2024 & wave == 2 ~ 1,
    year == 2024 & wave == 6 ~ 1,
    year == 2025 & wave == 3 ~ 1,
    year == 2025 & wave == 4 ~ 1,
    year == 2025 & wave == 5 ~ 1,
    TRUE ~ 0 
  ))

sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024 == 1], na.rm = TRUE)
sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2025_imp == 1], na.rm = TRUE)

cod_hadd_trips1 <- cod_hadd_trips1 %>%
  mutate(fy2024_current = case_when(
    year == 2024 & wave == 3 ~ 1,
    year == 2024 & wave == 4 ~ 1,
    year == 2024 & wave == 5 ~ 1,  
    TRUE ~ 0 
  ))

cod_hadd_trips1 <- cod_hadd_trips1 %>%
  mutate(fy2025_current = case_when(
    year == 2025 & wave == 3 ~ 1,
    year == 2025 & wave == 4 ~ 1,
    year == 2025 & wave == 5 ~ 1,  
    TRUE ~ 0 
  ))



##### Top row of table 1
pct_diff <- (((sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2025_imp == 1], na.rm = TRUE)) - 
  (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024 == 1], na.rm = TRUE))) /  
    (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024 == 1], na.rm = TRUE))) * 100
pct_diff <- sprintf("%.1f%%", pct_diff)

pct_diff2 <- (((sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2025_current == 1], na.rm = TRUE)) - 
                (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024_current == 1], na.rm = TRUE))) /  
               (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024_current == 1], na.rm = TRUE))) * 100
pct_diff2 <- sprintf("%.1f%%", pct_diff2)

fy2024 <- (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024 == 1], na.rm = TRUE))
fy2025_imp <- (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2025_imp == 1], na.rm = TRUE))
fy2024_current <- (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024_current == 1], na.rm = TRUE))
fy2025_current <- (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2025_current == 1], na.rm = TRUE))
fy2024 <- formatC(fy2024, format = "f", big.mark = ",", digits = 0)
fy2025_imp <- formatC(fy2025_imp, format = "f", big.mark = ",", digits = 0)
fy2024_current <- formatC(fy2024_current, format = "f", big.mark = ",", digits = 0)
fy2025_current <- formatC(fy2025_current, format = "f", big.mark = ",", digits = 0)

fy_trips <- data.frame(
  fy2024 = c(fy2024),
  fy2025_imp = c(fy2025_imp),
  pct_diff = c(pct_diff),
  fy2024_current = c(fy2024_current),
  fy2025_current = c(fy2025_current),
  pct_diff2 = c(pct_diff2),
  row.names = c("Cod/haddock angler trips")
)

knitr::kable(fy_trips, caption = "Western Gulf of Maine Cod/Haddock Angler Trips")
# try kableExtra package for styling


##### Top row of table 2 (wave 5 estimates)
w5_2024 <- (sum(cod_hadd_trips1$value[cod_hadd_trips1$year == 2024 & cod_hadd_trips1$wave == 5], na.rm = TRUE))
w5_2025 <- (sum(cod_hadd_trips1$value[cod_hadd_trips1$year == 2025 & cod_hadd_trips1$wave == 5], na.rm = TRUE))
w5_2024 <- formatC(w5_2024, format = "f", big.mark = ",", digits = 0)
w5_2025 <- formatC(w5_2025, format = "f", big.mark = ",", digits = 0)

w5_trips <- data.frame(
  w5_2024 = c(w5_2024),
  w5_2025 = c(w5_2025),
  row.names = c("Cod/haddock angler trips")
)

knitr::kable(w5_trips, caption = "Wave 5 Western Gulf of Maine Cod/Haddock Angler Trips")


##### Table with trips by mode
pct_diff_h <- (((sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2025_imp == 1 & cod_hadd_trips1$mode == "headboat"], na.rm = TRUE)) - 
                (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024 == 1 & cod_hadd_trips1$mode == "headboat"], na.rm = TRUE))) /  
               (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024 == 1 & cod_hadd_trips1$mode == "headboat"], na.rm = TRUE))) * 100
pct_diff_h <- sprintf("%.1f%%", pct_diff_h)

pct_diff_c <- (((sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2025_imp == 1 & cod_hadd_trips1$mode == "charter"], na.rm = TRUE)) - 
                  (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024 == 1 & cod_hadd_trips1$mode == "charter"], na.rm = TRUE))) /  
                 (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024 == 1 & cod_hadd_trips1$mode == "charter"], na.rm = TRUE))) * 100
pct_diff_c <- sprintf("%.1f%%", pct_diff_c)

pct_diff_p <- (((sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2025_imp == 1 & cod_hadd_trips1$mode == "private"], na.rm = TRUE)) - 
                  (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024 == 1 & cod_hadd_trips1$mode == "private"], na.rm = TRUE))) /  
                 (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024 == 1 & cod_hadd_trips1$mode == "private"], na.rm = TRUE))) * 100
pct_diff_p <- sprintf("%.1f%%", pct_diff_p)

pct_diff_s <- (((sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2025_imp == 1 & cod_hadd_trips1$mode == "shore"], na.rm = TRUE)) - 
                  (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024 == 1 & cod_hadd_trips1$mode == "shore"], na.rm = TRUE))) /  
                 (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024 == 1 & cod_hadd_trips1$mode == "shore"], na.rm = TRUE))) * 100
pct_diff_s <- sprintf("%.1f%%", pct_diff_s)

fy2024_h <- (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024 == 1 & cod_hadd_trips1$mode == "headboat"], na.rm = TRUE))
fy2024_c <- (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024 == 1 & cod_hadd_trips1$mode == "charter"], na.rm = TRUE))
fy2024_p <- (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024 == 1 & cod_hadd_trips1$mode == "private"], na.rm = TRUE))
fy2024_s <- (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2024 == 1 & cod_hadd_trips1$mode == "shore"], na.rm = TRUE))
fy2025_h <- (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2025_imp == 1 & cod_hadd_trips1$mode == "headboat"], na.rm = TRUE))
fy2025_c <- (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2025_imp == 1 & cod_hadd_trips1$mode == "charter"], na.rm = TRUE))
fy2025_p <- (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2025_imp == 1 & cod_hadd_trips1$mode == "private"], na.rm = TRUE))
fy2025_s <- (sum(cod_hadd_trips1$value[cod_hadd_trips1$fy2025_imp == 1 & cod_hadd_trips1$mode == "shore"], na.rm = TRUE))

fy2024_h <- formatC(fy2024_h, format = "f", big.mark = ",", digits = 0)
fy2024_c <- formatC(fy2024_c, format = "f", big.mark = ",", digits = 0)
fy2024_p <- formatC(fy2024_p, format = "f", big.mark = ",", digits = 0)
fy2024_s <- formatC(fy2024_s, format = "f", big.mark = ",", digits = 0)
fy2025_h <- formatC(fy2025_h, format = "f", big.mark = ",", digits = 0)
fy2025_c <- formatC(fy2025_c, format = "f", big.mark = ",", digits = 0)
fy2025_p <- formatC(fy2025_p, format = "f", big.mark = ",", digits = 0)
fy2025_s <- formatC(fy2025_s, format = "f", big.mark = ",", digits = 0)

fy_trips_mode <- data.frame(
  fy2024 = c(fy2024_h, fy2024_c, fy2024_p, fy2024_s, fy2024),
  fy2025 = c(fy2025_h, fy2025_c, fy2025_p, fy2025_s, fy2025_imp),
  pct_diff = c(pct_diff_h, pct_diff_c, pct_diff_p, pct_diff_s, pct_diff),
  row.names = c("Head", "Charter", "Private", "Shore", "Total")
)

knitr::kable(fy_trips_mode, caption = "Western Gulf of Maine Cod/Haddock Angler Trips by Mode")




# Make that row of the table with what you have for trips. And table 2 with trips by mode 
# get directed trips by mode (and total for all modes) for FY2024 and FY2025, generate pct_diff_fy, pct_diff_current

#### NOW A CLEAN SCRIPT,  then catch and catch per trip, then append and make the table we want and some plots
## send kim a directed trips script to look at then you can add in catch stuff



dtrips_annual <- data.frame(
  area = rep("wgom", times=5),
  disposition = rep("cod/haddock trips", times=5),
  mode = c("charter", "headboat", "private", "shore", "total")
)
print(dtrips_annual)

cod_hadd_all_w %>%
  filter(fy2024 == 1) %>% 
  group_by(mode) %>%
  summarise(dtrip = sum(dtrip, na.rm = TRUE))

df_final <- df1 %>%
  left_join(df2, by = "product_id") %>%        # Join based on a shared key
  mutate(total_cost = price * tax_rate) 

dtrips_annual <- dtrips_annual %>%
  left_join(cod_hadd_all_w, by = "mode") %>%
  filter(fy2024 == 1) %>% 
  group_by(mode) %>%
  mutate(dtrip = sum(dtrip, na.rm = TRUE))

rm(dtrips_annual)




