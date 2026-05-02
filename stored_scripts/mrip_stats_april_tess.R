######  MRIP STATISTICS April 2026  ######


# if Rstudio is making you enter a username and PAT and then not letting you push, 
# enter this in the terminal and then enter user and PAT: 
# git config --global credential.helper store

library(dplyr)
library(readr)
library("mriptacklebox")
library(tidyverse)
library(survey)



#RUN pull_mrip.R
# or read in from data/main but w/correct date in the filename
mrip_statistics <- read_rds("data/raw/mrip_statistics_2026-04-29.Rds")


#load elements in the list into dataframes
trip<-mrip_statistics$trip
catch<-mrip_statistics$catch
size<-mrip_statistics$size
size_b2<-mrip_statistics$size_b2

#may just need to clean the above trip and catch files directly rather than use 
# Sam's effort and catch functions but will try his functions first

# make column names and text lowercase
names(trip) <- tolower(names(trip))
trip[] <- lapply(trip, function(x) if(is.character(x)) tolower(x) else x)
names(catch) <- tolower(names(catch))
catch[] <- lapply(catch, function(x) if(is.character(x)) tolower(x) else x)




####### COD EFFORT #######
# which(colnames(trip) == "leader") ## leader IS in there it's just hidden

# typ is pulling trips where cod were stated as primary OR
# were landed-A, unobserved-B1, or discarded-B2
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

# 261611 trips before we clean it except w new mrip pull it's 267506
#sometimes mrip data has slight updates w/o an announcement 
sum(cod_effort$n_trip, na.rm = TRUE)

##the rows are unique trips. they're multiplied by the weight wp_int to estimate trips
## rows are unique on id_code
n_distinct(cod_effort$id_code)
# summary by mode. there's a private trip record with 15k trips
tapply(cod_effort$n_trip, cod_effort$mode_fx, summary)
## why are there rows where there are 0 trips? 
nrow(cod_effort[cod_effort$n_trip == 0, ])






####### COD CATCH #######
cod_catch <- mrip_catch(comname = 'ATLANTIC COD', 
                        dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'STRAT_ID', 
                                'PSU_ID', 'ID_CODE', 'WP_INT'), 
                        microdata = mrip_statistics, estimate_var = FALSE)

## the above gives a list, we want the estimates
cod_catch <- cod_catch$estimates  |>
  dplyr::filter(ST %in% c("25", "23", "33") & YEAR %in% c("2024", "2025"))

names(cod_catch) <- tolower(names(cod_catch))
cod_catch[] <- lapply(cod_catch, function(x) if(is.character(x)) tolower(x) else x)
cod_catch <- subset(cod_catch, select = -c(se, cv))


####### MERGE COD TRIPS AND CATCH #######
cod_effort$source <- "effort"
cod_catch$source <- "catch"
cod_effort_catch <- left_join(cod_effort, cod_catch, 
                              by = c("common", "year", "wave", "mode_fx", "st", 
                                     "strat_id", "psu_id", "id_code"))
# lou merged on year, strat_id, psu_id, id_code 

# 159 trips without catch. lou kept them. assign their catch values as 0?
cod_effort_catch %>% count(source.x, source.y)

cod_effort_catch$date <- substr(cod_effort_catch$id_code, 6, 13)
cod_effort_catch$month <- substr(cod_effort_catch$date, 5, 6)
cod_effort_catch$day <- substr(cod_effort_catch$date, 7, 8)
##none of these are in the data (if they do show up in other cases we would drop)
sum(cod_effort_catch$day == "9x")
sum(cod_effort_catch$day == "xx")



####### HADDOCK EFFORT #######
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

## 318244 trips before we clean 
sum(hadd_effort$n_trip, na.rm = TRUE)

####### HADDOCK CATCH #######
hadd_catch <- mrip_catch(comname = 'HADDOCK', 
                        dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'STRAT_ID', 
                                'PSU_ID', 'ID_CODE', 'WP_INT'), 
                        microdata = mrip_statistics, estimate_var = FALSE)

## the above gives a list, we want the estimates
hadd_catch <- hadd_catch$estimates  |>
  dplyr::filter(ST %in% c("25", "23", "33") & YEAR %in% c("2024", "2025"))

names(hadd_catch) <- tolower(names(hadd_catch))
hadd_catch[] <- lapply(hadd_catch, function(x) if(is.character(x)) tolower(x) else x)
hadd_catch <- subset(hadd_catch, select = -c(se, cv))

####### MERGE HADDOCK TRIPS AND CATCH #######
hadd_effort$source <- "effort"
hadd_catch$source <- "catch"
hadd_effort_catch <- left_join(hadd_effort, hadd_catch, 
                              by = c("common", "year", "wave", "mode_fx", "st", 
                                     "strat_id", "psu_id", "id_code"))

##there are 367 trips without catch
#keep them but assign their catch values as 0? lou kept them, assigned missing claim=0
hadd_effort_catch %>% count(source.x, source.y)

hadd_effort_catch$date <- substr(hadd_effort_catch$id_code, 6, 13)
hadd_effort_catch$month <- substr(hadd_effort_catch$date, 5, 6)
hadd_effort_catch$day <- substr(hadd_effort_catch$date, 7, 8)
##none of these are in the data (if they do show up in other cases we would drop)
sum(hadd_effort_catch$day == "9x")
sum(hadd_effort_catch$day == "xx")


###### APPEND COD AND HADDOCK ######
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



####### Read in COD SITE LIST (stock and stat areas) #######
##combinations of intsite, stock area, and stat area are not unique.. 
#lou took the 1st unique obs in the group (should it be the most common stat area for each intsite?)
cod_site_list <- read.csv("data/raw/MRIP_COD_ALL_SITE_LIST.csv")
names(cod_site_list) <- tolower(names(cod_site_list))
n_distinct(cod_site_list$intsite)
n_distinct(cod_site_list$nmfs_stock_area)
n_distinct(cod_site_list$intsite, cod_site_list$nmfs_stat_area, cod_site_list$nmfs_stock_area)

# lou did this and after merging he made nmfs_stat_area="NH" if state=="NH" 
cod_site_list <- cod_site_list %>% filter(state %in% c("MA", "ME"))
cod_site_list <- subset(cod_site_list, select = c(state, intsite, nmfs_stock_area, nmfs_stat_area))
cod_site_list <- cod_site_list[order(cod_site_list$intsite, cod_site_list$nmfs_stock_area), ]
cod_site_list <- cod_site_list %>% distinct(nmfs_stock_area, intsite, nmfs_stat_area, state, .keep_all = TRUE)

cod_site_list %>% count(nmfs_stat_area)

##  these are WGOM according to stata code: 513 514 515 521 526 NH
cod_site_list <- cod_site_list %>%
  mutate(wgom = case_when(
    nmfs_stat_area == 513 | nmfs_stat_area == 514  ~ 1,
    nmfs_stat_area == 515 | nmfs_stat_area == 521  ~ 1,
    nmfs_stat_area == 526  ~ 1,
    TRUE ~ 0 # Catch-all for all other cases
  ))

cod_site_list %>% count(wgom)


## MERGE cod sites into trips on intsite
cod_hadd_all <- left_join(cod_hadd_all, cod_site_list, by = c("state", "intsite"))

# label NH trips as part of WGOM and fill in their stat area
cod_hadd_all <- cod_hadd_all %>%
  mutate(wgom = if_else(state == "NH", 1, wgom))

# why does NH have way more observations than MA and ME...
cod_hadd_all %>% count(state)

cod_hadd_all$nmfs_stat_area <- as.character(cod_hadd_all$nmfs_stat_area)
cod_hadd_all <- cod_hadd_all %>%
  mutate(nmfs_stat_area = if_else(state == "NH", "NH", nmfs_stat_area))

## keep if WGOM. dropped less than 300 rows
cod_hadd_all <- cod_hadd_all %>% 
  filter(wgom == 1)

cod_hadd_all <- cod_hadd_all %>%
  mutate(fy2024 = case_when(
    year == 2024 & wave >= 3 ~ 1,
    year == 2025 & wave == 2 ~ 1,
    TRUE ~ 0 
  ))

cod_hadd_all <- cod_hadd_all %>%
  mutate(fy2025_imp = case_when(
    year == 2024 & wave == 2 ~ 1,
    year == 2024 & wave == 6 ~ 1,
    year == 2025 & wave == 3 ~ 1,
    year == 2025 & wave == 4 ~ 1,
    year == 2025 & wave == 5 ~ 1,
    TRUE ~ 0 
  ))

#lou kept the trips with no catch data, assigned claim=0 when it was missing. 
# make those rows have value=0 and variable =claim (not creating rows for other catch vars)
cod_hadd_all$value[is.na(cod_hadd_all$value)] <- 0
cod_hadd_all$variable[is.na(cod_hadd_all$variable)] <- "claim"

nrow(cod_hadd_all[cod_hadd_all$variable == "claim", ])
nrow(cod_hadd_all[cod_hadd_all$variable == "claim" & cod_hadd_all$value == 0, ])

#  2301/19297
n_distinct(cod_hadd_all$leader)
# 2653
n_distinct(cod_hadd_all$id_code)



####### DEAL WITH GROUP CATCH before you can get trips #######
# lou generates dom_id=1 if common / prim1_common is atlanticcod or haddock, dom_id=2 otherwise
# gen domain_claim=claim

#need to wide out the catch variables. 
cod_hadd_all_w <- cod_hadd_all %>% spread(key = variable, value = value)

#remove spaces in 'atlantic cod'
cod_hadd_all_w$common <- gsub(" ", "", cod_hadd_all_w$common)

#dom_id is 1 if cod or haddock and 2 if else
cod_hadd_all_w$dom_id <- ifelse(cod_hadd_all_w$common == "atlanticcod"|cod_hadd_all_w$common == "haddock", 1, 2)

#domain_claim is claim if common is cod or haddock
cod_hadd_all_w <- cod_hadd_all_w %>%
  mutate(domain_claim = ifelse(common %in% c("atlanticcod", "haddock"), claim, 0))

sum(cod_hadd_all_w$domain_claim == 0, na.rm = TRUE)


# gc_flag assigns each leader the lowest dom_id in the group, flagging the whole 
#group if at least one record is cod or haddock
#bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]

#this finds the max domain claim in the group
#bysort strat_id psu_id leader (domain_claim): gen claim_flag=domain_claim[_N]

# this recodes a trip as dom_id=1 if it was previously 2 if claim_flag>0 and gc_flag is 1
#replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")
cod_hadd_all_w <- cod_hadd_all_w %>%
  group_by(strat_id, psu_id, leader) %>%
  mutate(
    # Lowest dom_id (if "1" exists in the group, gc_flag becomes "1")
    gc_flag = min(dom_id, na.rm = TRUE),
    # Highest domain_claim in the group
    claim_flag = max(domain_claim, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Re-classify the trip into the domain of interest
  mutate(dom_id = ifelse(
    dom_id == "2" & !is.na(claim_flag) & claim_flag > 0 & gc_flag == "1",
    "1",
    dom_id
  ))

sum(cod_hadd_all_w$claim_flag == 0, na.rm = TRUE)
sum(cod_hadd_all_w$dom_id == 2, na.rm = TRUE)
table(cod_hadd_all_w$gc_flag)




##OLD NOTE
## 13k cod trips in new england seems low
#wp_int is not in the trip data frame but you can pull it with mrip_effort
# probably comes from merging it in from catch
nrow(trip[trip$prim1_common == "atlantic cod" & trip$year == 2024 & 
            trip$st == 25|trip$st == 23|trip$st == 33, ])












####COD CATCH OLD####
# this is just pooling stuff by wave, not sure if you will match lou but try
# can't limit this to WGOM using cod sites without insite and intsite is only in trip data
# you will need to get trip level catch and individual trips so you can merge 
# them and deal w group catch
cod_catch <- mrip_catch(comname = 'ATLANTIC COD', 
                            dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX'), 
                            microdata = mrip_stats_041026, estimate_var = FALSE)

##spits out a list and we want the estimates
cod_catch <- cod_catch$estimates  |>
  dplyr::filter(ST %in% c("25", "23", "33") & YEAR %in% c("2024", "2025"))

names(cod_catch) <- tolower(names(cod_catch))

cod_catch <- cod_catch %>%
  mutate(mode = case_when(
    mode_fx == 3|mode_fx==2|mode_fx==1 ~ "shore",
    mode_fx == 5 ~ "charter",
    mode_fx == 7 ~ "private",
    mode_fx == 4 ~ "headboat"
  ))

cod_catch <- cod_catch %>%
  mutate(state = case_when(
    st == "25" ~ "MA",
    st == "33" ~ "NH",
    st == "23" ~ "ME"
  ))

n_distinct(cod_catch$year, cod_catch$strat_id, cod_catch$psu_id, cod_catch$id_code)



####COD EFFORT OLD####
## common IDs species in catch data and prim1/prim2 IDs target species in trip data
## need the stock area intsite and mode (use MODE_FX)
cod_df <- mrip_effort(dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'INTSITE'),
            microdata = mrip_stats_041026,
            dir_trip = list(comname = 'ATLANTIC COD',
                            typ = c('PRIM1', 'A', 'B1', 'B2')))|>
  dplyr::filter(ST %in% c("25", "23", "33") & YEAR %in% c("2024", "2025"))

# try pulling for the various prim1 and A and the B's separately

names(cod_df) <- tolower(names(cod_df))

cod_df <- cod_df %>%
  mutate(mode = case_when(
    mode_fx == 3|mode_fx==2|mode_fx==1 ~ "shore",
    mode_fx == 5 ~ "charter",
    mode_fx == 7 ~ "private",
    mode_fx == 4 ~ "headboat"
  ))

cod_df <- cod_df %>%
  mutate(state = case_when(
    st == "25" ~ "MA",
    st == "33" ~ "NH",
    st == "23" ~ "ME"
  ))

cod_df %>% count(wave, year)

cod_df <- subset(cod_df, select = -c(hrsf))

merge_cod <- left_join(cod_df, cod_site_list, by = c("state", "intsite"))

##keep wgom
merge_cod <- merge_cod %>% 
  filter(wgom == 1)
  
cod_collapse <- merge_cod %>%
  group_by(mode, year, wave) %>%
  summarise(dtrip = sum(n_trip, na.rm = TRUE))

cod_collapse

cod_collapse <- cod_collapse %>%
  mutate(fy2024 = case_when(
    year == 2024 & wave >= 3 ~ 1,
    year == 2025 & wave == 2 ~ 1,
    TRUE ~ 0 
  ))

cod_collapse <- cod_collapse %>%
  mutate(fy2025_imp = case_when(
    year == 2024 & wave == 2 ~ 1,
    year == 2024 & wave == 6 ~ 1,
    year == 2025 & wave == 3 ~ 1,
    year == 2025 & wave == 4 ~ 1,
    year == 2025 & wave == 5 ~ 1,
    TRUE ~ 0 
  ))

##kicks out  2025 wave 5
cod_collapse %>% count(fy2024, fy2025_imp)
#cod_collapse1 <- subset(cod_collapse, !(fy2024 == 0 & fy2025_imp == 0))


#FY2024
cod_collapse2 <- cod_collapse %>%
  group_by(mode, fy2024) %>%
  summarise(dtrip_ym = sum(dtrip, na.rm = TRUE))
cod_collapse2 <- subset(cod_collapse2, fy2024 == 1)

cod_collapse2 <- cod_collapse2 %>%
  mutate(year = case_when(
    fy2024 == 1 ~ "fy2024"
  ))
cod_collapse2 <- subset(cod_collapse2, select = -c(fy2024))
cod_collapse2

##fy2025 impute
cod_collapse3 <- cod_collapse %>%
  group_by(mode, fy2025_imp) %>%
  summarise(dtrip_ym = sum(dtrip, na.rm = TRUE))
cod_collapse3 <- subset(cod_collapse3, fy2025_imp == 1)

cod_collapse3 <- cod_collapse3 %>%
  mutate(year = case_when(
    fy2025_imp == 1 ~ "fy2025_imp"
  ))
cod_collapse3 <- subset(cod_collapse3, select = -c(fy2025_imp))
cod_collapse3

#append
cod_ym <- rbind(cod_collapse2, cod_collapse3)
cod_ym <- rename(cod_ym, dtrip_cod_ym = dtrip_ym)
cod_ym

##these numbers higher than lou's bc double counting bc of cod AND hadd trips
## must deal with group catch
## but also something is going on with your fishing year variable bc look at shore trips
## theyre the same in 2024 and 2025impute but lou doesnt have that so you did somethjing
## YOU DID SOMETHING WRONG. your 2025impute trips are LOWER than lou's now



####HADDOCK effort OLD####
hadd_df <- mrip_effort(dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'INTSITE'),
                      microdata = mrip_stats_041026,
                      dir_trip = list(comname = 'HADDOCK',
                                      typ = c('PRIM1', 'A', 'B1', 'B2')))|>
  dplyr::filter(ST %in% c("25", "23", "33") & YEAR %in% c("2024", "2025"))

names(hadd_df) <- tolower(names(hadd_df))

hadd_df <- hadd_df %>%
  mutate(mode = case_when(
    mode_fx == 3|mode_fx==2|mode_fx==1 ~ "shore",
    mode_fx == 5 ~ "charter",
    mode_fx == 7 ~ "private",
    mode_fx == 4 ~ "headboat"
  ))

hadd_df <- subset(hadd_df, select = -c(hrsf))

hadd_df <- hadd_df %>%
  mutate(state = case_when(
    st == "25" ~ "MA",
    st == "33" ~ "NH",
    st == "23" ~ "ME"
  ))

hadd_df %>% count(wave, year)


merge_hadd <- left_join(hadd_df, cod_site_list, by = c("state", "intsite"))

merge_hadd %>% count(wgom)


##keep wgom
merge_hadd <- merge_hadd %>% 
  filter(wgom == 1)

hadd_collapse <- merge_hadd %>%
  group_by(mode, year, wave) %>%
  summarise(dtrip = sum(n_trip, na.rm = TRUE))
hadd_collapse


hadd_collapse <- hadd_collapse %>%
  mutate(fy2024 = case_when(
    year == 2024 & wave >= 3 ~ 1,
    year == 2025 & wave == 2 ~ 1,
    TRUE ~ 0 
  ))

hadd_collapse <- hadd_collapse %>%
  mutate(fy2025_imp = case_when(
    year == 2024 & wave == 2 ~ 1,
    year == 2024 & wave == 6 ~ 1,
    year == 2025 & wave == 3 ~ 1,
    year == 2025 & wave == 4 ~ 1,
    year == 2025 & wave == 5 ~ 1,
    TRUE ~ 0 
  ))

##kicks out  2025 wave 5,6
hadd_collapse %>% count(fy2024, fy2025_imp)

#hadd_collapse1 <- subset(hadd_collapse, !(fy2024 == 0 & fy2025_imp == 0))

#FY2024
hadd_collapse2 <- hadd_collapse %>%
  group_by(mode, fy2024) %>%
  summarise(dtrip_ym = sum(dtrip, na.rm = TRUE))
hadd_collapse2 <- subset(hadd_collapse2, fy2024 == 1)

hadd_collapse2 <- hadd_collapse2 %>%
  mutate(year = case_when(
    fy2024 == 1 ~ "fy2024"
  ))
hadd_collapse2 <- subset(hadd_collapse2, select = -c(fy2024))
hadd_collapse2

##fy2025 impute
hadd_collapse3 <- hadd_collapse %>%
  group_by(mode, fy2025_imp) %>%
  summarise(dtrip_ym = sum(dtrip, na.rm = TRUE))
hadd_collapse3 <- subset(hadd_collapse3, fy2025_imp == 1)

hadd_collapse3 <- hadd_collapse3 %>%
  mutate(year = case_when(
    fy2025_imp == 1 ~ "fy2025_imp"
  ))
hadd_collapse3 <- subset(hadd_collapse3, select = -c(fy2025_imp))
hadd_collapse3

#append
hadd_ym <- rbind(hadd_collapse2, hadd_collapse3)
hadd_ym <- rename(hadd_ym, dtrip_hadd_ym = dtrip_ym)
hadd_ym


merge_cod_hadd_ym <- merge(cod_ym, hadd_ym, by = c("mode", "year"), all = TRUE)

merge_cod_hadd_ym$dtrip_cod_ym[is.na(merge_cod_hadd_ym$dtrip_cod_ym)] <- 0
merge_cod_hadd_ym$dtrip_ym <- merge_cod_hadd_ym$dtrip_cod_ym + merge_cod_hadd_ym$dtrip_hadd_ym

##add rows for total trips by year
merge_cod_hadd_ym <- merge_cod_hadd_ym %>%
  bind_rows(
    merge_cod_hadd_ym %>%
      filter(year == "fy2024") %>%
      summarise(
        across(where(is.numeric), sum),
        year = "fy2024", mode = "total"
      )
  )

merge_cod_hadd_ym <- merge_cod_hadd_ym %>%
  bind_rows(
    merge_cod_hadd_ym %>%
      filter(year == "fy2025_imp") %>%
      summarise(
        across(where(is.numeric), sum),
        year = "fy2025_imp", mode = "total"
      )
  )
merge_cod_hadd_ym

##Lou cod/haddock WGOM trips 2024 private= 197908, 2025 private=179869
## Now need to stop double counting the cod AND haddock trips
## deal with group catch and hopefully we match
## want cod trips, hadd trips, cod and hadd trips. this current merge_cod_hadd_ym
# should equal all those summed up (ie, private 2024=258196 and 2025=242693)



######## GROUP CATCH ############
## can deal with this later
## append cod and hadd trips, merge trip with catch data: 
# lou merged on year, strat_id, psu_id, id_code



##My pull exactly matches the MRIP query tool for 2024, private, 2024 cal year, MA
cod_df %>% 
  filter(state == "MA", year == "2024", mode_fx=="7") %>% 
  summarize(total_sum = sum(n_trip, na.rm = TRUE))



## something to ask Sam Truesdall: Lou is using 'leader' to help ID trips but the
# microdata function doesn't seem to pull leader. leader=group catch leader

# need to deal with the WGOM mismatch issue with stock area
## and need to change 2025 to 2025 impute to match lou

#need to also get trips where prim1 and prim2 are cod/haddock haddock/cod?/ caught both
# in lou code this is WGOM: 513 514 515 521 526 NH
# and EGOM is 511 and 512

## do we just want WGOM trips?? Lou's output pulled EGOM and SNE trips too



