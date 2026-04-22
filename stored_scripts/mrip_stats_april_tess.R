######  MRIP STATISTICS April 2026  ######


library(dplyr)
library(readr)
library("mriptacklebox")
library(tidyverse)
library(survey)


#rm(mrip_stats_041026)
#mrip_stats_041026 <- readRDS("data/main/mrip_statistics_2026-04-10.Rds") 
#class(mrip_stats_041026)

# tidyverse 
mrip_stats_041026 <- read_rds("data/main/mrip_statistics_2026-04-10.Rds")

#library(purrr)
#trip <- map_dfr(mrip_stats_041026[[1]], as.data.frame)


#load the elements in the list into dataframes
trip <- as_tibble(mrip_stats_041026$trip)
catch <- as_tibble(mrip_stats_041026$catch)
size <- as_tibble(mrip_stats_041026$size)
size_b2 <- as_tibble(mrip_stats_041026$size_b2)

# make column names and everything lowercase
names(trip) <- tolower(names(trip))
trip[] <- lapply(trip, function(x) if(is.character(x)) tolower(x) else x)


### Read in site list from Lou that has stock and stat areas that he merges in on intsite
##combinations of intsite, stock area, and stat area are not unique btw 
cod_site_list <- read.csv("data/raw/MRIP_COD_ALL_SITE_LIST.csv")
names(cod_site_list) <- tolower(names(cod_site_list))
n_distinct(cod_site_list$intsite)
n_distinct(cod_site_list$nmfs_stock_area)

cod_site_list %>% 
  count(intsite) %>% 
  filter(n > 1)

n_distinct(cod_site_list$intsite, cod_site_list$nmfs_stock_area)
n_distinct(cod_site_list$intsite, cod_site_list$nmfs_stock_area, cod_site_list$nmfs_stat_area)

#this is what lou did except he didnt keep NH so this wont match:
cod_site_list <- cod_site_list %>% filter(state %in% c("MA", "ME", "NH"))
cod_site_list[order(cod_site_list$intsite, cod_site_list$nmfs_stock_area), ]
cod_site_list <- subset(cod_site_list, select = c(nmfs_stock_area, intsite, nmfs_stat_area, state))
cod_site_list <- cod_site_list %>% distinct(nmfs_stock_area, intsite, nmfs_stat_area, state, .keep_all = TRUE)

## think these are WGOM? 513 514 515 521 526 NH
cod_site_list %>% count(nmfs_stat_area)
cod_site_list %>% count(state)

cod_site_list <- cod_site_list %>%
  mutate(wgom = case_when(
    state == "NH" ~ 1,
    nmfs_stat_area == 513 | nmfs_stat_area == 514  ~ 1,
    nmfs_stat_area == 515 | nmfs_stat_area == 521  ~ 1,
    nmfs_stat_area == 526  ~ 1,
    TRUE ~ 0 # Catch-all for all other cases
  ))




## Sam's example for using mrip effort function:
mrip_effort(dom = c('YEAR', 'ST'),
            microdata = mrip_stats_041026) |>
  dplyr::filter(ST == 25)


mrip_effort(dom = c('YEAR', 'WAVE'),
            microdata = mrip_stats_041026,
            dir_trip = list(comname = 'HADDOCK',
                            typ = c('PRIM1', 'PRIM2', 'A', 'B1')))|>
  dplyr::filter(ST == 25|ST == 23 & YEAR == 2024|YEAR == 2025)


# 25 is MA, 23 is ME, 33 is NH
# The way I defined typ, it's trips where cod/haddock were either stated as the 
#  primary target by the angler OR were landed-A, unobserved-B1, or discarded-B2
mrip_effort(dom = c('YEAR', 'WAVE', 'ST'),
            microdata = mrip_stats_041026,
            dir_trip = list(comname = 'HADDOCK',
                            typ = c('PRIM1', 'A', 'B1', 'B2')))|>
  dplyr::filter(ST %in% c("25", "23", "33") & YEAR %in% c("2024", "2025"))





####COD CATCH MOVE THIS####
cod_catch_df <- mrip_catch(comname = 'ATLANTIC COD', 
                            dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX'), 
                            microdata = mrip_stats_041026, estimate_var = FALSE)

##spits out a list and we want the estimates
cod_catch_df <- cod_catch_df$estimates  |>
  dplyr::filter(ST %in% c("25", "23", "33") & YEAR %in% c("2024", "2025"))

names(cod_catch_df) <- tolower(names(cod_catch_df))

cod_catch_df <- cod_catch_df %>%
  mutate(mode = case_when(
    mode_fx == 3|mode_fx==2|mode_fx==1 ~ "shore",
    mode_fx == 5 ~ "charter",
    mode_fx == 7 ~ "private",
    mode_fx == 4 ~ "headboat"
  ))

cod_catch_df <- cod_catch_df %>%
  mutate(state = case_when(
    st == "25" ~ "MA",
    st == "33" ~ "NH",
    st == "23" ~ "ME"
  ))



####COD EFFORT####
## common IDs species in catch data and prim1/prim2 IDs target species in trip data
## need the stock area intsite and mode (use MODE_FX)
cod_df <- mrip_effort(dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'INTSITE'),
            microdata = mrip_stats_041026,
            dir_trip = list(comname = 'ATLANTIC COD',
                            typ = c('PRIM1', 'A', 'B1', 'B2')))|>
  dplyr::filter(ST %in% c("25", "23", "33") & YEAR %in% c("2024", "2025"))

## pulling  trips where all three are cod or at least one is cod? 
# try including B2, try pulling for the various prim1 and A and the B's separately
# try merging in stock area on intsite using the csv from lou but they may not be 1:1

names(cod_df) <- tolower(names(cod_df))

cod_df <- cod_df %>%
  mutate(mode = case_when(
    mode_fx == 3|mode_fx==2|mode_fx==1 ~ "shore",
    mode_fx == 5 ~ "charter",
    mode_fx == 7 ~ "private",
    mode_fx == 4 ~ "headboat"
  ))

cod_df <- subset(cod_df, select = -c(hrsf))

cod_df <- cod_df %>%
  mutate(state = case_when(
    st == "25" ~ "MA",
    st == "33" ~ "NH",
    st == "23" ~ "ME"
  ))

cod_df %>% count(wave, year)


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



####HADDOCK####
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
merge_cod_hadd_ym

##Lou cod/haddock WGOM trips 2024 private= 197908, 2025 private=179869
## Now need to stop double counting the cod AND haddock trips


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



