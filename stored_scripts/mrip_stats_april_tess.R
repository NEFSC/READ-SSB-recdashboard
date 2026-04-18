######  MRIP STATISTICS April 2026  ######


library(dplyr)
library(readr)
library("mriptacklebox")



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



## Sam's example for using mrip effort function:
mrip_effort(dom = c('YEAR', 'ST'),
            microdata = mrip_stats_041026) |>
  dplyr::filter(ST == 25)

mrip_effort(dom = c('YEAR'),
           microdata = mrip_stats_041026,
           dir_trip = list(comname = 'SCUP',
                           typ = c('PRIM1', 'PRIM2', 'B1')))

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


mrip_effort(dom = c('YEAR', 'WAVE', 'ST'),
            microdata = mrip_stats_041026,
            dir_trip = list(comname = 'ATLANTIC COD',
                            typ = c('PRIM1', 'A', 'B1', 'B2')))|>
  dplyr::filter(ST %in% c("25", "23", "33") & YEAR %in% c("2024", "2025"))

## you need the stock area intsite and mode (use MODE_FX)
cod_df <- mrip_effort(dom = c('YEAR', 'WAVE', 'ST', 'MODE_FX', 'INTSITE'),
            microdata = mrip_stats_041026,
            dir_trip = list(comname = 'ATLANTIC COD',
                            typ = c('PRIM1', 'A', 'B1')))|>
  dplyr::filter(ST %in% c("25", "23", "33") & YEAR %in% c("2024", "2025"))


##look at cod_df  and see how far off we are 
# generate the mode variable, drop hours fished, try with and without NH, 
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


##combinations of intsite, stock area, and stat area aren't unique. 
cod_site_list <- read.csv("data/raw/MRIP_COD_ALL_SITE_LIST.csv")
names(cod_site_list) <- tolower(names(cod_site_list))
n_distinct(cod_site_list$intsite)
n_distinct(cod_site_list$nmfs_stock_area)

cod_site_list %>% 
  count(intsite) %>% 
  filter(n > 1)

n_distinct(cod_site_list$intsite, cod_site_list$nmfs_stock_area)
n_distinct(cod_site_list$intsite, cod_site_list$nmfs_stock_area, cod_site_list$nmfs_stat_area)


#this is what lou did except he didnt keep NH:
cod_site_list <- cod_site_list %>% filter(state %in% c("MA", "ME", "NH"))
cod_site_list[order(cod_site_list$intsite, cod_site_list$nmfs_stock_area), ]
cod_site_list <- subset(cod_site_list, select = c(nmfs_stock_area, intsite, nmfs_stat_area, state))
cod_site_list <- cod_site_list %>% distinct(nmfs_stock_area, intsite, nmfs_stat_area, state, .keep_all = TRUE)


merge_cod <- left_join(cod_df, cod_site_list, by = c("state", "intsite"))
  
cod_collapse <- merge_cod %>%
  group_by(mode, year, wave) %>%
  summarise(dtrip = sum(n_trip, na.rm = TRUE))

cod_collapse

## something to ask Sam Truesdall: Lou is using 'leader' to help ID trips but the
# microdata function doesn't seem to pull leader. leader=group catch leader

# need to deal with the WGOM mismatch issue with stock area

#need to also get trips where prim1 and prim2 are cod/haddock haddock/cod?/ caught both
# in lou code this is WGOM: 513 514 515 521 526 NH
# and EGOM is 511 and 512

## do we just want WGOM trips?? Lou's output pulled EGOM and SNE trips too



