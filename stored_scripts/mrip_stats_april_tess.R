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




## something to ask Sam Truesdall: Lou is using 'leader' to help ID trips but the
# microdata function doesn't seem to pull leader. leader=group catch leader

# need to deal with the WGOM mismatch issue with stock area

#need to also get trips where prim1 and prim2 are cod/haddock haddock/cod?/ caught both
# in lou code this is WGOM: 513 514 515 521 526 NH
# and EGOM is 511 and 512

## do we just want WGOM trips?? Lou's output pulled EGOM and SNE trips too



