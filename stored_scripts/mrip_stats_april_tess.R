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



## commit and push to github and then ask what is happening and how to get catch and not just trips
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
# The way I defined typ, it's trips where fish were either stated as targeted by
# the angler OR were landed
mrip_effort(dom = c('YEAR', 'WAVE', 'ST'),
            microdata = mrip_stats_041026,
            dir_trip = list(comname = 'HADDOCK',
                            typ = c('PRIM1', 'PRIM2', 'A', 'B1')))|>
  dplyr::filter(ST %in% c("25", "23", "33") & YEAR %in% c("2024", "2025"))


mrip_effort(dom = c('YEAR', 'WAVE', 'ST'),
            microdata = mrip_stats_041026,
            dir_trip = list(comname = 'ATLANTIC COD',
                            typ = c('PRIM1', 'PRIM2', 'A', 'B1')))|>
  dplyr::filter(ST %in% c("25", "23", "33") & YEAR %in% c("2024", "2025"))








