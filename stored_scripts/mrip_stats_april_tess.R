######  MRIP STATISTICS April 2026  ######


library(dplyr)
library(readr)



#rm(mrip_stats_041026)
#mrip_stats_041026 <- readRDS("data/main/mrip_statistics_2026-04-10.Rds")

class(mrip_stats_041026)

# tidyverse 
mrip_stats_041026 <- read_rds("data/main/mrip_statistics_2026-04-10.Rds")

#library(purrr)
#trip <- map_dfr(mrip_stats_041026[[1]], as.data.frame)


#load the elements in the list into dataframes
trip <- as_tibble(mrip_stats_041026$trip)
catch <- as_tibble(mrip_stats_041026$catch)
size <- as_tibble(mrip_stats_041026$size)
size_b2 <- as_tibble(mrip_stats_041026$size_b2)

