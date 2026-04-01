#script1

#push button not available after I made two commits, 
# 'git push' command in the terminal didn't work
#was able to push from github desktop. 

library(dplyr)
#library(tidyverse)



## Catch Per Trip
mean_catch_per_trip <- read.csv("cod_haddock/mean_catch_per_trip.csv")
#directed_trips <- read.csv("cod_haddock/directed_trips.csv")
#baseline_catch_at_length <- read.csv("cod_haddock/baseline_catch_at_length.csv")
#projected_catch_at_length <- read.csv("cod_haddock/projected_catch_at_length.csv")


# want data in format with the following columns: 
# species	mode data_version	year wave	metric value units

# catch per trip data is at the month-mode level for April-Nov, 201 draws
# looks like only the for hire is open in April, only private open in Nov (14 obs per draw)
#for groundfish, catch per trip isn't broken out by state?

# plan to start with catch per trip, make two data frames, one haddock and one cod, bind on additional columns, then row bind

# first add static columns like year
# assuming it's all 2025?
mean_catch_per_trip$year <- "2025"
mean_catch_per_trip$data_version <- "2025V1"
mean_catch_per_trip$metric <- "catch-per-trip"
mean_catch_per_trip$units <- "number of fish"

#create wave from month
mean_catch_per_trip <- mean_catch_per_trip %>%
  mutate(wave = case_when(
    month <= 2 ~ "1",
    month == 3|month==4 ~ "2",
    month == 5|month==6 ~ "3",
    month == 7|month==8 ~ "4",
    month == 9|month==10 ~ "5",
    month >= 11 ~ "6"
  ))

#make cod data frame
#leaving in some extra columns like month and draw for now, can delete later
cod_mean_catch_per_trip <- mean_catch_per_trip[, c("mode", "data_version", "year", "month", "wave", "metric", "cod_catch", "units", "draw")]
cod_mean_catch_per_trip$species <- "Cod"
#rename cod catch column as value
cod_mean_catch_per_trip <- cod_mean_catch_per_trip %>% rename(value = cod_catch)

#make haddock data frame
hadd_mean_catch_per_trip <- mean_catch_per_trip[, c("mode", "data_version", "year", "month", "wave", "metric", "hadd_catch", "units", "draw")]
hadd_mean_catch_per_trip$species <- "Haddock"
#rename cod catch column as value
hadd_mean_catch_per_trip <- hadd_mean_catch_per_trip %>% rename(value = hadd_catch)


#reorder the columns, drop unnecessary columns, bind cod and haddock rows

# # base R way
# cod_mean_catch_per_trip[, c("species", "mode", "data_version", "year", "wave", "metric", "value", "units")]
# hadd_mean_catch_per_trip[, c("species", "mode", "data_version", "year", "wave", "metric", "value", "units")]
# #data_cpt <- rbind(cod_mean_catch_per_trip, hadd_mean_catch_per_trip)

#dplyr
data_cpt <- bind_rows(
  cod_mean_catch_per_trip %>% select(species, mode, data_version, year, wave, metric, value, units),
  hadd_mean_catch_per_trip %>% select(species, mode, data_version, year, wave, metric, value, units)
)

# Should the data be collapsing at any level? Assume we want to show the distribution across draws?
## Should I be averaging the catch per trip within a mode-wave-draw? 
# that would tend to overweight the average catch from a month with less trips..
# the way I have it, each mode-wave-draw has two observations for the two months



##Directed Trips

#this has month, mode, kind of day
# do we want to add things up so total number of trips by mode-wave ?

# For trips, probably
directed_trips <- read.csv("cod_haddock/directed_trips.csv")

summary(directed_trips)
table(directed_trips$kod)
##there are some 2024 trips
table(directed_trips$year)



summary(mean_catch_per_trip)





##Baseline Catch at Length 
baseline_catch_at_length <- read.csv("cod_haddock/baseline_catch_at_length.csv")
# draw, season, length (inches or cm?), fitted probability, observed probability
#only a summer and winter season
#is summer waves 3-5 and winter waves 6-2?
table(baseline_catch_at_length$season)




