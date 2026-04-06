#script1

#push button not available after I made two commits, 
# 'git push' command in the terminal didn't work
#was able to push from github desktop. 

library(dplyr)
#library(tidyverse)
library(tidyr)
library(ggplot2)
library(viridis)

# Load data
mean_catch_per_trip <- read.csv("cod_haddock/mean_catch_per_trip.csv")
directed_trips <- read.csv("cod_haddock/directed_trips.csv")
baseline_catch_at_length <- read.csv("cod_haddock/baseline_catch_at_length.csv")
#projected_catch_at_length <- read.csv("cod_haddock/projected_catch_at_length.csv")


# want data from all the datasets in long format with the following columns: 
# species	mode data_version	year wave	metric value units



### DIRECTED TRIPS

#this has month, mode, kind of day
# we want total trips by mode-wave; combine weekend and weekday trips
# First need to calculate weights for the months within a wave so average 
# catch per trip per wave doesn't overweight catch in months with less trips
# Get average trips by mode-month-kod-year, collapse out kod, 
# make trips by month within a wave wide, add them, generate w1m and w2m as the 
# weights for first month in wave and second month

#directed_trips$ID_mode_month_yr_kod <- paste(mode, month, year, kod, sep = "_")
#directed_trips <- directed_trips %>% unite(directed_trips, mode, month, year, kod, sep = "_")
# Concatenate to make an ID
directed_trips <- directed_trips %>% 
  mutate(ID_mode_month_yr_kod = paste(mode, month, year, kod, sep = "_"))

# 26 groups here
table(directed_trips$ID_mode_month_yr_kod)

summary(directed_trips)
##there are some 2024 trips
table(directed_trips$year)
table(directed_trips$month)

# November trips (wave 6) are from 2024, waves 1-5 are 2025
table(directed_trips$year, directed_trips$month)
# In April, there are only weekday for hire trips and weekend private trips
# In November 2024, there are only weekend private trips
# In Oct, private trips are only on weekends
table(directed_trips$ID_mode_month_yr_kod, directed_trips$month)


#yes, close to the weekend total number of trips in june so this is not daily
# averages across draws are close to what Lou showed
directed_trips %>%
  filter(mode == "pr", month==6, kod== "we") %>%
  summarise(mean_val = mean(dtrip, na.rm = TRUE))

directed_trips %>%
  filter(mode == "pr", month==4, kod== "we") %>%
  summarise(mean_val = mean(dtrip, na.rm = TRUE))

# means by group
directed_trips %>%
  group_by(mode, month) %>%
  summarise(mean_val = mean(dtrip, na.rm = TRUE))

directed_trips %>%
  group_by(mode, kod, month) %>%
  summarise(mean_val = mean(dtrip, na.rm = TRUE))


## Collapse the draws to get average trips per mode-month-kod
dtrips_1collapse <- directed_trips %>%
  group_by(mode, month, year, kod, ID_mode_month_yr_kod) %>%
  summarise(mean_dtrip_mmyk = mean(dtrip, na.rm = TRUE))


dtrips_1collapse <- dtrips_1collapse %>% 
  mutate(ID_mode_month = paste(mode, month, sep = "_"))

table(dtrips_1collapse$ID_mode_month)

## Collapse out kind of day, don't need year (we know only Nov is 2024)
dtrips_1collapse <- dtrips_1collapse %>%
  group_by(mode, month, ID_mode_month) %>%
  summarise(mean_dtrip_mm = sum(mean_dtrip_mmyk, na.rm = TRUE))


## Generate waves and wavemonth
dtrips_1collapse <- dtrips_1collapse %>%
  mutate(wave = case_when(
    month <= 2 ~ "1",
    month == 3|month==4 ~ "2",
    month == 5|month==6 ~ "3",
    month == 7|month==8 ~ "4",
    month == 9|month==10 ~ "5",
    month >= 11 ~ "6"
  ))

table(dtrips_1collapse$wave)

dtrips_1collapse <- dtrips_1collapse %>%
  mutate(wave_month = case_when(
    month == 1|month==3|month == 5|month==7|month == 9|month==11 ~ "wm1",
    month == 2|month==4|month == 6|month==8|month == 10|month==12 ~ "wm2"
  ))


#Wide out the month

# drop the month
dtrips_wide_wm <- subset(dtrips_1collapse, select = -c(month, ID_mode_month))

dtrips_wide_wm <- dtrips_wide_wm %>% spread(key = wave_month, value = mean_dtrip_mm)
# other way to go from long to wide
# dtrips_wide_wm <- dtrips_wide_wm %>% 
#   pivot_wider(names_from = wave_month, values_from = mean_dtrip_mm)

# replace na with 0, first one makes every na in the datafame 0
#dtrips_wide_wm[is.na(dtrips_wide_wm)] <- 0
dtrips_wide_wm <- dtrips_wide_wm %>% mutate_at(vars(wm1:wm2), ~replace(., is.na(.), 0))

# generate trips per mode-wave
dtrips_wide_wm <- dtrips_wide_wm %>% 
  mutate(dtrips_per_mode_wave = rowSums(across(c(wm1, wm2)), na.rm = TRUE))

#generate proportion of trips per wave in each wave-month
dtrips_wide_wm <- dtrips_wide_wm %>% mutate(weight_wm1_trips = wm1 / dtrips_per_mode_wave)
dtrips_wide_wm <- dtrips_wide_wm %>% mutate(weight_wm2_trips = wm2 / dtrips_per_mode_wave)

dtrips_wide_wm <- dtrips_wide_wm %>% 
  mutate(mode_wave = paste(mode, wave, sep = "_"))


# wide out the cod and hadd catch by wavemonth and merge in at mode wave level
# average haddock catch per trip = average haddock catch for march * weight_wm1_trips +
# average haddock catch for april * weight_wm2_trips


## basic line graph
ggplot(dtrips_wide_wm, aes(x = wave, y = dtrips_per_mode_wave, color = mode, group = mode)) + 
  geom_line() +
  labs(x = "Wave", y = "Number of Trips", color = "Mode") +
  scale_color_discrete(labels = c("pr" = "Private", "fh" = "For Hire")) + #didnt work
  ggtitle("Directed Trips") +
  scale_color_viridis_d(option = "plasma") +
  theme_classic() +
  geom_point() + scale_color_discrete(labels = c("For Hire", "Private"))





### CATCH PER TRIP

# catch per trip data is at the month-mode level for April-Nov, 201 draws
# only the for hire is open in April, only private open in Nov (14 obs per draw)
# GF catch per trip isn't broken out by state - yes, its all wgom

# start with catch per trip, make two data frames for haddock and cod, 
# bind on additional columns, then bind the rows

# Adding static columns like year
# assuming all 2025? or should wave 6 catch be assumed 2024 since wave 6 trips were 2024
mean_catch_per_trip$year <- "2025"
mean_catch_per_trip$data_version <- "2025V1"
mean_catch_per_trip$metric <- "catch-per-trip"
mean_catch_per_trip$units <- "number of fish"

# Generate wave from month
mean_catch_per_trip <- mean_catch_per_trip %>%
  mutate(wave = case_when(
    month <= 2 ~ "1",
    month == 3|month==4 ~ "2",
    month == 5|month==6 ~ "3",
    month == 7|month==8 ~ "4",
    month == 9|month==10 ~ "5",
    month >= 11 ~ "6"
  ))

##adding stuff 4/3
## Collapse the draws to get average catch per mode-month
catch_1collapse <- mean_catch_per_trip %>%
  group_by(mode, month, wave, year, data_version, metric, units) %>%
  summarise(mean_hadd_mm = mean(hadd_catch, na.rm = TRUE), mean_cod_mm = mean(cod_catch, na.rm = TRUE))

table(mean_catch_per_trip$mode, mean_catch_per_trip$month)

catch_1collapse <- catch_1collapse %>%
  mutate(wave_month = case_when(
    month == 1|month==3|month == 5|month==7|month == 9|month==11 ~ "wm1",
    month == 2|month==4|month == 6|month==8|month == 10|month==12 ~ "wm2"
  ))

# drop the month
catch_1collapse <- subset(catch_1collapse, select = -c(month))

catch_wide_wm <- catch_1collapse %>% 
  pivot_wider(names_from = wave_month, values_from = c(mean_cod_mm, mean_hadd_mm))

# replace na with 0
catch_wide_wm <- catch_wide_wm %>% mutate_at(vars(mean_cod_mm_wm2:mean_hadd_mm_wm1), ~replace(., is.na(.), 0))

# create mode wave variable to merge with directed trip weights
catch_wide_wm <- catch_wide_wm %>% 
  mutate(mode_wave = paste(mode, wave, sep = "_"))

#a bit of a mismatch because directed trips has april trip for private but 
#we don't have an april catch per trip estimate for private

merge_catch_trips <- full_join(catch_wide_wm, dtrips_wide_wm, by = "mode_wave")

merge_catch_trips <- merge_catch_trips %>% mutate_at(vars(mean_cod_mm_wm2:mean_hadd_mm_wm1), ~replace(., is.na(.), 0))

# average haddock catch per trip = average haddock catch for march * weight_wm1_trips +
# average haddock catch for april * weight_wm2_trips
merge_catch_trips <- merge_catch_trips %>% 
  mutate(mean_hadd_catch_per_wave = (mean_hadd_mm_wm1 * weight_wm1_trips) + (mean_hadd_mm_wm2 * weight_wm2_trips),
         mean_cod_catch_per_wave = (mean_cod_mm_wm1 * weight_wm1_trips) + (mean_cod_mm_wm2 * weight_wm2_trips))


# make these into side by side bar charts
p1 <- ggplot(na.omit(merge_catch_trips), aes(x = wave.x, y = mean_cod_catch_per_wave, color = mode.x, group = mode.x)) + 
  geom_line() +
  labs(x = "Wave", y = "Number of Fish", color = "Mode") +
  ggtitle("Average Cod Catch per Trip") +
  scale_color_viridis_d(option = "plasma") +
  theme_classic() +
  geom_point() + scale_color_discrete(labels = c("For Hire", "Private"))

p2 <- ggplot(na.omit(merge_catch_trips), aes(x = wave.x, y = mean_hadd_catch_per_wave, color = mode.x, group = mode.x)) + 
  geom_line() +
  labs(x = "Wave", y = "Number of Fish", color = "Mode") +
  ggtitle("Average Haddock Catch per Trip") +
  scale_color_viridis_d(option = "plasma") +
  theme_classic() +
  geom_point() + scale_color_discrete(labels = c("For Hire", "Private"))

p1 + p2   #doesnt work


###### NOW ITS TIME TO TURN merge_catch_trips into long and separate the species !!!!
# https://shanghai.hosting.nyu.edu/data/r/reshaping.html#:~:text=)%20library(dplyr)-,long%20to%20wide,wide%20format%20with%20spread()%20.&text=spread()%20converts%20data%20from,and%20value%20is%20from%20gpa%20.&text=We%20can%20rename%20the%20two%20new%20columns%20if%20we%20want%20to.&text=Recently%2C%20tidyr%20has%20updated%20spread,not%20be%20under%20active%20development.&text=If%20there%20are%20multiple%20variables,()%20in%20the%20values_from%20argument.






##back to 4/1 edits on mean_catch_per_trip - will be redoing the stuff below


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










  





# Calculate mean for every combination of Var1 and Var2
df %>%
  group_by(Var1, Var2) %>%
  summarise(mean_val = mean(TargetVar, na.rm = TRUE))

# Calculate mean for a specific subset only
df %>%
  filter(Var1 == "A", Var2 > 10) %>%
  summarise(mean_val = mean(TargetVar, na.rm = TRUE))



summary(mean_catch_per_trip)





### BASELINE CATCH AT LENGTH 
baseline_catch_at_length <- read.csv("cod_haddock/baseline_catch_at_length.csv")
# draw, season, length (inches or cm?), fitted probability, observed probability
#only a summer and winter season
#is summer waves 3-5 and winter waves 6-2? Ask Lou, Winter may be 6,1 only
table(baseline_catch_at_length$season)




