## separating out lower half of groundfish_trips_catch.R
## This file (messily) calculates estimates by fishing year and has some notes and
# some to-do items for Tess

# First need to run groundfish_trips_catch.R




##Note: If we use the older pull from 4/10, the 2024 directed trips match lou's but
## 2025 does not because he was using older data when we ran his script to get yearly_mrip_stats.dta 
# The 4/29 pull must have had a tiny update to 2024 trips compared to the 4/10 pull
# 4/10 pull is here on Tess's machine
# confirm that the numbers for this pull for FY2024 match what lou has. 
# If we both pulled MRIP on same day and ran our respective code, it should match
mrip_statistics <- readRDS("~/GitHub/mrip_statistics_2026-04-10.Rds")
file_date<-"2026-04-10"


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
# footnote on fy2025_imp: "Waves 2 and 6 of 2024 used as proxies for FY 2025",
# the current fy's are waves 3, 4, 5



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


##### Trips by mode table
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
# footnote on fy2025_imp: "Waves 2 and 6 of 2024 used as proxies for FY 2025"






#### Just grabbing the catch numbers to compare with Lou's numbers 
#(have not added them into the tables like I did for trips)
cod_hadd_all_w2 <- cod_hadd_all_w2 %>%
  mutate(fy2024 = case_when(
    year == 2024 & wave >= 3 ~ 1,
    year == 2025 & wave == 2 ~ 1,
    TRUE ~ 0 
  ))

cod_hadd_all_w2 <- cod_hadd_all_w2 %>%
  mutate(fy2025_imp = case_when(
    year == 2024 & wave == 2 ~ 1,
    year == 2024 & wave == 6 ~ 1,
    year == 2025 & wave == 3 ~ 1,
    year == 2025 & wave == 4 ~ 1,
    year == 2025 & wave == 5 ~ 1,
    TRUE ~ 0 
  ))

#These are super close to lou, it would likely match with the 4/10 data
sum(cod_hadd_all_w2$catch[cod_hadd_all_w2$fy2024 == 1 & cod_hadd_all_w2$common == "atlanticcod"], na.rm = TRUE)
sum(cod_hadd_all_w2$catch[cod_hadd_all_w2$fy2024 == 1 & cod_hadd_all_w2$common == "haddock"], na.rm = TRUE)
sum(cod_hadd_all_w2$discards[cod_hadd_all_w2$fy2024 == 1 & cod_hadd_all_w2$common == "atlanticcod"], na.rm = TRUE)
sum(cod_hadd_all_w2$discards[cod_hadd_all_w2$fy2024 == 1 & cod_hadd_all_w2$common == "haddock"], na.rm = TRUE)

sum(cod_hadd_all_w2$catch[cod_hadd_all_w2$fy2025_imp == 1 & cod_hadd_all_w2$common == "atlanticcod"], na.rm = TRUE)
sum(cod_hadd_all_w2$catch[cod_hadd_all_w2$fy2025_imp == 1 & cod_hadd_all_w2$common == "haddock"], na.rm = TRUE)


# cod_hadd_catch <- cod_hadd_catch %>%
#   mutate(fy2024 = case_when(
#     year == 2024 & wave >= 3 ~ 1,
#     year == 2025 & wave == 2 ~ 1,
#     TRUE ~ 0 
#   ))
# sum(cod_hadd_catch$value[cod_hadd_catch$fy2024 == 1 & cod_hadd_catch$common == "atlanticcod" & cod_hadd_catch$metric == "catch"], na.rm = TRUE)





###### TO DO's
# Make a df for catch per trip (merge trips into cod_hadd_catch on state mode yr wave?, do division, clean, append?)
# Append trips, catch, catch per trip
# Keep populating the tables with catch and catch per trip? Make some plots?
## needs cleanup,  so code that doesn't need repeating isn't repeated and things are named intuitively. Automate-able.
# get feedback on efficiency, professionalizing and changes to make that make kim's job of functionizing easier





#### Catch per trip ####
cod_hadd_all_w2 <- cod_hadd_all_w2 %>%
  mutate(fy2024 = case_when(
    year == 2024 & wave >= 3 ~ 1,
    year == 2025 & wave == 2 ~ 1,
    TRUE ~ 0 
  ))
cod_hadd_all_w2 <- cod_hadd_all_w2 %>%
  mutate(fy2025_imp = case_when(
    year == 2024 & wave == 2 ~ 1,
    year == 2024 & wave == 6 ~ 1,
    year == 2025 & wave == 3 ~ 1,
    year == 2025 & wave == 4 ~ 1,
    year == 2025 & wave == 5 ~ 1,
    TRUE ~ 0 
  ))

# I was calculating catch per trip using trip_species_composition to get trips that 
# targeted haddock or had haddock catch (so haddock_only or cod_and_haddock trips) and trips that caught/targeted 
# cod rather than dividing catch by the cod/haddock angler trips metric (ie, haddock_only + cod_and_haddock + cod_only trips)

# trips that had any cod catch: (this was done before you collapsed)
#sum(cod_hadd_all_w2$dtrip[cod_hadd_all_w2$fy2024 == 1 & cod_hadd_all_w2$common == "atlanticcod"], na.rm = TRUE)

#sum(cod_hadd_catch$value[cod_hadd_catch$fy2024 == 1 & cod_hadd_catch$common == "atlanticcod" & cod_hadd_catch$metric == "catch"], na.rm = TRUE)

#quick check on catch per trip cod 2024
267891.9/117430.8
# I got 2.28 but lou got 1.15
267885/231963
# lou divided cod catch by total cod and/or haddock trips not just trips that caught cod


#sum(cod_hadd_catch$value[cod_hadd_catch$fy2024 == 1 & cod_hadd_catch$common == "haddock" & cod_hadd_catch$metric == "catch"], na.rm = TRUE)
#sum(cod_hadd_all_w2$dtrip[cod_hadd_all_w2$fy2024 == 1 & cod_hadd_all_w2$common == "haddock"], na.rm = TRUE)
1385001/188674.3
#lou:
1384427/231963








