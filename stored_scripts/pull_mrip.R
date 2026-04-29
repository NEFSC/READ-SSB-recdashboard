###############################################################################
# Purpose:  A small R script to get MRIP data off the network and save it.  

# Requirements:
# On the NEFSC network or on VPN
# Have access to mrfss drive
# In your .Rprofile, set the variable "network_path" to contain the directory that is 1 level up from the mrfss directory.

# Outputs:
# mrip_statistics_{data_vintage}.Rds - 
# List containing trips, catch, size, size_b2 

###############################################################################  
# Purpose 

# Depends: 




#Load Libraries
library("tidyverse")
library("tigris")
library("here")
library("glue")
tacklebox_main_lib <- file.path(Sys.getenv("R_LIBS_USER"), "MRIPtacklebox_main_install")
library("mriptacklebox",lib.loc = tacklebox_main_lib)
library("conflicted")

# deal with directories
here::i_am("stored_scripts/pull_mrip.R")

#get a data vintage
data_vintage<-as.character(Sys.Date())

# set directory of mrip data
# pd <- file.path(network_path,"mrfss","products","mrip_estim","Public_data_cal2018")



# pull complete years of trip data
y <- 2023:2025
w <- 1:6

mrip_statistics<-mrip_microdata(pubdir = pd, years = y, waves = w,
              typ = c("trip", "catch","size","size_b2"), format = 'sas7bdat')

write_rds(
  mrip_statistics,
  file=here("data","raw",glue("mrip_statistics_{data_vintage}.Rds")) )



##################################
# You may want to pull partial years.   
#################################
# 
# y <- 2023
# w <- 5:6
# 
# mrip_statistics_period1<-mrip_microdata(pubdir = pd, years = y, waves = w,
#                                 typ = c("trip", "catch","size","size_b2"), format = 'sas7bdat')
# 
# y2 <- 2024
# w2 <- 1:4
# 
# mrip_statistics_period2<-mrip_microdata(pubdir = pd, years = y2, waves = w2,
#                                 typ = c("trip", "catch","size","size_b2"), format = 'sas7bdat')
# 
# mrip_statistics <- list(
#   trip  = bind_rows(mrip_statistics_period1$trip,  mrip_statistics_period2$trip),
#   catch = bind_rows(mrip_statistics_period1$catch, mrip_statistics_period2$catch),
#   size = bind_rows(mrip_statistics_period1$size, mrip_statistics_period2$size),
#   sizeb2 = bind_rows(mrip_statistics_period1$sizeb2, mrip_statistics_period2$sizeb2)
# )
# 
  
 
 
#Unroll them like this

#trip<-mrip_statistics$trip
#catch<-mrip_statistics$catch
#size<-mrip_statistics$size
#size_b2<-mrip_statistics$size_b2
 
 
 
 
