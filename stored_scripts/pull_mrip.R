# R script to get MRIP data

#Load Libraries
library("tidyverse")
library("here")
library("glue")
tacklebox_main_lib <- file.path(Sys.getenv("R_LIBS_USER"), "MRIPtacklebox_main_install")
library("mriptacklebox",lib.loc = tacklebox_main_lib)
library("conflicted")

# deal with directories
here::i_am("stored_scripts/pull_mrip.R")

#get a data vintage
data_vintage<-as.character(Sys.Date())


# pull trip data
 y <- 2022:2025
 w <- 1:6
 pd <- '~/mrfss/products/mrip_estim/Public_data_cal2018'
 mrip_statistics<-mrip_microdata(pubdir = pd, years = y, waves = w,
               typ = c("trip", "catch","size","size_b2"), format = 'sas7bdat')

 write_rds(
   mrip_statistics,
   file=here("data","raw",glue("mrip_statistics_{data_vintage}.Rds")) )
#Unroll them like this
 
#trip<-mrip_statistics$trip
#catch<-mrip_statistics$catch
#size<-mrip_statistics$size
#size_b2<-mrip_statistics$size_b2
 
