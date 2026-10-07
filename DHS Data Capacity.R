#########################################################
##### DHS DATA - CAPACITY METRICS #####

#This script is meant to examine the DHS data 2023-24,
#specifically with respect to health care capacity.
#HOWEVER, I didn't really get far with this script
#beyond loading in the data because I ended up not using 
#DHS data in general. Therefore, I'm not sure how much utility 
#this code has since there are packages specifically used for 
#cleaning DHS data that could make this a lot easier.

#Last updated 5/27/26 with annotations added on 10/1/26

#########################################################

library(haven)
library(stats)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(terra)

setwd("/Volumes/LaCie")

#########################################################
#### Read in and clean data ####

#Load in DHS cluster level data
geo <- st_read("DHS Data/CDGE81FL Spatial Data/CDGE81FL.shp")
geo <- st_transform(geo, "EPSG:3201")
geo <- st_crop(geo, ext(hzone))

clust <- geo$DHSCLUST

#Load in facility data
fac <- read_sas("DHS Data/CDFC71SDSP Facility/CDFC71FLSP.SAS7BDAT")
fac <- fac %>% dplyr::select(FACIL, REC_TYPE, PROVINCE, ZONE, FTYPE,     #facility descriptions 
                             FACHIVTST, FACHIVDXTX,                      #hiv test/treatment 
                             FACMALTST, FACMALDXTX,                      #malaria test/treatment
                             ELIGPROV,                                   #N of eligible providers
                             Q102_07,                                    #labor/delivery services
                             Q1012_2, Q1012_3,                           #polio/measles vax
                             Q460,                                       #health data system
                             Q465,                                       #designated employee infectious disease
                             Q1203)                                      #follow ICMI guidelines

#Combine all data
dhs <- left_join(mh, ind, by=c("V001", "V002"), relationship="many-to-many") %>% 
  left_join(., kid, by=c("V001", "V002"), relationship="many-to-many") %>% 
  left_join(., hh, by=c("V001", "V002"))

clust <- geo %>% dplyr::select(DHSCLUST, geometry) %>% rename(V001=DHSCLUST)
dhs <- left_join(dhs, clust, by="V001") %>% unique()



