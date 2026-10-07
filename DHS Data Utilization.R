#########################################################
##### DHS DATA - UTILIZATION METRICS #####

#This script is meant to examine the DHS data 2023-24,
#specifically with respect to health care utilization.

#I cleaned a lot of this myself, but there are packages
#specifically used for DHS data that could make this
#a lot easier.

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
#### Read in data ####

#Load in health zones, filter to Equateur, then project to correct CRS
hzone <- st_read("Data/Grid3 Health Zones/GRID3_COD_health_zones_v9_0.shp")
hzone <- hzone %>% filter(province=="Equateur")
hzone <- st_transform(hzone, "EPSG:3201")

#Load in DHS cluster level data
geo <- st_read("DHS Data/CDGE81FL Spatial Data/CDGE81FL.shp")
geo <- st_transform(geo, "EPSG:3201")
geo <- st_crop(geo, ext(hzone))

clust <- geo$DHSCLUST

#Load in household level data
hh <- read_sas("DHS Data/CDHR81SD Household/CDHR81FL.SAS7BDAT")
hh <- hh %>% 
  #only keep the data from the clusters we need
  filter(HV001 %in% clust) %>% 
  #save cluster, household number, and wealth index
  dplyr::select(HV001, HV002, HV270) %>% 
  rename(V001=HV001, V002=HV002)

#Load in maternal health data
mh <- read_sas("DHS Data/CDNR81SD Pregnancy/CDNR81FL.SAS7BDAT")
mh <- mh %>% 
  #only keep the data from the clusters we need
  filter(V001 %in% clust) %>% 
  #save cluster, household number, and variables of interest
  dplyr::select(V001, V002, V005, V021, V022, V023, M15)

#Load in individual level data
ind <- read_sas("DHS Data/CDIR81SD Individual/CDIR81FL.SAS7BDAT")
ind <- ind %>% 
  #only keep the data from the clusters we need
  filter(V001 %in% clust) %>% 
  #save cluster, household number, and variables of interest
  dplyr::select(V001, V002, V025, V158, V467B, V467C, V467D, V467F, V483A, V483B, V484A, V484B, V781)

#Load in child health data
kid <- read_sas("DHS Data/CDKR81SD Children/CDKR81FL.SAS7BDAT")
kid <- kid %>% 
  #only keep the data from the clusters we need
  filter(V001 %in% clust) %>% 
  #save cluster, household number, and variables of interest
  dplyr::select(V001, V002, H10, H1, H4, H9, H32A:H32H, H32J:H32NC, H46B, H12A:H12H, H12J:H12NB)

#Combine all of the that we just loaded in
dhs <- left_join(mh, ind, by=c("V001", "V002"), relationship="many-to-many") %>% 
  left_join(., kid, by=c("V001", "V002"), relationship="many-to-many") %>% 
  left_join(., hh, by=c("V001", "V002"))

clust <- geo %>% dplyr::select(DHSCLUST, geometry) %>% rename(V001=DHSCLUST)
dhs <- left_join(dhs, clust, by="V001") %>% unique()

#########################################################
#### Do some recoding of values ####

#These were determined by reading through the DHS code book online

dhs_index <- dhs %>% unique() %>%
#where birth took place
  mutate(M15 = case_when(M15 <= 12 ~ "Home", M15 > 12 & M15 <= 35 ~ "Health facility", M15 == 96 ~ "Other")) %>%
#is getting permission a big problem when going to facility               
  mutate(V467B = case_when(V467B == 1 ~ "Big problem", V467B == 2 ~ "Not a big problem")) %>%
#is getting money a big problem when going to facility               
  mutate(V467C = case_when(V467C == 1 ~ "Big problem", V467C == 2 ~ "Not a big problem")) %>%
#is distance a big problem when going to facility               
  mutate(V467D = case_when(V467D == 1 ~ "Big problem", V467D == 2 ~ "Not a big problem")) %>%
#is going alone a big problem when going to facility               
  mutate(V467F = case_when(V467F == 1 ~ "Big problem", V467F == 2 ~ "Not a big problem")) %>%
#typical transport to facility               
  mutate(V483B = case_when(V483B <= 19 ~ "Motorized", V483B > 19 ~ "Not motorized")) %>%
#went to hf for fever
  mutate(H32 = rowSums(across(starts_with("H32")), na.rm = TRUE)) %>% dplyr::select(-(H32A:H32NC)) %>%
#went to hf for diarrhea
  mutate(H12 = rowSums(across(starts_with("H12")), na.rm = TRUE)) %>% dplyr::select(-(H12A:H12NB)) 
               
#Code below changes variables to binary, 
#keeping only the variables that will be used in the index, 
#then recodes all NAs to 0
#below, 1 = good/utilized, 0 = bad/not utilized
dhs_index <- dhs_index %>% 
#rural or urban
  mutate(urban = if_else(V025 == 1, 1, 0)) %>%
#radio at home y/n
  mutate(radio = if_else(V158 >= 1, 1, 0)) %>%
#had birth at hf, y/n
  mutate(birth = if_else(M15 == "Health facility", 1, 0)) %>%
#health care barrier - getting permission
  mutate(bar_permiss = if_else(V467B == "Big problem", 0, 1)) %>%
#health care barrier - getting money
  mutate(bar_money = if_else(V467C == "Big problem", 0, 1)) %>%
#health care barrier - distance
  mutate(bar_dist = if_else(V467D == "Big problem", 0, 1)) %>%
#health care barrier - going alone
  mutate(bar_alone = if_else(V467F == "Big problem", 0, 1)) %>%
#received MCV
  mutate(mcv = if_else(H9 >= 1, 1, 0)) %>%
#recevied polio vax
  mutate(polio = if_else(H4 >= 1, 1, 0)) %>%
#went to hf for fever or diarrhea
  mutate(hfvisit = if_else(H32 == 1 | H12 == 1, 1, 0)) %>%
#has a health card
  mutate(healthcard = if_else(H1 >= 1, 1, 0)) %>%
  dplyr::select(V483A, V781, bar_permiss, bar_money, bar_dist, bar_alone, mcv, polio,
                birth, hfvisit, healthcard, urban, radio, HV270) %>%
  rename(hiv = V781, tt = V483A, wealth=HV270) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  mutate(across(everything(), ~replace(., . == 8, 0)))

#Rescale the self-reported travel time to health facility from 0 to 1
dhs_index$tt_scaled <- scales::rescale(dhs_index$tt, to=c(1,0))
#Rescale the self-reported wealth index
dhs_index$wealth_scaled <- scales::rescale(dhs_index$wealth)
dhs_index <- dhs_index %>% dplyr::select(-tt, -wealth)

#########################################################
#### Calculate the utilization index ####

#Domain 1 - barriers to getting health care
dhs_index$d1 <- (dhs_index$bar_permiss + dhs_index$bar_money + dhs_index$bar_dist + dhs_index$bar_alone) / 4
#Domain 2 - services utilized
dhs_index$d2 <- (dhs_index$birth + dhs_index$hiv + dhs_index$mcv + dhs_index$polio + dhs_index$hfvisit + dhs_index$healthcard) / 6
#Domain 3 - distance to HF / urbanicity
dhs_index$d3 <- (dhs_index$tt_scaled + dhs_index$urban) / 2
#Domain 4 - wealth
dhs_index$d4 <- (dhs_index$radio + dhs_index$wealth_scaled) / 2

#Combine the 4 domains into 1 metric
#Tested two different ways of weighting the domains -- 
#-A more service focused metric, that emphasizes the services used
#-A user focused metric, that emphasizes why a person may or may not go to a HF
dhs_index$index_serv <- (dhs_index$d1 * 0.2) + (dhs_index$d2 * 0.6) + (dhs_index$d3 * 0.1) + (dhs_index$d3 * 0.1)
dhs_index$index_user <- (dhs_index$d1 * 0.4) + (dhs_index$d2 * 0.4) + (dhs_index$d3 * 0.1) + (dhs_index$d3 * 0.1)

#Examine the differences between weighting service focused vs user focused
cor(dhs_index$index_serv, dhs_index$index_user)
plot(dhs_index$index_serv, dhs_index$index_user)

#########################################################
#### Turning the DHS clusters into polygons ####

#Try to draw polygons of the DHS clusters that they
#took data from. Warning: it's... really ugly....
#Maybe don't actually do this lol.

clust_poly <- st_union(clust) %>% st_voronoi()
clust_poly <- st_as_sf(st_collection_extract(clust_poly, "POLYGON"))
clust_poly <- st_crop(clust_poly, ext(hzone))

#########################################################
#### Visualize the data ####

#Add our utilization index to the bigger DHS dataframe
dhs$index <- dhs_index$index_user
dhs <- st_set_geometry(dhs, "geometry")

#Calculate the mean utilization index value by DHS cluster and plot
cluster_zone <- dhs %>% distinct() %>% group_by(V001) %>% 
  summarize(mean_index = mean(index, na.rm = TRUE), households = n())

ggplot() + 
  geom_sf(data=hzone, inherit.aes=FALSE) + 
  geom_sf(data=cluster_zone, aes(size=households, color=mean_index)) + 
  scale_color_viridis_c()

#Calculate the mean utilization index by health zone and plot
cluster_zone <- dhs %>% distinct() %>% group_by(V001) %>% 
  summarize(mean_index = mean(index, na.rm = TRUE), households = n())
cluster_zone <- st_join(clust_poly, cluster_zone)

ggplot() + 
  geom_sf(data=cluster_zone, aes(fill=mean_index)) + 
  geom_sf(data=hzone, color="black", fill="transparent")

#########################################################
#### Log Regression ####

#Code below is for testing whether there is an association 
#between specified variables and distance as a perceived barrier 
#to accessing health care

#Pull from the DHS dataframe
regressdata <- dhs %>% as.data.frame() %>% unique() %>% 
  #Just select the variables we need
  dplyr::select(V025, V467B, V467C, V467D, V467F, V158, V483A, V483B, HV270) %>%
  #In order, urban/rural, permission as barrier (to health care), money as barrier,
  #distance as barrier, going alone as barrier, travel time, having a radio,
  #method of transport, wealth index
  rename(urban=V025, permiss=V467B, money=V467C, dist=V467D, alone=V467F, ttime=V483A, radio=V158, transport=V483B, wealth=HV270) %>%
  #Recode the perceived barrier questions as binary and set levels
  mutate(across(c("permiss", "money", "dist", "alone"), ~if_else(. == 1, 1, 0))) %>%
  mutate(across(c("permiss", "money", "dist", "alone"), ~factor(., levels=c(0,1), labels=c("Big problem", "Not a big problem")))) %>%
  #Recode the transport variable as motorized vs nonmotorized
  mutate(transport = case_when(transport <= 19 ~ 1, transport > 19 ~ 0))

regressdata$transport <- factor(regressdata$transport, levels=c(0,1), labels=c("Not motorized", "Motorized"))
regressdata$wealth <- factor(regressdata$wealth)
regressdata$radio <- factor(regressdata$radio)
regressdata$urban <- factor(regressdata$urban)

#Run regression
mod <- glm(dist ~ ., data=regressdata, family="binomial")
summary(mod)

#Get OR, CI, and p-values
or <- exp(cbind(oddsratio = coef(mod), confint(mod)))
pval <- summary(mod)$coefficients[,4]
regressfinal <- data.frame(or, pvalue = pval)
kable(regressfinal, digits = 3, col.names = c("Odds Ratio", "Lower CI", "Upper CI", "P-Value"))

