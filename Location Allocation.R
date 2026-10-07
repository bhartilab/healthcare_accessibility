#########################################################
##### Location Allocation Models #####

#This script will identify the ideal locations for new
#health facilities under different scenarios.

#This version is outdated -- it places facilities all at once, 
#rather than iteratively. 

#Last updated on 7/10/26, with edits and annotations 
#added on 10/1/26.

#########################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(terra)
library(sf)
library(spopt)
library(stringr)
library(gdistance)

setwd("/Volumes/LaCie")
set.seed(123)

#########################################################
##### Loading in data #####

#Load in health zones, filter to Equateur, then project to correct CRS
hzone <- st_read("Data/Grid3 Health Zones/GRID3_COD_health_zones_v9_0.shp")
hzone <- hzone %>% filter(province=="Equateur")
hzone <- st_transform(hzone, "EPSG:3201")

#Make template for all incoming data. This analysis is at 1km resolution.
targ <- rast(ext(hzone), res=1000, crs="EPSG:3201")

#Load in health facilities
hf <- st_read("Data/Grid3 Health Facilities/HF_v9_edited.shp")
hf <- st_transform(hf, "EPSG:3201")
hf <- st_crop(hf, ext(hzone))

#Create a raster version of health zones
hz_ras <- vect(hzone)
hz_ras <- rasterize(hz_ras, targ, field="zonesante")

#Load in baseline travel time
basett <- rast("Data/Travel Time Catchments/tt_baseline_v6.tif")
basett <- resample(basett, targ, method="bilinear")

#Load population data
p24 <- rast("Data/LandScan Population Density/LandScan24.tif")
p24 <- project(p24, targ, method="near")

#Create a dataframe where all observations is a populated point,
#and the columns give the # of people, residing health zone, and baseline
#travel time to health facility per point.
allstack <- c(hz_ras, p24, basett)
names(allstack) <- c("health zone", "pop", "traveltime")
allstack <- as.data.frame(allstack, xy=TRUE, na.rm=TRUE)

#########################################################
#### Demand and Candidate Points ####

# The location models require demand points (who needs 
# a facility), and candidate points (where facilities can
# be placed).

# Calculate demand weights based on population size and baseline
# travel time
allstack$weight <- log(allstack$pop + 1) * allstack$traveltime

# Define demand points: all points where the population is > 0
demand <- allstack %>% filter(pop>0)
demand <- st_as_sf(demand, coords=c("x","y"), crs="EPSG:3201")

#########################################################
##### Cost Surface ##### 

# The location models also require a cost surface that
# is N x M (total demand x total candidate points)
# where the values represent the time it takes to get from
# each demand point to each candidate point.

# !!!!If the matrix is already calculated, read it in here!!!!
time_matrix_jittered <- readRDS("Data/R output/timemat100.rds")

#Load in friction surface
friction_r <- rast("Data/Travel Time Catchments/traveltime_v6.tif")
friction_r <- resample(friction_r, targ, method="bilinear")
friction_r <- raster::raster(friction_r)

#Calculate transition matrix
transition_matrix <- transition(friction_r, transitionFunction = function(x) 1/mean(x), directions = 8)
#Correct for diagonal travel
transition_matrix <- geoCorrection(transition_matrix, type = "c")

#Turn demand into spatial points
demand_sp  <- as(demand, "Spatial")

#Generate cost surface
time_matrix <- costDistance(transition_matrix, fromCoords = demand_sp, toCoords = demand_sp)
#Convert to matrix
time_matrix <- as.matrix(time_matrix)

#Since the demand and candidate points are the same, we get a symmetric 
#matrix. Unfortunately, the location models will collapse this and it won't work.
#So - we need to jitter the values just a bit (by 1min) so it doesn't collapse.
jitter_noise <- matrix(runif(prod(dim(time_matrix)), min = -1, max = 1), nrow = nrow(time_matrix))
time_matrix_jittered <- pmax(time_matrix + jitter_noise, 0)
diag(time_matrix_jittered) <- 0

#Save the matrix if it's not already
saveRDS(time_matrix_jittered, "Data/R output/timemat100.rds")

#Clean up a bit
rm(demand_sp, friction_r, transition_matrix, time_matrix, jitter_noise)

#########################################################
##### Unlimited Resources Scenario #####

#This location model will place the minimum amount of 
#facilities needed to cover everyone within a specified
#service radius (in minutes).

#Run LSCP
lscp_results <- lscp(demand=demand, facilities=demand, service_radius=30, cost_matrix=time_matrix_jittered)
#Extract the facilities
lscp_results <- lscp_results$facilities[lscp_results$facilities$.selected,]
lscp_results <- lscp_results %>% dplyr::select(geometry)
    
#Plot where the facilities are
ggplot() + 
  #add health zones
  geom_sf(data=hzone, inherit.aes=FALSE) +
  #add the people
  geom_sf(data=demand, inherit.aes=FALSE, fill="yellow", color="yellow", size=0.5) + 
  #add the new facilities
  geom_sf(data=lscp_results, fill="red", color="red", size=0.5)

#Save the facilities -- make a dataframe that keeps
#both the existing and candidate facilities, tagging them
#with 'existing' and 'candidate'
allhf <- hf %>% dplyr::select(geometry) %>% mutate(status="existing")
lscp_results <- lscp_results %>% mutate(status="candidate")
allhf <- rbind(allhf, lscp_results)

st_write(allhf, paste("Data/R output/unlimited_30min.shp", sep=""), append=FALSE)

#########################################################
##### SYSTEM REALIGNMENT (TRAVEL TIME) #####

#This location model will replace the existing
#facilities inside Equateur, where the service radius 
#is based on travel time.

#Test scenario with 30 and 60 min service radii
servrad <- c(30,60)
mclp <- data.frame(geometry=I(list()), cands=character(), dems=character())

for (i in 1:length(servrad)) {
  print(servrad[[i]])
  
  #Run mclp
  mclp_results <- mclp(demand = demand, facilities = demand, n_facilities = nrow(hf), service_radius = servrad[[i]],
                       weight_col = "weight", cost_matrix=time_matrix_jittered)
  #Extract facilities
  mclp_results <- mclp_results$facilities[mclp_results$facilities$.selected,]
  mclp_results <- mclp_results %>% dplyr::select(geometry) %>% mutate(servrad=servrad[[i]])
  
  mclp <- rbind(mclp, mclp_results)
}

#Plot to see where the facilities were placed
ggplot() + 
  #add the health zones
  geom_sf(data=hzone, inherit.aes=FALSE) +
  #add the people
  geom_sf(data=demand, inherit.aes=FALSE, fill="yellow", color="yellow", size=0.5) + 
  #add the facilities  
  geom_sf(data=mclp, fill="red", color="red", size=0.5) + facet_wrap(~servrad)

#The model only replaced the facilities inside Equateur, 
#keeping the facilities directly outside of Equateur intact.
#Need to combine those and save. 

#Extract the facilities outside of Equateur, then save
z <- st_union(hzone)
hfout <- hf %>% dplyr::select(geometry) %>% mutate(bound = "outside")
hfout <- hfout[st_disjoint(hfout, z, sparse = FALSE), ]

dems <- unique(mclp$servrad)
for (i in dems) {
  mclpex <- mclp %>% filter(servrad == i) %>% dplyr::select(geometry) %>% mutate(bound="inside")
  mclpex <- rbind(mclpex, hfout)
  
  st_write(mclpex, paste("Data/R output/realignment_", i, "min.shp", sep=""), append=FALSE)
}
rm(mclpex, hfout, z)

#########################################################
##### SYSTEM REALIGNMENT (EUCLIDEAN) #####

#This location model will replace the existing
#facilities inside Equateur, where the service radius 
#is based on distance. Notice the cost matrix is not 
#included as an input.

#Test by placing facilities with a 5km and 10km service radius
servrad <- c(5000, 10000)
mclp <- data.frame(geometry=I(list()), cands=character(), dems=character())

for (i in 1:length(servrad)) {
  print(servrad[[i]])
  
  #Run MCLP
  mclp_results <- mclp(demand = demand, facilities = demand, n_facilities = nrow(hf), 
                       service_radius = servrad[[i]], weight_col = "weight")
  #Extract facilities
  mclp_results <- mclp_results$facilities[mclp_results$facilities$.selected,]
  mclp_results <- mclp_results %>% dplyr::select(geometry) %>% mutate(servrad=servrad[[i]])
  
  mclp <- rbind(mclp, mclp_results)
}

#Plot the facilities
ggplot() + 
  #add the health zones
  geom_sf(data=hzone, inherit.aes=FALSE) +
  #add the people
  geom_sf(data=demand, inherit.aes=FALSE, fill="yellow", color="yellow", size=0.5) + 
  #add the facilities
  geom_sf(data=mclp, fill="red", color="red", size=0.5) + facet_wrap(~servrad)

#The model only replaced the facilities inside Equateur, 
#keeping the facilities directly outside of Equateur intact.
#Need to combine those and save. 

#Extract the facilities outside of Equateur, then save
z <- st_union(hzone)
hfout <- hf %>% dplyr::select(geometry) %>% mutate(bound = "outside")
hfout <- hfout[st_disjoint(hfout, z, sparse = FALSE), ]

dems <- unique(mclp$servrad)
for (i in dems) {
  mclpex <- mclp %>% filter(servrad == i) %>% dplyr::select(geometry) %>% mutate(bound="inside")
  mclpex <- rbind(mclpex, hfout)
  
  st_write(mclpex, paste("Data/R output/realignment_", i, "m.shp", sep=""), append=FALSE)
}
rm(mclpex, hfout, z)

#########################################################
##### LIMITED RESOURCES #####

#This location model places a limited number of facilities,
#adding on to the existing. It uses a service radius based
#on time (not euclidean). 

#Define the # of facilities to place (the numbers below are for
#a 10% (n=35) and 25% (n=86) increase from the existing 344 facilities.
nfacil <- c(35, 86)
mclp_nfacil <- data.frame(geometry=I(list()), cands=character())

for (i in nfacil) {
    print(i)
  
    #Run MCLP
    mclp_results <- mclp(demand = demand, facilities = demand, n_facilities = i, service_radius = 120,
                         weight_col = "weight", cost_matrix=time_matrix_jittered)
    #Extract facilities
    mclp_results <- mclp_results$facilities[mclp_results$facilities$.selected,]
    mclp_results <- mclp_results %>% dplyr::select(geometry) %>% mutate(nfacil=i)
    
    mclp_nfacil <- rbind(mclp_nfacil, mclp_results)
}

#Plot to see where the facilities are
ggplot() +  
  #add the health zones
  geom_sf(data=hzone, inherit.aes=FALSE) +
  #add the people
  geom_sf(data=demand, color="yellow", size=0.5) + 
  #add the existing health facilities
  geom_sf(data=hf, fill="black", color="black") + 
  #add the new facilities
  geom_sf(data=mclp_nfacil, fill="red", color="red") + facet_wrap(~nfacil)

#Save the facilities -- make a dataframe that keeps
#both the existing and candidate facilities, tagging them
#with 'existing' and 'candidate'
for (i in nfacil) {
    allhf <- hf %>% dplyr::select(geometry) %>% mutate(status="existing")
    mclp_filtered <- mclp_nfacil %>% filter(nfacil == i) %>% dplyr::select(geometry) %>% mutate(status="candidate")
    allhf <- rbind(allhf, mclp_filtered)
    st_write(allhf, paste("Data/R output/limited_", i, "facils.shp", sep=""), append=FALSE)
}
rm(allhf)
