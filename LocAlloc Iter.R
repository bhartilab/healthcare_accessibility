#########################################################
##### location allocation #####

#This script will calculate candidate locations for potential
#health facilities. Different from what spopt models originally do,
#this will iteratively place health facilities.

#last updated 10/13/26

#########################################################
##### libraries and wd #####
library(dplyr)
library(tidyr)
library(ggplot2)
library(terra)
library(sf)
library(spopt)
library(stringr)
library(gdistance)
library(patchwork)
library(classInt)

setwd("/Volumes/LaCie")
set.seed(123)

#########################################################
##### data needed #####

#Load in baseline travel time from ArcGIS (30m)
base <- rast("Data/Travel Time Catchments/tt_baseline_v6.tif")
base_out <- rast("Data/Travel Time Catchments/tt_baseline_v6_out.tif")

#Need to create a template for other datasets that need cropping and resampling
targ1km <- rast(ext(base), res=1000, crs="EPSG:3201")

#Make a copy of baseline travel time that is 1km resolution
base <- resample(base, targ1km, method = "bilinear")
base_out <- resample(base_out, targ1km, method = "bilinear")

#Load in friction surface (30m) and resample to 1km resolution
friction_r <- rast("Data/Travel Time Catchments/traveltime_v6.tif")
friction_r <- resample(friction_r, targ1km, method="bilinear")

#Load in health zones - filter down to those in Equateur province and project
hzone <- st_read("Data/Grid3 Health Zones/GRID3_COD_health_zones_v9_0.shp")
hzone <- hzone %>% filter(province=="Equateur")
hzone <- st_transform(hzone, "EPSG:3201")
#Create a rasterized version of the health zones
hz_ras <- vect(hzone)
hz_ras <- rasterize(hz_ras, targ1km, field="zonesante")

#Load in health facilities
hf <- st_read("Data/Grid3 Health Facilities/HF_v9_edited.shp")
hf <- st_transform(hf, "EPSG:3201")

#Merge all health zones into one polygon -- extract all HF outside of this
hzone_one <- st_union(hzone)
hf_outside <- hf[st_disjoint(hf, hzone_one, sparse = FALSE), ]
rm(hzone_one)

#Load in population estimates and crop to extent (1km)
p24 <- rast("Data/LandScan Population Density/LandScan24.tif")
p24 <- project(p24, targ1km, method="near")

#Create one data frame for all populated points, that includes the
#coordinates, health zone, and baseline travel time
allstack <- c(hz_ras, p24, base, base_out)
names(allstack) <- c("healthzone", "pop", "traveltime", "ttout")
allstack <- as.data.frame(allstack, xy=TRUE, na.rm=TRUE)

#########################################################
##### DEMAND AND CANDIDATE POINTS

#Demand points will be defined by all populated points.
#Candidate points will be defined by all populated points.

demcan <- allstack %>% filter(pop>0)
demcan <- st_as_sf(demcan, coords=c("x","y"), crs="EPSG:3201")

#Weights will be derived from population and baseline
#travel time. For System Realignment, weights will 
#be derived by population only.
demcan$weight_poptt <- log(demcan$pop + 1) * (demcan$traveltime^2)
demcan$weight_pop <- log(demcan$pop + 1) 

#Visualize demand/candidate points with weights
ggplot() + geom_sf(data=hzone, inherit.aes=FALSE) + geom_sf(data=demcan, aes(color=weight_poptt), size=0.5) + 
  scale_color_viridis_c(option="magma", direction=-1)

#########################################################
##### Cost Surface
#A cost surface is necessary for input into spopt models,
#that display the travel cost of moving from all demand
#points to all candidate points.

###Read in file if already made
#cost_matrix <- readRDS("Data/R output/timemat100.rds")
###

#Convert the surfaces to raster objects
friction <- raster::raster(friction_r)
#Prepare demand points for costDist function
demand_sp  <- as(demcan, "Spatial")

#Create transition matrix
base_conductance <- transition(friction, transitionFunction = function(x) 1 / mean(x), directions = 16)
#Geocorrect for diagonal movement
base_conductance <- geoCorrection(base_conductance, type = "c")
#Calculate travel time from all pairs of demand and candidate points
cost_matrix <- costDistance(base_conductance, fromCoords = demand_sp, toCoords = demand_sp)
cost_matrix <- as.matrix(cost_matrix)

##cost_matrix is a symmetric matrix - 
##need to jitter values just a bit so R does not collapse the matrix in the mclp models
##edited 10/13/26: I actually ran the MCLP with the symmetric matrix (no jittering).
##I'll leave this here but commented out in case it's needed later.
# jitter_noise <- matrix(runif(prod(dim(cost_matrix)), min = -1, max = 1), nrow = nrow(cost_matrix))
# time_matrix_jittered <- pmax(cost_matrix + jitter_noise, 0)
# diag(time_matrix_jittered) <- 0

#Clean up the matrices generated in this step
rm(base_conductance, cost_matrix, jitter_noise)

#Save to RDS object for later
saveRDS(cost_matrix, "Data/R output/timemat100.rds")

#########################################################
##### (UN)LIMITED RESOURCES
#This scenario will place health facilities until all 
#populated pixels are within X amount of travel time.

#Get existing health facilities and give ID number
hf_points <- hf %>% dplyr::select(geometry) %>% mutate(id = 1:nrow(hf), status = "existing", global_id = 1:nrow(hf))
#Make copy of dem/can points for use in loop
dem <- demcan
can <- demcan
time_mat <- cost_matrix#time_matrix_jittered
#Other misc params
nhf <- nrow(hf)
count <- 1
#Grab current travel time
current_tt <- dem$traveltime

repeat {
  
  print(count)
  
  ###Step 1: Place 1 facility.
  mclp_results <- mclp(demand = dem, facilities = can, n_facilities = 1, service_radius = 120,
                       weight_col = "weight_poptt", cost_matrix=time_mat)
  print("Facility placed.")
  
  #Identify which facility was chosen
  index <- which(mclp_results$facilities$.selected)
  #Isolate chosen facility from list of candidates, save to running list of HF
  newfac <- can[index,]
  newfac <- newfac %>% dplyr::select(geometry) %>% mutate(id = count, 
            status = "candidate", global_id = nhf+count)
  hf_points <- rbind(hf_points, newfac)
  
  #Extract travel times to new facility
  newtime <- time_mat[,index]
  current_tt <- pmin(current_tt, newtime, na.rm=TRUE)
  dem$traveltime <- current_tt
  
  #Update weights - if travel time <120, change to 0
  dem$weight_poptt <- log(dem$pop + 1) * (dem$traveltime^2)
  dem$weight_poptt[dem$traveltime <= 120] <- 0

  #Extract rows that are still above the travel time req
  active_indices <- which(dem$traveltime > 120)
  #Update candidates and time matrix
  can <- demcan[active_indices,]
  time_mat <- cost_matrix[,active_indices]
  
  #3b STOPPING CRITERIA
  if (max(dem$traveltime) <= 120) {
    print("Exiting.")
    break} 
  else if (nrow(can) == 1) {
    print("No more candidates.")
    break}
  
  #Status update and add to counter
  print(paste("Max: ", round(max(dem$traveltime, na.rm=TRUE), 2), 
              " Mean: ", round(mean(dem$traveltime, na.rm=TRUE), 2), sep=""))
  count <- count + 1
}

#Visualize
ggplot() +  geom_sf(data=hzone, inherit.aes=FALSE) +
  geom_sf(data=demcan, color="yellow") + 
  geom_sf(data=hf_points, aes(color=status))

#Export
st_write(hf_points, paste("Data/R output/unlimited_limited.shp", sep=""), append=FALSE)

#########################################################
##### SYSTEM REALIGNMENT BY TRAVEL TIME
#This scenario will replace the existing
#health facilities. Service radius is given by travel time.

#Get existing health facilities and give ID number
hf_points <- data.frame(geometry=I(list()), id=numeric())
#Make copy of dem/can points for use in loop
dem <- demcan
can <- demcan
time_mat <- cost_matrix#time_matrix_jittered
#Other misc params
nhf <- nrow(hf)-nrow(hf_outside)
#Adding in the existence of health facilities outside of Equateur
dem <- dem %>% mutate(weight_pop = if_else(traveltime == ttout & ttout <= 120, 0, weight_pop)) %>%
  mutate(traveltime = if_else(weight_pop == 0, traveltime, Inf)) %>% dplyr::select(-ttout)
current_tt <- dem$basett #dem$traveltime

for (i in 1:nhf) {
  
  print(i)
  
  ###Step 1: Place 1 facility.
  mclp_results <- mclp(demand = dem, facilities = can, n_facilities = 1, service_radius = 120,
                       weight_col = "weight_pop", cost_matrix=time_mat)
  print("Facility placed.")
  
  #Identify which facility was chosen
  index <- which(mclp_results$facilities$.selected)
  #Isolate chosen facility from list of candidates, save to running list of HF
  newfac <- can[index,]
  newfac <- newfac %>% dplyr::select(geometry) %>% mutate(id = i)
  hf_points <- rbind(hf_points, newfac)
  
  #Extract travel times to new facility
  newtime <- time_mat[,index]
  current_tt <- pmin(current_tt, newtime, na.rm=TRUE)
  dem$basett <- current_tt
  
  #Update weights - if travel time <120, change to 0
  dem$weight_pop[dem$basett <= 120] <- 0
  
  #Extract rows that are still above the travel time req
  active_indices <- which(dem$basett > 120)
  #Update candidates and time matrix
  can <- demcan[active_indices,]
  time_mat <- cost_matrix[,active_indices]
  
  #Status update and add to counter
  print(paste("Max: ", round(max(dem$basett, na.rm=TRUE), 2), 
              " Mean: ", round(mean(dem$basett, na.rm=TRUE), 2), sep=""))
}

ggplot() + geom_sf(data=hzone, inherit.aes=FALSE) +
  geom_sf(data=demcan, color="yellow", size=0.5) + 
  geom_sf(data=hf_points, color="blue")

#Export - need to append the surrounding HF to the ones that were newly placed
hf_outside <- hf_outside %>% dplyr::select(geometry) %>% mutate(id=seq(373,495,by=1))
hf_points <- rbind(hf_points, hf_outside)
st_write(hf_points, paste("Data/R output/sysrealign_tt.shp", sep=""), append=FALSE)

#########################################################
##### SYSTEM REALIGNMENT BY EUCLIDEAN
#This scenario will replace the existing
#health facilities. Service radius is given by euclidean distance.

#Get existing health facilities and give ID number
hf_points <- data.frame(geometry=I(list()), id=numeric())
#Make copy of dem/can points for use in loop
dem <- demcan 
can <- demcan
#Other misc params
nhf <- nrow(hf)-nrow(hf_outside)

#Adding in the existence of health facilities outside of Equateur
#Create list to append to
affected_indices <- c()
#Create 10km buffer around all outside HF
search_buffer <- st_buffer(hf_outside, dist=10000)
#For each outside HF, search for demand points within them
for (i in 1:nrow(search_buffer)) {
  b <- which(st_intersects(dem, search_buffer[i,], sparse = FALSE))
  affected_indices <- c(affected_indices, b)}
#For those demand points identified, change their weight to 0
dem <- as.data.frame(dem)
dem$weight_pop[affected_indices] <- 0
dem <- st_set_geometry(dem, "geometry")
rm(b)

for (i in 1:nhf) {
  
  print(i)
  
  ###Step 1: Place 1 facility.
  mclp_results <- mclp(demand = dem, facilities = can, n_facilities = 1, service_radius = 10000,
                       weight_col = "weight_pop")
  mclp_results <- mclp_results$facilities[mclp_results$facilities$.selected,]
  mclp_results <- mclp_results %>% dplyr::select(geometry) %>% mutate(id = i)
  print("Facility placed.")
  
  ###Step 2: Adjust weights
  #2a Bind new facility to previous.
  hf_points <- rbind(hf_points, mclp_results)
  
  #2b Identify all demand points within 10km - if within buffer, weight changes to 0
  search_buffer <- st_buffer(mclp_results, dist=10000)
  affected_indices <- which(st_intersects(dem, search_buffer, sparse = FALSE))
  
  dem <- as.data.frame(dem)
  dem$weight_pop[affected_indices] <- 0
  dem <- st_set_geometry(dem, "geometry")
  
  #2c Update candidates
  affected_indices <- which(dem$weight_pop == 0)
  can <- demcan[-affected_indices,]
}

ggplot() + #geom_sf(data=hzone, inherit.aes=FALSE) +
  geom_sf(data=demcan, color="yellow", size=0.5) + 
  geom_sf(data=hf_points, color="blue") 

#Export - need to append the surrounding HF to the ones that were newly placed
hf_outside <- hf_outside %>% dplyr::select(geometry) %>% mutate(id=seq(373,495,by=1))
hf_points <- rbind(hf_points, hf_outside)
st_write(hf_points, paste("Data/R output/sysrealign_km.shp", sep=""), append=FALSE)
