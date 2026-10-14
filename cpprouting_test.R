#########################################################
#####         code for testing:            #####        
#####  testing alternatives for computing  ##### 
#####  cost distance matrix. this tests    ##### 
#####       the package cppRouting.        ##### 

#Last updated 10/14/26

#########################################################
library(cppRouting)
library(dplyr)
library(terra)
library(sf)
library(gdistance)
library(spopt)

getwd()
set.seed(123)

#You can test over various sized areas -- the full grid has 74681 populated points
#I wasn't able to run the entire grid on Rogue. After a week, it was seemingly
#running but R was not rendering its UI. I would estimate the full grid to take around 8-10 
#days.
#big (21126 pop points, takes 8 hours on Rogue)
#ext <- c(900000,1100000,-100000,100000)
#med (4825 pop points, takes 1 hour on Rogue)
#ext <- c(800000, 900000, -40000, 0)
#small (54 pop points, takes 21 sec locally)
ext <- c(800000, 810000, -10000, 0)

##################################DATA
#load baseline travel time
base <- rast("Data/Travel Time Catchments/tt_baseline_v6.tif")
#crop to smaller extent if needed
base <- crop(base, ext)

#templates
temp <- rast(ext(base), res=100, crs="EPSG:3201")

#resample baseline
base <- resample(base, temp, method = "bilinear")

#load in friction surface
friction_r <- rast("Data/Travel Time Catchments/traveltime_v6.tif")
friction_r <- resample(friction_r, temp, method = "bilinear")

#population
grid3_100m <- rast("Data/Grid3 Population Estimates/grid3_pop_eqonly.tif")
grid3_100m <- project(grid3_100m, temp)
grid3_100m <- trim(grid3_100m)
base100m <- crop(base, ext(grid3_100m))

#make one dataframe of health zone, pop, and baseline travel
allstack <- c(grid3_100m, base100m)
names(allstack) <- c("pop", "basett")
allstack <- as.data.frame(allstack, xy=TRUE, na.rm=TRUE)

allstack <- st_as_sf(allstack, coords=c("x","y"), crs="EPSG:3201")
allstack$weight_pop <- log(allstack$pop + 1)

##################################COST MATRIX

#Convert the surface to raster objects
friction <- raster::raster(friction_r)
#Create transition matrix
base_conductance <- transition(friction, transitionFunction = function(x) 1 / mean(x), directions = 16)
#Geocorrect for diagonal movement
base_conductance <- geoCorrection(base_conductance, type = "c")
tm <- base_conductance@transitionMatrix

rm(friction, base_conductance)
gc() 

#Convert to triplet (from, to, cost) and get nonzero edges
tm_triplet <- as(tm, "dgTMatrix")
edges <- data.frame(from=tm_triplet@i + 1, to=tm_triplet@j + 1, conductance = tm_triplet@x)
edges$cost <- 1 / edges$conductance

rm(tm, tm_triplet)
gc() 

#Make graph
graph <- makegraph(edges[, c("from", "to", "cost")], directed = FALSE)
graph <- cpp_simplify(graph)

#Contract graph --
#This function is the one that takes forever. Over a week!! And I didn't even
#get it to successfully finish. I would try running as a background job first.
#Or you could try chunking the contraction so you can have some checkpoints and relief
#points for computer's memory. 

t1 <- Sys.time()
message("Starting cpp_contract(): ", t1)

contracted_graph <- cpp_contract(graph, silent = FALSE)

t2 <- Sys.time() 
message("cpp_contract() completed: ", t2)
message("Elapsed time: ", difftime(t2, t1, units = "hours"))

#saveRDS(contracted_graph, "Data/R output/cpptest_sample_contractedgraph.RDS")
#message("Contracted graph checkpoint saved.")

rm(graph)
gc()

#Get coords of all demand points & extract cell ID
point_coords <- st_coordinates(allstack)
point_cell_ids <- cellFromXY(friction_r, point_coords)

#Calculate cost matrix
cost_matrix <- get_distance_matrix(contracted_graph, from = point_cell_ids, to = point_cell_ids, algorithm = "mch")

#saveRDS(cost_matrix, "Data/R output/cpptest_sample_costmatrix.RDS")

##################################MCLP

dem <- allstack %>% mutate(traveltime = Inf)
can <- allstack
time_mat <- cost_matrix_cpp
hf_points_cpp <- data.frame(geometry=I(list()), id=numeric())
current_tt <- dem$traveltime

for (i in 1:15) {
  
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
  hf_points_cpp <- rbind(hf_points_cpp, newfac)
  
  #Extract travel times to new facility
  newtime <- time_mat[,index]
  current_tt <- pmin(current_tt, newtime, na.rm=TRUE)
  dem$traveltime <- current_tt
  
  #Update weights - if travel time <120, change to 0
  dem$weight_pop[dem$traveltime <= 120] <- 0
  
  #Extract rows that are still above the travel time req
  active_indices <- which(dem$traveltime > 120)
  #Update candidates and time matrix
  can <- allstack[active_indices,]
  time_mat <- cost_matrix_cpp[,active_indices]
  
  #Status update and add to counter
  print(paste("Max: ", round(max(dem$traveltime, na.rm=TRUE), 2), 
              " Mean: ", round(mean(dem$traveltime, na.rm=TRUE), 2), sep=""))
}
ggplot() + geom_sf(data=allstack) + geom_sf(data=hf_points, color="red")
hfp100m <- hf_points %>% mutate(candidates="100m")

##################################VISUALS AND STATS

cand_1km  <- rast("Data/Travel Time Catchments/test - cand res/ttpercatch_test_1km.tif")
cand_100m  <- rast("Data/Travel Time Catchments/test - cand res/ttpercatch_test_100m.tif")
cand_cent <- rast("Data/Travel Time Catchments/test - cand res/ttpercatch_test_cent.tif")

cand_1km <- resample(cand_1km, temp, method="bilinear")
cand_100m <- resample(cand_100m, temp, method="bilinear")
cand_cent <- resample(cand_cent, temp, method="bilinear")

allstack <- c(grid3_100m, cand_1km, cand_100m, cand_cent)
names(allstack) <- c("pop", "cand1km", "cand100m", "candcent")
allstack <- as.data.frame(allstack, xy=TRUE, na.rm=TRUE)
allstack_piv <- allstack %>% pivot_longer(cols=4:6, names_to="scenario", values_to="travel time")
allstack_piv$scenario <- factor(allstack_piv$scenario, levels=c("cand100m", "cand1km", "candcent"))

#violin plot
ggplot() + geom_violin(data=allstack_piv, aes(x=scenario, y=`travel time`, weight=pop, fill=scenario)) + 
  coord_flip() + theme(legend.position="none") + labs(y="Travel time (minutes)") + scale_y_continuous(breaks=seq(0, 1500, by=150))

#max travel per scenario
allstack_piv %>% group_by(scenario) %>% summarize(mintt = min(`travel time`), 
                                                  maxtt = max(`travel time`), meantt = mean(`travel time`))

#calculate percentiles
apply(allstack[4:6], 2, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)

#total cells within 2h
colSums(allstack[, 5:7] < 120, na.rm = TRUE)

#total pop covered within 2h
allstack_piv %>% filter(`travel time` <= 120) %>% group_by(scenario) %>% summarize(ppltot = sum(pop), 
                                                                                   proptot = 100*(sum(pop)/sum(allstack$pop)))



