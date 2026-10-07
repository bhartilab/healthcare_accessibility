#########################################################
##### Location Allocation Visuals #####

#This script calculates descriptive stats and 
#creates visualizations after MCLP models are run.

#Last updated: 10/1/26

#########################################################
##### Libraries and WD #####

library(dplyr)
library(tidyr)
library(ggplot2)
library(terra)
library(sf)
library(stringr)
library(patchwork)

setwd("/Volumes/LaCie")
set.seed(123)

#########################################################
##### Loading Data #####

#Load in baseline travel time
base <- rast("Data/Travel Time Catchments/tt_baseline_v6.tif")

#Create a template for other datasets that need cropping and resampling
targ <- rast(ext(base), res=100, crs="EPSG:3201")

#Resample baseline travel
base <- resample(base, targ, method = "bilinear")

#Load in health zone 
hzone <- st_read("Data/Grid3 Health Zones/GRID3_COD_health_zones_v9_0.shp")
#Filter down to Equateur
hzone <- hzone %>% filter(province=="Equateur")
hzone <- st_transform(hzone, "EPSG:3201")
#Create a raster version health zones
hz_ras <- vect(hzone)
hz_ras <- rasterize(hz_ras, targ, field="zonesante")

#Load in population estimates
pop <- rast("Data/Grid3 Population Estimates/grid3_pop_eqonly.tif")
pop <- project(pop, targ, method="near")

#########################################################
##### Loading in Model Results #####

#After the location models are run, the shape files for each set
#of new facilities is saved. Those are then run in ArcGIS to 
#calculate travel time with Distance Allocation tool.
#Then, brought back here for final figures.

#The object suffixes align with each scenario:
#tt -- system realignment, travel time service radius
#km -- system realignment, distance based service radius
#ul -- unlimited resources

#Load in HF shape files
hf_tt  <- st_read("Data/R output/sysrealign_tt.shp")
hf_km  <- st_read("Data/R output/sysrealign_km.shp")
hf_ul  <- st_read("Data/R output/unlimited_limited.shp")

#Load in travel times and resample
sys_km <- rast("Data/Travel Time Catchments/iter v5.1/ttpercatch_v5.1_sysrealign_km.tif")
sys_tt <- rast("Data/Travel Time Catchments/iter v5.1/ttpercatch_v5.1_sysrealign_tt.tif")
unltd  <- rast("Data/Travel Time Catchments/iter v5.1/ttpercatch_v5.1_unlimited_limited.tif")

sys_km <- resample(sys_km, targ, method="bilinear")
sys_tt <- resample(sys_tt, targ, method="bilinear")
unltd  <- resample(unltd, targ, method="bilinear")

#Create a dataframe where every row represents a populated point, and includes
#the following data: # people per point, residing health zone, baseline travel to HF, 
#and travel time to all new HF for each scenario
allstack <- c(hz_ras, pop, base, sys_km, sys_tt, unltd)
names(allstack) <- c("health zone", "pop", "baseline", "syskm", "systt", "unltd")
allstack <- as.data.frame(allstack, xy=TRUE, na.rm=TRUE)

#Create copies of allstack, in long format and in populated points
allstack_coord <- st_as_sf(allstack, coords=c("x","y"), crs="EPSG:3201")
allstack_long <- allstack %>% pivot_longer(cols="baseline":"unltd", names_to="scenario", values_to="traveltime")

#########################################################
##### VISUALIZATIONS & STATS

#Map baseline travel time
ggplot() +  
  #add health zones
  geom_sf(data=hzone, inherit.aes=FALSE) +
  #plot populated points, colored by baseline travel time
  geom_point(data=allstack, aes(x=x, y=y, color=baseline), size=0.5) +
  #aesthetics
  scale_color_viridis_c(option="magma") + 
  theme(panel.grid = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank()) +
  labs(color="Baseline Travel Time (min)")

#Calculate weighted percentiles of baseline travel time
ext <- rep(allstack$baseline, allstack$pop)
quantile(ext, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
rm(ext)

#Map health facilities from unlimited resources scenario
a <- ggplot() +  
  #add health zones
  geom_sf(data=hzone, inherit.aes=FALSE) +
  #add populated points
  geom_sf(data=allstack_coord, color="yellow", size=0.5) + 
  #add health facilities
  geom_sf(data=hf_ul, aes(color=status)) + 
  #aesthetics
  ggtitle(paste('Unlimited Resources (N=', nrow(hf_ul), ')', sep="")) +
  theme(panel.grid = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(),
        legend.title = element_blank())

#Map health facilities from system realignment (tt) scenario
b <- ggplot() +  
  #add health zones
  geom_sf(data=hzone, inherit.aes=FALSE) +
  #add populated points
  geom_sf(data=allstack_coord, color="yellow", size=0.5) + 
  #add health facilities
  geom_sf(data=hf_tt, color="#619CFF") + 
  #aesthetics
  ggtitle('Realignment: Travel Time Service Radius') +
  theme(panel.grid = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())

#Map health facilities from system realignment (km) scenario
c <- ggplot() +  
  #add health zones
  geom_sf(data=hzone, inherit.aes=FALSE) +
  #add populated points
  geom_sf(data=allstack_coord, color="yellow", size=0.5) + 
  #add health facilities
  geom_sf(data=hf_km, color="#619CFF") + 
  #aesthetics
  ggtitle('Realignment: Distance Service Radius') +
  theme(panel.grid = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())

#Plots for limited scenario -- map and plot of prop covered
temp <- hf_ul %>% filter(status=="candidate")
d <- ggplot() +  
  #add health zones
  geom_sf(data=hzone, fill=NA) +
  #add health facilities, color is filled by order placed
  geom_sf(data=temp, aes(color=id)) + 
  #aesthetics
  scale_color_viridis_c(option="magma") +
  ggtitle('Limited Resources') + theme(panel.grid = element_blank(), axis.title = element_blank(), 
  axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())

(a+d)
(b+c)
rm(a,b,c,d,temp)

#Plot violins to view distribution of travel times by scenario
ggplot() + 
  geom_violin(data=allstack_long, aes(x=scenario, y=traveltime, weight=pop, fill=scenario)) + 
  coord_flip() + 
  theme(legend.position="none") + 
  labs(y="Travel time (minutes)") + 
  scale_y_continuous(breaks=seq(0, 1500, by=150))

#Calculate maximum travel time per scenario
allstack_long %>% group_by(scenario) %>% summarize(`Max Travel Time (min)` = max(traveltime))

#Calculate the # and proportion of population that is within 2 hours of travel,
#and the difference from baseline
allstack_long %>% 
  filter(traveltime <= 120) %>% group_by(scenario) %>% 
  summarize(ppltot = sum(pop), proptot = 100*(sum(pop)/sum(allstack$pop))) %>% 
  mutate(ppldiff = ppltot - first(ppltot)) %>%
  mutate(propdiff = 100*(ppldiff/sum(allstack$pop)))

#Calculate and plot the change in travel time from existing -> rearranged
ttdiff <- allstack %>% mutate(dt_tt=baseline-syskm, dt_km=baseline-systt)
ttdiff <- ttdiff %>% dplyr::select(x, y, pop, dt_tt, dt_km) %>% 
                     pivot_longer(cols=4:5, names_to="scenario", values_to="dt")

#Categorize the change in travel time to worsened, same, and improved. 
#Currently categorizes based on +/- 10 minutes, but this can be adjusted.
ttdiff <- ttdiff %>% 
  mutate(indicator = case_when(dt < -10 ~ "Worsened", dt > -10 & dt < 10 ~ "Same", dt >= 10 ~ "Improved"))
ttdiff %>% group_by(scenario, indicator) %>% summarise(proportion=100*(sum(pop)/sum(allstack$pop)))

ggplot() + 
  #add health zones
  geom_sf(data=hzone, inherit.aes=FALSE) + 
  #plot populated points, color indicates difference in travel time
  geom_point(data=ttdiff, aes(x=x, y=y, color=indicator), size=0.5) + 
  facet_wrap(~scenario) + 
  #aesthetics
  theme(panel.grid = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())

