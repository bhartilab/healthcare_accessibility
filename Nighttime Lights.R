#########################################################
##### VIIRS Nighttime Lights ##### 

## This script was meant for exploratory analysis 
## of the nighttime lights data and seeing how it
## compares with population density. Wasn't used 
## for anything long term, though, so I'm also not 100% 
## sure this code is functional from start to finish.

# Code was initially written around Feb 2026 with
# edits for cleaning + annotations added on 10/1/26

#########################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(terra)
library(sf)
library(exactextractr)

setwd("/Volumes/LaCie")

#########################################################
##### Loading and Cleaning Data #####

#Load in health zones
hzone <- st_read("Data/Grid3 Health Zones/GRID3_COD_health_zones_v9_0.shp")
hzone <- hzone %>% filter(province=="Equateur")
hzone <- st_transform(hzone, "EPSG:3201")

#Set a template so all the data is uniform
targ <- rast(ext(hzone), res=1000, crs="EPSG:3201")

#Load in nighttime lights data
#First need to separate by season, then take mean for each season from 2021-2025.
#Then, extract means by health zone.
seasons <- list("dry1" = c("01Jan", "02Feb"), 
                "rainy1" = c("03Mar", "04Apr", "05May"), 
                "dry2" = c("06Jun", "07Jul"),
                "rainy2" = c("08Aug", "09Sep", "10Oct", "11Nov", "12Dec"))
years <- 2021:2025
hzmean <- hzone %>% dplyr::select(zonesante)

for (y in years) {
  for (m in names(seasons)) {
    szn <- seasons[[m]]
    
    fnames <- paste("Data/VIIRS Nighttime Lights/", y, "/", szn, substr(y,3,4), ".tif", sep="")
    rast_stack <- rast(fnames)
    rast_mean <- mean(rast_stack, na.rm = TRUE)
    rast_mean <- resample(rast_mean, targ, method="bilinear")
    
    zonemean <- exact_extract(rast_mean, hzone, fun="mean")
    hzmean <- hzmean %>% mutate(!!m := zonemean, year := y)
  }}

hzmean <- pivot_longer(hzmean, cols=names(seasons), names_to="season", values_to="radiance")
ord <- c("dry1", "rainy1", "dry2", "rainy2")
hzmean$season <- factor(hzmean$season, levels=ord)

#Load in population data
poprast <- rast("Data/LandScan Population Density/LandScan24.tif")
poprast <- resample(poprast, targ, method="bilinear")
popmean <- exact_extract(poprast, hzone, fun="mean")
popmean <- data.frame(hzone$zonesante, popmean)

#########################################################
##### Plots #####

#Radiance values over time
ggplot(hzmean, aes(x=season, y=radiance, group=zonesante, color=zonesante, text=paste(zonesante))) + 
  geom_line()

#Population density per health zone
ggplot(popmean, aes(x=hzone.zonesante, y=mean.LandScan_1)) + geom_col()

#########################################################

#I believe the code below is for comparing 
#population and nighttime lights data
#from a specific year

#Population data, 2023
p23 <- rast("Data/LandScan Population Density/LandScan23.tif")
p23 <- project(p23, targ, method="near")

#Nighttime lights data, 2023
months <- c("01Jan", "02Feb", "03Mar", "04Apr", "05May", "06Jun", "07Jul",
            "08Aug", "09Sep", "10Oct", "11Nov", "12Dec")
ntl <- paste("./Data/VIIRS Nighttime Lights/2023/", months, "23.tif", sep="")

ntl <- rast(ntl)
ntl <- clamp(ntl, upper=0.5)
names(ntl) <- months
ntl <- project(ntl, targ, method="bilinear")
ntl_df <- as.data.frame(ntl, xy=TRUE)
ntl_df_long <- ntl_df %>% pivot_longer(cols=months, names_to="month", values_to="value")

#Plot the lights
ggplot(ntl_df_long, aes(x=x, y=y, fill=value, color=value)) + facet_wrap(~month) + geom_raster() + scale_fill_viridis_c(option="magma")

#Plot the population with values filled by lights
pop_light <- c(p23, ntl)
pop_light <- as.data.frame(pop_light, xy=TRUE, na.rm=TRUE)
pop_light_long <- pop_light %>% pivot_longer(cols=months, names_to="month", values_to="value")

p <- pop_light_long %>% filter(month=="12Dec")
ggplot(p, aes(x=LandScan23, y=value)) + facet_wrap(~month) + geom_point()

