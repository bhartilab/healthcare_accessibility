#########################################################
##### ALIGNMENT APP #####

#This Shiny app is meant to help visualize how the facilities
#differ by location and order placed between the
#two System Realignment scenarios -- service radius
#by Travel Time and by Euclidean Distance.

#Last updated 8/26/26 with annotations added 10/1/26

#########################################################

library(dplyr)
library(sf)
library(terra)
library(ggplot2)
library(shiny)

setwd("/Volumes/LaCie/")

#########################################################
##### Load in necessary data #####

#Load in health zones
hzone <- st_read("Data/Grid3 Health Zones/GRID3_COD_health_zones_v9_0.shp")
hzone <- hzone %>% filter(province=="Equateur")
hzone <- st_transform(hzone, "EPSG:3201")
targ1km <- rast(ext(hzone), res=1000, crs="EPSG:3201")

#Create a raster version of the health zones
hz_ras <- vect(hzone)
hz_ras <- rasterize(hz_ras, targ1km, field="zonesante")

#Load in population estimates and crop to extent (1km)
p24 <- rast("Data/LandScan Population Density/LandScan24.tif")
p24 <- project(p24, targ1km, method="near")

#Create dataframe where each row is a populated point - 
#values represent # people at each point and the residing health zone
allstack <- c(hz_ras, p24)
names(allstack) <- c("health zone", "pop")
allstack <- as.data.frame(allstack, xy=TRUE, na.rm=TRUE)

demcan <- allstack %>% filter(pop>0)
demcan <- st_as_sf(demcan, coords=c("x","y"), crs="EPSG:3201")

rm(allstack)

#Read in the health facilities
#!!!NOTE -- if the shape file also includes the existing
#health facilities that are outside of Equateur, those 
#need to be removed. You only need to plot the facilities
#that were replaced.
hf_tt  <- st_read("Data/R output/sysrealign_tt.shp")
hf_km  <- st_read("Data/R output/sysrealign_km.shp")

hftot <- nrow(hf_tt)

#########################################################

ui <- fluidPage(
  
  fluidRow(column(12, sliderInput("facility_slider", "ID of Facilities to Plot:", 
                                  min = 1, max = hftot, value = c(1, hftot)))),
  
  hr(),
  
  fluidRow(column(6, plotOutput("mapPlot")),
           column(6, plotOutput("mapPlot2")))
)

server <- function(input, output, session) {
  output$mapPlot <- renderPlot({
    
    selected_facilities <- hf_tt[input$facility_slider[1]:input$facility_slider[2], ]
    
    ggplot() +
      geom_sf(data = hzone) +
      geom_sf(data = demcan, color = "black", size = 0.5) +
      geom_sf(data = selected_facilities, color = "red") +
      ggtitle('Realignment by Travel Time')
    })
  
  output$mapPlot2 <- renderPlot({
    
    selected_facilities2 <- hf_km[input$facility_slider[1]:input$facility_slider[2], ]
    
    ggplot() +
      geom_sf(data = hzone) +
      geom_sf(data = demcan, color = "black", size = 0.5) +
      geom_sf(data = selected_facilities2, color = "red") +
      ggtitle('Realignment by Distance')
  })
}

shinyApp(ui, server)
