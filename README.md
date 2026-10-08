# healthcare_accessibility

This code is associated with analyses that aim to evaluate the accessibility of health care facilities in the Equateur province of DRC and simulate potential improvements to the health facility system.

### Publicly Available Data
---
The data referenced throughout this code is publicly available for download. 

-Health Facilities: [Grid3](https://data.grid3.org/datasets/GRID3::grid3-cod-health-facilities-v9-0/about)

-Health Zones: [Grid3](https://data.grid3.org/datasets/GRID3::grid3-cod-health-zones-v9-0/about)

-Population Estimates: [Grid3](https://data.grid3.org/maps/a3db539c0fae4c05aed92ed67e11fe2b/explore) [LandScan](https://landscan.ornl.gov/)

### Data Folder
---
The data folder in this repo stores .RDS objects that are referenced throughout the code:

-friction.RDS: friction surface, created in ArcGIS using the following publicly available data sources - [Copernicus Elevation](https://portal.opentopography.org/raster?opentopoID=OTSDEM.032021.4326.3) [HOSM Roads](https://data.humdata.org/dataset/hotosm_cod_roads) [OSM Waterways](https://data.humdata.org/dataset/hotosm_cod_waterways)

-baselinett.RDS: baseline travel time to health facilities in Equateur, calculated in ArcGIS

-baselinett_out.RDS: baseline travel time to health facilities outside of Equateur, calculated in ArcGIS


### Access vs Burden.R 
---
Explores the association between baseline travel time to health facilities and measles burden – mostly calculating descriptive stats and making plots

### Alignment App.R
---
This is a Shiny app used to view the two system realignment scenarios (service radius differing by travel time and distance) side by side. The app has a slider so you can see the order in which facilities are placed.

### Case Plots by Province.R 
---
Code for the Shiny app used to plot the time series of measles burden data, by province. You can choose whether each panel represents a province (and each line on the plot is a health zone), or each panel represents a health zone’s individual time series within a province.

### Centroid test.R
---
Code used to test using population centroids generated with k-means as candidate points for location allocation models.

### Cleaning.R
---
Code used to clean the MSF vaccination data and Ministry of Health measles burden data.

### Cpprouting_test.R
---
Code used to test the calculation of the cost matrix (an input for the location allocation models) using the cppRouting package in R. 

### DHS data capacity.R
---
Code used to clean DHS data and create an index of the capacity of the health facilities surveyed

### DHS data utilization.R
---
Code used to clean DHS data and create an index of health care utilization among households/women surveyed

### Loc Alloc Iter.R
---
This script contains the location allocation models, where facilities are placed iteratively. This is the current version of models used for this project.

### Location Allocation.R
---
This script contains the location allocation models, where facilities are placed simultaneously. 

### MCLP Visuals.R
---
Code needed to analyze the results of the location allocation models (calculate stats and make visualizations). 
