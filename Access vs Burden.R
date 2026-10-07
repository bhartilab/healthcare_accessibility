#########################################################
##### Measles x Health Care Accessibility ##### 

# This code is meant to help explore the association
# between measles burden and travel time to health
# care facilities. Mostly filled with calculating
# descriptive stats and generating figures. 

# Last updated 9/21/26, with edits and annotations
# added on 10/5/26.

#########################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(terra)
library(DT)
library(gganimate)
library(exactextractr)
library(patchwork)

setwd("/Volumes/LaCie/")

#########################################################

#Load in the data here! 
load("/Volumes/LaCie/access_v_burden.RData")

#########################################################

#Define a list of the health zones in Equateur and in the
#surrounding area that will be included in these analyses

hz_other <- c("Bangabola","Basankusu","Befale","Bikoro","Boende","Bolenge","Bolomba",        
              "Bomongo","Bongandanga","Boso-Mondanda","Budjala","Bokonzi",
              "Djombo","Iboko","Ingende","Inongo","Kungu",        
              "Irebu","Kiri","Lilanga-Bobangi","Lolanga-Mampoko","Lotumbe",       
              "Lukolela","Makanza","Mbandaka","Monieka","Monkoto",
              "Ntandembelo","Ntondo","Pimu", "Wangata","Yumbi") 

#########################################################
##### Loading in data #####

##Measles case data -- you need to run the cleaning script first, 
#then you can filter down to the health zones needed starting here: 
cases <- case_w_date %>% filter(zs_clean %in% hz_other)

##Load in baseline travel time to health facilities
base <- rast("Data/Travel Time Catchments/tt_baseline_v6.tif")

#Make a template for all incoming data
temp <- rast(ext(base), res=100, crs="EPSG:3201")

#Resample baseline travel time to correct res
base <- resample(base, temp, method = "bilinear")

##Load in population data
p24 <- rast("Data/Grid3 Population Estimates/grid3_pop_eqonly.tif")
p24 <- project(p24, temp)
p24 <- trim(p24)
base <- crop(base, ext(p24))

##Load in health zone data -- this needs a bit of cleaning, particularly
#because the names of health zones from Grid3 and in the measles
#burden data don't align very well. Most (maybe all) of this code is a repeat of what
#is in the Case Plots by Province code.
hzone <- st_read("Data/Grid3 Health Zones/GRID3_COD_health_zones_v9_0.shp")
hzone <- hzone %>% dplyr::select(zonesante, geometry)
hzone$zonesante <- gsub(" ", "-", hzone$zonesante)

#Fuzzy matching wasn't doing the job well enough for me... so I ended up just hand coding these.
hzone$zonesante[hzone$zonesante == "Banzow-Moke"] <- "Banjow-Moke"
hzone$zonesante[hzone$zonesante == "Massa"] <- "Masa"
hzone$zonesante[hzone$zonesante == "Moanza"] <- "Moanda"
hzone$zonesante[hzone$zonesante == "Mampoko"] <- "Lolanga-Mampoko"
hzone$zonesante[hzone$zonesante == "Bogosenubea"] <- "Bogosenubia"
hzone$zonesante[hzone$zonesante == "Kabeya-Kamwanga"] <- "Kabeya-Kamuanga"
hzone$zonesante[hzone$zonesante == "Tshilenge"] <- "Tshitenge"
hzone$zonesante[hzone$zonesante == "Mweneditu"] <- "Mwene-Ditu"
hzone$zonesante[hzone$zonesante == "Djalo-Ndjeka"] <- "Djalo-Djeka"
hzone$zonesante[hzone$zonesante == "Kiambi"] <- "Kiyambi"
hzone$zonesante[hzone$zonesante == "Kalamu-1"] <- "Kalamu-I"
hzone$zonesante[hzone$zonesante == "Kalamu-2"] <- "Kalamu-Ii"
hzone$zonesante[hzone$zonesante == "Kalamu-2"] <- "Kalamu-Ii"
hzone$zonesante[hzone$zonesante == "Maluku-1"] <- "Maluku-I"
hzone$zonesante[hzone$zonesante == "Maluku-2"] <- "Maluku-Ii"
hzone$zonesante[hzone$zonesante == "Masina-1"] <- "Masina-I"
hzone$zonesante[hzone$zonesante == "Masina-2"] <- "Masina-Ii"
hzone$zonesante[hzone$zonesante == "Mont-Ngafula-1"] <- "Mont-Ngafula-I"
hzone$zonesante[hzone$zonesante == "Mont-Ngafula-2"] <- "Mont-Ngafula-Ii"
hzone$zonesante[hzone$zonesante == "Gety"] <- "Gethy"
hzone$zonesante[hzone$zonesante == "Mongbwalu"] <- "Mongbalu"
hzone$zonesante[hzone$zonesante == "Nyankunde"] <- "Nyakunde"
hzone$zonesante[hzone$zonesante == "Haut-Plateau"] <- "Hauts-Plateaux"
hzone$zonesante[hzone$zonesante == "Pendjwa"] <- "Penjwa"
hzone$zonesante[hzone$zonesante == "Bena-Tshadi"] <- "Bena-Tshiadi"
hzone$zonesante[hzone$zonesante == "Malemba-Nkulu"] <- "Malemba"
hzone$zonesante[hzone$zonesante == "Busanga"] <- "Bosanga"
hzone$zonesante[hzone$zonesante == "Wamba-Luadi"] <- "Wamba-Lwadi"
hzone$zonesante[hzone$zonesante == "Yalifafu"] <- "Yalifafo"
hzone$zonesante[hzone$zonesante == "Ruashi"] <- "Rwashi"
hzone$zonesante[hzone$zonesante == "Kimbao"] <- "Kimbau"

hzone <- hzone %>% filter(zonesante %in% hz_other) %>% dplyr::select(zonesante, geometry)
hzone <- st_transform(hzone, "EPSG:3201")

#Create a rasterized version of the health zones
hz_ras <- vect(hzone)
hz_ras <- rasterize(hz_ras, temp, field="zonesante")
hz_ras <- crop(hz_ras, ext(p24))

##Load in health facilities
hf <- st_read("Data/Grid3 Health Facilities/HF_v9_edited.shp")
#Just a couple of corrections to the Grid3 data that need to be made:
hf$zonesante[hf$zonesante == "Lilanga Bobangi"] <- "Lilanga-Bobangi"
hf$zonesante[hf$zonesante == "Mampoko"] <- "Lolanga-Mampoko"
#Filter down to the facilities we need:
hf <- hf %>% filter(province == "Equateur" | zonesante %in% hz_other)

##Load in the health facility polygons -- these polygons dictate the bounds
#of pull into the respective facility, i.e., it is the closest facility 
#for those residing in the shape
hf_poly <- st_read("Data/Grid3 Health Facilities/ttcatch_v6_polygons.shp")
hf_polyras <- vect(hf_poly)
hf_polyras <- rasterize(hf_polyras, temp, field="Id")
hf_polyras <- crop(hf_polyras, ext(p24))

##Vaccine data -- also need to run the cleaning script to get the vax
#data sorted, then can filter to what we need from here: 
vax <- vaxdata %>% filter(zs %in% hz_other) %>% dplyr::select(year:cv_admin) %>% drop_na()

#########################################################
##### Create data frames putting all of the 
##### accessibility data together

#First, at the health facility level. Create a dataframe
#where each each row represents a populated cell. Columns 
#will represent the # people residing, their baseline travel time,
#what health zone they reside in, and what health facility they 
#are closest to.
access <- c(p24, base, hz_ras, hf_polyras)
names(access) <- c("pop", "tt", "reszone", "healthfacility")
access <- as.data.frame(access, xy=TRUE, na.rm=TRUE)

#We have a column for the health zone people reside in, but this may be
#different from the health zone that they are most likely to seek health
#care from. Code below creates a var for this. 
a <- hf_poly %>% st_join(., hzone, largest=TRUE) %>% mutate(Id=as.factor(Id))
access <- access %>% mutate(healthfacility=as.factor(healthfacility)) 
access <- left_join(access, dplyr::select(a, "Id", "zonesante"), by=c("healthfacility"="Id")) %>% 
          st_drop_geometry() %>% dplyr::select(-geometry) %>%
          rename(hfzone=zonesante)
rm(a)

#Now, calculate some descriptive stats for each health facility,
#such as proportion of people who can reach the facility w/in 2h,
#total population that each facility serves, and min/mean/max of travel 
#to each facility. 
access_hf <- access %>% mutate(twohr_base = if_else(tt <= 120, "yes", "no"))
access_hf <- access_hf %>% group_by(healthfacility) %>% summarize(
  total_pop=sum(pop), 
  meantt=weighted.mean(tt, pop), 
  mintt=min(tt),
  maxtt=max(tt),
  pcttwohr=sum(pop[twohr_base == "yes"]) / sum(pop) * 100)
access_hf <- hf_poly %>% dplyr::select(Id, geometry) %>% mutate(Id=as.factor(Id)) %>%
             left_join(., access_hf, by=c("Id"="healthfacility")) %>%
             filter(!is.na(total_pop))
access_hf <- st_set_geometry(access_hf, "geometry")

#Now, calculate the same descriptive stats but at the health zone level:
access_hz <- access %>% mutate(twohr_base = if_else(tt <= 120, "yes", "no"))
access_hz <- access_hz %>% group_by(hfzone) %>% summarize(
  total_pop=sum(pop), 
  meantt=weighted.mean(tt, pop), 
  mediantt = median(tt),
  mintt=min(tt),
  maxtt=max(tt),
  pcttwohr=sum(pop[twohr_base == "yes"]) / sum(pop) * 100)

#Also, calculate the # facilities per health zone, then merge to the 
#dataframe at the healthzone level
hf_hz <- hf %>% group_by(zonesante) %>% summarize(totalhf = n()) %>% st_drop_geometry()
access_hz <- left_join(access_hz, hf_hz, by=c("hfzone"="zonesante"))

#Then, convert to # facilities per 100k population. 
#***Note: for the provinces surrounding Equateur, the numbers are going to be 
#very inflated since the number only reflects both the Equateur population
#and the facilities that Equateur residents are likely to attend.
access_hz <- access_hz %>% mutate(totalhf_pop = (totalhf/total_pop)*100000)
access_hz <- access_hz[-6,]
rm(hf_hz)

#########################################################
##### Create data frames for metrics of measles burden

#Summarize burden at the health zone level,
#including total cases overall, total population per health zone,
#mean incidence per year, and total incidence from 2001-24.
#***Note: I've only used the 2024 population data here. Consider
#getting the pop data from other years for more accurate
#calculations of incidence.
cases_hz <- cases %>% group_by(zs_clean) %>% 
            summarize(total_cases=sum(cases),
            total_pop=mean(pop_2024),
            meaninc=(total_cases/total_pop)*(100000/24),
            totinc=(total_cases/total_pop)*100000)

#The code below is to identify the single week of every year with the
#most reported cases (per health zone). Also identifies whether that week
#is during rainy/dry season, and what month. 
peak_weeks <- cases %>% group_by(year, zs_clean) %>% filter(cases == max(cases, na.rm = TRUE)) %>%
  filter(row_number() == 1) %>% ungroup() %>% filter(cases>0) %>% arrange(zs_clean, year) %>%
  mutate(season= if_else(week<9 | week>20 & week<36, "dry", "rainy"), month=month(date, label=TRUE))

#The code below is meant to identify whether a week is part of an "outbreak" 
#(really just high measles activity - 2+ consecutive weeks of reported cases). 
#The logic of it is far from perfect, and still needs tinkering. 
outbreaks <- cases %>% group_by(zs_clean) %>% arrange(date, .by_group=TRUE) %>% 
  mutate(diff=cases-lag(cases, default = first(cases))) %>%
  mutate(outbreakwk = case_when(
    cases == 0 & lag(cases) == 0 | lead(cases) == 0 ~ "no",     
    cases > 0 & lag(cases) > 0 | lead(cases) > 0 ~ "yes",
    cases == 0 & lag(cases) > 0 & lead(cases) > 0 ~ "yes", 
    cases > 0 & lag(diff) != 0 & lead(cases) == 0 ~ "yes",
    cases > 0 & lag(diff) == 0 & lead(cases) == 0 ~ "no"))

#########################################################
##### Visualizations #####

#Plot a map of total incident cases per health zone, and where the health facilities are
a <- left_join(cases_hz, hzone, by=c("zs_clean"="zonesante")) %>% st_set_geometry(., "geometry")
ggplot() + 
  #plot health zones, fill color is equal to total incidence
  geom_sf(data=b, aes(fill=totinc), color="black") + 
  #plot health facility locations
  geom_sf(data=a, aes(color=study), size=0.7) + 
  theme(panel.grid = element_blank(), axis.title = element_blank(), 
     axis.text = element_blank(), axis.ticks = element_blank(), 
     axis.line = element_blank()) + labs(fill="Total Incident Cases\n (2001-2024)\n per 100,000")
rm(a)

#Now make a few plots of the measles tie series, per health zone. Need to first
#make sure we can arrange all the health zones in rough geography:
cases <- cases %>% arrange(desc(lat), lon) %>% mutate(zs_clean = factor(zs_clean, levels = unique(zs_clean))) 

#First plot: a graph of the entire time series
ggplot() + 
  #plot time by cases per 100k pop
  geom_line(data=cases, aes(x=date, y=(cases/pop_2024)*100000)) +
  #add vertical lines indicating each 1 Jan
  geom_vline(xintercept = as.numeric(seq(as.Date("2001-01-01"), as.Date("2024-01-01"), by = "1 year")), 
             color = "gray70", linewidth = 0.1) +
  #fix x-axis breaks
  scale_x_date(breaks = as.Date(c("2001-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", "2024-01-01")),
               date_labels = "%Y", limits = c(as.Date("2001-01-01"), as.Date("2024-12-31")), expand = c(0, 0)) +
  #plot a health zone on each panel
  facet_wrap(~zs_clean, scales="fixed", ncol=4) + 
  #aesthetics
  ylab("Cases per 100,000 Population") +
  theme(axis.text.x = element_text(angle=90), legend.position="none", axis.title.x = element_blank(),
        panel.grid.minor.y=element_blank(), panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())

#Second plot, to assess seasonality: a line graph where every line is one year
ggplot() +
  #plot each year of data as a separate line (lighter years are more recent)
  geom_line(data=cases, aes(x=week, y=(cases/pop_2024)*100000, group=year, color=year)) +
  #add shaded areas to indicate rainy and dry season
  annotate("rect", xmin=9, xmax=20, ymin=0, ymax=Inf, fill="darkgrey", alpha=0.2) +
  annotate("rect", xmin=36, xmax=52, ymin=0, ymax=Inf, fill="darkgrey", alpha=0.2) +
  #one panel per health zone
  facet_wrap(~zs_clean, scales="fixed", ncol=4) +
  #aesthetics
  ylab("Cases per 100,000 Population") +
  xlab("Week") +
  theme(panel.grid.minor.y=element_blank(), panel.grid.minor.x=element_blank(), 
        panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank())

#Now plot the time series for health zones that had vaccine campaigns
a <- cases %>% filter(zs_clean %in% vax$zs, year>2019) %>% dplyr::select(-province, -dps, -zs) %>% arrange(desc(lat), lon) %>% rename(zs=zs_clean)

ggplot() + 
  #plot a shaded area dictating when campaigns happened
  geom_rect(data=vax, aes(xmin=`start date`, xmax=`end date`, ymin=0, ymax=Inf), color="red", fill="red", alpha=0.5) +
  #plot time versus cases
  geom_line(data=a, aes(x=date, y=(cases/pop_2024)*100000)) +
  #plot vert lines for every 1 Jan
  geom_vline(xintercept = as.numeric(seq(as.Date("2020-01-01"), as.Date("2024-01-01"), by = "1 year")), 
             color = "gray70", linewidth = 0.1) +
  #fix x-axis breaks
  scale_x_date(breaks = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01")),
               date_labels = "%Y", limits = c(as.Date("2020-01-01"), as.Date("2024-12-31")), expand = c(0, 0)) +
  #one panel per health zone
  facet_wrap(~zs, scales="fixed", ncol=3) + 
  #aesthetics
  ylab("Cases per 100,000 Population") +
  theme(axis.text.x = element_text(angle=90), legend.position="none", axis.title.x = element_blank(),
        panel.grid.minor.y=element_blank(), panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())

#Create a gif of the map of Equateur (and surrounding) to help visualize
#the # of incident cases every year per health zone.
#***Note: The code listed here is not perfect and some adjustments to the
#aesthetics of the gif are needed. I also plot the raw total cases rather than total cases 
#per 100,000 population. 
# p <- ggplot() + geom_sf(data=a, aes(fill=total_cases)) + scale_fill_viridis_c(option="magma") +
#   theme(panel.grid = element_blank(), axis.title = element_blank(), 
#   axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank()) +
#   labs(title = 'Year: {frame_time}') + transition_time(year)
# animate(p, fps=10, duration=30)
# anim_save("measlescasesmap.gif")

#Make a violin plot to visualize the accessibility of each health facility
access$hfzone <- factor(access$hfzone, levels=levels(cases$zs_clean))
ggplot() + 
  #plot the distribution of travel time to each health facility
  geom_violin(data=access, aes(x=healthfacility, y=tt, weight=pop, fill=healthfacility)) +
  #each panel will represent the health facilities within a health zone
  facet_wrap(~hfzone, ncol=4, scales="free_x") +
  #aesthetics
  #coord_flip() + 
  theme(legend.position="none") + 
  labs(y="Travel time (minutes)") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank())

#Another violin plot, but this shows accessibility only at the health zone level
ggplot() + 
  geom_violin(data=access, aes(x=hfzone, y=tt, weight=pop, fill=hfzone)) + 
  coord_flip() + 
  theme(legend.position="none") + 
  labs(y="Travel time (minutes)")

#Map the accesibility metrics we calculated earlier, at the health zone level
ggplot() + geom_sf(data=access_hf, aes(fill=meantt)) + scale_fill_viridis_c() + labs(fill="Mean Travel Time (min)") + 
  theme(panel.grid = element_blank(), axis.ticks=element_blank(), axis.text=element_blank())
ggplot() + geom_sf(data=access_hf, aes(fill=mintt)) + scale_fill_viridis_c() + labs(fill="Min Travel Time (min)") + 
  theme(panel.grid = element_blank(), axis.ticks=element_blank(), axis.text=element_blank())
ggplot() + geom_sf(data=access_hf, aes(fill=maxtt)) + scale_fill_viridis_c() + labs(fill="Max Travel Time (min)") + 
  theme(panel.grid = element_blank(), axis.ticks=element_blank(), axis.text=element_blank())
ggplot() + geom_sf(data=access_hf, aes(fill=pcttwohr))+ scale_fill_viridis_c() + labs(fill="% population within 2 hours of HF") + 
  theme(panel.grid = element_blank(), axis.ticks=element_blank(), axis.text=element_blank())
ggplot() + geom_sf(data=access_hf, aes(fill=total_pop)) + scale_fill_viridis_c() + labs(fill="Total Population") + 
  theme(panel.grid = element_blank(), axis.ticks=element_blank(), axis.text=element_blank())

#########################################################
##### Below is code where I start taking a more direct
# look at how accessibility and burden may be associated:

#First, comparing incident cases vs mean travel time
a <- left_join(cases_hz, access_hz, by=c("zs_clean"="hfzone"))
a <- a[-11,]
#Make a scatter plot of total incident cases vs mean travel time for each health zone
ggplot() + 
  geom_point(data=a, aes(x=totinc, y=meantt, color=zs_clean))+
  labs(x="Total Incident Cases per 100,000", y="Mean Travel Time (min)") + 
  theme(legend.title=element_blank())
#Run a Spearman's correlation over it
cor.test(a$totinc, a$meantt, method="spearman")

#Now, compare the # of "outbreak weeks" to what the 10th/90th percentile
#of travel time is. I defined an "outbreak week" as any week with 5+ cases
#(5 was chosen more or less arbitrarily).
a <- cases %>% group_by(zs_clean) %>% summarize(total_weeks=sum(cases >= 5, na.rm=TRUE))
a1 <- access %>% group_by(hfzone) %>% summarize(pctl_10=quantile(tt, probs=c(0.1)), 
                                               pctl_90=quantile(tt, probs=c(0.9)))
a <- left_join(a, a1, by=c("zs_clean"="hfzone"))

#Scatterplot, then correlation test
ggplot() + 
  geom_point(data=a, aes(x=total_weeks, y=pctl_90, color=zs_clean)) +
  labs(x="Total # weeks w/ 5+ cases", y="90th Pctl Travel Time (min)") + 
  theme(legend.title=element_blank())
cor.test(a$total_weeks, a$pctl_90, method="spearman")

