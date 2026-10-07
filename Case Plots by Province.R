#########################################################
##### Measles Burden Plots  #####

#Builds Shiny app to view the time series of measles
#burden, by province or by health zone

#Last updated 10/1/26

#########################################################

library(dplyr)
library(tidyr)
library(sf)
library(terra)
library(ggplot2)
library(readxl)
library(stringr)
library(stringi)
library(stringdist)
library(lubridate)
library(shiny)
library(exactextractr)

setwd("/Volumes/LaCie/")

#########################################################
##### importing case data and cleaning #####

#Load in the workspace here
load("caseplots.RData")

### OR,
### Run the MoH Measles cleaning script FIRST, then run from here:

#Filter measles data to pre-2024, and remove unnecessary columns
casedata_clean <- casedata_clean %>% 
  filter(year <= 2024) %>% 
  dplyr::select(-disease, -population) %>%
  mutate(updated_province = if_else(is.na(dps), province, dps))

#Noticed one slight typo that needs correcting:
casedata_clean$updated_province[casedata_clean$updated_province == "Maindombe"] <- "Mai-Ndombe"

#Add a column for the date in YYYY-MM-DD format
case_w_date <- casedata_clean %>% 
  mutate(date = parse_date_time(paste(year, week, 1, sep = "-"), "%Y-%W-%u") %>% as.Date())

#Yes, I did manually arrange the provinces in rough geography
case_w_date$updated_province <- factor(case_w_date$updated_province, 
                                       levels=c("Sud-Ubangi", "Nord-Ubangi", "Bas-Uele", "Haut-Uele",
                                       "Equateur", "Mongala", "Tshopo","Ituri",
                                       "Mai-Ndombe", "Tshuapa", "Maniema", "Nord-Kivu", "Sud-Kivu",
                                       "Kinshasa", "Kwilu", "Kasai", "Sankuru",
                                       "Kongo-Central", "Kwango", "Kasai-Central", "Kasai-Oriental", 
                                       "Lomami", "Tanganyika", "Lualaba", "Haut-Lomami", "Haut-Katanga"))

#Load in health zone data
hzone <- st_read("Data/Grid3 Health Zones/GRID3_COD_health_zones_v9_0.shp")
hzone <- hzone %>% dplyr::select(zonesante, geometry)
hzone$zonesante <- gsub(" ", "-", hzone$zonesante)

#A lot of the health zones listed by Grid3 are different from how they are listed
#in the measles data. These corrections are so they are uniform. But fuzzy matching 
#wasn't doing it for me... so I ended up just hand coding these.
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

#Load in population estimates and extract by health zone
pop <- rast("Data/Grid3 Population Estimates/COD_Population_v4_4_gridded.tif")
hzone <- hzone %>% mutate(pop_2024 = exact_extract(pop, hzone, 'sum'))

#So we can organize the plots by geography in the app,
#determine the center of each healthzone and create columns 
#for its lat and lon
hzone <- hzone %>% st_as_sf() %>% st_centroid() %>% 
  mutate(lon = st_coordinates(geometry)[,1], lat = st_coordinates(geometry)[,2]) %>% st_drop_geometry()

#Now merge with the measles data
case_w_date <- left_join(case_w_date, hzone, by=c("zs_clean"="zonesante"))

#########################################################
#### Static Plot ####

# I needed a plot of only the provinces that are east
# of Equateur province. I just put the code here, but it is
# not part of the app.

#filter down to the provinces of interest
a <- case_w_date %>% filter(updated_province %in% c("Equateur", "Nord-Kivu", "Tshuapa", "Tshopo"))
#organize from west to east
a$updated_province <- factor(a$updated_province, levels=c("Equateur", "Tshuapa", "Tshopo", "Nord-Kivu"))

#plot
ggplot() + 
  #plot dates by cases, group by health zone
  geom_line(data=a, aes(x=date, y=(cases/pop_2024)*100000, group=zs_clean, color=zs_clean), linewidth = 0.2, alpha = 0.6) +
  #plot vert lines for every 1 Jan
  geom_vline(xintercept = as.numeric(seq(as.Date("2001-01-01"), as.Date("2024-01-01"), by = "1 year")), 
             color = "gray70", linewidth = 0.1) +
  #fix x-axis breaks
  scale_x_date(breaks = as.Date(c("2001-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", "2024-01-01")),
               date_labels = "%Y", limits = c(as.Date("2001-01-01"), as.Date("2024-12-31")), expand = c(0, 0)) +
  #plot by panel
  facet_wrap(~updated_province, scales="fixed", ncol=1) + 
  #aesthetics
  theme(axis.text.x = element_text(angle=90), legend.position="none", #panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(), panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank(),
        axis.title.x = element_blank()) + ylab("Cases per 100,000 population")

#########################################################
##### Shiny App #####

ui <- fluidPage(
  
  #create a dropdown to choose between plotting province vs health zone
  fluidRow(column(6, selectInput("plot_level", "Level to Plot:", 
                                  choices=c("Province", "Health Zone"),
                                  selected="Province")),
  #create a drop down so you can choose which province to view at the health zone level
           column(6, conditionalPanel(condition="input.plot_level == 'Health Zone'",
                                  selectInput("prov", "Province:",
                                  choices=sort(unique(case_w_date$updated_province)),
                                  selected="Equateur")))),
  
  hr(),
  
  fluidRow(column(12, plotOutput("caseplot", height="80vh")))
)

server <- function(input, output, session) {
  
  #If plotting at the health zone level, filter down to the specified
  #province, then organize by geography
  a <- reactive({case_w_date %>% 
      filter(updated_province == input$prov) %>%
      arrange(desc(lat), lon) %>%
      mutate(zs_clean = factor(zs_clean, levels = unique(zs_clean)))
      })
  
  output$caseplot <- renderPlot({
    
    if (input$plot_level == "Province") {
      ggplot() + 
        #plot dates by cases, group by health zone
        geom_line(data=case_w_date, aes(x=date, y=(cases/pop_2024)*100000, group=zs_clean, color=zs_clean), linewidth = 0.2, alpha = 0.6) +
        #plot vert lines for every 1 Jan
        geom_vline(xintercept = as.numeric(seq(as.Date("2001-01-01"), as.Date("2024-01-01"), by = "1 year")), 
                   color = "gray70", linewidth = 0.1) +
        #fix x-axis breaks
        scale_x_date(breaks = as.Date(c("2001-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", "2024-01-01")),
                     date_labels = "%Y", limits = c(as.Date("2001-01-01"), as.Date("2024-12-31")), expand = c(0, 0)) +
        #plot by panel
        facet_wrap(~updated_province, scales="fixed", ncol=4) + 
        #other plot details
        theme(axis.text.x = element_text(angle=90), legend.position="none", #panel.grid.major.y=element_blank(),
              panel.grid.minor.y=element_blank(), panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank(),
              axis.title.x = element_blank()) + ylab("Cases per 100,000 population")
    }
    
    else {
    
      ggplot() + 
        #plot dates by cases, group by health zone
        geom_line(data=a(), aes(x=date, y=(cases/pop_2024)*100000)) +
        #plot vert lines for every 1 Jan
        geom_vline(xintercept = as.numeric(seq(as.Date("2001-01-01"), as.Date("2024-01-01"), by = "1 year")), 
                   color = "gray70", linewidth = 0.1) +
        #fix x-axis breaks
        scale_x_date(breaks = as.Date(c("2001-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", "2024-01-01")),
                     date_labels = "%Y", limits = c(as.Date("2001-01-01"), as.Date("2024-12-31")), expand = c(0, 0)) +
        #plot by panel
        facet_wrap(~zs_clean, scales="fixed", ncol=4) + 
        #other plot details
        theme(axis.text.x = element_text(angle=90), legend.position="none", #panel.grid.major.y=element_blank(),
              panel.grid.minor.y=element_blank(), panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank(),
              axis.title.x = element_blank()) + ylab("Cases per 100,000 population")
      
    }
  })
}

shinyApp(ui, server)
