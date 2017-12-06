library(tidycensus)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sf)
library(viridis)
library(stringr)
library(ggmap)
library(readr)
library(rgdal)


#homeless density and shelter optimization

#UNCOMMENT WHEN DOING FOR THE FIRST TIME, we are getting lats and longs from internet, no need to do it again and again once its a variable in environment.

# lonlat <- geocode(c("Downtown Los Angeles NC",
#                     "East Hollywood NC",
#                     "East Los Angeles",
#                     "Empowerment Congress North AREA",
#                     "Empowerment Congress Southeast NC",
#                     "Hacienda Heights",
#                     "Inglewood",
#                     "Lancaster",
#                     "MacArthur Park NC",
#                     "Mid-Town North Hollywood NC",
#                     "Montebello",
#                     "Pacoima NC",
#                     "Skid Row",
#                     "Sun Valley Area NC",
#                     "Unincorporated Antelope Valley" ,
#                     "Compton" ,
#                     "Palmdale" ,
#                     "Voices of 90037 NC",
#                     "Westmont / West Athens",
#                     "Zapata-King NC" ) )

#UNCOMMENT WHEN DOING FOR THE FIRST TIME, file is big and takes time. Hence load once and keep it as a  temp variable.
 
# pricing = read.csv("Datasets/Clean_Updated_Pricing.csv")
# pricing = na.omit(pricing)

combined_pit_data_plot_ll = read.csv("Datasets/combined_pit_data_plot_ll.csv")

##Getting shelter data
shelter_data = read.csv("Datasets/6_shelters_w_CTs20171102134808.csv")
shelter_data$NAME = as.character(shelter_data$NAME)

shelters = shelter_data %>% select(LONGITUDE,LATITUDE,GEOID10,NAME)

# Make data with several positions

data_red= data.frame(combined_pit_data_plot_ll %>% filter(Trend == "Rise") %>%
                       select(Detailed_Name,lon,lat))

data_blue=data.frame(combined_pit_data_plot_ll %>% filter(Trend == "Fall") %>%
                       select(Detailed_Name,lon,lat))

data_green=data.frame(shelter_data %>% select(LONGITUDE,LATITUDE,GEOID10,NAME))

data_shelter_rise = data.frame(shelter_data %>% select(LONGITUDE,LATITUDE,GEOID10,NAME,CITY)%>%
                                 filter(CITY %in% c("Compton","Palmdale")))

icon.pop <- awesomeIcons(icon = 'home',
                         library = 'fa',
                         iconColor = 'black')

#UNCOMMENT WHEN DOING FOR THE FIRST TIME. Cleaning pricing data frame.

# pricing$TotalLandImpValue = as.numeric(pricing$TotalLandImpValue)
# pricing$ZIPcode5 = as.character(pricing$ZIPcode5)
# pricing = pricing %>% filter (TaxRateArea_CITY == "LOS ANGELES")

#creating dataframe with zipcodes and land prices
zip = pricing %>% group_by(ZIPcode5) %>% summarise(avg_price = mean(TotalLandImpValue), lat = mean(CENTER_LAT), lon = mean(CENTER_LON))
zip <- zip[-c(7, 60, 101, 111), ]


##Input of Shape File
shape_c <- readOGR(dsn = ".", layer = "CouncilDistricts")
PRO <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
DATA <- sp::spTransform(shape_c,PRO)


#Color Palettes
palette <- colorBin(c('#fee5d9', '#fcbba1','#fc9272',
                      '#fb6a4a',
                      '#de2d26',
                      '#a50f15'), 
                    bins = c(52000, 70000, 80000, 90000, 100000, 1600000))


#Creating a popup
popup_pricing <- paste0("Avg Land Value in 2016",
                 "<br>Price: $",             
                 zip$avg_price,
                 "<br> Zipcode:",
                 zip$ZIPcode5)

#color for property
palette_property <- colorBin(c('#f2f0f7', '#cbc9e2','#9e9ac8','#6a51a3'),
                             bins = c(10000, 40000,90000,160000))


##Getting Help from Social Entreprenuers/Non profits:
Chrysalis = c("Chrysalis Downtown","Chrysalis Santa Monica","Chrysalis Paconima")
latlon = geocode(c("Chrysalis Downtown","Chrysalis Santa Monica","Chrysalis Paconima"))

Chrysalis_data = data.frame(cbind(Chrysalis[], latlon)) 

colnames(Chrysalis_data)[1] <- "Name"

# Initialize the leaflet map1:
#print("building maps 1")
map_density_optimization<-leaflet() %>% 
  setView(lng=-118.243683, lat=	34.052235, zoom=9 ) %>%
  
  # Add two tiles
  addProviderTiles("Esri.NatGeoWorldMap", group="Council Districts") %>%
  addTiles(options = providerTileOptions(noWrap = FALSE), group="Census Tract") %>%
  
  
  # Add 2 marker groups
  addCircleMarkers(data=data_red, lng=~lon , lat=~lat, radius=8 , color="black",  
                   fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="Rise",label = ~as.character(Detailed_Name)) %>%
  addCircleMarkers(data=data_blue, lng=~lon , lat=~lat, radius=8 , color="black",  
                   fillColor="blue", stroke = TRUE, fillOpacity = 0.8, group="Fall",label = ~as.character(Detailed_Name)) %>%
  addAwesomeMarkers(data=data_green, lng=~LONGITUDE , lat=~LATITUDE,icon = icon.pop,labelOptions = T,
                    group="Shelters",
                    #clusterOptions = markerClusterOptions(labelOptions= labelOptions(noHide = F)),
                    label = ~as.character(NAME)) %>%
  addAwesomeMarkers(data=data_shelter_rise, lng=~LONGITUDE , lat=~LATITUDE,icon = icon.pop,labelOptions = T,
                    group="Shelters In Rise Areas",
                    ##clusterOptions = markerClusterOptions(labelOptions= labelOptions(noHide = T)),
                    label = ~as.character(NAME)) %>%
  addPolygons(data = DATA, 
              fillColor = ~palette_property(zip$avg_price),
              weight = .5, popup = popup_pricing,labelOptions = T,
              color = "darkgrey",group = "Land Pricing") %>%
  addCircleMarkers(data=Chrysalis_data, lng=~lon , lat=~lat, radius=4 , color="black",  
                   fillColor="black",stroke = TRUE, fillOpacity = 1, group="Non Profits",label = ~as.character(Chrysalis)) %>%
  
  # Add the control widget
  addLayersControl(overlayGroups = c("Rise","Fall","Shelters","Shelters In Rise Areas","Land Pricing","Non Profits"),baseGroups = c("Census Tract","Council District"),
                   options = layersControlOptions(collapsed = FALSE))  %>%
  
  ##Add Legend
  addLegend(position = 'bottomleft', 
            colors = c('#f2f0f7', '#cbc9e2','#9e9ac8','#6a51a3'), 
            labels = c('$10,000',"","", '$200,000'),  
            opacity = 0.6,      
            title = "Property Value")


#census and crime data

#UNCOMMENT WHEN DOING FOR THE FIRST TIME. Then save this  file on your system so you dont have to make API calls again and again.

#census_api_key('e989e48b7c45f28d56c8bba04c80dba7ff9a4ea5',install = TRUE,overwrite = TRUE)
#readRenviron("~/.Renviron")
#LAB = get_decennial(state = "CA", county = "Los Angeles", geography = "tract", geometry = TRUE,variables = "TRACT")
#saveRDS(LAB,file="Datasets/temp.rds")

LAB =readRDS("Datasets/temp.rds")

data_homeless <- read.csv("Datasets/homeless.csv")
colnames(data_homeless)[1] = "tractID"
data_crime <- read.csv("Datasets/crime_w_CTs.csv")

LAB$GEOID = as.numeric(LAB$GEOID)

homeless_map = left_join(LAB,data_homeless,by = c("GEOID" = "tractID"))

pal_homeless<- colorNumeric(
  palette = "YlOrRd",
  domain = homeless_map$totPeople)

data_f = data_crime %>%
  filter(VICTIM.SEX == "F")
data_m = data_crime %>%
  filter(VICTIM.SEX =="M")
#print("building maps 2")
map_crime<-homeless_map %>% 
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet() %>% 
  addPolygons(fillOpacity = 0.8,
              stroke = FALSE, 
              smoothFactor = 0.2, 
              fillColor = ~pal_homeless(totPeople),
              label = ~as.character(GEOID),group = "Homeless Density") %>%
  addTiles()%>%
  addCircleMarkers(data=data_f, lng=~LONGITUDE , lat=~LATITUDE, radius=3 , color = "pink",  
                   fillColor="pink", stroke = TRUE, fillOpacity = 0.8, group="Crime victims - Female")%>%
  addCircleMarkers(data=data_m, lng=~LONGITUDE , lat=~LATITUDE, radius=3 , color = "lightblue",
                   fillColor="lightblue", stroke = TRUE, fillOpacity = 0.8, group="Crime victims - Male")%>%
  addLegend("bottomright", 
            pal = pal_homeless, 
            values = ~ totPeople,
            title = "Homeless Count",
            opacity = 1
  ) %>%
  addLayersControl(
    overlayGroups = c("Crime victims - Female","Crime victims - Male"), 
    baseGroups = c("Homeless Density"),
    options = layersControlOptions(collapsed = FALSE)) 
#set default view    
setView(map_crime,lng =-118.243683, lat= 34.052235, zoom=10 )


##Mental Health Care

##READING MENTAL HEALTH DATA
mentaldata = read.csv("Datasets/Mental_Health_Counseling.csv") 
#print("building maps 3")
##Initialize the leaflet map:
map_mental_health<-homeless_map %>% 
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet() %>% 
  addPolygons(fillOpacity = 0.8,
              stroke = FALSE, 
              smoothFactor = 0.2, 
              fillColor = ~pal_homeless(totPeople),
              label = ~as.character(GEOID))%>%
  setView(lng=-118.243683, lat=	34.052235, zoom=10 ) %>% 
  
  # Add two tiles
  addProviderTiles("Esri.NatGeoWorldMap", group="Backdrop - Census Tract Counts") %>%
  addTiles(options = providerTileOptions(noWrap = FALSE), group="Backdrop - Census Tract Counts" ) %>%
  # Add 2 marker groups
  addCircleMarkers(data=mentaldata, lng=~longitude , lat=~latitude, radius=4 , color="black",  
                   fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="Mental Health Centers",label = ~as.character(Name)) %>%
  # Add the control widget
  addLayersControl(overlayGroups = c("Mental Health Centers"),
                   baseGroups = c("Backdrop - Census Tract Counts"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  
  addLegend("bottomright", 
            pal = pal_homeless, 
            values = ~ totPeople,
            title = "Homeless Count",
            opacity = 1) 