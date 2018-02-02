# LeafletMapsExample.R
#Gates

##########################################################################
##This code works well and creates an interactive layered leaflet/R map
## The map is choropleth and markered
##
## Required datasets are here:
##    CancerCountyFIPS.csv
##    CancerCountyFIPS_Breast.csv
##    LandUseDatasetREALLatlong.csv
## AND ##
############
# Download county shape file.
## !! This is important. Shape files can be found here
#https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
#us.map <- tigris::counties(cb = TRUE, year = 2015)
#OR
# Download county shape file from Tiger.
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
# I downloaded the zip and placed all files in the zip into my RStudio folder
#us.map <- readOGR(dsn = ".", layer = "cb_2016_us_county_20m", stringsAsFactors = FALSE)
##head(us.map)
###############
##Not all of these libraries are required for this code, but
## they are good for more generalized goals
############################################################################

library(leaflet)
library(sp)
library(mapproj)
library(maps)
library(mapdata)
library(maptools)
library(htmlwidgets)
library(magrittr)
library(XML)
library(plyr)
library(rgdal)
library(WDI)
library(raster)
library(noncensus)
library(stringr)
library(tidyr)
library(tigris)
library(rgeos)
library(ggplot2)
library(scales)
library(ggmap)

data(zip_codes)
data(counties)

################################################################
##https://www.statecancerprofiles.cancer.gov/incidencerates/index.php?stateFIPS=99&cancer=001&race=07&sex=0&age=001&type=incd&sortVariableName=rate&sortOrder=default#results
CancerRates <- read.csv('CancerCountyFIPS.csv')
#head(CancerRates)
CancerRatesB <- read.csv('CancerCountyFIPS_Breast.csv')
#head(CancerRatesB)
LandUse <- read.csv('LandUseDatasetREALLatlong.csv')

# load additional data
pop1 = read.csv('data_pop1.csv')
pop2 = read.csv('data_pop2.csv')



# drop unused column
pop1 = pop1[,1:2]
pop2 = pop2[,c(1,3)]
#head(LandUse)
## Not using this dataset yet...
#PowerPlants <- read.csv("PowerPlants.csv")
#head(PowerPlants)

## Make all the column names lowercase
names(CancerRates) <- tolower(names(CancerRates))
#head(CancerRates)

# Rename columns to make for a clean df merge later.
##GEOID is the same as FIPS
colnames(CancerRates) <- c("location", "GEOID", "rate")
#head(CancerRates)
colnames(CancerRatesB) <- c("location", "GEOID", "rate")
#head(CancerRatesB)
colnames(LandUse) <- c("offset", "lat", "lng", "url", "name")
#head(LandUse)
colnames(pop1) = c('location', 'population')
colnames(pop2) = c('location', 'density')
##Add leading zeos to any FIPS code that's less than 5 digits long to get a good match.
##formatC is from C code formatting - creates a 5 digit int
CancerRates$GEOID <- formatC(CancerRates$GEOID, width = 5, format = "d", flag = "0")
#head(CancerRates)
CancerRatesB$GEOID <- formatC(CancerRatesB$GEOID, width = 5, format = "d", flag = "0")
head(CancerRatesB)

## Convert column called location to two columns: State and County
CancerRates <- separate(CancerRates, location, into = c("county", "state"), sep = ", ")
#head(CancerRates)
CancerRatesB <- separate(CancerRatesB, location, into = c("county", "state"), sep = ", ")
#head(CancerRatesB)
pop1 <- separate(pop1, location, into = c("city", "state"), sep = ", ")
pop2 <- separate(pop2, location, into = c("city", "state"), sep = ", ")

# add latitude and logitude
geocodes <- geocode(as.character(pop1$city))
pop1 <- data.frame(pop1,geocodes)

geocodes2 <- geocode(as.character(pop2$city))
pop2 <- data.frame(pop2,geocodes2)

##Remove the (...) from the state values
CancerRates[] <- lapply(CancerRates, function(x) gsub("\\s*\\([^\\)]+\\)", "", x))
head(CancerRates)
CancerRatesB[] <- lapply(CancerRatesB, function(x) gsub("\\s*\\([^\\)]+\\)", "", x))
head(CancerRatesB)

##Remove the space# from the end of some of the values in the rate column
CancerRatesB[] <- lapply(CancerRatesB, function(x) gsub("\\#", "", x))
#CancerRatesB

# Convert full state names to abbreviations for a clean df merge later.
CancerRates$state <- state.abb[match(CancerRates$state,state.name)]
#head(CancerRates)
CancerRatesB$state <- state.abb[match(CancerRatesB$state,state.name)]
#head(CancerRatesB)

#Change CancerRates$rate to a number
CancerRates$rate <- as.numeric(as.character(CancerRates$rate))
#head(CancerRates)
CancerRatesB$rate <- as.numeric(as.character(CancerRatesB$rate))
#head(CancerRatesB)


# Download county shape file.
## !! This is important. Shape files can be found here
#https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
#us.map <- tigris::counties(cb = TRUE, year = 2015)
#OR
# Download county shape file from Tiger.
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
# I downloaded the zip and placed all files in the zip into my RStudio folder
us.map <- readOGR(dsn = ".", layer = "cb_2016_us_county_20m", stringsAsFactors = FALSE)
head(us.map)
# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
#  Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]
#head(us.map)

# Make sure other outling islands are removed.
us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]

# Merge spatial df with downloaded data.
## This is important
## Now we have our data and the needed carto data
cancermap <- merge(us.map, CancerRates, by=c("GEOID"))
cancermapB <- merge(us.map, CancerRatesB, by=c("GEOID"))

# Format popup data for leaflet map.
popup_dat <- paste0("<strong>County: </strong>", 
                    cancermap$county, 
                    "<br><strong>Cancer Rate (Age Adjusted) Out of 100,000: </strong>", 
                    cancermap$rate)

popup_dat_B <- paste0("<strong>County: </strong>", 
                    cancermapB$county, 
                    "<br><strong>Breast Cancer Rate (Age Adjusted) Out of 100,000: </strong>", 
                    cancermapB$rate)


#Grouping for map options and User Choices
#https://rstudio.github.io/leaflet/showhide.html

##Make pop up for the land use sites
# Format popup data for leaflet map.
popup_LU <- paste0("<strong>Use Name: </strong>", 
                   LandUse$name, 
                   "<br><strong>Link: </strong>", 
                   LandUse$url)

popup_p1 <- paste0("<strong>City: </strong>", 
                   pop1$city, 
                   "<br><strong>Total population: </strong>", 
                   pop1$population)

popup_p2 <- paste0("<strong>City: </strong>", 
                   pop2$city, 
                   "<br><strong>Population density: </strong>", 
                   pop2$density)

pal <- colorQuantile("YlOrRd", NULL, n = 9)

pal2 <- colorQuantile("Greens", NULL, n = 9)

# icons
greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

redLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-red.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

gmap <- leaflet(data = cancermap) %>%
  # Base groups
  addTiles() %>%
  setView(lng = -105, lat = 40, zoom = 4) %>% 
  addPolygons(fillColor = ~pal(rate), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = popup_dat,
              group="Cancer Rate/100,000 by Counties") %>% 
  
  # add breast cancer
  addPolygons(fillColor = ~pal2(rate), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = popup_dat_B,
              group="Breast Cancer Rate/100,000 by Counties") %>% 
  
  # Overlay groups
  addMarkers(data=LandUse,lat=~lat, lng=~lng, popup=popup_LU, group = "Land Use Sites") %>% 
  addMarkers(data=pop1,lat=~lat, lng=~lon, popup=popup_p1, group = "Most Populous", icon = greenLeafIcon) %>% 
  addMarkers(data=pop2,lat=~lat, lng=~lon, popup=popup_p2, group = "Highest Population Density", icon = redLeafIcon) %>% 

  # Layers control
  addLayersControl(
    baseGroups = c("Cancer Rate/100,000 by Counties","Breast Cancer Rate/100,000 by Counties" ),
    overlayGroups = c("Land Use Sites", "Most Populous", "Highest Population Density"),
    options = layersControlOptions(collapsed = FALSE)
  )
gmap
saveWidget(gmap, 'US_county_cancer_poll_map.html', selfcontained = TRUE)