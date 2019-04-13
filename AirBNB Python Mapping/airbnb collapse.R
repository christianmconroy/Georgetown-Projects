setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/ProgrammingStats/FinalGroupProject")

# Read in airbnb data
Airbnb <- read.csv("airbnbdata.csv", header=T)

# Add column for wards 
table(as.factor(Airbnb$neighborhood))
Airbnb$Ward <- 0
Airbnb$Ward <- ifelse(Airbnb$neighborhood == "Adams Morgan" | Airbnb$neighborhood == "Columbia Heights" | Airbnb$neighborhood == "Kalorama" | Airbnb$neighborhood == "Ledroit Park" | Airbnb$neighborhood == "Mount Pleasant" | Airbnb$neighborhood == "U Street Corridor", 1, ifelse(Airbnb$neighborhood == "Downtown" | Airbnb$neighborhood == "Dupont Circle" | Airbnb$neighborhood == "Foggy Bottom" | Airbnb$neighborhood == "Georgetown" | Airbnb$neighborhood == "Logan Circle" | Airbnb$neighborhood == "Mount Vernon Square" | Airbnb$neighborhood == "Shaw", 2, ifelse(Airbnb$neighborhood == "Au-Tenleytown" | Airbnb$neighborhood == "Barnaby Woods" | Airbnb$neighborhood == "Berkley" | Airbnb$neighborhood == "Chevy Chase" | Airbnb$neighborhood == "Cleveland Park" | Airbnb$neighborhood == "Foxhall Village" | Airbnb$neighborhood == "Friendship Heights" | Airbnb$neighborhood == "Glover Park" | Airbnb$neighborhood == "The Palisades" | Airbnb$neighborhood == "Woodley Park", 3, ifelse(Airbnb$neighborhood == "Brightwood" | Airbnb$neighborhood == "Fort Totten-Upper Northeast" | Airbnb$neighborhood == "Petworth" | Airbnb$neighborhood == "Takoma Park", 4, ifelse(Airbnb$neighborhood == "Brentwood" | Airbnb$neighborhood == "Catholic University-Brookland", 5, ifelse(Airbnb$neighborhood == "Capitol Hill" | Airbnb$neighborhood == "South West", 6, ifelse(Airbnb$neighborhood == "Deanwood" | Airbnb$neighborhood == "Stadium-Armory", 7, ifelse(Airbnb$neighborhood == "Anacostia", 8, NA))))))))

Airbnb_collapse <- Airbnb %>%
  group_by(Ward) %>%
  summarise_all(funs(mean))

write.csv(Airbnb_collapse, "airbnbwards.csv")

# Looking at DC Ward GEOJSON
library(rgdal)
map <- readOGR(dsn = "dc-ward-map-overlay.geojson", layer = "dc-ward-map-overlay")
map@data$name

# Comparing with Florida for Choropleth County
map2 <- readOGR(dsn = "https://raw.githubusercontent.com/plotly/datasets/master/florida_county_data.geojson", layer = "florida_county_data")


# Comparing with Colorado for Choropleth County
map3 <- readOGR(dsn = "http://catalog.civicdashboards.com/dataset/bae1d256-eb2b-4ece-b8dc-fa149b66a84e/resource/c9ddc844-6d01-4c7c-8c98-df932ea94597/download/2e3b193fffed4788a621b0095cd8182dtemp.geojson", layer = "2e3b193fffed4788a621b0095cd8182dtemp")

# Comparing with other county one 
map4 <- readOGR(dsn = "cb_2016_us_county_500k.shp", layer = "cb_2016_us_county_500k")

