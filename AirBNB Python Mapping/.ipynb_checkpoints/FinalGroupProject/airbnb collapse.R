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


