###### Redistricting Project Analysis ######

##### Analyzing the Existing District Map #####

setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/BIGWork/BIGDataFiles")
VACurMap <- read.csv("StateHouseDist.csv", header=T)
VACurMap$
  # Subsetting to Columns I want #
  str(VACurMap)
VACurMap$RepPercent <- VACurMap$Republican/VACurMap$Grand.Total
VACurMap$RepPercent
VACurMap$DemPercent <- VACurMap$Democratic/VACurMap$Grand.Total
VACurMap$DemPercent
VACurMapSub <- subset(VACurMap, select=c("house2013", "party", "RepPercent", "DemPercent", "np_score", "mrp_mean", "mrp_sd", "mrp_lower", "mrp_upper", "raw_mean", "sample_size", "pres_2012", "LegislatorChamberIPGap_Party", "LegParGap", "DistProportion", "X.Wht",	"X.Blk",	"X.Ind",	"X.Asn", "X.Hispanic", "X18._.Wht",	"X18._.Blk",	"X18._.Ind",	"X18._.Asn", "X18._.Hispanic"))

# Add in the chamber-level data for House 2013
# From https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K7ELHW
VACurMapSub$houmed <- .619
VACurMapSub$demchammed <- -.85
VACurMapSub$repchammed <- .72
VACurMapSub$houpartdiffs <- 1.57
VACurMapSub$hourepdist <- .8535995

# Calculate absolute value distance of legislator ideal point from mean policy perfernece of his or her district to get metric of accountability
VACurMapSub$distrepaccount <- abs(VACurMapSub$mrp_mean-VACurMapSub$np_score)

# Calculate margin of competitiveness 
VACurMapSub$PolComp <- abs(VACurMapSub$RepPercent-VACurMapSub$DemPercent)
VACurMapSub$PolComp

# Calculate District Averages for Metrics 
summary(VACurMapSub$distrepaccount)
summary(VACurMapSub$PolComp)

# Print the table for use in Presentation - Need to Figure Out!!!!
VACurMapSub2 <- subset(VACurMapSub, select=c("house2013", "party", "distrepaccount", "PolComp", "X.Wht",	"X.Blk",	"X.Hispanic", "X18._.Wht",	"X18._.Blk", "X18._.Hispanic"))
str(VACurMapSub2)

library(gridExtra)
png(filename = "output1.png", width=800,height=1200) 
grid.table(VACurMapSub2) 
dev.off() 
# This is not very useful to do for powerpoint though as it will look like shit regardless. 

################# Analyzing Randomly Generated Maps ########################
#### Conducting Analysis on One Map for Proof of Concept ####
# Importing the data
# Method 1: Export Attribute Table from QGIS as a CSV file and import into R via read.csv (What we use below)
setwd("/Users/Administrator/Documents/ForParallel")
cfbdata <- read.csv("OpenAttributesS10cfb16.csv", header=T)

# Method 2: Import directly into R via rjson (Need to do for looping)
library(rgdal)
map2 <- readOGR(dsn = "/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/BIGWork/BIGDataFiles/maps/sampled_maps/0cfb16c1-90c2-412d-bb60-2fec34c75e9a.geojson", layer = "0cfb16c1-90c2-412d-bb60-2fec34c75e9a")
summary(map2)
map2 <- as.data.frame(map2)

# Calculating the Efficiency Gap 
# EG = S ??? .5 ??? 2(V ??? .5) where S is the major-party proportion of legislative seats won by the focal party (in our case, Republicans) and V is the two-party proportion of statewide votes won by that party.

# An example with analyzing just one of the GEOJSON Files
str(cfbdata)
cfbdata$reptotal <- (cfbdata$surveyed_republican_percentage/100)*cfbdata$surveyed_total
cfbdata$demtotal <- (cfbdata$surveyed_democrat_percentage/100)*cfbdata$surveyed_total
cfbdata$NAME <- NULL
aggdata <-aggregate(cfbdata, by=list(cfbdata$cluster), 
                    FUN=sum, na.rm=TRUE)
# Rep district victory is 1 and Dem district victory is 0
aggdata$result <- ifelse(aggdata$reptotal > aggdata$demtotal,1, ifelse(aggdata$demtotal > aggdata$reptotal,0, NA))

EffGapCalc <- subset(aggdata, select=c("cluster","reptotal","demtotal","surveyed_total", "result"))

# Step 1: Calculate Dem Wasted, Rep Wasted, and Net Wasted

EffGapCalc$repwasted <- ifelse(EffGapCalc$result == 1, EffGapCalc$reptotal - (.51*EffGapCalc$surveyed_total), ifelse(EffGapCalc$result == 0, EffGapCalc$reptotal, NA))

EffGapCalc$demwasted <- ifelse(EffGapCalc$result == 0, EffGapCalc$demtotal - (.51 * EffGapCalc$surveyed_total), ifelse(EffGapCalc$result == 1, EffGapCalc$demtotal, NA))

EffGapCalc$netwasted <- abs(EffGapCalc$repwasted - EffGapCalc$demwasted)

# Step 2: Sum Total Wasted Rep and Dem Votes
totrepwasted <- sum(EffGapCalc$repwasted)
totdemwasted <- sum(EffGapCalc$demwasted)
netwaste <- ifelse(totrepwasted>totdemwasted, totrepwasted-totdemwasted, ifelse(totrepwasted<totdemwasted, totdemwasted-totrepwasted))
netwaste
# Democrats had a net waste (more wasted votes) of 74289.6

# Step 3: Divide Net Wasted by Total Number of Votes Case
sum(EffGapCalc$surveyed_total)
totalsurvtot <- sum(EffGapCalc$surveyed_total)
netwaste/totalsurvtot
# Efficiency Gap = .0359 [3.60%]
# Perfect. Tolerable Efficiency Gap.

#### Aggregate Analysis of All GEOJSON FILES #### 
setwd("/Users/Administrator/Documents/ForParallel/output_maps_2")
install.packages('foreach')
library(foreach)
install.packages('doParallel')
library(doParallel)

ncores <- detectCores() -1
cl <- makeCluster(ncores)
registerDoParallel(cl)

# Bring in All GEOJSON FILES IN ONE GO
install.packages('rgdal')
library(rgdal)
install.packages('rgeos')
library(rgeos)
testfile <- list.files(path = "/Users/Administrator/Documents/ForParallel/output_maps_2", pattern="*.geojson", full.names = TRUE)

## Calculating the Efficiency Gap ##

# Build the function that you want to run on every geojson file (i.e. on every map attribute table)

effgap <- function(filename) {
  map1 <- readOGR(dsn = filename, layer = ogrListLayers(filename))
  map1$reptotal <- (map1$surveyed_republican_percentage/100)*map1$surveyed_total;
  map1$demtotal <- (map1$surveyed_democrat_percentage/100)*map1$surveyed_total;
  map1$NAME <- NULL;
  map1$PRECINCT <- NULL;
  map1$PRECINCT2 <- NULL;
  map1$COUNTY <- NULL;
  aggdata <-aggregate(map1, by=list(map1$cluster), 
                      FUN=sum, na.rm=TRUE);
  aggdata$result <- ifelse(aggdata$reptotal > aggdata$demtotal,1, ifelse(aggdata$demtotal > aggdata$reptotal,0, NA));
  
  EffGapCalc <- subset(aggdata, select=c("cluster","reptotal","demtotal","surveyed_total", "result"));
  # Step 1: Calculate Dem Wasted, Rep Wasted, and Net Wasted
  EffGapCalc$repwasted <- ifelse(EffGapCalc$result == 1, EffGapCalc$reptotal - (.51*EffGapCalc$surveyed_total), ifelse(EffGapCalc$result == 0, EffGapCalc$reptotal, NA));
  
  EffGapCalc$demwasted <- ifelse(EffGapCalc$result == 0, EffGapCalc$demtotal - (.51 * EffGapCalc$surveyed_total), ifelse(EffGapCalc$result == 1, EffGapCalc$demtotal, NA));
  
  EffGapCalc$netwasted <- abs(EffGapCalc$repwasted - EffGapCalc$demwasted);
  
  # Step 2: Sum Total Wasted Rep and Dem Votes
  totrepwasted <- sum(EffGapCalc$repwasted);
  totdemwasted <- sum(EffGapCalc$demwasted);
  netwaste <- ifelse(totrepwasted>totdemwasted, totrepwasted-totdemwasted, ifelse(totrepwasted<totdemwasted, totdemwasted-totrepwasted));
  netwaste
  
  # Step 3: Divide Net Wasted by Total Number of Votes Case
  totalsurvtot <- sum(EffGapCalc$surveyed_total);
  netwaste/totalsurvtot;
}

# Get the average efficiency gap for all maps - data to data1 just so that I don't have to run the whole lapply again if I mess up somewhere; data1 to data2 to bind all efficiency gap values into one column of a matrix; data2 to data3 to turn the matrix into a data frame for easier analysis. 
clusterExport(cl, list("effgap", "readOGR", "ogrListLayers"))
data <- parLapply(cl, testfile, effgap)
data1 <- data
data2 <- cbind(data1[1:234])
data3 <- as.data.frame(data2)
data3$V1 <- as.numeric(data3$V1)
summary(data3$V1)
write.csv(data3,'Efficiency Gap.csv')
# The mean efficiency gap for all maps is .037458, or 3.74%

## Calculating Avg. Partisan Symmetry ##
# Calculate Seatshare
seatshare <- function(filename) {
  map2 <- readOGR(dsn = filename, layer = ogrListLayers(filename))
  map2$reptotal <- (map2$surveyed_republican_percentage/100)*map2$surveyed_total;
  map2$demtotal <- (map2$surveyed_democrat_percentage/100)*map2$surveyed_total;
  map2$NAME <- NULL;
  map2$PRECINCT <- NULL;
  map2$PRECINCT2 <- NULL;
  map2$COUNTY <- NULL;
  aggdata2 <-aggregate(map2, by=list(map2$cluster), 
                       FUN=sum, na.rm=TRUE);
  aggdata2$result <- ifelse(aggdata2$reptotal > aggdata2$demtotal,1, ifelse(aggdata2$demtotal > aggdata2$reptotal,0, NA));
  aggdata2$repper <- aggdata2$reptotal/aggdata2$surveyed_total;
  aggdata2$demper <- aggdata2$demtotal/aggdata2$surveyed_total;
  
  PartSymCalc <- subset(aggdata2, select=c("cluster", "reptotal", "demtotal", "repper","demper","surveyed_total", "result"));
  
  # Calculate average sear share per map
  seattotal <- mean(PartSymCalc$result);
}

# Calculate Republican vote share average
reptot <- function(filename) {
  map2 <- readOGR(dsn = filename, layer = ogrListLayers(filename))
  map2$reptotal <- (map2$surveyed_republican_percentage/100)*map2$surveyed_total;
  map2$demtotal <- (map2$surveyed_democrat_percentage/100)*map2$surveyed_total;
  map2$NAME <- NULL;
  map2$PRECINCT <- NULL;
  map2$PRECINCT2 <- NULL;
  map2$COUNTY <- NULL;
  aggdata2 <-aggregate(map2, by=list(map2$cluster), 
                       FUN=sum, na.rm=TRUE);
  aggdata2$result <- ifelse(aggdata2$reptotal > aggdata2$demtotal,1, ifelse(aggdata2$demtotal > aggdata2$reptotal,0, NA));
  aggdata2$repper <- aggdata2$reptotal/aggdata2$surveyed_total;
  aggdata2$demper <- aggdata2$demtotal/aggdata2$surveyed_total;
  
  PartSymCalc <- subset(aggdata2, select=c("cluster", "reptotal", "demtotal", "repper","demper","surveyed_total", "result"));
  
  # Calculate Rep Total for State (Can probably just get elsewhere)
  stateoutcomerep <- sum(PartSymCalc$reptotal)/sum(PartSymCalc$surveyed_total)
}

# Calculate Dem Total for State
demtot <- function(filename) {
  map2 <- readOGR(dsn = filename, layer = ogrListLayers(filename))
  map2$reptotal <- (map2$surveyed_republican_percentage/100)*map2$surveyed_total;
  map2$demtotal <- (map2$surveyed_democrat_percentage/100)*map2$surveyed_total;
  map2$NAME <- NULL;
  map2$PRECINCT <- NULL;
  map2$PRECINCT2 <- NULL;
  map2$COUNTY <- NULL;
  aggdata2 <-aggregate(map2, by=list(map2$cluster), 
                       FUN=sum, na.rm=TRUE);
  aggdata2$result <- ifelse(aggdata2$reptotal > aggdata2$demtotal,1, ifelse(aggdata2$demtotal > aggdata2$reptotal,0, NA));
  aggdata2$repper <- aggdata2$reptotal/aggdata2$surveyed_total;
  aggdata2$demper <- aggdata2$demtotal/aggdata2$surveyed_total;
  
  PartSymCalc <- subset(aggdata2, select=c("cluster", "reptotal", "demtotal", "repper","demper","surveyed_total", "result"));
  
  # Calculate Dem Total for State (Can probably just get elsewhere) 
  stateoutcomedem <- sum(PartSymCalc$demtotal)/sum(PartSymCalc$surveyed_total)
}

# Calculate Seatshare with Prop Table
propseat <- function(filename) {
  map2 <- readOGR(dsn = filename, layer = ogrListLayers(filename))
  map2$reptotal <- (map2$surveyed_republican_percentage/100)*map2$surveyed_total;
  map2$demtotal <- (map2$surveyed_democrat_percentage/100)*map2$surveyed_total;
  map2$NAME <- NULL;
  map2$PRECINCT <- NULL;
  map2$PRECINCT2 <- NULL;
  map2$COUNTY <- NULL;
  aggdata2 <-aggregate(map2, by=list(map2$cluster), 
                       FUN=sum, na.rm=TRUE);
  aggdata2$result <- ifelse(aggdata2$reptotal > aggdata2$demtotal,1, ifelse(aggdata2$demtotal > aggdata2$reptotal,0, NA));
  aggdata2$repper <- aggdata2$reptotal/aggdata2$surveyed_total;
  aggdata2$demper <- aggdata2$demtotal/aggdata2$surveyed_total;
  
  PartSymCalc <- subset(aggdata2, select=c("cluster", "reptotal", "demtotal", "repper","demper","surveyed_total", "result"));
  
  # Calculate average sear share per map
  prop.table(table(PartSymCalc$result));
}
clusterExport(cl, list("seatshare","propseat", "reptot", "demtot"))
seatshare1 <- parLapply(cl, testfile, seatshare)
seatsharedata <- seatshare1
seatsharedata2 <- cbind(seatshare1[1:234])
seatsharedata3 <- as.data.frame(seatsharedata2)
seatsharedata3$V1 <- as.numeric(seatsharedata3$V1)
write.csv(seatsharedata3,'Seatshare.csv')
propseat1 <- parLapply(cl, testfile, propseat)
propseatdata <- propseat1
propseatdata2 <- data.frame(matrix(unlist(propseatdata), nrow=234, byrow=T))
names(propseatdata2) <- c("Democrat Share", "Republican Share")
write.csv(propseatdata2,'Propseat.csv')
reptot1 <- parLapply(cl, testfile, reptot)
reptotdata <- reptot1
reptotdata2 <- cbind(reptot1[1:234])
reptotdata3 <- as.data.frame(reptotdata2)
reptotdata3$V1 <- as.numeric(reptotdata3$V1)
write.csv(reptotdata3,'Reptot.csv')
demtot2 <- parLapply(cl, testfile, demtot)
demtotdata <- demtot2
demtotdata2 <- cbind(demtotdata[1:234])
demtotdata3 <- as.data.frame(demtotdata2)
demtotdata3$V1 <- as.numeric(demtotdata3$V1)
write.csv(demtotdata3,'Demtot.csv')

summary(seatsharedata3$V1)
# On average, maps produced outcomes where Republicans held 68%, or 68, of the seats, and  Democrats held 32%, or 32 o the seats... Need to make sure we calculated this right as it doesn't really make sense given the efficiency gap estimate. 
summary(propseatdata2$`Democrat Share`)
summary(propseatdata2$`Republican Share`)
# Confirmed the above. Will have to assess after the updated maps come in. 
summary(reptotdata3$V1)
# Republicans won 54.00% of the seats statewide 
demtot2
# Democrats won 40.32% of the seats statewide

#### Public Preference Change Analysis ####
## Cleaning and Merging ZipCode Level Preference Data with Tigerline ZipCode Shape file Zipcodes 

#   ### Method 1: Use Maptitude default Zip layer (2017 I think) and Warshaw data with range reduced based on the Maptitude range
# ## Load in File 1 ##
# setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/BIGWork/BIGDataFiles/ZipCodeYearTests")
# Zips <- read.csv("DigitZIPCodeMaptDefault.csv", header=T)
# head(Zips)
# 
# ## Load  in File 2 ##
# install.packages('haven')
# library(haven)
#   ## The "WarshawZip below is for that cleaned up based on the ranges of the zip codes for Virginia in Maptitude. 
# setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/BIGWork/BIGDataFiles")
# warszips <- read_dta('WarshawZip.dta')
# head(warszips)
# str(warszips)
# 
# ## Make sure all is numeric and take out what is not numeric##
# class(warszips$zip)
# class(warszips$respondent)
# class(warszips$ideal_point)
# class(warszips$ideal_point_se)
# warszips$zip <- as.numeric(warszips$zip)
# warszips$source <- NULL
# warszips$zipnum <- NULL
# warszips$notnumeric <- NULL
# 
# ## Test for similarities#
# warszips2 <- na.omit(warszips)
# wzagg <-aggregate(warszips2, by=list(warszips2$zip), 
#                   FUN=mean, na.rm=TRUE)
# 
# wzagg$zip[!wzagg$zip %in% Zips$ZIP5]
# warszips$zip[!warszips$zip %in% Zips$ZIP5] <- NA
# 
# # When we dropped those zipcodes in the Warshaw data that do not have a counterpart in the Maptitude Zip File, we went from 18926 respondents to 11066.

# When we dropped those zipcodes in the Warshaw data that do not have a counterpart in the Maptitude Zip File, we went from 1153 to 682 zipcodes

# # 471 zipcodes out of place between the two. Need to fix this!!!
# 
# # Merging the files 
# newtable <- merge(wzagg, Zips, by.x = "zip", by.y = "ZIP5")
# 
# # Export adjusted Warshaw files 
# write.csv(newtable, "newtable.csv")

## Method 2: Use other years to compare - Take the national zipcode file and use the clip layers function of Maptitude to confine it to the boundaries of Virginia 
## Testing is we can get better matches with the zip codes of different years 

## Load  in File 3 ##
# ## The warszipsfull has only been adjusted to eliminate those with missing zip codes.  
# setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/BIGWork/BIGDataFiles")
# warszipsfull <- read_dta('WarShawFullNoMiss.dta')
# head(warszipsfull)
# str(warszipsfull)
# 
# ## Make sure all is numeric and take out what is not numeric##
# class(warszipsfull$zipnum)
# class(warszipsfull$respondent)
# class(warszipsfull$ideal_point)
# class(warszipsfull$ideal_point_se)
# warszipsfull$source <- NULL
# warszipsfull$zip <- NULL
# warszipsfull$notnumeric <- NULL
# 
# warszipsfull <- subset(warszipsfull, nchar(as.character(warszipsfull$zipnum)) == 5)

# ## Loading Zip Codes Years ##
# ##2007 beased on 2000 Census
# setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/BIGWork/BIGDataFiles/ZipCodeYearTests")
# VAZips2007 <- read.csv("VAZips2007.csv", header=T)
# head(VAZips2007)
# 
# ## Subset Warshaw based on range of imported VA 2007 file range
# VAZips2007$ZCTA5CE00 <- as.numeric(as.character(VAZips2007$ZCTA5CE00))
# VAZips2007 <- na.omit(VAZips2007)
# sum(is.na(VAZips2007$ZCTA5CE00))
# summary(VAZips2007$ZCTA5CE00)
# 
# warszipsfull <- subset(warszipsfull, warszipsfull$zipnum >=20024 & warszipsfull$zipnum <= 41837)
# 
# wzaggfull <-aggregate(warszipsfull, by=list(warszipsfull$zipnum), 
#                   FUN=mean, na.rm=TRUE)
# summary(wzaggfull$zipnum)
# 
# #wzaggfull zips not in the Zips2007
# wzaggfull$zipnum[!wzaggfull$zipnum %in% VAZips2007$ZCTA5CE00] <- NA
# wzaggfull <- na.omit(wzaggfull)
# wzaggfull$zipnum[!wzaggfull$zipnum %in% VAZips2007$ZCTA5CE00]
# 
# # Save the adjusted output so that I can attach it in QGIS.
# write.csv(wzaggfull, "wazaggVa2007adj.csv")


#### Aggregating zipcode data up to new districts ####
# setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/BIGWork/BIGDataFiles/ZipCodeYearTests")
# 
# # To get the file below, we:
# # 1. imported the random map file and zip files in 
# # 2. Changed the respective CRSs to equal each other and saved as two new files
# # 3. Joined the ideal point data to the new crs zip code layer
# # 4. Conducted spatial join of ZipCRS to random map CRS
# # Need to figure out how to use the API to automate all of the above for all files
# VAPrcIdeNum <- read.csv("PrecintIIdealNumsTest.csv", header=T)
# head(VAPrcIdeNum)
# VAPrcIdeNum$NAME <- NULL
# 
# VAClustIdeNum <-aggregate(VAPrcIdeNum, by=list(VAPrcIdeNum$cluster), 
#                       FUN=mean, na.rm=TRUE)
# 
# summary(VAClustIdeNum$meanForVA2007QGISJoin_ideal_point)
# # The average policy preference per district is at .01523, meaning that it is relatively centralist. 
# 
# # What we still need
# # 1. Figure out how to do all of this via APIs so I can run it for all maps
# # 2. Try to ensure we get a measure of how many individual responses come in each district/map and zipcodes that come in each district/map.
# 3. Use Maptitude's other recommendation (i.e. not clipping) to try again. 
# 4. Assess again whether 2000 census is the right zip code to be using. 

# Can use GDAL, SF, and SP
# Can merge shape files to CSVs

# R OVER -> That should do a spatial join
# Send a note to Chen's Georgetown email to get further help on this!!!

##### Trying out Chen's Methods and Automating Everything in Here#####
setwd("/Users/Administrator/Documents/ForParallel")
## Subsetting Zipcode Data to Just VA (No looping needed)
# Importing VA state Boundary, US Zip Codes, and random maps
library(rgdal)
library(rgeos)

USzips <- readOGR(".", "fe_2007_us_zcta500")
VABoundary <- readOGR(".", "VAState")
oofmap <- readOGR(".", "00f76")

# Ensuring the two layers use the same CRS. 
USzips <- spTransform(USzips, CRS(proj4string(VABoundary))) 
plot(VABoundary)

# Subsetting/Clipping to make it just VA Zipcodes 
VA_zipsub <- USzips[VABoundary,]
plot(VABoundary)
plot(VA_zipsub)

# Matching and Attaching Warshaw Ideal Points Data 
setwd("/Users/Administrator/Documents/ForParallel")
install.packages('haven')
library(haven)
warszipsfull <- read_dta('WarShawFullNoMiss.dta')
head(warszipsfull)
str(warszipsfull)

## Make sure all is numeric and take out what is not numeric##
class(warszipsfull$zipnum)
class(warszipsfull$respondent)
class(warszipsfull$ideal_point)
class(warszipsfull$ideal_point_se)
warszipsfull$source <- NULL
warszipsfull$zip <- NULL
warszipsfull$notnumeric <- NULL
warszipsfull$count <- 1

## Subset Warshaw based on range of imported VA 2007 file range
wzaggfull1 <-aggregate(warszipsfull, by=list(warszipsfull$zipnum), 
                       FUN=mean, na.rm=TRUE)
wzaggfull2 <- aggregate(. ~ zipnum, warszipsfull, length)
wzaggfull <- merge(wzaggfull1, wzaggfull2, by = "zipnum")
wzaggfull <- subset(wzaggfull, select = -c(Group.1, respondent.x, count.x, respondent.y, ideal_point.y, ideal_point_se.y))

# Ensuring same CRS for zip and precincts
VA_zipsubCRS <- spTransform(VA_zipsub, CRS(proj4string(oofmap))) 

#wzaggfull zips not in the the Virginia Zips
wzaggfull$zipnum[!wzaggfull$zipnum %in% VA_zipsubCRS@data$ZCTA5CE00] <- NA
wzaggfull <- na.omit(wzaggfull)
names(wzaggfull) <- c("ZCTA5CE00", "IdePoAv", "IdePoSE", "Rescount")

# Attaching warshaw ideal_points to VA_zipsubCRS
install.packages('raster')
library(raster)
VA_zipsubCRSmerge <- merge(VA_zipsubCRS, wzaggfull, by='ZCTA5CE00')
VA_zipsubCRSmerge@data

# Merge Precincts and Zipcodes
library(sp)
library(raster)
library(rgdal)
library(rgeos)
install.packages('maptools')
library(maptools)

# This should be performing same thing as spTransform above if I understand correctly. 
projection(oofmap) <- projection(VA_zipsubCRSmerge)

inters <- intersect(VA_zipsubCRSmerge, oofmap)
plot(VA_zipsubCRSmerge, axes=T); plot(oofmap, add=T); plot(inters, add=T, col='red')

inters$area <- area(inters)

mergeddata <- as.data.frame(inters@data)
mergeddata <- mergeddata[!is.na(mergeddata$IdePoAv),]

mergedistrict1 <-aggregate(mergeddata, by=list(mergeddata$cluster), 
                           FUN=mean, na.rm=TRUE)
mergedistrict2 <- aggregate(. ~ cluster, mergeddata, length)
mergedistrict3 <- aggregate(. ~ cluster, mergeddata, sum)
mergedistrict4 <- merge(mergedistrict1, mergedistrict2, by = "cluster")
mergefull <- merge(mergedistrict4, mergedistrict3, by = "cluster")
mergefull <- subset(mergefull, select = c(cluster, IdePoAv.x, IdePoSE.x, ZCTA5CE00.y, Rescount))

IdePoAvfu <- mean(mergefull$IdePoAv.x)
IdePoAvse <- mean(mergefull$IdePoSE.x)
Respperzipav <- mean(mergefull$Rescount)
Zipperdistav <- mean(mergefull$ZCTA5CE00.y)
Output <- cbind(IdePoAvfu, IdePoAvse, Respperzipav, Zipperdistav)
print(Output)
# Mean ideal preference for map is .019, which is just a bit over what we found the first time, but also fairly central in terms of policy preference. 

# Figure out how to get number of zips and respondants in each. 

##### Conducting the ideal preferences analysis for all maps #####

#################### For lapply method. 
setwd("/Users/Administrator/Documents/ForParallel/output_maps_2")
testfiles <- list.files(path = "/Users/Administrator/Documents/ForParallel/output_maps_2", pattern="*.geojson", full.names = TRUE)


idepo <- function(filename) {
  precinctmaps1 <- readOGR(dsn = filename, layer = ogrListLayers(filename))
  projection(precinctmaps1) <- projection(VA_zipsubCRSmerge)
  inters1 <- raster::intersect(VA_zipsubCRSmerge, precinctmaps1)
  
  inters1$area <- area(inters1)
  
  mergeddata1 <- as.data.frame(inters1@data)
  mergeddata1 <- mergeddata1[!is.na(mergeddata1$IdePoAv),]
  
  mergedistrict11 <-aggregate(mergeddata1, by=list(mergeddata1$cluster), 
                              FUN=mean, na.rm=TRUE)
  mergedistrict21 <- aggregate(. ~ cluster, mergeddata1, length)
  mergedistrict31 <- aggregate(. ~ cluster, mergeddata1, sum)
  mergedistrict41 <- merge(mergedistrict11, mergedistrict21, by = "cluster")
  mergefull1 <- merge(mergedistrict41, mergedistrict31, by = "cluster")
  mergefull1 <- subset(mergefull1, select = c(cluster, IdePoAv.x, IdePoSE.x, ZCTA5CE00.y, Rescount))
  
  IdePoAvfu1 <- mean(mergefull1$IdePoAv.x)
  IdePoAvse1 <- mean(mergefull1$IdePoSE.x)
  Respperzipav1 <- mean(mergefull1$Rescount)
  Zipperdistav1 <- mean(mergefull1$ZCTA5CE00.y)
  Output1 <- cbind(IdePoAvfu1, IdePoAvse1,  Respperzipav1, Zipperdistav1)
  print(Output1)
}

clusterExport(cl, list("idepo", "projection<-", "VA_zipsubCRSmerge", "area"))
data <- parLapply(cl, testfiles, idepo)
data1 <- data
data2 <- cbind(data1[1:234])
# Will need to figure out from here how to turn this into something useful. 
data3 <- as.data.frame(data)
data3$V1 <- as.numeric(data3$V1)
summary(data3$V1)

stopCluster(cl)

