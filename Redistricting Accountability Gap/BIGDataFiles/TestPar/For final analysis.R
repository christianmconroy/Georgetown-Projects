#### Aggregate Analysis of All GEOJSON FILES #### 
setwd("/home/ec2-user/ForParallel")
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

##### Geospatial Analysis #####
setwd("/home/ec2-user/ForParallel")
## Subsetting Zipcode Data to Just VA (No looping needed)
# Importing VA state Boundary, US Zip Codes, and random maps
library(rgdal)
library(rgeos)

USzips <- readOGR(".", "fe_2007_us_zcta500")
VABoundary <- readOGR(".", "VAState")
oofmap <- readOGR(".", "00f76")

# Ensuring the two layers use the same CRS. 
USzips <- spTransform(USzips, CRS(proj4string(VABoundary))) 


# Subsetting/Clipping to make it just VA Zipcodes 
VA_zipsub <- USzips[VABoundary,]


# Matching and Attaching Warshaw Ideal Points Data 
setwd("/home/ec2-user/ForParallel")
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
setwd("/home/ec2-user/ForParallel")
testfiles <- list.files(path = "/home/ec2-user/ForParallel/output_maps_2", pattern="*.geojson", full.names = TRUE)


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
