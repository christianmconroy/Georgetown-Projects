#### Aggregate Analysis of All GEOJSON FILES #### 
require(rgdal)
VA_zipCRSmerge <- readOGR(".", 'VAzipsubCRSmerge')
library(foreach)
library(doParallel)
install.packages('raster')
#Clusters
n <- detectCores()
cl <- makeCluster(detectCores())
#clusterExport(cl, c(VA_zipCRSmerge), globalenv())
registerDoParallel(cl)

#All files -- this needs to be on the same machine 
#Also, if each iteration requires a different set of files, 
#testfiles should be a data frame where each row is a list of all the files that are needed, 
#then in the loop, each iteration should be a row number and within the loop code
#you would call upon each row-column combination



testfiles <- setNames(as.data.frame(list.files(path = "~/output_maps_2", pattern="*.geojson", full.names = TRUE)), c('Files'))
vafiles <- setNames(as.data.frame(list.files(path = "~/output_maps_2", pattern="*.shp", full.names = TRUE)), c("Files"))
vafiles1 <- setNames(as.data.frame(list.files(path = "~/output_maps_2", pattern="*.dbf", full.names = TRUE)), c("Files"))
vafiles2 <- setNames(as.data.frame(list.files(path = "~/output_maps_2", pattern="*.shx", full.names = TRUE)), c("Files"))
vafiles1 <- as.data.frame(vafiles1[-1,])
names(vafiles1) <- c("Files")
testfiles <- rbind(testfiles, vafiles, vafiles1, vafiles2)

#For testing, keep it to 10 to see if it works. If it does work, replace with testfiles in the foreach
codes <- as.character(testfiles[c(1:237), 1])

example <- foreach(i = codes, .combine = rbind, .export = "VA_zipCRSmerge") %dopar% {
  require(sp)
  require(rgdal)
  require(rgeos)
  require(raster)
  
  #Put any other libraries here, you don't put all libraries in here, it won't work
  VA_zipCRSmerge <- readOGR(".", 'VAzipsubCRSmerge')
  #Any custom function needs to be called, almost like loading a library
  
  idepo <- function(filename) {
    precinctmaps1 <- readOGR(dsn = filename, layer = ogrListLayers(filename))
    projection(precinctmaps1) <- projection(VA_zipCRSmerge)
    inters1 <- intersect(VA_zipCRSmerge, precinctmaps1)
    inters1$area <- area(inters1)
    
    mergeddata1 <- as.data.frame(inters1@data)
    mergeddata1 <- mergeddata1[!is.na(mergeddata1$IdePoAv),]
    
    mergedistrict11 <-aggregate(mergeddata1, by=list(mergeddata$cluster), 
                                FUN=mean, na.rm=TRUE)
    mergedistrict21 <- aggregate(. ~ cluster, mergeddata, length)
    mergedistrict31 <- aggregate(. ~ cluster, mergeddata, sum)
    mergedistrict41 <- merge(mergedistrict11, mergedistrict21, by = "cluster")
    mergefull1 <- merge(mergedistrict41, mergedistrict31, by = "cluster")
    mergefull1 <- subset(mergefull1, select = c(cluster, IdePoAv.x, IdePoSE.x, ZCTA5CE00.y, Rescount))
    
    IdePoAvfu1 <- mean(mergefull1$IdePoAv.x)
    IdePoAvse1 <- mean(mergefull1$IdePoSE.x)
    Respperzipav1 <- mean(mergefull1$Rescount)
    Zipperdistav1 <- mean(mergefull1$ZCTA5CE00.y)
    
    
    Output1 <- data.frame(IdePoAvfu1, IdePoAvse1,  Respperzipav1, Zipperdistav1)
    return(Output1)
  }
  
  #Assuming your output is directly from idepo(i), then you're good
  return(idepo(i))
}

