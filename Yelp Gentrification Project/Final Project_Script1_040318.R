## Final Project Script ##

# Questions 
# 1. What is the reason to use a tibble over a dataframe? 
# 2. What can we do about the review file? Seems to be too big. 
# 174567 total observations. Not really anything to use for DC, MD, or VA. Does Yelp have something different to use? 

## Importing in the Data ##
# Yelp Data Untarring #
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/DataScienceIntro")

# Load in required packages 
library(jsonlite)
library(ggplot2)
library(tidyverse)
library(zoo)
library(stringr)
library(tidyr)
library(dplyr)
library(readr)


## Check contents of tar file
untar("yelp_dataset.tar",list=TRUE) 

############### Business Dataset #################
#### Load in business dataset

yelp_business <- fromJSON(sprintf("[%s]", paste(readLines("dataset/business.json"), collapse=",")), simplifyDataFrame=TRUE, flatten=TRUE)

# Deal with the way NAs imported. 
yelp_business[] <- lapply(yelp_business, str_trim)
is.na(yelp_business) <- yelp_business==''

# Review missingness
sapply(yelp_business, function(x) sum(is.na(x)))
# Will have to drop the 623 with missing postal code or use a Google API or something to fill in. Shouldn't be terribly hard to fill in. 

### Use R ggmap package to fill in neighborhood names based on lat/lon
# Subset date frame to missing (to test)

# yelp_business <- as.data.frame(jsonlite::stream_in(file("dataset/business.json")), flatten = TRUE)
# head(yelp_business, 10)


# Assess counts of states
yelp_business$state <- yelp_business$state %>% as.factor
yelp_business$state %>% summary

# Not a great distribution across states. Looks like there's lots in Arizona, Ohio, Ontario, Pennsylvania, North Carolina, and Quebec and Wisconsin to a smaller degree.

# Alternative code for the state level frequencies 
# yelp_business$state <- as.factor(yelp_business$state)
# table(yelp_business$state)

# Assess counts of cities 
yelp_business$city <- yelp_business$city %>% as.factor
yelp_business$city %>% summary

# On the city front, it looks like Las Vegas, Phoenix, and Toronto are the biggest. Then we have smaller in Charlotte, Scottsdale, and Pittsburgh, etc. 
# Question: Should we cut down? Should we eliminate Canada? This is obviously not ALL of Yelp's data. Can we figure out where we'd get all of DMV? 

# Alternative code for the city portion commented here
# yelp_business$city <- as.factor(yelp_business$city)
# library(plyr)    n  n       
# citytab <- count(yelp_business, 'city')
# citytab <- citytab[order(-citytab$freq),]
# head(citytab, 50)

## Assess counts of categories 
yelp_business%>%select(-starts_with("hours"), -starts_with("attribute")) %>% unnest(categories) %>%
  select(name, categories)%>%group_by(categories)%>%summarise(n=n())%>%arrange(desc(n))%>%head(20)

# Restaurants are top category (54618), but we might want to consider others. 
## Assess counts of categories to account for closures. 
yelp_business %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(is_open==1) %>%
  unnest(categories) %>%
  select(name, categories)%>%group_by(categories)%>%summarise(n=n())%>%arrange(desc(n))%>%head(20)

# Rankings essentially the same, but now at 40394. 

## Dying business proportion by city 
yelp_business %>%select(-starts_with("hours"), -starts_with("attribute"))%>%
  unnest(categories) %>%group_by(city)%>%summarise(n=n(), dead_prop=sum(is_open==0)/n,live_prop=sum(is_open==1)/n)%>%filter(n>100)%>%arrange(desc(dead_prop),desc(n))

# Toronto also pretty high here (#9), so could be interesting. 

# So if we want the above just for Toronto, we can do this: 
yelp_business %>%select(-starts_with("hours"), -starts_with("attribute"))%>%
  unnest(categories) %>%group_by(city,categories)%>%summarise(n=n(), dead_prop=sum(is_open==0)/n,live_prop=sum(is_open==1)/n)%>%filter(n>100)%>%arrange(desc(dead_prop),desc(n))%>%filter(city=="Toronto")%>%head(20)

# Haha. No one likes French food. 

## Comparing the star ratings of Toronto, Las Vegas, and Toronto
yelp_business %>%select(-starts_with("hours"), -starts_with("attribute"))%>%filter(review_count>30)%>%
  unnest(categories)%>%
  filter(city=="Cleveland"|city=="Toronto"|city=="Las Vegas")%>%filter( categories=="Restaurants")%>%ggplot(aes(x=stars,fill=city))+geom_histogram(position="dodge",binwidth = 0.25)+ggtitle("Number of Restaurants with different star ratings in several cities.")

# Las Vegas kicks ass. Toronto comes close between 3 and 4. Cleveland just sucks at everything. 

# Look at what types of restaurants we have and which are most popular by state
yelp_business %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants") %>%
  count(state, categories) %>%
  filter(n > 10) %>%
  group_by(state) %>%
  top_n(1, n)

# Use the below code to cut (Toronto for this example)
yelp_business_to <- yelp_business[which(yelp_business$city=='Toronto'),]
toronto <- unique(yelp_business_to$business_id)

# Save as CSV for convenience of future imports 
# NEED TO FIGURE OUT WHAT TO DO TO EXPORT OUT! (ALREADY FLATTENED SO NOT SURE WHAT ELSE TO DO)
write.csv(yelp_business_to, "yelpbusinessTO.csv")
write.csv(yelp_business, "yelpbusiness.csv")

############### Review Dataset #################
# Stream in approach with function
con_out <- file(tmp <- tempfile(), open = "wb")
jsonlite::stream_in(file("dataset/review.json"), handler = function(df) {
  df <- dplyr::filter(df, business_id %in% toronto)
  #df <- left_join(df, yelp_business_to, by='business_id', match='all')
  stream_out(df, con_out, pagesize = 1000)
}, pagesize = 5000)
# close(con_out)

# stream it back in
mydata <- stream_in(file(tmp))
yelp_review <- mydata
write.csv(mydata, "yelpreview.csv")

yelp_review_geomatch <- left_join(yelp_review, yelp_business, by='business_id', match='all')
length(unique(yelp_review_geomatch$business_id))
names(yelp_review_geomatch)

# Here is how I can load in subset of data based on a specified quantity. What I need to figure out is how to load in based on a specific condition. The challenge is that the condition is based on another dataset (I want only those business_ids that correspond to zipcodes and/or cities in the yelp_business dataset)
# x<-readLines("dataset/review.json", n=100000)
# out <- lapply(x, fromJSON)
# yelp_review <- data.frame(matrix(unlist(out), nrow=100000, byrow=T))
# names(yelp_review) <- c("review_id", "user_id", "business_id", "stars", "date", "test", "useful", "funny", "cool")
# length(unique(yelp_review$business_id))
# # The unique levels are less than the total observations, which is good. So in any merge, we should have multiple repeats of business ids for each line of review. 



# # Chunks approach #
# f <- function(x, pos) return(x)
# 
# yelp_review <- fromJSON(sprintf("[%s]", paste(read_lines_chunked("dataset/review.json", callback=DataFrameCallback$new(f), chunk_size = 10000), collapse=",")), simplifyVector=FALSE, simplifyDataFrame=TRUE, simplifyMatrix=TRUE , flatten=TRUE)
# # simplyVector = FALSE supposedly to help it go much faster. N
# # Am I not chunking right? I get the error below: 
# # Error in paste(read_lines_chunked("dataset/review.json", callback = DataFrameCallback$new(f),  : 
#                                     result would exceed 2^31-1 bytes
#                                   In addition: Warning message:
#                                     In (function (..., deparse.level = 1)  :
#                                           number of columns of result is not a multiple of vector length (arg 527)
                                        
# stream_in approach # 
# yelp_review <- jsonlite::stream_in(file("dataset/review.json"))
# Because parsing huge JSON strings is difficult and inefficient, JSON streaming is done using lines of minified JSON records, a.k.a. ndjson



############################### Zillow Dataset ############################
# That other data set
unzip("zecon.zip",list=TRUE)  ## check contents

zillow <- read_csv("zecon/Zip_Zri_AllHomesPlusMultifamily.csv", col_names = TRUE)
head(zillow)

## Assess which city would be best to merge with for initial ##
zillow$City <- zillow$City %>% as.factor
zillow$City %>% summary
#Damn it. Zillow data set super dispersed. We'll have to build model using small data and then loop over to see what we can get out of it. 

###### Combined Data Set ########
# Can't do Toronto because Zillow data doesn't have Toronto here!!!
library(plyr)
names (zillow)[2] <- "postal_code"
combinedData <- inner_join(yelp_review_geomatch, zillow, by='postal_code', match='all')
head(combinedData)
combinedData$stars.y <- NULL

# Review missingness
sapply(combinedData, function(x) sum(is.na(x)))
# Have full data on stars, but it's not a time series, so that's a problem.

# Get average per month on stars in order to match time periods of housing. 
#  Convert to date if not already
combinedData$date <- as.Date(combinedData$date)
combinedData$Month <- match(months(combinedData$date), month.name)
combinedData$Year <- format(combinedData$date,format="%Y")
combinedData$YearMonth <- as.yearmon(paste(combinedData$Year, combinedData$Month ), "%Y %m")
combinedData$stars.x <- as.numeric(combinedData$stars.x)

# Next question is how do we get the below to create new columns for each month-year combo with the average stars for each business_id (rows) for the respective time period? 

rolledAvg <-
  combinedData %>%
  group_by(business_id)
  group_by(YearMonth) %>%
  summarise(mean_stars = mean(stars.x)) %>%
  ungroup() %>%
  arrange(YearMonth)

combinedData_stars <- left_join(combinedData, rolledAvg)
head(combinedData_stars, 10)

avstars <- as.data.frame(aggregate(stars.x ~ Month + Year, combinedData, mean))
avstars$Date <- with(avstars, sprintf("%s-%02s", Year, Month))

#  Get months
df1$Month <- months(df1$X1)

#### Load in check-in data 
yelp_checkin <- as.data.frame(jsonlite::stream_in(file("dataset/checkin.json")), flatten = TRUE)


yelp_photos <- jsonlite::stream_in(file("dataset/photos.json"))

# Can't load in below directly as is. Need to figure out how to cut on import. 
yelp_review <- jsonlite::stream_in(file("dataset/review.json"))
yelp_tip <- jsonlite::stream_in(file("dataset/tip.json"))
yelp_user <- jsonlite::stream_in(file("dataset/user.json"))



# Loading in dataseta from untarred Yelp Data ##


