## Final Project Script ##

## Importing in the Data ##
# Set WD where the files are. 
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

############### Business Dataset #################
#### Load in business dataset

yelp_business <- fromJSON(sprintf("[%s]", paste(readLines("dataset/business.json"), collapse=",")), simplifyDataFrame=TRUE, flatten=TRUE)

# Deal with the way NAs imported. 
yelp_business[] <- lapply(yelp_business, str_trim)
is.na(yelp_business) <- yelp_business==''

# Review missingness
sapply(yelp_business, function(x) sum(is.na(x)))

## Descriptive Stats of Business Data ##

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

# Cut data so that we can work with a reasonably sized data set. Using Phoenix because of a better match with Zillow. We'll want to loop through all cities eventually, but the dataset is too big to do all at once. 
yelp_business_ph <- yelp_business[which(yelp_business$city=='Phoenix'),]
phoenix <- unique(yelp_business_ph$business_id)

############### Review Dataset #################
# Load in Dataset in chunks specifying the subset of Phoenix from the start. Otherwise too big. 
# Stream in approach with function
con_out <- file(tmp <- tempfile(), open = "wb")
jsonlite::stream_in(file("dataset/review.json"), handler = function(df) {
  df <- dplyr::filter(df, business_id %in% phoenix)
  #df <- left_join(df, yelp_business_to, by='business_id', match='all')
  stream_out(df, con_out, pagesize = 5000)
}, pagesize = 5000)
close(con_out)

# stream it back in
yelp_review <- stream_in(file(tmp))
write.csv(mydata, "yelpreview_pho.csv")

# Merge business and review datasets
yelp_review_geomatch <- left_join(yelp_review, yelp_business_ph, by='business_id', match='all')
length(unique(yelp_review_geomatch$business_id))
names(yelp_review_geomatch)


############################### Zillow Dataset ############################
# Load in zillow dataset

zillow <- read_csv("zecon/Zip_Zri_AllHomesPlusMultifamily.csv", col_names = TRUE)
head(zillow)

## Assess which city would be best to merge with for initial ##
zillow$City <- zillow$City %>% as.factor
zillow$City %>% summary
#Damn it. Zillow data set super dispersed. We'll have to build model using small data and then loop over to see what we can get out of it. 

###### Combined Data Set ########
# Can't do Toronto because Zillow data doesn't have Toronto here!!!
names (zillow)[2] <- "postal_code"
combinedData <- inner_join(yelp_review_geomatch, zillow, by='postal_code', match='all')
head(combinedData)
combinedData$stars.y <- NULL
names(combinedData)

# Drop others to free up space 
rm(yelp_review, yelp_business_ph)

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
names(combinedData)

# The goal of the below is to end up with variables for each year-month combo for both the Zillow Data (A column with all the avg. prices by zipcode in April 2013 for example) and the Yelp data avgs. (A column with the avg. stars given by a reviewer for each unique business id in April 2013 for example). 

# Full Try - IT HAS WORKED NOW AND I HAVE NO IDEA HOW!!!!!
# Subset first to match up zillow and yelp months. 
combsub <- subset(combinedData, date>="2010-11-01" & date<="2018-02-28")
# Get avg. of stars for each business per monthyear
cobined_agg <- aggregate(stars.x ~ business_id + YearMonth, combsub, mean, drop = FALSE)
# Convert to wide form to have columns for both yelp and zillow yearmonths
cobined_collapse1 <- spread(cobined_agg, key = YearMonth, value = stars.x)
# Join back to original data
cobined_collapse2 <- inner_join(combsub, cobined_collapse1, by='business_id')
cobined_collapse2$`2018-02` <- NULL
cobined_collapse2$`2018-01` <- NULL
names(cobined_collapse2)

write.csv(cobined_collapse2, "FULLDATASETYZ.csv")

### Finish here as of 4/16/18







############Alternatives that might have to be used with bigger data ########
# IF THERE IS PROBLEMS DOING ANY OF THE ABOVE BECAUSE 
# Loop method must be used here to go through each date, aggregate, and then convert to wide 

yearmonthu <- unique(unlist(combinedData$YearMonth))

# Still just Zillow - 2010-11 through 2018-02
test <- subset(combinedData, YearMonth == yearmonthu[4])
names(test)
# Do not have the zillow in this. Just have the vector for avg. stars by business by yearmonth period
test1 <- aggregate(stars.x ~ business_id + YearMonth, test, mean, drop = FALSE)
names(test1)
# Spread First
test2 <- spread(test1, key = YearMonth, value = stars.x)
names(test2)
# So the ones that are "Month Year" are for avg. stars and the ones that are "Year-Month" are for Zillow 
test3 <- inner_join(test, test2, by='business_id')
names(test3)


for (i in 1:length(yearmonthu)) {
  
cobined_sub <- subset(combinedData, YearMonth == yearmonthu[i])

cobined_agg <- aggregate(stars.x ~ business_id + YearMonth, cobined_sub, mean, drop = FALSE)

cobined_collapse1 <- spread(cobined_agg, key = YearMonth, value = stars.x)
  
cobined_collapse2 <- inner_join(cobined_sub, cobined_collapse1, by='business_id')

return(cobined_collapse2)
}

names(cobined_collapse2)

# Function Try 

collapseit <- function(yearmonth) {

cobined_sub <- subset(combinedData, YearMonth == yearmonth[i])

cobined_agg <- aggregate(stars.x ~ business_id + YearMonth, cobined_sub, mean, drop = FALSE)

cobined_collapse1 <- spread(cobined_agg, key = YearMonth, value = stars.x)

cobined_collapse2 <- inner_join(cobined_sub, cobined_collapse1, by='business_id')

print(cobined_collapse2)

}

data1 <- lapply(yearmonthu, collapseit)

#############################################################################

########### Now we can start the actual analysis!!!!!########################

# Small Literature Review for Approach To Take 
# https://www.cct.lsu.edu/~pkondi1/bare_jrnl
  # ARCH and GARCH Models 
      # We model this data for restaurant business using variants of Autoregressive Conditional Heteroskedasticity (ARCH/GARCH) and Vector Autoregressive (VAR) models to infer about the future business trends.
    #Thus the goal of this work is to develop statistical models for monthly review count for a business and use this information for forecasting future business trends  
    # In particular, we model monthly review count about a business as univariate time series of count and also develop a VAR [2] model in order to include time varying features such as average number of stars and reviews identified as cool, funny and useful.
    # The basic version of the least squares model assumes
    # that the expected value of all error terms, when squared,
    # is the same at any given point. This assumption is called
    # homoskedasticity, and it is this assumption that is the focus
    # of ARCH/GARCH [5] models.
    # ARCH and GARCH models treat heteroskedasticity as a variance to be modeled

# https://www.yelpblog.com/2018/02/harvard-economists-yelp-data-can-help-predict-gentrification
  # The number of businesses in a neighborhood, as measured by Yelp, is a predictor of housing price changes. For example, when a cafe opens in a neighborhood, home prices there rise by 0.5% relative to other areas in the coming year.

# https://www.hbs.edu/faculty/Publication%20Files/18-077_a0e9e3c7-eceb-4685-8d72-21e0f518b3f3.pdf
  # Combining data on businesses from Yelp with data on gentrification from the Census, Federal Housing Finance Agency, and Streetscore (an algorithm using Google Streetview), we find that gentrifying neighborhoods tend to have growing numbers of local groceries, cafes, restaurants, and bars, with little evidence of crowd-out of other types of businesses.

## Do some line graphing first! - Can borrow from ALA problem set and problem codes!
# # Something like this: 
# plot <- ggplot(aerospace, aes(x = year, y = female.ratio))
# plot
# 
# plot <- plot + geom_line()
# plot
# 
# plot <- plot + theme_classic()
# plot



#### Have not gotten to the below yet. Maybe useful. Maybe not. ############

#### Load in check-in data 
yelp_checkin <- as.data.frame(jsonlite::stream_in(file("dataset/checkin.json")), flatten = TRUE)


yelp_photos <- jsonlite::stream_in(file("dataset/photos.json"))

# Can't load in below directly as is. Need to figure out how to cut on import. 
yelp_review <- jsonlite::stream_in(file("dataset/review.json"))
yelp_tip <- jsonlite::stream_in(file("dataset/tip.json"))
yelp_user <- jsonlite::stream_in(file("dataset/user.json"))



# Loading in dataseta from untarred Yelp Data ##


