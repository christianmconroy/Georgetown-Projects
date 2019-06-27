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
library(tibble)
library(stringr)
library(tidyr)
library(dplyr)
library(Amelia)

## Check contents of tar file
untar("yelp_dataset.tar",list=TRUE) 

############### Business Dataset #################
#### Load in business dataset

yelp_business <- fromJSON(sprintf("[%s]", paste(readLines("dataset/business.json"), collapse=",")), simplifyVector=TRUE, simplifyDataFrame=TRUE, simplifyMatrix=TRUE , flatten=TRUE)
head(yelp_business, 10)

# Original Code
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
# library(plyr)
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

# Save as CSV for convenience of future imports 
# NEED TO FIGURE OUT WHAT TO DO TO EXPORT OUT! (ALREADY FLATTENED SO NOT SURE WHAT ELSE TO DO)
write.csv(yelp_business_to, "yelpbusinessTO.csv")
write.csv(yelp_business, "yelpbusiness.csv")

############### Review Dataset #################
yelp_review <- fromJSON(sprintf("[%s]", paste(readLines("dataset/review.json"), collapse=",")))

review_flatten <- flatten ( review)
names (review_flatten)[3] <- "review_stars"
names (review_flatten)[10] <- "text_reviewed_as_cool"
names (review_flatten)[8] <- "text_reviewed_as_funny"
names (review_flatten)[9] <- "text_reviewed_as_useful"
review <- review_flatten
review <- review [,-6]
user <- fromJSON(sprintf("[%s]", paste(readLines("user.json"), collapse=",")), flatten=TRUE)

#### Load in check-in data 
yelp_checkin <- as.data.frame(jsonlite::stream_in(file("dataset/checkin.json")), flatten = TRUE)


yelp_photos <- jsonlite::stream_in(file("dataset/photos.json"))

# Can't load in below directly as is. Need to figure out how to cut on import. 
yelp_review <- jsonlite::stream_in(file("dataset/business.json"))
yelp_tip <- jsonlite::stream_in(file("dataset/tip.json"))
yelp_user <- jsonlite::stream_in(file("dataset/user.json"))



# Loading in dataseta from untarred Yelp Data ##


# That other data set
unzip("zecon.zip",list=TRUE)  ## check contents
unzip("zecon.zip")