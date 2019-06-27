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
library(scales)

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
# Collapse first.
names (zillow)[2] <- "postal_code"
yelp_review_geomatch$date <- as.Date(yelp_review_geomatch$date)
yelp_review_geomatch$Month <- match(months(yelp_review_geomatch$date), month.name)
yelp_review_geomatch$Year <- format(yelp_review_geomatch$date,format="%Y")
yelp_review_geomatch$YearMonth <- as.yearmon(paste(yelp_review_geomatch$Year, yelp_review_geomatch$Month ), "%Y %m")
yelp_review_geomatch$stars.x <- as.numeric(yelp_review_geomatch$stars.x)
names(yelp_review_geomatch)

cobined_agg <- aggregate(stars.x ~ postal_code + YearMonth, yelp_review_geomatch, mean, drop = FALSE)

cobined_collapse <- spread(cobined_agg, key = YearMonth, value = stars.x)

# Join back to original data

# Start Again here for the redo!!!!!!!!!!!!!!!!!!! ON 4/23!!! Figure out where to go from here!

# This is the wide form we should be working with. 
combinedData <- inner_join(cobined_collapse, zillow, by='postal_code', match='all')

write.csv(Full_data_wide, "FULLDATASETYZWIDETRUE.csv")


# Drop others to free up space 
rm(yelp_review, yelp_business_ph)

# Review missingness
sapply(combinedData, function(x) sum(is.na(x)))
# Have full data on stars, but it's not a time series, so that's a problem.

# Get average per month on stars in order to match time periods of housing. 
#  Convert to date if not already

# The goal of the below is to end up with variables for each year-month combo for both the Zillow Data (A column with all the avg. prices by zipcode in April 2013 for example) and the Yelp data avgs. (A column with the avg. stars given by a reviewer for each unique business id in April 2013 for example). 

# Full Try - IT HAS WORKED NOW AND I HAVE NO IDEA HOW!!!!!
# Subset first to match up zillow and yelp months. 
combsub <- subset(combinedData, date>="2010-11-01" & date<="2018-02-28")
# Get avg. of stars for each business per monthyear (in current dataset, there may be some businesses without reviews for month periods and hence the NaNs)
# Eliminate duplicates to get Zillow price and other info by business_id (Rest doesn't matter here). Use this to merge to create both wide and long

# Convert to wide form to have columns for both yelp and zillow yearmonths
cobined_collapse1 <- spread(cobined_agg, key = YearMonth, value = stars.x)
# Join back to original data
Full_data_wide <- inner_join(cobined_collapse1, combsubunique, by='postal_code')
names(Full_data_wide)

# Eliminate columns not needed in analysis 
Full_data_wide  <- Full_data_wide[ -c(90, 92:95, 287:288)]
names(Full_data_wide)

# Convert wide to long
Full_data_add <- reshape(Full_data_wide, varying = list(names(Full_data_wide[2:87])), times = names(Full_data_wide[2:87]), idvar = 'business_id', v.names = 'stars' , direction = 'long')
Full_data_add2 <- reshape(Full_data_wide, varying = list(names(Full_data_wide[196:281])), times = names(Full_data_wide[196:281]), idvar = 'business_id', v.names = 'rentprice' , direction = 'long')

Full_data_long <- cbind(Full_data_add[c(1:109, 198:200)], Full_data_add2[200])

Full_data_long <- Full_data_long[-111]

length(unique(Full_data_long$YearMonth))
write.csv(Full_data_wide, "FULLDATASETYZWIDE.csv")
write.csv(Full_data_long, "FULLDATASETYZLONG.csv")

############Alternatives that might have to be used with bigger data ########
# IF THERE IS PROBLEMS DOING ANY OF THE ABOVE BECAUSE 
# Loop method must be used here to go through each date, aggregate, and then convert to wide 

yearmonthu <- unique(unlist(combinedData$YearMonth))

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
# Chen
# If sales price lag is 0.98, then you won't get that much signal from your contemporaneous business variables

# Do current period price as a function of previous period Yelp
  # Lag the independent Yelp variable 
  # Try a lag with the dependent variable 
  # Do both

# Can also split and run different models with home prices and rental prices 
# Out of sample errors
# Maybe just produce one step ahead. 
# With 300, build on 200, predict on 1st, and so on. 
# Sliding window

# ARIMA vs. Holt-Winter?

# Facebook Prophet - Bayesian Time Series Forecasting (API)

# Small Literature Review for Approach To Take 
# https://www.cct.lsu.edu/~pkondi1/bare_jrnl
  # ARCH and GARCH Models
  # Assumes errors get wider and smaller so is good for cases with a lot of variance
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
  # They just use a normal regression with lags and fixed effects and cluster standard errors! 
  # Combining data on businesses from Yelp with data on gentrification from the Census, Federal Housing Finance Agency, and Streetscore (an algorithm using Google Streetview), we find that gentrifying neighborhoods tend to have growing numbers of local groceries, cafes, restaurants, and bars, with little evidence of crowd-out of other types of businesses.
# # Our first measure of gentrification is housing price data provided by the Federal Housing Finance Agency (FHFA). This data is an annual repeat sales index for over 18,000 five-digit ZIP codes in the U.S., described in Bogin, Doerner and Larson (2016)
# We start by following Rascoff and Humphries (2015) who link proximity to Starbucks and price growth on Zillow
  # All regressions include a full set of calendar year dummies and cluster standard errors at the ZIP Code level. 

# http://www.hbs.edu/faculty/Publication%20Files/18-022_b618d193-9486-4de3-abc4-232e1baecbeb.pdf
  # Random Walk and Regression with lags
  # To assess the overall predictive power of Yelp, we use a random forest algorithm to predict the growth in CBP establishments. 
    # We leverage random forest algorithms to evaluate whether Yelp measures can provide gains in nowcasting CBP measures before the release of official statistics. We are interested in the ability of Yelp to predict changes in overall CBP establishments and restaurants over and above the prediction power generated by lagged CBP data
  #We start by predicting the change in CBP establishments with the two lags of changes in CBP establishments, as well as ZIP code and year fixed effects. We then work with the residual quantity. 
  # We find that contemporaneous and lagged Yelp data can generate an algorithm that is able to explain 21.4 percent of the variance of residual quantity using an out-of-bag estimate in the training sample, which represents 75 percent of the data. 
  # In a testing sample not used to generate the algorithm, our prediction is able to explain 29.2 percent of the variance of this residual quantity. 


##### Plotting the time series for stars and rent prices 

# Mean Stars Over Time (Looks like there might be too much missingness based on how I've broken it down with Phoenix. Not sure if this is a problem with what I did or a problem with the underlying data itself.)
Full_data_wide_plotstars <- Full_data_wide[2:87]

plot(c(1:86), apply(Full_data_wide_plotstars, 2, mean),  ylim = c(0,5),  xlab = 'Time (YearMonths)', ylab = 'Avg. Stars', main = 'Mean Stars per Month', type = 'n')
abline(v = axTicks(1), h = axTicks(2), col = rgb(0.75, 0.75, 0.75, alpha = 0.5), lty = 3)
lines(c(1:86), apply(Full_data_wide_plotstars, 2, mean), type = 'b', pch = 1, lty = 2)

# Time Series of Avg. Rent Prices by month
Full_data_wide_plotrent <- Full_data_wide[195:280]

plot(c(1:86), apply(Full_data_wide_plotrent, 2, mean),  ylim = c(1100,1500),  xlab = 'Time (YearMonths)', ylab = 'Avg. Rent', main = 'Mean Rent per Month', type = 'n')
abline(v = axTicks(1), h = axTicks(2), col = rgb(0.75, 0.75, 0.75, alpha = 0.5), lty = 3)
lines(c(1:86), apply(Full_data_wide_plotrent, 2, mean), type = 'b', pch = 1, lty = 2)

## Performing regression analysis on a series - http://yunus.hacettepe.edu.tr/~iozkan/eco665/archgarch.html
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# Create a new zoo object containing Rent Prices of and avg. stars. - Need to do the pasting
tmp <- Full_data_long[c(110,112:113)]
names(Full_data_long)
tmp$YearMonth <- as.Date(strptime(paste(1, tmp$YearMonth),"%d %b %Y"))

tmp1 <- aggregate(stars ~ time, tmp, mean, drop = FALSE)
tmp2 <- aggregate(rentprice ~ time, tmp, mean, drop = FALSE)
tmp3 <- left_join(tmp1, tmp2, by = 'time')

tmp4 <- zoo(tmp3[,-1], order.by=as.Date(strptime(as.character(tmp3[,1]), "%Y-%m-%d")))

tmp.ret <- CalculateReturns(tmp4)
colnames(tmp) <- colnames(tmp.ret) <- c("stars","rentprice")
plot(tmp4, main="Stars and Rent Prices Index", xlab="YearMonth Date")
# Looks like the trends maybe would match up if we had less missingness and a more coprehensive data set. 
plot(tmp.ret, main="Return Series for Stars and Rent Prices Index", xlab="Date")
# Not really sure what this tells us or why it is different from above to be honest. 
plot(coredata(na.omit(tmp.ret)), pch=".", main="Return Series for Stars vs Rent Prices Index")
# Again, here also not sure what we're getting from this. 

# CAN'T GET COEFFICIENT OF REGRESSIONS BELOW TO WORK!!! Is it about the data or what I am using here? Can't get the summary of this or the return series, which uses code similar to what is below. 
rr <- rollapply(na.approx(na.trim(tmp4, side="both")), width = 120,
                FUN = function(z) coef(lm(rentprice ~ tmp3, data = as.data.frame(z))),
                by.column = FALSE, align = "left")

rr.var <- rollapply(na.approx(na.trim(tmp4, side="both")), width = 120,
                    FUN = function(z) sd(residuals(lm(rentprice ~ tmp3[1:2], data = as.data.frame(z)))),
                    by.column = FALSE, align = "left")

res <- merge(rr,rr.var)
colnames(res)[3] <- "St. Dev. Resid." 
plot(res, main="Coefficients of Regs.", xlab="Date")

# Once we have three data objects - all are zoo objects - and it is now easy to perform some preprocessing steps such as missing value tratment, aggregation (changing from high frequency to low frequency), outlier detectin and treatment etc.




# Detecting autocorrelation
# Estimate basic model for stars by time
StarOLS <- lm(stars ~ time, data= Full_data_long)
# Save residuals 
ErrStars <- resid(StarOLS)
# Plot residuals over time
plot (time, ErrStars)
# Generate lagged error variable 
LagErrStars <- c(NA, ErrStars[1:(length(ErrStars) - 1)])
# Auxiliary regression
LagErrOLS <- lm(ErrStars ~ LagErrStars)
summary(LagErrOLS)

# According to the p-value of 0.667, we don't have autocorrelation, right? 

# Estimate basic model for Zillow Prices by time
RentOLS <- lm(rentprice ~ time, data= Full_data_long)
# Save residuals 
ErrRent <- resid(RentOLS)
# Plot residuals over time
plot (time, ErrRent)
# Generate lagged error variable 
LagErrRent <- c(NA, ErrRent[1:(length(ErrRent) - 1)])
# Auxiliary regression
LagErrOLSRent <- lm(ErrRent ~ LagErrRent)
summary(LagErrOLSRent)

# According to the p-value of <2e-16, we have autocorrelation, right? 

# https://msperlin.github.io/pafdR/models.html
# 1. Panel Data Models 
  # The main motivation to use panel data models is to allow common effects within the groups.
# We're essentially assuming different coefficients for each firm
# We can test the model specification using package plm. Function phtest executes the Hausman test (Hausman 1978), a statistical procedure that tests the null hypothesis that the best model is the random effects and not the fixed effect. Let's try it for our data.
# set options for Hausman test

# Go back and manipulate different date format. -> Probably need to figure out how 
Full_data_long$YearMonth <- as.Date(strptime(paste(1, Full_data_long$YearMonth),"%d %b %Y"))
duplong <- Full_data_long[duplicated(c(Full_data_long$business_id, Full_data_long$YearMonth)),]
duplong

my.formula <- rentprice ~ stars
my.index <- c('business_id','YearMonth')

# do Hausman test
my.hausman.test <- phtest(x = my.formula, 
                          data = Full_data_long,
                          model = c('within', 'random'),
                          index = my.index)

# print result
print(my.hausman.test)

# The p-value of < 2.2 e -16 means we can reject the null hypothesis that the most efficient panel data model is the random effects. We have strong statistical evidence that a fixed effects model is better suited than a random effect type for the dataset.

install.packages('plm')
library(plm)
my.pdm <- plm(data = Full_data_long, 
              formula = rentprice ~ stars, 
              model = 'within',
              index = c('business_id','YearMonth'))
summary(my.pdm)

# Firm - business_id ; year - time; inv - rentprice ; value/capital - stars
# Coefficients are significant, but weirdly say that more stars means lower prices. Probably a nature of the sparse data as it is now. 
# R-squared equal to -0.0744. So terrible in essence. 
# Sucks. Do something else. 


# GARCH Model 
# In its simplest format, you have two main equations: a process that sets the conditional mean, and another that defines the variance of the error
library(fGarch)

# Example in looks more like time series (https://msperlin.github.io/pafdR/models.html#panel-data-models) for S&P 500. What if we want to look at the  relationship between continuous variables? Can Garch be used here? 


#### Have not gotten to the below yet. Maybe useful. Maybe not. ############

#### Load in check-in data 
yelp_checkin <- as.data.frame(jsonlite::stream_in(file("dataset/checkin.json")), flatten = TRUE)


yelp_photos <- jsonlite::stream_in(file("dataset/photos.json"))

# Can't load in below directly as is. Need to figure out how to cut on import. 
yelp_review <- jsonlite::stream_in(file("dataset/review.json"))
yelp_tip <- jsonlite::stream_in(file("dataset/tip.json"))
yelp_user <- jsonlite::stream_in(file("dataset/user.json"))


# Next Questions to ask: 
#1. Mean Stars Over Time (Looks like there might be too much missingness based on how I've broken it down with Phoenix. Not sure if this is a problem with what I did or a problem with the underlying data itself. ??
# Multilevel modeling? Is that useful here? 



