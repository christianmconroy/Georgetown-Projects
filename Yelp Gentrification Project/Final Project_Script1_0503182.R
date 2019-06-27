## Final Project Script ##

##################### Setting Up ########################
# Set WD where the files are. 
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/DataScienceIntro")

# Load in required packages 
library(zoo)
library(readr)
library(tidyr)
library(dplyr)
library(Amelia)
library(mice)
library(lattice)


################33## Importing in the Data ##################
# Load in Yelp Checkin Data 
yelp_checkin <- as.data.frame(jsonlite::stream_in(file("dataset/checkin.json")), flatten = TRUE)

renquote <- function(l) if (is.list(l)) lapply(l, renquote) else enquote(l)

yelp_checkin_flat <- as.data.frame(lapply(unlist(renquote(yelp_checkin)), eval))
names(yelp_checkin_flat)

# How to reshape? 
yelp_checkin_flat_long <- reshape(yelp_checkin_flat, varying = list(names(yelp_checkin_flat[1:168])), times = names(yelp_checkin_flat[1:168]), idvar = 'business_id', v.names = 'checkin' , direction = 'long')
names(yelp_checkin_flat_long)
yelp_checkin_flat_long$time <- str_replace(yelp_checkin_flat_long$time, "time.", "")
yelp_checkin_flat_long$time <- gsub('[[:digit:]]+', '', yelp_checkin_flat_long$time)

yelp_checkin_flat_long$time = substr(yelp_checkin_flat_long$time,1,nchar(yelp_checkin_flat_long$time)-2)
names(yelp_checkin_flat_long)

# Collapse to get checkins per business by each day of the week. 
yelp_checkin_collapse_mean <- as.data.frame(aggregate(checkin ~ business_id + time, yelp_checkin_flat_long , mean))
yelp_checkin_collapse_sum <- as.data.frame(aggregate(checkin ~ business_id + time, yelp_checkin_flat_long , sum))

# Convert long to wide
yelp_checkin_wide_mean <- spread(yelp_checkin_collapse_mean, key = time, value = checkin)
yelp_checkin_wide_sum <- spread(yelp_checkin_collapse_sum, key = time, value = checkin)

# Merge
yelp_checkin_wide <- inner_join(yelp_checkin_wide_mean, yelp_checkin_wide_sum, by='business_id', match='all')
colnames(yelp_checkin_wide) <- c("business_id", "Friday_ave", "Monday_ave", "Saturday_ave", "Sunday_ave", "Thursday_ave", "Tuesday_ave", "Wednesday_ave", "Friday_total", "Monday_total", "Saturday_total", "Sunday_total", "Thursday_total", "Tuesday_total", "Wednesday_total")

write.csv(yelp_checkin_wide, "yelp_checkin_wide.csv")

# Load back in business dataset to merge and aggregate by zipcode
yelp_business <- fromJSON(sprintf("[%s]", paste(readLines("dataset/business.json"), collapse=",")), simplifyDataFrame=TRUE, flatten=TRUE)

checkinbiz <- inner_join(yelp_business, yelp_checkin_wide, by=c('business_id'), match='all')
# Eliminate useless columns
checkinbiz <- checkinbiz[-c(2:6, 8:101)]


checkinzipmean <- as.data.frame(aggregate(. ~ postal_code, checkinbiz[2:9], mean))
checkinzipsum <- as.data.frame(aggregate(. ~ postal_code, checkinbiz[c(2, 10:16)], sum))

checkinfull <- inner_join(checkinzipmean, checkinzipsum, by=c('postal_code'), match='all')


############### Import Yelp Data 
yelp_review_long <- read.csv("yelplongPC_updated2.csv", header = T, na.strings=c("NA"))
sapply(yelp_review_long, function(x) sum(is.na(x)))
# No missing postal codes
length(unique(yelp_review_long$postal_code))
# 15890 unique postal codes 
yelp_review_long$YearMonth <- as.yearmon(yelp_review_long$YearMonth)

# Expand Grid for Yelp Data by creating a backbone
dates <- setNames((seq(as.Date("2010-11-01"), as.Date("2017-12-31"), by= "month")), c("YearMonth"))
dates1 <- match(months(dates), month.name)
dates2 <- format(dates,format="%Y")
dates <- as.yearmon(paste(dates2, dates1), "%Y %m")
upc <- unique(yelp_review_long$postal_code)
yelp_review_long_eg <- expand.grid(YearMonth = dates, postal_code = upc)

# Merge to have all unique combos of zip codes and yearmonths
yelp_review_long_eg <- merge(yelp_review_long, yelp_review_long_eg, by= c("YearMonth", "postal_code"), all.x = TRUE, all.y = TRUE)
names(yelp_review_long_eg)
length(unique(yelp_review_long_eg$postal_code))

# Review Missingness 
sapply(yelp_review_long_eg, function(x) sum(is.na(x)))
# There is significant missingness for stars, which must mean that a number of zipcode just didn't have any ratings for particular yearmonth combos. 

############# Import Zillow Data 
# Load in Zillow Data 
zillow <- read_csv("zecon/Zip_Zri_AllHomesPlusMultifamily.csv", col_names = TRUE)
# Convert zillow to long
names (zillow)[2] <- "postal_code"
zillow <- as.data.frame(zillow)


sapply(zillow, function(x) sum(is.na(x)))


# Convert Zillow to Long in Order to Merge with Yelp
zillow_long <- reshape(zillow, varying = list(names(zillow[8:95])), times = names(zillow[8:95]), idvar = 'postal_code', v.names = 'rentprice' , direction = 'long')

sapply(zillow_long, function(x) sum(is.na(x)))
# Some missingness (28354)

# Change date format of zillow long 
zillow_long$time<- as.Date(strptime(paste(1, zillow_long$time),"%d %Y-%m"))

# Cut to meet length of Yelp Data 
zillow_long <- zillow_long[zillow_long$time >= "2010-11-01" & zillow_long$time <= "2017-12-31",]

zillow_long$month <- match(months(zillow_long$time), month.name)
zillow_long$year <- format(zillow_long$time,format="%Y")
zillow_long$YearMonth <- as.yearmon(paste(zillow_long$year, zillow_long$month), "%Y %m")
names(zillow_long)

# Review Missingness 
sapply(zillow_long, function(x) sum(is.na(x)))

# Expand Grid for Zillow 
upc2 <- unique(zillow_long$postal_code)
zillow_long_eg <- expand.grid(YearMonth = dates, postal_code = upc2)
zillow_long_eg <- merge(zillow_long, zillow_long_eg, by= c("YearMonth", "postal_code"), all.x = TRUE, all.y = TRUE)
names(yelp_review_long_eg)
length(unique(zillow_long_eg$postal_code))

############### Merge Checkin with Yelp 
# Expanded
yelp_review_long_eg <- left_join(yelp_review_long_eg, checkinfull, by=c('postal_code'), match='all')

# Nonexpanded
yelp_review_long <- left_join(yelp_review_long, checkinfull, by=c('postal_code'), match='all')

############### Merge Zillow with Yelp
# Expanded
Full_data_long_ex <- inner_join(yelp_review_long_eg, zillow_long_eg, by=c('postal_code', 'YearMonth'), match='all')
names(Full_data_long_ex)
length(unique(Full_data_long_ex$postal_code))
sapply(Full_data_long_ex, function(x) sum(is.na(x)))

# Non-Expanded
Full_data_long <- inner_join(yelp_review_long, zillow_long, by=c('postal_code', 'YearMonth'), match='all')
length(unique(Full_data_long$postal_code))
sapply(Full_data_long, function(x) sum(is.na(x)))


# Save so that we never have to do the above again. 
write.csv(Full_data_long, "OfficialLongWithCheck.csv")
write.csv(Full_data_long_ex, "OfficialGrigCombinedUpdated.csv")

########### Now we can start the actual analysis!!!!!###############
# https://msperlin.github.io/pafdR/models.html
# 1. Panel Data Models 
  # The main motivation to use panel data models is to allow common effects within the groups.
# We're essentially assuming different coefficients for each firm
# We can test the model specification using package plm. Function phtest executes the Hausman test (Hausman 1978), a statistical procedure that tests the null hypothesis that the best model is the random effects and not the fixed effect. Let's try it for our data.

names(Full_data_long)

# How many lags should we do. Confusing. 
pacf(Full_data_long$rentprice, na.action = na.pass)
pacf(Full_data_long$starsav, na.action = na.pass)

############ Panel Data Model Using Machine Learning Approach
library(plm)

##### Normal Dataset

# Create train and test set using the last 12 months (1 year) for the test set 
table(as.factor(Full_data_long$time))
Full_data_long_train <- Full_data_long[Full_data_long$time < "2017-01-01",]
Full_data_long_test <- Full_data_long[Full_data_long$time >= "2017-01-01",]

# Build the mixed effects model (Can I do this before the RF or do I have to do it after somehow?)
# Hausman Test
# set options for Hausman test
names(Full_data_long)
my.formula <- rentprice ~ starsav + is_openave + funnyav + coolav + usefulav
# Had to take Friday_ave + Monday_ave + Saturday_ave + Sunday_ave + Tuesday_ave + Wednesday_ave out because of the NAs.   
# Had too take standard deviation out because of all the NAs. What to do? Can impute those too? 
# Could try and fix with Amelia, Mice, or maybe even hot deck? 

my.index <- c('postal_code','time')
names(Full_data_long)
# Conduct Hausman Test
my.hausman.test.train <- phtest(x = my.formula, 
                          data = Full_data_long_train,
                          model = c('within', 'random'),
                          index = my.index)
                  
# print result
print(my.hausman.test.train)

# High p-value means random is the way to go here. 

# Built random effects model on train
my.pdm.train <- plm(data = Full_data_long_train, 
              formula = my.formula, 
              model = 'random',
              index = my.index)
summary(my.pdm.train)

# Stars not significant here, which is interesting. (Marginal)

# Predict 
# We need to figure out how to predict here better. I think we need to do getfe. 
Full_data_long_test$pred.plm.test <- predict(my.pdm.train, Full_data_long_test, type='response')

plmmape <- 100*mean(abs(Full_data_long_test$pred.plm.test/Full_data_long_test$rentprice-1), na.rm = T)
print(plmmape)

# MAPE is only 21.39% right now (Capture variance. Might need to go back to google cloud)

##### Imputed Dataset. [CAN;T GET TO WORK!]
# Multiple Imputation for Missing Values Using the Amelia Package - It uses bootstrapping and Expectation-Maximization algorithm, to impute the missing values in a data set. 

############ AMELIA!
sapply(Full_data_long, function(x) sum(is.na(x)))
Imputed_Full_data_long <-amelia(Full_data_long,ts= 'time', cs= 'postal_code', p2s=0, intercs = FALSE, idvars=c('City', 'State', 'Metro', 'CountyName', 'year', 'month', 'YearMonth'))

# Do we need polytime or setting the power of polynomial for time effects (integer between 0 and 3)

write.amelia(obj=Imputed_Full_data_long, file.stem="imputedfull")

data1 <- read.csv("imputedfull1.csv")
data2 <- read.csv("imputedfull2.csv")
data3 <- read.csv("imputedfull3.csv")
data4 <- read.csv("imputedfull4.csv")
data5 <- read.csv("imputedfull5.csv")

data1 <- pdata.frame(data1, index = c("postal_code", "time"))
data2 <- pdata.frame(data2, index = c("postal_code", "time"))
data3 <- pdata.frame(data3, index = c("postal_code", "time"))
data4 <- pdata.frame(data4, index = c("postal_code", "time"))
data5 <- pdata.frame(data5, index = c("postal_code", "time"))
View(data5)
allimp <- imputationList(list(data1,data2,data3,data4,data5))
names(data5)

# Create train and test set using the last 12 months (1 year) for the test set 
data5$time <- as.Date(data5$time, "%Y-%m-%d")
data5_train <- data5[data5$time < "2017-01-01",]
data5_test <- data5[data5$time >= "2017-01-01",]

# Build the mixed effects model (Can I do this before the RF or do I have to do it after somehow?)
# Hausman Test
# set options for Hausman test
my.formula3 <- rentprice ~ starsav + starssd + is_openave + funnyav + coolav + usefulav + Friday_ave + Monday_ave + Saturday_ave + Sunday_ave + Thursday_ave + Tuesday_ave + Wednesday_ave + Friday_total + Monday_total + Saturday_total + Sunday_total + Thursday_total + Tuesday_total + Wednesday_total
# Had too take standard deviation out because of all the NAs. What to do? Can impute those too?  

my.index <- c('postal_code','time')

# Conduct Hausman Test
my.hausman.test.train3 <- phtest(x = my.formula3, 
                                data = data5_train,
                                model = c('within', 'random'),
                                index = my.index)
# print result
print(my.hausman.test.train3)

# Low p-value, but we'll go with random. 

# Built random effects model on train
my.pdm.train3 <- plm(data = data5_train, 
                    formula = my.formula3, 
                    model = 'random',
                    index = my.index)
summary(my.pdm.train3)

# Stars not significant here, which is interesting. (Marginal)

# Predict 
# We need to figure out how to predict here better. I think we need to do getfe. 
data5_test$pred.plm.test <- predict(my.pdm.train3, data5_test, type='response')

plmmape_impute <- 100*mean(abs(data5_test$pred.plm.test/data5_test$rentprice-1), na.rm = T)
print(plmmape_impute)

# Imputation gives us 21.23

################## Panel Data Model with Lags for both (Mixed effects with autocorrelation is what we want the mmost, right?)
  # Random forest is definitely not useful with one variable though. # The point of using rando forest would be to test the importance of each explanatory variable though right? 

# I think plm is the base model and everything else is essentially an add on (GARCH/Arch based on the autocorrelation and heteroskedasticity and RF based on having a lot of variables to choose from)

###### Lagging dependent - [CAN'T GET TO WORK!]
Full_data_long$log_rentprice <- log(Full_data_long$rentprice)
summary(Full_data_long$log_rentprice)

Full_data_long$rentprice_growth <- ave(Full_data_long$log_rentprice, Full_data_long$postal_code, FUN=function(x) c(0, diff(x)))

lg <- function(x)c(NA, x[1:(length(x)-1)])
Full_data_long$rentpriceL1 <-unlist(tapply(Full_data_long$rentprice_growth, Full_data_long$postal_code, lg))

Full_data_long$StarsLag <- lag(Full_data_long$starav)
Full_data_long$RentLag <- lag(Full_data_long$rentprice)

# # Lagged Stars
# Full_data_long$LagStars <- c(NA, Full_data_long$stars.x[1:(N-1)])
# # Lagged time
# Full_data_long$LagTime <- c(NA, Full_data_long$time[1:(N-1)])

# Train/Test
Full_data_long_train2 <- Full_data_long[Full_data_long$time < "2017-01-01",]
Full_data_long_test2 <- Full_data_long[Full_data_long$time >= "2017-01-01",]

# set options for Hausman test
my.formula2 <- RentLag ~ StarsLag
my.index2 <- c('postal_code','time')

# do Hausman test
my.hausman.test2 <- phtest(x = my.formula2, 
                          data = Full_data_long_train2,
                          model = c('within', 'random'),
                          index = my.index2)

# print result
print(my.hausman.test2)

# The p-value of 8.974e-14 means we can reject the null hypothesis that the most efficient panel data model is the random effects. We have strong statistical evidence that a fixed effects model is better suited than a fixed effect type for the dataset.

my.pdmlag <- plm(data = Full_data_long_train2, 
              formula = my.formula2, 
              model = 'within',
              index = my.index2)
summary(my.pdmlag)

# Predict from PLM 
Full_data_long_test2$pred.plm.test <- predict(my.pdmlag, Full_data_long_test2, type='response')

plmmape2 <- 100*mean(abs(Full_data_long_test$pred.plm.test/Full_data_long_test$rentprice-1), na.rm = T)
print(plmmape2)






# Next Questions to ask: 
#1. Mean Stars Over Time (Looks like there might be too much missingness based on how I've broken it down with Phoenix. Not sure if this is a problem with what I did or a problem with the underlying data itself. ??
# Multilevel modeling? Is that useful here? 

# Counts of stars for standard deviation (Try both)

# We're not capturing variance. Throw in the variance too (SD). 

# See if we can do random forest represented as timee (Give each time a unique ID)
# USe correlograms to look at how many 
  # Look into 


# Would it make sense at all to impute on the Yelp set? Definitely impute on the zillow. 