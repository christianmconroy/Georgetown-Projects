## Final Project Script ##

# Top counts sorted bars subsetted, 

##################### Setting Up ########################
# Set WD where the files are. 
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/DataScienceIntro")

# Load in required packages 
library(zoo)
library(readr)
library(tidyr)
library(dplyr)
library(Amelia)
library(lattice)
library(stringr)
library(jsonlite)
library(mitools)
library(plm)

################33## Importing in the Data ##################
# Load in Yelp Checkin Data 
yelp_checkin <- as.data.frame(jsonlite::stream_in(file("dataset/checkin.json")), flatten = TRUE)
class(yelp_checkin)

renquote <- function(l) if (is.list(l)) lapply(l, renquote) else enquote(l)

yelp_checkin_flat <- as.data.frame(lapply(unlist(renquote(yelp_checkin)), eval))

# Reshape checkin data
yelp_checkin_flat_long <- reshape(yelp_checkin_flat, varying = list(names(yelp_checkin_flat[1:168])), times = names(yelp_checkin_flat[1:168]), idvar = 'business_id', v.names = 'checkin' , direction = 'long')

yelp_checkin_flat_long$time <- str_replace(yelp_checkin_flat_long$time, "time.", "")
yelp_checkin_flat_long$time <- gsub('[[:digit:]]+', '', yelp_checkin_flat_long$time)

yelp_checkin_flat_long$time = substr(yelp_checkin_flat_long$time,1,nchar(yelp_checkin_flat_long$time)-2)

# Collapse to get checkins per business by each day of the week. 
yelp_checkin_collapse_mean <- as.data.frame(aggregate(checkin ~ business_id + time, yelp_checkin_flat_long , mean))
yelp_checkin_collapse_sum <- as.data.frame(aggregate(checkin ~ business_id + time, yelp_checkin_flat_long , sum))

# Convert long to wide
yelp_checkin_wide_mean <- spread(yelp_checkin_collapse_mean, key = time, value = checkin)
yelp_checkin_wide_sum <- spread(yelp_checkin_collapse_sum, key = time, value = checkin)

# Merge
yelp_checkin_wide <- inner_join(yelp_checkin_wide_mean, yelp_checkin_wide_sum, by='business_id', match='all')
colnames(yelp_checkin_wide) <- c("business_id", "Friday_ave", "Monday_ave", "Saturday_ave", "Sunday_ave", "Thursday_ave", "Tuesday_ave", "Wednesday_ave", "Friday_total", "Monday_total", "Saturday_total", "Sunday_total", "Thursday_total", "Tuesday_total", "Wednesday_total")

# write.csv(yelp_checkin_wide, "yelp_checkin_wide.csv")

# Load back in business dataset to merge and aggregate by zipcode
# yelp_business <- fromJSON(sprintf("[%s]", paste(readLines("dataset/business.json"), collapse=",")), simplifyDataFrame=TRUE, flatten=TRUE)
yelp_business <- as.data.frame(jsonlite::stream_in(file("dataset/business.json")), flatten = TRUE)
class(yelp_business)

checkinbiz <- inner_join(yelp_business, yelp_checkin_wide, by=c('business_id'), match='all')
# Eliminate useless columns
checkinbiz <- checkinbiz[-c(2:6, 8:101)]

checkinzipmean <- as.data.frame(aggregate(. ~ postal_code, checkinbiz[2:9], mean))
checkinzipsum <- as.data.frame(aggregate(. ~ postal_code, checkinbiz[c(2, 10:16)], sum))

# Full Clean Checkin
checkinfull <- inner_join(checkinzipmean, checkinzipsum, by=c('postal_code'), match='all')

############### Import Yelp Data 
yelp_review_long <- read.csv("yelplongPC_updated2.csv", header = T, na.strings=c("NA"))
sapply(yelp_review_long, function(x) sum(is.na(x)))
# No missing postal codes
length(unique(yelp_review_long$postal_code))
# 15890 unique postal codes 
yelp_review_long$YearMonth <- as.yearmon(yelp_review_long$YearMonth)

############# Import Zillow Data 
# Load in Zillow Data 
zillow <- read_csv("zecon/Zip_Zri_AllHomesPlusMultifamily.csv", col_names = TRUE)
# Convert zillow to long
names (zillow)[2] <- "postal_code"
zillow <- as.data.frame(zillow)

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


############### Merge Checkin with Yelp 
# Nonexpanded
yelp_review_long <- left_join(yelp_review_long, checkinfull, by=c('postal_code'), match='all')

############### Merge Zillow with Yelp
# Non-Expanded
Full_data_long <- inner_join(yelp_review_long, zillow_long, by=c('postal_code', 'YearMonth'), match='all')
length(unique(Full_data_long$postal_code))
sapply(Full_data_long, function(x) sum(is.na(x)))

# Save so that we never have to do the above again. 
# write.csv(Full_data_long, "OfficialLongWithCheck.csv")

################Exploratory Data Analysis###########################
# Subset business based on eventual merging with Zillow
Full <- unique(Full_data_long$postal_code)
#yelp_business <- as.data.frame(yelp_business)
business_zillow <- dplyr::filter(yelp_business, postal_code %in% Full)

# Assess counts of states
Full_data_long$State <- Full_data_long$State %>% as.factor
Full_data_long$State %>% summary
# Top: Arizona, Ohio, Pennsylvania, Nevada, North Carolina, Wisconsin, Illinois
ggplot(Full_data_long, aes(State)) + geom_bar() + labs(title= " Frequency of States in Yelp Challenge Dataset")

# Assess counts of cities
Full_data_long$City <- Full_data_long$City %>% as.factor
Full_data_long$City %>% summary
# Apart from Other, top include Phoenix, Las Vegas, Charlotte, Pittsburgh, Cleveland

temp <- row.names(as.data.frame(summary(Full_data_long$City, max=12))) # create a df or something else with the summary output.
Full_data_long$City <- as.character(Full_data_long$City) # IMPORTANT! Here was the problem: turn into character values
Full_data_long$top <- ifelse(
  Full_data_long$City %in% temp, ## condition: match aDDs$answer with row.names in summary df 
  Full_data_long$City, ## then it should be named as aDDs$answer
  "Other" ## else it should be named "Other"
)
Full_data_long$top <- as.factor(Full_data_long$top) # factorize the output again
ggplot(Full_data_long[Full_data_long$top!="Other",],aes(x=factor(top, levels=names(sort(table(top),increasing=TRUE))))) + geom_bar() + labs(title="Frequency of Top Cities in Yelp Challenge Dataset") + xlab("City") + ylab("Count")

## Assess counts of categories 
catplot <- business_zillow%>%select(-starts_with("hours"), -starts_with("attribute")) %>% unnest(categories) %>%
  select(name, categories)%>%group_by(categories)%>%summarise(n=n())%>%arrange(desc(n))%>%head(20)
catplot <- as.data.frame(catplot)
names(catplot)
ggplot(data=catplot, aes(x=categories, y=n)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="Frequency of Categories in Dataset") + xlab("Category") + ylab("Count")

# Top: Restaurants, Shopping, Food, Home Services, Beauty & Spa

## Assess counts of categories only for those that are open
catplot_open <- business_zillow %>% select(-starts_with("hours"), -starts_with("attribute")) %>%
  filter(is_open==1) %>%
  unnest(categories) %>%
  select(name, categories)%>%group_by(categories)%>%summarise(n=n())%>%arrange(desc(n))%>%head(20)

catplot_open  <- as.data.frame(catplot_open )

ggplot(data=catplot_open , aes(x=categories, y=n)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="Frequency of Open Categories in Dataset") + xlab("Category") + ylab("Count")
# Top: Restaurants, Shopping, Home Services, Food, Health & Medical (Looks like Food and Beauty & Spas might be closing at higher rate)

## Comparing the rent prices across cities (collapse into cities because too many zips)
# Reload in Zillow Data 
zillow <- read_csv("zecon/Zip_Zri_AllHomesPlusMultifamily.csv", col_names = TRUE)
zillow <- zillow[-c(94:95)]
names(zillow)

zillow_collapse_wide <- zillow %>% 
  group_by(City) %>% 
  summarize_all(funs(mean))
names(zillow_long)

zillow_collapse_long <- zillow_long %>% 
  group_by(State, time) %>% 
  summarize(rentprice = mean(rentprice))

# Plot Time Series of Rents over Time
ggplot(zillow_collapse_long, aes(x = time, y=rentprice, group = State, colour = State)) + geom_line()  + scale_colour_discrete(guide = 'none')  + scale_x_date(expand=c(0.1, 0)) + geom_dl(aes(label = State), method = list(dl.trans(x = x + .2), "last.points")) +
  geom_dl(aes(label = State), method = list(dl.trans(x = x - .2), "first.points")) + labs(title = "Rent Prices over Time by State", xlab = "Rent Price (USD)")

## Comparing the avg. stars across cities (collapse into cities because too many zips)
# Collapse by State
FDL_collapse_long <- Full_data_long %>% 
  group_by(State, time) %>% 
  summarize(starsav = mean(starsav))

# Plot Time Series of Stars over Time
ggplot(FDL_collapse_long, aes(x = time, y=starsav, group = State, colour = State)) + geom_line()  + scale_colour_discrete(guide = 'none')  + scale_x_date(expand=c(0.1, 0)) + geom_dl(aes(label = State), method = list(dl.trans(x = x + .2), "last.points")) +
  geom_dl(aes(label = State), method = list(dl.trans(x = x - .2), "first.points")) + labs(title = "Average Stars over Time by State", xlab = "Avg. Stars (1-5)")
# Not useful at all. No trends or patterns. Just random it seems. 

########### Now we can start the actual analysis!!!!!###############
# How many lags should we do. Confusing. 
# pacf(Full_data_long$rentprice, na.action = na.pass)
# pacf(Full_data_long$starsav, na.action = na.pass)

############ Panel Data Model Using Machine Learning Approach

##### Normal Dataset

# Create train and test set using the last 12 months (1 year) for the test set 
Full_data_long_train <- Full_data_long[Full_data_long$time < "2017-01-01",]
Full_data_long_test <- Full_data_long[Full_data_long$time >= "2017-01-01",]

# Build the mixed effects model
# Hausman Test
# set options for Hausman test
my.formula <- rentprice ~ starsav + is_openave + funnyav + coolav + usefulav 
# Had to take + Friday_ave + Monday_ave + Saturday_ave + Sunday_ave + Tuesday_ave + Wednesday_ave + Friday_total + Monday_total + Saturday_total + Sunday_total + Thursday_total + Tuesday_total + Wednesday_totalout because of the NAs.   
# Had too take standard deviation out because of all the NAs. 
my.index <- c('postal_code','time')
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

# Weekend are statsig, which is essentially what I thought would happen (i.e. more people come visit a place on the weekends)

# Predict 
Full_data_long_test$pred.plm.test <- predict(my.pdm.train, Full_data_long_test, type='response')

# MAPE
plmmape <- 100*mean(abs(Full_data_long_test$pred.plm.test/Full_data_long_test$rentprice-1), na.rm = T)
print(plmmape)

# MAPE is only 21.39% right now. 

ggplot(Full_data_long_test, aes(x=rentprice, y=pred.plm.test)) +geom_point() + labs(title="Predicted vs. Actual Real Estate Prices") + xlab("Actual") + ylab("Predicted")

#### Lagged Model 

# Try to make sure both sets have same postal codes
# 05440 was staying in as factor in test for some reason. Take out here. 
Full_data_long_train <- Full_data_long[Full_data_long$time < "2017-01-01",]
Full_data_long_test <- Full_data_long[Full_data_long$time >= "2017-01-01",]
Full_data_long_test <- Full_data_long_test[Full_data_long_test$postal_code!="05440",]

train <- unique(Full_data_long_train$postal_code)
test<- unique(Full_data_long_test$postal_code)

Full_data_long_test <- dplyr::filter(Full_data_long_test, postal_code %in% train)
Full_data_long_train <- dplyr::filter(Full_data_long_train, postal_code %in% test)

length(unique(Full_data_long_test$postal_code))
length(unique(Full_data_long_train$postal_code))

# Specification 
my.lag.formula <- rentprice ~ lag(rentprice, 1) + starsav + is_openave + funnyav + coolav + usefulav + Number_of_reviews 

# Conduct Hausman Test
my.hausman.test.train.lag <- phtest(x = my.lag.formula, 
                                data = Full_data_long_train,
                                model = c('within', 'random'),
                                index = my.index)

# print result
print(my.hausman.test.train.lag)

# Low p-value so do fixed effects)

# Regular LM using zip dummies
my.pdm.train.lag.lm <- lm (rentprice ~ lag(rentprice, 1) + starsav + is_openave + funnyav + coolav + usefulav + Number_of_reviews + postal_code + time, data = Full_data_long_train) 
summary(my.pdm.train.lag.lm)

# Predict 
#LM 
Full_data_long_test$pred.plm.test.lag <- predict(my.pdm.train.lag.lm, Full_data_long_test)

  # MAPE
  plmmape.lag <- 100*mean(abs(Full_data_long_test$pred.plm.test.lag/Full_data_long_test$rentprice-1), na.rm = T)
  print(plmmape.lag)

# Mape of 

ggplot(Full_data_long_test, aes(x=rentprice, y=pred.plm.test.lag)) +geom_point() + labs(title="Predicted vs. Actual Real Estate Prices") + xlab("Actual") + ylab("Predicted")

##### Imputed Dataset (Throwing almost all independent variables in)
# Multiple Imputation for Missing Values Using the Amelia Package - It uses bootstrapping and Expectation-Maximization algorithm, to impute the missing values in a data set. 

############ AMELIA
sapply(Full_data_long, function(x) sum(is.na(x)))
Full_data_long <- Full_data_long[-c(40)]
Imputed_Full_data_long <-amelia(Full_data_long,ts= 'time', cs= 'postal_code', p2s=0, intercs = FALSE, idvars=c('City', 'State', 'Metro', 'CountyName', 'year', 'month', 'YearMonth'))

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

allimp <- imputationList(list(data1,data2,data3,data4,data5))

# Create train and test set using the last 12 months (1 year) for the test set 
data5$time <- as.Date(data5$time, "%Y-%m-%d")
data5_train <- data5[data5$time < "2017-01-01",]
data5_test <- data5[data5$time >= "2017-01-01",]

# 05440 was staying in as factor in test for some reason. Take out here. 
data5_test <- data5_test[data5_test$postal_code!="5440",]
data5_test <- data5_test[data5_test$postal_code!="8054",]

trainimp <- unique(data5_train$postal_code)
testimp <- unique(data5_test$postal_code)

data5_test <- dplyr::filter(data5_test, postal_code %in% trainimp)
data5_train <- dplyr::filter(data5_train, postal_code %in% testimp)

# Build the mixed effects model 
# Hausman Test
# set options for Hausman test
my.formula.impute.lag <- rentprice ~ lag(rentprice, 12) + starsav + starssd + is_openave + funnyav + coolav + usefulav + Number_of_reviews + Number_of_businesses + Friday_ave + Monday_ave + Saturday_ave + Sunday_ave + Thursday_ave + Tuesday_ave + Wednesday_ave + Friday_total + Monday_total + Saturday_total + Sunday_total + Thursday_total + Tuesday_total + Wednesday_total
# Had too take standard deviation out because of all the NAs. What to do? Can impute those too?  

my.index <- c('postal_code','time')

# Conduct Hausman Test
my.hausman.test.train.impute.lag <- phtest(x = my.formula.impute.lag , data = data5_train, model = c('within', 'random'),index = my.index)
# print result
print(my.hausman.test.train.impute.lag)

# Low p-value, so best to go with fixed effects.  

# Regular LM using zip dummies
my.pdm.train.lag.lm.immpute <- lm (rentprice ~ lag(rentprice, 12) + starsav + starssd + is_openave + funnyav + coolav + usefulav + Number_of_reviews + Number_of_businesses + Friday_ave + Monday_ave + Saturday_ave + Sunday_ave + Thursday_ave + Tuesday_ave + Wednesday_ave + Friday_total + Monday_total + Saturday_total + Sunday_total + Thursday_total + Tuesday_total + Wednesday_total + postal_code + time, data = data5_train) 
summary(my.pdm.train.lag.lm.immpute)

# Predict 
#LM (Can't do. Would it be fine to make time continuous?)
data5_test$my.pdm.train.lag.lm.immpute <- predict(my.pdm.train.lag.lm.immpute, data5_test)

plmmape_impute_lag <- 100*mean(abs(data5_test$my.pdm.train.lag.lm.immpute/data5_test$rentprice-1), na.rm = T)
print(plmmape_impute_lag)

# Imputation gives us 4.768442 (Might be different if we tried the other 4 imputed data sets)

ggplot(data5_test, aes(x=rentprice, y=my.pdm.train.lag.lm.immpute)) +geom_point() + labs(title="Predicted vs. Actual Real Estate Prices") + xlab("Actual") + ylab("Predicted")

# Reduced Imputed Model (Sans Checkin Data)
my.formula.impute.lag.Simple <- rentprice ~ lag(rentprice, 12) + starsav + starssd + is_openave + funnyav + coolav + usefulav + Number_of_reviews + Number_of_businesses 
# Had too take standard deviation out because of all the NAs. What to do? Can impute those too?  

my.index <- c('postal_code','time')

# Conduct Hausman Test
my.hausman.test.train.impute.lag.Simple <- phtest(x = my.formula.impute.lag.Simple , data = data5_train, model = c('within', 'random'),index = my.index)
# print result
print(my.hausman.test.train.impute.lag.Simple)

# Low p-value, but we'll go with random. 

# Built fixed random effects model on train
my.pdm.train.impute.lag.Simple  <- lm (rentprice ~ lag(rentprice, 12) + starsav + starssd + is_openave + funnyav + coolav + usefulav + Number_of_reviews + Number_of_businesses + postal_code + time, data = data5_train) 
summary(my.pdm.train.impute.lag.Simple)

# Predict 
data5_test$pred.plm.test.impute.lag.Simple <- predict(my.pdm.train.impute.lag.Simple, data5_test)

plmmape_impute_lag.Simple <- 100*mean(abs(data5_test$pred.plm.test.impute.lag.Simple/data5_test$rentprice-1), na.rm = T)
print(plmmape_impute_lag.Simple)

# Imputation gives us 2.39 (Might be different if we tried the other 4 imputed data sets)

ggplot(data5_test, aes(x=rentprice, y=pred.plm.test.impute.lag.Simple)) +geom_point() + labs(title="Predicted vs. Actual Real Estate Prices") + xlab("Actual") + ylab("Predicted")

##################### Food and Bar Subset ##########################
################33## Importing in the Data ##################
############### Import Yelp Data 
yelp_review_long_food <- read.csv("yelplongPC_food.csv", header = T, na.strings=c("NA"))
sapply(yelp_review_long_food, function(x) sum(is.na(x)))
# No missing postal codes
length(unique(yelp_review_long_food$postal_code))
# 3274 unique postal codes 
yelp_review_long_food$YearMonth <- as.yearmon(yelp_review_long_food$YearMonth)

############### Merge Checkin with Yelp 
# Nonexpanded
yelp_review_long_food <- left_join(yelp_review_long_food, checkinfull, by=c('postal_code'), match='all')

############### Merge Zillow with Yelp

# Non-Expanded
Full_data_long_food <- inner_join(yelp_review_long_food, zillow_long, by=c('postal_code', 'YearMonth'), match='all')
length(unique(Full_data_long_food$postal_code))
sapply(Full_data_long_food, function(x) sum(is.na(x)))

########### Now we can start the actual analysis!!!!!###############
##### Normal Dataset

# Create train and test set using the last 12 months (1 year) for the test set 
Full_data_long_food_train <- Full_data_long_food[Full_data_long_food$time < "2017-01-01",]
Full_data_long_food_test <- Full_data_long_food[Full_data_long_food$time >= "2017-01-01",]

# Build the mixed effects model 
# Hausman Test
# set options for Hausman test
names(Full_data_long_food)
my.formula <- rentprice ~ starsav + is_openave + funnyav + coolav + usefulav 

my.index <- c('postal_code','time')
# Conduct Hausman Test
my.hausman.test.train.food <- phtest(x = my.formula, 
                                data = Full_data_long_food_train,
                                model = c('within', 'random'),
                                index = my.index)

# print result
print(my.hausman.test.train.food)

# High p-value means random is the way to go here. 

# Built random effects model on train
my.pdm.train.food <- plm(data = Full_data_long_food_train, 
                    formula = my.formula, 
                    model = 'random',
                    index = my.index)
summary(my.pdm.train.food)

# Predict 
Full_data_long_food_test$pred.plm.test <- predict(my.pdm.train.food, Full_data_long_food_test, type='response')

# MAPE
plmmape.food <- 100*mean(abs(Full_data_long_food_test$pred.plm.test/Full_data_long_food_test$rentprice-1), na.rm = T)
print(plmmape.food)

# MAPE is 18.69%

ggplot(Full_data_long_food_test, aes(x=rentprice, y=pred.plm.test)) +geom_point() + labs(title="Predicted vs. Actual Real Estate Prices") + xlab("Actual") + ylab("Predicted")

#### Lagged Model 
# Other zips were staying in as factors in test for some reason. Take out here. 
Full_data_long_food_test <- Full_data_long_food_test[Full_data_long_food_test$postal_code!="28803",]
Full_data_long_food_test <- Full_data_long_food_test[Full_data_long_food_test$postal_code!="85266",]

trainimpfood <- unique(Full_data_long_food_train$postal_code)
testimpfood <- unique(data5_test$postal_code)

Full_data_long_food_test <- dplyr::filter(Full_data_long_food_test, postal_code %in% trainimpfood)
Full_data_long_food_train <- dplyr::filter(Full_data_long_food_train, postal_code %in% testimpfood)

my.lag.formula <- rentprice ~ lag(rentprice, 1) + starsav + is_openave + funnyav + coolav + usefulav + Number_of_reviews

# Conduct Hausman Test
my.hausman.test.food.train.lag <- phtest(x = my.lag.formula, 
                                    data = Full_data_long_food_train,
                                    model = c('within', 'random'),
                                    index = my.index)

# print result
print(my.hausman.test.food.train.lag)

# Wouldn't low p-value mean we should go with fixed effect instead of random? But just including within doesn't work below. 

# Built fixed effects model on train
my.pdm.train.impute.lag.Simple  <- lm (rentprice ~ lag(rentprice, 1) + starsav + is_openave + funnyav + coolav + usefulav + Number_of_reviews + postal_code + time, data = Full_data_long_food_train) 
summary(my.pdm.train.impute.lag.Simple)

# Predict 
Full_data_long_food_test$pred.plm.test.lag <- predict(my.pdm.train.impute.lag.Simple, Full_data_long_food_test)

# MAPE
plmmape.lag.food <- 100*mean(abs(Full_data_long_food_test$pred.plm.test.lag/Full_data_long_food_test$rentprice-1), na.rm = T)
print(plmmape.lag.food)

# Mape of  6.675503e-13

ggplot(Full_data_long_food_test, aes(x=rentprice, y=pred.plm.test.lag)) +geom_point() + labs(title="Predicted vs. Actual Real Estate Prices") + xlab("Actual") + ylab("Predicted")

##### Imputed Dataset (Throwing almost all independent variables in)
# Multiple Imputation for Missing Values Using the Amelia Package - It uses bootstrapping and Expectation-Maximization algorithm, to impute the missing values in a data set. 

############ AMELIA - Would not work., 
sapply(Full_data_long_food, function(x) sum(is.na(x)))
Full_data_long_food <- Full_data_long_food[-c(6, 13:14)] 
#
Imputed_Full_data_long_food <-amelia(Full_data_long_food,ts= 'time', cs= 'postal_code', p2s=0, intercs = FALSE, idvars=c('City', 'State', 'Metro', 'CountyName', 'year', 'month', 'YearMonth'))

# Do we need polytime or setting the power of polynomial for time effects (integer between 0 and 3)

write.amelia(obj=Imputed_Full_data_long_food, file.stem="imputedfull_foodf")

data6 <- read.csv("imputedfull_foodf1.csv")
data7 <- read.csv("imputedfull_foodf2.csv")
data8 <- read.csv("imputedfull_foodf3.csv")
data9 <- read.csv("imputedfull_foodf4.csv")
data10 <- read.csv("imputedfull_foodf5.csv")

data6 <- pdata.frame(data6, index = c("postal_code", "time"))
data7 <- pdata.frame(data7, index = c("postal_code", "time"))
data8 <- pdata.frame(data8, index = c("postal_code", "time"))
data9 <- pdata.frame(data9, index = c("postal_code", "time"))
data10 <- pdata.frame(data10, index = c("postal_code", "time"))

allimp <- imputationList(list(data6,data7,data8,data9,data10))

# Create train and test set using the last 12 months (1 year) for the test set 
data10$time <- as.Date(data10$time, "%Y-%m-%d")
data10_train <- data10[data10$time < "2017-01-01",]
data10_test <- data10[data10$time >= "2017-01-01",]

trainimpfin <- unique(data10_train$postal_code)
testimpfin <- unique(data10_test$postal_code)

data10_test <- dplyr::filter(data10_test, postal_code %in% trainimpfin)
data10_train <- dplyr::filter(data10_train, postal_code %in% testimpfin)
length(unique(data10_test$postal_code))
length(unique(data10_train$postal_code))

# Build the mixed effects model
# Hausman Test
# set options for Hausman test
my.formula.impute.lag.food <- rentprice ~ lag(rentprice, 12) + starsav + starssd + is_openave + funnyav + coolav + usefulav + Number_of_reviews + Number_of_businesses + Friday_ave + Monday_ave + Saturday_ave + Sunday_ave + Thursday_ave + Tuesday_ave + Wednesday_ave + Friday_total + Monday_total + Saturday_total + Sunday_total + Thursday_total + Tuesday_total + Wednesday_total + postal_code + time

my.index <- c('postal_code','time')

# Conduct Hausman Test
my.hausman.test.train.impute.lag.food <- phtest(x = my.formula.impute.lag.food, data = data10_train, model = c('within', 'random'),index = my.index)
# print result
print(my.hausman.test.train.impute.lag.food)

# Low p-value, so we'll go with fixed.  

# Built fixed effects model on train
my.pdm.train.impute.lag.food  <- lm (rentprice ~ lag(rentprice, 12) + starsav + starssd + is_openave + funnyav + coolav + usefulav + Number_of_reviews + Number_of_businesses + Friday_ave + Monday_ave + Saturday_ave + Sunday_ave + Thursday_ave + Tuesday_ave + Wednesday_ave + Friday_total + Monday_total + Saturday_total + Sunday_total + Thursday_total + Tuesday_total + Wednesday_total + postal_code + time, data = data10_test) 
summary(my.pdm.train.impute.lag.food)

# Predict - Could not get it to work. 
data10_test$pred.plm.test.impute.lag <- predict(my.pdm.train.impute.lag.food, data10_test)

plmmape_impute_lag_food <- 100*mean(abs(data10_test$pred.plm.test.impute.lag/data10_test$rentprice-1), na.rm = T)
print(plmmape_impute_lag_food)

# Imputation gives us %, which is better than the full set. 

ggplot(data10_test, aes(x=rentprice, y=pred.plm.test.impute.lag)) +geom_point() + labs(title="Predicted vs. Actual Real Estate Prices") + xlab("Actual") + ylab("Predicted")

# Reduced Imputed Model (Sans Checkin Data)
my.formula.impute.lag.Simple.food <- rentprice ~ lag(rentprice, 12) + starsav + starssd + is_openave + funnyav + coolav + usefulav + Number_of_reviews + Number_of_businesses
# Had too take standard deviation out because of all the NAs. What to do? Can impute those too?  

# Conduct Hausman Test
my.hausman.test.train.impute.lag.Simple.food <- phtest(x = my.formula.impute.lag.Simple.food, data = data10_train, model = c('within', 'random'),index = my.index)
# print result
print(my.hausman.test.train.impute.lag.Simple.food)

# Low p-value, but we'll go with random. 

# Built random effects model on train
my.pdm.train.impute.lag.Simple.food <- plm(data = data10_train, 
                                      formula = my.formula.impute.lag.Simple.food, 
                                      model = 'random',
                                      index = my.index)
summary(my.pdm.train.impute.lag.Simple.food)

# Predict 
data10_test$pred.plm.test.impute.lag.Simple <- predict(my.pdm.train.impute.lag.Simple.food, data10_test, type='response')

plmmape_impute_lag.Simple.food <- 100*mean(abs(data10_test$pred.plm.test.impute.lag.Simple/data10_test$rentprice-1), na.rm = T)
print(plmmape_impute_lag.Simple.food)

# Imputation gives us 3.46, again better than the full set. 

ggplot(data10_test, aes(x=rentprice, y=pred.plm.test.impute.lag.Simple)) +geom_point() + labs(title="Predicted vs. Actual Real Estate Prices") + xlab("Actual") + ylab("Predicted")
