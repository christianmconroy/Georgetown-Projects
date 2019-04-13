getwd()
library(jsonlite)
install.packages('zoo')
library(zoo)
install.packages('dplyr')
library(dplyr)
install.packages('tidyr')
library(tidyr)
install.packages('readr')
library(readr)
untar("yelp_dataset.tar", list=TRUE)

# Load in Review Data
untar("yelp_dataset.tar", files = "dataset/review.json", exdir = "/home/student")
yelp_review <- jsonlite::stream_in(file("dataset/review.json"), pagesize = 10000, verbose = TRUE, flatten = TRUE)

# Merge Review and Business Data 
# Cut Yelp Review to Meet Zillow Data Frame
yelp_review_sub <- yelp_review[yelp_review$date >= "2010-11-01" & yelp_review$date <= "2017-12-31",]
names(yelp_review_sub)

# Set up YearMonth Dates for Aggregation 
yelp_review_sub$date <- as.Date(yelp_review_sub$date)
yelp_review_sub$Month <- match(months(yelp_review_sub$date), month.name)
yelp_review_sub$Year <- format(yelp_review_sub$date,format="%Y")
yelp_review_sub$YearMonth <- as.yearmon(paste(yelp_review_sub$Year, yelp_review_sub$Month), "%Y %m")
yelp_review_sub$stars <- as.numeric(yelp_review_sub$stars)
names(yelp_review_sub)


# Aggregate Reviews by Business and Year Month
# To first change yelp_review variables from as.logical to as.numeric (in order to average out the dummys):
dummys <- sapply(yelp_review_sub, is.logical)
yelp_review_sub[ , dummys] <- lapply(yelp_review_sub[ , dummys], as.numeric)

# Aggregate by means and also get the full count of reviews per business and sd of stars
yelp_review_subagg <- yelp_review_sub %>% 
  group_by(business_id, YearMonth) %>% 
  summarize(Number_of_reviews = n(),
            starsav = mean(stars),
            starssd = sd(stars),
            starsvar = var(stars),
            usefulav = mean(useful),
            funnyav = mean(funny),
            coolav = mean(cool))


# Old Code
# yelp_review_sub <- yelp_review_sub %>%
#   group_by(business_id, YearMonth) %>%
#   summarise_all(funs(mean))
# names(yelp_review_sub)

# Load in Business Data
untar("yelp_dataset.tar", files = "dataset/business.json", exdir = "/home/student")
yelp_business <- jsonlite::stream_in(file("dataset/business.json"), pagesize = 10000, verbose = TRUE, flatten = TRUE)

# Merge Review and Business 
yelp_review_sub <- inner_join(yelp_review_subagg, yelp_business, by='business_id', match='all')
names(yelp_review_sub)

# Eliminate useless columns
yelp_review_sub <- yelp_review_sub[-c(22:109)]
names(yelp_review_sub)

# Collapse by postal code 
yelp_review_long <- yelp_review_sub %>% 
  group_by(postal_code, YearMonth) %>% 
  summarize(Number_of_businesses = n(),
            Number_of_reviews = sum(Number_of_reviews),
            Avnumreviews = mean(Number_of_reviews),
            starsav = mean(starsav),
            starssd = sqrt(mean(starsvar)),
            usefulav = mean(usefulav),
            funnyav = mean(funnyav),
            coolav = mean(coolav),
            bizstars = mean(stars), 
            bizrevcount = sum(review_count), 
            bizrevav = mean(review_count),
            is_openave = mean(is_open))

# Old Code
# yelp_review_long <- yelp_review_sub %>%
#   group_by(postal_code, YearMonth) %>%
#   summarise_all(funs(mean))


# Save so that we never have to do the above again. 
write.csv(yelp_review_long, "yelplongPC_updated.csv")

# Expand Grid 
dates <- setNames((seq(as.Date("2010-11-01"), as.Date("2017-12-31"), by= "month")), c("YearMonth"))
dates1 <- match(months(dates), month.name)
dates2 <- format(dates,format="%Y")
dates <- as.yearmon(paste(dates2, dates1), "%Y %m")
upc <- unique(yelp_review_long$postal_code)

yelp_review_long_eg <- expand.grid(YearMonth = dates, postal_code = upc)
yelp_review_long_eg <- merge(yelp_review_long, yelp_review_long_eg, by= c("YearMonth", "postal_code"), all.x = TRUE, all.y = TRUE)
names(yelp_review_long_eg)

write.csv(yelp_review_long_eg, "yelpreviewlongexp.csv")

# Merge with Zillow
# Load in Zillow Data 
zillow <- read_csv("Zip_Zri_AllHomesPlusMultifamily.csv", col_names = TRUE)
# Convert zillow to long
names (zillow)[2] <- "postal_code"
names(zillow)
zillow <- as.data.frame(zillow)
zillow_long <- reshape(zillow, varying = list(names(zillow[8:95])), times = names(zillow[8:95]), idvar = 'postal_code', v.names = 'rentprice' , direction = 'long')
# Merge with Yelp
# Change date format of zillow long 
zillow_long$time<- as.Date(strptime(paste(1, zillow_long$time),"%d %Y-%m"))
zillow_long <- zillow_long[zillow_long$time >= "2010-11-01" & zillow_long$time <= "2017-12-31",]
zillow_long$month <- match(months(zillow_long$time), month.name)
zillow_long$year <- format(zillow_long$time,format="%Y")
zillow_long$YearMonth <- as.yearmon(paste(zillow_long$year, zillow_long$month), "%Y %m")

# Expand Grid for Zillow 
zillow_long_eg <- expand.grid(YearMonth = dates, postal_code = upc)
zillow_long_eg <- merge(zillow_long, zillow_long_eg, by= c("YearMonth", "postal_code"), all.x = TRUE, all.y = TRUE)
names(yelp_review_long_eg)

Full_data_long <- inner_join(yelp_review_long_eg, zillow_long_eg, by=c('postal_code', 'YearMonth'), match='all')
names(Full_data_long)
length(unique(Full_data_long$postal_code))

# Save so that we never have to do the above again. 
write.csv(Full_data_long, "fulllongPC")

# Spread Review/Business Dataset (Long to Wide)
yelp_review_spread <- spread(yelp_review_long_eg, key = YearMonth, value = stars.x)
length(unique(yelp_review_spread$postal_code))
# Join spread to zillow - Not sure why this isn't giving 15890 rows...
Full_data_wide <- inner_join(yelp_review_spread, zillow, by='postal_code', match='all')
names(Full_data_wide)
length(unique(Full_data_wide$postal_code))


## Analysis 
## TS Modeling 
Full_data_long$time <- paste0(Full_data_long$year, Full_data_long$month, collapse = "-")
Full_data_long$time <- as.Date(strptime(paste(1, Full_data_long$YearMonth),"%d %Y-%m"))
starsts <- Full_data_long[8:9]
starsts <- ts(starsts)
plot(starsts)

rentts <- Full_data_long[9:10]
rentts <- ts(rentts)
plot(rentts)

# Garch for star prices - Won't Work. 
archoutputstars <- garch(Full_data_long$stars, order=c(1, 1), trace = FALSE) 

# Garch for rent prices 
garchoutputrent <- garch(Full_data_long$rentprice, order=c(1, 1)) 
#  Estimated values of the conditional standard deviation process

# Display the coefficients -> I have no idea what this means. 
garchoutputrent
fitted(garchoutputrent)

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
LagErrOLSstars <- lm(ErrStars ~ LagErrStars)
summary(LagErrOLSstars)

# According to the p-value of 0.667, we don't have autocorrelation, right? 

# Plotting the autocorrelation 
acf(Full_data_long$stars, na.action = na.pass)
# na.pass returns the object unchanged
pacf(Full_data_long$stars, na.action = na.pass)
# Looks like we're dealing with serious autocorrelation here though.

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

# Plotting the autocorrelation 
acf(Full_data_long$rentprice)
pacf(Full_data_long$rentprice)
# Looks like we're dealing with serious autocorrelation here though.

# Correctting for autocorrelation of the stars

## Rho Transformation 
# Rho is Rho hat (p)
Rho <- summary(LagErrOLSstars)$coefficients[2]
# Length of Stars variable
N <- length(Full_data_long$stars)
# Lagged Stars
LagStars <- c(NA, Full_data_long$stars[1:(N-1)])
# Lagged time
LagTime <- c(NA, Full_data_long$time[1:(N-1)])
# Rho Transformed Stars (Not sure what to do here)
StarsRho <- Full_data_long$stars - Rho*LagStars
# Rho Transformed Time
TimeRho <- Full_data_long$time - Rho*LagTime
# Rho Transformed Model 
StarsRhoMod <- lm(StarsRho ~ TimeRho)
# Display Results 
summary(StarsRhoMod)
# 4653 observations deleted due to missingness is not what you want.
# P value is still statistically different from 0 though, so this is just telling us that the change in avg stars over time is statistically signficant? 

# Running a dynamic model for Stars
StarsLDV <- lm(Full_data_long$stars ~ LagStars + Full_data_long$time)
summary(StarsLDV)
# Same number dropped here. Both time (long term) and lag (short term) are statistically significant. 



# Correctting for autocorrelation of the rent prices

## Rho Transformation 
# Rho is Rho hat (p)
Rho2 <- summary(LagErrOLSRent)$coefficients[2]
# Length of Stars variable
N2 <- length(Full_data_long$rentprice)
# Lagged Stars
LagRent <- c(NA, Full_data_long$rentprice[1:(N2-1)])
# Lagged time
LagTime2 <- c(NA, Full_data_long$time[1:(N2-1)])
# Rho Transformed Stars (Not sure what to do here)
RentRho <- Full_data_long$rentprice - Rho2*LagRent
# Rho Transformed Time
TimeRho2 <- Full_data_long$time - Rho2*LagTime2
# Rho Transformed Model 
RentRhoMod <- lm(RentRho ~ TimeRho2)
# Display Results 
summary(RentRhoMod)
# P value is still statistically different from 0 though, so this is just telling us that the change in rent prices over time is statistically signficant? 

# Running a dynamic model for Rent Prices
RentLDV <- lm(Full_data_long$rentprice ~ LagRent + Full_data_long$time)
summary(RentLDV)
# Both time (long term) and lag (short term) are statistically significant. 

# https://msperlin.github.io/pafdR/models.html
# 1. Panel Data Models 
# The main motivation to use panel data models is to allow common effects within the groups.
# We're essentially assuming different coefficients for each firm
# We can test the model specification using package plm. Function phtest executes the Hausman test (Hausman 1978), a statistical procedure that tests the null hypothesis that the best model is the random effects and not the fixed effect. Let's try it for our data.

# Panel Data Model Using Machine Learning Approach
library(randomForest)
library(rpart)
library(plm)
library(rpart.plot)
install.packages('tree')
library(tree)

# Create train and test set using the last 12 months (1 year) for the test set 
table(as.factor(Full_data_long$time))
Full_data_long_train <- Full_data_long[Full_data_long$time < "2017-01-01",]
Full_data_long_test <- Full_data_long[Full_data_long$time >= "2017-01-01",]

sapply(Full_data_long, function(x) sum(is.na(x)))

# Build the mixed effects model (Can I do this before the RF or do I have to do it after somehow?)
# Hausman Test
# set options for Hausman test
my.formula <- rentprice ~ stars
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

# Stars NOT significant here, which is interesting. 

# Predict 
# We need to figure out how to predict here better. I think we need to do getfe. 
pred.plm.test <- predict(my.pdm.train, Full_data_long_test, type='response')

# A simple test for whether autocorrelation should be included in the linear mixed effects model is to compare the predictive power of the model with and without autocorrelation.
# To test for autocorrelation in a linear mixed effects model more formally, we can use a likelihood ratio test
# This test compares the log-likelihoods of the mixed effects fits in-sample with and without autocorrelation, correcting for the additional degrees of freedom (and therefore potential for overfitting) that the linear mixed effects model with autocorrelation uses


# set options for Hausman test
my.formula <- rentprice ~ stars
my.index <- c('postal_code','time')

# do Hausman test
my.hausman.test <- phtest(x = my.formula, 
                          data = Full_data_long,
                          model = c('within', 'random'),
                          index = my.index)

# print result
print(my.hausman.test)

# The p-value of 0.3758 means we can not reject the null hypothesis that the most efficient panel data model is the random effects. We have strong statistical evidence that a random effects model is better suited than a fixed effect type for the dataset.

my.pdm <- plm(data = Full_data_long, 
              formula = rentprice ~ stars, 
              model = 'random',
              index = my.index)
summary(my.pdm)

# Predict from PLM 
predict(my.pdm)

## Panel Data Model with Lags for both (Mixed effects with autocorrelation is what we want the mmost, right?)
# Random forest is definitely not useful with one variable though. # The point of using rando forest would be to test the importance of each explanatory variable though right? 

# I think plm is the base model and everything else is essentially an add on (GARCH/Arch based on the autocorrelation and heteroskedasticity and RF based on having a lot of variables to choose from)


# set options for Hausman test
my.formula2 <- LagRent ~ LagStars
my.index2 <- c('postal_code','time')

# do Hausman test
my.hausman.test2 <- phtest(x = my.formula2, 
                           data = Full_data_long,
                           model = c('within', 'random'),
                           index = my.index2)

# print result
print(my.hausman.test2)

# The p-value of 8.974e-14 means we can reject the null hypothesis that the most efficient panel data model is the random effects. We have strong statistical evidence that a fixed effects model is better suited than a fixed effect type for the dataset.

my.pdmlag <- plm(data = Full_data_long, 
                 formula = my.formula2, 
                 model = 'within',
                 index = my.index2)
summary(my.pdmlag)

# Predict from PLM 
predict(my.pdmlag)
