## Final Project Script ##

##################### Setting Up ########################
# Set WD where the files are. 
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/DataScienceIntro")

# Load in required packages 
library(zoo)
library(readr)
library(tidyr)
library(dplyr)
library(tidyverse)
################33## Importing in the Data ##################


############### Import Yelp Data 
yelp_review_long <- read.csv("yelplongPC", header = T, na.strings=c(""))
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
length(unique(zillow_long$postal_code))

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

############### Merge Zillow with Yelp
# Expanded
Full_data_long_ex <- inner_join(yelp_review_long_eg, zillow_long_eg, by=c('postal_code', 'YearMonth'), match='all')
names(Full_data_long)
length(unique(Full_data_long_ex$postal_code))
sapply(Full_data_long_ex, function(x) sum(is.na(x)))

# Non-Expanded
Full_data_long <- inner_join(yelp_review_long, zillow_long, by=c('postal_code', 'YearMonth'), match='all')
length(unique(Full_data_long$postal_code))
sapply(Full_data_long, function(x) sum(is.na(x)))


# Save so that we never have to do the above again. 
write.csv(Full_data_long, "OfficialLongCombined.csv")
write.csv(Full_data_long_ex, "OfficialGrigCombined.csv")



# Load in required packages 
library(ggplot2)
library(tidyverse)
library(zoo)
library(stringr)
library(readr)
library(scales)
library(jsonlite)

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

rev_biz_tidy <- yelp_business %>% 
  select(-starts_with("hour"), -starts_with("attribute"), -contains("votes"), -contains("type"))
names(yelp_business)
names(rev_biz_tidy)

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
names(zillow)

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
yelp_review_geomatch$YearMonth <- as.yearmon(paste(yelp_review_geomatch$Year, yelp_review_geomatch$Month), "%Y %m")
yelp_review_geomatch$stars.x <- as.numeric(yelp_review_geomatch$stars.x)
names(yelp_review_geomatch)

cobined_agg <- aggregate(stars.x ~ postal_code + YearMonth, yelp_review_geomatch, mean, drop = FALSE) # Start #
names(cobined_agg)

cobined_collapse <- spread(cobined_agg, key = YearMonth, value = stars.x)
names(cobined_collapse)

# Join back to original data
cobined_collapse2 <- left_join(yelp_review, yelp_business_ph, by='business_id', match='all')

# This is the wide form we should be working with. 
combinedData <- inner_join(cobined_collapse, zillow, by='postal_code', match='all')
combinedData <- combinedData[-c(2:68, 247:248)]
head(combinedData)

write.csv(combinedData, "FULLDATASETYZWIDETRUE.csv")

# Convert wide to long
Full_data_add <- reshape(combinedData, varying = list(names(combinedData[2:87])), times = names(combinedData[2:87]), idvar = 'postal_code', v.names = 'stars' , direction = 'long')
Full_data_add <- Full_data_add[-c(8:93)]
head(Full_data_add)
Full_data_add2 <- reshape(combinedData, varying = list(names(combinedData[94:179])), times = names(combinedData[94:179]), idvar = 'postal_code', v.names = 'rentprice' , direction = 'long')
Full_data_add2 <- Full_data_add2[-c(2:87)]

Full_data_long <- cbind(Full_data_add[c(1:7, 9)], Full_data_add2[8:9])
Full_data_wide <- combinedData

write.csv(Full_data_long, "FULLDATASETYZLONGTRUE.csv")

#############################################################################

read.csv("OfficialLongCombined.csv", header =T)

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

# The REEM Approach: https://link.springer.com/content/pdf/10.1007/s10994-011-5258-3.pdf
  # This paper presents a methodology that combines the structure of mixed effects models for longitudinal and clustered data with the flexibility of tree-based estimation methods
  # We apply the resulting estimation method, called the RE-EM tree, to pricing in online transactions, showing that the RE-EM tree is less sensitive to parametric assumptions and provides improved predictive power compared to linear models with random effects and regression trees without random effects
# This method combines the flexibility of a data mining method with the specific nature of a longitudinal or clustered data set.
# It is clear that a prediction of a future sale price based on hypothesized future attribute values for a title for which sales are already in the data set should take into account evidence in those previous sales that the title sells for systematically higher or lower prices than expected, something that is not possible using methods that treat each sale as an independent observation
    # Work by Galimberti and Montanari (2002) developed a way to create trees that include both time-varying attributes and a longitudinal data structure
    # Because they allowed for time-varying attributes, different observations for the same group could appear in different nodes; this made the split function particularly complicated (the method proposed here also allows different observations for the same group to appear in different nodes, but in a much more straightforward manner)
#  Since neither the random effects nor the fixed effects are known, we alternate between estimating the regression tree, assuming that our estimates of the random effects are correct, and estimating the random effects, assuming that the regression tree is correct.
# With rpart, Splitting continues as long as the increase in the proportion of variability accounted for by the tree (termed the complexity parameter, cp) is at least 0.001 and the number of observations in the node being considered for splitting is at least 20
  # Once the initial tree is formed, it is pruned based on 10-fold cross-validation
  # . First, the tree with final split corresponding to the cp value with minimized 10-fold cross-validated error is obtained. 
# Then, the tree with final split corresponding to the largest cp value with 10-fold cross-validated error that is no more than one standard error above the minimized value is determined; this is the final tree
  # In most of the results we present, we estimate the linear model with REML, because it yields unbiased estimates for the variance, Ri.

# Many statistical packages contain code to estimate linear mixed effects models; the lme function of the R nlme package (Pinheiro et al. 2009) is used here [Might try both using plm from electrolux and lme from ALA]

#  It fits the model using a combination of the ECME algorithm (Liu and Rubin 1994), a modification of the EM algorithm designed to speed its convergence, and the Newton-Raphson algorithm. This here is key (and it's the hardpart)

# A faster alternative that will also be explored here is to limit Step 2 above to one iteration. That is, an initial tree is fit ignoring the longitudinal structure, a mixed effects model is fit based on the resultant tree structure, and a final population-level tree is reported with the same structure, but with predicted responses that reflect the estimated random effects. [This could be the way to go but I am not sure how to do it beyond step 1.] 


##### Plotting the time series for stars and rent prices 

# Mean Stars Over Time (Looks like there might be too much missingness based on how I've broken it down with Phoenix. Not sure if this is a problem with what I did or a problem with the underlying data itself.)
Full_data_wide_plotstars <- Full_data_wide[2:87]
names(Full_data_wide)
plot(c(1:86), apply(Full_data_wide_plotstars, 2, mean),  ylim = c(0,5),  xlab = 'Time (YearMonths)', ylab = 'Avg. Stars', main = 'Mean Stars per Month', type = 'n')
abline(v = axTicks(1), h = axTicks(2), col = rgb(0.75, 0.75, 0.75, alpha = 0.5), lty = 3)
lines(c(1:86), apply(Full_data_wide_plotstars, 2, mean), type = 'b', pch = 1, lty = 2)

# Time Series of Avg. Rent Prices by month
Full_data_wide_plotrent <- Full_data_wide[94:179]

plot(c(1:86), apply(Full_data_wide_plotrent, 2, mean),  ylim = c(1100,1600),  xlab = 'Time (YearMonths)', ylab = 'Avg. Rent', main = 'Mean Rent per Month', type = 'n')
abline(v = axTicks(1), h = axTicks(2), col = rgb(0.75, 0.75, 0.75, alpha = 0.5), lty = 3)
lines(c(1:86), apply(Full_data_wide_plotrent, 2, mean), type = 'b', pch = 1, lty = 2)

## TS Modeling 
Full_data_long$time <- as.Date(strptime(paste(1, Full_data_long$time),"%d %Y-%m"))

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


#yelp_photos <- jsonlite::stream_in(file("dataset/photos.json"))

# Can't load in below directly as is. Need to figure out how to cut on import. 
yelp_review <- jsonlite::stream_in(file("dataset/review.json"))
yelp_tip <- jsonlite::stream_in(file("dataset/tip.json"))
yelp_user <- jsonlite::stream_in(file("dataset/user.json"))


# Next Questions to ask: 
#1. Mean Stars Over Time (Looks like there might be too much missingness based on how I've broken it down with Phoenix. Not sure if this is a problem with what I did or a problem with the underlying data itself. ??
# Multilevel modeling? Is that useful here? 



