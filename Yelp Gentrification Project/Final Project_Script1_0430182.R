## Final Project Script ##

##################### Setting Up ########################
# Set WD where the files are. 
setwd("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester4MPP/DataScienceIntro")

# Load in required packages 
library(zoo)
library(readr)
library(tidyr)
library(dplyr)

################33## Importing in the Data ##################


############### Import Yelp Data 
yelp_review_long <- read.csv("yelplongPC_updated.csv", header = T, na.strings=c(""))
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
write.csv(Full_data_long, "OfficialLongCombinedUpdated.csv")
write.csv(Full_data_long_ex, "OfficialGrigCombinedUpdated.csv")

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



# https://msperlin.github.io/pafdR/models.html
# 1. Panel Data Models 
  # The main motivation to use panel data models is to allow common effects within the groups.
# We're essentially assuming different coefficients for each firm
# We can test the model specification using package plm. Function phtest executes the Hausman test (Hausman 1978), a statistical procedure that tests the null hypothesis that the best model is the random effects and not the fixed effect. Let's try it for our data.

names(Full_data_long)

# How many lags should we do. Confusing. 
pacf(Full_data_long$rentprice, na.action = na.pass)
pacf(Full_data_long$starsav, na.action = na.pass)

# Panel Data Model Using Machine Learning Approach
library(plm)

# Create train and test set using the last 12 months (1 year) for the test set 
table(as.factor(Full_data_long$time))
Full_data_long_train <- Full_data_long[Full_data_long$time < "2017-01-01",]
Full_data_long_test <- Full_data_long[Full_data_long$time >= "2017-01-01",]

# Build the mixed effects model (Can I do this before the RF or do I have to do it after somehow?)
# Hausman Test
# set options for Hausman test
my.formula <- rentprice ~ starsav + Avnumreviews + is_openave + funnyav + coolav + usefulav
# Had too take standard deviation out because it led to error of:

# Error in solve.default(crossprod(ZBeta)) : 
#   Lapack routine dgesv: system is exactly singular: U[20,20] = 0

# It looks like there was a lot of missingness, so that might've been part of the proble. 

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

# MAPE is only 21.85% right now (Capture variance. Might need to go back to google cloud)

# A simple test for whether autocorrelation should be included in the linear mixed effects model is to compare the predictive power of the model with and without autocorrelation.
  # To test for autocorrelation in a linear mixed effects model more formally, we can use a likelihood ratio test
  # This test compares the log-likelihoods of the mixed effects fits in-sample with and without autocorrelation, correcting for the additional degrees of freedom (and therefore potential for overfitting) that the linear mixed effects model with autocorrelation uses

## Panel Data Model with Lags for both (Mixed effects with autocorrelation is what we want the mmost, right?)
  # Random forest is definitely not useful with one variable though. # The point of using rando forest would be to test the importance of each explanatory variable though right? 

# I think plm is the base model and everything else is essentially an add on (GARCH/Arch based on the autocorrelation and heteroskedasticity and RF based on having a lot of variables to choose from)

###### Lagging dependent
## Figure out why below did not work!!!
lg <- function(x)c(NA, x[1:(length(x)-1)])
Full_data_long$rentpriceL1 <-unlist(tapply(Full_data_long$rentprice, Full_data_long$postal_code, lg))

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

yelp_checkin <- jsonlite::stream_in(file("dataset/checkin.json"))

str(yelp_checkin)


# Next Questions to ask: 
#1. Mean Stars Over Time (Looks like there might be too much missingness based on how I've broken it down with Phoenix. Not sure if this is a problem with what I did or a problem with the underlying data itself. ??
# Multilevel modeling? Is that useful here? 

# Counts of stars for standard deviation (Try both)

# We're not capturing variance. Throw in the variance too (SD). 

# See if we can do random forest represented as timee (Give each time a unique ID)
# USe correlograms to look at how many 
  # Look into 


# Would it make sense at all to impute on the Yelp set? Definitely impute on the zillow. 