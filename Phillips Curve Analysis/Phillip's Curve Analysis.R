#### Phillip's Curve Analysis 
################ Part 1: Does the Phillip's Curve Apply to the US? ######################
# This is raw - Will turn into rmd and integrate into ppt

# Set WD
setwd("~/GeorgetownMPPMSFS/Internship Coworking Opportunities/FI Consulting")

# Load in required packages 
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(zoo)
library(gridExtra)
library(grid)
# install.packages('ggthemes')
library(ggthemes)
# install.packages('stargazer')
library(stargazer)
library(car)

# Load  in the data 
casedf <- read.csv('Case_Study_Data.csv', stringsAsFactors = FALSE)
head(casedf)

############## Cleaning the Data and Shaping the Data 
table(casedf$Data)
# We are missing labor force data for certain time periods because it is collected quarterly
# There is missingness in unemployment too, but only in the first year of data collection, which we will not get inflation for anyways

# Synchronize data labels 
unique(casedf$Data)
summary(casedf[casedf$Data == "Consumer Price Index",]$Value)
summary(casedf[casedf$Data == "CPI",]$Value)
casedf$Data[casedf$Data == "Consumer Price Index"] <- c("CPI")

# Convert to Wide Form 
casedf_wide <- spread(casedf, key = Data, value = Value)
# Create Quarter Indicator 
casedf_wide$Quarter <- ifelse(casedf_wide$Month <= 3,"Q1", ifelse(casedf_wide$Month > 3 & casedf_wide$Month <= 6, "Q2", ifelse(casedf_wide$Month > 6 & casedf_wide$Month <= 9, "Q3", ifelse(casedf_wide$Month > 9, "Q4", NA))))

# Look at Missingness across variables 
sapply(casedf_wide, function(x) sum(is.na(x)))
# We are missing unemployment only for 1947
# Keep in mind CLF data is quarterly and CPI and employment are monthly 

# Calculate the monthly inflation rate using the percent change in the monthly CPI
casedf_wide$inflation <- (casedf_wide$CPI - lag(casedf_wide$CPI,1))/lag(casedf_wide$CPI,1)

############ Collapse data to quarterly values 
# NOTE: We can't just simply average the monthly inflation rates to get the quarterly inflation rate because the rates are compounded and thus multiplicative. In other words, if inflation was 10% one month and 10% the next month, then over the two months prices went up 1.1*1.1 = 1.21 = 21%.

# Quarterly average data are derived by adding up the monthly data for each industry and dividing by 3 to determine an actual annual average

# Prep Inflation Number - Add one to all of them to get conversion factors (i.e., so that 0% inflation is converted to the number 1).
casedf_wide$inflationforcol <- casedf_wide$inflation+1

# Prep CLF
casedf_wide$`Civilian Labor Force`[is.na(casedf_wide$`Civilian Labor Force`)] <- 0

# Quarterly Phillips 
casedf_wide_quarter <- casedf_wide %>%
  group_by(Year,Quarter) %>%
  dplyr::summarize(inflation = (prod(inflationforcol) -1) *100,
                   unemployment = mean(`Unemployment Level`),
                   clf = sum(`Civilian Labor Force`),
                   cpi = mean(CPI))

casedf_wide_quarter$uerate <- (casedf_wide_quarter$unemployment/casedf_wide_quarter$clf) * 100

############### Exploratory Data Analysis 
casedf_wide_quarter$YearQuarter <- paste(as.character(casedf_wide_quarter$Year), as.character(casedf_wide_quarter$Quarter), sep = " ")

# Plot the variables of interest - The Phillips Curve 
ggplot(casedf_wide_quarter, aes(x=uerate, y=inflation)) + geom_point()
# It does not look as though the Phillips Curve really applies to the US economy

plot1 <- ggplot(casedf_wide_quarter, aes(x = uerate, y = inflation)) + geom_point(alpha = 0.01) + geom_smooth(se = FALSE, method = "lm") + geom_point() + labs(y = "Inflation Rate (%)", x = "Unemployment Rate (%)", title = "The Supposed Phillip's Curve of the United States") + scale_x_continuous(breaks=seq(0, 12, 1)) + scale_y_continuous(breaks=seq(-4, 5, 1)) + theme_economist() + scale_fill_economist()

title1=textGrob("Source: U.S. Federal Reserve", gp=gpar(fontface="italic", fontsize = 9), hjust =3)

grid.arrange(plot1, bottom=title1)

#save
g <- arrangeGrob(plot1, bottom=title1) #generates g
ggsave(file="FullPh.jpg", g, width=11, height=4) #saves g

# In fact, just as previous research has suggested, there is a flattening out (Include in Presentation)

######### How about if we break it down by decade?

# 1950-1959
plot2 <- ggplot(casedf_wide_quarter[casedf_wide_quarter$Year >= 1950 & casedf_wide_quarter$Year < 1960,], aes(x = uerate, y = inflation)) + geom_point(alpha = 0.01) + geom_smooth(se = FALSE, method = "lm") + geom_point() + labs(y = "Inflation Rate (%)", x = "Unemployment Rate (%)", title = "1950-1959") + scale_x_continuous(breaks=seq(0, 12, 1)) + scale_y_continuous(breaks=seq(-4, 5, 1)) + theme_economist() + scale_fill_economist()
# Does not appear to hold, but not completely flat

title1=textGrob("Source: U.S. Federal Reserve", gp=gpar(fontface="italic", fontsize = 9), hjust =3)
grid.arrange(plot2, bottom=title1)

#save
g <- arrangeGrob(plot2, bottom=title1) #generates g
ggsave(file="195059Ph.jpg", g, width=5, height=5) #saves g

# 1960-1969
plot3 <- ggplot(casedf_wide_quarter[casedf_wide_quarter$Year >= 1960 & casedf_wide_quarter$Year < 1969,], aes(x = uerate, y = inflation)) + geom_point(alpha = 0.01) + geom_smooth(se = FALSE, method = "lm") + geom_point() + labs(y = "Inflation Rate (%)", x = "Unemployment Rate (%)", title = "1960-1969") + scale_x_continuous(breaks=seq(0, 12, 1)) + scale_y_continuous(breaks=seq(-4, 5, 1)) + theme_economist() + scale_fill_economist()
# Appears to hold 

title1=textGrob("Source: U.S. Federal Reserve", gp=gpar(fontface="italic", fontsize = 9), hjust =3)
grid.arrange(plot3, bottom=title1)

#save
g <- arrangeGrob(plot3, bottom=title1) #generates g
ggsave(file="196069Ph.jpg", g, width=5, height=5) #saves g

# 1970-1979
plot4 <- ggplot(casedf_wide_quarter[casedf_wide_quarter$Year >= 1970 & casedf_wide_quarter$Year < 1979,], aes(x = uerate, y = inflation)) + geom_point(alpha = 0.01) + geom_smooth(se = FALSE, method = "lm") + geom_point() + labs(y = "Inflation Rate (%)", x = "Unemployment Rate (%)", title = "1970-1979") + scale_x_continuous(breaks=seq(0, 12, 1)) + scale_y_continuous(breaks=seq(-4, 5, 1)) + theme_economist() + scale_fill_economist()
# Does not appear to hold 

title1=textGrob("Source: U.S. Federal Reserve", gp=gpar(fontface="italic", fontsize = 9), hjust =3)
grid.arrange(plot4, bottom=title1)

#save
g <- arrangeGrob(plot4, bottom=title1) #generates g
ggsave(file="197079Ph.jpg", g, width=5, height=5) #saves g

# 1980-1989
plot5 <- ggplot(casedf_wide_quarter[casedf_wide_quarter$Year >= 1980 & casedf_wide_quarter$Year < 1989,], aes(x = uerate, y = inflation)) + geom_point(alpha = 0.01) + geom_smooth(se = FALSE, method = "lm") + geom_point() + labs(y = "Inflation Rate (%)", x = "Unemployment Rate (%)", title = "1980-1989") + scale_x_continuous(breaks=seq(0, 12, 1)) + scale_y_continuous(breaks=seq(-4, 5, 1)) + theme_economist() + scale_fill_economist()
# Does not appear to hold, but there is soe decline on the right side  

title1=textGrob("Source: U.S. Federal Reserve", gp=gpar(fontface="italic", fontsize = 9), hjust =3)
grid.arrange(plot5, bottom=title1)

#save
g <- arrangeGrob(plot5, bottom=title1) #generates g
ggsave(file="198089Ph.jpg", g, width=5, height=5) #saves g

# 1990-1999
plot6 <- ggplot(casedf_wide_quarter[casedf_wide_quarter$Year >= 1990 & casedf_wide_quarter$Year < 1999,], aes(x = uerate, y = inflation)) + geom_point(alpha = 0.01) + geom_smooth(se = FALSE, method = "lm") + geom_point() + labs(y = "Inflation Rate (%)", x = "Unemployment Rate (%)", title = "1990-1999") + scale_x_continuous(breaks=seq(0, 12, 1)) + scale_y_continuous(breaks=seq(-4, 5, 1)) + theme_economist() + scale_fill_economist()
# Does not appear to hold 

title1=textGrob("Source: U.S. Federal Reserve", gp=gpar(fontface="italic", fontsize = 9), hjust =3)
grid.arrange(plot6, bottom=title1)

#save
g <- arrangeGrob(plot6, bottom=title1) #generates g
ggsave(file="199099Ph.jpg", g, width=5, height=5) #saves g

# 2000-2009
plot7 <- ggplot(casedf_wide_quarter[casedf_wide_quarter$Year >= 2000 & casedf_wide_quarter$Year < 2009,], aes(x = uerate, y = inflation)) + geom_point(alpha = 0.01) + geom_smooth(se = FALSE, method = "lm") + geom_point() + labs(y = "Inflation Rate (%)", x = "Unemployment Rate (%)", title = "2000-2009") + scale_x_continuous(breaks=seq(0, 12, 1)) + scale_y_continuous(breaks=seq(-4, 5, 1)) + theme_economist() + scale_fill_economist()
# Does not appear to hold (outlier distorts to make it look like it does)

title1=textGrob("Source: U.S. Federal Reserve", gp=gpar(fontface="italic", fontsize = 9), hjust =3)
grid.arrange(plot7, bottom=title1)

#save
g <- arrangeGrob(plot7, bottom=title1) #generates g
ggsave(file="200009Ph.jpg", g, width=5, height=5) #saves g

# 2010-2016
plot8 <- ggplot(casedf_wide_quarter[casedf_wide_quarter$Year >= 2010 & casedf_wide_quarter$Year < 2017,], aes(x = uerate, y = inflation)) + geom_point(alpha = 0.01) + geom_smooth(se = FALSE, method = "lm") + geom_point() + labs(y = "Inflation Rate (%)", x = "Unemployment Rate (%)", title = "2010-2016") + scale_x_continuous(breaks=seq(0, 12, 1)) + scale_y_continuous(breaks=seq(-4, 5, 1)) + theme_economist() + scale_fill_economist()
# Does not appear to hold (outlier distorts to make it look like it does)

title1=textGrob("Source: U.S. Federal Reserve", gp=gpar(fontface="italic", fontsize = 9), hjust =3)
grid.arrange(plot8, bottom=title1)

#save
g <- arrangeGrob(plot8, bottom=title1) #generates g
ggsave(file="201016.jpg", g, width=5, height=5) #saves g

################ Regression Analysis 
#### First look for autocorrelation

# Because we are dealing with time series data, we have to account for the fact that data from one year depends on data from the year before 

# This approach treats the year-to-year dependence as the result of autocorrelated errors 
####### Estimate basic regression model 
casedf_wide_quarter$YearQuarter <- as.yearqtr(casedf_wide_quarter$YearQuarter, format = "%Y Q%q")
casedf_wide_quarter <- casedf_wide_quarter[casedf_wide_quarter$Year != 1947 & casedf_wide_quarter$Year != 2017,]

regbasic<- lm(inflation ~ uerate, data = casedf_wide_quarter)
summary(regbasic)

# Highly statistically insignificant, and the fact that correcting autocorrelation only typically leads to wider standard errors means that it will still likely be insignificant after controlling for autocorrelation. 

# Estimate with a quadratic
casedf_wide_quarter$ueratesqare <- casedf_wide_full_quarter$uerate^2
basicregquad <- lm(inflation ~ uerate + ueratesqare, data = casedf_wide_quarter)
summary(basicregquad)

# Captures the negative relationship better, but still statistically insignificant

# Estimate with Lags for unemployment 

casedf_wide_quarter$uelag1 <- lag(casedf_wide_quarter$uerate, 1)
casedf_wide_quarter$uelag2 <- lag(casedf_wide_quarter$uerate, 2) 
casedf_wide_quarter$uelag3 <- lag(casedf_wide_quarter$uerate, 3) 
casedf_wide_quarter$uelag4 <- lag(casedf_wide_quarter$uerate, 4) 

basicreguelag1 <- lm(inflation ~ uelag1, data = casedf_wide_quarter)
summary(basicreguelag1)

# Still not statistically significant and no negative relationship shown

# Lag 4 for unemployment as is common practice 
casedf_wide_quarter$uelag4 <- lag(casedf_wide_quarter$uerate, 1)
basicreguelag4 <- lm(inflation ~ uelag1 + uelag2 + uelag3 + uelag4, data = casedf_wide_quarter)
summary(basicreguelag4)

# Still not statistically significant and no negative relationship shown

# Esimtate with various lags on unemployment under assumption that there is stickiness in that low unemployment will take time to translate into higher inflation

# Look at errors for each variable graphically 
# Inflation
inf <- lm(inflation ~ YearQuarter, data = casedf_wide_quarter)
plot(casedf_wide_quarter$YearQuarter, resid(inf))
abline(lm(resid(inf) ~ casedf_wide_quarter$YearQuarter))

# Unemployment 
une <- lm(uerate ~ YearQuarter, data = casedf_wide_quarter)
plot(casedf_wide_quarter$YearQuarter, resid(une))
abline(lm(resid(une) ~ casedf_wide_quarter$YearQuarter))

# Auxiliary Regression Approach to Detecting Autocorrelation
# We estimate et = pet-1 +vt and if p is stat sig, we have evidence of autocorrelation
# For just the dependent variable (We'll use inflation)
pcols_inf <- lm(inflation ~ YearQuarter, data = casedf_wide_quarter)
summary(pcols_inf)
# Save residuals 
Err_inf <- resid(pcols_inf)
# Plot residuals over time 
plot(casedf_wide_quarter$YearQuarter, Err_inf)
# Generate Lagged Error Variable 
LagErr_inf <- c(NA, Err_inf[1:(length(Err_inf)-1)])
# Auxiliary regression
LagErrOLS_inf <- lm(Err_inf~LagErr_inf)
# Display results
summary(LagErrOLS_inf)
# Conclusion: Autocorrelation definitely exists for inflation 

# For the independent variable (unemployment)
pcols_uerate <- lm(uerate ~ YearQuarter, data = casedf_wide_quarter)
summary(pcols_uerate)
# Save residuals 
Err_uerate <- resid(pcols_uerate)
# Plot residuals over time 
plot(casedf_wide_quarter$YearQuarter, Err_uerate)
# Generate Lagged Error Variable 
LagErr_uerate <- c(NA, Err_uerate[1:(length(Err_uerate)-1)])
# Auxiliary regression
LagErrOLS_uerate <- lm(Err_uerate~LagErr_uerate)
# Display results
summary(LagErrOLS_uerate)
# There is definitely autocorrelation

# For both variables (Most relevant)
pcols <- lm(inflation ~ uerate, data = casedf_wide_quarter)
summary(pcols)
# Save residuals 
Err <- resid(pcols)
# Plot residuals over time 
plot(casedf_wide_quarter$YearQuarter, Err)
# Generate Lagged Error Variable 
LagErr <- c(NA, Err[1:(length(Err)-1)])
# Auxiliary regression
LagErrOLS <- lm(Err~LagErr)
# Display results
summary(LagErrOLS)
# Conclusion: Autocorrelation definitely exists

############ Correcting for autocorrelation 

### p-tranforming the data - Transform the model from one that suffers from autocorrelation to one that does not
# For each value, we're essentially subtracting the actual observation at t-1 times p from the actual observation at t for the variable
# Transforming in this way gives us unbiased and consistent coefficient estimates and more accurate standard errors
# Keep in mind the value of B1 shouldn't change at all, just the SEs

# Rho is rho-hat 
Rho <- summary(LagErrOLS)$coefficients[2]
# Length of Y
N <- length(casedf_wide_quarter$inflation)
# Lagged Y
LagInf <- lag(casedf_wide_quarter$inflation, 1)
# Lagged X
LagUnemp <- lag(casedf_wide_quarter$unemployment, 1)
# Rho-transformed Y
InfRho <- casedf_wide_quarter$inflation - Rho*LagInf
# Rho-transformed X
UnempRho <- casedf_wide_quarter$unemployment - Rho*LagUnemp
# Rho-transformed model
pcolsRho <- lm(InfRho ~ UnempRho)
summary(pcolsRho)
# Even afer identifying and controlling for autocorrelation, there is no statistically signficant effect that shows an inverse relationship between inflation and unemployment in the US

####### Dynamic Model 
# This second approach to time series data treats the dependent variable in one period as directly depending on what the value of the dependent variable was in the previous period. This is accomplished by including a lagged dependent variable as an independent variable   

LagInf <- lag(casedf_wide_quarter$inflation)
dynreg <- lm(inflation ~ LagInf + uerate, data = casedf_wide_quarter)
summary(dynreg)
# Even afer identifying and controlling for autocorrelation, there is no statistically signficant effect that shows an inverse relationship between inflation and unemployment in the US

# With quadratic 
dynreg2 <- lm(inflation ~ LagInf + uerate + ueratesqare, data = casedf_wide_quarter)
summary(dynreg2)

# Again seems to show the negative relationship better, but still not statistically significant 

# Estimate with Lags for unemployment 
dynbasicreguelag1 <- lm(inflation ~ LagInf + uelag1, data = casedf_wide_quarter)
summary(dynbasicreguelag1)

# Still not statistically significant and no negative relationship shown

# Lag 4 for unemployment as is common practice 
dynbasicreguelag4 <- lm(inflation ~ LagInf + uelag1 + uelag2 + uelag3 + uelag4, data = casedf_wide_quarter)
summary(dynbasicreguelag4)

##### Output Reg Results
stargazer(regbasic, basicregquad, pcolsRho, dynreg, dynreg2, dynbasicreguelag1, dynbasicreguelag4, out="Phillip's Curve.htm", style = "aer", title = "Evaluating the Phillip's Curve: Relationship Between Unemployment and Inflation", covariate.labels = c("Lagged Inflation", "Unemployment Rate (%)", "Unemployment Rate (%) - Squared", "Rho Transformed Unemployment Rate (%)", "Lagged Unemployment (1)", "Lagged Unemployment (2)", "Lagged Unemployment (3)", "Lagged Unemployment (4)"), add.lines = list(c("Dynamic",rep("NO", 3), rep("YES", 4)), c("Quadratic", "NO", "YES", "NO", "NO", "YES", "NO", "NO"), c("Rho Tranformed", "NO", "NO", "YES", "NO", "NO", "NO", "NO")), column.labels = c("Inflation (%)", "Inflation (%)", "Rho Transformed Inflation (%)", "Inflation (%)", "Inflation (%)", "Inflation (%)", "Inflation (%)"), dep.var.labels.include = FALSE, omit = c("Constant", "uelag4"))

# Results of same tests above applied for each decade omitted as also all demonstrated statistical insignificance. 

############ Part 2: Explore the relationship between inflation and the unemployment rate from a unique perspective not previously explored by William Phillips ###################

############ Loading in the supporting Data 
# Load  in the data 
# Historical Fed Funds Rate (1950-Present, Monthly) [https://fred.stlouisfed.org/series/INTDSRUSM193N]
Ints <- read.csv('INTDSRUSM193N.csv', stringsAsFactors = FALSE)
# Natural employment/NROU (1949-2029, Quarterly) [https://fred.stlouisfed.org/series/NROU]
NROU <- read.csv('NROU.csv', stringsAsFactors = FALSE)
# Average Hourly Wages - AVERAGE HOURLY EARNINGS OF PRODUCTION AND NONSUPERVISORY EMPLOYEES (1964-2019, Monthly)  [https://data.bls.gov/pdq/SurveyOutputServlet]
Wages <- read.csv('SeriesReport-20190609141253_3bd55d.csv', stringsAsFactors = FALSE, skip = 12)
# Import Productivity Data (1948-2018, Annual)
produ <- read.csv("Copy of nonfarm_business-annual-series.csv", skip=6)

# Create Dummy to signify inflation rate targeting policy adoption in US
casedf_wide_quarter$IRtarg <- ifelse((casedf_wide_quarter$Year > 2011), 1, 0)

# Merge everything to existing dataset based on month and year
######### Prepare datasets for merge
# FFR
Ints$Year <- as.integer(format(as.Date(Ints$DATE, format="%Y-%m-%d"),"%Y"))
Ints$Month <- format(as.Date(Ints$DATE, format="%Y-%m-%d"),"%m")
Ints$Month <- as.integer(Ints$Month)

# Collapse to quarterly (i.e. quarter ending rate)
# Create Quarter Indicator 
Ints$Quarter <- ifelse(Ints$Month <= 3,"Q1", ifelse(Ints$Month > 3 & Ints$Month <= 6, "Q2", ifelse(Ints$Month > 6 & Ints$Month <= 9, "Q3", ifelse(Ints$Month > 9, "Q4", NA))))
Ints_Quarter <- Ints[Ints$Month == 3 | Ints$Month == 6 | Ints$Month == 9 | Ints$Month == 12,]

Ints_Quarter$YearQuarter <- paste(as.character(Ints_Quarter$Year), as.character(Ints_Quarter$Quarter), sep = " ")
Ints_Quarter$YearQuarter <- as.yearqtr(Ints_Quarter$YearQuarter, format = "%Y Q%q")

# NROU
NROU$Year <- as.integer(format(as.Date(NROU$DATE, format="%Y-%m-%d"),"%Y"))
NROU$Month <- format(as.Date(NROU$DATE, format="%Y-%m-%d"),"%m")
NROU$Month <- as.integer(NROU$Month)

NROU$Quarter <- ifelse(NROU$Month <= 3,"Q1", ifelse(NROU$Month > 3 & NROU$Month <= 6, "Q2", ifelse(NROU$Month > 6 & NROU$Month <= 9, "Q3", ifelse(NROU$Month > 9, "Q4", NA))))
                       
NROU$YearQuarter <- paste(as.character(NROU$Year), as.character(NROU$Quarter), sep = " ")
NROU$YearQuarter <- as.yearqtr(NROU$YearQuarter, format = "%Y Q%q")
                      
# Wages
# Convert from wide to long 
Wages_long <- gather(Wages, key = Month, value = HourWage, Jan:Dec)
Wages_long$Month <- match(Wages_long$Month,month.abb)

Wages_long$Quarter <- ifelse(Wages_long$Month <= 3,"Q1", ifelse(Wages_long$Month > 3 & Wages_long$Month <= 6, "Q2", ifelse(Wages_long$Month > 6 & Wages_long$Month <= 9, "Q3", ifelse(Wages_long$Month > 9, "Q4", NA))))

Wages_long_quarter <- Wages_long %>%
  group_by(Year, Quarter) %>%
  dplyr::summarize(HourWage = mean(HourWage))

Wages_long_quarter$YearQuarter <- paste(as.character(Wages_long_quarter$Year), as.character(Wages_long_quarter$Quarter), sep = " ")
Wages_long_quarter$YearQuarter <- as.yearqtr(Wages_long_quarter$YearQuarter, format = "%Y Q%q")

##### Merge to existing datasets
######## For the monthly/quarterly data 
casedf_wide_full <- left_join(casedf_wide, Ints, by = c("Year", "Month"))
casedf_wide_full <- left_join(casedf_wide_full, NROU, by = c("Year", "Month"))
casedf_wide_full <- left_join(casedf_wide_full, Wages_long, by = c("Year", "Month"))
casedf_wide_full <- casedf_wide_full[,c(1:7, 10, 13, 15:16)]
names(casedf_wide_full)
colnames(casedf_wide_full)[6] <- c("Quarter")
colnames(casedf_wide_full)[10] <- c("YearQuarter")

## For the quarterly data
casedf_wide_quarter <- casedf_wide_quarter[,-c(10:13)]
casedf_wide_full_quarter <- left_join(casedf_wide_quarter, Ints_Quarter, by = "YearQuarter")
casedf_wide_full_quarter <- left_join(casedf_wide_full_quarter, NROU, by = "YearQuarter")
casedf_wide_full_quarter <- left_join(casedf_wide_full_quarter, Wages_long_quarter, by = "YearQuarter")
names(casedf_wide_full_quarter)
casedf_wide_full_quarter <- casedf_wide_full_quarter[,c(1:10,12,14, 17, 23)]
colnames(casedf_wide_full_quarter)[1] <- c("Year")
colnames(casedf_wide_full_quarter)[2] <- c("Quarter")
colnames(casedf_wide_full_quarter)[12] <- c("Month")
names(casedf_wide_full_quarter)

# Add a recession indicator
casedf_wide_full_quarter$recession <- ifelse(casedf_wide_full_quarter$YearQuarter >= "1953 Q2" & casedf_wide_full_quarter$YearQuarter <= "1954 Q2", "Recession", ifelse(casedf_wide_full_quarter$YearQuarter >= "1957 Q3" & casedf_wide_full_quarter$YearQuarter <= "1958 Q3", "Recession", ifelse(casedf_wide_full_quarter$YearQuarter >= "1960 Q2" & casedf_wide_full_quarter$YearQuarter <= "1961 Q1", "Recession", ifelse(casedf_wide_full_quarter$YearQuarter >= "1969 Q4" & casedf_wide_full_quarter$YearQuarter <= "1970 Q4", "Recession", ifelse(casedf_wide_full_quarter$YearQuarter >= "1973 Q4" & casedf_wide_full_quarter$YearQuarter <= "1975 Q1", "Recession", ifelse(casedf_wide_full_quarter$YearQuarter >= "1980 Q1" & casedf_wide_full_quarter$YearQuarter <= "1980 Q3", "Recession", ifelse(casedf_wide_full_quarter$YearQuarter >= "1981 Q3" & casedf_wide_full_quarter$YearQuarter <= "1982 Q4", "Recession", ifelse(casedf_wide_full_quarter$YearQuarter >= "1990 Q3" & casedf_wide_full_quarter$YearQuarter <= "1991 Q1", "Recession", ifelse(casedf_wide_full_quarter$YearQuarter >= "2001 Q1" & casedf_wide_full_quarter$YearQuarter <= "2001 Q4", "Recession", ifelse(casedf_wide_full_quarter$YearQuarter >= "2007 Q4" & casedf_wide_full_quarter$YearQuarter <= "2009 Q2", "Recession", "Non-Recession"))))))))))

############################# EDA #####################################
########### Wages vs Unemployment
# Under standard economic thinking, unemployment below a certain level, by raising incomes and wages, leads to higher inflation
# We have data on wages from 1964-2019 and data on unemployment from 1948-2016, so we'll restrict the data to 1964-2016
casedf_wide_full_quarter_wageue <- casedf_wide_full_quarter[casedf_wide_full_quarter$Year > 1963 & casedf_wide_full_quarter$Year < 2017,c(1:3, 6:8,14)]

# Create indicator variables to show time periods (After 2010, Before 2010 to start)
# casedf_wide_full_quarter_wageue$postrec <- ifelse(casedf_wide_full_quarter_wageue$Year < 2010,"Before 2010", ifelse(casedf_wide_full_quarter_wageue$Year >= 2010, "2010 and After", NA))

casedf_wide_full_quarter_wageue$decade <- ifelse(casedf_wide_full_quarter_wageue$Year >= 1950 & casedf_wide_full_quarter_wageue$Year < 1960,"1950-1959", ifelse(casedf_wide_full_quarter_wageue$Year >= 1960 & casedf_wide_full_quarter_wageue$Year < 1970,"1960-1969", ifelse(casedf_wide_full_quarter_wageue$Year >= 1970 & casedf_wide_full_quarter_wageue$Year < 1980,"1970-1979", ifelse(casedf_wide_full_quarter_wageue$Year >= 1980 & casedf_wide_full_quarter_wageue$Year < 1990,"1980-1989", ifelse(casedf_wide_full_quarter_wageue$Year >= 1990 & casedf_wide_full_quarter_wageue$Year < 2000,"1990-1999", ifelse(casedf_wide_full_quarter_wageue$Year >= 2000 & casedf_wide_full_quarter_wageue$Year < 2010,"2000-2009", ifelse(casedf_wide_full_quarter_wageue$Year >= 2010,"2010-2016", NA)))))))

# 4 Quarter Lag unemployment rate as is common 
casedf_wide_full_quarter_wageue$uerateLag <- lag(casedf_wide_full_quarter_wageue$uerate, 4)

# Convert to Change in avg hourly wages 
casedf_wide_full_quarter_wageue$wagechange <- 100 * (casedf_wide_full_quarter_wageue$HourWage/lag(casedf_wide_full_quarter_wageue$HourWage) - 1)

# we will use quarterly time periods 
plot9 <- ggplot(casedf_wide_full_quarter_wageue, aes(x=uerateLag, y=wagechange, col= decade)) + geom_point() + geom_smooth(method = "lm", fill = NA) + labs(y = "Wage Change (% Q/Q)", x = "Unemployment Rate (4 Quarter Lag)", title = "Unemployment is not Raising Income and Wages as Economists Suggest", subtitle = "Employers Not Paying Higher Wages to Fill Jobs - Flattening Out Post-2010") + scale_y_continuous(breaks=seq(0, 3.5, 0.2)) + scale_x_continuous(breaks=seq(0, 11, 0.5)) + theme_economist() + scale_fill_economist() + geom_text(x=4.65, y=1.3, label="1960-1969", col = "red4") + geom_text(x=8.5, y=2.1, label="1970-1979", col = "chartreuse4") + geom_text(x=10, y=1.05, label="1980-1989", col = "blue4") + geom_text(x=6.5, y=0.9, label="1990-1999", col = "grey3") + geom_text(x=4.2, y=.5, label="2000-2009", col = "orange1") + geom_text(x=9.5, y=.3, label="2010-2016", col = "darkmagenta") + scale_color_manual(breaks = c("1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2016"), values=c("red4", "chartreuse4", "blue4", "grey3", "orange1", "darkmagenta")) + theme(legend.position="none") 

title2=textGrob("Source: U.S. Federal Reserve, BLS", gp=gpar(fontface="italic", fontsize = 9), hjust =3)
  
grid.arrange(plot9, bottom=title2)
#save
g <- arrangeGrob(plot9, bottom=title2) #generates g
ggsave(file="WagesUnmdecades.jpg", g, width=11, height=6) #saves g

######## Comparing CPI and Unemployment over time with Natural Employment Included 
inuenr <- casedf_wide_full_quarter[,c(1:2, 8, 3, 7, 13, 15)]
inuenr$recdum <- ifelse((inuenr$recession == "Recession"), 1, 0)
inuenr$recdumcumsum <- ave(inuenr$recdum, inuenr$recdum, FUN=cumsum)
starts <- c(1, 6, 11, 15, 20, 26, 29, 35, 38, 42)
end <- c(5, 10, 14, 19, 25, 28, 34, 37, 41, 48)
inuenr$start <- ifelse((inuenr$recdumcumsum %in% starts), 1, 0)
inuenr$end <- ifelse((inuenr$recdumcumsum %in% end), 1, 0)
inuenr <- inuenr[,-c(8:9)]
start <- inuenr[inuenr$start == 1, 3]
end <- inuenr[inuenr$end == 1, 3]
recess <- cbind(start, end)
colnames(recess) <- c("start", "end")

inuenr <- gather(inuenr, Tag, Value, inflation:NROU)

plot10 <- ggplot(data = inuenr, aes(x = YearQuarter, y = Value, group = Tag)) +
geom_line(aes(colour=Tag)) + labs(x = "Quarter", title = "Phillip's Curve Appears to Hold More Strongly During Recessions", subtitle = "Recession Periods Represented with Grey Shading") + geom_rect(data = recess, inherit.aes = FALSE, aes(xmin = start, xmax = end, ymin = -Inf, ymax = +Inf), fill = 'darkgrey', alpha = 0.5) + scale_x_continuous(breaks=seq(1948, 2016, 1)) + theme_economist() + scale_fill_economist() + scale_color_manual(breaks = c("inflation", "uerate", "NROU"), values=c("red4", "chartreuse4", "blue4")) + theme(legend.position="none") + geom_text(x=1955, y= -2, label="Inflation Rate", col = "red4") + geom_text(x=1955, y=7.8, label="Unemployment Rate", col = "blue4") + geom_text(x=2009, y=3.8, label="Natural Unemployment Rate", col = "chartreuse4") +  theme(axis.text.x = element_text(angle = 90), axis.title.y = element_blank(), axis.title.x = element_blank())

title2=textGrob("Source: U.S. Federal Reserve, BLS", gp=gpar(fontface="italic", fontsize = 9), hjust =3)

grid.arrange(plot10, bottom=title2)

#save
g <- arrangeGrob(plot10, bottom=title2) #generates g
ggsave(file="NaR Comp.jpg", g, width=11, height=6) #saves g


############ Plot Economic Slack Against CPI
inuenr2 <- casedf_wide_full_quarter[,c(1:2, 8, 3, 6:7, 13, 15)]
inuenr2$econslack <- inuenr2$NROU - inuenr2$uerate
inuenr2 <- inuenr2[,c(1:3, 5, 9, 8)]
inuenr2 <- gather(inuenr2, Tag, Value, cpi:econslack)

plot11 <- ggplot(data = inuenr2, aes(x = YearQuarter, y = Value, group = Tag)) +
  geom_line(aes(colour=Tag)) + labs(x = "Quarter", title = "Phillip's Curve: The Link Between Economic Slack and Inflation", subtitle = "Recession Periods Represented with Grey Shading") + geom_rect(data = recess, inherit.aes = FALSE, aes(xmin = start, xmax = end, ymin = -Inf, ymax = +Inf), fill = 'darkgrey', alpha = 0.5) + scale_x_continuous(breaks=seq(1948, 2016, 1)) + theme_economist() + scale_fill_economist() + scale_color_manual(breaks = c("cpi", "econslack"), values=c("red4", "blue4")) + theme(legend.position="none") + geom_text(x=1955, y= 35, label="CPI", col = "red4") + geom_text(x=1955, y=10, label="Economic Slack", col = "blue4") +  theme(axis.text.x = element_text(angle = 90), axis.title.y = element_blank(), axis.title.x = element_blank())

title2=textGrob("Source: U.S. Federal Reserve, BLS", gp=gpar(fontface="italic", fontsize = 9), hjust =3)

grid.arrange(plot11, bottom=title2)

#save
g <- arrangeGrob(plot11, bottom=title2) #generates g
ggsave(file="SlackComp.jpg", g, width=11, height=6) #saves g


######## Evaluating relationship with Interest Rates 
intar <- casedf_wide_full_quarter[casedf_wide_full_quarter$YearQuarter >= "1980 Q1",c(1:2, 8, 3, 11, 15)]

colnames(intar)[5] <- c("interestrate")
intar <- gather(intar, Tag, Value, inflation:interestrate)

plot12 <- ggplot(data = intar, aes(x = YearQuarter, y = Value, group = Tag)) +
  geom_line(aes(colour=Tag)) + labs(x = "Quarter", title = "The Effects of Inflation-Targeting Are Partly to Blame for the Flattening Phillip's Curve", subtitle = "Recession Periods Represented with Grey Shading") + geom_rect(data = recess, inherit.aes = FALSE, aes(xmin = start, xmax = end, ymin = -Inf, ymax = +Inf), fill = 'darkgrey', alpha = 0.5) + scale_x_continuous(breaks=seq(1980, 2016, 1), limits = c(1980, 2016)) + theme_economist() + scale_fill_economist() + scale_color_manual(breaks = c("inflation", "interestrate"), values=c("red4", "blue4")) + theme(legend.position="none") + geom_text(x=1980, y= -1, label="Inflation Rate", col = "red4") + geom_text(x=1980, y=9, label="Interest Rate", col = "blue4") + geom_text(x=2014, y=2.5, label="Inflation Target = 2%") + theme(axis.text.x = element_text(angle = 90), axis.title.y = element_blank(), axis.title.x = element_blank()) + geom_hline(yintercept=2)

title2=textGrob("Source: U.S. Federal Reserve, BLS", gp=gpar(fontface="italic", fontsize = 9), hjust =3)

grid.arrange(plot12, bottom=title2)

#save
g <- arrangeGrob(plot12, bottom=title2) #generates g
ggsave(file="IntRates.jpg", g, width=11, height=6) #saves g

############ PLot Wages and Productivity 
wapr <- produ[,c(1,3, 6)]
wapr <- gather(wapr, Tag, Value, Labor.productivity:Hourly.compensation)

plot13 <- ggplot(data = wapr, aes(x = Year, y = Value, group = Tag)) +
  geom_line(aes(colour=Tag)) + labs(x = "Year", title = "Are Wages Rising with Productivity Gains?", subtitle = "Comparing Annual Changes in Productivity and Hourly Wages") + geom_rect(data = recess, inherit.aes = FALSE, aes(xmin = start, xmax = end, ymin = -Inf, ymax = +Inf), fill = 'darkgrey', alpha = 0.5) + scale_x_continuous(breaks=seq(1948, 2016, 1)) + theme_economist() + scale_fill_economist() + scale_color_manual(breaks = c("Labor.productivity", "Hourly.Compensation"), values=c("red4", "blue4")) + theme(legend.position="none") + geom_text(x=1960, y= 6.5, label="Labor productivity", col = "red4") + geom_text(x=1962.55, y=.9, label="Hourly Compensation", col = "blue4") +  theme(axis.text.x = element_text(angle = 90), axis.title.y = element_blank(), axis.title.x = element_blank())

title2=textGrob("Source: U.S. Federal Reserve, BLS", gp=gpar(fontface="italic", fontsize = 9), hjust =3)

grid.arrange(plot13, bottom=title2)

#save
g <- arrangeGrob(plot13, bottom=title2) #generates g
ggsave(file="Producandwages.jpg", g, width=11, height=6) #saves g

############# Regression Analysis Including average hourly wages, unemployment rate, Natural Unemployment

######### Create Lag Vars
casedf_wide_full_quarter$LagInf <- lag(casedf_wide_full_quarter$inflation, 1)
casedf_wide_full_quarter$LagUE1 <- lag(casedf_wide_full_quarter$uerate, 1) 
casedf_wide_full_quarter$LagUE2 <- lag(casedf_wide_full_quarter$uerate, 2) 
casedf_wide_full_quarter$LagUE3 <- lag(casedf_wide_full_quarter$uerate, 3) 
casedf_wide_full_quarter$LagUE4 <- lag(casedf_wide_full_quarter$uerate, 4) 

# Normal Dynamic Model
dynreg <- lm(inflation ~ LagInf + uerate + INTDSRUSM193N + NROU + factor(recession) + HourWage + factor(IRtarg), data = casedf_wide_full_quarter)
summary(dynreg)
vif(dynreg)

# With Quadratic
dynregsq <- lm(inflation ~ LagInf + uerate + ueratesqare + INTDSRUSM193N + NROU + factor(recession) + HourWage + factor(IRtarg), data = casedf_wide_full_quarter)
summary(dynregsq)

# With Lagged Unemployment (1)
LagUE1dynreg <- lm(inflation ~ LagInf + LagUE1  + INTDSRUSM193N + NROU + factor(recession) + HourWage + factor(IRtarg), data = casedf_wide_full_quarter)
summary(LagUE1dynreg)

# Add the Quad 
casedf_wide_full_quarter$LagUE1SQ <- casedf_wide_full_quarter$LagUE1^2
QuadLagUE1dynreg <- lm(inflation ~ LagInf + LagUE1 + LagUE1SQ + INTDSRUSM193N + NROU + factor(recession) + HourWage + factor(IRtarg), data = casedf_wide_full_quarter)
summary(QuadLagUE1dynreg)

# With Lagged Unemployment (4)
LagUE4dynreg <- lm(inflation ~ LagInf + LagUE1 + LagUE2 + LagUE3 + LagUE4 + INTDSRUSM193N + NROU + factor(recession) + HourWage + factor(IRtarg), data = casedf_wide_full_quarter)
summary(LagUE4dynreg)

## Interaction Effects
casedf_wide_full_quarter$recdum <- ifelse((casedf_wide_full_quarter$recession == "Recession"), 1, 0)
casedf_wide_full_quarter$recueratedum <- casedf_wide_full_quarter$uerate * casedf_wide_full_quarter$recdum 

casedf_wide_full_quarter$ITcueratedum <- casedf_wide_full_quarter$uerate * casedf_wide_full_quarter$IRtarg

casedf_wide_full_quarter$IRcueratedum <- casedf_wide_full_quarter$uerate * casedf_wide_full_quarter$INTDSRUSM193N

dynreg2 <- lm(inflation ~ LagInf + uerate + INTDSRUSM193N + NROU + factor(recession) + HourWage + factor(IRtarg) + recueratedum + ITcueratedum + IRcueratedum, data = casedf_wide_full_quarter)
summary(dynreg2)

# Interest Rates and the existence of inflation targeting policy appear to play a role here, but we can run again eliminating multicollinearity 
vif(dynreg2)

# Take out collinear covariates
dynreg3 <- lm(inflation ~ LagInf + uerate + INTDSRUSM193N + factor(recession) + HourWage + factor(IRtarg), data = casedf_wide_full_quarter)
summary(dynreg3)
vif(dynreg3)

# All we see here is that interest rates heavily influence inflation

##### Output Reg Results
stargazer(dynreg, dynregsq, LagUE1dynreg, QuadLagUE1dynreg, LagUE4dynreg, dynreg2, dynreg3, out="Phillip's Curve Multivar.htm", style = "aer", title = "Evaluating the Phillip's Curve: Relationship Between Unemployment and Inflation", covariate.labels = c("Lagged Inflation", "Unemployment Rate (%)", "Unemployment Rate (%) - Squared", "Lagged Unemployment (1)","Lagged Unemployment (1) - Squared", "Lagged Unemployment (2)", "Lagged Unemployment (3)", "Lagged Unemployment (4)", "Interest Rate", "Natural Rate of Unemployment", "Recession Dummy", "Avg. Hourly Wages", "Inflation Targeting Dummy", "Recession Unemployment Interaction", "Inflation Targeting Unemployment Interaction", "Interest Rate Unemployment Interaction"), add.lines = list(c("Quadratic", "NO", "YES", "NO", "YES", "NO", "NO", "NO"), c("Interactions", "NO", "NO", "NO", "NO", "NO", "YES", "NO")), column.labels = c("Inflation (%)", "Inflation (%)", "Inflation (%)", "Inflation (%)", "Inflation (%)", "Inflation (%)", "Inflation (%)"), dep.var.labels.include = FALSE, omit = c("Constant"))
