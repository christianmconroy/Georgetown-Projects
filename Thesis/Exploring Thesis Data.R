##### Exploring Thesis Data ####

# Set WD
setwd("~/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis")
library(ggplot2)
library(reshape)
library(stargazer)
library(plm)
library(dplyr)

################################### Load in data 
# A New Panel Database on Business Incentives for Economic Development Offered by State and Local Governments in the United States: https://upjohn.org/models/bied/database.php?_ga=2.76074133.1665192574.1551896116-79091449.1551896116 

Incentives <- read.csv("Panel Incentives 10yr Default.csv", stringsAsFactors = FALSE)

# We start with 38610 observations and 16 variables 

################################## Create Dummies to Signify whether industry had an incentive in given years

# Create Dummies to signify no incentives vs incentives (regardless of degree)
Incentives$JCTC_Dum <- ifelse((Incentives$Job.Creation.Tax.Credit > 0), 1, 0)
Incentives$ITC_Dum <- ifelse((Incentives$Investment.Tax.Credit > 0), 1, 0)
Incentives$RDC_Dum <- ifelse((Incentives$Research.and.Development.Credit > 0), 1, 0)
Incentives$PTA_Dum <- ifelse((Incentives$Property.Tax.Abatement > 0), 1, 0)
Incentives$CJTS_Dum <- ifelse((Incentives$Customized.Job.Training.Subsidy > 0), 1, 0)

# Tally how many state-industry-year observations were receiving an incentive or not (regardless of degree)
table(Incentives$JCTC_Dum)
# Have: 12951
# Don't Have: 25659
table(Incentives$ITC_Dum)
# Have: 10818
# Don't Have: 27792
table(Incentives$RDC_Dum)
# Have: 17578
# Don't Have: 21032
table(Incentives$PTA_Dum)
# Have: 26601
# Don't Have: 11909
table(Incentives$CJTS_Dum)
# Have: 19894
# Don't Have: 18716

# Calculate when 1s Changes to 0 (Do same for industry)

##### JCTC

## Calculating which year would be best for "treatment" and Building Up Table for Explanation
Incentives$JCTC_Dum_CumSum <- ave(Incentives$JCTC_Dum, Incentives$State, Incentives$Industry, FUN=cumsum)
IncenChY_JCTC <- Incentives[Incentives$JCTC_Dum_CumSum == 1,]
names(IncenChY_JCTC)
IncenChY_JCTC_YearAgg <- as.data.frame(aggregate(. ~ Base.Year, IncenChY_JCTC[c(7, 22)], sum))
IncenChY_JCTC_YearAgg <- IncenChY_JCTC_YearAgg[IncenChY_JCTC_YearAgg$Base.Year > 1996,]
IncenChY_JCTC_YearAgg$Base.Year[which.max(IncenChY_JCTC_YearAgg$JCTC_Dum_CumSum)]
JCTC1 <- 
JCTC2 <- max(IncenChY_JCTC_YearAgg$JCTC_Dum_CumSum)

# How many states in that year? 
JCTC3 <- length(unique(Incentives[Incentives$Base.Year == 2004 & Incentives$JCTC_Dum_CumSum == 1,]$State))

##### Calculating for Control
IncenChy_JCTC_control <- Incentives[Incentives$JCTC_Dum_CumSum == 0,]
sum(IncenChy_JCTC_control$JCTC_Dum_CumSum == 0 & IncenChy_JCTC_control$Base.Year == 2015)
length(unique(IncenChy_JCTC_control[IncenChy_JCTC_control$JCTC_Dum_CumSum == 0 & IncenChy_JCTC_control$Base.Year == 2015,]$State))
# The Year with the most policy adoptions for JCTC is 1996. In this year, 155 industries across 5 states adopted JCTC. Would be able to potentially compare against 637 in 30 states that never adopt.

# If we look at just post 1997 when the data merges, 2004 appears to bt the best year. In this year, 62  industries across states adopted JCTC. Would be able to potentially compare against 637 in 30 states that never adopt.

###### ITC
Incentives$ITC_Dum_CumSum <- ave(Incentives$ITC_Dum, Incentives$State, Incentives$Industry, FUN=cumsum)
IncenChY_ITC <- Incentives[Incentives$ITC_Dum_CumSum == 1,]
names(IncenChY_ITC)
IncenChY_ITC_YearAgg <- as.data.frame(aggregate(. ~ Base.Year, IncenChY_ITC[c(7, 23)], sum))
IncenChY_ITC_YearAgg <- IncenChY_ITC_YearAgg[IncenChY_ITC_YearAgg$Base.Year > 1996,]
IncenChY_ITC_YearAgg$Base.Year[which.max(IncenChY_ITC_YearAgg$ITC_Dum_CumSum)]
max(IncenChY_ITC_YearAgg$ITC_Dum_CumSum)

# How many states in that year? 
length(unique(Incentives[Incentives$Base.Year == 1998 & Incentives$ITC_Dum_CumSum == 1,]$State))

##### Calculating for Control
IncenChy_ITC_control <- Incentives[Incentives$ITC_Dum_CumSum == 0,]
sum(IncenChy_ITC_control$ITC_Dum_CumSum == 0 & IncenChy_ITC_control$Base.Year == 2015)
length(unique(IncenChy_ITC_control[IncenChy_ITC_control$ITC_Dum_CumSum == 0 & IncenChy_ITC_control$Base.Year == 2015,]$State))

# The Year with the most policy adoptions for ITC is 1995. In this year, 100 industries across 4 states adopted ITC. Could compare against 865 in 31 states that never adopt. 

##### RDC
Incentives$RDC_Dum_CumSum <- ave(Incentives$RDC_Dum, Incentives$State, Incentives$Industry, FUN=cumsum)
IncenChY_RDC <- Incentives[Incentives$RDC_Dum_CumSum == 1,]
names(IncenChY_RDC)
IncenChY_RDC_YearAgg <- as.data.frame(aggregate(. ~ Base.Year, IncenChY_RDC[c(7, 24)], sum))
IncenChY_RDC_YearAgg <- IncenChY_RDC_YearAgg[IncenChY_RDC_YearAgg$Base.Year != 1990,]
IncenChY_RDC_YearAgg$Base.Year[which.max(IncenChY_RDC_YearAgg$RDC_Dum_CumSum)]
max(IncenChY_RDC_YearAgg$RDC_Dum_CumSum)

# How many states in that year? 
length(unique(Incentives[Incentives$Base.Year == 2000 & Incentives$RDC_Dum_CumSum == 1,]$State))

##### Calculating for Control
IncenChy_RDC_control <- Incentives[Incentives$RDC_Dum_CumSum == 0,]
sum(IncenChy_RDC_control$RDC_Dum_CumSum == 0 & IncenChy_RDC_control$Base.Year == 2015)
length(unique(IncenChy_RDC_control[IncenChy_RDC_control$RDC_Dum_CumSum == 0 & IncenChy_RDC_control$Base.Year == 2015,]$State))

# The Year with the most policy adoptions for RDC is 2000. In this year, 134 industries across 4 states adopted RDC. Could compare against 460 across 25 states that never adopt. 

##### PTA
Incentives$PTA_Dum_CumSum <- ave(Incentives$PTA_Dum, Incentives$State, Incentives$Industry, FUN=cumsum)
IncenChY_PTA <- Incentives[Incentives$PTA_Dum_CumSum == 1,]
names(IncenChY_PTA)
IncenChY_PTA_YearAgg <- as.data.frame(aggregate(. ~ Base.Year, IncenChY_PTA[c(7, 25)], sum))
IncenChY_PTA_YearAgg <- IncenChY_PTA_YearAgg[IncenChY_PTA_YearAgg$Base.Year != 1990,]
IncenChY_PTA_YearAgg$Base.Year[which.max(IncenChY_PTA_YearAgg$PTA_Dum_CumSum)]
max(IncenChY_PTA_YearAgg$PTA_Dum_CumSum)

# How many states in that year? 
length(unique(Incentives[Incentives$Base.Year == 1994 & Incentives$PTA_Dum_CumSum == 1,]$State))


##### Calculating for Control
IncenChy_PTA_control <- Incentives[Incentives$PTA_Dum_CumSum == 0,]
sum(IncenChy_PTA_control$PTA_Dum_CumSum == 0 & IncenChy_PTA_control$Base.Year == 2015)
length(unique(IncenChy_PTA_control[IncenChy_PTA_control$PTA_Dum_CumSum == 0 & IncenChy_PTA_control$Base.Year == 2015,]$State))

# The Year with the most policy adoptions for PTA is 1994. In this year, 45 industries in 1 state adopted PTA. Could compare against 927 across 28 states that never adopted. 

# CJTS
Incentives$CJTS_Dum_CumSum <- ave(Incentives$CJTS_Dum, Incentives$State, Incentives$Industry, FUN=cumsum)
IncenChY_CJTS <- Incentives[Incentives$CJTS_Dum_CumSum == 1,]
names(IncenChY_CJTS)
IncenChY_CJTS_YearAgg <- as.data.frame(aggregate(. ~ Base.Year, IncenChY_CJTS[c(7, 26)], sum))
IncenChY_CJTS_YearAgg <- IncenChY_CJTS_YearAgg[IncenChY_CJTS_YearAgg$Base.Year != 1990,]
IncenChY_CJTS_YearAgg$Base.Year[which.max(IncenChY_CJTS_YearAgg$CJTS_Dum_CumSum)]
max(IncenChY_CJTS_YearAgg$CJTS_Dum_CumSum)

# How many states in that year? 
length(unique(Incentives[Incentives$Base.Year == 1993 & Incentives$CJTS_Dum_CumSum == 1,]$State))

##### Calculating for Control
IncenChy_CJTS_control <- Incentives[Incentives$CJTS_Dum_CumSum == 0,]
sum(IncenChy_CJTS_control$CJTS_Dum_CumSum == 0 & IncenChy_CJTS_control$Base.Year == 2015)
length(unique(IncenChy_CJTS_control[IncenChy_CJTS_control$CJTS_Dum_CumSum == 0 & IncenChy_CJTS_control$Base.Year == 2015,]$State))

# The Year with the most policy adoptions for CJTS is 1993. In this year, 93 industries across 3 states adopted CJTS. Could compare against 599 in 27 states that never adopted. 

write.csv(Incentives, "Incentives Cumsum.csv")

## State-Year Level Analysis

# Aggregate for State and See if At Least Some Industries Have 
Incentives_StateAgg <- as.data.frame(aggregate(. ~ State + Base.Year, Incentives[c(1, 7, 17:21)], sum))


# Create Dummies to signify no incentives vs incentives (regardless of degree)
Incentives_StateAgg$JCTC_DumState <- ifelse((Incentives_StateAgg$JCTC_Dum > 0), 1, 0)
Incentives_StateAgg$ITC_DumState <- ifelse((Incentives_StateAgg$ITC_Dum > 0), 1, 0)
Incentives_StateAgg$RDC_DumState <- ifelse((Incentives_StateAgg$RDC_Dum > 0), 1, 0)
Incentives_StateAgg$PTA_DumState <- ifelse((Incentives_StateAgg$PTA_Dum > 0), 1, 0)
Incentives_StateAgg$CJTS_DumState <- ifelse((Incentives_StateAgg$CJTS_Dum > 0), 1, 0)

table(Incentives_StateAgg$JCTC_DumState)
# Have: 410
# Don't Have: 448
table(Incentives_StateAgg$ITC_DumState)
# Have: 382
# Don't Have: 476
table(Incentives_StateAgg$RDC_DumState)
# Have: 461
# Don't Have: 397
table(Incentives_StateAgg$PTA_DumState)
# Have: 399
# Don't Have: 459
table(Incentives_StateAgg$CJTS_DumState)
# Have: 610
# Don't Have: 248

write.csv(Incentives_StateAgg, "10yr Default State Collapse Panel.csv")

library(plyr)
library(tidyverse)

################# Import in State-Industry_GDP Time Series and Merge #########################
setwd("~/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis/BEA State Industry GDP Data")

# Load in all years of data and then cbind to get the time series
allfilesbea2 <- list.files(path = "./NAICS Classified", pattern="*.csv", full.names = TRUE)

datalist = list()

for (i in allfilesbea2) {
  # ... make some data
  dat <- read.csv(i, header = TRUE, na.strings = c("", "(L)"))
  datalist[[i]] <- dat # add it to your list
}

big_data2 = do.call(cbind, datalist)

# Clean up data 
big_data2 <- big_data2[big_data2$`./NAICS Classified/1997 BEA.csv.Industry` != "Addenda:",]

drop2 <- c("./NAICS Classified/2015 BEA.csv.Industry", "./NAICS Classified/2014 BEA.csv.Industry", "./NAICS Classified/2013 BEA.csv.Industry", "./NAICS Classified/2012 BEA.csv.Industry", "./NAICS Classified/2011 BEA.csv.Industry", "./NAICS Classified/2010 BEA.csv.Industry", "./NAICS Classified/2009 BEA.csv.Industry", "./NAICS Classified/2008 BEA.csv.Industry", "./NAICS Classified/2007 BEA.csv.Industry", "./NAICS Classified/2006 BEA.csv.Industry", "./NAICS Classified/2005 BEA.csv.Industry", "./NAICS Classified/2004 BEA.csv.Industry", "./NAICS Classified/2003 BEA.csv.Industry", "./NAICS Classified/2002 BEA.csv.Industry", "./NAICS Classified/2001 BEA.csv.Industry", "./NAICS Classified/2000 BEA.csv.Industry", "./NAICS Classified/1999 BEA.csv.Industry", "./NAICS Classified/1998 BEA.csv.Industry", "./NAICS Classified/2015 BEA.csv.Area", "./NAICS Classified/2014 BEA.csv.Area", "./NAICS Classified/2013 BEA.csv.Area", "./NAICS Classified/2012 BEA.csv.Area", "./NAICS Classified/2011 BEA.csv.Area", "./NAICS Classified/2010 BEA.csv.Area", "./NAICS Classified/2009 BEA.csv.Area", "./NAICS Classified/2008 BEA.csv.Area", "./NAICS Classified/2007 BEA.csv.Area", "./NAICS Classified/2006 BEA.csv.Area", "./NAICS Classified/2005 BEA.csv.Area", "./NAICS Classified/2004 BEA.csv.Area", "./NAICS Classified/2003 BEA.csv.Area", "./NAICS Classified/2002 BEA.csv.Area", "./NAICS Classified/2001 BEA.csv.Area", "./NAICS Classified/2000 BEA.csv.Area", "./NAICS Classified/1999 BEA.csv.Area", "./NAICS Classified/1998 BEA.csv.Area", "./NAICS Classified/2015 BEA.csv.Fips", "./NAICS Classified/2014 BEA.csv.Fips", "./NAICS Classified/2013 BEA.csv.Fips", "./NAICS Classified/2012 BEA.csv.Fips", "./NAICS Classified/2011 BEA.csv.Fips", "./NAICS Classified/2010 BEA.csv.Fips", "./NAICS Classified/2009 BEA.csv.Fips", "./NAICS Classified/2008 BEA.csv.Fips", "./NAICS Classified/2007 BEA.csv.Fips", "./NAICS Classified/2006 BEA.csv.Fips", "./NAICS Classified/2005 BEA.csv.Fips", "./NAICS Classified/2004 BEA.csv.Fips", "./NAICS Classified/2003 BEA.csv.Fips", "./NAICS Classified/2002 BEA.csv.Fips", "./NAICS Classified/2001 BEA.csv.Fips", "./NAICS Classified/2000 BEA.csv.Fips", "./NAICS Classified/1999 BEA.csv.Fips", "./NAICS Classified/1998 BEA.csv.Fips", "./NAICS Classified/2015 BEA.csv.IndCode", "./NAICS Classified/2014 BEA.csv.IndCode", "./NAICS Classified/2013 BEA.csv.IndCode", "./NAICS Classified/2012 BEA.csv.IndCode", "./NAICS Classified/2011 BEA.csv.IndCode", "./NAICS Classified/2010 BEA.csv.IndCode", "./NAICS Classified/2009 BEA.csv.IndCode", "./NAICS Classified/2008 BEA.csv.IndCode", "./NAICS Classified/2007 BEA.csv.IndCode", "./NAICS Classified/2006 BEA.csv.IndCode", "./NAICS Classified/2005 BEA.csv.IndCode", "./NAICS Classified/2004 BEA.csv.IndCode", "./NAICS Classified/2003 BEA.csv.IndCode", "./NAICS Classified/2002 BEA.csv.IndCode", "./NAICS Classified/2001 BEA.csv.IndCode", "./NAICS Classified/2000 BEA.csv.IndCode", "./NAICS Classified/1999 BEA.csv.IndCode", "./NAICS Classified/1998 BEA.csv.IndCode")

big_data2 <- big_data2[,!(names(big_data2) %in% drop2)]

names(big_data2) <- c("Fips", "Area", "IndCode", "Industry", "1997", "1998", "1999", "2000", "2001", "2002", "2003",  "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")

############## Merge Incentives and BLS Data ###################
##### Clean Up Industry Titles ######

Incentives2 <- Incentives

Incentives2$Industry[Incentives2$Industry == "Broadcasting and Telecommunications"] <- c("Broadcasting (except Internet) and telecommunications")

Incentives2$Industry[Incentives2$Industry == "Food, beverage, and tobacco Manufacturing
"] <- c("Food and beverage and tobacco products manufacturing")

Incentives2$Industry[Incentives2$Industry == "furniture and related product manufacturing"] <- c("Furniture and related product manufacturing")

Incentives2$Industry[Incentives2$Industry == "Information and data processing services"] <- c("Information")

Incentives2$Industry[Incentives2$Industry == "Management of companies (holding companies)"] <- c("Management of companies and enterprises")

Incentives2$Industry[Incentives2$Industry == "miscellaneous manufacturing"] <- c("Miscellaneous manufacturing")

Incentives2$Industry[Incentives2$Industry == "Motor Vehicles, bodies and trailers, and parts"] <- c("Motor vehicles, bodies and trailers, and parts manufacturing")

Incentives2$Industry[Incentives2$Industry == "Motor Vehicles, bodies and trailers, and parts"] <- c("Motor vehicles, bodies and trailers, and parts manufacturing")

Incentives2$Industry[Incentives2$Industry == "Rental and leasing services and lessors of intangible assets"] <- c("Rental and leasing services and lessors of nonfinancial intangible assets")

Incentives2$Industry[Incentives2$Industry == "Securities, commodity contracts, other financial investments, and related activities"] <- c("Securities, commodity contracts, and other financial investments and related activities")

Incentives2$Industry[Incentives2$Industry == "Textile Mills and textile product mills
"] <- c("Textile mills and textile product mills")

Incentives2$Industry[Incentives2$Industry == "warehousing and storeage"] <- c("Warehousing and storage")

Incentives2$Industry[Incentives2$Industry == "Food, beverage, and tobacco Manufacturing"] <- c("Food and beverage and tobacco products manufacturing")

Incentives2$Industry[Incentives2$Industry == "Other Services"] <- c("Other services (except government and government enterprises)")

Incentives$Industry[Incentives$Industry == "Other Transportation Equipment"] <- c("Other transportation and support activities")

Incentives2$Industry[Incentives2$Industry == "Apparel, Leather and allied product Manufacturing"] <- c("Apparel, leather, and allied product manufacturing")

Incentives2$Industry[Incentives2$Industry == "Fabricated Metal Product Manufacturing"] <- c("Fabricated metal product manufacturing")

Incentives2$Industry[Incentives2$Industry == "Food, beverage, and tobacco Manufacturing"] <- c("Food and beverage and tobacco products manufacturing")

Incentives2$Industry[Incentives2$Industry == "Paper Manufacturing"] <- c("Paper manufacturing")

Incentives2$Industry[Incentives2$Industry == "Performing arts, spectator sports, museums and entertainment"] <- c("Performing arts, spectator sports, museums, and related activities")

Incentives2$Industry[Incentives2$Industry == "Plastics and Rubber products manufacturing
"] <- c("Plastics and rubber products manufacturing")

Incentives2$Industry[Incentives2$Industry == "Primary Metal Manufacturing"] <- c("Primary metal manufacturing")

Incentives2$Industry[Incentives2$Industry == "Publishing industries (includes software)"] <- c("Publishing industries (except Internet)")

Incentives2$Industry[Incentives2$Industry == "Waste Management and remediation services"] <- c("Waste management and remediation services")

Incentives2$Industry[Incentives2$Industry == "Wood Product Manufacturing"] <- c("Wood product manufacturing")

Incentives2$Industry[Incentives2$Industry == "Retail Trade"] <- c("Retail trade")

# Convert BEA/BLS dataset from wide to long 
BEABLS_long <- reshape(big_data2, varying = list(names(big_data2)[5:23]), idvar = 'ID',
                 v.names = 'GDP', direction = 'long', timevar = "Year", times =1997:2015)

# Merge on State-Industry-Year
# Incentives Vars: State-Industry-Base.Year
# Big_data2 (BEA/BLS) Vars: Area- Industry - Year
# Change var names to match
names(BEABLS_long)[2] <- "State"
names(Incentives2)[7] <- "Year"
# Merge Abbreviations in to BEABLS_long in order to match up states
library(haven)
StateAbbrevs <- read_dta("state_name_abbreviation.dta")
names(StateAbbrevs)[1] <- "State"
BEABLS_longst <- left_join(BEABLS_long, StateAbbrevs, by = "State")
names(BEABLS_longst)[2] <- "StateFull"
names(BEABLS_longst)[8] <- "State"
# Before Merge counts (will obv lose some)
# Incentives: 38610 , BEA/BLS: 37800
library(dplyr)

# Eliminate leading spaces stemming from hierarchies in BEABLS Data 
BEABLS_longst$Industry <- trimws(BEABLS_longst$Industry, which = "both")

# Export and Check Text of Industry Names
unique(BEABLS_longst$Industry)
unique(Incentives2$Industry)

# Get just the annual state GDPs for later merge
BEABLS_longststates <- BEABLS_longst[BEABLS_longst$Industry == "All industry total",]

# Merge Full 

FullIncGDP_9015 <- left_join(Incentives2, BEABLS_longst, by = c("State", "Industry", "Year"))

FullIncGDP_9715 <- FullIncGDP_9015[FullIncGDP_9015$Year > 1996,]

# Eliminate industries without matching GDP Data 

FullIncGDP_9715 <- FullIncGDP_9715[!is.na(FullIncGDP_9715$GDP),]

write.csv(FullIncGDP_9715, "Full Merged Dataset NAICS 102518.csv")


  ########################## DiD Checks - New ################
  # Load in data
FullIncGSP_9715 <- read.csv("Full Merged Dataset NAICS 102518.csv", stringsAsFactors = FALSE)
  # JCTC 
  IncenChY_JCTC <- FullIncGDP_9715[FullIncGDP_9715$JCTC_Dum_CumSum == 1,]
  names(IncenChY_JCTC)
  IncenChY_JCTC_YearAgg <- as.data.frame(aggregate(. ~ Year, IncenChY_JCTC[c(7, 22)], sum))
  IncenChY_JCTC_YearAgg$Year[which.max(IncenChY_JCTC_YearAgg$JCTC_Dum_CumSum)]
  JCTC1 <- 2004
  JCTC2 <- max(IncenChY_JCTC_YearAgg$JCTC_Dum_CumSum[IncenChY_JCTC_YearAgg$Year == 2004])
  
  # How many states in that year? 
  JCTC3 <- length(unique(FullIncGDP_9715[FullIncGDP_9715$Year == 2004 & FullIncGDP_9715$JCTC_Dum_CumSum == 1,]$State))
  
  ##### Calculating for Control
  IncenChy_JCTC_control <- FullIncGDP_9715[FullIncGDP_9715$JCTC_Dum_CumSum == 0,]
  JCTC4 <- sum(IncenChy_JCTC_control$JCTC_Dum_CumSum == 0 & IncenChy_JCTC_control$Year == 2015)
  JCTC5 <- length(unique(IncenChy_JCTC_control[IncenChy_JCTC_control$JCTC_Dum_CumSum == 0 & IncenChy_JCTC_control$Year == 2015,]$State))
  # If we look at just post 1997 when the data merges, 2004 appears to bt the best year. In this year, 62  industries across states adopted JCTC. Would be able to potentially compare against 637 in 30 states that never adopt.
  
  Incentive <- "JCTC"
  DiD_Year <- JCTC1
  Treatment_Industries <- JCTC2
  Treatment_States <- JCTC3
  Control_Industries <- JCTC4
  Control_States <- JCTC5
  
  DiDEstimators1 <- data.frame(Incentive, DiD_Year, Treatment_Industries, Treatment_States, Control_Industries, Control_States)
  
  ###### ITC
  IncenChY_ITC <- FullIncGDP_9715[FullIncGDP_9715$ITC_Dum_CumSum == 1,]
  names(IncenChY_ITC)
  IncenChY_ITC_YearAgg <- as.data.frame(aggregate(. ~ Year, IncenChY_ITC[c(7, 23)], sum))
  IncenChY_ITC_YearAgg <- IncenChY_ITC_YearAgg[IncenChY_ITC_YearAgg$Year > 1996,]
  ITC1 <- IncenChY_ITC_YearAgg$Year[which.max(IncenChY_ITC_YearAgg$ITC_Dum_CumSum)]
  ITC2 <- max(IncenChY_ITC_YearAgg$ITC_Dum_CumSum)
  
  # How many states in that year? 
  ITC3 <- length(unique(FullIncGDP_9715[FullIncGDP_9715$Year == 1998 & FullIncGDP_9715$ITC_Dum_CumSum == 1,]$State))
  
  ##### Calculating for Control
  IncenChy_ITC_control <- FullIncGDP_9715[FullIncGDP_9715$ITC_Dum_CumSum == 0,]
  ITC4 <- sum(IncenChy_ITC_control$ITC_Dum_CumSum == 0 & IncenChy_ITC_control$Year == 2015)
  ITC5 <- length(unique(IncenChy_ITC_control[IncenChy_ITC_control$ITC_Dum_CumSum == 0 & IncenChy_ITC_control$Year == 2015,]$State))
  
  # The Year with the most policy adoptions for ITC is 1995. In this year, 100 industries across 4 states adopted ITC. Could compare against 865 in 31 states that never adopt. 
  
  Incentive <- "ITC"
  DiD_Year <- ITC1
  Treatment_Industries <- ITC2
  Treatment_States <- ITC3
  Control_Industries <- ITC4
  Control_States <- ITC5
  
  DiDEstimators2 <- data.frame(Incentive, DiD_Year, Treatment_Industries, Treatment_States, Control_Industries, Control_States)
  
  
  ##### RDC
  IncenChY_RDC <- FullIncGDP_9715[FullIncGDP_9715$RDC_Dum_CumSum == 1,]
  names(IncenChY_RDC)
  IncenChY_RDC_YearAgg <- as.data.frame(aggregate(. ~ Year, IncenChY_RDC[c(7, 24)], sum))
  IncenChY_RDC_YearAgg <- IncenChY_RDC_YearAgg[IncenChY_RDC_YearAgg$Year > 1996,]
  RDC1 <- IncenChY_RDC_YearAgg$Year[which.max(IncenChY_RDC_YearAgg$RDC_Dum_CumSum)]
  RDC2 <- max(IncenChY_RDC_YearAgg$RDC_Dum_CumSum)
  
  # How many states in that year? 
  RDC3 <- length(unique(Incentives[FullIncGDP_9715$Year == 2000 & FullIncGDP_9715$RDC_Dum_CumSum == 1,]$State))
  
  ##### Calculating for Control
  IncenChy_RDC_control <- FullIncGDP_9715[FullIncGDP_9715$RDC_Dum_CumSum == 0,]
  RDC4 <- sum(IncenChy_RDC_control$RDC_Dum_CumSum == 0 & IncenChy_RDC_control$Year == 2015)
  RDC5 <- length(unique(IncenChy_RDC_control[IncenChy_RDC_control$RDC_Dum_CumSum == 0 & IncenChy_RDC_control$Year == 2015,]$State))
  
  # The Year with the most policy adoptions for RDC is 2000. In this year, 134 industries across 4 states adopted RDC. Could compare against 460 across 25 states that never adopt. 
  
  Incentive <- "RDC"
  DiD_Year <- RDC1
  Treatment_Industries <- RDC2
  Treatment_States <- RDC3
  Control_Industries <- RDC4
  Control_States <- RDC5
  
  DiDEstimators3 <- data.frame(Incentive, DiD_Year, Treatment_Industries, Treatment_States, Control_Industries, Control_States)
  
  ##### PTA
  IncenChY_PTA <- FullIncGDP_9715[FullIncGDP_9715$PTA_Dum_CumSum == 1,]
  names(IncenChY_PTA)
  IncenChY_PTA_YearAgg <- as.data.frame(aggregate(. ~ Year, IncenChY_PTA[c(7, 25)], sum))
  IncenChY_PTA_YearAgg <- IncenChY_PTA_YearAgg[IncenChY_PTA_YearAgg$Year > 1996,]
  IncenChY_PTA_YearAgg$Year[which.max(IncenChY_PTA_YearAgg$PTA_Dum_CumSum)]
  PTA2 <- max(IncenChY_PTA_YearAgg$PTA_Dum_CumSum[IncenChY_PTA_YearAgg$Year == 2000])
  PTA1 <- "2000"
  
  # How many states in that year? 
  PTA3 <- length(unique(FullIncGDP_9715[FullIncGDP_9715$Year == 2000 & FullIncGDP_9715$PTA_Dum_CumSum == 1,]$State))
  
  ##### Calculating for Control
  IncenChy_PTA_control <- FullIncGDP_9715[FullIncGDP_9715$PTA_Dum_CumSum == 0,]
  PTA4 <- sum(IncenChy_PTA_control$PTA_Dum_CumSum == 0 & IncenChy_PTA_control$Year == 2015)
  PTA5 <- length(unique(IncenChy_PTA_control[IncenChy_PTA_control$PTA_Dum_CumSum == 0 & IncenChy_PTA_control$Year == 2015,]$State))
  
  # The Year with the most policy adoptions for PTA is 1994. In this year, 45 industries in 1 state adopted PTA. Could compare against 927 across 28 states that never adopted. 
  
  
  Incentive <- "PTA"
  DiD_Year <- PTA1
  Treatment_Industries <- PTA2
  Treatment_States <- PTA3
  Control_Industries <- PTA4
  Control_States <- PTA5
  
  DiDEstimators4 <- data.frame(Incentive, DiD_Year, Treatment_Industries, Treatment_States, Control_Industries, Control_States)
  
  
  # CJTS
  IncenChY_CJTS <- FullIncGDP_9715[FullIncGDP_9715$CJTS_Dum_CumSum == 1,]
  names(IncenChY_CJTS)
  IncenChY_CJTS_YearAgg <- as.data.frame(aggregate(. ~ Year, IncenChY_CJTS[c(7, 26)], sum))
  IncenChY_CJTS_YearAgg <- IncenChY_CJTS_YearAgg[IncenChY_CJTS_YearAgg$Year > 1996,]
  CJTS1 <- IncenChY_CJTS_YearAgg$Year[which.max(IncenChY_CJTS_YearAgg$CJTS_Dum_CumSum)]
  CJTS2 <- max(IncenChY_CJTS_YearAgg$CJTS_Dum_CumSum)
  
  # How many states in that year? 
  CJTS3 <- length(unique(FullIncGDP_9715[FullIncGDP_9715$Year == 2001 & FullIncGDP_9715$CJTS_Dum_CumSum == 1,]$State))
  
  ##### Calculating for Control
  IncenChy_CJTS_control <- FullIncGDP_9715[FullIncGDP_9715$CJTS_Dum_CumSum == 0,]
  CJTS4 <- sum(IncenChy_CJTS_control$CJTS_Dum_CumSum == 0 & IncenChy_CJTS_control$Year == 2015)
  CJTS5 <- length(unique(IncenChy_CJTS_control[IncenChy_CJTS_control$CJTS_Dum_CumSum == 0 & IncenChy_CJTS_control$Year == 2015,]$State))
  
  # The Year with the most policy adoptions for CJTS is 1993. In this year, 93 industries across 3 states adopted CJTS. Could compare against 599 in 27 states that never adopted. 
  
  Incentive <- "CJTS"
  DiD_Year <- CJTS1
  Treatment_Industries <- CJTS2
  Treatment_States <- CJTS3
  Control_Industries <- CJTS4
  Control_States <- CJTS5
  
  DiDEstimators5 <- data.frame(Incentive, DiD_Year, Treatment_Industries, Treatment_States, Control_Industries, Control_States)
  
  DiDEstimators <- rbind(DiDEstimators1, DiDEstimators2, DiDEstimators3, DiDEstimators4, DiDEstimators5)
  
write.table(DiDEstimators, file = "DiDEstimators.txt", sep = ",", quote = FALSE, row.names = F)

################### Visualizing best year for Difference-in-difference cut off in the data
FullIncGDP_9715$StateInd <- paste(FullIncGDP_9715$State, FullIncGDP_9715$Industry, sep = " ")

# JCTC
IncentivesJCTC10 <- FullIncGDP_9715 %>%
  group_by(Year, JCTC_Dum_CumSum) %>%
  summarize(cumsumcount = n())
IncentivesJCTC10 <- IncentivesJCTC10[IncentivesJCTC10$JCTC_Dum_CumSum == 0 | IncentivesJCTC10$JCTC_Dum_CumSum==1,]
IncentivesJCTC10$JCTC_Dum_CumSum[IncentivesJCTC10$JCTC_Dum_CumSum == 0] <- c("No Adoption")
IncentivesJCTC10$JCTC_Dum_CumSum[IncentivesJCTC10$JCTC_Dum_CumSum == 1] <- c("Adopted (First time)")

ggplot(IncentivesJCTC10, aes(x=factor(Year), y= cumsumcount, fill=factor(JCTC_Dum_CumSum))) + geom_bar(stat="identity", position = "dodge") + annotate("text", x= 8, y= 660, label = "2004", color = 'red') + scale_fill_brewer(palette = "Dark2")+ labs(y = "Number of State-Industries", x = "Year", title= "Figure 1: Vizualization of Potential DiD Year Cutoff for Job Creation Tax Credit", caption = "Source: Bartik 2017", fill = "Adoption") + geom_text(aes(label=cumsumcount), position=position_dodge(width=0.9), vjust=-0.25) + annotate("rect", xmin=7.45, xmax=8.67, ymin=0 , ymax=680, alpha=0.2, color="blue", fill="blue")

# What about introduction of other incentives after 2004? 
FullIncGDP_9715_JCTCSub <- FullIncGDP_9715[FullIncGDP_9715$Year == 2004 & FullIncGDP_9715$JCTC_Dum_CumSum == 1,]
FullIncGDP_9715JCTCTest <- subset(FullIncGDP_9715, StateInd %in% FullIncGDP_9715_JCTCSub$StateInd)
FullIncGDP_9715JCTCTest <- FullIncGDP_9715JCTCTest[FullIncGDP_9715JCTCTest$Year >= 2003,]

table(FullIncGDP_9715JCTCTest$Year, FullIncGDP_9715JCTCTest$ITC_Dum_CumSum)
# For ITC, 38 were 0 the whole time and other 16 were introduced before 2004
table(FullIncGDP_9715JCTCTest$Year, FullIncGDP_9715JCTCTest$RDC_Dum_CumSum)
# For RDC, all were receiving RDC by 2004, but by 2008, it looks like 27 lost their RDC. In 2014, that same 27 got the RDC back. 
table(FullIncGDP_9715JCTCTest$Year, FullIncGDP_9715JCTCTest$PTA_Dum_CumSum)
# For PTA, all were recieving by 2004
table(FullIncGDP_9715JCTCTest$Year, FullIncGDP_9715JCTCTest$CJTS_Dum_CumSum)
# For CJTS, all were receiving by 2004.


# ITC
IncentivesITC10 <- FullIncGDP_9715 %>%
  group_by(Year, ITC_Dum_CumSum) %>%
  summarize(cumsumcount = n())
IncentivesITC10 <- IncentivesITC10[IncentivesITC10$ITC_Dum_CumSum == 0 | IncentivesITC10$ITC_Dum_CumSum==1,]
IncentivesITC10$ITC_Dum_CumSum[IncentivesITC10$ITC_Dum_CumSum == 0] <- c("No Adoption")
IncentivesITC10$ITC_Dum_CumSum[IncentivesITC10$ITC_Dum_CumSum == 1] <- c("Adopted (First time)")

ggplot(IncentivesITC10, aes(x=factor(Year), y= cumsumcount, fill=factor(ITC_Dum_CumSum))) + geom_bar(stat="identity", position = "dodge") + annotate("text", x= 2, y= 780, label = "1998", color = 'red') + scale_fill_brewer(palette = "Dark2")+ labs(y = "Number of State-Industries", x = "Year", title= "Figure 2: Vizualization of Potential DiD Year Cutoff for Investment Tax Credit", caption = "Source: Bartik 2017", fill = "Adoption") + geom_text(aes(label=cumsumcount), position=position_dodge(width=0.9), vjust=-0.25) + annotate("rect", xmin=1.45, xmax=2.67, ymin=0 , ymax=800, alpha=0.2, color="blue", fill="blue")

# What about introduction of other incentives after 1998? 
FullIncGDP_9715_ITCSub <- FullIncGDP_9715[FullIncGDP_9715$Year == 1998 & FullIncGDP_9715$ITC_Dum_CumSum == 1,]
FullIncGDP_9715ITCTest <- subset(FullIncGDP_9715, StateInd %in% FullIncGDP_9715_ITCSub$StateInd)
FullIncGDP_9715ITCTest <- FullIncGDP_9715ITCTest[FullIncGDP_9715ITCTest$Year >= 1997,]

table(FullIncGDP_9715ITCTest$Year, FullIncGDP_9715ITCTest$JCTC_Dum_CumSum)
# For JCTC, 27 had before 1998. 27 adopted in 2006. Only 7 never adopted. 
table(FullIncGDP_9715ITCTest$Year, FullIncGDP_9715ITCTest$RDC_Dum_CumSum)
# For RDC, 34 had received before 1998. The other 27 all adopted in 2012. 
table(FullIncGDP_9715ITCTest$Year, FullIncGDP_9715ITCTest$PTA_Dum_CumSum)
# For PTA, 16 had adopted before 1998. 27 adopted in 2011. 18 never adopted. 
table(FullIncGDP_9715ITCTest$Year, FullIncGDP_9715ITCTest$CJTS_Dum_CumSum)
# For CJTS, 27 had adopted before 1998. This remained unchanged with 34 never adopting. 


# RDC
IncentivesRDC10 <- FullIncGDP_9715 %>%
  group_by(Year, RDC_Dum_CumSum) %>%
  summarize(cumsumcount = n())
IncentivesRDC10 <- IncentivesRDC10[IncentivesRDC10$RDC_Dum_CumSum == 0 | IncentivesRDC10$RDC_Dum_CumSum==1,]
IncentivesRDC10$RDC_Dum_CumSum[IncentivesRDC10$RDC_Dum_CumSum == 0] <- c("No Adoption")
IncentivesRDC10$RDC_Dum_CumSum[IncentivesRDC10$RDC_Dum_CumSum == 1] <- c("Adopted (First time)")

ggplot(IncentivesRDC10, aes(x=factor(Year), y= cumsumcount, fill=factor(RDC_Dum_CumSum))) + geom_bar(stat="identity", position = "dodge") + annotate("text", x= 4, y= 620, label = "2000", color = 'red') + scale_fill_brewer(palette = "Dark2")+ labs(y = "Number of State-Industries", x = "Year", title= "Figure 3: Vizualization of Potential DiD Year Cutoff for Research & Development Credit", caption = "Source: Bartik 2017", fill = "Adoption") + geom_text(aes(label=cumsumcount), position=position_dodge(width=0.9), vjust=-0.25) + annotate("rect", xmin=3.50, xmax=4.50, ymin=0 , ymax=640, alpha=0.2, color="blue", fill="blue")

# What about introduction of other incentives after 2000? 
FullIncGDP_9715_RDCSub <- FullIncGDP_9715[FullIncGDP_9715$Year == 2000 & FullIncGDP_9715$RDC_Dum_CumSum == 1,]
FullIncGDP_9715RDCTest <- subset(FullIncGDP_9715, StateInd %in% FullIncGDP_9715_RDCSub$StateInd)
FullIncGDP_9715RDCTest <- FullIncGDP_9715RDCTest[FullIncGDP_9715RDCTest$Year >= 1999,]

table(FullIncGDP_9715RDCTest$Year, FullIncGDP_9715RDCTest$JCTC_Dum_CumSum)
# For JCTC, 28 had before 2000. 54 adopted in 2004. 20 remaining that never adopted.  
table(FullIncGDP_9715RDCTest$Year, FullIncGDP_9715RDCTest$ITC_Dum_CumSum)
# For ITC, only 1 had adopted before 2000. 16 adopted in 2001. 
table(FullIncGDP_9715RDCTest$Year, FullIncGDP_9715RDCTest$PTA_Dum_CumSum)
# For PTA, 61 had adopted before 2000. The remaining never adopted. 
table(FullIncGDP_9715RDCTest$Year, FullIncGDP_9715RDCTest$CJTS_Dum_CumSum)
# For CJTS, 55 had adopted before 2000. The remaining never adopted.

# PTA
IncentivesPTA10 <- FullIncGDP_9715 %>%
  group_by(Year, PTA_Dum_CumSum) %>%
  summarize(cumsumcount = n())
IncentivesPTA10 <- IncentivesPTA10[IncentivesPTA10$PTA_Dum_CumSum == 0 | IncentivesPTA10$PTA_Dum_CumSum==1,]
IncentivesPTA10$PTA_Dum_CumSum[IncentivesPTA10$PTA_Dum_CumSum == 0] <- c("No Adoption")
IncentivesPTA10$PTA_Dum_CumSum[IncentivesPTA10$PTA_Dum_CumSum == 1] <- c("Adopted (First time)")

ggplot(IncentivesPTA10, aes(x=factor(Year), y= cumsumcount, fill=factor(PTA_Dum_CumSum))) + geom_bar(stat="identity", position = "dodge") + annotate("text", x= 4, y= 770, label = "2000", color = 'red') + scale_fill_brewer(palette = "Dark2") + labs(y = "Number of State-Industries", x = "Year", title= "Figure 4: Vizualization of Potential DiD Year Cutoff for Property Tax Abatement", caption = "Source: Bartik 2017", fill = "Adoption") + geom_text(aes(label=cumsumcount), position=position_dodge(width=0.9), vjust=-0.25) + annotate("rect", xmin=3.50, xmax=4.67, ymin=0 , ymax=790, alpha=0.2, color="blue", fill="blue")

# What about introduction of other incentives after 2000? 
FullIncGDP_9715_PTASub <- FullIncGDP_9715[FullIncGDP_9715$Year == 2000 & FullIncGDP_9715$PTA_Dum_CumSum == 1,]
FullIncGDP_9715PTATest <- subset(FullIncGDP_9715, StateInd %in% FullIncGDP_9715_PTASub$StateInd)
FullIncGDP_9715PTATest <- FullIncGDP_9715PTATest[FullIncGDP_9715PTATest$Year >= 1999,]

table(FullIncGDP_9715PTATest$Year, FullIncGDP_9715PTATest$JCTC_Dum_CumSum)
# For JCTC, all had adopted by 2000 
table(FullIncGDP_9715PTATest$Year, FullIncGDP_9715PTATest$ITC_Dum_CumSum)
# For ITC, all never adopted. 
table(FullIncGDP_9715PTATest$Year, FullIncGDP_9715PTATest$RDC_Dum_CumSum)
# For RDC, 24 had adopted before 2000. The remaining never adopted. 
table(FullIncGDP_9715PTATest$Year, FullIncGDP_9715PTATest$CJTS_Dum_CumSum)
# For CJTS, 27 had adopted before 2000. The remaining never adopted.


# CJTS
IncentivesCJTS10 <- FullIncGDP_9715 %>%
  group_by(Year, CJTS_Dum_CumSum) %>%
  summarize(cumsumcount = n())
IncentivesCJTS10 <- IncentivesCJTS10[IncentivesCJTS10$CJTS_Dum_CumSum == 0 | IncentivesCJTS10$CJTS_Dum_CumSum==1,]
IncentivesCJTS10$CJTS_Dum_CumSum[IncentivesCJTS10$CJTS_Dum_CumSum == 0] <- c("No Adoption")
IncentivesCJTS10$CJTS_Dum_CumSum[IncentivesCJTS10$CJTS_Dum_CumSum == 1] <- c("Adopted (First time)")

ggplot(IncentivesCJTS10, aes(x=factor(Year), y= cumsumcount, fill=factor(CJTS_Dum_CumSum))) + geom_bar(stat="identity", position = "dodge") + annotate("text", x= 5, y= 480, label = "2001", color = 'red') + scale_fill_brewer(palette = "Dark2")+ labs(y = "Number of State-Industries", x = "Year", title= "Figure 5: Vizualization of Potential DiD Year Cutoff for Custom Job Training Subsidy", caption = "Source: Bartik 2017", fill = "Adoption") + geom_text(aes(label=cumsumcount), position=position_dodge(width=0.9), vjust=-0.25) + annotate("rect", xmin=4.50, xmax=5.50, ymin=0 , ymax=500, alpha=0.2, color="blue", fill="blue")

# What about introduction of other incentives after 2000? 
FullIncGDP_9715_CJTSSub <- FullIncGDP_9715[FullIncGDP_9715$Year == 2001 & FullIncGDP_9715$CJTS_Dum_CumSum == 1,]
FullIncGDP_9715CJTSTest <- subset(FullIncGDP_9715, StateInd %in% FullIncGDP_9715_CJTSSub$StateInd)
FullIncGDP_9715CJTSTest <- FullIncGDP_9715CJTSTest[FullIncGDP_9715CJTSTest$Year >= 2000,]

table(FullIncGDP_9715CJTSTest$Year, FullIncGDP_9715CJTSTest$JCTC_Dum_CumSum)
# For JCTC, 27 had adopted before 2001. 27 adopted in 2011. 10 later took away in 2013. 
table(FullIncGDP_9715CJTSTest$Year, FullIncGDP_9715CJTSTest$ITC_Dum_CumSum)
# For ITC, none had adopted before 2001. 27 adopted in 2003. 10 adopted in 2013.  
table(FullIncGDP_9715CJTSTest$Year, FullIncGDP_9715CJTSTest$RDC_Dum_CumSum)
# For RDC, 60 had adopted before 2001. Just 1 never adopted. 
table(FullIncGDP_9715CJTSTest$Year, FullIncGDP_9715CJTSTest$PTA_Dum_CumSum)
# For PTA,  16 adopted before 2001. All others never adopted. 

################### Descriptive Statistics #######################
# Frequency Counts 
# Create Unique Identified for State-Industry
FullIncGDP_9715$stateindID <- as.numeric(factor(paste0(FullIncGDP_9715$State, FullIncGDP_9715$Industry)))

# Frequency Distributions 

# Per year state-industries with each incentive in place (dodge with the five)
  # Are there period of time that saw jumps in adoption?

# Prep the data - subset to cumsums and reshape from wide to long
AdoptIncenticesTime <- FullIncGDP_9715[,c(1,3,7,22:26)]
AdoptIncenticesTime_long <- reshape(AdoptIncenticesTime, varying = list(names(AdoptIncenticesTime)[4:8]), idvar = 'ID', v.names = "cumsum", direction = 'long', timevar = "Incentive", times = c("JCTC_Dum_CumSum", "ITC_Dum_CumSum", "RDC_Dum_CumSum", "PTA_Dum_CumSum", "CJTS_Dum_CumSum"))
AdoptIncentivesTime_long_sub <- AdoptIncenticesTime_long[AdoptIncenticesTime_long$cumsum != 0,]

AdoptIncentivesTime_long_sub$Incentive[AdoptIncentivesTime_long_sub$Incentive == "CJTS_Dum_CumSum"] <- c("CJTS")
AdoptIncentivesTime_long_sub$Incentive[AdoptIncentivesTime_long_sub$Incentive == "JCTC_Dum_CumSum"] <- c("JCTC")
AdoptIncentivesTime_long_sub$Incentive[AdoptIncentivesTime_long_sub$Incentive == "ITC_Dum_CumSum"] <- c("ITC")
AdoptIncentivesTime_long_sub$Incentive[AdoptIncentivesTime_long_sub$Incentive == "RDC_Dum_CumSum"] <- c("RDC")
AdoptIncentivesTime_long_sub$Incentive[AdoptIncentivesTime_long_sub$Incentive == "PTA_Dum_CumSum"] <- c("PTA")


AdoptIncentivesTime_long_sub <- AdoptIncentivesTime_long_sub %>%
  group_by(Year, Incentive) %>%
  dplyr::summarize(NumberHave = n())

# Bar
ggplot(AdoptIncentivesTime_long_sub, aes(x=factor(Year), y= NumberHave, fill=factor(Incentive))) + geom_bar(stat="identity", position = position_dodge(width = 0.8), width = 1) + scale_fill_brewer(palette = "Dark2") + labs(y = "Number of State-Industries Receiving Incentive", x = "Year", title= "Figure 1: Number of State-Industries Receiving Each Incentive by Year", caption = "Source: Bartik 2017", fill = "Incentive Type") + geom_text(aes(label=NumberHave), position=position_dodge(width=0.9), size = 3, angle = 90)

# Line
ggplot(data = AdoptIncentivesTime_long_sub, aes(x = factor(Year), y = NumberHave, group = Incentive)) +geom_line(aes(colour=Incentive)) + labs(y = "Number of State-Industries Receiving Incentive", x = "Year", title= "Figure 1: Number of State-Industries Receiving Each Incentive by Year") #+ geom_text(aes(label=NumberHave), position=position_dodge(width=0.9), size = 3)

# Average abatement over time by incentive offered (multiple on a line graph)
  # Are they rising at the same rate?
  # Does the rise in rate correspond to the rise in adoption or no? 

AdoptIncenticesTime <- FullIncGDP_9715[,c(1,3,7,12:16)]
AdoptIncenticesTime_long <- reshape(AdoptIncenticesTime, varying = list(names(AdoptIncenticesTime)[4:8]), idvar = 'ID', v.names = "Abatement", direction = 'long', timevar = "Incentive", times = c("Job.Creation.Tax.Credit", "Investment.Tax.Credit", "Research.and.Development.Credit", "Property.Tax.Abatement", "Customized.Job.Training.Subsidy"))

AdoptIncenticesTime_long$Incentive[AdoptIncenticesTime_long$Incentive == "Job.Creation.Tax.Credit"] <- c("JCTC")
AdoptIncenticesTime_long$Incentive[AdoptIncenticesTime_long$Incentive == "Investment.Tax.Credit"] <- c("ITC")
AdoptIncenticesTime_long$Incentive[AdoptIncenticesTime_long$Incentive == "Property.Tax.Abatement"] <- c("PTA")
AdoptIncenticesTime_long$Incentive[AdoptIncenticesTime_long$Incentive == "Research.and.Development.Credit"] <- c("RDC")
AdoptIncenticesTime_long$Incentive[AdoptIncenticesTime_long$Incentive == "Customized.Job.Training.Subsidy"] <- c("CJTS")

AdoptIncentivesTime_long_sub <- AdoptIncenticesTime_long %>%
  group_by(Year, Incentive) %>%
  summarize(NumberHave = n(),
            Avgabatement = mean(Abatement))

ggplot(data = AdoptIncentivesTime_long_sub, aes(x = as.numeric(Year), y = Avgabatement)) +
  geom_line(aes(colour=Incentive)) + labs(y = "Average Percent in Tax Abatement", x = "Year", title= "Figure: 7 Average Percent in Tax Abatement per Incentive Type Across All State-Industries")

# Average level of abatement and Industry GDP growth by state over time
  # Which industries seem to receive bigger incentives? Do those industries have higher growth rates or no? 
  # Which incentives are driving the top industry abatements (can do separate call out)

GDPandIncentivesTime <- FullIncGDP_9715[,c(1,3,7,12:16, 30)]
GDPandIncentivesTime$TotalAbatement <- eval(GDPandIncentivesTime$Job.Creation.Tax.Credit + GDPandIncentivesTime$Investment.Tax.Credit + GDPandIncentivesTime$Research.and.Development.Credit + GDPandIncentivesTime$Property.Tax.Abatement + GDPandIncentivesTime$Customized.Job.Training.Subsidy)

GDPandIncentivesTime_chart <- GDPandIncentivesTime %>%
  group_by(Year, State) %>%
  summarize(NumberHave = n(),
            Totalabatement = sum(TotalAbatement),
            TotalGDP = sum(GDP))

GDPandIncentivesTime_chart_wide1 <- spread(GDPandIncentivesTime_chart[,c(1:2,4)], key = Year, value = Totalabatement)
GDPandIncentivesTime_chart_wide1$Measure <- "Total Abatment"

GDPandIncentivesTime_chart_wide2 <- spread(GDPandIncentivesTime_chart[,c(1:2,5)], key = Year, value = TotalGDP)
GDPandIncentivesTime_chart_wide2$Measure <- "Total GDP"

Final_GDPandIncentivesTime <- rbind(GDPandIncentivesTime_chart_wide1, GDPandIncentivesTime_chart_wide2)

Final_GDPandIncentivesTime$Fiveyearch <- round(eval((Final_GDPandIncentivesTime$`2002`- Final_GDPandIncentivesTime$`1997`)/Final_GDPandIncentivesTime$`1997`), 2) 
Final_GDPandIncentivesTime$Tenyearch <- round(eval((Final_GDPandIncentivesTime$`2007`- Final_GDPandIncentivesTime$`1997`)/Final_GDPandIncentivesTime$`1997`), 2)  
Final_GDPandIncentivesTime$Totalch <- round(eval((Final_GDPandIncentivesTime$`2015`- Final_GDPandIncentivesTime$`1997`)/Final_GDPandIncentivesTime$`1997`), 2) 

Final_GDPandIncentivesTime <- Final_GDPandIncentivesTime[order(Final_GDPandIncentivesTime$State),]

Final_GDPandIncentivesTime <- Final_GDPandIncentivesTime[,c(1, 21, 2,3,4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 23, 24)]

write.table(Final_GDPandIncentivesTime, file = "StateAbtandGrow.txt", sep = ",", quote = FALSE, row.names = F)

# Average level of abatement and Industry GDP growth by industry over time
GDPandIncentivesTime_chart <- GDPandIncentivesTime %>%
  group_by(Year, Industry) %>%
  summarize(NumberHave = n(),
            Totalabatement = sum(TotalAbatement),
            TotalGDP = sum(GDP))

names(GDPandIncentivesTime_chart)

GDPandIncentivesTime_chart_wide1 <- spread(GDPandIncentivesTime_chart[,c(1:2,4)], key = Year, value = Totalabatement)
GDPandIncentivesTime_chart_wide1$Measure <- "Total Abatment"

GDPandIncentivesTime_chart_wide2 <- spread(GDPandIncentivesTime_chart[,c(1:2,5)], key = Year, value = TotalGDP)
GDPandIncentivesTime_chart_wide2$Measure <- "Total GDP"

Final_GDPandIncentivesTime <- rbind(GDPandIncentivesTime_chart_wide1, GDPandIncentivesTime_chart_wide2)

Final_GDPandIncentivesTime$Fiveyearch <- round(eval((Final_GDPandIncentivesTime$`2002`- Final_GDPandIncentivesTime$`1997`)/Final_GDPandIncentivesTime$`1997`), 2) 
Final_GDPandIncentivesTime$Tenyearch <- round(eval((Final_GDPandIncentivesTime$`2007`- Final_GDPandIncentivesTime$`1997`)/Final_GDPandIncentivesTime$`1997`), 2)  
Final_GDPandIncentivesTime$Totalch <- round(eval((Final_GDPandIncentivesTime$`2015`- Final_GDPandIncentivesTime$`1997`)/Final_GDPandIncentivesTime$`1997`), 2) 

Final_GDPandIncentivesTime <- Final_GDPandIncentivesTime[order(Final_GDPandIncentivesTime$Industry),]

names(Final_GDPandIncentivesTime)

Final_GDPandIncentivesTime <- Final_GDPandIncentivesTime[,c(1, 21, 2,3,4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 23, 24)]

Final_GDPandIncentivesTimeShort <- Final_GDPandIncentivesTime[,c(1:13, 23)] 
names(Final_GDPandIncentivesTimeShort)
write.table(Final_GDPandIncentivesTimeShort, file = "StateAbtandGrow.txt", sep = ",", quote = FALSE, row.names = F)

################## Analysis Attempt 2 -  #######################
setwd("~/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis")
IncentivesData <- read.csv("Full Merged Dataset NAICS 102518.csv", stringsAsFactors = FALSE)

# Load in Analysis Packages 
require(plm)
# install.packages('lfe')
require(lfe)
require(broom)

# Does this work in the same way that including them separately would have? 
IncentivesData$StateInd <- paste(IncentivesData$State, IncentivesData$Industry, sep = " ")

###### Method 1: Panel Data Fixed Effects - Industry GDP
# JCTC

#### Model with all three separate
#'felm' is used to fit linear models with multiple group fixed effects, similarly to lm. It uses the Method of Alternating projections to sweep out multiple group effects from the normal equations before estimating the remaining coefficients with OLS.

# Two-way fixed effects with no controls for characteristics that differ across state and time or across industry and time
JCTCmod3 <- felm(GDP ~ Job.Creation.Tax.Credit + Investment.Tax.Credit + Research.and.Development.Credit + Property.Tax.Abatement + Customized.Job.Training.Subsidy  | State + Industry + Year, data = IncentivesData)
summary(JCTCmod3)
# Try with weighting 


# Standard errors are really high here already before I add in the interacted statetime and industrytime fixed effects

# Tidy is used here to turn the regression output into a tibble that can then be better formatted into a clean table 
tidy_JCTCmod3 <- tidy(JCTCmod3)
write.csv(tidy_JCTCmod3, "tidy_JCTCmod3.csv")

# Two-way fixed effects with no controls for characteristics that differ across state and time or across industry and time

# Create unique identifiers that serve as the respective "interactions" between state and time and industry and time
IncentivesData$statetimeID <- as.numeric(factor(paste0(IncentivesData$State, IncentivesData$Year)))
IncentivesData$industrytimeID <- as.numeric(factor(paste0(IncentivesData$Industry, IncentivesData$Year)))

# I've included five fixed effects terms here - State, Industry, Year, StateYear, and IndustryYear. Is this the correct way to do the Triple Diff? 
JCTCmod4 <- felm(GDP ~ Job.Creation.Tax.Credit + Investment.Tax.Credit + Research.and.Development.Credit + Property.Tax.Abatement + Customized.Job.Training.Subsidy  | State + Industry + Year + statetimeID + industrytimeID, data = IncentivesData)
summary(JCTCmod4)

# Standard Errors are absolutely gigantic: If I have done this right, this leads me to believe that I need to go down the path of merging in as much controll data (population for example). What are other factors to maybe try and control for? 


##### Model with State-Industry
# PLM is used here as we only have two parameters for the fixed effects, which is the max of PLM
JCTCmod5 <- plm(GDP ~ Job.Creation.Tax.Credit + Investment.Tax.Credit + Research.and.Development.Credit + Property.Tax.Abatement + Customized.Job.Training.Subsidy, data = IncentivesData, index = c("StateInd", "Year"), model = "within", effect = "twoways")
summary(JCTCmod5)

# Really big standard error here too. 

tidy_JCTCmod <- tidy(JCTCmod)
write.csv(tidy_JCTCmod, "tidy_JCTCmod.csv")

# The attempt (?) at triple diff
IncentivesData$stateindustrytimeID <- as.numeric(factor(paste0(IncentivesData$StateInd, IncentivesData$Year)))
JCTCmod6 <- plm(GDP ~ Job.Creation.Tax.Credit + Investment.Tax.Credit + Research.and.Development.Credit + Property.Tax.Abatement + Customized.Job.Training.Subsidy, data = IncentivesData, index = c("StateInd", "Year", "stateindustrytimeID"), model = "within", effect = "twoways")
summary(JCTCmod6)

# Not exactly sure why I was able to do here with PLM and not above 

# High Standard Errors again

# How do we compare these models? Not sure we can use an F Stat or Anova here...


###### Method 2: Panel Data Fixed Effects -> State GDP
# Merge in Staye :Level GDP
names(BEABLS_longststates)
BEABLS_longststates2 <- BEABLS_longststates[,c(5:6, 8)]
names(BEABLS_longststates2)[2] <- "FullStateGDP"
IncentivesData25 <- left_join(IncentivesData, BEABLS_longststates2, by = c("State", "Year"))

write.csv(IncentivesData25, "IncentivesData011719.csv")

##############################################################################################
setwd("~/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis")
IncentiveswithStateGDP <- read.csv("IncentivesData011719.csv", stringsAsFactors = FALSE)
# Merging in all of the new data to conduct propensity score matching process 

# Census API
install.packages('censusapi')
library(censusapi)

listCensusApis()

# Time Series Small Area Income and Poverty Estimates: State and County (State)
StateIncomeandPoverty <- getCensus(name = "timeseries/poverty/saipe", key = "a211d64d407137a2d7c8c153711ceae31ed29c2f", region = "state:*", vars = c("SAEMHI_PT", "SAEPOVRTALL_PT", "STABREV", "YEAR"))
  ## GREAT.
# Time Series Small Area Health Insurance Estimates: Small Area Health Insurance Estimates (State)
StateHealthInsurance <- getCensus(name = "timeseries/healthins/sahie", key = "a211d64d407137a2d7c8c153711ceae31ed29c2f", region = "state", vars = c("AGE_DESC", "AGECAT", "GEOCAT", "GEOID", "NAME", "PCTIC_PT", "PCTUI_PT", "RACE_DESC", "RACECAT", "SEX_DESC", "SEXCAT", "STABREV", "STATE", "YEAR"))

########### Merging everything together
library(dplyr)
library(tidyr)
library(gdata)

### Merge in State-Level Data

# Merge in Median Household Income and Poverty Rate by State  
names(IncentiveswithStateGDP)[3] <-"STABREV"
names(StateIncomeandPoverty)[5] <- "Year"
IncentiveswithStateGDP$Year <- as.character(IncentiveswithStateGDP$Year)
Incentivesm1 <- left_join(IncentiveswithStateGDP, StateIncomeandPoverty, by=c("STABREV", "Year"))

# Merge in health insurance coverage for under 65 year olds
StateHealthInsurancesub <- StateHealthInsurance[StateHealthInsurance$AGECAT == 0 & StateHealthInsurance$RACECAT == 0 & StateHealthInsurance$SEXCAT == 0,]
StateHealthInsurancesub <- StateHealthInsurancesub[,c(8,13,15)]
names(StateHealthInsurancesub)[3] <- "Year"
Incentivesm2 <- left_join(Incentivesm1, StateHealthInsurancesub, by=c("STABREV", "Year"))

### Merge in industry level data

## Merge in annual wages for both states (total across all industries) and state-industries 
# Read in and Merge Diffferent Years for CBP #
library(plyr)

# Bind the files

Stincome9701 <- read.csv("StateIndustryWages/Stindustry wagesindustry9701.csv", skip = 4, header = TRUE)
Stincome0205 <- read.csv("StateIndustryWages/Stindustry wagesindustry0205.csv", skip = 4, header = TRUE)
Stincome0609 <- read.csv("StateIndustryWages/Stindustry wagesindustry0609.csv", skip = 4, header = TRUE)
Stincome1013 <- read.csv("StateIndustryWages/Stindustry wagesindustry1013.csv", skip = 4, header = TRUE)
Stincome1417 <- read.csv("StateIndustryWages/Stindustry wagesindustry1417.csv", skip = 4, header = TRUE)

StincomeFull <- cbind(Stincome9701, Stincome0609, Stincome1013, Stincome1417)
Stincome0205 <- Stincome0205[,c(1,3,5:8)]
StincomeFull <- StincomeFull[,c(1:8, 13:16, 21:24, 29:32)]
StincomeFull <- left_join(StincomeFull, Stincome0205, by=c("GeoFips", "LineCode"))

names(StincomeFull) <- c("GeoFips", "GeoName", "LineCode", "Description", "1998",  "1999", "2000", "2001",  "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015",  "2016",  "2017", "2002", "2003", "2004",  "2005")

# Convert from wide to long ahead of merge
StincomeFull_long <- gather(StincomeFull, Year, AnnualWages, `1998`:`2005`, factor_key=TRUE)



# Merge with existing dataset for incentives
StincomeFull_long$Description <- trim(StincomeFull_long$Description)
names(StincomeFull_long)[2] <- "StateFull"
names(StincomeFull_long)[4] <- "Industry"
Incentivesm3 <- left_join(Incentivesm2, StincomeFull_long, by=c("StateFull", "Industry", "Year"))

# To get just the total wages for states
StincomeFull_long_states <- StincomeFull_long[StincomeFull_long$LineCode == 50,]
names(StincomeFull_long_states)[6] <- "AnnualWagesFullState"
Incentivesm4 <- left_join(Incentivesm3, StincomeFull_long_states, by=c("StateFull", "Year"))

# Looks to be lots of missing, but not sure how to rectify
# BEA data only goes to 1998, so we're missing 1997

### Merge   in State-Industry Subsidies data
Stinsubs9700 <- read.csv("StateIndustrySubsidies/Stindustry subs_9700.csv", skip = 4, header = TRUE)
Stinsubs0104 <- read.csv("StateIndustrySubsidies/Stindustry subs_0104.csv", skip = 4, header = TRUE)
Stinsubs0508 <- read.csv("StateIndustrySubsidies/Stindustry subs_0805.csv", skip = 4, header = TRUE)
Stinsubs0912 <- read.csv("StateIndustrySubsidies/Stindustry subs_0912.csv", skip = 4, header = TRUE)
Stinsubs1317 <- read.csv("StateIndustrySubsidies/Stindustry subs_1317.csv", skip = 4, header = TRUE)

StinsubsFull <- cbind(Stinsubs9700, Stinsubs0104, Stinsubs0508, Stinsubs0912)
Stinsubs1317 <- Stinsubs1317[,c(1,3,5:9)]
StinsubsFull <- StinsubsFull[,c(1:8, 13:16, 21:24, 29:32)]
StinsubsFull <- left_join(StinsubsFull, Stinsubs1317, by=c("GeoFips", "LineCode"))

names(StinsubsFull) <- c("GeoFips", "GeoName", "LineCode", "Description", "1997",  "1998", "1999", "2000", "2001", "2002", "2003", "2004",  "2005",  "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015",  "2016",  "2017")

# Convert from wide to long ahead of merge
StinsubsFull_long <- gather(StinsubsFull, Year, AnnualSubs, `1997`:`2017`, factor_key=TRUE)

# Merge with existing dataset for incentives
StinsubsFull_long$Description <- trim(StinsubsFull_long$Description)
names(StinsubsFull_long)[2] <- "StateFull"
names(StinsubsFull_long)[4] <- "Industry.x"
Incentivesm5 <- left_join(Incentivesm4, StinsubsFull_long, by=c("StateFull", "Industry.x", "Year"))

names(Incentivesm4)
# To get just the total wages for states
StinsubsFull_long_states <- StinsubsFull_long[StinsubsFull_long$LineCode == 1,]
names(StinsubsFull_long_states)[6] <- "AnnualSubsFullState"
Incentivesm6 <- left_join(Incentivesm5, StinsubsFull_long_states, by=c("StateFull", "Year"))


# Merge employee compensation data

### Merge   in State-Industry Subsidies data
Stinec9798 <- read.csv("EmployeeCompensation/Employeecompensationbyind9798.csv", skip = 4, header = TRUE)
Stinec9902 <- read.csv("EmployeeCompensation/Employeecompensationbyind9902.csv", skip = 4, header = TRUE)
Stinec0307 <- read.csv("EmployeeCompensation/Employeecompensationbyind0307.csv", skip = 4, header = TRUE)
Stinec0812 <- read.csv("EmployeeCompensation/Employeecompensationbyind0812.csv", skip = 4, header = TRUE)
Stinec1317 <- read.csv("EmployeeCompensation/Employeecompensationbyind1317.csv", skip = 4, header = TRUE)

StinecFull <- cbind(Stinec9798, Stinec0307, Stinec0812)
Stinec9902 <- Stinec9902[,c(1,3,5:8)]
StinecFull <- StinecFull[,c(1:6, 11:15, 20:24)]
StinecFull <- left_join(StinecFull, Stinec9902, by=c("GeoFips", "LineCode"))

Stinec1317 <- Stinec1317[,c(1,3,5:9)]
StinecFull <- left_join(StinecFull, Stinec1317, by=c("GeoFips", "LineCode"))

names(StinecFull) <- c("GeoFips", "GeoName", "LineCode", "Description", "1997",  "1998", "2003", "2004",  "2005",  "2006", "2007", "2008", "2009", "2010", "2011", "2012", "1999", "2000", "2001", "2002",  "2013", "2014", "2015",  "2016",  "2017")

# Convert from wide to long ahead of merge
StinecFull_long <- gather(StinecFull, Year, AnnualEC, `1997`:`2017`, factor_key=TRUE)

# Merge with existing dataset for incentives
StinecFull_long$Description <- trim(StinecFull_long$Description)
names(StinecFull_long)[2] <- "StateFull"
names(StinecFull_long)[4] <- "Industry.x.x"
Incentivesm7 <- left_join(Incentivesm6, StinecFull_long, by=c("StateFull", "Industry.x.x", "Year"))

names(Incentivesm6)
# To get just the total wages for states
StinecFull_long_states <- StinecFull_long[StinecFull_long$LineCode == 1,]
names(StinecFull_long_states)[6] <- "AnnualECFullState"
Incentivesm8 <- left_join(Incentivesm7, StinecFull_long_states, by=c("StateFull", "Year"))

## Merge in per capita personal consumption expenditures by state-industry

Stinperco9798 <- read.csv("Personalconsumptionexp/PersonalExpendituresState9701.csv", skip = 4, header = TRUE)
Stinperco0209 <- read.csv("Personalconsumptionexp/PersonalExpendituresState0209.csv", skip = 4, header = TRUE)
Stinperco1017 <- read.csv("Personalconsumptionexp/PersonalExpendituresState1017.csv", skip = 4, header = TRUE)

StinpercoFull <- cbind(Stinperco9798, Stinperco0209, Stinperco1017)

StinpercoFull <- StinpercoFull[,c(1:9, 14:21, 26:33)]
names(StinpercoFull) <- c("GeoFips", "GeoName", "LineCode", "Description", "1997",  "1998", "1999", "2000", "2001", "2002", "2003", "2004",  "2005",  "2006", "2007", "2008", "2009", "2010", "2011", "2012",  "2013", "2014", "2015",  "2016",  "2017")

# Convert from wide to long ahead of merge
StinpercoFull_long <- gather(StinpercoFull, Year, AnnualPerco, `1997`:`2017`, factor_key=TRUE)

# Merge with existing dataset for incentives
StinpercoFull_long$Description <- trim(StinpercoFull_long$Description)
names(StinpercoFull_long)[2] <- "StateFull"
names(StinpercoFull_long)[4] <- "Industry.x.x.x"
Incentivesm9 <- left_join(Incentivesm8, StinpercoFull_long, by=c("StateFull", "Industry.x.x.x", "Year"))

names(Incentivesm9)
# To get just the total wages for states
StinpercoFull_long_states <- StinpercoFull_long[StinpercoFull_long$LineCode == 1,]
names(StinpercoFull_long_states)[6] <- "AnnualPercoFullState"
Incentivesm10 <- left_join(Incentivesm9, StinpercoFull_long_states, by=c("StateFull", "Year"))

## Merge in StateEconProfiles - A bit more complicated because of multiple indicators instead of just one (No 1997 data)

Stecon9802 <- read.csv("StateEconProfiles/StateEconProfiles9802.csv", skip = 4, header = TRUE)
Stecon0310 <- read.csv("StateEconProfiles/StateEconProfiles0310.csv", skip = 4, header = TRUE)
Stecon1017 <- read.csv("StateEconProfiles/StateEconProfiles1017.csv", skip = 4, header = TRUE)

SteconFull <- cbind(Stecon9802, Stecon0310, Stecon1017)

SteconFull <- SteconFull[,c(1:9, 14:21, 26:33)]
SteconFull$X2010.1 <- NULL
names(SteconFull) <- c("GeoFips", "GeoName", "LineCode", "Description", "1998", "1999", "2000", "2001", "2002", "2003", "2004",  "2005",  "2006", "2007", "2008", "2009", "2010", "2011", "2012",  "2013", "2014", "2015",  "2016",  "2017")

# Convert from wide to long ahead of merge to get years long
SteconFull_long <- gather(SteconFull, Year, Description, `1998`:`2017`, factor_key=TRUE)
names(SteconFull_long)[6] <- "Value"
names(SteconFull_long)[2] <- "StateFull"
SteconFull_long$Description <- trim(SteconFull_long$Description)

# # Convert from long to wide ahead of merge to get indicators wide
# SteconFull_wide <- spread(SteconFull_long, key = Description, value = Value)
SteconFull_long$GeoFips <- NULL
SteconFull_long$LineCode <- NULL
SteconFull_long <- SteconFull_long[,c(1,3,2,4)]

# Pop out to PowerBI to convert to wide because too hard with two categories to pivot on
SteconFull_wide <- read.csv("SteconFull_wide.csv", header = TRUE, 
             na.strings=c("null"), stringsAsFactors = FALSE)
SteconFull_wide$Year <- as.character(SteconFull_wide$Year)

# Merge with existing dataset for incentives
Incentivesm11 <- left_join(Incentivesm10, SteconFull_wide, by=c("StateFull", "Year"))

names(Incentivesm11)
Incentivesm11 <- Incentivesm11[,c(3:38, 40:42, 45, 49, 52, 56, 59, 63, 66, 70:115)]

write.csv(Incentivesm11, "Incentiveswmatching012219.csv")

## 

setwd("~/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis")
FullDataSet <- read.csv("Incentiveswmatching012219.csv", stringsAsFactors = FALSE)
sapply(FullDataSet, function(x) sum(is.na(x)))
# There does not appear to be enough data for state-industry matching, so we'll use state matching for now
# Because of the missingness in 1997 for state characteristics data, we'll have to drop that year from the dataset

FullDataSet <- FullDataSet[FullDataSet$Year != 1997,]
unique(FullDataSet$Year)
sapply(FullDataSet, function(x) sum(is.na(x)))
write.csv(FullDataSet, "FullDataSetupdnoindex_013119.csv")


### Building the Index ####

## Way 1: Easiest but not necessarily best

FullDataSet$AddIndex <- FullDataSet$Job.Creation.Tax.Credit + FullDataSet$Investment.Tax.Credit + FullDataSet$Research.and.Development.Credit + FullDataSet$Property.Tax.Abatement + FullDataSet$Customized.Job.Training.Subsidy

summary(FullDataSet$AddIndex)
# There is an industry that received 17% tax abatement one year

## Way 2: Indexing through Normalization
# https://warwick.ac.uk/fac/soc/pais/research/researchcentres/csgr/index/guide2.pdf

# Normalization
FullDataSet$Normjctc <- (FullDataSet$Job.Creation.Tax.Credit - min(FullDataSet$Job.Creation.Tax.Credit))/(max(FullDataSet$Job.Creation.Tax.Credit)-min(FullDataSet$Job.Creation.Tax.Credit))

FullDataSet$Normitcmean <- (FullDataSet$Investment.Tax.Credit - min(FullDataSet$Investment.Tax.Credit))/(max(FullDataSet$Investment.Tax.Credit)-min(FullDataSet$Investment.Tax.Credit))

FullDataSet$Normrdcmean <- (FullDataSet$Research.and.Development.Credit - min(FullDataSet$Research.and.Development.Credit))/(max(FullDataSet$Research.and.Development.Credit)-min(FullDataSet$Research.and.Development.Credit))

FullDataSet$Normptamean <- (FullDataSet$Property.Tax.Abatement - min(FullDataSet$Property.Tax.Abatement))/(max(FullDataSet$Property.Tax.Abatement)-min(FullDataSet$Property.Tax.Abatement))

FullDataSet$Normcjtsmean <- (FullDataSet$Customized.Job.Training.Subsidy - min(FullDataSet$Customized.Job.Training.Subsidy))/(max(FullDataSet$Customized.Job.Training.Subsidy)-min(FullDataSet$Customized.Job.Training.Subsidy))

# Weighting - Average of normalized variables multiplied by proprtion of value added to State GDP 

FullDataSet$AvgNormIndex <- eval((FullDataSet$Normjctc + FullDataSet$Normitcmean +FullDataSet$Normrdcmean + FullDataSet$Normptamean + FullDataSet$Normcjtsmean)/5)

FullDataSet$IndGdpConWeight <- eval(FullDataSet$GDP/FullDataSet$FullStateGDP)
summary(FullDataSet$IndGdpConWeight)

FullDataSet$MatchIndex <- eval(FullDataSet$AvgNormIndex*FullDataSet$IndGdpConWeight)

# Split index up into four stratifications in order to do multilevel treatment matching (easier than on continuous)
FullDataSet$MatchIndexTreatment <- ifelse(FullDataSet$MatchIndex <= quantile(FullDataSet$MatchIndex, .25),1, ifelse(FullDataSet$MatchIndex > quantile(FullDataSet$MatchIndex, .25) & FullDataSet$MatchIndex <= quantile(FullDataSet$MatchIndex, .50), 2, ifelse(FullDataSet$MatchIndex > quantile(FullDataSet$MatchIndex, .50) & FullDataSet$MatchIndex <= quantile(FullDataSet$MatchIndex, .75), 3, ifelse(FullDataSet$MatchIndex > quantile(FullDataSet$MatchIndex, .75), 4, NA))))

table(FullDataSet$MatchIndexTreatment)

write.csv(FullDataSet, 'FullDataSetforALAMatching_013119.csv')

# We now have four even stratifications against which to match

############# Matching 
# Check Missingness
sapply(FullDataSetim, function(x) sum(is.na(x)))
# Propensity score matching in R using the MatchIt package with nearest-neighbor 1-to-1 matching based on State Characteristics 
install.packages('CBPS')
library(CBPS)

##### Preparing Data for DiD Matching Process - Drop all years that aren't before and after year and convert from long to wide in order to be able to calculate (PLACEHOLDER WILL CHANGE)
FullDataSetDiD <- FullDataSet[FullDataSet$Year == 1998 | FullDataSet$Year == 2015,]

################ Matching for Continuous 
# https://cran.r-project.org/web/packages/twang/vignettes/mnps.pdf

# Load in package for multinomial logistic regression
library(nnet)
install.packages('twang')
library(twang)
library(car)

# Assess collinearity 
reg <- lm(MatchIndexTreatment ~ SAEMHI_PT + SAEPOVRTALL_PT + AnnualWagesFullState + AnnualSubsFullState + AnnualECFullState + AnnualPercoFullState + Personal.income..millions.of.dollars. + Net.earnings.by.place.of.residence + Personal.current.transfer.receipts + Income.maintenance.benefits.1. + Unemployment.insurance.compensation + Retirement.and.other + Dividends..interest..and.rent + Personal.dividend.income + Personal.interest.income + Imputed.interest.receipts.2. + Monetary.interest.receipts + Rental.income.of.persons + Imputed.rent + Monetary.rent + Population..persons..3. + Per.capita.personal.income.4. + Per.capita.personal.current.transfer.receipts.4. + Per.capita.income.maintenance.benefits.4. +  Per.capita.unemployment.insurance.compensation.4. + Per.capita.retirement.and.other.4. + Per.capita.dividends..interest..and.rent.4. + Per.capita.dividends.4. + Per.capita.interest.4. + Per.capita.rent.4. + Earnings.by.place.of.work + Wages.and.salaries + Supplements.to.wages.and.salaries + Employer.contributions.for.employee.pension.and.insurance.funds.5. + Employer.contributions.for.government.social.insurance + Proprietors..income + Nonfarm.proprietors..income + Total.employment..number.of.jobs. + Proprietors.employment + Farm.proprietors.employment.6. + Average.earnings.per.job..dollars. + Average.wages.and.salaries + Average.nonfarm.proprietors..income, data = FullDataSet)

# Run vif (perfect multicollinearity already eliminated with alias function)
vif(reg)

# Run again with high VIFs eliminated (All under 100)
reg2 <- lm(MatchIndexTreatment ~ SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Retirement.and.other + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.personal.income.4. +  Per.capita.unemployment.insurance.compensation.4. + Proprietors.employment + Farm.proprietors.employment.6.+ Average.wages.and.salaries + Average.nonfarm.proprietors..income, data = FullDataSet)

vif(reg2)

multinomlog <- multinom(MatchIndexTreatment ~ SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Retirement.and.other + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.personal.income.4. +  Per.capita.unemployment.insurance.compensation.4. + Proprietors.employment + Farm.proprietors.employment.6.+ Average.wages.and.salaries + Average.nonfarm.proprietors..income, data = FullDataSet)

# Can come back to this later, but it seems like KNN is just too hard for propensity score matching with multiple treatment vars. 

mnps.match.ate <- mnps(factor(MatchIndexTreatment) ~ SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Retirement.and.other + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.personal.income.4. +  Per.capita.unemployment.insurance.compensation.4. + Proprietors.employment + Farm.proprietors.employment.6.+ Average.wages.and.salaries + Average.nonfarm.proprietors..income, data = FullDataSet, estimand = "ATE", verbose = FALSE, stop.method = c("es.mean", "ks.mean"), n.trees = 10000)
mnps.match.ate

# So we would have to do this for each one, with dummies separated out for each (But only per not per pair)

FullDataSet$Strat1 <- ifelse((FullDataSet$MatchIndexTreatment == 1), 1, 0)
FullDataSet$Strat2 <- ifelse((FullDataSet$MatchIndexTreatment == 2), 1, 0)
FullDataSet$Strat3 <- ifelse((FullDataSet$MatchIndexTreatment == 3), 1, 0)
FullDataSet$Srtat4 <- ifelse((FullDataSet$MatchIndexTreatment == 4), 1, 0)

# Strata 1
mnps.match.att <- mnps(factor(MatchIndexTreatment) ~ SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Retirement.and.other + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.personal.income.4. +  Per.capita.unemployment.insurance.compensation.4. + Proprietors.employment + Farm.proprietors.employment.6.+ Average.wages.and.salaries + Average.nonfarm.proprietors..income, data = FullDataSet, estimand = "ATT", treatATT = "1", verbose = FALSE, stop.method = c("es.mean", "ks.mean"), n.trees = 7500)

# Strata 2
mnps.match.att <- mnps(factor(MatchIndexTreatment) ~ SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Retirement.and.other + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.personal.income.4. +  Per.capita.unemployment.insurance.compensation.4. + Proprietors.employment + Farm.proprietors.employment.6.+ Average.wages.and.salaries + Average.nonfarm.proprietors..income, data = FullDataSet, estimand = "ATT", treatATT = "2", verbose = FALSE, stop.method = c("es.mean", "ks.mean"), n.trees = 7500)

# Strata 3
mnps.match.att <- mnps(factor(MatchIndexTreatment) ~ SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Retirement.and.other + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.personal.income.4. +  Per.capita.unemployment.insurance.compensation.4. + Proprietors.employment + Farm.proprietors.employment.6.+ Average.wages.and.salaries + Average.nonfarm.proprietors..income, data = FullDataSet, estimand = "ATT", treatATT = "3", verbose = FALSE, stop.method = c("es.mean", "ks.mean"), n.trees = 7500)

# Strata 4
mnps.match.att <- mnps(factor(MatchIndexTreatment) ~ SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Retirement.and.other + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.personal.income.4. +  Per.capita.unemployment.insurance.compensation.4. + Proprietors.employment + Farm.proprietors.employment.6.+ Average.wages.and.salaries + Average.nonfarm.proprietors..income, data = FullDataSet, estimand = "ATT", treatATT = "4", verbose = FALSE, stop.method = c("es.mean", "ks.mean"), n.trees = 7500)

# The twang methods rely on tree-based regression models that are built in an iterative fashion
# The four stopping rules are defined by two components: a balance metric for covariates and rule for summarizing across covariates
# mnps estimates weights by repeated use of the ps function and comparing each treatment the pooled sample of other treatments

par(mfrow = c(2,2))
plot(mnps.match, plots = 1)

# A key assumption in propensity score analyses is that each experimental unit has a non-zero probability of receiving each treatment.
# The plausibility of this assumption may be assessed by examining the overlap of the empirical propensity score distributions

plot(mnps.match, plots = 2, subset = "es.mean")

plot(mnps.match, plots = 3)

#  In particular, when the plots argument is set equal to 3, it provides comparisons of the absolute standardized mean differences (ASMD) between the treatment groups on the pretreatment covariates, before and after weighting

plot(mnps.match, plots = 4)

# As shown, for each pretreatment variable, the maximum ASMD has decreased and the minimum p-values have increased after applying weights that arise from either stop.method. (What we want)













####################################### Dummy DiD Method ###############################################
# Can I develop an index in any way here? - How to account for a) Different years of adoption, b) Having different combination of incentives at different points? 
names(FullDataSet)
FullDataSet$NoIncentivesTab <- ifelse((FullDataSet$JCTC_Dum_CumSum > 0 | FullDataSet$ITC_Dum_CumSum > 0 | FullDataSet$RDC_Dum_CumSum > 0 | FullDataSet$PTA_Dum_CumSum > 0 | FullDataSet$CJTS_Dum_CumSum > 0), 1, 0)

table(FullDataSet$NoIncentivesTab) # There are 1,374 industries that were not receiving anything at some point!


# Imputing to fix missing data issue 
install.packages('Amelia', dependencies = TRUE)
library(Amelia)

names(FullDataSet)[4] <- "Industry"
FullDataSet <- FullDataSet[,-c(47, 49, 51, 66, 77)]
FullDataSet$AnnualWages <- as.numeric(FullDataSet$AnnualWages)
FullDataSet$AnnualEC <- as.numeric(FullDataSet$AnnualEC)
FullDataSetim <- FullDataSet[,-c(3, 5, 6:7, 9, 10:36, 83:85, 89:96)]
names(FullDataSetim)
Imputed_Full_data_long <-amelia(FullDataSetim,ts= 'Year', cs= 'X', p2s=0, intercs = FALSE, idvars=c('STABREV', 'Industry'))

library(MatchIt)

fmla <- as.formula(NoIncentivesTab ~ SAEMHI_PT + SAEOVRTALL_PT + PCTUI_PT + AnnualWagesFullState + AnnualSubsFullState + EnnualECFullState + AnnualPercoFullState + Personal.income..millions.of.dollars. +  Net.earnings.by.place.of.residence + Personal.current.transfer.receipts + Income.maintenance.benefits.1. + Unemployment.insurance.compensation + Retirement.and.other + Dividends..interest..and.rent + Personal.dividend.income + Personal.interest.income + Imputed.interest.receipts.2. + Monetary.interest.receipts + Rental.income.of.persons + Imputed.rent + Monetary.rent + Population..persons..3. + Per.capita.incomes..dollars. + Per.capita.personal.income.4. + Per.capita.net.earnings.4. + Per.capita.personal.current.transfer.receipts.4.. + Per.capita.income.maintenance.benefits.4. + Per.capita.unemployment.insurance.compensation.4. + Per.capita.retirement.and.other.4. + Per.capita.dividends..interest..and.rent.4. + Per.capita.dividends.4. + Per.capita.interest.4. + Per.capita.rent.4. + Earnings.by.place.of.work + Wages.and.salaries + Supplements.to.wages.and.salaries + Employer.contributions.for.employee.pension.and.insurance.funds.5. + Employer.contributions.for.government.social.insurance + Proprietors..income + Farm.proprietors..income + Farm.proprietors..income +  Nonfarm.proprietors..income + Total.employment..number.of.jobs. +  Wage.and.salary.employment + Proprietors.employment + Farm.proprietors.employment.6. + Nonfarm.proprietors.employment + Average.earnings.per.job..dollars. + Average.wages.and.salaries + Average.nonfarm.proprietors..income)


m.out = matchit(formula = fmla,
                data = FullDataSet, method = "nearest",
                ratio = 1)
summary(m.out)
plot(m.out, type = "jitter")
plot(m.out, type = "hist")







########################## State Spending Finance Data #############################
library(readxl)
library(plyr)
setwd("~/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis/Annual Survey of State Government Finances")

# File Type 1
GovFin1 <- list.files(path = "./1315", pattern="*.csv", full.names = TRUE)

read_csv_filename <- function(filename){
  stats <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE, skip = 1)
  stats$source <- filename #EDIT
  stats
}

GovFin1315 <- ldply(GovFin1, read_csv_filename)

# File Type 2
GovFin2 <- list.files(path = "./2012", pattern="*.csv", full.names = TRUE)

GovFin12 <- ldply(GovFin2, read_csv_filename)

# File Type 3
GovFin3 <- list.files(path = "./0411", pattern="*.xls", full.names = TRUE)

read_xl2_filename <- function(filename){
  stats <- read_excel(filename, skip=2, sheet = 1)
  stats$source <- filename #EDIT
  stats
}

GovFin0411 <- ldply(GovFin3, read_xl2_filename)

# File Type 4
GovFin4 <- list.files(path = "./9903", pattern="*.xls", full.names = TRUE)

GovFin9903 <- ldply(GovFin4, read_xl2_filename)

# File Type 5
GovFin98 <- read_excel("./1998/98states.xls", skip=2, sheet = 2)


#### 0411
# Clean up for bind - Long Format to Combine with other data
names(GovFin0411)
### 0411 - Elminate NA space and convert wide to long
GovFin0411 <- GovFin0411[!is.na(GovFin0411$`ALABAMA`),]

# Convert sources to years
GovFin0411$source[GovFin0411$source == "./0411/04statess.xls"] <- c("2004")
GovFin0411$source[GovFin0411$source == "./0411/05statess.xls"] <- c("2005")
GovFin0411$source[GovFin0411$source == "./0411/06statess.xls"] <- c("2006")
GovFin0411$source[GovFin0411$source == "./0411/07statess.xls"] <- c("2007")
GovFin0411$source[GovFin0411$source == "./0411/08statess.xls"] <- c("2008")
GovFin0411$source[GovFin0411$source == "./0411/09statess.xls"] <- c("2009")
GovFin0411$source[GovFin0411$source == "./0411/10statess.xls"] <- c("2010")
GovFin0411$source[GovFin0411$source == "./0411/11statess.xls"] <- c("2011")
colnames(GovFin0411)[53] <- c("Year")

unique(GovFin0411$Item)
GovFin0411$Item[GovFin0411$Item == "Total revenue"] <- c("Total Revenue")
GovFin0411$Item[GovFin0411$Item == "Liquor stores revenue"] <- c("Liquor store revenue")
GovFin0411$Item[GovFin0411$Item == "Liquor stores expenditure"] <- c("Liquor store expenditure")
GovFin0411$Item[GovFin0411$Item == "Governmental administration"] <- c("Government administration")



# Convert dataset from wide to long 
library(reshape2)
GovFin04112 <- GovFin0411
names(GovFin04112)

GovFin04112 <- gather(GovFin04112, State, Values, `UNITED STATES`:`WYOMING`, factor_key=TRUE)

GovFin04113 <- GovFin04112 %>% 
  group_by(State, Year, Item) %>% 
  mutate(ind = row_number()) %>% 
  spread(Item, Values)

GovFin04113  <- GovFin04113 [!is.na(GovFin04113 $`Capital outlay`),]

## Govfin 9913

GovFin9903$source[GovFin9903$source == "./9903/99statess.xls"] <- c("1999")
GovFin9903$source[GovFin9903$source == "./9903/00statess.xls"] <- c("2000")
GovFin9903$source[GovFin9903$source == "./9903/01statess.xls"] <- c("2001")
GovFin9903$source[GovFin9903$source == "./9903/02statess.xls"] <- c("2002")
GovFin9903$source[GovFin9903$source == "./9903/03statess.xls"] <- c("2003")
colnames(GovFin9903)[155] <- c("Year")

# Subset to just amount 

GovFin99031 <- GovFin9903[,c(1:2., 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50, 53, 56, 59, 62, 65, 68, 71, 74, 77, 80, 83, 86, 89, 92, 95, 98, 101, 104, 107, 110, 113, 116, 119, 122, 125, 128, 131, 134, 137, 140, 143, 146, 149, 152, 155)]

GovFin99031 <- GovFin99031[!is.na(GovFin99031$`ALABAMA`),]
GovFin99031 <- GovFin99031[!is.na(GovFin99031$Item),]

unique(GovFin99031$Item)
GovFin99031$Item[GovFin99031$Item == "Population (thousands, July, 2002)"] <- c("Population")
GovFin99031$Item[GovFin99031$Item == "Population (thousands, July 1, 1999)"] <- c("Population")
GovFin99031$Item[GovFin99031$Item == "Population (thousands, 2003)"] <- c("Population")
GovFin99031$Item[GovFin99031$Item == "Population (thousands, April 1, 2000)"] <- c("Population")
GovFin99031$Item[GovFin99031$Item == "Population (thousands, July 1, 2001)"] <- c("Population")
GovFin99031$Item[GovFin99031$Item == "Personal income (millions, calendar year 1998)"] <- c("Personal Income")
GovFin99031$Item[GovFin99031$Item == "Personal income (millions, calendar year 1999)"] <- c("Personal Income")
GovFin99031$Item[GovFin99031$Item == "Personal income (millions, calendar year 2000)"] <- c("Personal Income")
GovFin99031$Item[GovFin99031$Item == "Total revenue"] <- c("Total Revenue")

GovFin99032 <- gather(GovFin99031, State, Values, `UNITED STATES`:`WYOMING`, factor_key=TRUE)

GovFin99032 <-GovFin99032 %>% 
  group_by(State, Year, Item) %>% 
  mutate(ind = row_number()) %>% 
  spread(Item, Values)

GovFin99032  <- GovFin99032[!is.na(GovFin99032$`Capital outlay`),]

#1315
library(stringr)

GovFin13152 <- GovFin1315

GovFin13152 <- separate(data = GovFin13152, col = Meaning.of.Finance.Source, into = c("a", "b", "c"), sep = "-")
GovFin13152 <- separate(data = GovFin13152, col = a, into = c("aa"), sep = ":")
GovFin13152$b <- ifelse((is.na(GovFin13152$c)), GovFin13152$b, GovFin13152$c)
GovFin13152$aa <- ifelse((is.na(GovFin13152$b)), GovFin13152$aa, GovFin13152$b)
colnames(GovFin13152)[8] <- c("Item")
colnames(GovFin13152)[3] <- c("State")
colnames(GovFin13152)[11] <- c("Values")

GovFin13152 <- GovFin13152[,c(3, 4, 8, 11)]

GovFin13153 <-GovFin13152 %>% 
  group_by(State, Year, Item) %>% 
  mutate(ind = row_number()) %>% 
  spread(Item, Values)

# 2012 
names(GovFin12)
GovFin121 <- gather(GovFin12, Item, Values, `Total.Revenue`:`Cash.and.Security.Holdings`, factor_key=TRUE)
GovFin122 <- separate(data = GovFin121, col = Item, into = c("a", "b", "c" , "d"), sep = "[.][.][.]")
GovFin122 <- separate(data = GovFin122, col = a, into = c("aa", "bb"), sep = "[.][.]")
GovFin122 <- separate(data = GovFin122, col = b, into = c("cc", "dd"), sep = "[.][.]")

GovFin122$c <- ifelse((is.na(GovFin122$d)), GovFin122$c, GovFin122$d)
GovFin122$cc <- ifelse((is.na(GovFin122$c)), GovFin122$cc, GovFin122$c)
GovFin122$aa <- ifelse((is.na(GovFin122$cc)), GovFin122$aa, GovFin122$cc)

GovFin122$aa <- gsub("\\.", " ", GovFin122$aa) 
GovFin122$aa <- str_trim(GovFin122$aa)

GovFin122 <- GovFin122[,c(3, 4, 5, 11)]
colnames(GovFin122)[1] <- c("State")
colnames(GovFin122)[2] <- c("Year")
colnames(GovFin122)[3] <- c("Item")
GovFin122$Year[GovFin122$Year == "./2012/SGF_2012_SGF001_with_ann.csv"] <- c("2012")

GovFin122 <-GovFin122 %>% 
  group_by(State, Year, Item) %>% 
  mutate(ind = row_number()) %>% 
  spread(Item, Values)

GovFin122 <- GovFin122[!is.na(GovFin122$`Capital Outlay`),]

# 1998
GovFin98 <- GovFin98[!is.na(GovFin98$X__1),]

GovFin98 <- gather(GovFin98, State, Values, `United States`:`Wyoming`, factor_key=TRUE)
GovFin98$Year <- 1998
colnames(GovFin98)[1] <- c("Item")

unique(GovFin98$Item)

GovFin981 <-GovFin98 %>% 
  group_by(State, Year, Item) %>% 
  mutate(ind = row_number()) %>% 
  spread(Item, Values)

GovFin981 <- GovFin981[!is.na(GovFin981$`Capital outlay`),]

colnames(GovFin981)[40] <- c("Population")

# Restrict Data Sets to Variables We Actually Need Ahead of Appending 
# 1998
GovFin981 <- GovFin981[,c(1, 2, 13, 20, 21, 30, 41)]
names(GovFin981)
GovFin981[,c(3:7)] <- sapply(GovFin981[,c(3:7)],as.numeric) 
sapply(GovFin981, class)
unique(GovFin981$State)
GovFin981$Year <- as.character(GovFin981$Year)

GovFin99032 <- GovFin99032[,c(1, 2, 13, 19, 20, 29, 41)]
names(GovFin99032)
GovFin99032 <- GovFin99032[,c(2, 1, 3, 4, 5, 6, 7)] # Reorder
GovFin99032[,c(3:7)] <- sapply(GovFin99032[,c(3:7)],as.numeric) 
sapply(GovFin99032, class)

GovFin04113 <- GovFin04113[,c(1, 2, 13, 19, 20, 29, 39)]
names(GovFin04113)
GovFin04113 <- GovFin04113[,c(2, 1, 3, 4, 5, 6, 7)] # Reorder
GovFin04113[,c(3:7)] <- sapply(GovFin04113[,c(3:7)],as.numeric) 
sapply(GovFin04113, class)

GovFin122 <- GovFin122[,c(1, 2, 15, 23, 24, 34, 44)]
names(GovFin122)
GovFin122[,c(3:7)] <- sapply(GovFin122[,c(3:7)],as.numeric) 
sapply(GovFin122, class)
colnames(GovFin122)[6] <- c("Intergovernmental revenue")
colnames(GovFin122)[7] <- c("Public welfare")

GovFin13153 <- GovFin13153[,c(1, 2, 10, 14, 15, 26, 35)]
colnames(GovFin13153)[3] <- c("Education")
colnames(GovFin13153)[4] <- c("Health")
colnames(GovFin13153)[5] <- c("Highways")
colnames(GovFin13153)[6] <- c("Public welfare")
colnames(GovFin13153)[7] <- c("Intergovernmental revenue")
GovFin13153 <- GovFin13153[,c(1, 2, 3, 4, 5, 7, 6)]
GovFin13153[,c(3:7)] <- sapply(GovFin13153[,c(3:7)],as.numeric) 
sapply(GovFin13153, class)
GovFin13153$Year <- as.character(GovFin13153$Year)

GovFinances <- rbind(GovFin981, GovFin99032, GovFin04113, GovFin122, GovFin13153)
library(tools)

GovFinances$State <- tolower(GovFinances$State)
GovFinances$State <- toTitleCase(GovFinances$State)
unique(GovFinances$State)

# Merge
setwd("~/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis")
FullIncentivesforMerge <- read.csv("FullDataSetforALAMatching_013119.csv", stringsAsFactors = FALSE)
names(FullIncentivesforMerge)
colnames(GovFinances)[1] <- c("StateFull")
FullIncentivesforMerge$Year <- as.character(FullIncentivesforMerge$Year)

FullDataSetForMerge <- left_join(FullIncentivesforMerge, GovFinances, by = c("StateFull", "Year"))
names(FullDataSetForMerge)

write.csv(FullDataSetForMerge, "FullDataSetforALAMatching_022519.csv")

################################################################
FullDataSet <- read.csv('FullDataSetforALAMatching_022519.csv', stringsAsFactors = FALSE)

# Comparing Industry-Value Added and Industry GDP 
summary(FullDataSet$Industry.Value.Added)
summary(FullDataSet$GDP)


