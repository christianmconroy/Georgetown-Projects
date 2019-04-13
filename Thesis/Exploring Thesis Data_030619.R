##### Exploring Thesis Data ####

# Set WD
setwd("~/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis")
library(ggplot2)
library(reshape)
library(stargazer)
library(plm)
library(dplyr)
library(tidyverse)
library(tidyr)
library(haven)
library(censusapi)
library(readxl)
library(reshape2)
library(stringr)
library(tools)
# install.packages('statar')
library(statar)
library(memisc)
library(lmtest)

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

# ## Calculating which year would be best for "treatment" and Building Up Table for Explanation - Calculate when 1s Changes to 0 (Do same for industry)

##### JCTC

Incentives$JCTC_Dum_CumSum <- ave(Incentives$JCTC_Dum, Incentives$State, Incentives$Industry, FUN=cumsum)

###### ITC
Incentives$ITC_Dum_CumSum <- ave(Incentives$ITC_Dum, Incentives$State, Incentives$Industry, FUN=cumsum)

##### RDC
Incentives$RDC_Dum_CumSum <- ave(Incentives$RDC_Dum, Incentives$State, Incentives$Industry, FUN=cumsum)

##### PTA
Incentives$PTA_Dum_CumSum <- ave(Incentives$PTA_Dum, Incentives$State, Incentives$Industry, FUN=cumsum)

##### CJTS
Incentives$CJTS_Dum_CumSum <- ave(Incentives$CJTS_Dum, Incentives$State, Incentives$Industry, FUN=cumsum)

################# Import in State-Industry_GDP Time Series and Merge #########################
library(plyr)
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

#### Clean up data 

# Get rid of notation information that was in the csv files 
big_data2 <- big_data2[big_data2$`./NAICS Classified/1997 BEA.csv.Industry` != "Addenda:",]

# Prepare to convert file names to years in the wide form
drop2 <- c("./NAICS Classified/2015 BEA.csv.Industry", "./NAICS Classified/2014 BEA.csv.Industry", "./NAICS Classified/2013 BEA.csv.Industry", "./NAICS Classified/2012 BEA.csv.Industry", "./NAICS Classified/2011 BEA.csv.Industry", "./NAICS Classified/2010 BEA.csv.Industry", "./NAICS Classified/2009 BEA.csv.Industry", "./NAICS Classified/2008 BEA.csv.Industry", "./NAICS Classified/2007 BEA.csv.Industry", "./NAICS Classified/2006 BEA.csv.Industry", "./NAICS Classified/2005 BEA.csv.Industry", "./NAICS Classified/2004 BEA.csv.Industry", "./NAICS Classified/2003 BEA.csv.Industry", "./NAICS Classified/2002 BEA.csv.Industry", "./NAICS Classified/2001 BEA.csv.Industry", "./NAICS Classified/2000 BEA.csv.Industry", "./NAICS Classified/1999 BEA.csv.Industry", "./NAICS Classified/1998 BEA.csv.Industry", "./NAICS Classified/2015 BEA.csv.Area", "./NAICS Classified/2014 BEA.csv.Area", "./NAICS Classified/2013 BEA.csv.Area", "./NAICS Classified/2012 BEA.csv.Area", "./NAICS Classified/2011 BEA.csv.Area", "./NAICS Classified/2010 BEA.csv.Area", "./NAICS Classified/2009 BEA.csv.Area", "./NAICS Classified/2008 BEA.csv.Area", "./NAICS Classified/2007 BEA.csv.Area", "./NAICS Classified/2006 BEA.csv.Area", "./NAICS Classified/2005 BEA.csv.Area", "./NAICS Classified/2004 BEA.csv.Area", "./NAICS Classified/2003 BEA.csv.Area", "./NAICS Classified/2002 BEA.csv.Area", "./NAICS Classified/2001 BEA.csv.Area", "./NAICS Classified/2000 BEA.csv.Area", "./NAICS Classified/1999 BEA.csv.Area", "./NAICS Classified/1998 BEA.csv.Area", "./NAICS Classified/2015 BEA.csv.Fips", "./NAICS Classified/2014 BEA.csv.Fips", "./NAICS Classified/2013 BEA.csv.Fips", "./NAICS Classified/2012 BEA.csv.Fips", "./NAICS Classified/2011 BEA.csv.Fips", "./NAICS Classified/2010 BEA.csv.Fips", "./NAICS Classified/2009 BEA.csv.Fips", "./NAICS Classified/2008 BEA.csv.Fips", "./NAICS Classified/2007 BEA.csv.Fips", "./NAICS Classified/2006 BEA.csv.Fips", "./NAICS Classified/2005 BEA.csv.Fips", "./NAICS Classified/2004 BEA.csv.Fips", "./NAICS Classified/2003 BEA.csv.Fips", "./NAICS Classified/2002 BEA.csv.Fips", "./NAICS Classified/2001 BEA.csv.Fips", "./NAICS Classified/2000 BEA.csv.Fips", "./NAICS Classified/1999 BEA.csv.Fips", "./NAICS Classified/1998 BEA.csv.Fips", "./NAICS Classified/2015 BEA.csv.IndCode", "./NAICS Classified/2014 BEA.csv.IndCode", "./NAICS Classified/2013 BEA.csv.IndCode", "./NAICS Classified/2012 BEA.csv.IndCode", "./NAICS Classified/2011 BEA.csv.IndCode", "./NAICS Classified/2010 BEA.csv.IndCode", "./NAICS Classified/2009 BEA.csv.IndCode", "./NAICS Classified/2008 BEA.csv.IndCode", "./NAICS Classified/2007 BEA.csv.IndCode", "./NAICS Classified/2006 BEA.csv.IndCode", "./NAICS Classified/2005 BEA.csv.IndCode", "./NAICS Classified/2004 BEA.csv.IndCode", "./NAICS Classified/2003 BEA.csv.IndCode", "./NAICS Classified/2002 BEA.csv.IndCode", "./NAICS Classified/2001 BEA.csv.IndCode", "./NAICS Classified/2000 BEA.csv.IndCode", "./NAICS Classified/1999 BEA.csv.IndCode", "./NAICS Classified/1998 BEA.csv.IndCode")

big_data2 <- big_data2[,!(names(big_data2) %in% drop2)]

# Clean to have just years for the wide form
names(big_data2) <- c("Fips", "Area", "IndCode", "Industry", "1997", "1998", "1999", "2000", "2001", "2002", "2003",  "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015")

############## Merge Incentives and BLS Data ###################
##### Clean Up Industry Titles ######

# To be safe
Incentives2 <- Incentives

# Clean up names of industries to match the panel dataset on taxes and incentives 
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

# Merge Abbreviations into BEABLS_long in order to match up states
StateAbbrevs <- read_dta("state_name_abbreviation.dta")
names(StateAbbrevs)[1] <- "State"
BEABLS_longst <- left_join(BEABLS_long, StateAbbrevs, by = "State")
names(BEABLS_longst)[2] <- "StateFull"
names(BEABLS_longst)[8] <- "State"
# Before Merge counts (will obv lose some)
# Incentives: 38610 , BEA/BLS: 37800

# Eliminate leading spaces stemming from hierarchies in BEABLS Data 
BEABLS_longst$Industry <- trimws(BEABLS_longst$Industry, which = "both")

# Get just the annual state GDPs for later merge
BEABLS_longststates <- BEABLS_longst[BEABLS_longst$Industry == "All industry total",]

# Merge Full 

FullIncGDP_9015 <- left_join(Incentives2, BEABLS_longst, by = c("State", "Industry", "Year"))

# Need to eliminate years before 1997 as the GDP by NAICS match only works for 1997 on (Could technicallt use the industry-value added that Bartik includes, but it would not be accurate)
FullIncGDP_9715 <- FullIncGDP_9015[FullIncGDP_9015$Year > 1996,]
# The above takes us from 38610 to 28215 observations

# Eliminate industries without matching GDP Data 

FullIncGDP_9715 <- FullIncGDP_9715[!is.na(FullIncGDP_9715$GDP),]
# The above takes us from 28215 to 21284 observations

write.csv(FullIncGDP_9715, "Full Merged Dataset NAICS 102518.csv")

################### Descriptive Statistics #######################
# Frequency Counts 
# Create Unique Identified for State-Industry
FullIncGDP_9715$stateindID <- as.numeric(factor(paste0(FullIncGDP_9715$State, FullIncGDP_9715$Industry)))

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
  dplyr::summarize(NumberHave = n(),
            Avgabatement = mean(Abatement))

ggplot(data = AdoptIncentivesTime_long_sub, aes(x = as.numeric(Year), y = Avgabatement)) +
  geom_line(aes(colour=Incentive)) + labs(y = "Average Percent in Tax Abatement", x = "Year", title= "Figure: 7 Average Percent in Tax Abatement per Incentive Type Across All State-Industries")


##############################################################################################
setwd("~/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis")
IncentiveswithStateGDP <- read.csv("IncentivesData011719.csv", stringsAsFactors = FALSE)
# Merging in all of the new data to conduct propensity score matching process 

# Time Series Small Area Income and Poverty Estimates: State and County (State)
StateIncomeandPoverty <- getCensus(name = "timeseries/poverty/saipe", key = "a211d64d407137a2d7c8c153711ceae31ed29c2f", region = "state:*", vars = c("SAEMHI_PT", "SAEPOVRTALL_PT", "STABREV", "YEAR"))
  ## GREAT.
# Time Series Small Area Health Insurance Estimates: Small Area Health Insurance Estimates (State)
StateHealthInsurance <- getCensus(name = "timeseries/healthins/sahie", key = "a211d64d407137a2d7c8c153711ceae31ed29c2f", region = "state", vars = c("AGE_DESC", "AGECAT", "GEOCAT", "GEOID", "NAME", "PCTIC_PT", "PCTUI_PT", "RACE_DESC", "RACECAT", "SEX_DESC", "SEXCAT", "STABREV", "STATE", "YEAR"))

########### Merging everything together

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

### Merge  in State-Industry Subsidies data
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

write.csv(FullDataSet, 'FullDataSetforALAMatching_013119.csv')

########################## State Spending Finance Data #############################
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

GovFin04112 <- GovFin0411
names(GovFin04112)

GovFin04112 <- gather(GovFin04112, State, Values, `UNITED STATES`:`WYOMING`, factor_key=TRUE)

GovFin04113 <- GovFin04112 %>% 
  group_by(State, Year, Item) %>% 
  dplyr::mutate(ind = row_number()) %>% 
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
  dplyr::mutate(ind = row_number()) %>% 
  spread(Item, Values)

GovFin99032  <- GovFin99032[!is.na(GovFin99032$`Capital outlay`),]

#1315
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
  dplyr::mutate(ind = row_number()) %>% 
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
  dplyr::mutate(ind = row_number()) %>% 
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
  dplyr::mutate(ind = row_number()) %>% 
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

##### Descriptive Statistics 
library(stargazer)
names(FullDataSetForMerge)
# stargazer(FullDataSetForMerge[,c(32, 38, 14, 15, 16, 17, 18, 103, 39, 40, 45, 49, 56, 59, 65, 66, 72, 85, 94, 11, 12, 13, 7, 8, 108, 106, 105)], mean.sd = TRUE, min.max = FALSE, nobs = FALSE, type = "text", rownames = FALSE, out = "Descstat.txt")

FullDataSetForMerge$Year <- as.numeric(FullDataSetForMerge$Year)
FullDataSetForMerge$Yearcat <- ifelse(FullDataSetForMerge$Year <= 2002,1, ifelse(FullDataSetForMerge$Year > 2002 & FullDataSetForMerge$Year <=2007, 2, ifelse(FullDataSetForMerge$Year > 2007 & FullDataSetForMerge$Year <= 2011, 3, ifelse(FullDataSetForMerge$Year > 2011, 4, NA))))

table(FullDataSetForMerge$Yearcat)

# descstat <- FullDataSetForMerge[,c(32, 38, 14, 15, 16, 17, 18, 103, 39, 40, 45, 49, 56, 59, 65, 66, 72, 85, 94, 11, 12, 13, 7, 8, 108, 106, 105, 109)]

install.packages('doBy')
library(doBy)
library(tibble)
library(dplyr)

# Means
means1 <- summaryBy(Job.Creation.Tax.Credit + Investment.Tax.Credit + Research.and.Development.Credit + Property.Tax.Abatement + Customized.Job.Training.Subsidy + MatchIndex + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + GDP + FullStateGDP + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income ~ Yearcat, FUN=c(mean), data=FullDataSetForMerge)

means2 <- summaryBy(`Public welfare` + Health + Highways ~ Yearcat, FUN=c(mean), data=FullDataSetForMerge, na.rm = TRUE)
means <- as.data.frame(t(cbind(means1, means2)))
means <- means %>% rownames_to_column("Variable")
means <- means[means$Variable != "Yearcat",]
colnames(means) <- c("Var1", "1998-2002 mean", "2003-2007 mean", "2008-2011 mean", "2012-2015 mean")

# SDs
sd1 <- summaryBy(Job.Creation.Tax.Credit + Investment.Tax.Credit + Research.and.Development.Credit + Property.Tax.Abatement + Customized.Job.Training.Subsidy + MatchIndex + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + GDP + FullStateGDP + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income ~ Yearcat, FUN=c('sd'), data=FullDataSetForMerge)

sd2 <- summaryBy(`Public welfare` + Health + Highways ~ Yearcat, FUN=c('sd'), data=FullDataSetForMerge, na.rm = TRUE)
sd <- as.data.frame(t(cbind(sd1, sd2)))
sd <- sd %>% rownames_to_column("Variable")
sd <- sd[sd$Variable != "Yearcat",]
colnames(sd) <- c("Var2", "1998-2002 sd", "2003-2007 sd", "2008-2011 sd", "2012-2015 sd")


meanssd <- cbind(means, sd)
meanssd$Var2 <- NULL

meanssd$Var1 = substr(meanssd$Var1,1,nchar(meanssd$Var1)-5)

# Create the combinations
meanssd$`1998-2002` <- paste(round(meanssd$`1998-2002 mean`,3)," (",round(meanssd$`1998-2002 sd`,3),")",sep="") 
meanssd$`2003-2007` <- paste(round(meanssd$`2003-2007 mean`,3)," (",round(meanssd$`2003-2007 sd`,3),")",sep="")
meanssd$`2008-2011` <- paste(round(meanssd$`2008-2011 mean`,3)," (",round(meanssd$`2008-2011 sd`,3),")",sep="")
meanssd$`2012-2016` <- paste(round(meanssd$`2012-2015 mean`,3)," [",round(meanssd$`2012-2015 sd`,3),"]",sep="")

meanssd <- meanssd[,c(1, 10, 11, 12, 13)]
colnames(meanssd)[1] <- c("Variable")
rownames(meanssd) <- NULL


# Fix Value names for Variables 
meanssd$Variable[meanssd$Variable == "Job.Creation.Tax.Credit"] <- c("JCTC (Tax Abatement)")
meanssd$Variable[meanssd$Variable == "Investment.Tax.Credit"] <- c("ITC (Tax Abatement)")
meanssd$Variable[meanssd$Variable == "Research.and.Development.Credit"] <- c("RDTC (Tax Abatement)")
meanssd$Variable[meanssd$Variable == "Property.Tax.Abatement"] <- c("PTA (Tax Abatement)")
meanssd$Variable[meanssd$Variable == "Customized.Job.Training.Subsidy"] <- c(" CJTS (Tax Abatement)")
meanssd$Variable[meanssd$Variable == "MatchIndex"] <- c("All Incentives (Avg Normalized Tax Abatement")
meanssd$Variable[meanssd$Variable == "Property.Tax"] <- c("Property Tax")
meanssd$Variable[meanssd$Variable == "Sales.Tax"] <- c("Sales Tax")
meanssd$Variable[meanssd$Variable == "Corporate.Income.Tax"] <- c("Corporate Income Tax")
meanssd$Variable[meanssd$Variable == "Export.Industry"] <- c("Export Industry")
meanssd$Variable[meanssd$Variable == "Manufacturing.Industry"] <- c("Manufacturing Industry")
meanssd$Variable[meanssd$Variable == "GDP"] <- c("State-Industry GDP (millions)")
meanssd$Variable[meanssd$Variable == "FullStateGDP"] <- c("State GDP (millions)")
meanssd$Variable[meanssd$Variable == "SAEMHI_PT"] <- c("Median Household Income")
meanssd$Variable[meanssd$Variable == "SAEPOVRTALL_PT"] <- c("All ages in Poverty, Count")
meanssd$Variable[meanssd$Variable == "AnnualSubsFullState"] <- c("Annual State Subsidies")
meanssd$Variable[meanssd$Variable == "AnnualPercoFullState"] <- c("Annual Personal Consumption by State")
meanssd$Variable[meanssd$Variable == "Unemployment.insurance.compensation"] <- c("Unemployment Insurance Compensation")
meanssd$Variable[meanssd$Variable == "Personal.dividend.income"] <- c("Personal Dividend Income")
meanssd$Variable[meanssd$Variable == "Monetary.rent"] <- c("Monetary Rent")
meanssd$Variable[meanssd$Variable == "Population..persons..3."] <- c("Population")
meanssd$Variable[meanssd$Variable == "Per.capita.unemployment.insurance.compensation.4."] <- c("Per Capita Unemployment Insurance Compensation")
meanssd$Variable[meanssd$Variable == "Farm.proprietors..income"] <- c("Farm Proprietors Income")
meanssd$Variable[meanssd$Variable == "Average.nonfarm.proprietors..income"] <- c("Avg Nonfarm Proprietors Income")
meanssd$Variable[meanssd$Variable == "Public welfare"] <- c("Public Welfare Spending (thousands)")
meanssd$Variable[meanssd$Variable == "Highways"] <- c("Highways Spending (thousands)")
meanssd$Variable[meanssd$Variable == "Health"] <- c("Health Spending (thousands)")
unique(meanssd$Variable)


stargazer(meanssd, summary = FALSE, type = "latex", out = "Descstats.html", title = "TABLE 1: Means and [Standard Deviations] for Variables in Regression Models", style = "aer", rownames = FALSE, notes = "<em> Notes: </em> In the above, the explanatory variables include JCTC, ITC, RDTC, PTA, CJTS, and All Incentives. The dependent variables that will be evaluated include State-Industry GDP and State GDP. All others serve as controls and characteristics for matching. A full description of each variable is provided in the appendix.")


###############################################################
install.packages('lm.beta')
library(lm.beta)

setwd("~/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis")
FullDataSetForMerge <- read.csv("FullDataSetforALAMatching_022519.csv", stringsAsFactors = FALSE)

names(FullDataSetForMerge)

########################### Basic Reg Without FE  ##################################
############# Index
# 1.State-Industry GDP Dependent Variable Full State Set (i.e. no state financing vars that would knock out DC)
reg1 <- lm.beta(lm(GDP ~ MatchIndex + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income, data=FullDataSetForMerge))
reg1.rse <- coeftest(reg1, vcov. = vcovHC(reg1, type = "HC1"))

# 2. State GDP Dependent Variable Full State Set (i.e. no state financing vars that would knock out DC)
reg2 <- lm.beta(lm(FullStateGDP ~ MatchIndex + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income, data=FullDataSetForMerge))
reg2.rse <- coeftest(reg2, vcov. = vcovHC(reg2, type = "HC1"))

# 3. State-Industry GDP Dependent Variable Reduced State Set (i.e. state financing vars knock out DC)
reg3 <- lm.beta(lm(GDP ~ MatchIndex + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + Public.welfare + Highways + Health, data=FullDataSetForMerge))
reg3.rse <- coeftest(reg3, vcov. = vcovHC(reg3, type = "HC1"))

# 4. State GDP Dependent Variable Reduced State Set (i.e. state financing vars knock out DC)
reg4 <- lm.beta(lm(FullStateGDP ~ MatchIndex + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + Public.welfare + Highways + Health, data=FullDataSetForMerge))
reg4.rse <- coeftest(reg4, vcov. = vcovHC(reg4, type = "HC1"))

############# All Incentives Individually Included!
# 1.State-Industry GDP Dependent Variable Full State Set (i.e. no state financing vars that would knock out DC)
library(lmtest)
reg5 <- lm.beta(lm(GDP ~ Job.Creation.Tax.Credit + Investment.Tax.Credit + Research.and.Development.Credit + Property.Tax.Abatement + Customized.Job.Training.Subsidy + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income, data=FullDataSetForMerge))
reg5.rse <- coeftest(reg5, vcov. = vcovHC(reg5, type = "HC1"))

# 2. State GDP Dependent Variable Full State Set (i.e. no state financing vars that would knock out DC)
reg6 <- lm.beta(lm(FullStateGDP ~ Job.Creation.Tax.Credit + Investment.Tax.Credit + Research.and.Development.Credit + Property.Tax.Abatement + Customized.Job.Training.Subsidy + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income, data=FullDataSetForMerge))
reg6.rse <- coeftest(reg6, vcov. = vcovHC(reg6, type = "HC1"))

# 3. State-Industry GDP Dependent Variable Reduced State Set (i.e. state financing vars knock out DC)
reg7 <- lm.beta(lm(GDP ~ Job.Creation.Tax.Credit + Investment.Tax.Credit + Research.and.Development.Credit + Property.Tax.Abatement + Customized.Job.Training.Subsidy + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + Public.welfare + Highways + Health, data=FullDataSetForMerge))
reg7.rse <- coeftest(reg7, vcov. = vcovHC(reg7, type = "HC1"))

# 4. State GDP Dependent Variable Reduced State Set (i.e. state financing vars knock out DC)
reg8 <- lm.beta(lm(FullStateGDP ~ Job.Creation.Tax.Credit + Investment.Tax.Credit + Research.and.Development.Credit + Property.Tax.Abatement + Customized.Job.Training.Subsidy + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + Public.welfare + Highways + Health, data=FullDataSetForMerge))
reg8.rse <- coeftest(reg8, vcov. = vcovHC(reg8, type = "HC1"))

########################### With FE and Clustered Standard Error ################################
# Stock and Watson (2008) have shown that the White robust errors are inconsistent in the case of the panel fixed-effects regression model.
# http://www.richard-bluhm.com/clustered-ses-in-r-and-stata-2/
#install.packages('clubSandwich')
library(clubSandwich)
#install.packages('clusterSEs')
library(clusterSEs)
library(multiwayvcov)

FullDataSetForMerge$StateNumID <- as.numeric(factor(FullDataSetForMerge$STABREV))
unique(FullDataSetForMerge$StateNumID)

############ For All Incentives 

# 1.State-Industry GDP Dependent Variable Full State Set (i.e. no state financing vars that would knock out DC)
fe1 <- lm.beta(lm(GDP ~ MatchIndex + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + factor(StateNumID) + factor(Year), data=FullDataSetForMerge))

fe1.cl.cov1 <- cluster.vcov(fe1, FullDataSetForMerge$StateNumID) # cluster-robust SEs for ols1
fe1.cl.robust.se.1 <- sqrt(diag(fe1.cl.cov1))

# 2. State GDP Dependent Variable Full State Set (i.e. no state financing vars that would knock out DC)
fe2 <- lm.beta(m(FullStateGDP ~ MatchIndex + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + factor(StateNumID) + factor(Year), data=FullDataSetForMerge))

fe2.cl.cov1 <- cluster.vcov(fe2, FullDataSetForMerge$StateNumID) # cluster-robust SEs for ols1
fe2.cl.robust.se.1 <- sqrt(diag(fe2.cl.cov1))

# 3. State-Industry GDP Dependent Variable Reduced State Set (i.e. state financing vars knock out DC)
fe3 <- lm.beta(lm(GDP ~ MatchIndex + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + factor(StateNumID) + factor(Year) + Public.welfare + Highways + Health, data=FullDataSetForMerge))

fe3.cl.cov1 <- cluster.vcov(fe3, FullDataSetForMerge$StateNumID) # cluster-robust SEs for ols1
fe3.cl.robust.se.1 <- sqrt(diag(fe3.cl.cov1))

# 4. State GDP Dependent Variable Reduced State Set (i.e. state financing vars knock out DC)
fe4 <- lm.beta(lm(FullStateGDP ~ MatchIndex + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + Public.welfare + Highways + Health + factor(StateNumID) + factor(Year), data=FullDataSetForMerge))

fe4.cl.cov1 <- cluster.vcov(fe4, FullDataSetForMerge$StateNumID) # cluster-robust SEs for ols1
fe4.cl.robust.se.1 <- sqrt(diag(fe4.cl.cov1))

############# All Incentives Individually Included!
# 1.State-Industry GDP Dependent Variable Full State Set (i.e. no state financing vars that would knock out DC)
fe5 <- lm.beta(lm(GDP ~ Job.Creation.Tax.Credit + Investment.Tax.Credit + Research.and.Development.Credit + Property.Tax.Abatement + Customized.Job.Training.Subsidy + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + factor(StateNumID) + factor(Year), data=FullDataSetForMerge))

fe5.cl.cov1 <- cluster.vcov(fe5, FullDataSetForMerge$StateNumID) # cluster-robust SEs for ols1
fe5.cl.robust.se.1 <- sqrt(diag(fe5.cl.cov1))

# 2. State GDP Dependent Variable Full State Set (i.e. no state financing vars that would knock out DC)
fe6 <- lm.beta(lm(FullStateGDP ~ Job.Creation.Tax.Credit + Investment.Tax.Credit + Research.and.Development.Credit + Property.Tax.Abatement + Customized.Job.Training.Subsidy + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + factor(StateNumID) + factor(Year), data=FullDataSetForMerge))

fe6.cl.cov1 <- cluster.vcov(fe6, FullDataSetForMerge$StateNumID) # cluster-robust SEs for ols1
fe6.cl.robust.se.1 <- sqrt(diag(fe6.cl.cov1))

# 3. State-Industry GDP Dependent Variable Reduced State Set (i.e. state financing vars knock out DC)
fe7 <- lm(GDP ~ Job.Creation.Tax.Credit + Investment.Tax.Credit + Research.and.Development.Credit + Property.Tax.Abatement + Customized.Job.Training.Subsidy + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + factor(StateNumID) + factor(Year) + Public.welfare + Highways + Health, data=FullDataSetForMerge)

fe7.cl.cov1 <- cluster.vcov(fe7, FullDataSetForMerge$StateNumID) # cluster-robust SEs for ols1
fe7.cl.robust.se.1 <- sqrt(diag(fe7.cl.cov1))

stargazer(reg1, reg3, reg5, reg7, fe1, fe3, fe5, fe7, se=list(reg1.rse, reg3.rse, reg5.rse, reg7.rse, fe1.cl.robust.se.1, fe3.cl.robust.se.1, fe5.cl.robust.se.1, fe7.cl.robust.se.1), type="html",
          dep.var.labels=c("State-Industry GDP"),
          covariate.labels=c("All Incentives"), out="stateindgdpmodels.htm", style = "aer", title = "These are awesome results!", omit = c("Year", "StateNumID"))
# If I end up wanting to omit any vars from the output - omit = c("SAEMHI_PT", "SAEPOVRTALL_PT")

# 4. State GDP Dependent Variable Reduced State Set (i.e. state financing vars knock out DC)
fe8 <- lm.beta(lm(FullStateGDP ~ Job.Creation.Tax.Credit + Investment.Tax.Credit + Research.and.Development.Credit + Property.Tax.Abatement + Customized.Job.Training.Subsidy + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + Public.welfare + Highways + Health + factor(StateNumID) + factor(Year), data=FullDataSetForMerge))

fe8.cl.cov1 <- cluster.vcov(fe8, FullDataSetForMerge$StateNumID) # cluster-robust SEs for ols1
fe8.cl.robust.se.1 <- sqrt(diag(fe8.cl.cov1))

############################ Propensity Score Matching #############################
############## Matching Based on Whether industries in states EVER had an incentive type

## Generate Dummy for industries in states that never had any incentive types This serves as the treatment variable

FullDataSetForMerge$IncentivesTab <- ifelse((FullDataSetForMerge$Job.Creation.Tax.Credit > 0 | FullDataSetForMerge$Investment.Tax.Credit > 0 | FullDataSetForMerge$Research.and.Development.Credit > 0 | FullDataSetForMerge$Customized.Job.Training.Subsidy > 0 | FullDataSetForMerge$Property.Tax.Abatement > 0), 1, 0)

table(FullDataSetForMerge$IncentivesTab)
eval(18239/(18239+1329))
#Received: 18,239 (93.2%) Never Received: 1,329 (6.8%)

#### Stage 1: Logistic Regression for PSM Process
psmlog1 <- glm(IncentivesTab ~ SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income, data = FullDataSetForMerge, family = 'binomial')

# Predict to get the propensity scores 
FullDataSetForMerge$IncentivesTab_hat <- predict(psmlog1, type = 'response')

## Look at areas of common Support 
# Summarize for both treatment and non-treatment 
summary(FullDataSetForMerge[FullDataSetForMerge$IncentivesTab == 0,]$IncentivesTab_hat)
summary(FullDataSetForMerge[FullDataSetForMerge$IncentivesTab == 1,]$IncentivesTab_hat)

# Look at graphically
ggplot(FullDataSetForMerge, aes(IncentivesTab_hat)) + geom_density(data = FullDataSetForMerge[FullDataSetForMerge$IncentivesTab == 1,], fill = "green", alpha = 0.2) + geom_density(data = FullDataSetForMerge[FullDataSetForMerge$IncentivesTab == 0,], fill = "blue", alpha = 0.2)

# Consider using this or not. 

# Filter based on common support 
minTr <- min(FullDataSetForMerge[FullDataSetForMerge$IncentivesTab==1,]$IncentivesTab_hat)
# 0.751
maxNoTr <- max(FullDataSetForMerge[FullDataSetForMerge$IncentivesTab==0,]$IncentivesTab_hat)
#0.995

# Again, good common support, but the PSMs are very high. At the same time,  the propensity scores for receiving incentives has a min of .751 and a max of .997, while the min for never receiving incentives is slightly higher at  .751 with the same max at .995. The conclusion is that those being dropped are very close to the thresholds. */

# Once we've identified our common support area we limit the sample to those observations that fall within the common support region. In general (but not always), you'll use the LOWEST propensity score from the TREATMENT group as the bottom end of your common support region and the HIGHEST propensity score from your CONTROL group as the top end of your common support region.

FullDataSetForMerge2 <- FullDataSetForMerge[FullDataSetForMerge$IncentivesTab_hat >= minTr & FullDataSetForMerge$IncentivesTab_hat <= maxNoTr,]

nrow(FullDataSetForMerge) - nrow(FullDataSetForMerge2)
# If we use logit, we end up dropping 193
# If we use probit, we end up dropping 193

# Create 5 Stratum 
FullDataSetForMerge2$pq <- xtile(FullDataSetForMerge2$IncentivesTab_hat, 5)
table(FullDataSetForMerge2$pq)

### Within each propensity score block 
# Calculate the mean state-industry GDP in states with and without tax incentive 
aggregate(GDP ~ IncentivesTab + pq, FUN=mean, data= FullDataSetForMerge2)

########## Run Fixed Effects Reg For Each PS Grouping 
# 1.State-Industry GDP Dependent Variable Full State Set (i.e. no state financing vars that would knock out DC)
# Create functions for the model and for the clustered SEs
psmfe <- function(x){
  lm.beta(lm(GDP ~ MatchIndex + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + factor(StateNumID) + factor(Year), data = x))
}

psm.cl.rob <- function(mod,x){
  sqrt(diag(cluster.vcov(mod, x$StateNumID)))
}

psm1 <- psmfe(FullDataSetForMerge2[FullDataSetForMerge2$pq == 1,])
psm1.cl.cov1 <- psm.cl.rob(psm1, FullDataSetForMerge2[FullDataSetForMerge2$pq == 1,])

psm2 <- psmfe(FullDataSetForMerge2[FullDataSetForMerge2$pq == 2,])
psm2.cl.cov1 <- psm.cl.rob(psm2, FullDataSetForMerge2[FullDataSetForMerge2$pq == 2,])

psm3 <- psmfe(FullDataSetForMerge2[FullDataSetForMerge2$pq == 3,])
psm3.cl.cov1 <- psm.cl.rob(psm3, FullDataSetForMerge2[FullDataSetForMerge2$pq == 3,])

psm4 <- psmfe(FullDataSetForMerge2[FullDataSetForMerge2$pq == 4,])
psm4.cl.cov1 <- psm.cl.rob(psm4, FullDataSetForMerge2[FullDataSetForMerge2$pq == 4,])

psm5 <- psmfe(FullDataSetForMerge2[FullDataSetForMerge2$pq == 5,])
psm5.cl.cov1 <- psm.cl.rob(psm5, FullDataSetForMerge2[FullDataSetForMerge2$pq == 5,])

# 2. State GDP Dependent Variable Full State Set (i.e. no state financing vars that would knock out DC)

psmfe2 <- function(x){
  lm.beta(lm(FullStateGDP ~ MatchIndex + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + factor(StateNumID) + factor(Year), data = x))
}

psm6 <- psmfe2(FullDataSetForMerge2[FullDataSetForMerge2$pq == 1,])
psm6.cl.cov1 <- psm.cl.rob(psm6, FullDataSetForMerge2[FullDataSetForMerge2$pq == 1,])

psm7 <- psmfe(FullDataSetForMerge2[FullDataSetForMerge2$pq == 2,])
psm7.cl.cov1 <- psm.cl.rob(psm7, FullDataSetForMerge2[FullDataSetForMerge2$pq == 2,])

psm8 <- psmfe2(FullDataSetForMerge2[FullDataSetForMerge2$pq == 3,])
psm8.cl.cov1 <- psm.cl.rob(psm8, FullDataSetForMerge2[FullDataSetForMerge2$pq == 3,])

psm9 <- psmfe2(FullDataSetForMerge2[FullDataSetForMerge2$pq == 4,])
psm9.cl.cov1 <- psm.cl.rob(psm9, FullDataSetForMerge2[FullDataSetForMerge2$pq == 4,])

psm10 <- psmfe2(FullDataSetForMerge2[FullDataSetForMerge2$pq == 5,])
psm10.cl.cov1 <- psm.cl.rob(psm10, FullDataSetForMerge2[FullDataSetForMerge2$pq == 5,])

# 3. State-Industry GDP Dependent Variable Reduced State Set (i.e. state financing vars knock out DC)
# We have to do the whole matching process again here obviously

fe7 <- lm(GDP ~ Job.Creation.Tax.Credit + Investment.Tax.Credit + Research.and.Development.Credit + Property.Tax.Abatement + Customized.Job.Training.Subsidy + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + factor(StateNumID) + factor(Year) + Public.welfare + Highways + Health, data=FullDataSetForMerge)

fe7.cl.cov1 <- cluster.vcov(fe7, FullDataSetForMerge$StateNumID) # cluster-robust SEs for ols1
fe7.cl.robust.se.1 <- sqrt(diag(fe7.cl.cov1))

stargazer(reg1, reg3, reg5, reg7, fe1, fe3, fe5, fe7, se=list(reg1.rse, reg3.rse, reg5.rse, reg7.rse, fe1.cl.robust.se.1, fe3.cl.robust.se.1, fe5.cl.robust.se.1, fe7.cl.robust.se.1), type="html",
          dep.var.labels=c("State-Industry GDP"),
          covariate.labels=c("All Incentives"), out="stateindgdpmodels.htm", style = "aer", title = "These are awesome results!", omit = c("Year", "StateNumID"))
# If I end up wanting to omit any vars from the output - omit = c("SAEMHI_PT", "SAEPOVRTALL_PT")

# 4. State GDP Dependent Variable Reduced State Set (i.e. state financing vars knock out DC)
fe8 <- lm.beta(lm(FullStateGDP ~ Job.Creation.Tax.Credit + Investment.Tax.Credit + Research.and.Development.Credit + Property.Tax.Abatement + Customized.Job.Training.Subsidy + Property.Tax + Sales.Tax + Corporate.Income.Tax + Export.Industry + Manufacturing.Industry + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + Public.welfare + Highways + Health + factor(StateNumID) + factor(Year), data=FullDataSetForMerge))

########################### Long Term Effects Model ################################
## Preparation 
collapsedincentives <- FullDataSetForMerge %>%
  group_by(StateInd) %>%
  dplyr::summarize(avgjctc = mean(Job.Creation.Tax.Credit),
                   avgitc = mean(Investment.Tax.Credit),
                   avgrdc = mean(Research.and.Development.Credit),
                   avgpta = mean(Property.Tax.Abatement),
                   avgcjts = mean(Customized.Job.Training.Subsidy),
                   avgindex = mean(MatchIndex), 
                   avgpropertytax = mean(Property.Tax),
                   avgsalestax = mean(Sales.Tax),
                   avgcit = mean(Corporate.Income.Tax))

collapsedcontrolbase <- FullDataSetForMerge[FullDataSetForMerge$Year == 1998,]
collapsedcontrolbase <- collapsedcontrolbase[,-c(12:19, 33, 39, 7)]

collapseddependent <- FullDataSetForMerge[FullDataSetForMerge$Year == 2015,]
collapseddependent <- collapseddependent[,c(10, 33, 35, 110, 39, 7)]

longtermeffects1 <- left_join(collapseddependent, collapsedincentives, by = c("StateInd"))
longtermeffects2 <- left_join(longtermeffects1, collapsedcontrolbase, by=c("StateInd"))

####### With Index as Main Independent 
# 1. State-Industry GDP Dependent Variable Full State Set (i.e. no state financing vars that would knock out DC)

lteffreg1 <- lm.beta(lm(GDP ~ avgindex + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + avgpropertytax + avgsalestax + avgcit + Export.Industry + Manufacturing.Industry, data = longtermeffects2))

lteffreg1.rse <- coeftest(lteffreg1, vcov. = vcovHC(lteffreg1, type = "HC1"))

# 2. State GDP Dependent Variable Full State Set (i.e. no state financing vars that would knock out DC)
lteffreg2 <- lm.beta(lm(FullStateGDP ~ avgindex + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + avgpropertytax + avgsalestax + avgcit + Export.Industry + Manufacturing.Industry, data = longtermeffects2))

lteffreg2.rse <- coeftest(lteffreg2, vcov. = vcovHC(lteffreg2, type = "HC1"))

# 3. State-Industry GDP Dependent Variable Reduced State Set (i.e. state financing vars knock out DC)
lteffreg3 <- lm.beta(lm(GDP ~ avgindex + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + avgpropertytax + avgsalestax + avgcit + Export.Industry + Manufacturing.Industry + Public.welfare + Highways + Health, data = longtermeffects2))

lteffreg3.rse <- coeftest(lteffreg3, vcov. = vcovHC(lteffreg3, type = "HC1"))

# 4. State GDP Dependent Variable Reduced State Set (i.e. state financing vars knock out DC)
lteffreg4 <- lm.beta(lm(FullStateGDP ~ avgindex + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + avgpropertytax + avgsalestax + avgcit + Export.Industry + Manufacturing.Industry + Public.welfare + Highways + Health, data = longtermeffects2))

lteffreg4.rse <- coeftest(lteffreg4, vcov. = vcovHC(lteffreg4, type = "HC1"))

####### With Individual Incentives as Main Independent 
# 1. State-Industry GDP Dependent Variable Full State Set (i.e. no state financing vars that would knock out DC)

lteffreg5 <- lm.beta(lm(GDP ~ avgjctc + avgitc + avgrdc + avgpta + avgcjts + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + avgpropertytax + avgsalestax + avgcit + Export.Industry + Manufacturing.Industry, data = longtermeffects2))

lteffreg5.rse <- coeftest(lteffreg5, vcov. = vcovHC(lteffreg5, type = "HC1"))

# 2. State GDP Dependent Variable Full State Set (i.e. no state financing vars that would knock out DC)
lteffreg6 <- lm.beta(lm(FullStateGDP ~ avgjctc + avgitc + avgrdc + avgpta + avgcjts + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + avgpropertytax + avgsalestax + avgcit + Export.Industry + Manufacturing.Industry, data = longtermeffects2))

lteffreg6.rse <- coeftest(lteffreg6, vcov. = vcovHC(lteffreg6, type = "HC1"))

# 3. State-Industry GDP Dependent Variable Reduced State Set (i.e. state financing vars knock out DC)
lteffreg7 <- lm.beta(lm(GDP ~ avgjctc + avgitc + avgrdc + avgpta + avgcjts + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + avgpropertytax + avgsalestax + avgcit + Export.Industry + Manufacturing.Industry + Public.welfare + Highways + Health, data = longtermeffects2))

lteffreg7.rse <- coeftest(lteffreg7, vcov. = vcovHC(lteffreg7, type = "HC1"))

# 4. State GDP Dependent Variable Reduced State Set (i.e. state financing vars knock out DC)
lteffreg8 <- lm.beta(lm(FullStateGDP ~ avgjctc + avgitc + avgrdc + avgpta + avgcjts + SAEMHI_PT + SAEPOVRTALL_PT + AnnualSubsFullState + AnnualPercoFullState + Unemployment.insurance.compensation + Personal.dividend.income + Monetary.rent + Population..persons..3. + Per.capita.unemployment.insurance.compensation.4. + Farm.proprietors..income + Average.nonfarm.proprietors..income + avgpropertytax + avgsalestax + avgcit + Export.Industry + Manufacturing.Industry + Public.welfare + Highways + Health, data = longtermeffects2))

lteffreg8.rse <- coeftest(lteffreg8, vcov. = vcovHC(lteffreg8, type = "HC1"))

stargazer(reg1, reg3, reg5, reg7, fe1, fe3, fe5, fe7, lteffreg1, lteffreg3, lteffreg5, lteffreg7, se=list(reg1.rse, reg3.rse, reg5.rse, reg7.rse, fe1.cl.robust.se.1, fe3.cl.robust.se.1, fe5.cl.robust.se.1, fe7.cl.robust.se.1, lteffreg1.rse, lteffreg3.rse, lteffreg5.rse, lteffreg7.rse), type="html",
          dep.var.labels=c("State-Industry GDP"),
          covariate.labels=c("All Incentives"), out="stateindgdpmodels.htm", style = "aer", title = "These are awesome results!", omit = c("Year", "StateNumID"), add.lines = list(c("State fixed effects", rep("NO", 4), rep("YES", 4), rep("NO", 4)), c("Time fixed effects", rep("NO", 4), rep("YES", 4), rep("NO", 4)), c("Government Finance Controls", "Yes", "No", "Yes", "No", "YES", "NO", "YES", "NO", "YES", "NO", "YES", "NO"), c("Robust Standard Errors", rep("YES", 4), rep("NO", 4), rep("YES", 4)), c("Clustered Standard Errors", rep("NO", 4), rep("YES", 4), rep("NO", 4))), column.labels = c(rep("Basic OLS", 4), rep("Two-Way Fixed Effects", 4), rep("Long-Term Effects", 4)))
# If I end up wanting to omit any vars from the output - omit = c("SAEMHI_PT", "SAEPOVRTALL_PT")
# STILL TO DO! DECIDE WHICH VARS TO TAKE OUT, FIXATE ON THE RIGHT TABLE NAME, UNDERLINE THE DEP VAR, DOUBLE CHECK ON EXAMPLES 

stargazer(reg2, reg4, reg6, reg8, fe2, fe4, fe6, fe8,lteffreg2, lteffreg4, lteffreg6, lteffreg8, se=list(reg2.rse, reg4.rse, reg6.rse, reg8.rse, fe2.cl.robust.se.1, fe4.cl.robust.se.1, fe6.cl.robust.se.1, fe6.cl.robust.se.1, lteffreg2.rse, lteffreg4.rse, lteffreg6.rse, lteffreg8.rse), type="html",
          dep.var.labels=c("State GDP"),
          covariate.labels=c("All Incentives"), out="stateindgdpmodels.htm", style = "aer", title = "These are awesome results!", omit = c("Year", "StateNumID"), add.lines = list(c("State fixed effects", rep("NO", 4), rep("YES", 4), rep("NO", 4)), c("Time fixed effects", rep("NO", 4), rep("YES", 4), rep("NO", 4)), c("Government Finance Controls", "Yes", "No", "Yes", "No", "YES", "NO", "YES", "NO", "YES", "NO", "YES", "NO"), c("Robust Standard Errors", rep("YES", 4), rep("NO", 4), rep("YES", 4)), c("Clustered Standard Errors", rep("NO", 4), rep("YES", 4), rep("NO", 4))), column.labels = c(rep("Basic OLS", 4), rep("Two-Way Fixed Effects", 4), rep("Long-Term Effects", 4)))









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
