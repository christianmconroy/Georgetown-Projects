hellb <- read.table("/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester3Fall2017MPP/Math426ALA/hellbender.txt", header = T)
attach(hellb)
library(nlme)
#################################################################### #### Longitudinally Sampled Variable 1 - Current #####

# First restrict the data to the first longitudinally sampled variable we are interested in and then convert to wide format. 
install.packages('tidyverse', repos = "http://cran.us.r-project.org")
library(tidyverse)
current <- hellb[c(1:4)]
attach(current)
current_wide <- spread(current, key=Month, value=Current)

# Descriptive statistics for the two groups 
library(psych)
describeBy(current_wide, current_wide$Known)

# Plot the observed trajectories for the individual streams in each group
current_wide_k <- current_wide[which(current_wide$Known == 1),3:12]
current_wide_u <- current_wide[which(current_wide$Known == 0),3:12]

matplot(c(6,6.5,7,7.6,8,8.5,9,9.5,10,10.5), t(current_wide_k), type='b', pch=20, lty=1, main= "Current by Bimonth (Known)", col='gray', xlab =
          "Bimonth", ylab = "Current")
matplot(c(6,6.5,7,7.6,8,8.5,9,9.5,10,10.5), t(current_wide_u), type='b', pch=20, lty=1, main= "Current by Bimonth (Unknown)", col='gray', xlab =
          "Bimonth", ylab = "Current")

# PLot the mean response profiles for Current for each respective group (With Hellbender population and without)
plot(c(6,6.5,7,7.5, 8, 8.5, 9, 9.5, 10, 10.5), apply(current_wide_k, 2, mean),  ylim = c(0,1.75), xlab = 'Time (bimonthly)', ylab = 'Mean Current', main = "Current response per stream", type = 'n')
abline(v = axTicks(1), h = axTicks(2), col = rgb(0.75, 0.75, 0.75, alpha = 0.5), lty = 3)
lines(c(6,6.5,7,7.5, 8, 8.5, 9, 9.5, 10, 10.5), apply(current_wide_k, 2, mean), type = 'b', pch = 1, lty = 2)
lines(c(6,6.5,7,7.5, 8, 8.5, 9, 9.5, 10, 10.5), apply(current_wide_u, 2, mean), type = 'b', pch = 16)
legend('bottomleft', legend = c('Known', 'Unknown'), lty =
         c(2,1), pch = c(1,16), cex = 0.5)

# Under certain conditions, we can look at the raw matrix to give me a sense of where to start with making assumptions about the covariance. 
cov(current_wide[,c(3:12)])
cor(current_wide[,c(3:12)])

#### Assessing Covariance Structures ####
# Setting up the data 
attach(current)

current$occur <- rep(1:10, length(unique(current$ID)))

head(current)

current <- current[order(current$ID),]

# Unstructured Covariance - Does not work as there are too many parameters (55).  

# Assessing the compound symmetry structure 
#Homogeneous
model2 <- gls(Current ~ factor(Known) * factor(Month), data = current, correlation = corCompSymm(form = ~ occur | ID))
summary(model2)

getVarCov(model2)
cov2cor(getVarCov(model2)) 

# Heterogeneous
model3 <- gls(Current ~ factor(Known) * factor(Month), data = current, correlation = corCompSymm(form = ~ occur | ID), weights = varIdent(form = ~ 1 | occur))
summary(model3)

getVarCov(model3)
cov2cor(getVarCov(model3)) 

anova(model2, model3)
# In this case it looks like, based on the p-value of 0.5736, we fail to reject the null and would conclude that homogeneous is a better fit here (Model2). 

# Toeplitz 

# Homogeneous 
model4 <- gls(Current ~ factor(Known)*factor(Month), data = current, correlation = corARMA(form = ~ 1 | ID, p = 3, q = 0))
summary(model4)

getVarCov(model4)
cov2cor(getVarCov(model4)) 

# Heterogeneous 
model5 <- gls(Current ~ factor(Known)*factor(Month), data = current, correlation = corARMA(form = ~ 1 | ID, p = 3, q = 0),
              weights = varIdent( form = ~ 1 | occur))
summary(model5)

getVarCov(model5)
cov2cor(getVarCov(model5)) 

anova(model4, model5)
# Based on the p-value of 0.3719, we fail to reject the null and conclude that homogeneous Toeplitz is more appropriate for fitting the model (Model4).

# Checking out AR1 Structure
# Homogeneous 
model6 <- gls(Current ~ factor(Known) * factor(Month), data = current, correlation = corAR1(form = ~ occur | ID))
summary(model6)

getVarCov(model6)
cov2cor(getVarCov(model6)) 

# Heterogeneous 
model7 <- gls(Current ~ factor(Known) * factor(Month), data = current, correlation = corAR1(form = ~ occur | ID), weights = varIdent(form = ~ 1 | occur))
summary(model7)

getVarCov(model7)
cov2cor(getVarCov(model7)) 

anova(model6, model7)
# In this case,based on the p-value of 0.2979, it looks like we fail to reject the null and would conclude that homogeneous is a better fit here (Model6). 

# Homogeneous Toeplitz and Homogeneous AR1
anova(model4,model6)
# These are nested. Based on the p-value of 2e-04, it would appear that Homogeneous AR1 is the most appropriate model. However, Ar1 is more restrictive of an assumption. Given that the unstructured model doesn't converge, the Toeplitz model is the most conservative. You might be making too strong of an assumption with AR1, so we wouldn't be able to trust the inferences. In fact, we can confirm this in comparing the standard errors on the coefficients between the heterogeneous Toeplitz model and the homogeneous AR1 model. Overall, the standard errors are smaller on the majority of the coefficients in the Toeplitz model. Similarily, the p-values are also generally lower on coefficients in the model with the Toeplitz covariance structure. Therefore, we will choose to use the Toeplitz covariance structure to fit the model.If we had sampled more streams, potentially we would have a clearer picture. 

# Homogeneous Compound and Homogeneous AR1 - AIC/BIC
anova(model2, model6)
# These are not nested. But in looking at the AIC and BIC, it looks like compound symmetry may be better because we are being constrained to just two parameters. However, this is not a test with null hypotheses and we can therefore not make strong conclusions from just looking at the AIC and BIC. Compound symmetric suggests that successive measurements have the same amount of correlation between them.Is there any affect of time on the correlation as the measurements get farther apart? Yes, and therefore compound symmetry is likely not the most appropriate. 

# Homogeneous Compound and Homogeneous Toeplitz
anova(model2, model4)
# These are nested. As made evident by the p-value of 0.3077, the compound symmetric model appears more appropriate.

# Homogeneous Toeplitz and Homogeneous AR1
anova(model4,model6)
# These are nested. Based on the p-value of 2e-04, it would appear that Homogeneous AR1 is the most appropriate model. However, Ar1 is more restrictive of an assumption. Given that the unstructured model doesn't converge, the Toeplitz model is the most conservative. You might be making too strong of an assumption with AR1, so we wouldn't be able to trust the inferences. In fact, we can confirm this in comparing the standard errors on the coefficients between the heterogeneous Toeplitz model and the homogeneous AR1 model. Overall, the standard errors are smaller  on the majority of the coefficients in the Toeplitz model. Similarily, the p-values are also generally lower on coefficients in the model with the Toeplitz covariance structure. Therefore, we will choose to use the Toeplitz covariance structure to fit the model.If we had sampled more streams, potentially we would have a clearer picture. 


###### Assessing whether the parametric model with time as linear might be appropriate ########
model7 <- gls(Current ~ factor(Known) * factor(Month), data = current, correlation = corARMA(form = ~ 1 | ID, p = 3, q = 0), method = 'ML')

model8 <- gls(Current ~ factor(Known) + Month, data = current, correlation = corARMA(form = ~ 1 | ID, p = 3, q = 0), method = 'ML')

anova(model7, model8)

# As evident by the p-value of 0.1403 and lower AIC for Model8, a non-parametric linear model may have been appropriate here.  

###### Assessing quadratics #######
current$Month2 <- current$Month^2

model9 <- gls(Current ~ Known*Month + Known*Month2 - Known, 
              data = current, correlation = corARMA(form = ~ 1 | ID, p = 3, q = 0), method = 'ML')

anova(model7, model9)
# the p-value of 0.0798 indicates that we can reject the null, and therefore may need to consider the non-parametric model with quadratics. 

# It is very clear from the plot that a linear splice model would not be appropriate here given how many potential  "knots" there would be. We therefore do not attempt to fit a linear splice model here. We're modeling the means, so we'd want to think about if we were to fit a linear model to this and treat it linearly, maybe it seems like it's flat and then a slight increase after a time-point, then that is the way to think about linear spllicing. When we pick a linear model also, we're trying to smooth out the variability in the means. 

##### Our tests for coicidence and parallel are below. #####
model4 <- gls(Current ~ factor(Known)*factor(Month), data = current, correlation = corARMA(form = ~ 1 | ID, p = 3, q = 0))
# Test the null that the mean response profiles of the two groups are idential. (Slide 12: Coincidence deals with group)
# Test the null that the mean response profiles of the two groups are flat (Slide 12: Coincidence deals with time)
# Test the null hypothesis that the pattern of means over bimonthly measurement intervals are parallel for the two groups. (Slide 12: Parallel deals with group * time)

capture.output(anova(model4), file = "New.txt")

# As evidenced by the p-value of .2187 on factor(Known), we cannot reject the null and conclude that the two groups are identical (coincicent). 
# As evidenced by the p-value of .7115 on factor(Month), we can reject the null and conclude that the mean response profiles are not flat.
# As evidenced by the p-value of .3964 on factor(Known):factor(Month), we cannot reject the null and conclude that the mean response profiles are parallel
# Based on these results, we would conclude that the two groups are not flat, but they are parallel and coincident. 

# The potential lack of significant result here could be due to some limitations of the data. 

## Conclusion of the output for our selected model ##
install.packages('sandwich', repos = "http://cran.us.r-project.org")
library(sandwich)
indModel <- lm(Current ~ factor(Known) * factor(Month), data = current)
diag(sandwich(indModel))
sqrt(diag(sandwich(indModel)))

# Look at empirical estimators -> The most delicious of all statistical estimators. If we are overly concerend about fit. How do the inferences change? We fit an independence model (Multiple linear regression). Do it without weighting, covariance structure. This gives us the variances of the coefficients. So we'll look at the variances. Take the sqrt of the variances diagonal. The sandwich estimator is robust to misspecification. If we're concerned that we've placed too much structure. It's not necessarily that one will be stronger than the other, but putting too strict a structure on your model would lead to incorrect standard errors which could lead to false inferences. If SE from compound are small, you might reject coefficients, but robust estimators might be larger leading you to fail to reject. If you correctly specificy the model, you'll gain power. Sandwich is robust but not powerful. 
summary(model4)


# We do not have any statistically significant coefficients.The presence of the salamnnders in the stream does not appear to have any influence on the environmental factor of the stream.  

####################################################################
##Longitudinally Sampled Variable 2 - Dissolved Oxygen Level ##

disox <- hellb[c(1:3,5)]
attach(disox)
disox_wide <- spread(disox, key=Month, value=DO)

# Descriptive statistics for the two groups 
library(psych)
describeBy(disox_wide, disox_wide$Known)

# Plot the observed trajectories for the individual streams in each group
disox_wide_k <- disox_wide[which(disox_wide$Known == 1),3:12]
disox_wide_u <- disox_wide[which(disox_wide$Known == 0),3:12]

matplot(c(6,6.5,7,7.6,8,8.5,9,9.5,10,10.5), t(disox_wide_k), type='b', pch=20, lty=1, main= "DO by Bimonth (Known)", col='gray', xlab =
          "Bimonth", ylab = "DO")
matplot(c(6,6.5,7,7.6,8,8.5,9,9.5,10,10.5), t(disox_wide_u), type='b', pch=20, lty=1, main= "DO by Bimonth (Unknown)", col='gray', xlab =
          "Bimonth", ylab = "DO")

# PLot the mean response profiles for Current for each respective group (With Hellbender population and without)
plot(c(6,6.5,7,7.5, 8, 8.5, 9, 9.5, 10, 10.5), apply(disox_wide_k, 2, mean),  ylim = c(7,13), xlab = 'Time (bimonthly)', ylab = 'Mean DO', main = "DO response per stream", type = 'n')
abline(v = axTicks(1), h = axTicks(2), col = rgb(0.75, 0.75, 0.75, alpha = 0.5), lty = 3)
lines(c(6,6.5,7,7.5, 8, 8.5, 9, 9.5, 10, 10.5), apply(disox_wide_k, 2, mean), type = 'b', pch = 1, lty = 2)
lines(c(6,6.5,7,7.5, 8, 8.5, 9, 9.5, 10, 10.5), apply(disox_wide_u, 2, mean), type = 'b', pch = 16)
legend('bottomleft', legend = c('Known', 'Unknown'), lty =
         c(2,1), pch = c(1,16), cex = 0.5)

# Under certain conditions, we can look at the raw matrix to give me a sense of where to start with making assumptions about the covariance. 
cov(disox_wide[,c(3:12)])
cor(disox_wide[,c(3:12)])

# No evident pattern from the covariance structure. However, similar to with Current, we suspect that we'll want a covariance structure that does not have constant variance and that allows correlations to change as intervals between time measurements being correlated increase. Interesting that there are negative values though. 

#### Assessing Covariance Structures ####
# Setting up the data 
attach(disox)

disox$occur <- rep(1:10, length(unique(disox$ID)))

head(disox)

disox <- disox[order(disox$ID),]

# Assessing the compound symmetry structure 
#Homogeneous
model10 <- gls(DO ~ factor(Known) * factor(Month), data = disox, correlation = corCompSymm(form = ~ occur | ID))
summary(model10)

getVarCov(model10)
cov2cor(getVarCov(model10)) 

# Heterogeneous
model11 <- gls(DO ~ factor(Known) * factor(Month), data = disox, correlation = corCompSymm(form = ~ occur | ID), weights = varIdent(form = ~ 1 | occur))
summary(model11)

getVarCov(model11)
cov2cor(getVarCov(model11)) 

anova(model10, model11)
# In this case, as made evident by the p-value of 0.3139, it looks like we fail to reject the null and would conclude that homogeneous is a better fit here (Model10). 

# Toeplitz 

# Homogeneous 
model12 <- gls(DO ~ factor(Known) * factor(Month), data = disox, correlation = corARMA(form = ~ 1 | ID, p = 3, q = 0))
summary(model12)

getVarCov(model12)
cov2cor(getVarCov(model12)) 

# Heterogeneous 
model13 <- gls(DO ~ factor(Known) * factor(Month), data = disox, correlation = corARMA(form = ~ 1 | ID, p = 3, q = 0),
              weights = varIdent( form = ~ 1 | occur))
summary(model13)

getVarCov(model13)
cov2cor(getVarCov(model13)) 

anova(model12, model13)
# Based on the p-value of 0.3237, it looks again like the homogeneous Toeplitz structure is best here. 

# Checking out AR1 Structure
# Homogeneous 
model14 <- gls(DO ~ factor(Known) * factor(Month), data = disox, correlation = corAR1(form = ~ occur | ID))
summary(model14)

getVarCov(model14)
cov2cor(getVarCov(model14)) 

# Heterogeneous 
model15 <- gls(DO ~ factor(Known) * factor(Month), data = disox, correlation = corAR1(form = ~ occur | ID), weights = varIdent(form = ~ 1 | occur))
summary(model15)

getVarCov(model15)
cov2cor(getVarCov(model15)) 

anova(model14, model15)
# In this case, as made evident by the p-value of 0.3899, it looks like we fail to reject the null and would conclude that homogeneous is a better fit here (Model14). 

# Homogeneous Compound and Homogeneous AR1 - AIC/BIC
anova(model10, model14)
# These are not nested. But in looking at the AIC and BIC, it looks like compound symmetry may be marginally better because we are being constrained to just two parameters. However, this is not a test with null hypotheses and we can therefore not make strong conclusions from just looking at the AIC and BIC. Compound symmetric suggests that successive measurements have the same amount of correlation between them. Espeically given that the AIC is only slightly better for homogeneous CS, we can reject the conclusion that CS would be the best fit. 

# Homogeneous Compound and Homogeneous Toeplitz
anova(model10, model12)
# The high p-value of 0.7245 would lead us to fail to reject the null and conclude that the homogeneous CS model would be the best. However, again given that CS does not suit the longitudinal data, we will decide to fit with the Homogeneous Toeplitz. 

# Homogeneous AR1 and Homogeneous Toeplitz
anova(model14, model12)
# The high p-value of 0.6639 would lead us to the conclusion that the AR1 model might be most appropriate here. However, Ar1 is more restrictive of an assumption. Given that the unstructured model doesn't converge, the Toeplitz model is the most conservative. You might be making too strong of an assumption with AR1, so we wouldn't be able to trust the inferences. In fact, we can confirm this in comparing the standard errors on the coefficients between the heterogeneous Toeplitz model and the homogeneous AR1 model. Overall, the standard errors are smaller  on the majority of the coefficients in the Toeplitz model. Similarily, the p-values are also generally lower on coefficients in the model with the Toeplitz covariance structure. Therefore, we will choose to use the Toeplitz covariance structure to fit the model.If we had sampled more streams, potentially we would have a clearer picture. 


###### Assessing whether the parametric model with time as linear might be appropriate ########
model16 <- gls(DO ~ factor(Known) * factor(Month), data = disox, correlation = corAR1(form = ~ occur | ID), method = 'ML')

model17 <- gls(DO ~ factor(Known) + Month, data = disox, correlation = corAR1(form = ~ occur | ID), method = 'ML')

anova(model16, model17)

# As evident by the p-value of <.0001 and the lower AIC for model16, we can conclude that the non-parametric model is more appropriate for fitting the data. 

# It is very clear from the plot that a linear splice model would not be appropriate here given how many potential  "knots" there would be. We therefore do not attempt to fit a linear splice model here. We're modeling the means, so we'd want to think about if we were to fit a linear model to this and treat it linearly, maybe it seems like it's flat and then a slight increase after a time-point, then that is the way to think about linear spllicing. When we pick a linear model also, we're trying to smooth out the variability in the means. 

##### Our tests for coicidence and parallel are below. #####

# Test the null that the mean response profiles of the two groups are idential. (Slide 12: Coincidence deals with group)
# Test the null that the mean response profiles of the two groups are flat (Slide 12: Coincidence deals with time)
# Test the null hypothesis that the pattern of means over bimonthly measurement intervals are parallel for the two groups. (Slide 12: Parallel deals with group * time)
# Coincidence
model19 <- gls(DO ~ factor(Month), data = disox, correlation = corAR1(form = ~ occur | ID), method = 'ML')

model20 <- gls(DO ~ factor(Known) * factor(Month), data = disox, correlation = corAR1(form = ~ occur | ID), method = 'ML')

anova(model19, model20)
# Based on the p-value of .0108 and the lower AIC for model20, we conclude that there is not coincidence. 

# Flat
model21 <- gls(DO ~ factor(Known), data = disox, correlation = corAR1(form = ~ occur | ID), method = 'ML')

model22 <- gls(DO ~ factor(Known) * factor(Month), data = disox, correlation = corAR1(form = ~ occur | ID), method = 'ML')

anova(model21, model22)
# As evident by the p-value of < .00001 and the lower AIC for Model22, we can conclude that there is no flatness. 

# Parallel
model23 <- gls(DO ~ factor(Known) + factor(Month), data = disox, correlation = corAR1(form = ~ occur | ID), method = 'ML')

model24 <- gls(DO ~ factor(Known) * factor(Month), data = disox, correlation = corAR1(form = ~ occur | ID), method = 'ML')

anova(model23, model24)
# As evident by the p-value of .0097 and the lower AIC for model24, we can conclude that they are not parallel. 

# The bigger issue is just the small samples period period (One has 2 and the other only has 4)

## Conclusion of the output for our selected model ##
indModel2 <- lm(DO ~ factor(Known) * factor(Month), data = disox)
diag(sandwich(indModel2))
sqrt(diag(sandwich(indModel2)))


summary(model14)

####################################################################
# Longitudinally Sampled Variable 3 - Conductivity #

conductivity <- hellb[c(1:3, 6)]
attach(conductivity)
conductivity_wide <- spread(conductivity, key=Month, value=Conductivity)

# Descriptive statistics for the two groups 
describeBy(conductivity_wide, conductivity_wide$Known)

# Plot the observed trajectories for the individual streams in each group
conductivity_wide_k <- conductivity_wide[which(conductivity_wide$Known == 1),3:12]
conductivity_wide_u <- conductivity_wide[which(conductivity_wide$Known == 0),3:12]

matplot(c(6,6.5,7,7.6,8,8.5,9,9.5,10,10.5), t(conductivity_wide_k), type='b', pch=20, lty=1, main= "Conductivity by Bimonth (Known)", col='gray', xlab =
          "Bimonth", ylab = "Conductivity")
matplot(c(6,6.5,7,7.6,8,8.5,9,9.5,10,10.5), t(conductivity_wide_u), type='b', pch=20, lty=1, main= "Conductivity by Bimonth (Unknown)", col='gray', xlab =
          "Bimonth", ylab = "Conductivity")

# PLot the mean response profiles for Current for each respective group (With Hellbender population and without)
plot(c(6,6.5,7,7.5, 8, 8.5, 9, 9.5, 10, 10.5), apply(conductivity_wide_k, 2, mean),  ylim = c(30,175), xlab = 'Time (bimonthly)', ylab = 'Mean Conductivity', main = "Conductivity response per stream", type = 'n')
abline(v = axTicks(1), h = axTicks(2), col = rgb(0.75, 0.75, 0.75, alpha = 0.5), lty = 3)
lines(c(6,6.5,7,7.5, 8, 8.5, 9, 9.5, 10, 10.5), apply(conductivity_wide_k, 2, mean), type = 'b', pch = 1, lty = 2)
lines(c(6,6.5,7,7.5, 8, 8.5, 9, 9.5, 10, 10.5), apply(conductivity_wide_u, 2, mean), type = 'b', pch = 16)
legend('bottomleft', legend = c('Known', 'Unknown'), lty =
         c(2,1), pch = c(1,16), cex = 0.5)

# Under certain conditions, we can look at the raw matrix to give me a sense of where to start with making assumptions about the covariance. 
cov(conductivity_wide[,c(3:12)])
cor(conductivity_wide[,c(3:12)])

#### Assessing Covariance Structures ####
# Setting up the data 
attach(conductivity)

conductivity$occur <- rep(1:10, length(unique(conductivity$ID)))

head(conductivity)

conductivity <- conductivity[order(conductivity$ID),]

# Assessing the compound symmetry structure 
#Homogeneous
model25 <- gls(Conductivity ~ factor(Known) * factor(Month), data = conductivity, correlation = corCompSymm(form = ~ occur | ID))
summary(model25)

getVarCov(model25)
cov2cor(getVarCov(model25)) 

# Heterogeneous
model26 <- gls(Conductivity ~ factor(Known) * factor(Month), data = conductivity, correlation = corCompSymm(form = ~ occur | ID), weights = varIdent(form = ~ 1 | occur))
summary(model26)

getVarCov(model26)
cov2cor(getVarCov(model26)) 

anova(model25, model26)
# In this case it looks like, based on the p-value of 0.4967, we fail to reject the null and would conclude that homogeneous is a better fit here (Smaller df) (Model9). 

# Toeplitz 

# Homogeneous 
model27 <- gls(Conductivity ~ factor(Known)*factor(Month), data = conductivity, correlation = corARMA(form = ~ 1 | ID, p = 3, q = 0))
summary(model27)

getVarCov(model27)
cov2cor(getVarCov(model27)) 

# Heterogeneous 
model28 <- gls(Conductivity ~ factor(Known)*factor(Month), data = conductivity, correlation = corARMA(form = ~ 1 | ID, p = 3, q = 0),
              weights = varIdent( form = ~ 1 | occur))
summary(model28)

getVarCov(model28)
cov2cor(getVarCov(model28)) 

anova(model27, model28)
# Given the p-value of 0.0028 and the lower AIC for model28, we conclude that the heterogeneous Toeplitz model is better here. 

# Checking out AR1 Structure
# Homogeneous 
model29 <- gls(Conductivity ~ factor(Known)*factor(Month), data = conductivity, correlation = corAR1(form = ~ occur | ID))
summary(model29)

getVarCov(model29)
cov2cor(getVarCov(model29)) 

# Heterogeneous 
model30 <- gls(Conductivity ~ factor(Known)*factor(Month), data = conductivity, correlation = corAR1(form = ~ occur | ID), weights = varIdent(form = ~ 1 | occur))
summary(model30)

getVarCov(model30)
cov2cor(getVarCov(model30)) 

anova(model29, model30)
# In this case, based on the p-value of 0.0026 and the lower AIC on model30, we can reject the null and would conclude that heterogeneous is a better fit here (Model30). 

# Homogeneous Compound and Heterogeneous Toeplitz
anova(model25, model28)
# Based on the p-value of .013 and the lower AIC on Model28, we'd conclude that the heterogeneous Toeplitz is a better fit. 

# Homogeneous Compound and Heterogeneous AR1 - AIC/BIC
anova(model25, model30)
# In looking at the p-value of 0.3121 and the lower df on model25, we'd conclude that compound symmetric is best here. 

# Heterogeneous Toeplitz and Heterogeneous AR1
anova(model28, model30)
# Based on the p-value of 0.0012 and the lower AIC on model28, we'd go with the heterogeneous toeplitz model here. 
 

###### Assessing whether the parametric model with time as linear might be appropriate ########
model31 <- gls(Conductivity ~ factor(Known)*factor(Month), data = conductivity, correlation = corARMA(form = ~ 1 | ID, p = 3, q = 0),
               weights = varIdent( form = ~ 1 | occur), method = 'ML')

model32 <- gls(Conductivity ~ factor(Known) + Month, data = conductivity, correlation = corARMA(form = ~ 1 | ID, p = 3, q = 0),
               weights = varIdent( form = ~ 1 | occur), method = 'ML')

anova(model31, model32)

# While the p-value of .0084 allows us to reject the null and conclude that the two models are statistically different, given that the AICs are virtually similar, we are going to choode the the non-parametric model in order to better assess the potential effect of time on how the treatment affects conductivity. 

###### Assessing quadratics #######
conductivity$Month2 <- conductivity$Month^2

model33 <- gls(Conductivity ~ Known*Month + Known*Month2 - Known, 
              data=conductivity, correlation = corARMA(form = ~ 1 | ID, p = 3, q = 0), weights = varIdent( form = ~ 1 | occur), method = 'ML')


anova(model31, model33)
# The p-value demonstrates statistical significance and the AIC is slightly lower for the non-quadratic. 

##### Our tests for coicidence and parallel are below. #####

# Test the null that the mean response profiles of the two groups are idential. (Slide 12: Coincidence deals with group)
# Test the null that the mean response profiles of the two groups are flat (Slide 12: Coincidence deals with time)
# Test the null hypothesis that the pattern of means over bimonthly measurement intervals are parallel for the two groups. (Slide 12: Parallel deals with group * time)
# Coincidence
model34 <- gls(Conductivity ~ factor(Month), data = conductivity, correlation = corARMA(form = ~ 1 | ID, p = 3, q = 0),
               weights = varIdent( form = ~ 1 | occur), method = 'ML')

anova(model34, model31)
# Based on the p-value of .2036 and the lower df for model34, we conclude that there is coincidence. 

# Flat
model35 <- gls(Conductivity ~ factor(Known), data = conductivity, correlation = corARMA(form = ~ 1 | ID, p = 3, q = 0),
               weights = varIdent( form = ~ 1 | occur), method = 'ML')

anova(model35, model31)
# As evident by the p-value of .0123 and the lower AIC for Model35, we can conclude that there is flatness. 

# Parallel
model36 <- gls(Conductivity ~ factor(Known)+factor(Month), data = conductivity, correlation = corARMA(form = ~ 1 | ID, p = 3, q = 0),
               weights = varIdent( form = ~ 1 | occur), method = 'ML')

anova(model36, model31)
# As evident by the p-value of 0.2619 and the lower AIC for model36, we can also conclude parallel. 

## Conclusion of the output for our selected model ##
indModel3 <- lm(Conductivity ~ factor(Known) * factor(Month), data = conductivity)
diag(sandwich(indModel3))
sqrt(diag(sandwich(indModel3)))

summary(model28)

install.packages("stargazer", repos = "http://cran.us.r-project.org")
library(stargazer)
stargazer(model4, model14, model28, type="text",title="Analysis of Mean Response Profile Results", digits=4,style="all", out="Model14dtdfg.txt")


