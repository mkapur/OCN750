####Load Required Packages
library(stats)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
#install.packages("car")
library(car)
install.packages("effects")
library(effects)
#install.packages("gdata")
library(gdata)
library(ggplot2)
#install.packages("lmtest")
library(lmtest)

#Set Working Directory
setwd("~/Dropbox/2015 Fall/OCN 750")

#### 1. From the “Lab Cultures” worksheet, plot the mean length (Length..mm.)
#### by Sex and Region. Plot error bars for +/- 1 standard error of the mean. There are
#### many ways to do this; some options for summarizing data in a dataframe include
#### using tapply(), or using ddply() and summarize() from the “plyr” package. To plot
#### the means and error bars you can combine plot() and segments(), or google one of
#### many functions people have written to do it.

#Load appropriate CSV
labcult = read.csv("lab_cultures.csv")

####Create Boxplot
boxplot(labcult$Length..mm. ~ labcult$Sex*labcult$Region,
         main = "Mean Isopod Length by Sex and Region",
         ylab = "mean length (mm)")
  
#### 2. Use lm(), and F-tests on the model, to test whether there is a significant
#### difference between Sexes in mean length, whether there is a significant difference
#### between Regions in mean length, and whether the effect of Region differs between
#### Sexes. Report F-statistics and degrees of freedom. How do you interpret these
#### results?

#Generate a linear model for variables of interest, testing for an interaction between the Sex and Region Factors
lsr.noint = lm(labcult$Length..mm. ~ labcult$Region)

lsr.interaction = lm(Length..mm. ~ Sex*Region, data = labcult)

culteff = effect("Sex*Region", lsr.interaction)
####Run ANOVA test using car package to examine significant differences
Anova(lsr.interaction)     
####See F-statistics and Degrees of Freedom in above output.
####It appears there is a significant difference in between sexes and regions in mean length, 
####but a non-significant difference of Region's effect between sexes. Upon visual inspection, this
####appears to make sense; both Males and Females exhibit higher mean lengths in the North than the South.


#### 3 Plot the model-fitted group means and standard errors.
plot(culteff)

#having trouble with "cannot find object" error

#### 4: Using the “Egg Data” worksheet, plot Number of Eggs vs. Length, and
#### color code the plotted points by Region.

eggs = read.csv("eggs.csv")

####Make new column and designate south/north regions for all populations
eggs$Region = NA
eggs$Region[1:50] = "NORTH"
eggs$Region[51:200] = "NORTH"
eggs$Region[51:150] = "SOUTH"
    
#####Subset individual populations
nahant = subset(eggs, eggs$Population == "Nahant")
Magnolia = subset(eggs, eggs$Population == "Magnolia")
north = rbind(nahant, Magnolia)

VIMS = subset(eggs, eggs$Population == "VIMS")
CCVA = subset(eggs, eggs$Population == "CCVA")
south = rbind(VIMS, CCVA)

#####Plot Regionally-Colored data
ggplot(data=eggs, aes(x=Length..mm., color = Region)) +
  geom_point(data = nahant, aes(y = Number.of.Eggs, color = "North")) +
  geom_point(data = CCVA, aes(y = Number.of.Eggs, color = "South")) +
  geom_point(data = VIMS, aes(y = Number.of.Eggs, color = "South")) +
  geom_point(data = Magnolia, aes(y = Number.of.Eggs, color = "North")) +
  ylab("Number of Eggs") +
  xlab("Length (mm)") +
  ggtitle("Number of Eggs vs Length in Two Regions")

#### 5. Fit a linear model to test whether the relationship between Number of
####Eggs and Length differs between Regions. Perform and report the appropriate F-test
####to test this question. How might you interpret these results?

#Create a linear model that incorporates both the number of eggs and length for each region
lmr = lm(Number.of.Eggs ~ Length..mm.*Region, data = eggs) #All Regions
lmn = lm(north$Number.of.Eggs ~ north$Length..mm.) #Just the North
lms = lm(south$Number.of.Eggs ~ south$Length..mm.) #Just the South

Anova(lmr)

####6. For #5 you have essentially fit two linear regressions, one for each Region.
####Take your plot from #4 and add these two lines. abline() will help.

ggplot(data=eggs, aes(x=Length..mm., color = Region, group = 1)) +
  geom_point(data = nahant, aes(y = Number.of.Eggs, color = "North")) +
  geom_point(data = CCVA, aes(y = Number.of.Eggs, color = "South")) +
  geom_point(data = VIMS, aes(y = Number.of.Eggs, color = "South")) +
  geom_point(data = Magnolia, aes(y = Number.of.Eggs, color = "North")) +
  geom_abline(intercept = coef(lmn)[1], slope = coef(lmn)[2], col = "coral", size = 1) +
  geom_abline(intercept = coef(lms)[1], slope = coef(lms)[2], col = "turquoise", size = 1)
  scale_y_discrete("Number of Eggs") +
  scale_x_continuous("Length (mm)") +
  ggtitle("Number of Eggs vs Length in Two Regions")


#### 7. Model diagnostics: for the model you fit in #5, make some plots that
####explore whether the residuals of the model are normally distributed, whether the
####variance of the residuals increases as Length increases, and whether the variance of
####the residuals varies between regions.

#Generate residuals and variance and add to data frame
#as.data.frame(lmr.residual)
#var.lm(lmr)
#vared = var(resid(lmr))
#lenvar = var(eggs$Length..m)
#as.data.frame(lenva)
)

#Plot to inspect distribution of residuals
qqPlot(lmr, col = "blue")
abline(0,0)

####The residuals themselves appear normally distributed, with deviation from normality at the tails of the distribution.

#Variance of Residuals vs Length
#Calculate model residuals & add to data frame
lmr.residual = resid(lmr)
as.data.frame(lmr.residual)
eggs = cbind(eggs, lmr.residual)

#Plot the Length variable against the residuals
plot(eggs$Length..mm., eggs$lmr.residual, col = "coral", xlab = "Length (mm)", ylab = "Residuals", main = "Residuals vs. Length")
abline(0,0)
####The above plot suggests that variance in residuals increases with length

#Variance of Residuals between Regions
plot(eggs$Number, eggs$lmr.residual)

boxplot(eggs$lmr.residual ~ eggs$Region,
        main = "Variance of Residuals between Regions",
        ylab = "residuals",
        xlab = "region")

ggplot(data = eggs, aes(x = Region, y = lmr.residual) +
     geom_bar(stat = "identity"))


eggs$Length..mm., eggs$lmr.residual, col = "coral", xlab = "Length (mm)", ylab = "Residuals", main = "Residuals vs. Length")
abline(0,0)
