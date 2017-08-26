## HW3 Reboot
setwd("G://")
chan = read.csv("channel_islands.csv")
library(plyr)
library(ggplot2)
library(dplyr)

## Slow & Steady Version. Similar to Kyle's.
## I would NOT recommend looping through this whole subsetting process.
#subset for species of interest and counts greater than 0

sub1 <- subset(chan, subset = SpeciesName %in% c("Embiotoca jacksoni, adult", "Paralabrax clathratus, adult") & count > 0)

## Aggregate new dataframe to see how many of each species were counted at each site in a given year. If that species wasn't seen at that site that year (e.g. EMJA in site 1 in 1987), it won't appear in this datafram
#To take mean of sites with >2 counts in a given year, we simply aggregate the data for each site in a given year and take the mean. Sites with only one count in a given year, e.g. site 8 in 1985, will return that value divided by one. We now have a single row for each site during each year, with two different data frames (one for each species)
sub2 = aggregate(count ~ SpeciesName * Year * Site, data = sub1, mean)

## Create a data frame that says how often a survey occurs at that site for that year.
## Counts of 1 indicate that only one spp. has data for that site for that year, 
## so we only want to keep those with a frequency of 2. 

## dont confuse the 'count' function with the 'count' column. the function just returns
## how many times that record (site + year) occur in the data frame.
library(plyr)
sitecounts <- count(sub2, c(Year, Site))


#this vector records years and sites for which there were >2 records (the species were co-ocurring).
sitecounts <- subset(sitecounts, freq == 2)

#Now we need a vector of years in which the species co-occur 
## (meaning there are at least 6 sites in that year for the dataframe we just made)

#Create vector of every year included in the dataset, for testing purposes
yrs = seq(1985,2011,1)

#an empty vector, to fill with values
cooc.years = vector()

# a for loop to check co-occurence and create a vector
for (k in yrs) { ## loops through each year in yrs
  #check if the length of the co-ocurrance vector is >= 6 (e.g. they co-occured at least 6 times)
  if (nrow(sitecounts[sitecounts$Year == k,]) >= 6) {
    cooc.years = rbind(cooc.years, k) ## make a vector of the years which meet our 
  }
}
unique(cooc.years)

#Looks like they have overlap through year 2004.

# take out the co-occuring years from the data frame
sub3 <- sub2[sub2$Year %in% cooc.years,]
finaldf = subset(meanfish, Year %in% sixsites)

## take means across all sites, leaving behind the year and species distinctions
meancount <- aggregate(count ~ Year * SpeciesName *Site, data = sub3, FUN = mean)

## a for-loop that plots count ratios for each species
length(cooc.years) ## 20 years
par(mfrow = c(4,5)) ## set up your matrix

## a for loop plot for sites at each year
## you should add in a legend or brief description saying that the red points are PACL, and black are EMJA
for (k in cooc.years) {
  d = subset(meancount, meancount$Year == k & meancount$SpeciesName == "Paralabrax clathratus, adult")
  a = subset(meancount, meancount$Year == k & meancount$SpeciesName == "Embiotoca jacksoni, adult")
  plot(d$count, d$Site, pch = 19, col = 'red', xlab = 'site', ylab = 'count') ## plot point(s) for the first species
  points(a$count, a$Site, pch = 19) ## plot point(s) for the second species
}

## a much shorter for loop that does the same thing
for(k in cooc.years){
with(subset(meancount, Year == k), plot(count, Site, col = factor(SpeciesName))
}

## a ggplot approach
ggplot(data = meancount, aes(x = Site, y = count, colour = SpeciesName)) +
  geom_point()+
  theme_bw() +
  ylim(0,20) +
  facet_wrap(~Year)

## Haxxor approach
chan %>% 
subset(SpeciesName %in% c("Embiotoca jacksoni, adult", "Paralabrax clathratus, adult") & count > 0) %>%
  aggregate(count ~ SpeciesName * Year * Site, FUN = mean) %>%
  
## Run a similar for-loop to calculate spearman-rank correlations for each year.
## The spearman function is part of the Hmisc package.

library(Hmisc)
par(mfrow = c(1,1))
speran = NULL
for (k in 1985:2004){
  m = subset(sub3, sub3$Year == k)
  output = spearman(m$SpeciesName, m$count)
  speran = rbind(speran, output)
}

speran = as.data.frame(speran)
head(speran)
hist(as.numeric(speran$rho), breaks = 10, main = "Frequency of Spearman-Rank Coefficients", col = "grey", xlab = "rho")

#By visual inspection, it appears that there is a slight trend favoring abundance of E. Jacksoni. 
## The rho values appear bi-modally distributed on either site of zero,.
## This may account for the early years in which P. calthanthus was more abundant (negative correlation).
## This trend reversed around 1992. 
