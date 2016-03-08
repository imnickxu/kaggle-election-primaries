setwd("C:/Users/MrBear/Desktop/Kaggle/2016_presidential_election")

library(ISLR)
library(leaps)
library(car)
library(zoo)
library(plyr)
library(dplyr)
library(broom)
library(ggplot2)
library(forecast)
library(randomForest)
library(plotly)
library(reshape)
library(gridExtra)
library(tree)

data <- read.csv("C:/Users/MrBear/Desktop/Kaggle/2016_presidential_election/combined_table.csv", header= T)

democrat <- data[data$party %in% c('Democrat'), ]

##drop candidates with 0% of the vote
democrat <- subset(democrat, candidate == "Hillary Clinton" | candidate == "Bernie Sanders")

#relevant candidates
relevant <- subset(data, candidate == "Hillary Clinton" | candidate == "Bernie Sanders" | candidate == "Donald Trump" | candidate == "Ted Cruz")

#clean up names and trim dataframe
relevant <- rename(relevant, c(Persons.under.18.years..percent..2014 = "under18",
                                      Persons.65.years.and.over..percent..2014 = "over65",
                                      Black.or.African.American.alone..percent..2014 = "black",
                                      Asian.alone..percent..2014 = "asian",
                                      Hispanic.or.Latino..percent..2014 = "latino",
                                      White.alone..not.Hispanic.or.Latino..percent..2014 = "white",
                                      Foreign.born.persons..percent..2009.2013 = "foreign",
                                      Bachelor.s.degree.or.higher..percent.of.persons.age.25...2009.2013 = "college",
                                      Veterans..2009.2013 = "veterans",
                                      Homeownership.rate..2009.2013 = "homeowners",
                                      Median.household.income..2009.2013 = "median_income",
                                      Persons.below.poverty.level..percent..2009.2013 = "poverty",
                                      Population.per.square.mile..2010 = "pop_density"
                                      ))

#normalize variables
relevant$median_income <- relevant$median_income/1000
relevant$fraction_votes <- relevant$fraction_votes*100

#isolate candidate data
Hillary <- subset(relevant, candidate == "Hillary Clinton")
Bernie <- subset(relevant, candidate == "Bernie Sanders")
trump <- subset(relevant, candidate == "Donald Trump")
cruz <- subset(relevant, candidate == "Ted Cruz")


#plot Hillary
g1 <- qplot(x = median_income, y = fraction_votes, data = Hillary, ylab = "fraction of votes") +
  geom_smooth(method='lm',formula=y~x)

g2 <- qplot(x = college, y = fraction_votes, data = Hillary, ylab = "fraction of votes") +
  geom_smooth(method='lm',formula=y~x)

g3 <- qplot(x = white, y = fraction_votes, data = Hillary, ylab = "fraction of votes") +
  geom_smooth(method='lm',formula=y~x)

g4 <- qplot(x = under18, y = fraction_votes, data = Hillary, ylab = "fraction of votes") +
  geom_smooth(method='lm',formula=y~x)

grid.arrange(g1, g2, g3, g4, nrow =2, ncol = 2)


#plot income
ggplot(democrat, aes(x=median_income, y=college, size=votes)) + geom_point(aes(color = candidate), alpha = 0.5)

#randomforest on Hillary
set.seed(131)
rfmodel_hillary <- randomForest(fraction_votes ~ median_income + under18 + over65 + black + asian + latino + white + foreign + college + veterans + homeowners + pop_density, data = Hillary, importance=TRUE, ntree=100)
plot(rfmodel_hillary)
varImpPlot(rfmodel_hillary)

#randomforest on Bernie
set.seed(131)
rfmodel_bernie <- randomForest(fraction_votes ~ median_income + under18 + over65 + black + asian + latino + white + foreign + college + veterans + homeowners + pop_density, data = Bernie, importance=TRUE, ntree=100)
plot(rfmodel_bernie)
varImpPlot(rfmodel_bernie)


###classification of Winners needs a separate data set

#re-level candidates
levels(democrat$candidate)
democrat$candidate <- factor(democrat$candidate)

#classification RF
set.seed(131)
rfmodelclass <- randomForest(candidate ~ median_income + under18 + over65 + black + asian + latino + white + foreign + college + veterans + homeowners + pop_density, data = democrat, importance=TRUE, ntree=5)
plot(rfmodelclass)
varImpPlot(rfmodelclass)

#tree
tree <- tree(candidate ~ median_income + under18 + over65 + black + asian + latino + white + foreign + college + veterans + homeowners + pop_density, data=democrat)
plot(tree); text(tree)

cv <- cv.tree(tree, K=100)
plot(cv, pch=21, bg=8, type="p", cex=1.5)

cut <- prune.tree(tree, best=3)

plot(cut)
text(cut)
