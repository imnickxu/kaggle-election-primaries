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
library(caret)

data <- read.csv("C:/Users/MrBear/Desktop/Kaggle/2016_presidential_election/combined_table.csv", header= T)



##drop candidates with 0% of the vote
democrat <- subset(democrat, candidate == "Hillary Clinton" | candidate == "Bernie Sanders")

#relevant candidates
relevant <- subset(data, candidate == "Hillary Clinton" | candidate == "Bernie Sanders" | candidate == "Donald Trump" | candidate == "Ted Cruz")

#separate by party
democrat <- relevant[relevant$party %in% c('Democrat'), ]
republican <- relevant[relevant$party %in% c('Republican'), ]

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
relevant$winner <- as.factor(relevant$winner) 

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
democrat_winner <- subset(relevant, winner == 1 & party == "Democrat")
ggplot(democrat_winner, aes(x=college, y=median_income, size=votes)) + geom_point(aes(color = candidate), alpha = 0.5)

republican_winner <- subset(relevant, winner == 1 & party == "Republican")
ggplot(republican_winner, aes(x=college, y=median_income, size=votes)) + geom_point(aes(color = candidate), alpha = 0.5)

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


###classification of Winners needs a separate data set of just winners####

##DEMOCRATS###

democrat_winner <- subset(relevant, winner == 1 & party == "Democrat")

#re-level candidates
levels(democrat_winner$candidate)
democrat_winner$candidate <- factor(democrat_winner$candidate)


#democrat tree
tree <- tree(candidate ~ median_income + under18 + over65 + black + asian + latino + white + foreign + college + homeowners + pop_density, data=democrat_winner, mincut=1, mindev=0.005)
plot(tree); text(tree)

cv <- cv.tree(tree, K=100)
plot(cv, pch=21, bg=8, type="p", cex=1.5)

cut <- prune.tree(tree, best=4)

plot(cut)
text(cut)

#classification RF
set.seed(131)
rfmodelclass <- randomForest(candidate ~ median_income + under18 + over65 + black + asian + latino + white + foreign + college + homeowners + pop_density, data = democrat_winner, importance=TRUE, ntree=100)
plot(rfmodelclass)
varImpPlot(rfmodelclass)



##REPUBLICANS###

republican_winner <- subset(relevant, winner == 1 & party == "Republican")

#re-level candidates
levels(republican_winner$candidate)
republican_winner$candidate <- factor(republican_winner$candidate)


#republican tree
reptree <- tree(candidate ~ median_income + under18 + over65 + black + asian + latino + white + foreign + college + homeowners + pop_density, data=republican_winner, mincut=1, mindev=0.005)
plot(reptree); text(reptree)

cv <- cv.tree(reptree, K=100)
plot(cv, pch=21, bg=8, type="p", cex=1.5)

repcut <- prune.tree(reptree, best=4)

plot(repcut)
text(repcut)

#classification RF
set.seed(131)
rfmodelclass <- randomForest(candidate ~ median_income + under18 + over65 + black + asian + latino + white + foreign + college + homeowners + pop_density, data = republican_winner, importance=TRUE, ntree=150)
plot(rfmodelclass)
varImpPlot(rfmodelclass)


############Analyze the Trump#########

g1 <- qplot(x = median_income, y = fraction_votes, data = trump, ylab = "fraction of votes") +
  geom_smooth(method='lm',formula=y~x)

g2 <- qplot(x = college, y = fraction_votes, data = trumpy, ylab = "fraction of votes") +
  geom_smooth(method='lm',formula=y~x)

g3 <- qplot(x = white, y = fraction_votes, data = trump, ylab = "fraction of votes") +
  geom_smooth(method='lm',formula=y~x)

g4 <- qplot(x = over65, y = fraction_votes, data = trump, ylab = "fraction of votes") +
  geom_smooth(method='lm',formula=y~x)

grid.arrange(g1, g2, g3, g4, nrow =2, ncol = 2)




# splitdf function will return a list of training and testing sets
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

#apply the function
splits <- splitdf(trump, seed=808)
training <- splits$trainset
test <- splits$testset

#randomforest on Trump

rfmodel_trump <- randomForest(fraction_votes ~ median_income + under18 + over65 + black + asian + latino + white + foreign + college + homeowners + pop_density, data = training, importance=TRUE)
plot(rfmodel_trump)
varImpPlot(rfmodel_trump, type=1)

#predict using fitted model
predicted <- predict(rfmodel_trump, newdata=test[ ,-1])

#measure out of sample
actual <- test$fraction_votes
rsq <- 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2)
print(rsq)

#caret package
install.packages('e1071', dependencies=TRUE)
rf_model_caret<-train(winner ~ median_income + under18 + over65 + black + asian + latino + white + foreign + college + homeowners + pop_density, data=training , method="rf",
                trControl=trainControl(method="cv",number=5),
                prox=TRUE,allowParallel=TRUE)
print(rf_model_caret$finalModel)

#measure vs test
test$winner <- predict(rf_model_caret, newdata = test)

#impute missing values if needed
testSet$Fare <- ifelse(is.na(testSet$Fare), mean(testSet$Fare, na.rm = TRUE), testSet$Fare)

#check predictions
check <- test[,c("winner", "fips")]
write.table(check, file = "trumpcheck.csv", col.names = TRUE, row.names = FALSE, sep = ",")