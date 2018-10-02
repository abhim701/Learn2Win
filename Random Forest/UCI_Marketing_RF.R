data <- read.csv("G:/DataScience/practice_datasets/UCI_Bank_Marketing/bank-additional/bank-additional/bank-additional-full.csv",header = T,
                 sep = ';'
)

Y.freq <- table(data$y)
Y.freq

library(MASS)
library(randomForest )

data <- subset(data, select=-c(duration))

str(data)
dim(testDF)

set.seed(123)


library(caTools)
ind <- sample.split ( Y = data$y, SplitRatio  = 0.7)
trainDF <- data[ind,]
testDF <- data[!ind,]



library( randomForest )
modelRF  <-  randomForest(y ~ ., data = trainDF, mtry = 3, ntree = 20 )
modelRF

importance(modelRF)
varImp(modelRF)
varImpPlot(modelRF)


Predictions <- predict( modelRF , testDF, type = 'class')
t <- table(predictions = Predictions, actual = testDF$y )
t
sum(diag(t))/sum(t)




library(pROC)

predictionsWithProb <- predict( modelRF , testDF, type = 'prob')
predictionsWithProb
auc <- auc( testDF$y, predictionsWithProb[,2] )
auc

plot (roc(testDF$y, predictionsWithProb[,2]))



bestmtry <- tuneRF(trainDF, trainDF$y, ntreeTry   = 200, stepFactor = 1, improve = 0.01, trace = T, plot = T) 

bestmtry
plot(bestmtry)

TunedmodelRF  <-  randomForest(y ~ ., data = trainDF, mtry = 4, ntree = 200 )
TunedmodelRF
varImpPlot(TunedmodelRF)

Predictions <- predict( TunedmodelRF , testDF, type = 'class')

tmp <- predict(TunedmodelRF, testDF, "prob")
nrow(tmp)
t <- table(predictions = Predictions, actual = testDF$y )
t
sum(diag(t))/sum(t)

t[1,1]
t[1,2]
Sensitivity <-  419/(419 + 973)
Sensitivity
specificity <- 10718/(10718 + 246)
specificity

library(pROC)

predictionsWithProb <- predict( TunedmodelRF , testDF, type = 'prob')
predictionsWithProb
auc <- auc( testDF$y, predictionsWithProb[,2] )
auc
Area under the curve: 0.7878

plot (roc(testDF$y, predictionsWithProb[,2]))

# > sum(diag(t))/sum(t)
# [1] 0.9
# > predictionsWithProb <- predict( TunedmodelRF , testDF, type = 'prob')
# > auc <- auc( testDF$y, predictionsWithProb[,2] )
# > auc
# Area under the curve: 0.79
# > plot (roc(testDF$y, predictionsWithProb[,2]))
# install.packages("InformationValue") 
# library(InformationValue)

