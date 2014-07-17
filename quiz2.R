library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

# adData = data.frame(diagnosis,predictors)
# trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
# training = adData[trainIndex,]
# testing = adData[trainIndex,]##mistake, missing "-"

# adData = data.frame(diagnosis,predictors)
# train = createDataPartition(diagnosis, p = 0.50,list=FALSE)
# test = createDataPartition(diagnosis, p = 0.50,list=FALSE)

#maybe it's this one?

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

##-----------------
## Question 2

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
