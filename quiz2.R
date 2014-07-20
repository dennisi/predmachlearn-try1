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
## Questions 2,3

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

#todo: make plot of the outcome (CompressiveStrength) versus the index of the samples
#      Color by each of the variables in the data set 
#      (you may find the cut2() function in the Hmisc package useful for turning continuous covariates into factors). 
#      What do you notice in these plots?
qplot(row.names(training), training$CompressiveStrength, colour=cut2(training$Cement,g=5))
qplot(row.names(training), training$CompressiveStrength, colour=cut2(training$BlastFurnaceSlag,g=5))
qplot(row.names(training), training$CompressiveStrength, colour=cut2(training$FlyAsh,g=5))
qplot(row.names(training), training$CompressiveStrength, colour=cut2(training$Water,g=5))
qplot(row.names(training), training$CompressiveStrength, colour=cut2(training$Superplasticizer,g=5))
qplot(row.names(training), training$CompressiveStrength, colour=cut2(training$CoarseAggregate,g=5))
qplot(row.names(training), training$CompressiveStrength, colour=cut2(training$FineAggregate,g=5))
qplot(row.names(training), training$CompressiveStrength, colour=cut2(training$Age,g=5))
qplot(row.names(training), training$CompressiveStrength, colour=cut2(training$CompressiveStrength,g=5))

qplot(row.names(testing), testing$CompressiveStrength, colour=cut2(testing$Cement,g=5))
qplot(row.names(testing), testing$CompressiveStrength, colour=cut2(testing$BlastFurnaceSlag,g=5))
qplot(row.names(testing), testing$CompressiveStrength, colour=cut2(testing$FlyAsh,g=5))
qplot(row.names(testing), testing$CompressiveStrength, colour=cut2(testing$Water,g=5))
qplot(row.names(testing), testing$CompressiveStrength, colour=cut2(testing$Superplasticizer,g=5))
qplot(row.names(testing), testing$CompressiveStrength, colour=cut2(testing$CoarseAggregate,g=5))
qplot(row.names(testing), testing$CompressiveStrength, colour=cut2(testing$FineAggregate,g=5))
qplot(row.names(testing), testing$CompressiveStrength, colour=cut2(testing$Age,g=5))
qplot(row.names(testing), testing$CompressiveStrength, colour=cut2(testing$CompressiveStrength,g=5))

#---------------------------
## Questions 4,5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Find all the predictor variables in the training set that begin with IL. 

# Perform principal components on these variables with the preProcess() function from the caret package. 
# Calculate the number of principal components needed to capture 80% of the variance. How many are there?
