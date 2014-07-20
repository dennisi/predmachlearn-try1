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

# plot pairs
featurePlot(x=training[,c("Cement","BlastFurnaceSlag","FlyAsh")],
            y = training$CompressiveStrength,
            plot="pairs")

featurePlot(x=training[,-9],
            y = training$CompressiveStrength,
            plot="pairs")

#todo: make plot of the outcome (CompressiveStrength) versus the index of the samples
#      Color by each of the variables in the data set 
#      (you may find the cut2() function in the Hmisc package useful for turning continuous covariates into factors). 
#      What do you notice in these plots?
library("Hmisc")

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

# "just for fun" exploratory plot with smoothers

cutVar <- cut2(training$FlyAsh,g=5)
qq <- qplot(CompressiveStrength,Cement,colour=cutVar,data=training)
qq +  geom_smooth(method='lm',formula=y~x)

# ...with boxplots
p1 <- qplot(cutVar, CompressiveStrength, data=training, fill=cutVar,
            geom=c("boxplot"))
p1

# ... boxplot with points overlayed
library(gridExtra)
p2 <- qplot(cutVar, CompressiveStrength, data=training, fill=cutVar,
            geom=c("boxplot","jitter"), alpha=1/4)
grid.arrange(p1,p2,ncol=2)

table(training$Age,cutVar) 

# density and histogram plots
qplot( training$CompressiveStrength, colour=cut2(training$FlyAsh,g=5), geom="density")
qplot( training$CompressiveStrength, geom="density")
hist(training$FlyAsh, breaks = 20)
hist(log(training$FlyAsh), breaks = 20)
hist(training$CompressiveStrength, breaks = 20)
#hist(log(training$CompressiveStrength), breaks = 20)

#---------------------------
## Questions 4,5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing =  adData[-inTrain,]

# Find all the predictor variables in the training set that begin with IL. 
names(training[grepl('^IL',names(training))])

# Perform principal components on these variables with the preProcess() function from the caret package. 
# Calculate the number of principal components needed to capture 80% of the variance. How many are there?
# M <- abs(cor(training[,-58]))
            trainingIM <- training[grepl('^IL',names(training))]
            M <- abs(cor(trainingIM))
            diag(M) <- 0
            which(M > 0.7,arr.ind=T)
            # names(training[grepl('^IL',names(training))])[c(3,6)]
            plot(training$IL_16,training$IL_3)
            plot(trainingIM[,c(3,6)])
            smallIM <- trainingIM[,c(3,6)]
            prComp <- prcomp(smallIM)
            plot(prComp$x[,1],prComp$x[,2])
            prComp$rotation
            typeColor <- ((trainingIMd$diagnosis=="Impaired")*1 + 1)#((training$diagnosis=="Impaired")*1 + 1)
            prComp  <- prcomp(log10(trainingIMd[,-1]+1))
            prComp <- prcomp(log10(spam[,-58]+1))

# Try to repead the above using preProcess() functions

#--------------------------