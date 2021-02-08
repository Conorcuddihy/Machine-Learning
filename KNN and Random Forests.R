beer_training <- read.csv(file = "/Users/conorcuddihy/Desktop/beer_training_labelled.csv")
beer_training

beer_test <- read.csv(file = "/Users/conorcuddihy/Desktop/beer_test_labelled2.csv")
beer_test

install.packages("ISLR")
install.packages("caret")
install.packages("e1071")
library(lattice)
library(ggplot2)
library(ISLR)
library(caret)  
library(e1071)
 
# Manipulating Data Set 

View(beer_training)

drops <- c("beer_id","X")
beer_training[ , !(names(beer_training) %in% drops)] 

drops <- c("beer_id","X")
beer_test[ , !(names(beer_test) %in% drops)] 


# Training and Test Set

indxTrain <- createDataPartition(y = beer_training$style ,p = 0.75,list = FALSE)
training <- beer_training[indxTrain,]
testing <- beer_training[-indxTrain,]

#Checking distibution in origanl data and partitioned data

prop.table(table(training$style)) * 100

prop.table(table(training$style)) * 100

prop.table(table(beer_training$style)) * 100

###

trainX <- training[,names(training) != "style"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues

###

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = defaultSummary)
knnFit <- train(style ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

#Output of kNN fit
knnFit

plot(knnFit)

knnPredict <- predict(knnFit,newdata = testing )
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, testing$style )

mean(knnPredict == testing$style)

#Now verifying 2 class summary function

ctrl <- trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE,summaryFunction = defaultSummary)
knnFit <- train(style ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

knnFit

plot(knnFit, print.thres = 0.5, type="S")

knnPredict <- predict(knnFit,newdata = testing )
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, testing$style )

mean(knnPredict == testing$style)

##### Random Forest #####

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)

# Random forrest
rfFit <- train(style ~ ., data = training, method = "rf", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)


rfFit

plot(rfFit)

rfPredict <- predict(rfFit,newdata = testing )
confusionMatrix(rfPredict, testing$style )

mean(rfPredict == testing$style)


#With twoclasssummary
ctrl <- trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE,summaryFunction = defaultSummary)
# Random forrest
rfFit <- train(style ~ ., data = training, method = "rf", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)


rfFit 

plot(rfFit)

plot(rfFit, print.thres = 0.5, type="S")

rfPredict <- predict(rfFit,newdata = testing )
confusionMatrix(rfPredict, testing$style )

mean(rfPredict == testing$style)






