library(dplyr)
library(class)
library(gmodels)
library(RWeka)
library(tidyverse)
library(kernlab)
library(randomForest)

setwd("C:\\Sudhanshu\\SU\\Semester 2\\707\\Assignment 7\\")
trainData <- read.csv("Kaggle-digit-train.csv")
sampleTrainData <- sample_n(trainData,18200)
str(sampleTrainData)

testData <- read.csv("Kaggle-digit-test.csv")
sampleTestData <- sample_n(testData,28000)
str(sampleTestData)

NN <- make_Weka_filter("weka/filters/unsupervised/attribute/NumericToNominal") 
sampleTrainDataNormalized <- NN(data=sampleTrainData, control= Weka_control(R="1-3"), na.action = NULL)
trainLabels <- sampleTrainDataNormalized %>% select(starts_with("label"))
sampleTrainDataNormalizedP <- sampleTrainDataNormalized %>% select(starts_with("pixel"))
str(sampleTrainDataNormalized)

sampleTestDataNormalized <- NN(data=sampleTestData, control= Weka_control(R="1,3"), na.action = NULL)
testLabels <- sampleTestDataNormalized %>% select(starts_with("label"))
sampleTestDataNormalizedP <- sampleTestDataNormalized %>% select(starts_with("pixel"))
str(sampleTestDataNormalized)

cl = trainLabels[,1]
predKNN <- knn(train = sampleTrainDataNormalizedP,test = sampleTestDataNormalizedP, cl, k = 10)
predKNN
x=testLabels[,1]
CrossTable(x, y=predKNN, prop.chisq = FALSE)

# random forest
model1 <- randomForest(label ~ ., data = sampleTrainDataNormalized, importance = TRUE)
model1

# Fine tuning parameters of Random Forest model
model2 <- randomForest(label ~ ., data = sampleTrainDataNormalized, ntree = 500, mtry = 12, importance = TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, sampleTrainDataNormalized, type = "class")
# Checking classification accuracy
table(predTrain, sampleTrainDataNormalized$label)  

# Predicting on Validation set
predValid <- predict(model2, sampleTestDataNormalized, type = "class")
table(predValid,sampleTestDataNormalized$label)

#SVM
#Building the SVM model.
svm.model <- ksvm(label~., data = sampleTrainData, kernel = "rbfdot", kpar="automatic", C=50, cross=50, prob.model=TRUE
                  , type = "C-svc")
svm.model

svm.Pred <- predict(svm.model,sampleTestData)

table(svm.Pred,sampleTestData$label)


# submit to Kaggle
# first use textwrangler to insert "?," to each row, change "?," in the first row to "label,"
# create test ids
testid = seq(1, 28000, by=1)
# apply model to all test data
svm.Pred <- predict(svm.model,testData)
newpred=cbind(testid, svm.Pred)
colnames(newpred)=c("ImageId", "Label")
write.csv(newpred, file="C:\\Sudhanshu\\SU\\Semester 2\\707\\Assignment 7\\Submission.csv", row.names=FALSE)
