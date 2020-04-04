library(RWeka)
library(dplyr)

setwd("C:\\Sudhanshu\\SU\\Semester 2\\707\\Assignment 6\\")
trainData <- read.csv("Kaggle-digit-train.csv")
sampleTrainData <- sample_n(trainData,1400)
View(sampleTrainData)
str(sampleTrainData)

testData <- read.csv("Kaggle-digit-test.csv")
sampleTestData <- sample_n(testData,1000)
View(sampleTestData)
str(sampleTestData)

NN <- make_Weka_filter("weka/filters/unsupervised/attribute/NumericToNominal") 
sampleTrainData <- NN(data=sampleTrainData, control= Weka_control(R="1-3"), na.action = NULL)
sampleTestData <- NN(data=sampleTestData, control= Weka_control(R="1,3"), na.action = NULL)

treeModel=J48(label~., data = sampleTrainData)
treeModel=J48(label~., data = sampleTrainData, control=Weka_control(U=FALSE, M=2,C=0.5))

evaluationModel3 <- evaluate_Weka_classifier(treeModel,numFolds = 3,seed = 1, class = TRUE)
evaluationModel3

evaluationModel10 <- evaluate_Weka_classifier(treeModel,numFolds = 10,seed = 1, class = TRUE)
evaluationModel10

pred=predict (treeModel, newdata = sampleTestData, type = c("class"))
pred

NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
nb_model=NB(label~., data=sampleTrainData)

evaluationModelNB3 <- evaluate_Weka_classifier(nb_model, numFolds = 3, seed = 1, class = TRUE)
evaluationModelNB3

evaluationModelNB10 <- evaluate_Weka_classifier(nb_model, numFolds = 10, seed = 1, class = TRUE)
evaluationModelNB10

predNB = predict (nb_model, newdata = sampleTestData, type = c("class"))
predNB