
setwd("/home/permint/Documents/r_projects")

source("pripraviPodatke.R")
source("pomozneFunkcije.R")

library(timeDate)
library(CORElearn)
library(ipred)
library(rpart)
library(kernlab)
library(nnet)
library(e1071)
library(randomForest)
library(adabag)
library(kknn)

data <- read.table(file="podatkiSem1.txt", sep=",", header=TRUE)
data <- data[order(data$Datum), ]
data <- prepare_attributes_PM10_regression(data)

learn <- data[1:1734,]
test <- data[1735:nrow(data),]

observed <- test$PM10

sort(attrEval(PM10 ~ ., learn, "RReliefFexpRank"), decreasing = TRUE)

####################### Linear model ####################### 
lm.model <- lm(PM10 ~ ., data = learn)
predicted <- predict(lm.model, test)
mse(observed, predicted)
rmse(observed, predicted, mean(learn$PM10))
mae(observed, predicted)
rmae(observed, predicted, mean(learn$PM10))

####################### Regression tree ####################### 
rt.core <- CoreModel(PM10 ~ ., data=learn, model="regTree", modelTypeReg = 6, selectionEstimatorReg="RReliefFexpRank")
predicted <- predict(rt.core, test)
mse(observed, predicted)
rmse(observed, predicted, mean(learn$PM10))
mae(observed, predicted)
rmae(observed, predicted, mean(learn$PM10))

###################### RF #######################
rf.model <- randomForest(PM10 ~ ., learn, selectionEstimatorReg="RReliefFexpRank")
predicted <- predict(rf.model, test)
mse(observed, predicted)
rmse(observed, predicted, mean(learn$PM10))
mae(observed, predicted)
rmae(observed, predicted, mean(learn$PM10))

###################### KNN5 #######################
knn.model <- kknn(PM10 ~ ., learn, test, k = 5)
predicted <- fitted(knn.model)
mse(observed, predicted)
rmse(observed, predicted, mean(learn$PM10))
mae(observed, predicted)
rmae(observed, predicted, mean(learn$PM10))

###################### KNN10 #######################
knn.model <- kknn(PM10 ~ ., learn, test, k = 10)
predicted <- fitted(knn.model)
mse(observed, predicted)
rmse(observed, predicted, mean(learn$PM10))
mae(observed, predicted)
rmae(observed, predicted, mean(learn$PM10))

###################### KNN15 #######################
knn.model <- kknn(PM10 ~ ., learn, test, k = 15)
predicted <- fitted(knn.model)
mse(observed, predicted)
rmse(observed, predicted, mean(learn$PM10))
mae(observed, predicted)
rmae(observed, predicted, mean(learn$PM10))

###################### SVM #######################
svm.model <- svm(PM10 ~ ., learn)
predicted <- predict(svm.model, test)
mse(observed, predicted)
rmse(observed, predicted, mean(learn$PM10))
mae(observed, predicted)
rmae(observed, predicted, mean(learn$PM10))

