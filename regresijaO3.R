
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
data <- prepare_attributes_O3_regression(data)

learn <- data[1:1734,]
test <- data[1735:nrow(data),]

observed <- test$O3

sort(attrEval(O3 ~ ., learn, "RReliefFexpRank"), decreasing = TRUE)

####################### Linear model ####################### 
lm.model <- lm(O3 ~ ., data = learn)
predicted <- predict(lm.model, test)
mse(observed, predicted)
rmse(observed, predicted, mean(learn$O3))
mae(observed, predicted)
rmae(observed, predicted, mean(learn$O3))

####################### Regression tree ####################### 
rt.core <- CoreModel(O3 ~ ., data=learn, model="regTree", modelTypeReg = 6, selectionEstimatorReg="RReliefFexpRank")
predicted <- predict(rt.core, test)
mse(observed, predicted)
rmse(observed, predicted, mean(learn$O3))
mae(observed, predicted)
rmae(observed, predicted, mean(learn$O3))

###################### RF #######################
rf.model <- randomForest(O3 ~ ., learn, selectionEstimatorReg="MSEofMean")
predicted <- predict(rf.model, test)
mse(observed, predicted)
rmse(observed, predicted, mean(learn$O3))
mae(observed, predicted)
rmae(observed, predicted, mean(learn$O3))

###################### KNN5 #######################
knn.model <- kknn(O3 ~ ., learn, test, k = 5)
predicted <- fitted(knn.model)
mse(observed, predicted)
rmse(observed, predicted, mean(learn$O3))
mae(observed, predicted)
rmae(observed, predicted, mean(learn$O3))

###################### KNN10 #######################
knn.model <- kknn(O3 ~ ., learn, test, k = 10)
predicted <- fitted(knn.model)
mse(observed, predicted)
rmse(observed, predicted, mean(learn$O3))
mae(observed, predicted)
rmae(observed, predicted, mean(learn$O3))

###################### KNN15 #######################
knn.model <- kknn(O3 ~ ., learn, test, k = 15)
predicted <- fitted(knn.model)
mse(observed, predicted)
rmse(observed, predicted, mean(learn$O3))
mae(observed, predicted)
rmae(observed, predicted, mean(learn$O3))

###################### SVM #######################
svm.model <- svm(O3 ~ ., learn)
predicted <- predict(svm.model, test)
mse(observed, predicted)
rmse(observed, predicted, mean(learn$O3))
mae(observed, predicted)
rmae(observed, predicted, mean(learn$O3))

