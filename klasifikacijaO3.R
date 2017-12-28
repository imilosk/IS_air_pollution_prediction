
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

data <- read.table(file="podatkiSem1.txt", sep=",", header=TRUE)
data <- data[order(data$Datum), ]
data <- prepare_attributes_O3(data)

learn <- data[1:1734,]
test <- data[1735:nrow(data),]

observed <- test$O3Rate
obsMat <- model.matrix(~O3Rate - 1, test)
mypredict.generic <- function(object, newdata){predict(object, newdata, type = "class")}
mymodel.coremodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}
mypredict.coremodel <- function(object, newdata) {pred <- predict(object, newdata)$class; destroyModels(object); pred}

sort(attrEval(O3Rate ~ ., data, "ReliefFexpRank"), decreasing = TRUE)

####################### Majority class ####################### 
majority.class <- names(which.max(table(data$O3Rate)))
sum(data$O3Rate == majority.class) / length(data$O3Rate)

####################### DT #######################
dt <- CoreModel(O3Rate ~ ., learn, model="tree", selectionEstimator = "ReliefFequalK")
predicted <- predict(dt, test, type="class")
CA(observed, predicted)

predMat <- predict(dt, test, type = "prob")
brier.score(obsMat, predMat)

errorest(O3Rate~., data=learn, model = rpart, predict = mypredict.generic)

####################### NB ####################### 
cm.nb <- CoreModel(O3Rate ~ ., data = learn, model="bayes", selectionEstimator = "ReliefFequalK")
predicted <- predict(cm.nb, test, type="class")
CA(observed, predicted)

predMat <- predict(cm.nb, test, type = "probability")
brier.score(obsMat, predMat)

errorest(O3Rate~., data=learn, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="bayes")

####################### KNN k=5 ####################### 
cm.knn <- CoreModel(O3Rate ~ ., data = learn, model="knn", kInNN = 5)
predicted <- predict(cm.knn, test, type="class")
CA(observed, predicted)

predMat <- predict(cm.knn, test, type = "probability")
brier.score(obsMat, predMat)

errorest(O3Rate~., data=learn, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="knn")

####################### KNN k=10 ####################### 
cm.knn <- CoreModel(O3Rate ~ ., data = learn, model="knn", kInNN = 10)
predicted <- predict(cm.knn, test, type="class")
CA(observed, predicted)

predMat <- predict(cm.knn, test, type = "probability")
brier.score(obsMat, predMat)

errorest(O3Rate~., data=learn, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="knn")

####################### KNN k=15 ####################### 
cm.knn <- CoreModel(O3Rate ~ ., data = learn, model="knn", kInNN = 15)
predictedKNN15 <- predict(cm.knn, test, type="class")
caKNN15 <- CA(observed, predictedKNN15)
caKNN15

predKNN15 <- predict(cm.knn, test, type = "probability")
brier.score(obsMat, predKNN15)

errorest(O3Rate~., data=learn, model = mymodel.coremodel, predict = mypredict.coremodel, target.model="knn")

####################### RF ####################### 
cm.rf <- CoreModel(O3Rate ~ ., data = learn, model="rf", selectionEstimator="ReliefFequalK")
predictedRF <- predict(cm.rf, test, type="class")
caRF <- CA(observed, predictedRF)
caRF

predRF <- predict(cm.rf, test, type = "probability")
brier.score(obsMat, predRF)

mypredict.rf <- function(object, newdata){predict(object, newdata, type = "class")}
errorest(O3Rate~., data=learn, model = randomForest, predict = mypredict.generic)

####################### SVM ####################### 
model.svm <- ksvm(O3Rate ~ ., data = learn, kernel = "rbfdot", kpar=list(sigma=0.015))
predictedSVM <- predict(model.svm, test, type = "response")
caSVM <- CA(observed, predictedSVM)
caSVM

model.svm <- ksvm(O3Rate ~ ., data = learn, kernel = "rbfdot", prob.model = T)
predSVM <- predict(model.svm, test, type = "prob")
brier.score(obsMat, predSVM)

mypredict.ksvm <- function(object, newdata){predict(object, newdata, type = "response")}
errorest(O3Rate~., data=learn, model = ksvm, predict = mypredict.ksvm)

####################### Voting ####################### 
pred <- data.frame(predictedKNN15, predictedRF, predictedSVM)
predicted <- voting(pred)
CA(test$O3Rate, predicted)

####################### Weighted Voting ####################### 
predDT.prob <- predict(cm.knn, test, type="probability")
predNB.prob <- predict(cm.rf, test, type="probability")
predKNN.prob <- 1 - predict(model.svm, test, type="prob")

pred.prob <- caKNN15 * predDT.prob + caRF * predNB.prob + caSVM * predKNN.prob
pred.prob

highest <- apply(pred.prob, 1, which.max)
classes <- levels(learn$O3Rate)
predicted <- classes[highest]

CA(test$O3Rate, predicted)

####################### Bagging ####################### 
bag <- bagging(O3Rate ~ ., learn, nbagg=15)
bag.pred <- predict(bag, test, type="class")
CA(test$O3Rate, bag.pred)

####################### Boosting ####################### 
bm <- boosting(O3Rate ~ ., learn)
predictions <- predict(bm, test)
names(predictions)

predicted <- predictions$class
CA(test$O3Rate, predicted)

