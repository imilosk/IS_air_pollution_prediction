
setwd("/home/permint/Documents/r_projects")

source("pripraviPodatke.R")
source("pomozneFunkcije.R")

library(ipred)
library(CORElearn)

data <- read.table(file="podatkiSem1.txt", sep=",", header=TRUE)
data <- prepare_attributes_O3_regression(data)


# data$Datum <- NULL
# data$Hitrost_vetra_min <- NULL
# data$Sunki_vetra_min <- NULL
# data$Pritisk_min <- NULL
# data$Vlaga_min <- NULL
# data$Temperatura_lokacija_mean <- NULL
# data$DanVTednu <- NULL
# data$Padavine_mean<- NULL
# data$Padavine_sum <- NULL
# data$Sunki_vetra_mean <- NULL


dataNew         <- data.frame()
tempVector      <- vector()

days <- 2
n <- days - 1
for (i in days:nrow(data)){
  tempVector <- data[i, ]
  for (j in 1:n) {
    temp <- data[i - j,]
    temp <- unname(temp)
    tempVector <-  cbind(tempVector, temp)
  }
  dataNew         <- rbind(dataNew, tempVector)
}

my_names <- names(data)
my_names_final  <- my_names
for (i in 1:n){
  temp <- NULL
  temp <- paste(my_names, i, sep = '')
  #temp <- toString(temp)
  my_names_final <- c(my_names_final, temp)
}

colnames(dataNew) <- my_names_final

dataNew$O31 <- NULL

learn <- dataNew[1:1734,]
test <- dataNew[1735:nrow(dataNew),]
observed <- test$O3

svm.model <- svm(O3 ~ ., learn)
predicted <- predict(svm.model, test)
mse(observed, predicted)
rmse(observed, predicted, mean(learn$O3))
mae(observed, predicted)
rmae(observed, predicted, mean(learn$O3))



