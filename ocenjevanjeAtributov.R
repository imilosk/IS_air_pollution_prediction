
library(CORElearn)
library(ipred)
library(timeDate)

source("pomozneFunkcije.R")

data = read.table(file="podatkiSem1.txt", sep=",", header=TRUE)

####################### Remove glob_sevanje_min because contains only 0s ####################### 
data$Glob_sevanje_min <- NULL

####################### Convert date to season ####################### 
data$Datum <- getSeason(data$Datum)
data$Datum <- as.factor(data$Datum)

####################### PM rate ####################### 
pmRate <- vector() 
pmRate[data$PM10 <= 35.0] <- "NIZKA"
pmRate[data$PM10 > 35.0] <- "VISOKA"

####################### O3 rate ####################### 
o3Rate = vector()
o3Rate[data$O3 < 60.0] <- "NIZKA"
o3Rate[data$O3 >= 60 & data$O3 < 120] <- "SREDNJA"
o3Rate[data$O3 >= 120 & data$O3 < 180] <- "VISOKA"
o3Rate[data$O3 >= 180] <- "EKSTREMNA"

#######################  Comment PM rate if evaluating O3 and vice verca ####################### 
data$pmRate <- as.factor(pmRate)
#data$o3Rate <- as.factor(o3Rate)
data$PM10 <- NULL
#data$O3 <- NULL

####################### ReliefF ####################### 
sort(attrEval(pmRate ~ ., data, "ReliefFequalK"), decreasing = TRUE)

