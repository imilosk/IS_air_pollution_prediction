
prepare_attributes_O3 <- function(data){
  
  ####################### Remove glob_sevanje_min because contains only 0s ####################### 
  data$Glob_sevanje_min <- NULL
  
  ####################### Add week day attribute, 1 is weekday, 0 is weekend ######################  
  #weekday <- isWeekday(format(as.Date(data$Datum)), wday=1:5)
  #weekday <- as.numeric(weekday)
  #data$isWeekDay <- weekday
  data$DanVTednu <- weekdays(as.Date(data$Datum, "%Y-%m-%d"))
  DanVTednu = vector()
  DanVTednu[data$DanVTednu == "ponedeljek"] <- 1
  DanVTednu[data$DanVTednu == "torek"] <- 2
  DanVTednu[data$DanVTednu == "sreda"] <- 3
  DanVTednu[data$DanVTednu == "četrtek"] <- 4
  DanVTednu[data$DanVTednu == "petek"] <- 5
  DanVTednu[data$DanVTednu == "sobota"] <- 6
  DanVTednu[data$DanVTednu == "nedelja"] <- 7
  data$DanVTednu <- DanVTednu
  data$DanVTednu <- as.factor(data$DanVTednu)
  
  ####################### Convert date to season ####################### 
  data$Datum <- getSeason(data$Datum)
  data$Datum <- as.factor(data$Datum)
  
  ####################### O3 rate ####################### 
  o3Rate = vector()
  o3Rate[data$O3 < 60.0] <- "NIZKA"
  o3Rate[data$O3 >= 60 & data$O3 < 120] <- "SREDNJA"
  o3Rate[data$O3 >= 120 & data$O3 < 180] <- "VISOKA"
  o3Rate[data$O3 >= 180] <- "EKSTREMNA"
  
  ####################### ####################### 
  data$O3Rate <- as.factor(o3Rate)
  data$PM10 <- NULL
  data$O3 <- NULL
  data$Postaja <- NULL
  
  return(data)
}

prepare_attributes_O3_regression <- function(data){
  
  ####################### Remove glob_sevanje_min because contains only 0s ####################### 
  data$Glob_sevanje_min <- NULL
  
  ####################### Add week day attribute, 1 is weekday, 0 is weekend ######################  
  #weekday <- isWeekday(format(as.Date(data$Datum)), wday=1:5)
  #weekday <- as.numeric(weekday)
  #data$isWeekDay <- weekday
  data$DanVTednu <- weekdays(as.Date(data$Datum, "%Y-%m-%d"))
  DanVTednu = vector()
  DanVTednu[data$DanVTednu == "ponedeljek"] <- 1
  DanVTednu[data$DanVTednu == "torek"] <- 2
  DanVTednu[data$DanVTednu == "sreda"] <- 3
  DanVTednu[data$DanVTednu == "četrtek"] <- 4
  DanVTednu[data$DanVTednu == "petek"] <- 5
  DanVTednu[data$DanVTednu == "sobota"] <- 6
  DanVTednu[data$DanVTednu == "nedelja"] <- 7
  data$DanVTednu <- DanVTednu
  data$DanVTednu <- as.factor(data$DanVTednu)
  
  ####################### Convert date to season ####################### 
  data$Datum <- getSeason(data$Datum)
  data$Datum <- as.factor(data$Datum)
  
  ####################### ####################### 
  data$PM10 <- NULL
  data$Postaja <- NULL
  
  return(data)
}



prepare_attributes_PM10 <- function(data){
  
  
  ####################### Remove glob_sevanje_min because contains only 0s ####################### 
  data$Glob_sevanje_min <- NULL
  
  ####################### Add week day attribute, 1 is weekday, 0 is weekend ######################  
  #weekday <- isWeekday(format(as.Date(data$Datum)), wday=1:5)
  #weekday <- as.numeric(weekday)
  #data$isWeekDay <- weekday
  data$DanVTednu <- weekdays(as.Date(data$Datum, "%Y-%m-%d"))
  DanVTednu = vector()
  DanVTednu[data$DanVTednu == "ponedeljek"] <- 1
  DanVTednu[data$DanVTednu == "torek"] <- 2
  DanVTednu[data$DanVTednu == "sreda"] <- 3
  DanVTednu[data$DanVTednu == "četrtek"] <- 4
  DanVTednu[data$DanVTednu == "petek"] <- 5
  DanVTednu[data$DanVTednu == "sobota"] <- 6
  DanVTednu[data$DanVTednu == "nedelja"] <- 7
  data$DanVTednu <- DanVTednu
  data$DanVTednu <- as.factor(data$DanVTednu)
  
  ####################### Convert date to season ####################### 
  data$Datum <- getSeason(data$Datum)
  data$Datum <- as.factor(data$Datum)
  
  ####################### PM rate ####################### 
  pmRate <- vector() 
  pmRate[data$PM10 <= 35.0] <- "NIZKA"
  pmRate[data$PM10 > 35.0] <- "VISOKA"
  
  ####################### ####################### 
  data$PM10Rate <- as.factor(pmRate)
  data$PM10 <- NULL
  data$O3 <- NULL
  data$Postaja <- NULL
  
  return(data)
}


prepare_attributes_PM10_regression <- function(data){
  
  
  ####################### Remove glob_sevanje_min because contains only 0s ####################### 
  data$Glob_sevanje_min <- NULL
  
  ####################### Add week day attribute, 1 is weekday, 0 is weekend ######################  
  #weekday <- isWeekday(format(as.Date(data$Datum)), wday=1:5)
  #weekday <- as.numeric(weekday)
  #data$isWeekDay <- weekday
  data$DanVTednu <- weekdays(as.Date(data$Datum, "%Y-%m-%d"))
  DanVTednu = vector()
  DanVTednu[data$DanVTednu == "ponedeljek"] <- 1
  DanVTednu[data$DanVTednu == "torek"] <- 2
  DanVTednu[data$DanVTednu == "sreda"] <- 3
  DanVTednu[data$DanVTednu == "četrtek"] <- 4
  DanVTednu[data$DanVTednu == "petek"] <- 5
  DanVTednu[data$DanVTednu == "sobota"] <- 6
  DanVTednu[data$DanVTednu == "nedelja"] <- 7
  data$DanVTednu <- DanVTednu
  data$DanVTednu <- as.factor(data$DanVTednu)
  
  ####################### Convert date to season ####################### 
  data$Datum <- getSeason(data$Datum)
  data$Datum <- as.factor(data$Datum)
  
  ####################### ####################### 
  data$O3 <- NULL
  
  return(data)
}
