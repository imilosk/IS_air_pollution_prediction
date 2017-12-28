

library("corrplot")
library("plotly")

setwd("/home/permint/Documents/r_projects")

data = read.table(file="podatkiSem1.txt", sep=",", header=TRUE)

dataT <- subset(data, select = -c(Glob_sevanje_min, Postaja, Datum))

################## Draw plot01 ################## 
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(dataT),method="color",addCoef.col = "black",
    type="upper",cl.ratio = 0.2, tl.cex = 0.8, number.cex = 0.6,
    diag = FALSE, col = col(200))

################## Draw plot02 ################## 
dataDatum <- as.numeric(data$Datum)
p <- plot_ly(
  data, y = ~Glob_sevanje_mean, x = ~dataDatum,
  color = ~Glob_sevanje_mean, size = ~Glob_sevanje_mean
)
p


################## Draw plot03 ################## 
datumKoper <- data[data$Postaja=="Koper","Datum"]
O3Koper <-data[data$Postaja=="Koper","O3"]
datumKoper <-format(as.Date(datumKoper))
p <- plot_ly(
  data, y = ~O3, x = ~Temperatura_lokacija_mean,
  color = ~O3, size = ~O3
)
p

################## Draw plot04 ################## 
p <- plot_ly(
  data, y = ~PM10, x = ~Temperatura_lokacija_mean,
  color = ~PM10, size = ~PM10
)
p

################## Draw plot05 ################## 
PM10Ljubljana <-data[,"PM10"]
WindSpeedLjubljana <-data[,"Hitrost_vetra_max"]
p <- plot_ly(
  data, y = ~PM10, x = ~Hitrost_vetra_mean,
  color = ~PM10, size = ~PM10Ljubljana
)
p

################## Draw plot06 ################## 
p <- plot_ly(
  data, y = ~O3, x = ~Vlaga_mean,
  color = ~O3, size = ~Vlaga_mean
)
p

