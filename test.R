

data <- read.csv("RealEstateData1.csv", header = T)

library(pastecs)
attach(data)
scores <-cbind(SalePrice, GrLivArea, X1stFlrSF, GarageArea, LotArea, OverallQual)

options(scipen=100)
options(digits=2)
myTable <- stat.desc(scores)
print (myTable[-c(1, 2, 3, 7, 10, 11, 14 ), ])


