library("xlsx")
tripData<-read.xlsx("TripDataFish.xlsx", sheetName = "AllData")
write.csv(tripData,file="TripDataFish.csv")