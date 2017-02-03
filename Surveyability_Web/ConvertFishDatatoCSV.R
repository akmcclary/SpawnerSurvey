library("xlsx")
tripData<-read.xlsx("TripData.xlsx", sheetName = "TripDataFish")
write.csv(tripData,file="TripDataFish.csv")
tripData<-read.xlsx("TripData.xlsx", sheetName = "Individuals")
write.csv(tripData,file="Fish.csv")
tripData<-read.xlsx("TripData.xlsx", sheetName = "Redds")
write.csv(tripData,file="Redds.csv")

