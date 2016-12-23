library("xlsx")
tripData<-read.xlsx("TripData.xlsx", sheetName = "TripDataFish")
write.csv(tripData,file="TripDataFish.csv")
tripData<-read.xlsx("TripData.xlsx", sheetName = "Individuals")
write.csv(tripData,file="Fish.csv")
tripData<-read.xlsx("TripData.xlsx", sheetName = "Redds")
write.csv(tripData,file="Redds.csv")

for (name in CrewNames){
  CrewCounts$CrewName[i] = name
  CrewCounts$FishTotals[i]<-fish %>% filter(Season == "2016-2017")%>%filter(grepl(name, Crew))%>%filter(Species == "COHO SALMON")%>%nrow()
  i=i+1
}

CrewCounts<-data.frame(FishTotals=numeric(15))