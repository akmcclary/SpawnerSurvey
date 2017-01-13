library(shiny)
library("plyr")
library("dplyr")
library("plotly")
library("ggplot2")
library("magrittr")
library("lubridate")
library("DT")
library("RColorBrewer")
library("readr")
library("scales")
#flowData<- read.xlsx("Daily_Precip_Discharge_Monitoring.xlsx", sheetName = "DailyGauge")
#tripData<-read.xlsx("TripDataFish.xlsx", sheetName = "AllData")

#Import Data
flowData<- read_csv("Daily_Precip_Discharge_Monitoring.csv")
tripData<-read_csv("TripDataFish.csv")
fish<-read_csv("Fish.csv")
redds<-read_csv("Redds.csv")

#Convert Dates to Dates in R
tripData$DATE<-tripData$Date
flowData$DATE<-mdy(flowData$DATE)
# fish$Date<-mdy(fish$Date)
# redds$Date<-mdy(redds$Date)
fish<-mutate_each(fish,funs(toupper))
redds<-mutate_each(redds,funs(toupper))
tripData<-tripData%>%mutate_each(funs(toupper),Tributary)

uploadDate<-max(tripData$DATE)
fish<-filter(fish, Species != "-9999")
#FishTotals<-aggregate(fish, by= list(ReachName = fish$ReachName, Tributary = fish$Tributary, Season = fish$Season, Species = fish$Species), FUN = length)
#redds<-filter(redds,ReddAge==1)
#ReddTotals<-aggregate(redds, by= list(ReachName = redds$ReachName, Tributary = redds$Tributary, Season = redds$Season, Species = redds$Species), FUN = length)
myColors<-brewer.pal(5,"Set1")
names(myColors)<-unique(fish$Species)
colScale<-scale_color_manual(name="Species", values = myColors)
fillScale<-scale_fill_manual(name="Species", values = myColors)

speciesList<-c("COHO SALMON", "SALMONID SP", "STEELHEAD", "CHINOOK SALMON", "PACIFIC LAMPREY")
speciesList2<-c("COHO SALMON", "SALMONID SP", "STEELHEAD", "CHINOOK SALMON", "PACIFIC LAMPREY", "ALL")

gaugeNames<-c(colnames(flowData))
gaugeNames<-gaugeNames[2:14]
flowData[gaugeNames]<- sapply(flowData[gaugeNames],as.numeric)
mergedData<- join(flowData, tripData, by = "DATE", "inner")
mergedData$Fishing<-as.factor(mergedData$Fishing)

#fishplot<-fish%>%filter(Season=="2016-2017")%>%ggplot(aes(Species, fill = Species))+ geom_bar()+theme_classic()+ggtitle("Live Fish Seen")+stat_count(aes(y = ..count.. + 1, label=..count..), vjust=0, geom="text", position="identity")+ fillScale
#reddplot<-redds%>%filter(Season=="2016-2017")%>%filter(ReddAge == 1)%>%ggplot(aes(Species, fill = Species))+ geom_bar()+theme_classic()+ggtitle("Redds Seen")+stat_count(aes(y = ..count.. + 1, label=..count..), vjust=0, geom="text", position="identity")+ fillScale

#ReddTotalsPlot<-redds %>% filter(ReachName=="RED 1") %>% filter(Tributary == "REDWOOD CREEK") %>% filter(Season == "2015-2016") %>% ggplot(aes(x=Species, fill=Species))+ geom_bar()
#Get the most recent date the trip was surveyed if it was surveyed this season
mostRecentTrip <- tripData %>% filter(Season == "2016-2017")%>% filter(Fishing == 1) %>% group_by(ReachName, Tributary) %>% filter(DATE == max(DATE)) %>% arrange(ReachName,Tributary)
mostRecentTrip$daysSinceSurveyed<- (today("America/Los_Angeles"))-mostRecentTrip$DATE
TripCounts<-tripData %>% filter(Season == "2016-2017")%>% filter(Fishing == 1) %>%count_(c("ReachName", "Tributary"))
mostRecentTrip<-inner_join(TripCounts, mostRecentTrip, by = c("ReachName", "Tributary"))
#ZacTrips<-tripData %>% filter(Season == "2016-2017")%>% filter(Fishing == 1) %>%filter(grepl('ZR', Crew))
#FilteredData<- mergedData %>% filter(REACHNAME == selectedReach)
#maxSurveyedCFS<- max(FilteredData[selectedGauge])

#GreenValleyPlot <- tripData %>% filter(REACHNAME == selectedReach)%>% ggplot(aes(x=DATE, y = FISHING)) + geom_point()+ scale_x_date()
#GreenValleyPlot + geom_line(data = flowData, aes(DATE, MIL.School..ft..))
CrewNames<-c("ZR","WB","CO","MT","LE","AMJ","NB","AB","AM ","AI", "SB", "JP", "KS","RA", "BA", "JRR", "AJ")
CrewCounts<-data.frame(FishTotals=numeric(17), ReddTotals=numeric(17), CombinedTotals=numeric(17))
surveyedReaches<-unique(tripData$ReachName)
surveyedTributaries<-unique(tripData$Tributary)
#surveyedReaches<- as.vector.factor(surveyedReaches)
surveyedTributaries<-sort(surveyedTributaries)
surveyedTributaries<-append(surveyedTributaries, "ALL", 0)
surveyedReaches<-sort(surveyedReaches)
surveyedReaches<-append(surveyedReaches, "ALL", 0)
seasonList<- unique(redds$Season)
seasonList<-append(seasonList, "ALL",0)
seasonList<- sort(seasonList, decreasing = TRUE)


gaugeList <- colnames(flowData)
gaugeList <- gaugeList[-1]

ui<- fluidPage(
  theme="bootstrap.css",
  titlePanel(htmlOutput("uploadDate")),
  tabsetPanel( type = "tabs",
    tabPanel("Fish And Redd Counts",sidebarLayout(sidebarPanel(selectInput("Tributary", "Tributary:", surveyedTributaries), uiOutput("reachSelect"),
      selectInput("Season", "Season:", seasonList, selected = "2016-2017"), uiOutput("selectSeasonOrSpecies"), width = 2), mainPanel( htmlOutput("NumberOfFish"), 
      plotOutput("fishGraph"), htmlOutput("NumberOfRedds"), plotOutput("reddGraph")))), 
    tabPanel("Crew Totals", selectInput("Crew","Crew:", CrewNames),htmlOutput("CrewNumberOfFish"), 
      plotOutput("CrewfishGraph"), htmlOutput("CrewNumberOfRedds"), plotOutput("CrewreddGraph")),
    tabPanel("Crew Leader Boards", selectInput("SpeciesSelected", "Species:", speciesList2, selected = "ALL"), DT::dataTableOutput("CrewLeaderBoard")),
    tabPanel("Surveyability", 
      sidebarLayout(sidebarPanel( selectInput("ReachFlow", "Reach:", surveyedReaches, selected = "ALL"), selectInput("Gauge", "Gauge:", gaugeList),
        dateInput('startDate', label = 'Start Date: yyyy-mm-dd', value = '2016-01-1'),
        dateInput('enddate', label = 'End Date: yyyy-mm-dd', value = uploadDate), width = 2),
      mainPanel( htmlOutput("maxflow"), plotlyOutput("HydroGraphPlot"), DT::dataTableOutput("fishTable")))),
    tabPanel("DaysSinceSurveyed", DT::dataTableOutput("mostRecentSurveyTable"))))

server<- function(input, output) {
  
#Surveyability Tab  
  output$maxflow <-renderUI({
    if (input$ReachFlow == "ALL"){
      HTML(paste("<p> <br> </p> <b> Please select a specific reach</b> ")) 
    } else {
      FilteredData<- filter(mergedData, ReachName == input$ReachFlow)
      maxSurveyedCFS<- max(FilteredData[input$Gauge], na.rm = TRUE)
      HTML(paste("<p> <br> </p> <b> ", input$ReachFlow, " has been surveyed at a maximum gauge height of:", maxSurveyedCFS,"</b> "))
      }})
  
  
  output$HydroGraphPlot <- renderPlotly({
    if (input$ReachFlow == "ALL"){
      return(NULL)
      } else {
      FilteredData2<- mergedData %>% filter(ReachName == input$ReachFlow)
      maxGaugeHeight <- max(FilteredData2[input$Gauge], na.rm = TRUE)
      flowGraph <- mergedData %>% filter(ReachName == input$ReachFlow)%>% ggplot(aes_string(x="DATE", y = input$Gauge)) + theme_minimal() + geom_point(aes(color=Fishing, shape = Fishing), size = 6)+ scale_color_manual(values = c("0" = "red", "1"= "green")) + scale_shape_manual(values = c("0" = 4, "1" = 11))+ scale_x_date(limits = c(input$startDate,input$enddate)) + geom_line(data = flowData, aes_string("DATE", input$Gauge))
      plotlygraph <- ggplotly(flowGraph)
      plotlygraph
    }})
  output$fishTable = renderDataTable({
    FishData<- mergedData %>%filter(ReachName == input$ReachFlow)%>%filter(DATE >= input$startDate & DATE <= input$enddate)
    FishData<- FishData[,c("Date","ReachName", "Tributary", "Crew", "CohoIndividuals","SteelheadIndividuals","ChinookIndividuals", "SalmonidSpIndividuals", "CohoRedds", "SteelheadRedds", "ChinookRedds", "SalmonidSpRedds","Comments")]
    DT::datatable(FishData, colnames = c("Date","Reach", "Tributary", "Crew", "Coho","Steelhead","Chinook", "SalmonidSp", "Coho Redds", "Steelhead Redds", "Chinook Redds", "SalmonidSp Redds","Comments"), rownames = FALSE, options = list(order = list(list(0, 'desc')), pagelength = 10, autoWidth = FALSE))
  })
  #Put in Header
  output$uploadDate<- renderUI({
    HTML(paste("<b> Data current through:", uploadDate,"</b> "))
  })
  
  #Fish and Redds but maybe also for surveyability etc..
  output$reachSelect <- renderUI({
    if (input$Tributary=="ALL"){
      return(NULL)
      # reachList<-unique(tripData$ReachName)%>%append( "ALL", 0)%>%sort()
      # selectInput("Reach", "Reach: ", reachList)
      }
    else{
      filteredTripData<- tripData%>%filter(Tributary==input$Tributary)
      reachList<-unique(filteredTripData$ReachName)%>%append( "ALL", 0)%>%sort()
      selectInput("Reach", "Reach: ", reachList, selected = "ALL")
      }})
  
  output$selectSeasonOrSpecies<-renderUI({
    if (input$Season=="ALL"){
      radioButtons("SpeciesOrSeason","View by Species or Season",c( "Species" = "Species", "Season" = "Season"))
    } else return(NULL)
  })
  
  
  
  #Fish and Redds Tab
  output$fishGraph <- renderPlot({
    if(input$Season=="ALL"){
      if(input$Tributary=="ALL"){
        FishNumber<-fish%>%nrow()
        if (FishNumber>0){
          if(input$SpeciesOrSeason=="Season"){
          fishplot2<-fish%>%ggplot(aes(Species, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(strip.text= element_text(size=12), axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Total Fish Seen")+ stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_x_discrete(limits=speciesList) + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0))+facet_wrap(~Season) 
          fishplot2
        }else {
          fishplot2<-fish%>%ggplot(aes(Season, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(strip.text= element_text(size=12), axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Total Fish Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) + facet_wrap(~Species)
          fishplot2 
        }} else return(NULL)        
          
          
         
          } else {
        #Season is all but Tributary is not
       if(input$Reach=="ALL"){
        FishNumber<-fish%>%filter(Tributary == input$Tributary)%>%nrow()
        if (FishNumber>0){
          if (input$SpeciesOrSeason == "Season"){
          fishplot<-fish%>%filter(Tributary == input$Tributary)%>%ggplot(aes(Species, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(strip.text= element_text(size=12), axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Total Fish Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_x_discrete(limits=speciesList) + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) + facet_wrap(~Season)
          fishplot
          } else {
            fishplot<-fish%>%filter(Tributary == input$Tributary)%>%ggplot(aes(Season, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(strip.text= element_text(size=12), axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Total Fish Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) + facet_wrap(~Species)
            fishplot
            }
          
          # fishplotly<-ggplotly( fishplot)
          # fishplotly
          
          
        } else return(NULL)
          
      }else{
        FishNumber<-fish%>%filter(Tributary== input$Tributary)%>%filter(ReachName == input$Reach)%>%nrow()
        if (FishNumber>0){
          if (input$SpeciesOrSeason == "Season"){
            fishplot<-fish%>%filter(Tributary == input$Tributary)%>%filter(ReachName == input$Reach)%>%ggplot(aes(Species, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(strip.text= element_text(size=12), axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Total Fish Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_x_discrete(limits=speciesList) + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) + facet_wrap(~Season)
          } else {
            fishplot<-fish%>%filter(Tributary == input$Tributary)%>%filter(ReachName == input$Reach)%>%ggplot(aes(Season, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(strip.text= element_text(size=12), axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Total Fish Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) + facet_wrap(~Species)
          }
          # fishplotly<-ggplotly( fishplot)
          # fishplotly
          fishplot
          
        } else return(NULL)
        }
          
        }
      }else{
    if (input$Tributary == "ALL"){
     
      
        FishNumber<-fish%>%filter(Season==input$Season)%>%nrow()
        if (FishNumber>0){
        fishplot2<-fish%>%filter(Season==input$Season)%>%ggplot(aes(Species, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Total Fish Seen")+ stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_x_discrete(limits=speciesList) + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) 
        
        fishplot2} else return(NULL)
        
      } else {
        if(input$Reach=="ALL"){
          FishNumber<-fish%>%filter(Season==input$Season)%>%filter(Tributary == input$Tributary)%>%nrow()
          if (FishNumber>0){
            
            fishplot<-fish%>%filter(Season==input$Season)%>%filter(Tributary == input$Tributary)%>%ggplot(aes(Species, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Total Fish Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_x_discrete(limits=speciesList) + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) 
            fishplot
          }else return(NULL)
        }else {
        
     FishNumber<-fish%>%filter(Season==input$Season)%>%filter(Tributary == input$Tributary)%>%filter(ReachName == input$Reach)%>%nrow()
    if (FishNumber>0){
    
      fishplot<-fish%>%filter(Season==input$Season)%>%filter(Tributary == input$Tributary)%>%filter(ReachName == input$Reach)%>%ggplot(aes(Species, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Total Fish Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_x_discrete(limits=speciesList) + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) 
      fishplot
  
    } else return(NULL)   
      }}}
     }
    
  )
  
  output$NumberOfFish <- renderUI({
    if(input$Season == "ALL"){
      return(NULL)
    } else{
    if(input$Tributary == "ALL"){
      FishNumbers<-fish%>%filter(Season==input$Season)%>%nrow()
      if(input$Season == "2016-2017"){
        if(FishNumbers>0){
          HTML(paste("<p> <br> </p> <b> There have been ", FishNumbers,"  total fish (including carcasses) seen in the", input$Season,"season </b> ")) 
        } else HTML(paste("<p> <br> </p> <b> There have been no fish seen this season </b> "))
       }else {
        if(FishNumbers>0){
        HTML(paste("<p> <br> </p> <b> There were ", FishNumbers,"  total fish (including carcasses) seen in the", input$Season,"season </b> ")) 
      } else HTML(paste("<p> <br> </p> <b> There were no fish seen on in the", input$Season,"season </b> "))
      }
      
    }else {
    if (input$Reach=="ALL"){
      FishNumbers<-fish%>%filter(Season==input$Season)%>%filter(Tributary == input$Tributary)%>%nrow() 
      if(input$Season == "2016-2017"){
        if(FishNumbers>0){
          HTML(paste("<p> <br> </p> <b> There have been ", FishNumbers,"  total fish (including carcasses) seen on ", input$Tributary, "in the", input$Season,"season </b> ")) 
        } else HTML(paste("<p> <br> </p> <b> There have been no fish seen on ", input$Tributary, " this season </b> "))
      } else if(FishNumbers>0){
        HTML(paste("<p> <br> </p> <b> There were ", FishNumbers,"  total fish (including carcasses) seen on ", input$Tributary, "in the", input$Season,"season </b> ")) 
      } else HTML(paste("<p> <br> </p> <b> There were no fish seen on ", input$Tributary, "in the", input$Season,"season </b> ")) 
    }else{
      FishNumbers<-fish%>%filter(Season==input$Season)%>%filter(Tributary == input$Tributary)%>%filter(ReachName==input$Reach)%>%nrow()
      if(input$Season == "2016-2017"){
        if(FishNumbers>0){
          HTML(paste("<p> <br> </p> <b> There have been ", FishNumbers,"  total fish (including carcasses) seen on ", input$Reach, "in the", input$Season,"season </b> ")) 
        } else HTML(paste("<p> <br> </p> <b> There have been no fish seen on ", input$Reach, " this season </b> "))
      } else if(FishNumbers>0){
        HTML(paste("<p> <br> </p> <b> There were ", FishNumbers,"  total fish (including carcasses) seen on ", input$Reach, "in the", input$Season,"season </b> ")) 
      } else HTML(paste("<p> <br> </p> <b> There were no fish seen on ", input$Reach, "in the", input$Season,"season </b> "))
    }
    
    
    
    
      }}})
  
  output$NumberOfRedds <- renderUI({
    if(input$Season=="ALL"){  
          return(NULL)
    }else {
    
      if (input$Tributary=="ALL"){ 
        ReddNumbers<-redds%>%filter(Season==input$Season)%>%filter(ReddAge == 1)%>%nrow()
        if(input$Season == "2016-2017"){
            if(ReddNumbers>0){
            HTML(paste("<p> <br> </p> <b> There have been ", ReddNumbers,"  total redds seen in the", input$Season," season </b> ")) 
            } else HTML(paste("<p> <br> </p> <b> There have been no redds seen this season </b> "))} else { 
              if(ReddNumbers>0){
  #Previous years totals
              HTML(paste("<p> <br> </p> <b> There were ", ReddNumbers,"  total redds seen in the", input$Season," season </b> ")) 
               } else HTML(paste("<p> <br> </p> <b> There were no redds seen in the", input$Season," season </b> "))
         }} else {
           if(input$Reach=="ALL"){
             ReddNumbers<-redds%>%filter(Season==input$Season)%>%filter(ReddAge == 1)%>%filter(Tributary == input$Tributary)%>%nrow()
             if(input$Season == "2016-2017"){
               if(ReddNumbers>0){
                 HTML(paste("<p> <br> </p> <b> There have been ", ReddNumbers,"  total redds seen on ", input$Tributary, " in the", input$Season," season </b> ")) 
               } else HTML(paste("<p> <br> </p> <b> There have been no redds seen on ", input$Tributary, " this season </b> "))} else if(ReddNumbers>0){
                 HTML(paste("<p> <br> </p> <b> There were ", ReddNumbers,"  total redds seen on ", input$Tributary, " in the", input$Season," season </b> ")) 
               } else HTML(paste("<p> <br> </p> <b> There were no redds seen on ", input$Tributary, " in the", input$Season," season </b> ")) 
           }else{
  #Filtered by Reach and Tributary
            ReddNumbers<-redds%>%filter(Season==input$Season)%>%filter(ReddAge == 1)%>%filter(Tributary == input$Tributary)%>%filter(ReachName == input$Reach)%>%nrow()
            if(input$Season == "2016-2017"){
            if(ReddNumbers>0){
            HTML(paste("<p> <br> </p> <b> There have been ", ReddNumbers,"  total redds seen on ", input$Reach, " in the", input$Season," season </b> ")) 
            } else HTML(paste("<p> <br> </p> <b> There have been no redds seen on ", input$Reach, " this season </b> "))} else if(ReddNumbers>0){
            HTML(paste("<p> <br> </p> <b> There were ", ReddNumbers,"  total redds seen on ", input$Reach, " in the", input$Season," season </b> ")) 
            } else HTML(paste("<p> <br> </p> <b> There were no redds seen on ", input$Reach, " in the", input$Season," season </b> "))
         }}}}) 
  
  output$reddGraph<- renderPlot({
    if (input$Season=="ALL"){
      if (input$Tributary=="ALL"){
        reddNumber<-redds%>%filter(ReddAge==1)%>%nrow()
        if(reddNumber>0){
        if (input$SpeciesOrSeason=="Season"){
          reddplot<-redds%>%filter(ReddAge == 1)%>%ggplot(aes(Species, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(strip.text= element_text(size=12), axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Redds Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_x_discrete(limits=speciesList) + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) +facet_wrap(~Season)
          reddplot
       }else {
          reddplot<-redds%>%filter(ReddAge == 1)%>%ggplot(aes(Season, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(strip.text= element_text(size=12), axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Redds Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0))+facet_wrap(~Species)
          reddplot
       }}else return(NULL)
        }else{
         if (input$Reach=="ALL"){
           reddNumber<-redds%>%filter(ReddAge==1)%>%filter(Tributary==input$Tributary)%>%nrow()
           if(reddNumber>0){
           if (input$SpeciesOrSeason=="Season"){
             reddplot<-redds%>%filter(ReddAge == 1)%>%filter(Tributary==input$Tributary)%>%ggplot(aes(Species, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(strip.text= element_text(size=12), axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Redds Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_x_discrete(limits=speciesList) + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) +facet_wrap(~Season)
             reddplot
             }else { 
             reddplot<-redds%>%filter(ReddAge == 1)%>%filter(Tributary==input$Tributary)%>%ggplot(aes(Season, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(strip.text= element_text(size=12), axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Redds Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0))+facet_wrap(~Species)
             reddplot
             }} else return(NULL)
         }else {
           reddNumber<-redds%>%filter(ReddAge==1)%>%filter(Tributary==input$Tributary)%>%filter(ReachName==input$Reach)%>%nrow()
           if(reddNumber>0){
             if (input$SpeciesOrSeason=="Season"){
               reddplot<-redds%>%filter(ReddAge == 1)%>%filter(Tributary==input$Tributary)%>%filter(ReachName==input$Reach)%>%ggplot(aes(Species, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(strip.text= element_text(size=12), axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Redds Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_x_discrete(limits=speciesList) + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) +facet_wrap(~Season)
               reddplot
             }else { 
               reddplot<-redds%>%filter(ReddAge == 1)%>%filter(Tributary==input$Tributary)%>%filter(ReachName==input$Reach)%>%ggplot(aes(Season, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(strip.text= element_text(size=12), axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Redds Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0))+facet_wrap(~Species)
               reddplot
             }} else return(NULL)
           }
         
       }}else{
#Season is not ALL
    if (input$Tributary == "ALL"){  
  #Maybe fix this if I make the reach drop down dependent on choosing a tributary
      ReddNumber <- redds%>%filter(Season==input$Season)%>%filter(ReddAge == 1)%>%nrow()
      if (ReddNumber>0){
      reddplot<-redds%>%filter(Season==input$Season)%>%filter(ReddAge == 1)%>%ggplot(aes(Species, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Redds Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_x_discrete(limits=speciesList) + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) 
      reddplot } else return(NULL) 
      }else{
#Tributary is not All
        
        if (input$Reach == "ALL"){
          ReddNumber <- redds%>%filter(Season==input$Season)%>%filter(ReddAge == 1)%>%filter(Tributary==input$Tributary)%>%nrow()
          if(ReddNumber>0){
          reddplot<-redds%>%filter(Season==input$Season)%>%filter(ReddAge == 1)%>%filter(Tributary==input$Tributary)%>%ggplot(aes(Species, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Redds Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_x_discrete(limits=speciesList) + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) 
          reddplot } else return(NULL)
          }else {
          #Reach is not ALL
          ReddNumber <- redds%>%filter(Season==input$Season)%>%filter(ReddAge == 1)%>%filter(Tributary==input$Tributary)%>%filter(ReachName == input$Reach)%>%nrow()
          if (ReddNumber>0){
            reddplot<-redds%>%filter(Season==input$Season)%>%filter(ReddAge == 1)%>%filter(Tributary==input$Tributary)%>%filter(ReachName == input$Reach)%>%ggplot(aes(Species, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Redds Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_x_discrete(limits=speciesList) + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) 
            reddplot} else return(NULL)
      }   
        }
          }})
  
  #Crew Tab
  output$CrewfishGraph <- renderPlot({
    FishNumber<-fish%>%filter(Season == "2016-2017")%>%filter(grepl(input$Crew, Crew))%>%nrow()
    if (FishNumber>0){
      
      fishplot<-fish%>%filter(Season == "2016-2017")%>%filter(grepl(input$Crew, Crew))%>%ggplot(aes(Species, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Total Fish Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_x_discrete(limits=speciesList) + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) 
      # fishplotly<-ggplotly( fishplot)
      # fishplotly
      fishplot
      
    } else return(NULL)   
  }
  
  )
  
  output$CrewNumberOfFish <- renderUI({
    CrewFishNumbers<-fish %>% filter(Season == "2016-2017")%>%filter(grepl(input$Crew, Crew))%>%nrow()
    
    
    # if(input$Season == "2016-2017"){
      if(CrewFishNumbers>0){
        HTML(paste("<p> <br> </p> <b> There have been ", CrewFishNumbers,"  total fish seen by ", input$Crew, " this season </b> ")) 
      } else HTML(paste("<p> <br> </p> <b> There have been no fish seen by ", input$Crew, " this season :( </b> "))
    # } 
    # else if(CrewFishNumbers>0){
    #   HTML(paste("<p> <br> </p> <b> There were ", CrewFishNumbers,"  total fish seen on ", input$Crew, "in the", input$Season,"season </b> ")) 
    # } else HTML(paste("<p> <br> </p> <b> There were no fish seen by ", input$Crew, "in the", input$Season,"season </b> "))
  }
  )  
  
  
  output$CrewNumberOfRedds <- renderUI({
    CrewReddNumbers<-redds%>%filter(Season=="2016-2017")%>%filter(ReddAge == 1)%>%filter(grepl(input$Crew, Crew))%>%nrow()
    
    
    # if(input$Season == "2016-2017"){
      if(CrewReddNumbers>0){
        HTML(paste("<p> <br> </p> <b> There have been ", CrewReddNumbers,"  total redds seen by ", input$Crew, " this season </b> ")) 
      } else HTML(paste("<p> <br> </p> <b> There have been no redds seen by ", input$Crew, " this season </b> ")) 
    # else if(ReddNumbers>0){
    #     HTML(paste("<p> <br> </p> <b> There were ", CrewReddNumbers,"  total redds seen on ", input$Crew, " in the", input$Season," season </b> ")) 
    #   } else HTML(paste("<p> <br> </p> <b> There were no redds seen by ", input$Crew, " in the", input$Season," season </b> "))
  }
  ) 
  
  output$CrewreddGraph<- renderPlot({
    CrewReddNumber <- redds%>%filter(Season=="2016-2017")%>%filter(ReddAge == 1)%>%filter(grepl(input$Crew, Crew))%>%nrow()
    if (CrewReddNumber>0){
      Crewreddplot<-redds%>%filter(Season=="2016-2017")%>%filter(ReddAge == 1)%>%filter(grepl(input$Crew, Crew))%>%ggplot(aes(Species, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Redds Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_x_discrete(limits=speciesList) + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) 
      Crewreddplot
      
    } else return(NULL)
  })
  
  #Most Recent Survey Tab
  
  output$mostRecentSurveyTable = renderDataTable({
    mostRecentTrip<- mostRecentTrip[,c("daysSinceSurveyed", "ReachName", "Tributary", "Date", "n" )]
    DT::datatable(mostRecentTrip, colnames = c("Days Since Surveyed", "Reach", "Tributary", "Date Last Surveyed", "Number of Times Surveyed this Year" ), rownames = FALSE, options = list(pageLength = 57, order = list(list(0, 'desc'))))
  })
  # output$results<- renderTable({
  #   FilteredData<- filter(mergedData, REACHNAME == input$Reach)
  # maxSurveyedCFS<- max(FilteredData[input$Gauge])
  # meanGaugeHeight <- mean(FilteredData[input$Gauge])
  # maxSurveyedCFS
  # })
  
  
  #Crew Leader Tab
  output$CrewLeaderBoard = renderDataTable({
    i=1
    if(input$SpeciesSelected=="ALL"){
      for (name in CrewNames){
        CrewCounts$CrewName[i] = name
        CrewCounts$FishTotals[i]<-fish %>% filter(Season == "2016-2017")%>%filter(grepl(name, Crew))%>%nrow()
        CrewCounts$ReddTotals[i]<-redds%>% filter(Season == "2016-2017")%>%filter(ReddAge == 1)%>%filter(grepl(name, Crew))%>%nrow()
        i=i+1
      }
      CrewCounts$CombinedTotals<-CrewCounts$FishTotals + CrewCounts$ReddTotals
      
      DT::datatable(CrewCounts, colnames = c("Crew", "Fish Total", "Redd Total", "Combined Total"), rownames = FALSE, options = list( pageLength= 17, order=list(3,'desc')))
    } else for (name in CrewNames){
      CrewCounts$CrewName[i] = name
      CrewCounts$FishTotals[i]<-fish %>% filter(Season == "2016-2017")%>%filter(grepl(name, Crew))%>%filter(Species == input$SpeciesSelected)%>%nrow()
      CrewCounts$ReddTotals[i]<-redds%>% filter(Season == "2016-2017")%>%filter(ReddAge == 1)%>%filter(grepl(name, Crew))%>%filter(Species == input$SpeciesSelected)%>%nrow()
      
      i=i+1
    }
    CrewCounts$CombinedTotals<-CrewCounts$FishTotals + CrewCounts$ReddTotals
    CrewCounts<-CrewCounts[,c(4,1,2,3)]
    
    DT::datatable(CrewCounts, colnames = c("Crew", "Fish Total", "Redd Total", "Combined Total"), rownames = FALSE, options = list(pageLength= 17,  order=list(3,'desc')))
  })
  
}
shinyApp(ui= ui, server = server)