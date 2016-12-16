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
fish$Date<-mdy(fish$Date)
redds$Date<-mdy(redds$Date)
fish<-mutate_each(fish,funs(toupper))
redds<-mutate_each(redds,funs(toupper))
uploadDate<-max(tripData$DATE)
#FishTotals<-aggregate(fish, by= list(ReachName = fish$ReachName, Tributary = fish$Tributary, Season = fish$Season, Species = fish$Species), FUN = length)
#redds<-filter(redds,ReddAge==1)
#ReddTotals<-aggregate(redds, by= list(ReachName = redds$ReachName, Tributary = redds$Tributary, Season = redds$Season, Species = redds$Species), FUN = length)
myColors<-brewer.pal(6,"Set1")
names(myColors)<-unique(fish$Species)
colScale<-scale_color_manual(name="Species", values = myColors)
fillScale<-scale_fill_manual(name="Species", values = myColors)

speciesList<-c("COHO SALMON", "SALMONID SP", "STEELHEAD", "CHINOOK SALMON", "PACIFIC LAMPREY")

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

surveyedReaches<-unique(tripData$ReachName)
#surveyedReaches<- as.vector.factor(surveyedReaches)
surveyedReaches<-sort(surveyedReaches)

seasonList<- unique(redds$Season)
seasonList<- sort(seasonList, decreasing = TRUE)


gaugeList <- colnames(flowData)
gaugeList <- gaugeList[-1]

ui<- fluidPage(
  theme="bootstrap.css",
  # Application title
  titlePanel("Spawner Surveys"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("Reach",
                  "Reach:",
                  surveyedReaches), selectInput("Gauge", "Gauge:", gaugeList),
      dateInput('startDate',
                label = 'Start Date: yyyy-mm-dd',
                value = '2016-01-1'
      ),
      dateInput('enddate',
                label = 'End Date: yyyy-mm-dd',
                value = uploadDate
      ), htmlOutput("uploadDate"), width = 2
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel( type = "tabs",
                   tabPanel("Surveyability", htmlOutput("maxflow"), plotlyOutput("HydroGraphPlot"),
                            
                            DT::dataTableOutput("fishTable")),
                   tabPanel("DaysSinceSurveyed", DT::dataTableOutput("mostRecentSurveyTable")),
                   tabPanel("Fish And Redd Counts",
                             
                            selectInput("Season",
                                        "Season:",
                                        seasonList, selected = "2016-2017"),
                            htmlOutput("NumberOfFish"), plotOutput("fishGraph"), htmlOutput("NumberOfRedds"), plotOutput("reddGraph"))
      ))
    
    
  )
)
server<- function(input, output) {
  
  output$maxflow <-renderUI({
    FilteredData<- filter(mergedData, ReachName == input$Reach)
    maxSurveyedCFS<- max(FilteredData[input$Gauge], na.rm = TRUE)
    HTML(paste("<p> <br> </p> <b> This reach has been surveyed at a maximum gauge height of:", maxSurveyedCFS,"</b> "))
  })
  
  output$uploadDate<- renderUI({
    HTML(paste("<p> <br> </p> <b> Data current through: <p> <br> </p>", uploadDate,"</b> "))
  })
  
  output$fishGraph <- renderPlot({
    FishNumber<-fish%>%filter(Season==input$Season)%>%filter(ReachName == input$Reach)%>%nrow()
    if (FishNumber>0){
    
      fishplot<-fish%>%filter(Season==input$Season)%>%filter(ReachName == input$Reach)%>%ggplot(aes(Species, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Live Fish Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_x_discrete(limits=speciesList) + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) 
  # fishplotly<-ggplotly( fishplot)
  # fishplotly
      fishplot
  
    } else return(NULL)   
    }
    
  )
  
  output$NumberOfFish <- renderUI({
    FishNumbers<-fish%>%filter(Season==input$Season)%>%filter(ReachName == input$Reach)%>%nrow()
    
    
    if(input$Season == "2016-2017"){
    if(FishNumbers>0){
      HTML(paste("<p> <br> </p> <b> There have been ", FishNumbers,"  total fish seen on ", input$Reach, "in the", input$Season,"season </b> ")) 
    } else HTML(paste("<p> <br> </p> <b> There have been no fish seen on ", input$Reach, " this season </b> "))
    } else if(FishNumbers>0){
      HTML(paste("<p> <br> </p> <b> There were ", FishNumbers,"  total fish seen on ", input$Reach, "in the", input$Season,"season </b> ")) 
    } else HTML(paste("<p> <br> </p> <b> There were no fish seen on ", input$Reach, "in the", input$Season,"season </b> "))
  }
  ) 
  
  output$NumberOfRedds <- renderUI({
    ReddNumbers<-redds%>%filter(Season==input$Season)%>%filter(ReddAge == 1)%>%filter(ReachName == input$Reach)%>%nrow()
    
    
    if(input$Season == "2016-2017"){
    if(ReddNumbers>0){
      HTML(paste("<p> <br> </p> <b> There have been ", ReddNumbers,"  total redds seen on ", input$Reach, " in the", input$Season," season </b> ")) 
    } else HTML(paste("<p> <br> </p> <b> There have been no redds seen on ", input$Reach, " this season </b> "))} else if(ReddNumbers>0){
      HTML(paste("<p> <br> </p> <b> There were ", ReddNumbers,"  total redds seen on ", input$Reach, " in the", input$Season," season </b> ")) 
    } else HTML(paste("<p> <br> </p> <b> There were no redds seen on ", input$Reach, " in the", input$Season," season </b> "))
  }
  ) 
  
  output$reddGraph<- renderPlot({
    ReddNumber <- redds%>%filter(Season==input$Season)%>%filter(ReddAge == 1)%>%filter(ReachName == input$Reach)%>%nrow()
    if (ReddNumber>0){
    reddplot<-redds%>%filter(Season==input$Season)%>%filter(ReddAge == 1)%>%filter(ReachName == input$Reach)%>%ggplot(aes(Species, fill = Species))+ geom_bar(width = .75)+theme_classic()+theme(axis.text = element_text(face = "bold", hjust = .5, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust = .5, size = 16),  legend.title = element_text(face = "bold", hjust = .5, size = 12) )+ ggtitle("Redds Seen")+stat_count(aes(y = ..count.. /1.5  , label=..count..), vjust= 0, size = 6, geom="text", position="identity")+ fillScale + scale_x_discrete(limits=speciesList) + scale_y_continuous(breaks=pretty_breaks(n = 3, min.n=0)) 
  # reddplotly<-ggplotly(reddplot)
  # reddplotly
    reddplot
   
    } else return(NULL)
  })
  output$fishTable = renderDataTable({
    
    FishData<- mergedData %>%filter(ReachName == input$Reach)%>%filter(DATE >= input$startDate & DATE <= input$enddate)
    FishData<- FishData[,c("Date","ReachName", "Tributary", "Crew", "CohoIndividuals","SteelheadIndividuals","ChinookIndividuals", "SalmonidSpIndividuals", "CohoRedds", "SteelheadRedds", "ChinookRedds", "SalmonidSpRedds","Comments")]
    DT::datatable(FishData, colnames = c("Date","Reach", "Tributary", "Crew", "Coho","Steelhead","Chinook", "SalmonidSp", "Coho Redds", "Steelhead Redds", "Chinook Redds", "SalmonidSp Redds","Comments"), rownames = FALSE, options = list(order = list(list(0, 'desc')), pagelength = 10, autoWidth = FALSE))
  })
  
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
  output$HydroGraphPlot <- renderPlotly({
    
    FilteredData2<- mergedData %>% filter(ReachName == input$Reach)
    maxGaugeHeight <- max(FilteredData2[input$Gauge], na.rm = TRUE)
    flowGraph <- mergedData %>% filter(ReachName == input$Reach)%>% ggplot(aes_string(x="DATE", y = input$Gauge)) + theme_minimal() + geom_point(aes(color=Fishing, shape = Fishing), size = 6)+ scale_color_manual(values = c("0" = "red", "1"= "green")) + scale_shape_manual(values = c("0" = 4, "1" = 11))+ scale_x_date(limits = c(input$startDate,input$enddate)) + geom_line(data = flowData, aes_string("DATE", input$Gauge))
    plotlygraph <- ggplotly(flowGraph)
    plotlygraph
  })
  
}
shinyApp(ui= ui, server = server)