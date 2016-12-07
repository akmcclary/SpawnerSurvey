library(shiny)
library("plyr")
library("dplyr")
library("plotly")
library("ggplot2")
library("magrittr")
library("lubridate")
library("xlsx")
library("DT")
library("splitstackshape")

#flowData<- read.xlsx("Daily_Precip_Discharge_Monitoring.xlsx", sheetName = "DailyGauge")
#tripData<-read.xlsx("TripData.xlsx", sheetName = "TripDataFish")

flowData<- read.csv("Daily_Precip_Discharge_Monitoring.csv")
tripData<-read.csv("TripDataFish.csv")


tripData$DATE<-mdy(tripData$Date)
flowData$DATE<-mdy(flowData$DATE)
gaugeNames<-c(colnames(flowData))
gaugeNames<-gaugeNames[2:14]
flowData[gaugeNames]<- sapply(flowData[gaugeNames],as.numeric)
mergedData<- join(flowData, tripData, by = "DATE", "inner")
mergedData$Fishing<-as.factor(mergedData$Fishing)


#Get the most recent date the trip was surveyed if it was surveyed this season
mostRecentTrip <- tripData %>% filter(Season == "2016-2017")%>% filter(Fishing == 1) %>% group_by(ReachName, Tributary) %>% filter(DATE == max(DATE)) %>% arrange(ReachName,Tributary)
mostRecentTrip$daysSinceSurveyed<- (today("America/Los_Angeles"))-mostRecentTrip$DATE
TripCounts<-tripData %>% filter(Season == "2016-2017")%>% filter(Fishing == 1) %>%count_(c("ReachName", "Tributary"))
mostRecentTrip<-inner_join(TripCounts, mostRecentTrip, by = c("ReachName", "Tributary"))

#FilteredData<- mergedData %>% filter(REACHNAME == selectedReach)
#maxSurveyedCFS<- max(FilteredData[selectedGauge])

#GreenValleyPlot <- tripData %>% filter(REACHNAME == selectedReach)%>% ggplot(aes(x=DATE, y = FISHING)) + geom_point()+ scale_x_date()
#GreenValleyPlot + geom_line(data = flowData, aes(DATE, MIL.School..ft..))

surveyedReaches<-unique(tripData$ReachName)
surveyedReaches<- as.vector.factor(surveyedReaches)
surveyedReaches<-sort(surveyedReaches)

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
                value = '2016-1-1'
      ),
      dateInput('enddate',
                label = 'End input: yyyy-mm-dd',
                value = '2016-11-30'
      ), width = 2
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel( type = "tabs",
                   tabPanel("Surveyability", htmlOutput("maxflow"), plotlyOutput("HydroGraphPlot"),
                            
                            DT::dataTableOutput("fishTable")),
                   tabPanel("DaysSinceSurveyed", DT::dataTableOutput("mostRecentSurveyTable"))
      ))
    
    
  )
)
server<- function(input, output) {
  
  output$maxflow <-renderUI({
    FilteredData<- filter(mergedData, ReachName == input$Reach)
    maxSurveyedCFS<- max(FilteredData[input$Gauge], na.rm = TRUE)
    HTML(paste("<p> <br> </p> <b> This reach has been surveyed at a maximum gauge height of:", maxSurveyedCFS,"</b> "))
  })
  
  output$fishTable = renderDataTable({
    
    FishData<- mergedData %>%filter(ReachName == input$Reach)%>%filter(DATE >= input$startDate & DATE <= input$enddate)
    FishData<- FishData[,c("Date","ReachName", "Tributary", "Crew", "CohoIndividuals","SteelheadIndividuals","ChinookIndividuals", "SalmonidSpIndividuals", "CohoRedds", "SteelheadRedds", "ChinookRedds", "SalmonidSpRedds","Comments")]
    DT::datatable(FishData, colnames = c("Date","Reach", "Tributary", "Crew", "Coho","Steelhead","Chinook", "SalmonidSp", "Coho Redds", "Steelhead Redds", "Chinook Redds", "SalmonidSp Redds","Comments"), options = list(pagelength = 10, autoWidth = FALSE))
  })
  
  output$mostRecentSurveyTable = renderDataTable({
    mostRecentTrip<- mostRecentTrip[,c("daysSinceSurveyed", "ReachName", "Tributary", "Date", "n" )]
    DT::datatable(mostRecentTrip, colnames = c("Days Since Surveyed", "Reach", "Tributary", "Date Last Surveyed", "Number of Times Surveyed this Year" ), rownames = FALSE, options = list(pageLength = 55, order = list(list(0, 'desc'))))
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