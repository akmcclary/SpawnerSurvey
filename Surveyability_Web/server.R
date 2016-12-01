
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library("plotly")
library("plyr")
library("dplyr")
library("ggplot2")
library("magrittr")
library("lubridate")
library("xlsx")
library("DT")
flowData<- read.xlsx("Daily_Precip_Discharge_Monitoring.xlsx", sheetName = "DailyGauge")
tripData<-read.xlsx("TripData.xlsx", sheetName = "TripDataFish")
tripData$DATE<-ymd(tripData$Date)


# flowData$DRY.LAM.CFS[flowData$DRY.LAM.CFS=="down"] <- NA
# flowData$MIL.School..ft..[flowData$MIL.School..ft..=="down"] <- NA
# flowData$MIL.Puccioni..in..[flowData$MIL.Puccioni..in..=="down"] <- NA
# flowData$FEL..in..[flowData$FEL..in..=="down"] <- NA
# flowData$MAR.ChristmasTree..in..[flowData$MAR.ChristmasTree..in..=="down"]<-NA
# flowData$WIL.3rdBridge..ft..[flowData$WIL.3rdBridge..ft..=="down"]<-NA
# flowData$MAA.Stage..ft..[flowData$MAA.Stage..ft..=="down"]<-NA
# flowData$SAN.Stage..ft..[flowData$SAN.Stage..ft..]<-NA
gaugeNames<-c(colnames(flowData))
gaugeNames<-gaugeNames[2:14]
flowData[gaugeNames]<- sapply(flowData[gaugeNames],as.numeric)
mergedData<- join(flowData, tripData, by = "DATE", "inner")
mergedData$Fishing<-as.factor(mergedData$Fishing)


#FilteredData<- mergedData %>% filter(REACHNAME == selectedReach)
#maxSurveyedCFS<- max(FilteredData[selectedGauge])

#GreenValleyPlot <- tripData %>% filter(REACHNAME == selectedReach)%>% ggplot(aes(x=DATE, y = FISHING)) + geom_point()+ scale_x_date()
#GreenValleyPlot + geom_line(data = flowData, aes(DATE, MIL.School..ft..))

surveyedReaches<-unique(tripData$ReachName)
surveyedReaches<- as.vector.factor(surveyedReaches)
surveyedReaches<-sort(surveyedReaches)

gaugeList <- colnames(flowData)
gaugeList <- gaugeList[-1]


shinyServer(function(input, output) {
  
  output$maxflow <-renderUI({
    FilteredData<- filter(mergedData, ReachName == input$Reach)
    maxSurveyedCFS<- max(FilteredData[input$Gauge], na.rm = TRUE)
    HTML(paste("<b> This reach has been surveyed at a maximum gauge height of:</b>", maxSurveyedCFS))
  })
  
  output$fishTable = renderDataTable({
    
    FishData<- filter(mergedData, ReachName == input$Reach)
    FishData<- FishData[,c("Date","ReachName", "Tributary", "Crew", "CohoIndividuals","SteelheadIndividuals","ChinookIndividuals", "SalmonidSpIndividuals", "CohoRedds", "SteelheadRedds", "ChinookRedds", "SalmonidSpRedds","Comments")]
    DT::datatable(FishData, colnames = c("Date","Reach", "Tributary", "Crew", "Coho","Steelhead","Chinook", "SalmonidSp", "Coho Redds", "Steelhead Redds", "Chinook Redds", "SalmonidSp Redds","Comments"), options = list(pagelength = 10, autoWidth = FALSE))
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

})
