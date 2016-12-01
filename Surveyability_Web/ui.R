
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library("xlsx")
library(plotly)
library("DT")
flowData<- read.xlsx("Daily_Precip_Discharge_Monitoring.xlsx", sheetName = "DailyGauge")
tripData<-read.xlsx("TripData.xlsx", sheetName = "TripDataFish")
surveyedReaches<-unique(tripData$ReachName)
surveyedReaches<- as.vector.factor(surveyedReaches)
surveyedReaches<-sort(surveyedReaches)
gaugeList <- colnames(flowData)
gaugeList <- gaugeList[-1]

shinyUI(fluidPage(
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
      plotlyOutput("HydroGraphPlot"),
      htmlOutput("maxflow"),
      DT::dataTableOutput("fishTable")
      )
    
    
  )
))
