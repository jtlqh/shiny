library(shiny)
library(tmap)
library(leaflet)
library(DT)




fluidPage(
  titlePanel("NYC Motor Vehicle Collision Reports"),
  sidebarLayout(
    sidebarPanel(
      img(src="r.png",width="20%"),
      
      p(
        br()),
      
      
      dateRangeInput("dates", label = "Please Select Date Range:",
                     start = '2019-01-01',
                     end = "2019-01-31",
                     min = "2015-01-01",
                     max = "2019-03-4"),
      p("Interactive date range from Jan 2016 to Mar 2019")

      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
                 fluidRow(
                   leafletOutput("map")),
                 fluidRow(
                   plotOutput("hist"))
        ),
        
        tabPanel("Cause", 
                   DT::dataTableOutput("table"))
        ,
        tabPanel("Casualty", 
                 htmlOutput("casualty")
        ),
        tabPanel("Day", 
                 
                 fluidRow(
                   htmlOutput("day"), heigh=200),
                 fluidRow(
                   htmlOutput("holiday"), heigh=200)
        ),
        tabPanel("Month", 
                 fluidRow(
                   htmlOutput("month"))
                 ),
        tabPanel("Time", 
                 fluidRow( 
                   htmlOutput("time"))
        ),
        tabPanel("Plot", 
                 fluidRow( 
                   plotOutput("plot"), heigh=400),
                 fluidRow( 
                   p("Limited to the first 2000 points",align="center"))
        )
      )
    )
  )
)

