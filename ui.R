library(shiny)
library(tmap)
library(leaflet)
library(tidyverse)
library(DT)




fluidPage(
  titlePanel("NYC Motor Vehicle Collision Reports"),
  sidebarLayout(
    sidebarPanel(
      img(src="r.png",width="20%"),
      

      dateInput(inputId = "start",
                     label = h3("Start Date"),
                      value = as.Date('2019-01-01', format("%Y-%m-%d"))),
                        
      dateInput(inputId = "stop",
                     label = h3("End Date"),
                     value = avail_dates[length(avail_dates)])
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
                 fluidRow(
                   leafletOutput("map")),
                 fluidRow(
                   plotOutput("hist"),height=150)
        ),
        tabPanel("Plot", 
                 fluidRow( 
                   plotOutput("plot"), heigh=400)
        ),
        
        tabPanel("Cause", 
                   DT::dataTableOutput("table"))
        ,
        tabPanel("Casauties", 
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
                   htmlOutput("month"), heigh=400)
                 ),
        tabPanel("Time", 
                 fluidRow( 
                   htmlOutput("time"), heigh=400)
        )
        
      )
    )
  )
)

