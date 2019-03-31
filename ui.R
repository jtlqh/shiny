library(shiny)
library(tmap)
library(leaflet)
library(tidyverse)
library(DT)




fluidPage(
  titlePanel("NYC Motor Vehicle Collision Reports"),
  sidebarLayout(
    sidebarPanel(
      img(src="r.jpg",width="30%"),
      

      dateInput(inputId = "start",
                     label = h3("Start Date"),
                      value = as.Date('2019-01-01', format("%Y-%m-%d"))),
                        
      dateInput(inputId = "stop",
                     label = h3("End Date"),
                     value = avail_dates[length(avail_dates)])
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Neighborhoods",
                 fluidRow(
                   leafletOutput("map")),
                 fluidRow(
                   plotOutput("hist"))
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
                   htmlOutput("month"), heigh=160)
                 ),
        tabPanel("Time", 
                 fluidRow( 
                   htmlOutput("time"), heigh=160)
                 
                 
        )
        
      )
    )
  )
)

