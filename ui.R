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
      
      selectizeInput(inputId = "year",
                     label = h3("Select Year"),
                     choices = initial_year),
      
      dateInput(inputId = "start",
                     label = h3("Start Date"),
                      value = initial_date),
                        
      dateInput(inputId = "stop",
                     label = h3("End Date"),
                     value = initial_date[length(initial_date)])
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("By Neighborhoods",
          fluidRow(
            leafletOutput("map")),
          fluidRow(
            plotOutput("hist"))
        ),
        tabPanel("By Cause", 
                   DT::dataTableOutput("table"))
        ,
        tabPanel("By casauties", 
                 htmlOutput("casualty")
        ),
        tabPanel("By Month/Date/Time", 
          fluidRow( 
              htmlOutput("time"), heigh=160),
          fluidRow(
              htmlOutput("date"), heigh=160),
          fluidRow(
              htmlOutput("month"), heigh=160)
          )
      )
    )
  )
)


