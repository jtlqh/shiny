library(shiny)
library(tmap)
library(leaflet)



fluidPage(
  titlePanel("NYC Auto Vehicle Collision Reports"),
  sidebarLayout(
    sidebarPanel(
      img(src="r.jpg",width="30%"),
      
      selectizeInput(inputId = "year",
                     label = "Select Year",
                     choices = unique(
                       format(as.Date(collision$date), "%Y"))),
      
      selectizeInput(inputId = "start",
                     label = "Seletect Date From",
                      choices = unique(collision$date[collision$date > "2018-12-31"])),                   
                     
      
      selectizeInput(inputId = "stop",
                     label = "Select Date To",

                     choices = unique(collision$date[collision$date > "2018-12-31"]))
      
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
                 fluidRow(
                   plotOutput("factor1")),
                 fluidRow(
                   plotOutput("factor2"))
        ),
        tabPanel("By casauties", 
                 #leafletOutput("casualty")
                 htmlOutput("casualty")
        )
      )
    )
  )
)


