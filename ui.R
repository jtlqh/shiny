library(shiny)
library(leaflet)
library(DT)
library(shinydashboard)



dashboardPage(
  dashboardHeader( title = "NYC Auto Collision"),
    #title = "Li Dashboard"),
  dashboardSidebar(
 #   sidebarUserPanel("Li",image="r.png"),
    img(src="10crash1.600.jpg",width="100%"),
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Cause", tabName ="cause", icon = icon("key")),
      menuItem("Casualty", tabName = "casualty",  icon = icon("briefcase-medical")),
      menuItem("Day", tabName = "day", icon = icon("calendar-times")),
      menuItem("Month", tabName = "month", icon = icon("calendar-alt")),
      menuItem("Time", tabName = "time",  icon = icon("hourglass")),
      menuItem("Plot", tabName = "plot", icon = icon("image"))
    ),
    dateRangeInput("dates", label = "Select Date Range for Display",
                       start = '2019-01-01',
                       end = "2019-01-31",
                       min = "2016-01-01",
                       max = "2019-03-12")
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                leafletOutput("map") ),
              fluidRow(
                plotOutput("hist"))
      ),
      
      tabItem(tabName = "cause", 
              DT::dataTableOutput("table")
      ),
      tabItem(tabName = "casualty", 
              htmlOutput("casualty")
      ),
      tabItem(tabName = "day", 
              fluidRow(
                htmlOutput("day")),
              fluidRow(
                htmlOutput("holiday"))
      ),
      tabItem(tabName = "month", 
              fluidRow(
                htmlOutput("month"))
      ),
      tabItem(tabName = "time", 
              fluidRow( 
                htmlOutput("time"))
      ),
      tabItem(tabName = "plot", 
              fluidRow( 
                plotOutput("plot", heigh=600,  width=600)),
              fluidRow( 
                p("Limited to the first 2000 points",align="center"))
      )
      
    )
  )
)

