
library(shiny)
library(tmap)
library(leaflet)
library(googleVis)
library(DT)

function(input, output, session){
    
  observe({

      if(!grepl(input$year,input$start)){
        avail_dates <- collision %>% 
          filter(., year == input$year) %>% 
          .$date %>% 
          unique() %>% 
          sort()

        updateSelectizeInput(
          session, "start",
          choices = avail_dates,
          selected = avail_dates[1])
        

          updateSelectizeInput(
            session, "stop",
            choices = avail_dates,
            selected = avail_dates[length(avail_dates)])
        
        
      }
    if (input$start > input$stop ){
      avail_dates <- collision %>% 
        filter(., date > input$start & year == input$year) %>% 
        .$date %>% 
        unique() %>% 
        sort()
      updateSelectizeInput(
        session, "stop",
        choices = avail_dates,
        selected = avail_dates[length(avail_dates)])
      
    }
  })
  
  filter_fct <- reactive({
    req(input$start, input$stop, input$year,
        input$start < input$stop,
        grepl(input$year, input$start),  #start, stop should be in the same year as var(year)
        grepl(input$year, input$stop)
    )
    
    collision %>%
      filter(date >= input$start & date <= input$stop)
    
  })
  
  neighborhood_fct <- reactive({
    
    filter_fct() %>%
      group_by(neighborhood) %>% 
      summarise(number = n())
  })  
  

  output$map = renderLeaflet({
    new_df<- neighborhood_df
    new_df@data <- new_df@data %>% full_join(., neighborhood_fct(), id = neighborhood) %>% 
      mutate(., number=ifelse(is.na(number), 0 ,number))
    
    
    tm <- tm_shape(new_df, name="collision number in NYC neighborhood", simplify = 0.5) +
      tm_polygons("number", title = "collisions") 
      #tm_layout(basemaps = c('OpenStreetMap'))
    
    tmap_leaflet(tm) %>% leaflet::setView(lng=-73.9, lat=40.7, zoom=10)
  })

  output$hist <- renderPlot(
    neighborhood_fct() %>% 
      top_n(30) %>% 
      ggplot(aes(x = reorder(neighborhood, -number), y = number)) +
      geom_histogram(stat='identity', fill = "Grey45") +
      labs(title = "Number of Accidents by Neighborhood",
           x = "Neighborhoods",
           y = "Accident Number")+
      theme_bw() +
      theme(text = element_text(size=14, colour = "Black")) +
      theme(axis.text.x = element_text(angle=60, hjust=1))
  )
  output$table <- DT::renderDataTable({
    df <- filter_fct() %>%
      group_by(., contributing.factor.vehicle.1,
               contributing.factor.vehicle.2) %>% 
      summarise(n=n()) 
    
    df1<- df %>% 
      group_by(., contributing.factor.vehicle.1) %>% 
      summarise(number_1 = sum(n)) %>% 
      rename(., contributing.factor = contributing.factor.vehicle.1)
    
    df2 <- df %>% 
      group_by(., contributing.factor.vehicle.2) %>% 
      summarise(number_2 = sum(n)) %>% 
      rename(., contributing.factor = contributing.factor.vehicle.2)
    
    df <- df1 %>% left_join(., df2, id = contributing.factor ) %>% 
      filter(., contributing.factor != "unspecified") %>% 
      mutate(., number_2 = ifelse(is.na(number_2), 0, number_2)) %>% 
      mutate(., number = number_1+ number_2, 
             total = sum(number), 
             percent = round((100 * number/total), 1)) %>% 
      arrange(., desc(percent))
    
    df<- data.frame(cbind(1:nrow(df)), df$contributing.factor,
                    df$percent)
    
    names(df)<-c("Rank", "Contributing Factor", "percent")
      
    datatable(df, rownames=FALSE )
  })
    output$casualty <- renderGvis({
      
        filter_fct() %>% rename(., injured = number.of.persons.injured,
                                      killed = number.of.persons.killed) %>% 
          group_by(neighborhood, injured, killed) %>% 
          summarise(num_killed = sum(killed), num_injured=sum(injured)) %>% 
          group_by(neighborhood) %>% 
          summarise(injured=sum(num_injured), killed=sum(num_killed)) %>% 
        top_n(20, injured) %>% 
        arrange(., injured) %>% 
        gvisBarChart(., 
                        options=list(
                          title = "Injured & Killed from Motor Collision",
                          width=600, height=400))
      
      
    })   
    
    output$time <- renderGvis({
      
      df <- filter_fct() %>% 
#        mutate(hour = strftime(time, "%H")) %>% 
        group_by(., hour) %>% 
        summarise(Accidents=n()) %>% 
        arrange(hour) 
      hour_ticks <- c("00AM","01AM","02AM","03AM","04AM","05AM","06AM",
                      "07AM","08AM","09AM","10AM","11AM","12PM","01PM",
                      "02PM","03PM","04PM","05PM","06PM","07PM","08PM",
                      "09PM","10PM","11PM")
      df$hour <- hour_ticks[as.numeric(df$hour)+1]        
      gvisColumnChart(df, 
                        options = list(
                          title = "Accidents by Time",
                          width=600, height=300,
                          hAxes="[{title:'Time', titleTextStyle: {color: 'black'}}]",
                          vAxes="[{title:'Accidents', titleTextStyle: {color: 'black'}}]"
                          )
                        )
      })   
    output$date <- renderGvis({
      
      df <- filter_fct() %>% 
        group_by(., day) %>% 
        summarise(Accidents=n()) %>% 
        arrange(day) 
      
      day_ticks <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
      df$day <- day_ticks[as.numeric(df$day)]
      
      gvisColumnChart(df,
                        options = list(
                          title = "Accidents by Day of the Week",
                          width=600, height=300,
                          hAxes="[{title:'Day of the Week', titleTextStyle: {color: 'black'}}]",
                          vAxes="[{title:'Accidents', titleTextStyle: {color: 'black'}}]"
                        )
        )
    }) 
    
    output$month <- renderGvis({
      
      df <- filter_fct() %>% 
        group_by(., month) %>% 
        summarise(Accidents = n()) %>% 
        arrange(month)   
      
      mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
               "Aug", "Sep", "Oct", "Nov", "Dec")
      df$month =  mon[as.numeric(df$month)]
      
        gvisColumnChart(df, xvar = "month", yvar = 'Accidents',
                        options = list(
                          title = "Accidents by the Month",
                          width=600, height=300,
                          hAxes="[{title:'Month', titleTextStyle: {color: 'black'}}]",
                          vAxes="[{title:'Accidents', titleTextStyle: {color: 'black'}}]"
                        )
        )
    })
    
    
}

