
library(shiny)
library(tmap)
library(leaflet)
library(googleVis)
library(DT)
library(ggmap)


function(input, output, session){
    
  observe({
    updateDateInput(
      session, "start",
#      value = avail_dates[1],
      min = avail_dates[1],
      max = avail_dates[length(avail_dates)])

    updateDateInput(
      session, "stop",
#      value = avail_dates[length(avail_dates)],
      min = input$start + 1,
      max = avail_dates[length(avail_dates)])
  
    if(input$stop < input$start ){
      updateDateInput(
        session, "stop",
        value = input$start+1,
        min = input$start+1,
        max = avail_dates[length(avail_dates)])

      }
  })
  
  filter_fct <- reactive({
    req(input$start, input$stop,
        input$start < input$stop
    )
    
    collision %>%
      filter(date >= input$start & date <= input$stop)
    
  })
  
  neighborhood_fct <- reactive({
    
    filter_fct() %>%
      group_by(neighborhood) %>% 
      summarise(number = n())
  })  
  
  
  output$plot = renderPlot({
    collision_points <- filter_fct() %>% 
      select(long, lat)
    
    ggplot() + geom_sf(data = neighborhood_df) +
      geom_point(data = collision_points, aes(x=long, y=lat)) 
    
    api_key <- read.csv('google_key')[1,1]
    register_google(key = api_key)  
    nyc_map <- get_map(location = c(lon = -73.92, lat = 40.72), maptype = "terrain", zoom = 11)
 
    ggmap(nyc_map) + 
      geom_point(data = collision_points, aes(x=long, y=lat), alpha=0.1, color = "Red")
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
        arrange(., desc(injured)) %>% 
        gvisBarChart(., 
                        options=list(
                          title = "Injured & Killed from Motor Collision",
                          width=600, height=400,
                          isStacked = "true"))
    })   
    
    
    
    
    
    output$time <- renderGvis({
      
      num_days<- filter_fct() %>% 
        select(date) %>% 
        unique() %>% 
        summarise(n=n())
        
       df <- filter_fct() %>% 
        group_by(., hour) %>% 
        summarise(Accidents=n()) %>% 
         mutate(Accidents = round(Accidents/num_days$n,1))
        
      hour_ticks <- c("00AM","01AM","02AM","03AM","04AM","05AM","06AM",
                      "07AM","08AM","09AM","10AM","11AM","12PM","01PM",
                      "02PM","03PM","04PM","05PM","06PM","07PM","08PM",
                      "09PM","10PM","11PM")
      df$hour <- hour_ticks[as.numeric(df$hour)+1]        
      
      gvisColumnChart(df, 
                        options = list(
                          title = "Accidents by Time",
                          legend="none",
                          width=600, height=300,
                          hAxes="[{title:'Time', titleTextStyle: {color: 'black'}}]",
                          vAxes="[{title:'Accidents', titleTextStyle: {color: 'black'}}]"
                          )
                        )
      })   
    
    
    
    holiday_fct <- reactive({
      date_to_comp <- holidays %>% 
        filter(date >= input$start & date <= input$stop) 
      
      num_holidays<-  date_to_comp %>% 
        group_by(holiday) %>% 
        summarise(n=n())
      
      holiday_df <- filter_fct() %>% 
        full_join(date_to_comp, id = date) %>%
        filter(!is.na(holiday)) %>% 
        group_by(holiday) %>% 
        summarise(accidents=n()) %>% 
        full_join(num_holidays, id=holiday) %>% 
        mutate(Accidents = round(accidents/n))  
      
      x <- date_to_comp %>% .$holiday %>% unique()
      
      holiday_df[match(x, holiday_df$holiday),] 
      
    })
    
    output$holiday <- renderGvis({
        gvisColumnChart(holiday_fct(), xvar = 'holiday', yvar = 'Accidents',
                        options = list(
                          title = "Accidents on Holidays",
                          legend="none",
                          width=600, height=300,
                          hAxes="[{title:'Holiday', titleTextStyle: {color: 'black'}}]",
                          vAxes="[{title:'Accidents', titleTextStyle: {color: 'black'}}]"
                        #  viewWindowMode:'explicit', viewWindow:{min:0, max:650}}]"
                         # viewWindow: {min:0, max:650}]"
                        )
        )
    })
    
  day_fct <- reactive({
    day_ticks <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    
    #weeks <- filter_fct() %>% 
    #  select(date) %>% 
    #  unique() %>% 
    #  summarise(num_weeks = n()/7) 
    weeks <- filter_fct() %>% 
      select(date, day) %>% 
      unique() %>% 
      group_by(day) %>% 
      summarise(num_weeks = n()) 
    
    filter_fct() %>% 
      group_by(day) %>% 
      summarise(Accidents = n()) %>% 
      # full_join(weeks, id=day) %>% 
      mutate(Accidents = round(Accidents/weeks$num_weeks), 
             day = day_ticks)
    
  })    
    
    
    output$day <- renderGvis({
        gvisColumnChart(day_fct(), xvar = 'day', yvar = 'Accidents' ,
                        options = list(
                          title = "Accidents on Day Week",
                          legend="none",
                          width=600, height=300,
                          hAxes="[{title:'Day Week', titleTextStyle: {color: 'black'}}]",
                          vAxes="[{title:'Accidents', titleTextStyle: {color: 'black'}}]"
                     #     viewWindowMode:'explicit', viewWindow:{min:0, max:650}
                          
                        )
        )
      

    }) 
    
  

    
    
    
    output$month <- renderGvis({
      months_ <- filter_fct() %>% 
        select(year,month) %>% 
        unique() %>% 
        group_by(month) %>% 
        summarise(n=n())
      
      mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
               "Aug", "Sep", "Oct", "Nov", "Dec")
      
      df <- filter_fct() %>% 
        group_by(month) %>% 
        summarise(accidents=n()) %>% 
        full_join(months_,id=month) %>% 
        filter(!is.na(accidents)) %>% 
        mutate(Accidents = round(accidents/n)) 

      df$month =  mon[as.numeric(df$month)]
      
        gvisColumnChart(df, xvar = "month", yvar = 'Accidents',
                        options = list(
                          title = "Accidents by the Month",
                          legend="none",
                          width=600, height=300,
                          hAxes="[{title:'Month', titleTextStyle: {color: 'black'}}]",
                          vAxes="[{title:'Accidents', titleTextStyle: {color: 'black'}}]"
                        )
        )
    })
    
    
}

