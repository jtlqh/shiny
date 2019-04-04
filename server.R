library(RSQLite)
library(data.table)
library(shiny)
library(tmap)
library(leaflet)
library(googleVis)
library(DT)
library(ggmap)



function(input, output, session){
  conn <- dbConnector(session, dbname = dbname)
  
  observe({
    if (input$dates[1] >= input$dates[2]){
      showNotification(h4("End Date must be larger than Start Date in Date Range Field"), 
                       duration = 5,
                       closeButton = T)
    }
  })
  

  filter_fct <- reactive({
    req(input$dates, 
        input$dates[1] < input$dates[2]
    )
    
    dbGetData(conn = conn,
              tblname = tblname,
              start_date = input$dates[1],
              end_date = input$dates[2])  
  })
  
  neighborhood_fct <- reactive({
    
    filter_fct() %>%
      group_by(neighborhood) %>% 
      summarise(number = n())
  })  
  
  
  
  output$plot = renderPlot({
    
    collision_points <- filter_fct() %>% 
      select(long, lat)
    n <- 2000
    if (nrow(collision_points) > n) {
      collision_points <- collision_points[1:n,]
    } 
    

    api_key <- read.csv('google_key')[1,1]
    register_google(key = api_key)  
    nyc_map <- get_map(location = c(lon = -73.92, lat = 40.72), maptype = "terrain", zoom = 11)
    ggmap(nyc_map) + 
      geom_point(data = collision_points, aes(x=long, y=lat), alpha = 0.3, color='Red')+
      ggtitle( "Collision Locations") +
      theme(plot.title = element_text(hjust = 0.5)) #centering title
  })

  
  
  
  output$map = renderLeaflet({
    new_df<- neighborhood_df
    #new_df@data <- new_df@data %>% full_join(., neighborhood_fct(), id = neighborhood) %>%
    new_df <- new_df %>% full_join(., neighborhood_fct(), id = neighborhood) %>% 
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
      mutate( hour = format(as.POSIXct.numeric(time, origin = "1970-01-01", tz="EDT"), "%H")) %>% 
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
   req(input$dates, 
       input$dates[1] < input$dates[2]
   )
   
   holidays %>% 
     filter(date >= input$dates[1] & date <= input$dates[2]) 
   
 })   
        
    
  output$holiday <- renderGvis({
    
    num_holidays <-  holiday_fct() %>% 
      group_by(holiday) %>% 
      summarise(n=n())
    
    holiday_df <- filter_fct() %>% 
      full_join(holiday_fct(), id = date) %>%
      filter(!is.na(holiday)) %>% 
      group_by(holiday) %>% 
      summarise(accidents=n()) %>% 
      full_join(num_holidays, id=holiday) %>% 
      mutate(Accidents = round(accidents/n))  
    
    x <- holiday_fct() %>% .$holiday %>% unique()
    
    holiday_df <- holiday_df[match(x, holiday_df$holiday),] 

    gvisColumnChart(holiday_df, xvar = 'holiday', yvar = 'Accidents' ,
                    options = list(
                      title = "Accidents on Holidays",
                      legend="none",
                      width=600, height=300,
                      hAxes="[{title:'', titleTextStyle: {color: 'black'}}]",
                      vAxes="[{title:'Accidents', titleTextStyle: {color: 'black'}}]"
                      #     viewWindowMode:'explicit', viewWindow:{min:0, max:650}
                      
                    )
    )
                    
  })
  
  
    
    
    
  output$day <- renderGvis({
    day_ticks <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    
    collision_df <- filter_fct() %>% 
      mutate(day=as.numeric(format(as.Date(date,origin = "1970-01-01"),"%u"))) #add a column for dayweek
      
    weeks <- collision_df %>% 
      select(date, day) %>% 
      unique() %>% 
      group_by(day) %>% 
      summarise(num_weeks = n()) 
    
    df <- collision_df %>% 
      group_by(day) %>% 
      summarise(Accidents = n()) %>% 
      # full_join(weeks, id=day) %>% 
      mutate(Accidents = round(Accidents/weeks$num_weeks))
    df$day =  day_ticks[df$day]
             
    
    gvisColumnChart(df, xvar = 'day', yvar = 'Accidents' ,
                    options = list(
                      title = "Accidents on Day Week",
                      legend="none",
                      width=600, height=300,
                      hAxes="[{title:'', titleTextStyle: {color: 'black'}}]",
                      vAxes="[{title:'Accidents', titleTextStyle: {color: 'black'}}]"
                      #     viewWindowMode:'explicit', viewWindow:{min:0, max:650}
                      
                    )
    )
  }) 
  
  

  
  
    
    
  output$month <- renderGvis({
    collision_df <- filter_fct() %>% 
      mutate(month=as.numeric(format(as.Date(date, origin = "1970-01-01"),"%m")),
             year=format(as.Date(date, origin = "1970-01-01"), "%Y"))
    
    months_ <- collision_df %>% 
      select(year,month) %>% 
      unique() %>% 
      group_by(month) %>% 
      summarise(n=n())
    
    mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
             "Aug", "Sep", "Oct", "Nov", "Dec")
    
    df <- collision_df %>% 
      group_by(month) %>% 
      summarise(accidents = n()) %>% 
      full_join(months_,id = month) %>% 
      filter(!is.na(accidents)) %>% 
      mutate(Accidents = round(accidents/n)) 
    
    df$month =  mon[df$month]
    
    gvisColumnChart(df, xvar = "month", yvar = 'Accidents',
                    options = list(
                      title = "Accidents by the Month",
                      legend="none",
                      width=600, height=300,
                      hAxes="[{title:'', titleTextStyle: {color: 'black'}}]",
                      vAxes="[{title:'Accidents', titleTextStyle: {color: 'black'}}]"
                    )
    )
  })
  
    
}

