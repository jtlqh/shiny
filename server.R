
library(shiny)
library(tmap)
library(leaflet)
library(googleVis)

function(input, output, session){
    
  observe({

      if (!grepl(input$year,input$start)){
        print("start inside")
        avail_dates <- collision %>% 
          filter(., year == input$year) %>% 
          arrange(date) %>% 
          .$date %>% unique()
          
        print(head(avail_dates))
        updateSelectizeInput(
          session, "start",
          choices = avail_dates,
          selected = avail_dates[1])
        

          updateSelectizeInput(
            session, "stop",
            choices = avail_dates,
            selected = avail_dates[1])
        
        
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
    
    
    tm <- tm_shape(new_df, name="collisions") + tm_polygons("number", title = "collisions")
    tmap_leaflet(tm)
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
  
  output$factor1 <- renderPlot(
    filter_fct() %>% 
      group_by(contributing.factor.vehicle.1) %>% 
      summarise(number = n()) %>% 
      top_n(20) %>% 
      ggplot()+
      geom_col(aes(x=reorder(contributing.factor.vehicle.1, -number),
                   y=number), fill = "darkGreen")+
      theme_bw() +
      theme(text = element_text(size=16, colour="Black"))+
      theme(axis.text.x = element_text(angle=60, hjust=1)) +
      labs(title = "Contributing factor by vehicle 1",
           y= "Number of Accidents",
           x="")
  )
    output$factor2 <- renderPlot(
      filter_fct() %>% 
        group_by(contributing.factor.vehicle.2) %>% 
        summarise(number = n()) %>% 
        top_n(10) %>% 
        ggplot()+
        geom_col(aes(x=reorder(contributing.factor.vehicle.2, -number),
                     y=number), fill = "darkGreen")+
        theme_bw() +
        theme(text = element_text(size=16, colour="Black")) +
        theme(axis.text.x = element_text(angle=60, hjust=1))+
        labs(title = "Contributing factor by vehicle 2",
             y= "Number of Accidents",
             x="")
  )
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
    
    
}

#dat=data.frame(A=rpois(100, 20),
#               B=rpois(100, 5),
#               C=rpois(100, 50))
#plot( 
#  gvisHistogram(dat, options=list(
#    legend="{ position: 'top', maxLines: 2 }",
#    colors="['#5C3292', '#1A8763', '#871B47']",
#    width=600),
#    chartid="Histogram")
#)
