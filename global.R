library(tidyverse)
library(rgdal)
#library(maptools)
#library(gpclib)


# downloading nyc neighborhood boundary dataset 


#download.file('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson',
#             'nybb.geojson' 
#)

#loading neighborhood boundary dataset
neighborhood_df <- readOGR(dsn="./nybb.geojson", layer="nybb") 

collision <- read_csv("collision_2016_aft.csv")


holidays <- read_csv('./holidays.csv') %>% 
  mutate(date = as.Date(date, "%Y-%m-%d")) 
  

initial_year <- collision %>% 
  select(., year) %>% 
  unique() %>% 
  .$year

avail_dates <- collision %>% 
  #filter(., year==initial_year[1]) %>% 
  select(., date) %>% 
  unique() %>% 
  .$date %>% 
  sort()

filtered_collision<- collision[1,]





  
  
  

