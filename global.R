
library(tidyverse)
library(sf)


dbname = "./collisions.sqlite"
tblname = "collisions"

#download.file(url='http://jt10000.com/lqh/collisions.sqlite',
#              mode='wb',
#              destfile=dbname 
#)

source("./helpers.R")

# downloading nyc neighborhood boundary dataset 
download.file('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson',
             './nybb.geojson' 
)


neighborhood_df <- st_read(dsn="./nybb.geojson") 



holidays <- read_csv('holidays.csv') %>% 
  mutate(date=as.numeric(date))  






  
  
  

