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



# loading motor vehicle collision dataset, 
# this file can be downloaded from https://data.cityofnewyork.us/Public-Safety/NYPD-Motor-Vehicle-Collisions/h9gi-nx95
raw.df <- read_csv("NYPD_Motor_Vehicle_Collisions.csv")

# cleaning up collision dataset
new_cols <- names(raw.df) %>% gsub(" ", ".", .) %>% tolower() 
names(raw.df) <- new_cols  
raw.df<-raw.df %>% mutate(., date = as.Date(date, "%m/%d/%Y")) %>% 
  filter(., !is.na(latitude) & !is.na(longitude)) %>% 
  filter(., longitude > -74.6 & longitude < -73.6) %>% 
  filter(., latitude > 40.4 & latitude < 41.0)

# map collision location to NYC neighborhood boundary
acc_loc<- raw.df %>% 
  select(., long=longitude, lat=latitude)
sps <- acc_loc
coordinates(sps) <- ~long + lat
proj4string(sps) <- proj4string(neighborhood_df)
acc_loc <- cbind(acc_loc, over(sps, neighborhood_df))


collision <- raw.df %>% 
  select(., 
         date,
         time,
         number.of.persons.injured,
         number.of.persons.killed,
         contributing.factor.vehicle.1,
         contributing.factor.vehicle.2,
         unique.key)  
  

collision <-bind_cols(collision, acc_loc) %>% 
  filter(., !is.na(borough)) %>% 
  mutate(., number.of.persons.injured = 
           ifelse(is.na(number.of.persons.injured),
                  0,
                  number.of.persons.injured)) %>% 
  mutate(., number.of.persons.killed = 
           ifelse(is.na(number.of.persons.killed),
                  0,
                  number.of.persons.killed)) %>% 
  mutate(., contributing.factor.vehicle.1 = tolower(
    ifelse(is.na(contributing.factor.vehicle.1),
           "unspecified",
           ifelse(contributing.factor.vehicle.1 == "1",
                  "unspecified",
                  ifelse(contributing.factor.vehicle.1 == "80",
                         "unspecified",
                         contributing.factor.vehicle.1)
           )))) %>%  
 
  mutate(., contributing.factor.vehicle.2 = tolower(
    ifelse(is.na(contributing.factor.vehicle.2),
           "unspecified",
           ifelse(contributing.factor.vehicle.2 == "1",
                  "unspecified",
                  ifelse(contributing.factor.vehicle.2 == "80",
                         "unspecified",
                         contributing.factor.vehicle.2)
           )))) %>% 
 
  mutate(., year = format(as.Date(date), "%Y")) %>% 
  mutate(., month = format(as.Date(date), "%m")) %>% 
  mutate(., day = format(as.Date(date), "%u")) %>% 
  mutate(., hour = strftime(time, "%H"))

initial_year <- collision %>% 
  select(., year) %>% 
  unique() %>% 
  .$year

initial_date <- collision %>% 
  filter(., year==initial_year[1]) %>% 
  select(., date) %>% 
  unique() %>% 
  .$date %>% 
  sort()





  
  
  

