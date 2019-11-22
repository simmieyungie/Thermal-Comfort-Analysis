#My Undergraduate thesis
library(tidyverse)
## from the start
#Data compilation

#Filter for each location in the New dataset
ikeja <- New %>%
  select(Date, temp  = T.Ikeja, RH = RH.Ikeja) %>% 
  mutate(site = "Ikeja")

ikorodu <-  New %>% 
  select(Date, temp = T.IK, RH = RH.IK) %>% 
  mutate(site = "Ikorodu")

Lagos_Island <-  New %>% 
  select(Date, temp = T.LI, RH = RH.LI) %>% 
  mutate(site = "Lagos Island")

#rowbind a;; location data
full_data <- rbind(ikeja, ikorodu, Lagos_Island)


##COnvert to date the date column
full_data$Date <- as.Date(full_data$Date, "%m/%d/%Y")

#filter full data for 1989 - 2018 which is our Interest years
full_data <- full_data %>% 
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>% 
  filter(year %in% 1989:2018) %>% 
  select(-month, -year)

#Mutate Thermal comfort values using the index equation
project <- full_data %>% 
  select(Date, site, temp, RH) %>% 
  mutate(comfort = 0.8 * temp + ((RH * temp)/500))


#COnvert to date
project$Date <- as.Date(project$Date, "%m/%d/%Y")
