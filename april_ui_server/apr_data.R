library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny)

#load humidity and temperature.csv
humidity <- read.csv("humidity.csv")
#View(humidity)
temperature <- read.csv("temperature.csv")
#View(temperature)


#get long humidity data for US cities
humidity <- humidity %>% 
  subset(select = -c(Vancouver, Toronto, Montreal, Beersheba, Tel.Aviv.District, Eilat, Haifa,Nahariyya, Jerusalem)) %>% 
  gather(city,humidity, Portland:Boston) %>% 
  separate(datetime, c("date", "time"),sep = " ") %>% 
  na.omit()

    

#get long temperature data for US cities
temperature <- temperature %>% 
  subset(select = -c(Vancouver, Toronto, Montreal,Beersheba, Tel.Aviv.District, Eilat, Haifa,Nahariyya, Jerusalem)) %>% 
  gather(city,temperature, Portland:Boston) %>% 
  separate(datetime, c("date", "time"),sep = " ") %>% 
  na.omit()

#join two data
humi_temp<- left_join(humidity,temperature, by = c('date', 'time', 'city'))
#View(humi_temp)

