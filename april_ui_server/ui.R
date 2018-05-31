library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny)

source('apr_data.R')

#make a fluidpage
apr_ui <- fluidPage(
#header panel
  titlePanel("US City Weather Data 2012-2017"),
  
  p("The scatter plot based on the hourly measurement data of two weather attributes, 
    humidity and temperature for 27 US cities, showing the relationship between 
    humidity and temperature on x-axis and y-axis. The specific time of the chosen date and city 
    is characterized with different color.  "),
  
#sidebar panel  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("city",
                  "Select a city", choice = unique(humi_temp$city)),
      
      p("The date range is: Oct 1, 2012 - Nov 30, 2017"),
      
      dateInput("date", "Select or type in a date", 
                min = as.Date("2012-10-01","%Y-%m-%d"),
                max = as.Date("2017-11-30","%Y-%m-%d"),
                value=as.Date("2012-12-01"))
                
    ),
    
    mainPanel(
     
      plotOutput("humi_temp_point"), #width = 400, height = 300
    textOutput("text")
      )
  )
)

shinyUI(apr_ui)
