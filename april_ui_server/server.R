library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny)
library(jsonlite)


source('apr_data.R')

apr_server <- function(input, output){

  reactive_input <- reactive({
    humi_temp %>% 
     filter(city== input$city) %>% 
     filter(date== as.character(input$date))
  })
  
  output$humi_temp_point <- renderPlot({
  
   
  
  ggplot(reactive_input(), aes(x= humidity, y= temperature, color = time))+
        geom_point() +
        labs(title = "Relationship of humidity and temperature",
             x = "humidity",
             y = "temperature")
    
  })
  
  output$text <- renderText({
    df <- reactive_input()
   text <- paste("On", as.character(input$date),",", 
          "the averages of humidity and temperature of the city,", as.character(input$city), ", are", 
          mean(df$humidity),"and", mean(df$temperature),".", "By looking at the independent valuable on x-axis, 
          we are able to get the corresponding y value and receive information of the specfic time of the day 
          by referring to the color bar.",
          "Furthermore, the scatter plot enables us to 
          analyze the patterns of the relationship between two valuables that suggests us to find out 
          whether there are unusual features such as outliers, clusters and gaps exist in the data sets.") 
    text
  })
  
}
shinyServer(apr_server)