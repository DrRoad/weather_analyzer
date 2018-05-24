library(dplyr)
library(ggplot2)
library(tidyr)
library(httr)
library(jsonlite)
library(stringr)

shinyServer(
	function(input, output){
		output$example1 <- renderText({"Sample Text 1"})
		output$example2 <- renderText({"Sample Text 2"})
	}
)