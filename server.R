library(dplyr)
library(ggplot2)
library(tidyr)
library(httr)
library(jsonlite)
library(stringr)

# Source any supplemental files below:
source("seattle_rain_analysis.R")

shinyServer(
	function(input, output){
		output$example1 <- renderText({"Sample Text 1"})
		output$example2 <- renderText({"Sample Text 2"})
		output$example3 <- renderText({"Sample Text 3"})
		output$example4 <- renderText({"Sample Text 4"})
	}
)