library(tidyverse)
library(shiny)
library(httr)
library(jsonlite)
library(lubridate)
library(plotly)
library(shinyjs)

# Source supplemental files below:
source("seattle_rain_analysis.R")

shinyServer(
	function(input, output, session){
		output$example1 <- renderText({"Sample Text 1"})
		output$example2 <- renderText({"Sample Text 2"})
		output$example3 <- renderText({"Sample Text 3"})
		output$example4 <- renderText({"Sample Text 4"})
		
		output$sea_df <- renderDataTable(
			{seattle_rain_df},
			options = list(
				pageLength = 11
			)
		)
		
		output$sea_df_description <- renderUI({
			tagList(
				"This part of the Weather Analyzer uses a ",
				a("dataset",
					href = "https://www.kaggle.com/rtatman/did-it-rain-in-seattle-19482017"
				),
				"from ",
				a("Kaggle.",
					href = "https://www.kaggle.com"
				),
				"The dataset includes", format(nrow(seattle_rain_df), big.mark=",", trim=TRUE),
				"consecutive days of weather data for Seattle, ",
				"from 1948 to 2017. For each day, the provided data columns are: Date, ",
				"Inches of Precipitation, Maximum Temperature, Minimum Temperature, and ",
				"Did Rain (true/false, depending on whether or not it rained that day).",
				br(), br(),
				"The original data, without modification or processing, is viewable to",
				" the right. Feel free to search specific records, change the page, show",
				"more entries, or sort any column in increasing or decreasing order."
			)
		})
		
		sea_df <- reactive({
			df <- seattle_rain_df %>% filter(
				cd >= input$sea_day_dates[1],
				cd <= input$sea_day_dates[2]
			)
			return(df)
		})
		
		sampled <- reactive({
			sample_n(sea_df(), input$sea_day_samplesize)
		})
		
		sea_df_stats <- reactive({
			smoothness <- input$sea_day_span
			
			df <- sea_df() %>% group_by(cd) %>% summarize(
				line_min_Precipitation = min(Precipitation),
				line_mean_Precipitation = mean(Precipitation),
				line_max_Precipitation = max(Precipitation),
				line_min_Min_Temp = min(Min_Temp),
				line_mean_Min_Temp = mean(Min_Temp),
				line_mean_Max_Temp = mean(Max_Temp),
				line_max_Max_Temp = max(Max_Temp)
			) %>% rowwise() %>% mutate(
				line_mean_Temp = 0.5*(line_mean_Min_Temp+line_mean_Max_Temp)
			)
			
			df$index <- 1:nrow(df)
			
			df$spline_min_Precipitation = predict(loess(
				line_min_Precipitation ~ index,
				data = df,
				span = smoothness
			))
			
			df$spline_mean_Precipitation = predict(loess(
				line_mean_Precipitation ~ index,
				data = df,
				span = smoothness
			))
			
			df$spline_max_Precipitation = predict(loess(
				line_max_Precipitation ~ index,
				data = df,
				span = smoothness
			))
			
			df$spline_min_Min_Temp = predict(loess(
				line_min_Min_Temp ~ index,
				data = df,
				span = smoothness
			))
			
			df$spline_mean_Min_Temp = predict(loess(
				line_mean_Min_Temp ~ index,
				data = df,
				span = smoothness
			))
			
			df$spline_mean_Temp = predict(loess(
				line_mean_Temp ~ index,
				data = df,
				span = smoothness
			))
			
			df$spline_mean_Max_Temp = predict(loess(
				line_mean_Max_Temp ~ index,
				data = df,
				span = smoothness
			))
			
			df$spline_max_Max_Temp = predict(loess(
				line_max_Max_Temp ~ index,
				data = df,
				span = smoothness
			))
			
			return(df)
		})
		
		sea_df_rain <- reactive({
			df <- sea_df() %>% group_by(cd) %>% summarize(
				pct_rainy_days = sum(Did_Rain) / length(Did_Rain)
			)

			df$index <- 1:nrow(df)
			df$smooth <- predict(
				loess(
					pct_rainy_days ~ index,
					data = df,
					span = input$sea_day_span
				)
			)
			
			return(df)
		})
		
		output$sea_day_plot <- renderPlotly({
			datasource <- input$sea_day_data
			show <- input$sea_day_show
			features <- input$sea_day_features
			
			if(datasource == "Rain"){
				plot <- plot_ly(
					sea_df_rain(),
					connectgaps = TRUE
				)
				
				point_color  <- "#524C84"
				line_color   <- "#7DD1D1"
				spline_color <- "#D65D7A"
				bg_color   <- "#f2f2f2"
				
				if("point" %in% features){
					plot <- plot %>% add_trace(
						x = ~cd,
						y = ~pct_rainy_days,
						type = "scatter",
						name = "Points",
						mode = "markers",
						marker = list(
							opacity = 0.35,
							size = 17,
							color = point_color
						)
					)
				}
				
				if("line" %in% features){
					plot <- plot %>% add_lines(
						x = ~cd,
						y = ~pct_rainy_days,
						name = "Jagged Line",
						line = list(
							shape = "linear",
							width = 3,
							color = line_color
						)
					)
				}
				
				if("spline" %in% features){
					plot <- plot %>% add_lines(
						x = ~cd,
						y = ~smooth,
						name = "Smoothed Line",
						line = list(
							shape = "spline",
							width = 3,
							color = spline_color
						)
					)
				}
				
				return(
					plot %>%
					layout(
						title = "<b>Historical Frequency of Rain per Day of the Year</b>",
						yaxis = list(
							tickformat = "%",
							title = "<b>Frequency</b>",
							showgrid = TRUE,
							zeroline = FALSE,
							showline = TRUE
						),
						xaxis = list(
							zeroline = FALSE,
							showline = TRUE,
							showgrid = TRUE,
							tickformat = "%b %d",
							tick0 = "2000-01-01",
							dtick = "M1",
							title = "<b>Day of Year</b>",
							nticks = 13
						),
						showlegend = TRUE,
						legend = list(
							orientation = "h",
							x = 0.5,
							xanchor = "center",
							y = -0.2,
							bgcolor = bg_color,
							traceorder = "reversed"
						),
						plot_bgcolor = bg_color
					)
				)
			} else {
				
				plot <- plot_ly(
					sea_df_stats(),
					connectgaps = TRUE
				)
				
				point_color  <- "#F8EEB4"
				line_color   <- "#658525"
				spline_color <- "#092A35"
				bg_color     <- "#f2f2f2"
				
				
				if("point" %in% features){
					if(datasource == "Temp"){
						plot <- plot %>% add_trace(
							data = sampled(),
							x = ~cd,
							y = ~Max_Temp,
							type = "scatter",
							name = "All Highs",
							mode = "markers",
							marker = list(
								opacity = 0.1,
								size = 17#, color = point_color
							)
						) %>% add_trace(
							data = sampled(),
							x = ~cd,
							y = ~Min_Temp,
							type = "scatter",
							name = "All Lows",
							mode = "markers",
							marker = list(
								opacity = 0.1,
								size = 17#, color = point_color
							)
						)
					} else {
						plot <- plot %>% add_trace(
							data = sampled(),
							x = ~cd,
							y = ~Precipitation,
							type = "scatter",
							name = "Points",
							mode = "markers",
							marker = list(
								opacity = 0.1,
								size = 17#, color = point_color
							)
						)
					}
				}
				
				if("line" %in% features){
					if("Highest Temps" %in% show){
						plot <- plot %>% add_lines(
							data = sea_df_stats(),
							x = ~cd,
							y = ~line_max_Max_Temp,
							name = "Highest Temps",
							line = list(
								shape = "linear",
								width = 3#, color = line_color
							)
						)
					}
					if("Average High" %in% show){
						plot <- plot %>% add_lines(
							data = sea_df_stats(),
							x = ~cd,
							y = ~line_mean_Max_Temp,
							name = "Average High",
							line = list(
								shape = "linear",
								width = 3#, color = line_color
							)
						)
					}
					if("Average Temp" %in% show){
						plot <- plot %>% add_lines(
							data = sea_df_stats(),
							x = ~cd,
							y = ~line_mean_Temp,
							name = "Average Temp",
							line = list(
								shape = "linear",
								width = 3#, color = line_color
							)
						)
					}
					if("Average Low" %in% show){
						plot <- plot %>% add_lines(
							data = sea_df_stats(),
							x = ~cd,
							y = ~line_mean_Min_Temp,
							name = "Average Low",
							line = list(
								shape = "linear",
								width = 3#, color = line_color
							)
						)
					}
					if("Lowest Temps" %in% show){
						plot <- plot %>% add_lines(
							data = sea_df_stats(),
							x = ~cd,
							y = ~line_min_Min_Temp,
							name = "Lowest Temps",
							line = list(
								shape = "linear",
								width = 3#, color = line_color
							)
						)
					}
					if("Max" %in% show){
						plot <- plot %>% add_lines(
							data = sea_df_stats(),
							x = ~cd,
							y = ~line_max_Precipitation,
							name = "Max",
							line = list(
								shape = "linear",
								width = 3#, color = line_color
							)
						)
					}
					if("Average" %in% show){
						plot <- plot %>% add_lines(
							data = sea_df_stats(),
							x = ~cd,
							y = ~line_mean_Precipitation,
							name = "Average",
							line = list(
								shape = "linear",
								width = 3#, color = line_color
							)
						)
					}
					if("Min" %in% show){
						plot <- plot %>% add_lines(
							data = sea_df_stats(),
							x = ~cd,
							y = ~line_min_Precipitation,
							name = "Min",
							line = list(
								shape = "linear",
								width = 3#, color = line_color
							)
						)
					}
				}
				
				if("spline" %in% features){
					if("Highest Temps" %in% show){
						plot <- plot %>% add_lines(
							data = sea_df_stats(),
							x = ~cd,
							y = ~spline_max_Max_Temp,
							name = "Highest Temps",
							line = list(
								shape = "spline",
								width = 3#, color = line_color
							)
						)
					}
					if("Average High" %in% show){
						plot <- plot %>% add_lines(
							data = sea_df_stats(),
							x = ~cd,
							y = ~spline_mean_Max_Temp,
							name = "Average High",
							line = list(
								shape = "spline",
								width = 3#, color = line_color
							)
						)
					}
					if("Average Temp" %in% show){
						plot <- plot %>% add_lines(
							data = sea_df_stats(),
							x = ~cd,
							y = ~spline_mean_Temp,
							name = "Average Temp",
							line = list(
								shape = "spline",
								width = 3#, color = line_color
							)
						)
					}
					if("Average Low" %in% show){
						plot <- plot %>% add_lines(
							data = sea_df_stats(),
							x = ~cd,
							y = ~spline_mean_Min_Temp,
							name = "Average Low",
							line = list(
								shape = "spline",
								width = 3#, color = line_color
							)
						)
					}
					if("Lowest Temps" %in% show){
						plot <- plot %>% add_lines(
							data = sea_df_stats(),
							x = ~cd,
							y = ~spline_min_Min_Temp,
							name = "Lowest Temps",
							line = list(
								shape = "spline",
								width = 3#, color = line_color
							)
						)
					}
					if("Max" %in% show){
						plot <- plot %>% add_lines(
							data = sea_df_stats(),
							x = ~cd,
							y = ~spline_max_Precipitation,
							name = "Max",
							line = list(
								shape = "spline",
								width = 3#, color = line_color
							)
						)
					}
					if("Average" %in% show){
						plot <- plot %>% add_lines(
							data = sea_df_stats(),
							x = ~cd,
							y = ~spline_mean_Precipitation,
							name = "Average",
							line = list(
								shape = "spline",
								width = 3#, color = line_color
							)
						)
					}
					if("Min" %in% show){
						plot <- plot %>% add_lines(
							data = sea_df_stats(),
							x = ~cd,
							y = ~spline_min_Precipitation,
							name = "Min",
							line = list(
								shape = "spline",
								width = 3#, color = line_color
							)
						)
					}
				}
				
				return(plot)
			}
		})
		
		output$msg <- renderText("This is dope")
		
		observe({
			numrows <- nrow(sea_df())
			stepsize <- 500

			while(numrows < stepsize){
				stepsize <- stepsize/10
			}
			
			updateSliderInput(
				session,
				inputId = "sea_day_samplesize",
				max = numrows,
				step = stepsize
			)
		})
		
		n <- reactiveVal(3)
		
		observe({
			if(input$sea_day_data != "Rain"){
				point_in_features <- "point" %in% input$sea_day_features
				spline_or_line_in_features <-
					"spline" %in% input$sea_day_features | "line" %in% input$sea_day_features
				spline_in_features <- "spline" %in% input$sea_day_features
				line_in_features <- "line" %in% input$sea_day_features
				
				if(input$sea_day_data == "Temp" & n() == 3){
					n(4)
					updateCheckboxGroupInput(
						session,
						"sea_day_show",
						choices = c(
							"Highest Temps",
							"Average High",
							"Average Temp",
							"Average Low",
							"Lowest Temps"
						),
						selected = c()
					)
				} else if(input$sea_day_data == "Precipitation" & n() == 4){	
					n(3)
					updateCheckboxGroupInput(
						session,
						"sea_day_show",
						choices = c(
							"Max",
							"Average",
							"Min"
						),
						selected = c()
					)
				}
				
				if(point_in_features){
					shinyjs::enable("sea_day_samplesize")
				} else {
					shinyjs::disable("sea_day_samplesize")
				}
				
				if(spline_in_features){
					shinyjs::enable("sea_day_span")
				} else {
					shinyjs::disable("sea_day_span")
				}
				
				if(!spline_or_line_in_features){
					if(!point_in_features){
						shinyjs::enable("sea_day_show")
						updateCheckboxGroupInput(
							session,
							"sea_day_features",
							selected = "line"
						)
						updateCheckboxGroupInput(
							session,
							"sea_day_show",
							selected = c(
								"Average",
								"Average Temp"
							)
						)
					} else {
						shinyjs::disable("sea_day_show")
					}
				} else {
					shinyjs::enable("sea_day_show")
					if(is_empty(input$sea_day_show)){
						updateCheckboxGroupInput(
							session,
							"sea_day_show",
							selected = c(
								"Average",
								"Average Temp"
							)
						)
					}
				}
			} else {
				shinyjs::disable("sea_day_show")
				shinyjs::disable("sea_day_samplesize")
			}
		})
	}
)