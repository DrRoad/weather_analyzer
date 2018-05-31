library(tidyverse)
library(shiny)
library(httr)
library(jsonlite)
library(lubridate)
library(plotly)
library(ggplot2)
library(stringr)
library(shinyjs)

# Source supplemental files below:
source("seattle_rain_analysis.R")
source("arjun_analysis.R")
source("apr_data.R")

shinyServer(
	function(input, output, session) {
		
		# --- APRIL ---
		apr_reactive_input <- reactive({
			humi_temp %>% 
				filter(city== input$city) %>% 
				filter(date== as.character(input$date))
		})
		
		output$humi_temp_point <- renderPlot({
			ggplot(apr_reactive_input(), aes(x= humidity, y= temperature, color = time)) +
				geom_point() +
				labs(title = "Relationship of humidity and temperature",
						 x = "humidity",
						 y = "temperature")
		})
		
		output$apr_text <- renderText({
			df <- apr_reactive_input()
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
	  
		# --- ARJUN ---
		reactive_values <- reactive({
			reactive_table <- land_temp_df %>%
				filter(Country == input$givenCountry)
			
			reactive_table <- left_join(reactive_table, global_temperature_df)
			
			available_dates <- reactive_table[, 2]
			
			boolean_dates <- grepl(input$givenYear, available_dates)
			reactive_table <- reactive_table %>%
				mutate("Boolean_dates" = boolean_dates) %>%
				filter(Boolean_dates == "TRUE")
			
			average_global <- 
				round(mean(reactive_table$LandAverageTemperature), 1)
			min_global_temp <- min(reactive_table$LandAverageTemperature)
			max_global_temp <- max(reactive_table$LandAverageTemperature)
			
			average_temp_country <-
				round(mean(reactive_table$AverageTemperature), 1)
			min_temp <- min(reactive_table$AverageTemperature)
			max_temp <- max(reactive_table$AverageTemperature)
			
			list(
				the_table = reactive_table,
				average = average_temp_country,
				chosen_year = input$givenYear,
				chosen_country = input$givenCountry,
				min_temperature = min_temp,
				max_temperature = max_temp,
				averageGlobal = average_global,
				minGlobal = min_global_temp,
				maxGlobal = max_global_temp
			)
		})
	  
	  reactive_values_2 <- reactive({
	    available_dates <- land_temp_df[, 2]
	    boolean_dates <-
	      grepl(input$givenYearEstimation, available_dates)
	    
	    estimation_table <- land_temp_df %>%
	      mutate("Boolean_dates_Estimation" = boolean_dates) %>%
	      filter(Boolean_dates_Estimation == "TRUE") %>%
	      group_by(Country) %>%
	      summarize(average = mean(AverageTemperature))
	    
	    table_with_averages_in_range <- estimation_table %>%
	      filter(average > input$temperature_range[1] &
	               average < input$temperature_range[2])
	    
	    min_estimation <- estimation_table %>%
	      filter(average == min(estimation_table$average, na.rm = TRUE))
	    
	    max_estimation <- estimation_table %>%
	      filter(average == max(estimation_table$average, na.rm = TRUE))
	    
	    max_country <- max_estimation$Country
	    max_temp <- max_estimation$average
	    
	    min_country <- min_estimation$Country
	    min_temp <- min_estimation$average
	    
	    column_names <- c("Country", "Average Temperature")
	    colnames(table_with_averages_in_range) <- column_names
	    
	    list(
	      minimum_country = min_country,
	      minimum_temperature = round(min_temp, 2),
	      range_table = table_with_averages_in_range,
	      maximum_country = max_country,
	      maximum_temperature = round(max_temp, 2)
	    )
	    
	  })
	  reactive_values_3 <- reactive({
	    
	    specific_country_mean_by_year <- mean_by_year_df %>%
	      filter(Country == input$givenCountryPrediction)
	    specific_country_mean_by_year <- transform(specific_country_mean_by_year, Years = as.numeric(Years), meanByYear = as.numeric(meanByYear))
	    list("country_mean_data" = specific_country_mean_by_year)
	    
	  })
	  
	  reactive_forcast <- reactive({
	    
	    fit <- lm(meanByYear ~ Years, data = reactive_values_3()[['country_mean_data']])
	      intercept <- signif(fit$coef[[1]], 5)
	      slope <- signif(fit$coef[[2]], 5)
	    prediction <- slope * as.numeric(input$givenYearForcast) + intercept
	    summary_calculation <- coef(summary(fit))
	    #summary_calc_filtered <- select(as.data.frame(summary_calculation), Estimate)
	    summary_calc_filtered <- as.data.frame(summary_calculation)
	    list("Intercept" = intercept, "Slope" = slope, "Prediction" = prediction, "summary_calc" = summary_calc_filtered)
	    
	  })
	  
	  output$forcast_text <- renderText({
	    paste("The predicted average land temperature for the year", input$givenYearForcast, "is", reactive_forcast()[['Prediction']])
	  })
	  
	  output$error_table <- renderTable({
	    reactive_forcast()[['summary_calc']]
	    
	  })
	  
	  output$specific_country_mean_plot <- renderPlotly({
	    p <- ggplot(data = reactive_values_3()[["country_mean_data"]], 
	                aes(x = reactive_values_3()[["country_mean_data"]]$Years, 
	                    y = reactive_values_3()[["country_mean_data"]]$meanByYear)) + 
	      labs(title = "Scatter Plot showing Average temperatures of selected country from 1900-2012", x = "Year", y = "Average Temperature") +
	      geom_point() + 
	      geom_smooth(method = "lm", col = "red")
	    ggplotly(p)
	    p
	  })
	  
	  output$land_plot <- renderPlotly({
	  	TemperatureForGivenYear <- reactive_values()[['the_table']]
	  	p <-
	  		ggplot(data = TemperatureForGivenYear) +
	  		geom_line(aes(x = TemperatureForGivenYear$dt,
	  									y = TemperatureForGivenYear$AverageTemperature, group = input$givenCountry, color = input$givenCountry)
	  		) + geom_line(aes(x = TemperatureForGivenYear$dt,
	  											y = TemperatureForGivenYear$LandAverageTemperature, group = "Boolean", color = "Global")) + 
	  		scale_color_manual("",
	  											 breaks = c(input$givenCountry, "Global"),
	  											 values = c("red", "green")) +
	  		labs(title = "Country temperature vs Global temperature", x = "Year in [year/month/day] format", 
	  				 y = "Average Temperature in Celcius") + theme_gray()
	  	ggplotly(p)
	  })
	  
	  output$average_text <- renderText({
	    text <- paste("The average land temperature in", 
	                  reactive_values()[['chosen_country']], "for the year", 
	                  reactive_values()[['chosen_year']], "was",
	                  reactive_values()[['average']])
	    text <- paste0(text,".")
	    text <- paste(text, "The minimum temperature recorded for the given year was",
	                  reactive_values()[['min_temperature']])
	    text <- paste0(text, ".")
	    text <- paste(text, "The maximum temperature recorded in the given year was",
	                  reactive_values()[['max_temperature']])
	    text
	    
	  })
	  
	  output$estimation_text_min <- renderText({
	    paste(
	      "The lowest average temperature in the given year is estimated to be in",
	      reactive_values_2()[['minimum_country']],
	      "with a minimum temperature of:",
	      reactive_values_2()[['minimum_temperature']]
	    )
	  })
	  
	  output$esimation_text_max <- renderText ({
	    paste(
	      "The highest average temperature in the given year is estimated to be in", 
	      reactive_values_2()[['maximum_country']], "with a maximum temperature of",
	      reactive_values_2()[['maximum_temperature']]
	    )
	  })
	  
	  output$table_with_temperature_range <- renderTable({
	    reactive_values_2()[["range_table"]]
	  })
	  
	  output$global_text <- renderText ({
	    text <- paste("The average Global temperature for the given year was", 
	          reactive_values()[['averageGlobal']])
	    text <- paste0(text,".")
	    text <- paste(text, "The minimum average Global temperature recorded was",
	                  reactive_values()[['minGlobal']])
	    text <- paste0(text, ".")
	    text <- paste(text, "The maximum average Global temperature recorded was",
	                  reactive_values()[['maxGlobal']])
	    text
	
	  })
  
  # ---- START OF HEMIL'S CODE ----
  output$sea_question <- renderText({
  	paste0(
  		"1. What are some historical year-long weather patterns for Seattle?\n",
			"2. Can these patterns accurately predict future weather?\n"
		)
  })
  
  # Resets the predict slider to today when pressed
  observeEvent(input$sea_predict_reset, {
  	updateSliderInput(
  		session,
  		"sea_predict_dates",
  		value = as.Date(today())
  	)
  })
  
  # Stores and normalizes all 2018 API data according to the sea_predict slider bounds
  api_table <- reactive({
  	out <- api_2018 %>% filter(
  		time >= input$sea_predict_dates,
  		time <= as.Date(input$sea_predict_dates) + days(6)
  	) %>% rowwise() %>% mutate(
  		month_day = paste(format(as.Date(time), format = "%b %d"))
  	)
  	out$cd <- as.Date(out$cd)
  	return(out)
  })
  
  # Verbose description for the sea_day plot
  output$sea_day_msg <- renderText({
  	label <- if(input$sea_day_data == "Temp"){
  		"Temperature"
  	} else {
  		input$sea_day_data
  	}
  	
  	paste0(
  		"The above graph displays ", tolower(label), " data from all days between ",
  		format(as.Date(input$sea_day_dates[1]), format = "%b %d"),
  		" and ",
  		format(as.Date(input$sea_day_dates[2]), format = "%b %d"),
  		" in the past 70 years.\nThe features visible on the graph include the following: ",
  		toString(input$sea_day_features), "."
  	)	
  })
  
  # Partly calculates the percent error (experimental minus theoretical)/experimental
  # for each prediction series
  sea_difference <- reactive({
  	compare <- data.frame(sea_predictions()$cdt)
  	compare$max_temp <- 
  		(api_table()$max_temp - sea_predictions()$max_temp_smooth)/api_table()$max_temp
  	compare$min_temp <- 
  		(api_table()$min_temp - sea_predictions()$min_temp_smooth)/api_table()$min_temp
  	compare$pct_precip <- 
  		(api_table()$pct_precip - sea_predictions()$pct_precip_smooth)/api_table()$pct_precip
  	compare$inch_precip <- 
  		(api_table()$inch_precip - sea_predictions()$inch_precip_smooth)/api_table()$inch_precip
  	
  	return(compare)
  })
  
  # Stores loess-smoothed predictions for each prediction series
  sea_predictions <- reactive({
  	df <- api_table() %>% select(month_day, cd)
  	colnames(df) <- c("month_day", "cdt")
  	
  	smoothness <- 0.5
  	df$index <- 1:nrow(df)
  	
  	means <- seattle_rain_df %>% group_by(cd) %>% summarize(
  		Max_Temp = mean(Max_Temp),
  		Min_Temp = mean(Min_Temp),
  		Inches_Precip = mean(Precipitation),
  		Pct_Precip = sum(Did_Rain) / length(Did_Rain)
  	)
  	
  	means <- means[means$cd %in% as.Date(df$cdt),]
  	
  	df$max_temp_raw <- means$Max_Temp
  	df$min_temp_raw <- means$Min_Temp
  	df$inch_precip_raw <- means$Inches_Precip
  	df$pct_precip_raw <- means$Pct_Precip
  	
  	df$max_temp_smooth <- predict(loess(
  		max_temp_raw ~ index,
  		data = df,
  		span = smoothness
  	))
  	
  	df$min_temp_smooth <- predict(loess(
  		min_temp_raw ~ index,
  		data = df,
  		span = smoothness
  	))
  	
  	df$pct_precip_smooth <- predict(loess(
  		pct_precip_raw ~ index,
  		data = df,
  		span = smoothness
  	))
  	
  	df$inch_precip_smooth <- predict(loess(
  		inch_precip_raw ~ index,
  		data = df,
  		span = smoothness
  	))
  	
  	df$cdt <- as.Date(df$cdt)
  	
  	return(df)
  }) 
  
  # Finishes calculation for percent error, returns text that associates
  # each error value with the corresponding series
  output$sea_predict_error <- renderText({
  	pct_error <- c(
  		100*mean(abs(sea_difference()$max_temp[!is.infinite(sea_difference()$max_temp)])),
  		100*mean(abs(sea_difference()$min_temp[!is.infinite(sea_difference()$min_temp)])),
  		100*mean(abs(sea_difference()$pct_precip[!is.infinite(sea_difference()$pct_precip)])),
  		100*mean(abs(sea_difference()$inch_precip[!is.infinite(sea_difference()$inch_precip)]))
  	)
  	
  	pct_error <- round(pct_error, 4)
  	
  	paste0(
  		"Average Prediction Errors for:\n",
  		"Max Temp       - ", pct_error[1], "%\n",
  		"Min Temp       - ", pct_error[2], "%\n",
  		"Chance of Rain - ", pct_error[3], "%\n",
  		"Inch Rain      - ", pct_error[4], "%\n"
  	)
  })
  
  # Informational message about how to use the sea_predict page
  output$sea_predict_msg <- renderText({
  	paste0("The graph to the right compares weather predictions made from historical Seattle data to ",
  				 "values found from the API. The above slider adjusts the first day of the week for which predictions are calculated.",
  				 " Since the API does not provide precipitation predictions for more than a week beyond the current day,",
  				 " any selected slider day after today will not predict precipitation.")
  })
  
  # Plot for the predictions
  output$sea_predict_plot <- renderPlotly({
  	output <- NA
  	
  	# Are there four plots or two?
  	four <- FALSE
  	
  	first_tick <- as.Date(input$sea_predict_dates)
  	year(first_tick) <- 2000
  	
  	max_temp_plot <- plot_ly(
  		api_table(),
  		connectgaps = TRUE,
  		x = ~cd,
  		y = ~max_temp,
  		type = "bar",
  		name = "API Max Temp"
  	) %>% add_trace(
  		data = sea_predictions(),
  		x = ~cdt,
  		y = ~max_temp_smooth,
  		name = "Predicted Max Temp"
  	) %>% layout(
  		barmode = "group",
  		xaxis = list(
  			zeroline = FALSE,
  			showline = TRUE,
  			showgrid = TRUE,
  			tickformat = "%b %d",
  			tick0 = first_tick,
  			dtick = 86400000
  		),
  		yaxis = list(
  			zeroline = FALSE,
  			showline = TRUE,
  			title = "Temperature"
  		),
  		plot_bgcolor = "#f2f2f2"
  	)
  	
  	min_temp_plot <- plot_ly(
  		api_table(),
  		connectgaps = TRUE,
  		x = ~cd,
  		y = ~min_temp,
  		name = "API Min Temp",
  		type = "bar"
  	) %>% add_trace(
  		data = sea_predictions(),
  		x = ~cdt,
  		y = ~min_temp_smooth,
  		name = "Predicted Min Temp"
  	) %>% layout(
  		barmode = "group",
  		xaxis = list(
  			zeroline = FALSE,
  			showline = TRUE,
  			showgrid = TRUE,
  			tickformat = "%b %d",
  			tick0 = first_tick,
  			dtick = 86400000
  		),
  		yaxis = list(
  			zeroline = FALSE,
  			showline = TRUE,
  			title = "Temperature"
  		),
  		plot_bgcolor = "#f2f2f2",
  		title = "<b>My Predictions vs. API Data</b>",
  		showlegend = TRUE,
  		legend = list(
  			orientation = "h",
  			x = 0.5,
  			xanchor = "center",
  			y = -0.1,
  			bgcolor = "#f2f2f2"
  		)
  	)
  	
  	# Add precipitation prediction plots if the slider date is valid
  	if(as.Date(input$sea_predict_dates) <= today()){
  		four <- TRUE
  		pct_precip_plot <- plot_ly(
  			api_table(),
  			connectgaps = TRUE,
  			x = ~cd,
  			y = ~pct_precip,
  			name = "API Chance of Rain",
  			type = "bar"
  		) %>% add_trace(
  			data = sea_predictions(),
  			x = ~cdt,
  			y = ~pct_precip_smooth,
  			name = "Predicted Chance of Rain"
  		) %>% layout(
  			barmode = "group",
  			xaxis = list(
  				zeroline = FALSE,
  				showline = TRUE,
  				showgrid = TRUE,
  				tickformat = "%b %d",
  				tick0 = first_tick,
  				dtick = 86400000
  			),
  			yaxis = list(
  				zeroline = FALSE,
  				showline = TRUE,
  				title = "Percent",
  				tickformat = "%"
  			),
  			plot_bgcolor = "#f2f2f2"
  		)
  		
  		inch_precip_plot <- plot_ly(
  			api_table(),
  			connectgaps = TRUE,
  			x = ~cd,
  			y = ~inch_precip,
  			type = "bar",
  			name = "API Inches of Rain"
  		) %>% add_trace(
  			data = sea_predictions(),
  			x = ~cdt,
  			y = ~inch_precip_smooth,
  			name = "Predicted Inches of Rain"
  		) %>% layout(
  			barmode = "group",
  			xaxis = list(
  				zeroline = FALSE,
  				showline = TRUE,
  				showgrid = TRUE,
  				tickformat = "%b %d",
  				tick0 = first_tick,
  				dtick = 86400000
  			),
  			yaxis = list(
  				zeroline = FALSE,
  				showline = TRUE,
  				title = "Inches of Rain"
  			),
  			plot_bgcolor = "#f2f2f2"
  		)
  	}
  	
  	if(four){
  		output <- subplot(
  			max_temp_plot,
  			min_temp_plot,
  			pct_precip_plot,
  			inch_precip_plot,
  			nrows = 2,
  			titleY = TRUE,
  			titleX = FALSE,
  			shareX = TRUE
  		)
  	} else {
  		output <- subplot(
  			max_temp_plot,
  			min_temp_plot,
  			nrows = 2,
  			titleY = TRUE,
  			titleX = FALSE,
  			shareX = TRUE
  		)
  	}
  	
  	return(output)
  })
  
  # Original CSV for data description page (minimally cleaned)
  output$sea_df <- renderDataTable(
  	{seattle_rain_df},
  	options = list(
  		pageLength = 14
  	)
  )
  
  # Text for data description page, refers to API too
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
  		"The original data, with minimal modification and processing, is viewable to",
  		" the right. Feel free to search specific records, change the page, show",
  		"more entries, or sort any column in increasing or decreasing order.",
  		br(), br(),
  		"Additionally, this part of the Weather Analyzer uses data from the ",
  		"DarkSky Weather ",
  		a("API,",
  			href = "https://darksky.net/dev/docs"
  		), "and as such, is ",
  		a("Powered by DarkSky.",
  			href = "https://darksky.net/poweredby/"
  		),
  		br(), br(),
  		em("Exploring Seattle"), " page created by: Hemil Gajjar"
  	)
  })
  
  # DF used to store all rows from original data frame that are in the
  # bounds of the date slider
  sea_df <- reactive({
  	df <- seattle_rain_df %>% filter(
  		cd >= input$sea_day_dates[1],
  		cd <= input$sea_day_dates[2]
  	)
  	return(df)
  })
  
  # DF used to store random samples of sea_df()
  sea_day_sampled <- reactive({
  	sample_n(sea_df(), input$sea_day_samplesize)
  })
  
  # DF used for storing original and loess-smoothed means/mins/maxes
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
  
  # DF used for By Day Occurrences of Rain
  sea_df_rain <- reactive({
  	df <- sea_df() %>% group_by(cd) %>% summarize(
  		pct_rainy_days = sum(Did_Rain) / length(Did_Rain)
  	) %>% rowwise() %>% mutate(
  		month_day = paste(format(as.Date(cd), format = "%b %d"))
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
  
  # Plot for By Day
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
  						bgcolor = "#f2f2f2",
  						traceorder = "reversed"
  					),
  					plot_bgcolor = "#f2f2f2"
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
  					data = sea_day_sampled(),
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
  					data = sea_day_sampled(),
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
  					data = sea_day_sampled(),
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
  					name = "Highest Temps Jagged",
  					line = list(
  						shape = "linear",
  						width = 3
  					)
  				)
  			}
  			if("Average High" %in% show){
  				plot <- plot %>% add_lines(
  					data = sea_df_stats(),
  					x = ~cd,
  					y = ~line_mean_Max_Temp,
  					name = "Average High Jagged",
  					line = list(
  						shape = "linear",
  						width = 3
  					)
  				)
  			}
  			if("Average Temp" %in% show){
  				plot <- plot %>% add_lines(
  					data = sea_df_stats(),
  					x = ~cd,
  					y = ~line_mean_Temp,
  					name = "Average Temp Jagged",
  					line = list(
  						shape = "linear",
  						width = 3
  					)
  				)
  			}
  			if("Average Low" %in% show){
  				plot <- plot %>% add_lines(
  					data = sea_df_stats(),
  					x = ~cd,
  					y = ~line_mean_Min_Temp,
  					name = "Average Low Jagged",
  					line = list(
  						shape = "linear",
  						width = 3
  					)
  				)
  			}
  			if("Lowest Temps" %in% show){
  				plot <- plot %>% add_lines(
  					data = sea_df_stats(),
  					x = ~cd,
  					y = ~line_min_Min_Temp,
  					name = "Lowest Temps Jagged",
  					line = list(
  						shape = "linear",
  						width = 3
  					)
  				)
  			}
  			if("Max" %in% show){
  				plot <- plot %>% add_lines(
  					data = sea_df_stats(),
  					x = ~cd,
  					y = ~line_max_Precipitation,
  					name = "Max Jagged",
  					line = list(
  						shape = "linear",
  						width = 3
  					)
  				)
  			}
  			if("Average" %in% show){
  				plot <- plot %>% add_lines(
  					data = sea_df_stats(),
  					x = ~cd,
  					y = ~line_mean_Precipitation,
  					name = "Average Jagged",
  					line = list(
  						shape = "linear",
  						width = 3
  					)
  				)
  			}
  			if("Min" %in% show){
  				plot <- plot %>% add_lines(
  					data = sea_df_stats(),
  					x = ~cd,
  					y = ~line_min_Precipitation,
  					name = "Min Jagged",
  					line = list(
  						shape = "linear",
  						width = 3
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
  					name = "Highest Temps Smooth",
  					line = list(
  						shape = "spline",
  						width = 3
  					)
  				)
  			}
  			if("Average High" %in% show){
  				plot <- plot %>% add_lines(
  					data = sea_df_stats(),
  					x = ~cd,
  					y = ~spline_mean_Max_Temp,
  					name = "Average High Smooth",
  					line = list(
  						shape = "spline",
  						width = 3
  					)
  				)
  			}
  			if("Average Temp" %in% show){
  				plot <- plot %>% add_lines(
  					data = sea_df_stats(),
  					x = ~cd,
  					y = ~spline_mean_Temp,
  					name = "Average Temp Smooth",
  					line = list(
  						shape = "spline",
  						width = 3
  					)
  				)
  			}
  			if("Average Low" %in% show){
  				plot <- plot %>% add_lines(
  					data = sea_df_stats(),
  					x = ~cd,
  					y = ~spline_mean_Min_Temp,
  					name = "Average Low Smooth",
  					line = list(
  						shape = "spline",
  						width = 3
  					)
  				)
  			}
  			if("Lowest Temps" %in% show){
  				plot <- plot %>% add_lines(
  					data = sea_df_stats(),
  					x = ~cd,
  					y = ~spline_min_Min_Temp,
  					name = "Lowest Temps Smooth",
  					line = list(
  						shape = "spline",
  						width = 3
  					)
  				)
  			}
  			if("Max" %in% show){
  				plot <- plot %>% add_lines(
  					data = sea_df_stats(),
  					x = ~cd,
  					y = ~spline_max_Precipitation,
  					name = "Max Smooth",
  					line = list(
  						shape = "spline",
  						width = 3
  					)
  				)
  			}
  			if("Average" %in% show){
  				plot <- plot %>% add_lines(
  					data = sea_df_stats(),
  					x = ~cd,
  					y = ~spline_mean_Precipitation,
  					name = "Average Smooth",
  					line = list(
  						shape = "spline",
  						width = 3
  					)
  				)
  			}
  			if("Min" %in% show){
  				plot <- plot %>% add_lines(
  					data = sea_df_stats(),
  					x = ~cd,
  					y = ~spline_min_Precipitation,
  					name = "Min Smooth",
  					line = list(
  						shape = "spline",
  						width = 3
  					)
  				)
  			}
  		}
  		
  		ytitles <- c("Inches", "Degrees (F)")
  		ytitle <- NA
  		if(datasource == "Temp"){
  			ytitle <- ytitles[2]
  		} else {
  			ytitle <- ytitles[1]
  		}
  		
  		return(plot %>% layout(
  			title = paste0("<b>Historical ", datasource, " Data Per Day of the Year</b>"),
  			yaxis = list(
  				title = paste0("<b>", ytitle, "</b>"),
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
  				bgcolor = "#f2f2f2",
  				traceorder = "reversed"
  			),
  			plot_bgcolor = "#f2f2f2"
  		)
  		)
  	}
  })
  
  # This observe block changes the stepsize for the samplesize
  # slider when necessary
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
  
  # Tracks the size of the show checkboxgroupinput
  # (useful when updating its choices)
  sea_size_show <- reactiveVal(3)
  
  # This observe block handles selection, deselection, state (enabled/disabled),
  # and most dependencies for the checkboxinputs
  observe({
  	if(input$sea_day_data != "Rain"){
  		point_in_features <- "point" %in% input$sea_day_features
  		spline_or_line_in_features <-
  			"spline" %in% input$sea_day_features | "line" %in% input$sea_day_features
  		spline_in_features <- "spline" %in% input$sea_day_features
  		line_in_features <- "line" %in% input$sea_day_features
  		
  		if(input$sea_day_data == "Temp" & sea_size_show() == 3){
  			sea_size_show(4)
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
  		} else if(input$sea_day_data == "Precipitation" & sea_size_show() == 4){	
  			sea_size_show(3)
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
  
  # To avoid verbose messages
  options(warn = -1)
  
  # ---- END OF HEMIL'S CODE ----
  
	}
)