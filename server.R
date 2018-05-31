library('dplyr')
library('ggplot2')
library('tidyr')
library('httr')
library('jsonlite')
library('stringr')
library("plotly")

# Source any supplemental files below:
source("seattle_rain_analysis.R")
source("arjun_analysis.R")

shinyServer(function(input, output) {
  
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
      ggplot(
        data = TemperatureForGivenYear) +
      geom_line(aes(x = TemperatureForGivenYear$dt,
                    y = TemperatureForGivenYear$AverageTemperature, group = input$givenCountry, color = input$givenCountry)
      ) + geom_line(aes(x = TemperatureForGivenYear$dt,
                        y = TemperatureForGivenYear$LandAverageTemperature, group = "Boolean", color = "Global")) + 
      scale_color_manual("",
                         breaks = c(input$givenCountry, "Global"),
                         values = c("red", "green")) +
      labs(title = "Country temperature vs Global temperature", x = "Year in [year/month/day] format", 
           y = "Average Temperature in Celcius") + theme_gray()
    p
    
    
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
  
})