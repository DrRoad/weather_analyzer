library("shinythemes")

source("server.R")
source("ayah_data.R")
source("arjun_analysis.R")

shinyUI(
	navbarPage(
		"DATAvengers: Weather Analyzer",
		
		fluid = TRUE,
		inverse = TRUE,
		
		tabPanel("Ayah Idris",
			tabsetPanel(
				tabPanel(
					"Local Events!",
					br(),
					mainPanel(
						p("Based on the last section, you were able to view the forecasted weather patterns and chance of rain in Seattle.
							 If you are a tourist in Seattle or just a Seattlite in need of helpful and current weather-dependent tips,
							 this is the tab for you! Sometimes it can be difficult to know what to wear or where to go when the gloomy
							 weather takes over, but in this section you will find a real time forecast of the Rainy City with up to date
							 events in the area, weather-appropriate activity suggestions, along with some historic data that may help you
							 decide if the beauty (and trajectory) of the Pacific North West is captivating enough to make your new home!"),
						br(),
						p("The following table is a current list of upcoming events happening in the Seattle area. Information
							 courtesy of Yelp!"),
						tableOutput("ayah_events")
						)
					),
				
				tabPanel(
					"What To Wear and Where To Go!",
					br(),
					sidebarLayout(
						sidebarPanel(
							selectInput(
								"ayah_day_selection",
								"Select which day you would like to retrieve suggestions for (number of days from selected day in Seattle Model):",
								choices = c(
									"1 day" = 1,
									"2 days" = 2,
									"3 days" = 3,
									"4 days" = 4,
									"5 days" = 5,
									"6 days" = 6,
									"7 days" = 7
								)
							)
						),
						mainPanel(
							p("In the following table you will see suggestions for: what to wear and best sights to see
						on the forecasted day!"),
							tableOutput("ayah_table")
						)
					)
				),
				
				tabPanel(
					"Industrialization, Global Warming, and Rise in Cost of Living?",
					br(),
					mainPanel(
						p("Over time, data has shown a correlation between the population size increasing and increase in global
                warming; likely as a result of emissions produced by the increasing population."),
						br(),
						plotOutput("ayah_temperature"),
						br(),
						plotOutput("ayah_populations"),
						p("From the above graph you can see the positive correlation of time with both population size and
                increase temperatures.")
					)
				)
			)
		),
		
		tabPanel("Arjun Singh",
			tabsetPanel(
						 	
				# Breaks my page into tabs
				tabPanel(
					"Summarize Data",
					br(),
					sidebarLayout(
						sidebarPanel(
							selectInput(
								"givenCountry",
								"Choose a Country:",
								unique_countries
							),
							selectInput(
								"givenYear",
								"Choose year to display",
								c(
									"2000", "2001", "2002",
									"2003", "2004", "2005",
									"2006", "2007", "2008",
									"2009", "2010", "2011", "2012"
								)
							)
						),
						mainPanel(
							h3("The three tabs in this section can be used to navigate data regarding Average Land Temperatures."),
							h4("Have you ever wondered how a particular country's average temperature correlates with
			 					 the average Global temperature? If yes, then this is the tab for you! On the left,
			 					 you can select a country and year that you are interested in. Once selected, you will
			 					 see the chosen country's average temperature plotted over that year displayed
			 					 in red and the global average temperature plotted in Green. You can use this to investigate
			 					 countries of your choice. At the bottom of the plot you will see some information
			 					 about the selected country in the given year. One would expect to see country's trendline
			 					 follow the global average temperatures but, there are many countries that do not have this
			 					 trend and can be further investigated in the following tabs."
			 				),
							plotOutput("land_plot"),
							verbatimTextOutput("average_text"),
							#verbatimTextOutput("min_text"),
							#verbatimTextOutput("max_text"),
							verbatimTextOutput("global_text")
						)
					)
				),
				tabPanel(
					"Find range of temperatures ",
					br(),
					sidebarLayout(
						sidebarPanel(
							selectInput(
								"givenYearEstimation",
								"Choose year to display",
								c(
									"2000", "2001", "2002",
									"2003", "2004", "2005",
									"2006", "2007", "2008",
									"2009", "2010", "2011", "2012"
									)
							),
							sliderInput(
								"temperature_range",
								"Enter a range of temperature values: ",
								min = temperature_range[1],
								max = temperature_range[2],
								value = c(min, max)
								)
						),
						mainPanel(
							h4(
								"On this tab you can select a year of interest and a range of temperature values that you are
								interested in exploring and you will see a table with the countries listed in alphabetical order
								that fall into that given range. Using information from the previous tab you will be able to seek
								out countries that had similar trends to the ones you explored earlier. For example, if you select
								the country, Australia on the first tab you will see that its trend over any given year is almost the
								mirror image of the trendline for the global average temperature. This raises the question, maybe countries
								that had similar average temperatures as Australia followed this similar trend of having inverted trend lines
								when compared to average global temperatures. So, over here you could
								scope out countries with similar average temperatures and then plot them on the previous tab to check
								whether the two countries correlate (Feel free to try this example out!).
								This kind of analysis could potentially be done for any country."
			 				),
							verbatimTextOutput("estimation_text_min"),
							verbatimTextOutput("esimation_text_max"),
							tableOutput('table_with_temperature_range')
						)
					)
				),
				tabPanel(
					"Forecast Temperature",
					br(),
					sidebarLayout(
						sidebarPanel(
							selectInput(
								"givenCountryPrediction",
								"Choose a Country:",
								unique_countries
							),
							selectInput(
								"givenYearForcast",
								"Choose year to Forecast",
								c(2000:3000)
							)
						),
						mainPanel(
							h4(
			 					"This tab can be used to predict average land temperatures in the future because who does not
			 					like to be prepared for a rainy day! On the left you can select a Country of your choice and
			 					a year that you would like to forcast to. Bellow you will see a scatter plot with average land
			 					temperatures plotted from around 1800 - 2012. Using the range tab one can investigate whether
			 					countries that fall into similar ranges of average temperatures have similar forecasts.
			 					There is more information about the plot bellow!"
							),
							plotlyOutput('specific_country_mean_plot'),
							verbatimTextOutput('forcast_text'),
							h5(
			 					"The predicted Average land temperature was calculated based on the the linear regression model ploted above.
			 					Linear regression models are based on the function of the best fit line plotted on a set of data points. The
			 					standard equation of a line is y = mx + c, where m is the slope of the line and c is the y intercept. These
			 					values have been calculated from the line of best fit shown in the plot. This function can then be used to
			 					predict temperatures in the future with the assumption that the increase in average land temperature with respect
			 					to time will be linear as shown. While making this estimation it will most certainly have
			 					some error as the data points are scattered and the line
			 					does not pass through each and every one of these points. These errors in esimation are presented bellow and
			 					do not worry if you can't understand anything past the first 2 columns! Leave those for the staticians!"
			 				),
							tableOutput('error_table'),
							h4("Interpreting table above:"),
							h5(
			 					"The Estimate column has two rows. The first one shows the estimate chosen for the y-intercept and
			 					the second one shows the estimated value of the slope . The Std.Error shows the error in these calculated values and then the
			 					t columns shows you the t-test associated with testing the significance of the parameter
			 					listed in the first column! The pr(>t) column shows the probability of one getting a value higher than t."
							)
						)
					)
				)
			)
		),
		
		tabPanel(
			"April Huang",
			h1("US City Weather Data 2012-2017"),
			p(
				"The scatter plot based on the hourly measurement data of two weather attributes,
				humidity and temperature for 27 US cities, showing the relationship between
				humidity and temperature on x-axis and y-axis. The specific time of the chosen date and city
				is characterized with different color. "
			),
			sidebarLayout(
				sidebarPanel(
					selectInput("city",
											"Select a city", choice = unique(humi_temp$city)),
					
					p("The date range is: Oct 1, 2012 - Nov 30, 2017"),
					
					dateInput(
						"date",
						"Select or type in a date",
						min = as.Date("2012-10-01", "%Y-%m-%d"),
						max = as.Date("2017-11-30", "%Y-%m-%d"),
						value = as.Date("2012-12-01")
					)
					
				),
				
				mainPanel(plotOutput("humi_temp_point"), #width = 400, height = 300
									textOutput("apr_text"))
			)
			),
		
		navbarMenu(
			"Hemil Gajjar",
			tabPanel(
				"The Questions",
				verticalLayout(
					br(),
					br(),
					br(),
					verbatimTextOutput("sea_question"),
					tags$head(
						tags$style(
							HTML(
								"
								#sea_question {
								font-size: 20px;
								}
								"
							)
						)
					)
				)
			),
			tabPanel(
				"The Data",
				br(),
				sidebarLayout(
					sidebarPanel(uiOutput("sea_df_description")),
					mainPanel(dataTableOutput("sea_df"))
				)
			),
			
			tabPanel(
				"Patterns By Day",
				br(),
				sidebarLayout(
					sidebarPanel(
						selectInput(
							inputId = "sea_day_data",
							label = "Data to Plot:",
							choices = c(
								"Precipitation" = "Precipitation",
								"Temperature" = "Temp",
								"Occurrence of Rain" = "Rain"
							),
							selected = "Rain"
						),
						checkboxGroupInput(
							inputId = "sea_day_features",
							label = "Features:",
							choices = c(
								"Smoothed Line" = "spline",
								"Jagged Line" = "line",
								"Points" = "point"
							),
							selected = c("spline",
													 "line")
						),
						checkboxGroupInput(
							inputId = "sea_day_show",
							label = "Line(s) Through:",
							choices = c("Max",
													"Average",
													"Min"),
							selected = c("Average")
						),
						sliderInput(
							inputId = "sea_day_dates",
							label = "Dates:",
							min = cd_range[1],
							max = cd_range[2],
							value = c(cd_range[1],
												cd_range[2]),
							timeFormat = "%b %d"
						),
						sliderInput(
							inputId = "sea_day_span",
							label = "Degree of Smoothing",
							min = 0.05,
							max = 0.95,
							value = 0.4,
							step = 0.05
						),
						sliderInput(
							inputId = "sea_day_samplesize",
							label = "Number of Points to Plot",
							min = 0,
							max = 25548,
							value = 5000,
							step = 100
						)
					),
					mainPanel(
						plotlyOutput("sea_day_plot", height = 550),
						br(),
						verbatimTextOutput("sea_day_msg")
					)
				)
			),
			
			tabPanel(
				"Prediction Model",
				br(),
				sidebarLayout(
					sidebarPanel(
						sliderInput(
							"sea_predict_dates",
							"Choose a Day of 2018 to Week-Forecast:",
							min = api_date_range[1],
							max = as.Date(api_date_range[2]) - days(6),
							value = as.Date(today()),
							timeFormat = "%b %d %Y"
						),
						actionButton("sea_predict_reset", "Reset to Today"),
						br(),
						br(),
						textOutput("sea_predict_msg"),
						br(),
						verbatimTextOutput("sea_predict_error")
					),
					mainPanel(plotlyOutput("sea_predict_plot", height = 650))
				)
			)
		),
		shinyjs::useShinyjs()
	)
)