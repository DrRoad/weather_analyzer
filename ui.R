source("server.R")

# Overview first,
# zoom and filter,
# then details on demand

shinyUI(

	navbarPage(
		"DATAvengers - Weather Analyzer",

		tabPanel(
			"Ayah",
			sidebarLayout(
				sidebarPanel(),
				mainPanel(
					textOutput("example1")
				)
			)
		),
		
		tabPanel(
			"Arjun",
			sidebarLayout(
				sidebarPanel(),
				mainPanel(
					textOutput("example2")
				)
			)
		),
		
		tabPanel(
			"April",
			sidebarLayout(
				sidebarPanel(),
				mainPanel(
					textOutput("example3")
				)
			)
		),
		
		tabPanel(
			"Exploring Seattle",
			
			tabsetPanel(
				id = "sea_tabs",
				type = "pills",
				
				tabPanel(
					"The Data",
					br(),
					sidebarLayout(
						sidebarPanel(
							uiOutput("sea_df_description")
						),
						mainPanel(
							dataTableOutput("sea_df")
						)
					)
				),
				
				tabPanel(
					"By Day",
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
								inputId = "sea_day_show",
								label = "Show:",
								choices = c(
									"Max",
									"Average",
									"Min"
								),
								selected = c(
									"Average"
								)
							),
							checkboxGroupInput(
								inputId = "sea_day_features",
								label = "Features:",
								choices = c(
									"Smoothed Line" = "spline",
									"Jagged Line" = "line",
									"Points" = "point"
								),
								selected = c(
									"spline",
									"line"
								)
							),
							sliderInput(
								inputId = "sea_day_dates",
								label = "Dates:",
								min = cd_range[1],
								max = cd_range[2],
								value = c(
									cd_range[1],
									cd_range[2]
								),
								timeFormat="%b %d"
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
								label = "Number of Points to Sample",
								min = 0,
								max = 25548,
								value = 5000,
								step = 100
							)
						),
						mainPanel(
							plotlyOutput("sea_day_plot", height = 550),
							textOutput("msg")
						)
					)
				),
				
				tabPanel(
					"By Month",
					br(),
					sidebarLayout(
						sidebarPanel(),
						mainPanel()
					)
				),
				
				tabPanel(
					"By Year",
					br(),
					sidebarLayout(
						sidebarPanel(),
						mainPanel()
					)
				)
			)
		),
		
		fluid = TRUE,
		inverse = TRUE,
		shinyjs::useShinyjs()
	)
)