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
		
		navbarMenu(
			"Exploring Seattle",
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
						checkboxGroupInput(
							inputId = "sea_day_show",
							label = "Line(s) Through:",
							choices = c(
								"Max",
								"Average",
								"Min"
							),
							selected = c(
								"Average"
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
							timeFormat="%b %d %Y"
						),
						actionButton("sea_predict_reset", "Reset to Today"),
						br(), br(),
						textOutput("sea_predict_msg"),
						br(),
						verbatimTextOutput("sea_predict_error")
					),
					mainPanel(
						plotlyOutput("sea_predict_plot", height = 650)
					)
				)
			)
		),
		
		fluid = TRUE,
		inverse = TRUE,
		shinyjs::useShinyjs()
	)
)