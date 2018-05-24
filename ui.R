source("server.R")

shinyUI(
	navbarPage(
	"Weather Explorer",
	
	tabPanel(
		"Ayah",
		sidebarLayout(
			sidebarPanel(
				verbatimTextOutput("example1")
			),
			mainPanel(
				verbatimTextOutput("example2")
			)
		)
	),
	
	tabPanel(
		"Arjun",
		sidebarLayout(
			sidebarPanel(
				verbatimTextOutput("example1")
			),
			mainPanel(
				verbatimTextOutput("example2")
			)
		)
	),
	
	tabPanel(
		"April",
		sidebarLayout(
			sidebarPanel(
				verbatimTextOutput("example1")
			),
			mainPanel(
				verbatimTextOutput("example2")
			)
		)
	),
	
	tabPanel(
		"Hemil",
		sidebarLayout(
			sidebarPanel(
				verbatimTextOutput("example1")
			),
			mainPanel(
				verbatimTextOutput("example2")
			)
		)
	),
	
	fluid = TRUE,
	inverse = TRUE
	)
)