source("server.R")

shinyUI(
	navbarPage(
		"Weather Explorer",
		
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
			"Hemil",
			sidebarLayout(
				sidebarPanel(),
				mainPanel(
					textOutput("example4")
				)
			)
		),
		
		fluid = TRUE,
		inverse = TRUE
	)
)