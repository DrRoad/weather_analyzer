source("server.R")
source("arjun_analysis.R")
library("shinythemes")
shinyUI(
  navbarPage(
    "Weather Explorer",
    
    tabPanel("Ayah",
             sidebarLayout(sidebarPanel(),
                           mainPanel(
                             textOutput("example1")
                           ))),
    
    tabPanel("Arjun",
             tabsetPanel(
               # Breaks my page into tabs
               tabPanel("Summarize Data",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("givenCountry",
                                        "Choose a Country:",
                                        unique_countries),
                            selectInput(
                              "givenYear",
                              "Choose year to display",
                              c(
                                "2000",
                                "2001",
                                "2002",
                                "2003",
                                "2004",
                                "2005",
                                "2006",
                                "2007",
                                "2008",
                                "2009",
                                "2010",
                                "2011",
                                "2012"
                              )
                            )
                          ),
                          mainPanel(
                            h3("The three tabs can be used to navigate data regrading Average Land Temperatures."),
                            h4("On this tab you can choose a country and year of which you'd like to 
                               view average land temperatures. On the plot bellow you will see the country's
                               average temperature over the given year plotted in red and the global average
                               temperature plotted in green. At the bottom of the plot you will see some information
                               about the selected country in the given year."),
                            plotlyOutput("land_plot"),
                            verbatimTextOutput("average_text"),
                            #verbatimTextOutput("min_text"),
                            #verbatimTextOutput("max_text"),
                            verbatimTextOutput("global_text")
                          )
                        )),
               tabPanel("Find range of temperatures ",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              "givenYearEstimation",
                              "Choose year to display",
                              c(
                                "2000",
                                "2001",
                                "2002",
                                "2003",
                                "2004",
                                "2005",
                                "2006",
                                "2007",
                                "2008",
                                "2009",
                                "2010",
                                "2011",
                                "2012"
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
                            h4("This tool lets you select a year and a range of temperature values, and displays a table
                               with the Countries that fell into the range of the average temperatures provided. It also
                             reports countries that had the highest and lowest average temperature for that given year."),
                            verbatimTextOutput("estimation_text_min"),
                            verbatimTextOutput("esimation_text_max"),
                            tableOutput('table_with_temperature_range')
                          )
                        )),
               tabPanel(
                 "Forcast Temperature",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("givenCountryPrediction",
                                 "Choose a Country:",
                                 unique_countries),
                     selectInput(
                       "givenYearForcast",
                       "Choose year to Forcast",
                       c(
                         2000:3000
                       )
                     )
                     
                 ),
                 mainPanel(
                   h4("This tool can be used to predict average land temperatures in the future"),
                   plotlyOutput('specific_country_mean_plot'),
                   verbatimTextOutput('forcast_text'), 
                   h5("The predicted Average land temperature was calculated based on the the linear regression model ploted above. 
                      Linear regression models are based on the function of the best fit line plotted on a set of data points. The
                      standard equation of a line is y = mx + c, where m is the slope of the line and c is the y intercept. These
                      values have been calculated from the line of best fit shown in the plot. This function can then be used to 
                      predict temperatures in the future with the assumption that the increase in average land temperature with respect
                      to time will be linear as shown. While making this estimation it will most certainly have
                      some error as the data points are scattered and the line 
                      does not pass through each and every one of these points. These errors in esimation are presented bellow and 
                      do not worry if you can't understand anything past the first 2 columns! Leave those for the staticians!"),
                   tableOutput('error_table'),
                   h4("Interpeting table above:"),
                   h5("The Estimate column has two rows. The first one shows the estimate chosen for the y-intercept and
                      the second one shows the estimated value of the slope . The Std.Error shows the error in these calculated values and then the
                      t columns shows you the t-test associated with testing the significance of the parameter
                      listed in the first column! The pr(>t) column shows the probability of one getting a value higher than t")
                 )
             )))),
    
    tabPanel("April",
             sidebarLayout(sidebarPanel(),
                           mainPanel(
                             textOutput("example3")
                           ))),
    
    tabPanel("Hemil",
             sidebarLayout(sidebarPanel(),
                           mainPanel(
                             textOutput("example4")
                           ))),
    
    fluid = TRUE,
    inverse = TRUE
  )
)