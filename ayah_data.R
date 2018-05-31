# Ayah Cleaned Data
# Daily Yelp API limit 5000
library(httr)
library(jsonlite)
library(knitr)
library(dplyr)
library(ggplot2)
library(tidyr)
source("./ayah_api_keys.R")

# GET current Seattle events and details
yelp_base_uri <- "https://api.yelp.com/v3/"
yelp_resource <- "events"
yelp_query_params <- list(sort_on = "time_start", location = "seattle")
yelp_response <- GET(paste0(yelp_base_uri, yelp_resource), query = yelp_query_params, 
                  add_headers('Authorization' = paste0("Bearer ", yelp_api_key)))
yelp_body <- content(yelp_response, "text")
yelp_parsed_data <- fromJSON(yelp_body)
seattle_events <- yelp_parsed_data$events
seattle_events <- flatten(seattle_events) %>% as.data.frame() %>% 
  select(category, name, description, location.address1, time_start, time_end, cost, is_canceled, event_site_url)
colnames(seattle_events) <- c("Category", "Event Name", "Description", "Address", "Start Time", "End Time", "Cost",
                       "Has the event been canceled?", "Event Site URL")

View(seattle_events)


sygic_base_uri <- "https://api.sygictravelapi.com/1.0/en/"
sygic_resource <- "places/list"
sygic_query_params <- list(categories = "sightseeing", level = "city", lang = "en")
sygic_response <- GET(paste0(sygic_base_uri, sygic_resource), query = sygic_query_params,
                      add_headers("x-api-key" = sygic_api_key))
sygic_body <- content(sygic_response, "text")
sygic_parsed_data <- fromJSON(sygic_body)
sightseeing <- sygic_parsed_data$data$places
sightseeing <- flatten(sightseeing) %>% as.data.frame()

View(sightseeing)

"What To Wear" <- c("Bust out those flip flops and shorts cause it looks like it's not going to rain 
                  in this rainy city!", "Looks like there's a chance of rain in the rainy city. Be sure to carry around 
                  a light rain coat or umbrella as you explore the rainy city!", "Uh oh, it looks like there's a good 
                  chance of rain in the rainy city. Be sure not to leave the house without a raincoat or umbrella handy 
                  in true Seattle fashion!")

"Suggested Activities" <- c("Use this opportunity of dry weather to explore the beautiful Pacific Northwest. Go on hikes, 
                  visit the Space Needle, explore Pike Place Market, and hang out at Alki Beach.", "This is a great opportunity to visit some of the
                  indoor sights like the Museum of Pop Culture (MoPOP), the Starbucks Roastery, the Chihuly Garden 
                  and Glass Museum, the beautiful Central Public Library, or the Seattle Aquarium.", "This is a great
                  opportunity to explore the local museums and spend a relaxing vacation indoors at one of
                  Seattle's popular coffee shops like a true Seattlite.")

"Explore The City" <- c("Check out
                       https://www.tripadvisor.com/Attractions-g60878-Activities-c61-Seattle_Washington.html for a list of 
                       fun outdoor activties and https://www.switchbacktravel.com/great-day-hikes-near-seattle for a great 
                      list of day hikes.", "If it looks like it's going to rain, check out 
                      https://www.yelp.com/search?find_desc=things+to+do+on+a+rainy+day&find_loc=Seattle%2C+WA for more
                      rainy day activities.", "Check out 
                      https://seattle.eater.com/maps/essential-coffee-cafe-seattle for an extensive list of 'essential' 
                      Seattle cafes. Check out 
                      https://www.yelp.com/search?find_desc=things+to+do+on+a+rainy+day&find_loc=Seattle%2C+WA for more
                      rainy day activities.")

"Seattle's Must-See Sights" <- c("Check out this link to see Seattle's Top 25 Things To Do
                        https://www.visitseattle.org/things-to-do/sightseeing/top-25-attractions/.", "Check out this link 
                        to see Seattle's Top 25 Things To Do
                        https://www.visitseattle.org/things-to-do/sightseeing/top-25-attractions/.", "Check out this link 
                        to see Seattle's Top 25 Things To Do
                        https://www.visitseattle.org/things-to-do/sightseeing/top-25-attractions/.")

will_it_rain <- data.frame(`What To Wear`, `Suggested Activities`, `Explore The City`, `Seattle's Must-See Sights`)

forecast_day <- c(1:7)
suggestions <- function(sea_day_predict) {
if (sea_day_predict < 30){
  print(statement <- "LITTLE chance of rain today!")
  print(will_it_rain <- will_it_rain[1,])
} else if (sea_day_predict >= 30 & sea_day_predict < 60){
  print(statement <- "CHANCE of Rain!")
  print(will_it_rain <- will_it_rain[2,])
} else {
  print(statement <- "GOOD CHANCE of seeing some rain today!")
  print(will_it_rain <- will_it_rain[3,])
}
}


population <- read.csv("./seattle_population_estimates.csv", stringsAsFactors = FALSE)
population <- filter(population, NAME == "Seattle city" & COUNTY == 33)
population <- population %>% select(POPESTIMATE2010, POPESTIMATE2011, POPESTIMATE2012, POPESTIMATE2013,
                                        POPESTIMATE2014, POPESTIMATE2015, POPESTIMATE2016)
colnames(population) <- c("2010", "2011", "2012", "2013",
                          "2014", "2015", "2016")
population <- gather(population, key = year, value = population)
population <- filter(population, year != "City")
View(population)

temperature <- read.csv("./seattleWeather_2010-2016.csv", stringsAsFactors = FALSE)
temperature <- mutate(temperature, average_temp = (TMAX+TMIN)/2) %>% select(DATE, average_temp)
temperature_averages <- c((mean(temperature[1:365, 2])), (mean(temperature[366:730, 2])), (mean(temperature[731:1096, 2])),
                          (mean(temperature[1097:1461, 2])), (mean(temperature[1462:1826, 2])), (mean(temperature[1827:2191, 2])),
                          (mean(temperature[2192:2557, 2])))
year <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016)
temperature <- data.frame(year, temperature_averages)
View(temperature)

temp <- ggplot() +
  geom_point(temperature, mapping = aes(x = year, y = temperature_averages, color = temperature_averages)) 
population <- ggplot() +
  geom_point(population, mapping = aes(x = year, y = population, color = population))
