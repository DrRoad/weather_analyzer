source("sea_key.R")

# Cleaning the Seattle CSV
seattle_rain_df <- read.csv(
	"data/seattle_weather_1948_2017.csv",
	stringsAsFactors = FALSE
)
colnames(seattle_rain_df) <- c(
	"Date",
	"Precipitation",
	"Max_Temp",
	"Min_Temp",
	"Did_Rain"
)
seattle_rain_df <- seattle_rain_df[complete.cases(seattle_rain_df),]
seattle_rain_df$Date <- as.Date(seattle_rain_df$Date)
seattle_rain_df$cd <- seattle_rain_df$Date
year(seattle_rain_df$cd) <- 2000
cd_range <- range(seattle_rain_df$cd)

rows_to_fix <- seattle_rain_df %>%
	filter(Min_Temp > Max_Temp) %>%
	select(Date)

corrected <- seattle_rain_df[seattle_rain_df$Date == rows_to_fix$Date,]
temp <- corrected$Max_Temp
corrected$Max_Temp <- corrected$Min_Temp
corrected$Min_Temp <- temp

seattle_rain_df[seattle_rain_df$Date == rows_to_fix$Date,] <- corrected

# Reading in the previously collected 2018 API data
api_2018 <- read.csv(
	"data/api_data_2018.csv",
	stringsAsFactors = FALSE
)
api_date_range <- as.Date(range(api_2018$time))

# I have already ran this function once to generate 2018 csv data, but running the
# below line will generate a csv for the current year, even if it is not 2018:
			# generate_csv_from_api(year(Sys.time()))

generate_csv_from_api <- function(input_year){
	get_request <- function(full_uri){
		response <- GET(full_uri)
		body <- content(response, "text")
		output <- fromJSON(body)
		return(output)
	}
	
	get_sea_prediction <- function(timedate){
		sea_lat <- 47.6062
		sea_long <- -122.3321
		timedate <- paste(round(as.numeric(as.POSIXct(timedate))))
		
		d <- get_request(paste0(
			"https://api.darksky.net/forecast/", sea_key, "/",
			sea_lat,
			",",
			sea_long,
			",",
			timedate,
			"?exclude=currently,minutely,hourly"
		))$daily$data
		
		return(c(
			"time" = round(d$time),
			"max_temp" = d$temperatureHigh,
			"min_temp" = d$temperatureLow,
			"pct_precip" = d$precipProbability,
			"inch_precip" = 24 * d$precipIntensity
		))
	}
	
	get_noon <- function(day, timezone){
		hour(day) <- 12
		second(day) <- minute(day) <- 0
		tz(day) <- timezone
		return(as.POSIXct(day))
	}
	
	get_year_of_days <- function(day){
		full_year <- seq(
			day,
			day + years(1),
			by='days'
		)
		
		full_year <- full_year[-length(full_year)]
		
		get_noon(
			full_year,
			o$timezone
		)
	}
	
	df_out <- bind_rows(
		lapply(
			lapply(
				get_year_of_days(mdy(paste0("0101", input_year))),
				get_sea_prediction
			),
			as.data.frame.list
		)
	)
	
	df_out$time <- as.Date(as.POSIXct(dfd$time, origin = "1970-01-01"))
	df_out$year <- year(dfd$time)
	df_out$month <- month(dfd$time)
	df_out$day <- day(dfd$time)
	
	filename <- paste0("data/api_data_" + input_year + ".csv")
	write.csv(df_out, file = filename)
}