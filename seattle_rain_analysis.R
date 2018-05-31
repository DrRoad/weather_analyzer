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
