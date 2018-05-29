library("dplyr")

land_temp_df <- read.csv("data/CleanedLandTemperaturesByCountry.csv", stringsAsFactors=FALSE)

# Reads the cleaned global temperatures csv file and selects the global average temperature, date columns 
global_temperature_df <- read.csv("data/CleanedGlobalTemperatures.csv", stringsAsFactors=FALSE)
global_temperature_df <- select(global_temperature_df, dt, LandAverageTemperature, Boolean)
countries <- land_temp_df[, 5]

# Creates a vector with all the country names
unique_countries <- unique(countries, incomparables = FALSE)

# Creates a vector with the range of temperature values 
temperature_range <- range(land_temp_df$AverageTemperature, na.rm = TRUE)

# Creates a data frame that countains averages of temperatures in a country in each year from the start of time
# Groups by country and year while summarizing this data
mean_by_year_df <- read.csv("data/LandTemperaturesByCountryMean.csv", stringsAsFactors=FALSE)


