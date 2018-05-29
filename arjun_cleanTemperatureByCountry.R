library("dplyr")

# Reads in original csv file with Global Land temperatures by country
og_data_df <- read.csv("data/GlobalLandTemperaturesByCountry.csv", stringsAsFactors=FALSE)

# Creates a boolean vector so that its TRUE when it fins the year 2000:2012
dates_column <- og_data_df[, 1]
boolean <- grepl("2000", dates_column ) | grepl("2001", dates_column ) | grepl("2002", dates_column ) | grepl("2003", dates_column ) | grepl("2004", dates_column ) | 
  grepl("2005", dates_column ) | grepl("2006", dates_column ) | grepl("2007", dates_column ) | grepl("2008", dates_column ) | grepl("2009", dates_column ) | 
  grepl("2010", dates_column ) | grepl("2011", dates_column ) | grepl("2012", dates_column )

#Joins boolean vector to the original data frame and filters for TRUE so that we only have the 2000:2012 rows
# Also removes countrs A...land, do not know why this is in csv file but it is not a country.
joined_df <- mutate(og_data_df, "Boolean" = boolean)
joined_df <- filter(joined_df, Country != "Ã.land")
final_data_df <- filter(joined_df, Boolean == TRUE)
write.csv(final_data_df, file = "data/CleanedLandTemperaturesByCountry.csv")

# Filters the Global Temperatures dataframe so that it onlt includes years from 2000:2012, using the similar process as above
global_temp_df <- read.csv("data/GlobalTemperatures.csv", stringsAsFactors=FALSE)
global_dates_column <- global_temp_df[, 1]
boolean_global <- grepl("2000", global_dates_column ) | grepl("2001", global_dates_column ) | grepl("2002", global_dates_column ) | grepl("2003", global_dates_column ) | grepl("2004", global_dates_column ) | 
  grepl("2005", global_dates_column ) | grepl("2006", global_dates_column ) | grepl("2007", global_dates_column ) | grepl("2008", global_dates_column ) | grepl("2009", global_dates_column ) | 
  grepl("2010", global_dates_column ) | grepl("2011", global_dates_column ) | grepl("2012", global_dates_column )

global_df_with_boolean <- mutate(global_temp_df, "Boolean" = boolean_global)
final_global_data_df <- filter(global_df_with_boolean, Boolean == TRUE)
write.csv(final_global_data_df, file = "data/CleanedGlobalTemperatures.csv")

# Creates a data frame that contain averages of temperatures in a country in each year from the start of time
# Groups by country and year while summarizing this data
global_df_filtered <- global_df %>%
  filter(Country != "Ã.land") 
available_dates <- global_df_filtered$dt

# Changed that data frame so that is has a column with just the year in it and not the rest of the date, so that we can
# group in our function ahead while summarizing
years <- substr(available_dates, 1, 4)
global_df_years_added <- mutate(global_df_filtered, Years = years)

mean_by_year_df <- global_df_years_added %>%
  group_by(Country, Years) %>%
  summarize(meanByYear = mean(AverageTemperature, na.rm = TRUE)) 
write.csv(mean_by_year_df, file = "data/LandTemperaturesByCountryMean.csv")