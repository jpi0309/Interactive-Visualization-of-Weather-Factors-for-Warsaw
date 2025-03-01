library(httr)
library(jsonlite)
library(dplyr)

api_url = "https://archive-api.open-meteo.com/v1/archive"

params <- list(
  latitude = "52.2298",    # Latitude for Warsaw
  longitude = "21.0118",   # Longitude for Warsaw
  start_date = "1940-01-02",    # Start date of the data range
  end_date = "2023-12-31",      # End date of the data range
  hourly = "relative_humidity_2m,precipitation,sunshine_duration,wind_speed_10m,temperature_2m,shortwave_radiation,apparent_temperature", #weather factors
  timezone = "Europe/Warsaw"
)
response <- GET(url = api_url, query = params)

if (status_code(response) == 200) {
  print("Data retrieval successful!")
  data <- content(response, "parsed")
} else {
  print(paste("Failed to retrieve data. Status code:", status_code(response)))
}

warszawa = fromJSON(rawToChar(response$content))

warszawa = as.data.frame(warszawa$hourly) %>% mutate(time = as.character(time))
write.csv(warszawa, "data.csv", row.names = FALSE)
