library(tidyverse)


# load data

temperature_raw <- read.csv("Files/Clean_Files/temperature2013_2025.csv")

View(temperature_raw)

#rename columns

temperature <- temperature_raw %>% 
  rename("time" = "valid_time", "temperature" = "t2m")

# Extract year and month

temperature <- temperature %>% 
  mutate(year = year(time),
         month = month(time))

# filter to breeding season (march-september)

temperature <- temperature %>% 
  filter(month %in% 3:9)

# calculate monthly average temperatures

temperature <- temperature %>% 
  group_by(year, month) %>% 
  mutate(month_mean = mean(temperature)) %>% 
  ungroup()

# Remove time, raw temp, lat and long columns

temperature <- temperature %>% 
  select(year, month, month_mean)

# remove monthly duplicates

temperature <- temperature %>% 
  distinct(year, month, month_mean)

# Calculate annual mean temp 

temperature <- temperature %>% 
  group_by(year) %>% 
  mutate(mean_temp = mean(month_mean)) %>% 
  ungroup()

# remove monthly mean

temperature <- temperature %>% 
  select(year, mean_temp)

# remove duplicates

temperature <- temperature %>% 
  distinct(year, mean_temp)

# convert to celcius from kelvin

temperature <- temperature %>% 
  mutate(mean_temp_C = (mean_temp - 273.15))

View(temperature)

write_csv(temperature, "temperature_clean.csv")
         