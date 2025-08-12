### Cleaning of Movebank and Igor Telemetry Data ###

# Load package #

library(tidyverse)

##### MOVEBANK DATA #####

# Read data 

telemetry_MB_raw <- read.csv("Files/SteppeEagles_Kazakhstan_copy.csv")
View(telemetry_MB_raw)

## DATA CLEANING ## 

# Select the relevant columns, event-id, timestamp, individual-id and temperature

telemetry_MB <- dplyr::select(telemetry_MB_raw, "event.id", "timestamp", "individual.local.identifier")

# Rename columns 

telemetry_MB <- rename(telemetry_MB, event = "event.id", id = "individual.local.identifier")

# Make sure the timestamp column is in proper date format, don't need hms

telemetry_MB$timestamp <- ymd_hms(telemetry_MB$timestamp)

View(telemetry_MB)

# Create a year column 

telemetry_MB <- telemetry_MB %>% 
  mutate(year = year(timestamp))

# Remove spaces in id names

telemetry_MB$id <- str_replace_all(telemetry_MB$id, " ", "_")

## Calculate age ##

# First and last detected
telemetry_MB_age <- telemetry_MB %>% 
  group_by(id) %>%
  summarise(
    first_seen = min(timestamp, na.rm = TRUE),
    last_seen = max(timestamp, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate age in months
telemetry_MB_age <- telemetry_MB_age %>%
  mutate(
    age_days = as.numeric(difftime(last_seen, first_seen, units = "days")),
    age = round(age_days / 30.44, 1) # This was converting the age from days to months
    #30.44 was the average month day, 
    
  )

View(telemetry_MB_age)

# Remove all columns but id and age

telemetry_MB_age <- dplyr::select(telemetry_MB_age, id, age) # Now we have accurate ages in months for each individual

# Filter so timestamp entries are only July - mapping annual survival postbreeding (July-July)
# Create month column

telemetry_MB <- telemetry_MB %>%
  mutate(month = month(timestamp)) 

#telemetry_MB <- telemetry_MB %>% 
# filter(month == 7)

View(telemetry_MB)

# Remove timestamp column and duplicate years for each id (as we only need to know annual survival)

telemetry_MB <- dplyr::select(telemetry_MB, "event", "id", "year", "month")

telemetry_MB <- telemetry_MB %>% 
  distinct(id, year, month) 

View(telemetry_MB)


# Classify stages and add a stage column

telemetry_MB_stage <- telemetry_MB_age %>% 
  mutate( stage = case_when(
    age <= 2 ~ "nestling", # less than 2 months
    age > 2 & age <= 12 ~ "juvenile", # 2 months - 1 yr
    age > 12 & age < 48 ~ "subadult", # 1 yr to 4 yrs
    age >= 48 ~ "adult", # 4 years +
    TRUE ~ NA_character_
  )
  )

View(telemetry_MB_stage)

# Re-classify Todd's two eagles as they are different stages (nestling and adult)
# Only Adult needs to be adjusted *

telemetry_MB_stage <- telemetry_MB_stage %>% 
  mutate(stage = case_when(
    id == "Todd_2017_Ad_F" ~ "adult",
    TRUE ~ stage
  ) )

# Adjust age too - assume Ad_F 4 yrs 
telemetry_MB_stage <- telemetry_MB_stage %>% 
  mutate(age = case_when(
    id == "Todd_2017_Ad_F" ~ 48,
    TRUE ~ age
  ) )

View(telemetry_MB_stage)


# Need to create a first seen and last seen column 

MB_first_last <- telemetry_MB %>% 
  group_by(id) %>% 
  summarise(
    first_seen = min(year),
    last_seen = max(year),
    first_seen_month = min(telemetry_MB$month[telemetry_MB$year == min(year)]),
    last_seen_month = max(telemetry_MB$month[telemetry_MB$year == max(year)]),
    .groups = "drop"
  )


View(MB_first_last)

# Adjust it to bird year - census cut off month chosen as July 
# If month first/last seen was in or before July then they were seen in the 'previous' year, 
# if after then they're the current year

census_month <- 7

MB_first_last_BY <- MB_first_last %>%
  mutate(
    # Adjust first_seen based on month and if only seen once
    first_seen = case_when(
      # Tagged after census, died before next census: first_seen stays x (not x+1)
      (first_seen_month > census_month) & (first_seen == last_seen) ~ first_seen,  
      # Tagged after census, otherwise: first_seen is x+1
      (first_seen_month > census_month) ~ first_seen + 1,                                   
      TRUE ~ first_seen
    ),
    # Adjust last_seen
    last_seen = case_when(
      # If only seen once after census: last_seen stays x
      (first_seen_month > census_month) & (first_seen == last_seen) ~ last_seen,  
      # Survived to x+1 but not beyond: last_seen is x+2
      (first_seen_month > census_month) & (first_seen == last_seen-1) ~ last_seen + 1, 
      # Last seen after census: last_seen is x+1
      (last_seen_month > census_month) ~ last_seen + 1,                                    
      TRUE ~ last_seen
    )
  )

View(MB_first_last_BY)

# Combine with telemetry_MB_stage

MB_survival <- left_join(MB_first_last_BY, telemetry_MB_stage %>% 
                           select(id, age, stage), by = "id")

View(MB_survival) # dataframe with id, first year and last year detected, age in months, and stage


# Add censor column (1 for still alive, 0 for dead)

end_year <- 2025

MB_survival <- MB_survival %>% 
  mutate(censor = ifelse(last_seen < end_year, 0, 1)
  )

View(MB_survival)

##### IGOR DATA #####
# Want to end up with a similar dataset to MB_survival #

# Read data 

telemetry_I_raw <- read.csv("Files/igor_telemetry_copy.csv",
                            skip = 2)
View(telemetry_I_raw)

## DATA CLEANING ##

# Rename columns
names(telemetry_I_raw)

telemetry_I <- rename(telemetry_I_raw, 'number' = 'X', 'id' = "Name", "tag_date" = "Data.of.marking", 
                      "country" = "Country", "sex" = "Sex",
                      "tag_year" = "Year.of.marking", 
                      "death_date" = "Month.and.year.of.death...loss...breakdown", 
                      "age" = "Age", "site" = "Country.of.death...loss...breakdown", 
                      "current_status" = "The.exact.reason.for.the.signal.death...stop.signal", 
                      "possible_cause" = "Possible.cause.of.death.if.not.exactly.known", 
                      "citation" = "Citation..data.published.")

View(telemetry_I)

# Remove empty columns and rows

telemetry_I <- filter(telemetry_I, number == 1:43) # only keep the first 43 rows which have the data in it

telemetry_I <- dplyr::select(telemetry_I, "id", "tag_date", "sex", "tag_year", "death_date",
                             "site", "age", "current_status", "possible_cause")

# Convert death date to actual date and extract year

telemetry_I <- telemetry_I %>% 
  mutate(death_date = my(death_date)) %>%  # change format to yyyy-mm-dd
  # As death date had no day it automatically has put them all to the 1st of the month
  mutate(death_year = year(death_date),
         death_month = month(death_date))  # created a new column just for the year and month

# match tag date format to the death dates 
telemetry_I <- telemetry_I %>% 
  mutate(tag_date = dmy(tag_date),
         tag_month = month(tag_date))

View(telemetry_I)

# Reorder columns

telemetry_I <- dplyr::select(telemetry_I, "id", "sex", "age", "tag_date", "death_date",
                             "site", "current_status", "possible_cause", "tag_year", "tag_month",
                             "death_year", "death_month")

# Rename age column (as need to make a tidy version of it next)

telemetry_I <- rename(telemetry_I, "age_raw" = "age")

View(telemetry_I)

# Convert age to numeric values and into months

telemetry_I <- telemetry_I %>% 
  mutate(
    age_years = as.numeric(str_extract(age_raw, "\\d+(?=\\s*y)")), # takes the value before the y
    age_months = as.numeric(str_extract(age_raw, "\\d+(?=\\s*m)")),  # takes the value before the m
    age_years = replace_na(age_years, 0), # replace NAs with 0
    age_months = replace_na(age_months, 0),
    age = age_years * 12 + age_months # create new age column of age in months
  )

# Remove the additional age columns that aren't needed now

telemetry_I <- dplyr::select(telemetry_I, "id", "sex", "age", "tag_date", "death_date",
                             "site", "current_status", "possible_cause", "tag_year", "tag_month",
                             "death_year", "death_month")

View(telemetry_I)

# Classify them into stages

telemetry_I <- telemetry_I %>% 
  mutate( stage = case_when(
    age <= 2 ~ "nestling", # less than 2 months
    age > 2 & age <= 12 ~ "juvenile", # 2 months - 1 yr
    age > 12 & age < 48 ~ "subadult", # 1 yr to 4 yrs
    age >= 48 ~ "adult", # 4 years +
    TRUE ~ NA_character_
  )
  )

View(telemetry_I)

# Create a 'flag' column for eagles that have a broken tag or gone missing

telemetry_I <- telemetry_I %>% 
  mutate(flag = case_when(
    current_status %in% c("Tracker Broken", "Missing") ~ 1,
    TRUE ~ 0
  ))

View(telemetry_I)

# Remove unnecessary columns

I_survival <- dplyr::select(telemetry_I, "id", "tag_year", "tag_month",
                            "death_year", "death_month", "age", "stage", "sex", "flag")

View(I_survival)

# Rename tag_year and death_year to first_seen and last_seen

I_survival <- I_survival %>% 
  rename("first_seen" = 'tag_year', "last_seen" = "death_year")

View(I_survival)

# Replace the NAs in the last seen column to 2025 for those still alive

I_survival <- I_survival %>% 
  mutate(last_seen = replace_na(last_seen, 2025))


# Adjust first seen and last seen to bird year (cut off July)

census_month <- 7
I_survival <- I_survival %>%
  mutate(
    # Adjust first_seen (tagged after census)
    first_seen = case_when(
      # Tagged after census & died before next census: stays x
      (tag_month > census_month) & (first_seen == last_seen) ~ first_seen, 
      # Tagged after census: bird year = x+1
      (tag_month > census_month) ~ first_seen + 1,                                
      TRUE ~ first_seen
    ),
    # Adjust last_seen (when dead, adjust by death month)
    last_seen = case_when(
      # Still alive: leave as is
      is.na(death_month) ~ last_seen,    
      # Tagged after census & died before next census: stays x
      (tag_month > census_month) & (first_seen == last_seen) ~ last_seen,  
      # Tagged after census, survived one bird year: last_seen = x+2
      (tag_month > census_month) & (first_seen == last_seen-1) ~ last_seen + 1, 
      # Died after census: last_seen = x+1
      (death_month > census_month) ~ last_seen + 1,                              
      TRUE ~ last_seen
    )
  )

# Add censor column (1 for still alive, 0 for dead)

end_year <- 2025

I_survival <- I_survival %>% 
  mutate(censor = ifelse(last_seen < end_year, 0, 1)
  )  

# Any 'Missing' individuals can be marked as alive in their last known fate and then censored 

I_survival <- I_survival %>% 
  mutate(censor = ifelse(flag == '1', 1, censor))

View(I_survival)


# Reorder censor column

I_survival <- dplyr::select(I_survival, "id", "first_seen", 
                            "last_seen", "age", "stage", 
                            "censor", "sex", "flag")

View(I_survival)

# Fill sex column blanks with NA
I_survival <- I_survival %>% 
  mutate(sex = na_if(sex, "") )

View(I_survival)


##### COMBINE THE TWO DATASETS #####

View(I_survival)
View(MB_survival)

comb_survival <- bind_rows(I_survival, MB_survival)

View(comb_survival)

# Replace NAs in flag to 0? 

comb_survival <- comb_survival %>% 
  mutate(flag = replace_na(flag, 0))

# Add sex for Todd Adult female

comb_survival <- comb_survival %>% 
  mutate(sex = case_when(
    id == "Todd_2017_Ad_F" ~ "f",
    TRUE ~ sex
  ))

View(comb_survival)

## Adjust survival for Todds adult female to four years ##

comb_survival <- comb_survival %>% 
  mutate(first_seen = case_when(
    id == "Todd_2017_Ad_F" ~ first_seen - 3,
    TRUE ~ first_seen
  ))

View(comb_survival)

# Individuals whose first seen = last seen but we know died can have last seen +1

comb_survival <- comb_survival %>% 
  mutate(last_seen = case_when(
    first_seen == last_seen & censor == 0 ~ last_seen +1, 
    TRUE ~ last_seen
  ))

# remaining first seen = last seen can be removed

comb_survival <- comb_survival %>% 
  filter(!first_seen == last_seen)


###### Create Known Fate Data Frame #####

View(comb_survival)

# convert to long format with a year column with a row for each year an eagle was seen
known_fate <- comb_survival %>% 
  group_by(id) %>% 
  rowwise() %>%
  mutate(year = list(first_seen:last_seen)) %>%
  unnest(year)

View(known_fate)

# Re-calculate age for each year an eagle was seen (age at observation = age_obs)

known_fate <- known_fate %>% 
  mutate(age_obs = age - 12 * (last_seen - year))

View(known_fate)

# Correct for any negative values in age_obs column

known_fate <- known_fate %>% 
  mutate(age_obs = if_else(age_obs < 0, 0, age_obs))

View(known_fate)

# Remove old age, stage, first_seen and last_seen columns

known_fate <- dplyr::select(known_fate, id, sex, year, age_obs, flag )

# Re-classify stages at each year for each individual (stage_obs)

known_fate <- known_fate %>% 
  mutate(stage_obs = case_when(
    age_obs <= 2 ~ "nestling", # less than 2 months
    age_obs > 2 & age_obs <= 12 ~ "juvenile", # 2 months - 1 yr
    age_obs > 12 & age_obs < 48 ~ "subadult", # 1 yr to 4 yrs
    age_obs >= 48 ~ "adult", # 4 years +
    TRUE ~ NA_character_
  ))

View(known_fate)

# Re-order columns:

known_fate <- dplyr::select(known_fate, id, sex, year, age_obs, stage_obs, flag )

View(known_fate)

## Need to add a alive/censor status for known fate ##

known_fate <- known_fate %>% 
  group_by(id) %>% 
  mutate(last_year = max(year),
         survived = case_when(
           year < last_year ~ 1,
           year == last_year & year != 2025 ~ 0,
           year == last_year & year == 2025 ~ 1
         )) %>%
  ungroup() %>% 
  mutate(survived = ifelse(flag == '1', 1, survived))

View(known_fate)

# remove last_year row and reorder 

known_fate <- dplyr::select(known_fate, id, sex, year, survived, age_obs, stage_obs, flag)

View(known_fate)

##### SAVE AS CSV #####

write_csv(comb_survival, "FINAL_ALL_SteppeEagle_survival_combined.csv") # Combined datasets summarising survival (filtered)
write_csv(known_fate, "FINAL_ALL_SteppeEagle_known_fate.csv") # Long format survival data


