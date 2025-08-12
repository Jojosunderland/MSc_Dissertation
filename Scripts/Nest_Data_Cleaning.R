### Cleaning of Nest Data ###

# Load package #

library(tidyverse)
library(readxl)

###### LARGE NEST DATA ######

# Read Data #

nest_raw <- read_excel("Files/Eagle_data_KZ_copy.xlsx")
View(nest_raw)

# Rename column names

nest <- rename(nest_raw, "nest_id" = "Bird ID", "latitude" = "Latitude", "longitude" = "Longitude",
               "date" = "Date", "survival" = "Survival", "mortality_cause" = "Mortality_Cause",
               "age" = "Age", "sex"= "Sex", "adults" = "Adults", "subadults"="Subadult", 
               "juveniles"="Juveniles", "chicks"="Chicks", "eggs"="Eggs", "nest_status"="Nest",
               "comments"="Environmental data")

View(nest)

# Remove rows with NA in nest_status (as those are observations)

nest <- nest %>% 
  filter(!is.na(nest_status))

View(nest)

# Count the number of different nest status'

unique(nest$nest_status) # EGS: how many are there? It can be easier when you check
# back than having to run it all again :)
# JS: There are 16 listed nest status, but some are the same but vary with upper and lowercase (Old, old)

# Correct the comments so they are all formatted the same, e.g. Old and old

nest$nest_status <- tolower(nest$nest_status) 
View(nest)

# Check
unique(nest$nest_status) # JS: There are 11 unique nest status

# Add a year column

nest <- nest %>% 
  mutate(year = year(date))

# Move mortality cause into the comments

nest <- nest %>% 
  mutate(
    comments = case_when(
      !is.na(comments) & !is.na(mortality_cause) ~ paste(comments, "(Mortality Cause:", mortality_cause,")"),
      is.na(comments) & !is.na(mortality_cause) ~ paste("Mortality Cause:", mortality_cause),
      TRUE ~ comments
    ))


View(nest)
nest[405,15:16] # Checking it worked - may have printed it twice?
# EGS: did it work? # JS: Yes it worked, you just can't rerun the code or it keeps reprinting the comment!

# Manually remove the dead chicks from the count - create a 'dead' column
# EGS: perhaps we should check they are not their own count i.e. chicks = 3, 
# dead = 1, total = 4
# JS: Do you want us to check if they are already removed from the count? 
# 

nest <- nest %>% 
  mutate(dead_chick = case_when(
    grepl("dead", survival, ignore.case = TRUE) &
      grepl("chick", age, ignore.case = TRUE) ~ 1,
    TRUE ~ 0
  ))

View(nest)

nest <- nest %>% 
  mutate(alive_chicks = chicks - dead_chick)

View(nest)

# Replace NAs in counts with 0s (adults, subadults, juveniles, chicks, eggs, alive_chicks)

nest <- nest %>% 
  mutate(
    across(c(adults, subadults, juveniles, chicks, eggs, alive_chicks), ~ replace(., is.na(.), 0)))

View(nest)

# Remove unnecessary columns

nest_final <- dplyr::select(nest, nest_id, latitude, longitude, date, year, 
                            adults, subadults, juveniles, chicks, alive_chicks, 
                            eggs, nest_status, comments)

View(nest_final)

# Remove spaces in nest id

nest_final$nest_id <- str_replace_all(nest_final$nest_id, " ", "_")

##### DUPLICATES #####
## Check for duplicate nest visits in the same year and across the years ##

View(nest_final)
length(unique(nest_final$nest_id)) # 639 unique nest ids

nest_dup <- nest_final %>% 
  group_by(nest_id) %>% 
  summarise(n_years = n_distinct(year)) %>% 
  filter(n_years >1)

View(nest_dup) # 88 nests were visited more than once

nest_dup_sum <- nest_final %>% 
  filter(nest_id %in% nest_dup$nest_id) %>% 
  arrange(nest_id, year)

View(nest_dup_sum)

# Check if any nests were visited multiple times in the same year

nest_sameyear_dup <- nest_dup_sum %>% 
  group_by(nest_id, year) %>% 
  tally(name = "n_visits") %>% 
  filter(n_visits > 1)

View(nest_sameyear_dup) # 5 nests were visited twice in a year
# Need to decide what to do with these duplicates 

# Check the full rows for these nests

nest_sameyear <- nest_final %>% 
  filter(nest_id %in% nest_sameyear_dup$nest_id) %>% 
  arrange(nest_id, year)

View(nest_sameyear)

## Overview of duplicate year nests: ##

# Nest 1: has two recordings 3 weeks apart nest changes in status and no of chicks changes too
# Nest 2: identical duplicate (same date and time), duplicate row can probably be removed
# Nest 3: was the 'chick in may' duplicate, first recording has the chick present, second recording (September) counts are now 0
# could probably remove nest 3 September duplicate? 
# Nest 4: 3 days apart, there is a change in juvenile count and a new comment on rings present
# Nest 5: contradictory recordings - same timestamp, one records an adult and active nest, the other records an empty nest with nothing


## Clean up duplicates ##

# Identify identical rows

nest_final_dup <- nest_final %>% 
  group_by(across(everything())) %>% 
  filter(n() > 1)

View(nest_final_dup) # there are 3 identical/duplicate rows (including nest 2)

# Remove duplicate rows 
nest_final <- nest_final %>% 
  distinct() # 3 rows were removed 

View(nest_final) # 743 entries

# Remove the second duplicate of nest 3 (id = AN-KZ942-1_(2013_СтО27-г_СтО16г))

nest_final <- nest_final[-191,] # Remove row 191 

View(nest_final) # 742 entries

# Remove the two entries of nest 4

nest_final <- nest_final[-728,]
nest_final <- nest_final[-730,]

View(nest_final) # 740 entries

# Remove the contradictory recordings of nest 5 (id = СтО8_(СтО7-2015))

nest_final <- nest_final %>% 
  filter(!(nest_id == "СтО8_(СтО7-2015)" & year == 2015))

View(nest_final) # 738 entries

# Remove the assumed dead chick from the first nest 1 recording - leave the duplicate?
# Remove the duplicate

nest_final[313, "alive_chicks"] <- nest_final[313, "alive_chicks"] <- 1
View(nest_final) # 738 entries final sample size 

nest_final <- nest_final %>% 
  filter(!(nest_id == "AN-KZ870-1_(AN-K30)" & year == 2015 & 
             nest_status == 'active'))

View(nest_final) # 737 entries
# all duplicates are removed

##### TODD SUMMARY DATA - MANUALLY CHANGED IN EXCEL IN NEW SHEET #####

nest_sum <- read_excel("Files/Final_data_for_Emily_copy.xlsx", sheet = 2)

View(nest_sum)

# Remove spaces in the year column

nest_sum$year <- str_replace_all(nest_sum$year, " ", "_")

# Change years from year_1 to 2018-2023 (may be 2019-2024)

nest_sum <- nest_sum %>% 
  mutate(year = case_when(
    year == "year_1" ~ 2018, 
    year == "year_2" ~ 2019, 
    year == "year_3" ~ 2020, 
    year == "year_4" ~ 2021, 
    year == "year_5" ~ 2022, 
    year == "year_6" ~ 2023
  ))

View(nest_sum)

# Remove total row - which is now an NA

nest_sum <- nest_sum %>% 
  filter(!is.na(year))

View(nest_sum)

##### SUMMARISE NEST DATA TO MATCH TODD'S #####
# We want the proportion of active nests vs old nests, total chicks/eggs per year, 
# proportion of chicks in active vs old nests, with means and SDs, min and max chicks per year

View(nest_final)

year_sum_nest <- nest_final %>% 
  group_by(year) %>% 
  summarise(
    # Count active vs old nests
    n_active_nests = sum(nest_status == "active", na.rm = TRUE),
    n_old_nests = sum(nest_status == "old", na.rm = TRUE),
    # Sum of chicks and eggs across all nests
    chicks = sum(alive_chicks),
    eggs = sum(eggs),
    # Average chicks per active nest and old nest
    mean_chicks_active = mean(alive_chicks[nest_status == "active"], na.rm = TRUE),
    mean_chicks_old = mean(alive_chicks[nest_status == 'old'], na.rm = TRUE), # EGS: shouldn't this always be 0?
    # SD chicks per active and old nest
    sd_chicks_active = sd(alive_chicks[nest_status == "active"], na.rm = TRUE),
    sd_chicks_old = sd(alive_chicks[nest_status == "old"], na.rm = TRUE),
    # Repeat for eggs
    mean_eggs_active = mean(eggs[nest_status == "active"], na.rm = TRUE),
    mean_eggs_old = mean(eggs[nest_status == 'old'], na.rm = TRUE),
    # SD chicks per active and old nest
    sd_eggs_active = sd(eggs[nest_status == "active"], na.rm = TRUE),
    sd_eggs_old = sd(eggs[nest_status == "old"], na.rm = TRUE),
    # Min and max
    min_chick = min(alive_chicks, na.rm = TRUE),
    max_chick = max(alive_chicks, na.rm = TRUE),
    min_egg = min(eggs, na.rm = TRUE),
    max_egg = (max(eggs, na.rm = TRUE))
  )

View(year_sum_nest)

# After review, only active nests seem to have any counts at all # EGS: ah yes, answers my point above!
# Will re-write the above code to only calculate avg and sd for active nests but count all nest types

# EGS: agree with the re-write, currently that mean would be chicks per active nest.
# we might want to consider also chicks per occupied nest, this might need a new
# column and manual calculation of mean to be n_chicks/n_occupied. 
# JS: I just attempted to  make a new summary table with occupied nests too but only 1 nest was classed as occupied and it had
# no eggs or chicks in it, so I think it is better to stick with the active one below: 

unique(nest_final$nest_status)

year_sum_nest <- nest_final %>% 
  group_by(year) %>% 
  summarise(
    # Count nest types
    n_active_nests = sum(nest_status == "active", na.rm = TRUE),
    n_old_nests = sum(nest_status == "old", na.rm = TRUE),
    n_empty_nests = sum(nest_status == "empty", na.rm = TRUE),
    n_other_nests = sum(nest_status %in% c("destroyed", "repaired", "being built", 
                                           "burnt", "unclear", "lod", "unknown", "occupied"), na.rm = TRUE),
    total_nests = n_active_nests + n_old_nests + n_empty_nests + n_other_nests,
    # Sum of cihcks and eggs in active nests
    chicks_active = sum(alive_chicks[nest_status=="active"], na.rm = TRUE),
    eggs_active = sum(nest_final$eggs[nest_status=="active"], na.rm=TRUE),
    # Sum of chicks and eggs across all nests
    chicks_total = sum(alive_chicks, na.rm = TRUE),
    eggs_total = sum(nest_final$eggs, na.rm = TRUE),
    # Average chicks per active nest
    mean_chicks_active = mean(alive_chicks[nest_status == "active"], na.rm = TRUE),
    # SD chicks per active nest
    sd_chicks_active = sd(alive_chicks[nest_status == "active"], na.rm = TRUE),
    # Repeat for eggs
    mean_eggs_active = mean(eggs[nest_status == "active"], na.rm = TRUE),
    # SD chicks per active and old nest
    sd_eggs_active = sd(eggs[nest_status == "active"], na.rm = TRUE),
    # Min and max
    min_chick = min(alive_chicks, na.rm = TRUE),
    max_chick = max(alive_chicks, na.rm = TRUE),
    min_egg = min(nest_final$eggs, na.rm = TRUE),
    max_egg = max(nest_final$eggs, na.rm = TRUE)
  )

View(year_sum_nest)

# Summary of only active nests

active_nest_sum <- year_sum_nest %>% 
  dplyr::select(year, n_active_nests, total_nests, chicks_active, eggs_active, chicks_total, eggs_total,
                mean_chicks_active, sd_chicks_active, mean_eggs_active, sd_eggs_active, min_chick,
                max_chick, min_egg, max_egg)

View(active_nest_sum)

# Calculate proportion of active nests and active chicks and eggs

active_nest_sum <- active_nest_sum %>% 
  mutate(perc_active = n_active_nests/total_nests,
         perc_active_chicks = chicks_active/chicks_total,
         perc_active_eggs = eggs_active/eggs_total)

# Reorder columns

active_nest_sum <- active_nest_sum %>% 
  dplyr::select(year, n_active_nests, total_nests, perc_active, chicks_active, eggs_active, chicks_total, eggs_total,
                perc_active_chicks, perc_active_eggs, mean_chicks_active,
                sd_chicks_active, mean_eggs_active, sd_eggs_active, min_chick,
                max_chick, min_egg, max_egg)

View(active_nest_sum)

# Replace NaN in perc_active_chicks, mean_chicks_active and mean_eggs_active

active_nest_sum$perc_active_chicks[is.nan(active_nest_sum$perc_active_chicks)] <- 0
active_nest_sum$mean_chicks_active[is.nan(active_nest_sum$mean_chicks_active)] <- 0
active_nest_sum$mean_eggs_active[is.nan(active_nest_sum$mean_eggs_active)] <- 0



##### SAVE FILES ######

write_csv(active_nest_sum, "Active_nest_summary.csv")
write_csv(nest_final, "SteppeEagle_clean_nest.csv")
write_csv(nest_sum, "Todd_nest_summary.csv")

