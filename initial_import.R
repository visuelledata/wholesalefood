library(tidyverse)
library(janitor)
library(stringr)


### This script is to do initial import and initial cleaning of the data, then re-export it.

#Picul = 60.478982 kg
#catty = .60478982 kg



file_names_supply <- list.files(path = 'original_supply_data', pattern = '*.csv')
file_names_supply <- stringr::str_c('original_supply_data', file_names_supply, sep = '/')

supply_intake <- purrr::map(file_names_supply, read_csv) %>% 
  bind_rows() %>% 
  clean_names() %>% 
  remove_empty_rows() %>% 
  remove_empty_cols() %>% 
  select(
    fresh_food_category,
    food_type,
    intake,
    unit,
    intake_date,
    source_of_supply_if_appropriate,
    provided_by,
    last_revision_date
  ) %>% 
  rename(
    food_category = fresh_food_category,
    food_item = food_type,
    supply_intake = intake,
    data_from = intake_date,
    food_origin = source_of_supply_if_appropriate,
    date_reported = last_revision_date
  ) 

#parse columns to correct types
supply_intake$date_morning <- dmy(supply_intake$date_reported) - 1
supply_intake$food_category <- as.factor(supply_intake$food_category)
supply_intake$supply_intake <- as.double(supply_intake$supply_intake)


# Sort the columns into a more logical order
supply_intake <- supply_intake %>% 
  select(date_morning, everything()) %>% 
  select(-data_from, -date_reported) #all data is from 'yesterday'


supply_intake <- supply_intake %>% 
  remove_empty_rows() 

supply_intake <- supply_intake %>% 
  filter(!is.na(supply_intake) & supply_intake != 0) #removes row with missing values and days with no intake
# which is not useful information

write_csv(supply_intake, path = 'tidied_supply_intake.csv')





# Saves the filenames and modifies the strings to be the full path
file_names <- list.files(path = "original_price_data", pattern = '*.csv')
file_names <- stringr::str_c('original_price_data', file_names, sep = '/')

#Reads all .csv files, then combines all of the data frames
wholesale_price <- purrr::map(file_names, read_csv) %>% 
  bind_rows() %>% 
  clean_names() %>% 
  remove_empty_rows() %>% 
  remove_empty_cols() %>% 
  select(
    fresh_food_category,
    food_type,
    unit,
    intake_date,
    source_of_supply_if_appropriate,
    provided_by,
    last_revision_date,
    price_this_morning
  ) %>% 
  rename(
    food_category = fresh_food_category,
    food_item = food_type,
    price_morning = price_this_morning,
    data_from = intake_date,
    food_origin = source_of_supply_if_appropriate,
    date_reported = last_revision_date
  )

# Parse columns into correct types
wholesale_price$food_category <- as.factor(wholesale_price$food_category)
wholesale_price$date_morning <- dmy(wholesale_price$date_reported) - 1 #convert reported date to date that the data if from
wholesale_price$price_morning <- as.double(wholesale_price$price_morning)

# Sort the columns into a more logical order
wholesale_price <- wholesale_price %>% 
  select(date_morning, everything()) %>% 
  select(-data_from, -date_reported) #all data is from 'yesterday'


#string manipulation on supplier & units
wholesale_price$provided_by <- 
  str_replace(wholesale_price$provided_by, "Major wholesalers af the Government Cheung Sha Wan and Western Wholesale 
              Food Markets", 'Major wholesalers of the Government Cheung Sha Wan and Western Wholesale Food Markets')

#removes rows with no price information
wholesale_price <- wholesale_price %>% 
  filter(!is.na(price_morning)) 



write_csv(wholesale_price, path = 'tidied_wholesale_price.csv')

wholesale_price <- read_csv('tidied_wholesale_price.csv', na = c('-', 'NA'), quoted_na = FALSE)
supply_intake <- read_csv('tidied_supply_intake.csv', na = c('-', 'NA'), quoted_na = FALSE)


supply_intake <- supply_intake %>% 
  janitor::remove_empty_rows() %>% 
  select(date_reported, everything())
wholesale_price <- wholesale_price %>% 
  janitor::remove_empty_rows()
