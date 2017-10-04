library(dplyr)
library(readr)
library(janitor)


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
    last_revision_date,
    price_this_morning
  ) %>% 
  rename(
    food_category = fresh_food_category,
    food_item = food_type,
    supply_intake = intake,
    data_from = intake_date,
    food_origin = source_of_supply_if_appropriate,
    supplier = provided_by,
    date_reported = last_revision_date
  ) 

write_csv(supply_intake, path = 'tidy_supply_intake.csv')



# Saves the filenames and modifies the strings to be the full path
file_names <- list.files(path = "original_price_data", pattern = '*.csv')
file_names <- stringr::str_c('original_price_data', file_names, sep = '/')

#Reads all .csv files, then combines all of the data frames
wholesale_price <- purrr::map(file_names, read_csv) %>% 
  bind_rows()



# Removed all Chinese data as the data is identical between columns
# Removed 'ENGLISH CATEGORY' as 'INTAKE' is only valid for the 'Supply', therefore the 'ENGLISH CATEGORY' is redundant 
wholesale_price <- wholesale_price %>% 
  select(c('FRESH FOOD CATEGORY', 
           'FOOD TYPE', 
           'PRICE (THIS MORNING)', 
           'UNIT', 
           'INTAKE DATE', 
           'SOURCE OF SUPPLY (IF APPROPRIATE)', 
           'PROVIDED BY', 
           'Last Revision Date', 
           'INTAKE'))

    
# Renames all of the variables to be more friendly
wholesale_price <- wholesale_price %>% 
  rename(food_category = 'FRESH FOOD CATEGORY', 
         food_item = 'FOOD TYPE',
         price_morning = 'PRICE (THIS MORNING)',
         unit = 'UNIT', 
         data_from = 'INTAKE DATE', 
         food_origin = 'SOURCE OF SUPPLY (IF APPROPRIATE)', 
         supplier = 'PROVIDED BY',
         date_reported = 'Last Revision Date', 
         supply_intake = 'INTAKE')

write_csv(wholesale_price, path = 'tidy_wholesale_price.csv')

