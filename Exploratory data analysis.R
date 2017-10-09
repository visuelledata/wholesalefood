library(tidyverse)
library(lubridate)
library(htmlwidgets)

wholesale_price <- read_rds('tidied_wholesale_price.rds')
supply_intake <- read_rds('tidied_supply_intake.rds')

#rename variables appropriately
#convert everything to kg
#standardize supplier names
#rename supplier to provided_by


### Variables
# date_reported - the date that the data was reported (always 1 day following the day that the data is from)
# food_category - the overlying category of the food_item (livestock, fish, eggs, vegetables)
# food_item - the name of the food
# price_morning - the average morning price per unit that the food_item is sold at
# food_origin - whether or not the food is from Mainland China of local to Hong Kong
# supplier - the food seller 
# supply_intake - how much supply was taken in on that day (unsure; very few obs)


## Supply intake
# no food_item for specific fish
# unit - 
# nos = numbers

#divide supply livestock / poultry out of supply intake, as it is the only supply_intake cat with a food_item column
intake_livestock <- supply_intake %>% 
  filter(food_category == 'Livestock / Poultry')
rename(intake_livestock, num_animals = unit)

wholesale_supply_livestock <- wholesale_price %>% 
  filter(food_category == 'Livestock / Poultry') %>% 
  inner_join(intake_livestock, by = c('date_morning', 'food_category', 'food_item')) 
#No reason to match supply_intake to price_morning with other categories as there isn't any way to know how much of each
#fish was imported 

#EDA


#Not any correlation between price and supply intake
qplot(supply_intake, price_morning, data = wholesale_supply_livestock, color = food_item)
qplot(supply_intake, price_morning, data = wholesale_supply_livestock, color = food_item)
qplot(supply_intake, price_morning, data = filter(wholesale_supply_livestock, food_item == 'Live pig'), color = food_item)
#we dont have price data on multiple supply days hence the points clustered to the left

#EDA
wholesale_price %>% 
  ggplot(aes(date_morning, price_morning, color = food_category)) + 
  geom_point() + 
  facet_wrap(~food_category, scale = 'free_y') #can see some very strong associations within the data

wholesale_price %>% 
  group_by(date_morning, food_category) %>% 
  summarize(avg_morning_price = mean(price_morning)) %>% 
  ggplot(aes(date_morning, avg_morning_price, color = food_category)) + 
  geom_point() +  #there is also a strong correlation between 'freshwater fish, marine fish, and vegetables with time
  facet_wrap(~food_category, scale = 'free_y')
#data must be analyzed by food_category


#Livestock / Poultry must be analyzed by food_type

#Freshwater Fish
wholesale_price %>% 
  filter(food_category == 'Freshwater fish') %>% 
  ggplot(aes(date_morning, price_morning)) + 
  geom_point() + 
  facet_wrap(~food_item)

wholesale_price %>%
  filter(food_category == 'Freshwater fish') %>% 
  group_by(date_morning, food_category) %>% 
  summarize(avg_morning_price = mean(price_morning)) %>% 
  ggplot(aes(date_morning, avg_morning_price)) + 
  geom_point()

#Eggs
wholesale_price %>% 
  filter(food_category == 'Eggs') %>% 
  ggplot(aes(date_morning, price_morning, color = food_item)) + 
  geom_point() 

wholesale_price %>%
  select(-food_item) %>%
  filter(food_category == 'Eggs') %>% 
  group_by(date_morning, food_category) %>% 
  summarize(avg_morning_price = mean(price_morning)) %>% 
  ggplot(aes(date_morning, avg_morning_price)) + 
  geom_point()

#Vegetables
wholesale_price %>% 
  filter(food_category == 'Vegetables') %>% 
  ggplot(aes(date_morning, price_morning)) + 
  geom_point(aes(color = food_item))

wholesale_price %>%
  select(-food_item) %>%
  filter(food_category == 'Vegetables') %>% 
  group_by(date_morning, food_category) %>% 
  summarize(avg_morning_price = mean(price_morning)) %>% 
  ggplot(aes(date_morning, avg_morning_price)) + 
  geom_point() # there is a general trend in the price of vegetables


#Marine Fish
wholesale_price %>% 
  filter(food_category == 'Marine fish') %>% 
  ggplot(aes(date_morning, price_morning)) + 
  geom_point(aes(color = food_item))

wholesale_price %>% 
  filter(food_category == 'Marine fish') %>% 
  ggplot(aes(date_morning, price_morning, color = food_item)) + 
  geom_point() + 
  facet_wrap(~food_item, scale = 'free_y')

wholesale_price %>%
  select(-food_item) %>%
  filter(food_category == 'Marine fish') %>% 
  group_by(date_morning, food_category) %>% 
  summarize(avg_morning_price = mean(price_morning)) %>% 
  ggplot(aes(date_morning, avg_morning_price)) + 
  geom_point() # there is a general trend in the price of marine fish

