library(tidyverse)
library(lubridate)
library(htmltools)
library(htmlwidgets)
library(stringr)

wholesale_price <- read_csv('tidy_wholesale_price.csv', na = c('-', 'NA'), quoted_na = FALSE)
supply_intake <- read_csv('tidy_supply_intake.csv', na = c('-', 'NA'), quoted_na = FALSE)

supply_intake <- supply_intake %>% 
  janitor::remove_empty_rows() %>% 
  select(date_reported, everything())
wholesale_price <- wholesale_price %>% 
  janitor::remove_empty_rows()

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

#This shows that 'data_from' is not needed, since it's only value is '(Yesterday)' and there is no data when 'data_from' 
  # is 'NA'
# View(wholesale_price, filter(is.na(data_from)))  
wholesale_price <- wholesale_price %>% 
  filter(!is.na(data_from)) %>%  # Removes rows containing no data
  select(-data_from) #removes the 'data_from as the only value for it is '(Yesterday)'

#an observation of with a 'supply_intake value of 0' doesn't give us any additional information, so we can remove it
#
supply_intake <- supply_intake %>% 
  filter(!is.na(supply_intake) & supply_intake != 0) 


# Sort the columns into a more logical order
wholesale_price <- wholesale_price %>% 
  select(date_reported, everything())

# Parse columns into correct types
wholesale_price$food_category <- as.factor(wholesale_price$food_category)
wholesale_price$date_morning <- dmy(wholesale_price$date_reported) - 1 #convert reported date to date that the data if from
wholesale_price$price_morning <- as.double(wholesale_price$price_morning)
wholesale_price$supply_intake <- as.double(wholesale_price$supply_intake)

supply_intake$date_morning <- dmy(supply_intake$date_reported) - 1
supply_intake$food_category <- as.factor(supply_intake$food_category)
supply_intake$supply_intake <- as.double(supply_intake$supply_intake)


supply_intake <- supply_intake %>% 
  janitor::remove_empty_rows()

wholesale_price <- wholesale_price %>% 
  janitor::remove_empty_rows()

supply_intake <- supply_intake %>% 
  select(date_morning, everything(), -data_from, -date_reported, -price_this_morning)
wholesale_price <- wholesale_price %>% 
  select(date_morning, everything(), -date_reported, -supply_intake)

#convert to kg
#Picul = 60.478982 kg / picul
#catty = .60478982 kg / catty
wholesale_price <- wholesale_price %>% 
  filter((unit == '($ / Catty)') | (unit == '($ / Picul)') | (unit == '($ / Egg)')) 


wholesale_price1 <- wholesale_price %>% 
  filter(unit == '($ / Catty)') %>% 
  mutate(price_morning =  price_morning / .60478982,
         unit = 'hkd / kg') 
  
wholesale_price2 <- wholesale_price %>% 
  filter(unit == '($ / Picul)') %>% 
  mutate(price_morning = price_morning / 60.478982,
         unit = 'hkd / kg')

wholesale_price3 <- wholesale_price %>% 
  filter(unit != '($ / Picul)' & unit != '($ / Catty)')

wholesale_price <- bind_rows(wholesale_price1, wholesale_price2, wholesale_price3)

#divide supply livestock / poultry out of supply intake, as it is the only supply_intake cat with a food_item column
intake_livestock <- supply_intake %>% 
  filter(food_category == 'Livestock / Poultry')
rename(intake_livestock, num_animals = unit)

wholesale_supply_livestock <- wholesale_price %>% 
  filter(food_category == 'Livestock / Poultry') %>% 
  inner_join(intake_livestock, by = c('date_morning', 'food_category', 'food_item')) 
#No reason to match supply_intake to price_morning with other categories as there isn't any way to know how much of each
#fish was imported and any correlation to supply 



#string manipulation on supplier & units
wholesale_price$supplier <- 
  str_replace(wholesale_price$supplier, "Major wholesalers af the Government Cheung Sha Wan and Western Wholesale 
              Food Markets", 'Major wholesalers of the Government Cheung Sha Wan and Western Wholesale Food Markets')







#EDA


#Not any correlation between price and supply intake
qplot(supply_intake, price_morning, data = wholesale_supply_livestock, color = food_item)
qplot(supply_intake, price_morning, data = wholesale_supply_livestock, color = food_item)
qplot(supply_intake, price_morning, data = filter(wholesale_supply_livestock, food_item == 'Live pig'), color = food_item)


#EDA
wholesale_price %>%
  select(-food_item) %>% 
  group_by(date_morning, food_category) %>% 
  summarize(avg_morning_price = mean(price_morning)) %>% 
  ggplot(aes(date_morning, avg_morning_price)) + 
  geom_point()   #data must be analyzed by food_category


#Livestock / Poultry must be analyzed by food_type

#Freshwater Fish
wholesale_price %>% 
  filter(food_category == 'Freshwater fish') %>% 
  ggplot(aes(date_morning, price_morning)) + 
  geom_point() + 
  facet_wrap(~food_item)

wholesale_price %>%
  select(-food_item) %>%
  filter(food_category == 'Freshwater fish') %>% 
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
  ggplot(aes(date_morning, price_morning)) + 
  geom_point() + 
  facet_wrap(~food_item)

wholesale_price %>%
  select(-food_item) %>%
  filter(food_category == 'Marine fish') %>% 
  group_by(date_morning, food_category) %>% 
  summarize(avg_morning_price = mean(price_morning)) %>% 
  ggplot(aes(date_morning, avg_morning_price)) + 
  geom_point() # there is a general trend in the price of marine fish

