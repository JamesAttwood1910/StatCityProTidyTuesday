# Script - Food Carbon Footprints

#The below script details the data cleaning and visualization process for the 
#following data set: 

#https://www.nu3.de/blogs/nutrition/food-carbon-footprint-index-2018 
#https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-02-18/readme.md 


# Data

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

#Packages 

library(hablar)
library(dplyr)
library(ggplot2)
library(countrycode)
library(scales)

# Inital look at data
str(food_consumption)
summary(food_consumption)


#Create new Vegan or no Vegan variable

food_consumption$animal_product <- ifelse(food_consumption$food_category %in% c("Beef", "Eggs", "Fish",
                                                                       "Lamb & Goat", "Milk - inc. cheese",
                                                                       "Pork", "Poultry"), "Yes", "No")


# food consumption in Chile

food_consumption %>% filter(country == "Chile") 

ggplot(data =food_consumption %>% filter(country == "Chile"), 
       aes(x = food_category, y = co2_emmission, fill = animal_product)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  xlab("Food type") + ylab("Carbon footprint (kg)") +
  ggtitle("Chile carbon footprint (per person / per year)") + labs(fill = "Animal product")



ggplot(data =food_consumption %>% filter(country == "Chile"), 
       aes(x = food_category, y = consumption, fill = animal_product)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  xlab("Food type") + ylab("Kg food)") +
  ggtitle("Chile food consumption (per person / per year)") + 
  labs(fill = "Animal product")


# Animal products minus non animal products total. 

chileAnimalProducts <- food_consumption %>% filter(country == "Chile", animal_product == "Yes") 
chileAnimalProductsSum <- sum(chileAnimalProducts$co2_emmission)

chileNoAnimalProducts <- food_consumption %>% filter(country == "Chile", animal_product == "No")
chileNoAnimalProductsSum <- sum(chileNoAnimalProducts$co2_emmission)

potential_saving_total <- chileAnimalProductsSum - chileNoAnimalProductsSum

potential_saving_total

#If Chile stopped consuming animal products there would be potenial to save 1029.07 Kgs carbon per person per year.