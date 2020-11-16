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

food_consumption$vegan <- ifelse(food_consumption$food_category %in% c("Beef", "Eggs", "Fish",
                                                                       "Lamb & Goat", "Milk - inc. cheese",
                                                                       "Pork", "Poultry"), "No Vegan", "Vegan")


# food consumption in Chile

food_consumption %>% filter(country == "Chile") 

ggplot(data =food_consumption %>% filter(country == "Chile"), 
       aes(x = food_category, y = co2_emmission, fill = vegan)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  xlab("Food type") + ylab("Carbon footprint (kg)") +
  ggtitle("Carbon footprint (per person / per year)")



ggplot(data =food_consumption %>% filter(country == "Chile"), 
       aes(x = food_category, y = consumption, fill = vegan)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  xlab("Food type") + ylab("Kg food)") +
  ggtitle("Food consumption (per person / per year)")


# non vegan minus vegan total. 

chilenovegan <- food_consumption %>% filter(country == "Chile", vegan == "No Vegan") 
chilenovegansum <- sum(chilenovegan$co2_emmission)

chilevegan <- food_consumption %>% filter(country == "Chile", vegan == "Vegan")
chilevegansum <- sum(chilevegan$co2_emmission)

potential_saving_total <- chilenovegansum - chilevegansum

potential_saving_total

#If Chile stopped consuming animal products there would be potenial to save 1029.07 Kgs carbon per person per year.