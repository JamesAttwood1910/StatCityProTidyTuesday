#Load data

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')

eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

#Packages 

library(hablar)
library(dplyr)
library(ggplot2)
library(gt)

# Filter and clean data

head(eruptions) 

data <- eruptions %>% select(volcano_name, eruption_category, start_year) %>% hablar::convert(chr(3)) %>%
  count(volcano_name, eruption_category, sort = T) %>% filter(eruption_category == "Confirmed Eruption") %>%
  top_n(10)

# Colors 

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# graph data 

ggplot(data = data, aes(x = as.factor(volcano_name), y = n)) + 
  geom_bar(stat = "identity", fill = "#F0E442", color = "black") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle(html("The 10 most active volcanoes")) + xlab("Volcano") + ylab("Confirmed erruptions")


ggplot(data = data, aes(x = as.factor(volcano_name), y = n)) + 
  geom_bar(stat = "identity", fill = "#F0E442", color = "black") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle("Los diez volcanes más activos") + xlab("Volcanes") + ylab("Erupciones confirmadas")


# Table 

top10 <- levels(as.factor(data$volcano_name))

volcano %>% filter(volcano_name %in% top10) %>% select(volcano_name, primary_volcano_type, last_eruption_year,
                                                       country, latitude, longitude, population_within_5_km) %>%
  
  gt() %>%
  tab_header(
    title = html("<h3>The 10 most active volcanoes</h3>")
  )  %>%
  cols_label(volcano_name = md("**Volcano**"), 
             primary_volcano_type = md("**Primary volcano type**"),
             last_eruption_year = md("**Last erruption**"),
             country = md("**Country**"),
             latitude = md("**Latitude**"),
             longitude = md("**Longitude**"),
             population_within_5_km = md("**5km population**")
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "yellow", alpha = 0.4)
    ),
    locations = cells_body(rows = c(1,3,5,7,9))
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "gold", alpha = 0.7)
    ), 
    locations = cells_title(groups = c("title"))

) %>% 
  tab_style(
    style = list(
      cell_fill(color = "gold", alpha = 0.7)
    ), 
    locations = cells_title(groups = c("subtitle"))
  )




volcano %>% filter(volcano_name %in% top10) %>% select(volcano_name, primary_volcano_type, last_eruption_year,
                                                       country, latitude, longitude, population_within_5_km) %>%
  
  gt() %>%
  tab_header(
    title = html("<h3>Los 10 volcanes más activos</h3>")
  )  %>%
  cols_label(volcano_name = md("**Volcán**"), 
             primary_volcano_type = md("**Primer tipo de volcán**"),
             last_eruption_year = md("**Última erupción**"),
             country = md("**País**"),
             latitude = md("**Latitud**"),
             longitude = md("**Longitud**"),
             population_within_5_km = md("**Población en 5 kilómetros**")
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "yellow", alpha = 0.4)
    ),
    locations = cells_body(rows = c(1,3,5,7,9))
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "gold", alpha = 0.7)
    ), 
    locations = cells_title(groups = c("title"))
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "gold", alpha = 0.7)
    ), 
    locations = cells_title(groups = c("subtitle"))
  )







