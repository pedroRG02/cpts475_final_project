install.packages("usmap")
install.packages(c('flexdashboard',
                   'highcharter',
                   'gt',
                   'htmltools',
                   'viridis'))
install.packages(c('sf',
                   'tmap',
                   'tmaptools',
                   'leaflet'))
install.packages('DT')
install.packages("shiny")
library(tidyverse)
library(maps)
library(mapproj)
library(usmap)
library(ggplot2)
library(flexdashboard)
library(highcharter)
library(gt)
library(htmltools)
library(viridis)
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(DT)
library(shiny)

lungcancer_1 = read.csv("lungcancer_inc_per100k_pop_2015_2019.csv", sep = ",", header = TRUE)
lungcancer_race = read.csv("lungcancer_inc_per100k_pop_2015_2019_race.csv", sep = ",", header = TRUE)
lungcancer_gender = read.csv("lungcancer_inc_per100k_pop_2015_2019_gender.csv", sep = ",", header = TRUE)
livercancer_1 = read.csv("livercancer_inc_per100k_pop_2015_2019.csv", sep = ",", header = TRUE)
livercancer_race = read.csv("livercancer_inc_per100k_pop_2015_2019_race.csv", sep = ",", header = TRUE)
livercancer_gender = read.csv("livercancer_inc_per100k_pop_2015_2019_gender.csv", sep = ",", header = TRUE)
airquality_1 = read.csv("air_quality_pm2.5annualavg_bycounty_2018_2019.csv", sep = ",", header = TRUE)
arsenic = read.csv("arsenic_annual_mean_conc_2018_2019.csv", sep = ",", header = TRUE)
drinking = read.csv("binge_drinking_alcohol_adults_per100k_pop_2018_2019.csv", sep = ",", header = TRUE)
heart_disease = read.csv("coronary_heart_disease_per100k_pop_2018_2019.csv", sep = ",", header = TRUE)
asthma = read.csv("county_asthma_per100k_pop_2018_2019.csv", sep = ",", header = TRUE)
no_healthins = read.csv("county_noHealthIns_perc_pop_2018_2019.csv", sep = ",", header = TRUE)
diabetes = read.csv("diabetes_adults_per100k_pop_2018_2019.csv", sep = ",", header = TRUE)
obesity = read.csv("obesity_adults_per100k_pop_2018_2019.csv", sep = ",", header = TRUE)
smoking = read.csv("smoking_adults_per100k_pop_2018_2019.csv", sep = ",", header = TRUE)

###

poverty = read.csv("pop_perc_below_poverty_2018_2019.csv", sep = ",", header = TRUE)
states <- map_data("state")

names(poverty) <- tolower(names(poverty))
poverty$state <- tolower(poverty$state)
names(poverty)[names(poverty) == 'state'] <- 'region'
poverty <- spread(poverty, key = year, value = value)

poverty.geo <- merge(states, poverty, sort = FALSE, by = "region")
colnames(poverty.geo)[12] <- "value_2018"
colnames(poverty.geo)[13] <- "value_2019"
poverty.geo <- poverty.geo[order(poverty.geo$order),]

ggplot(poverty.geo, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = value_2018), colour = "black") +
  coord_map()


washington <- subset(state, region=="washington")
counties <- map_data("county")
washington_county <- subset(counties, region=="washington")

ca_map <- ggplot(data=washington, mapping=aes(x=long, y=lat, group=group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=washington_county, fill=NA, color="white") + 
  geom_polygon(color="black", fill=NA) + 
  ggtitle('Washington Map with Counties') + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

###

options(scipen = 999)

plot_usmap(regions = "counties") + 
  labs(title = "US Counties",
       subtitle = "This is a blank map of the counties of the United States.") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

###
states <- us_map(exclude = c("AK", "HI"))

livercancer_1%>%
  lapply(function(x) left_join(states, x, by = c("abbr" = "fips"))) %>% 
  lapply(function(x) tidyr::fill(x, year, .direction = "downup")) %>%
  bind_rows() %>% 
  arrange(year, group, order)

ggplot(states_data, aes(x, y, fill = value, group = group)) +
  geom_polygon() +
  scale_fill_continuous(low = "white", high = "red", name = "Ele gen (EJ)", label = scales::comma) +
  facet_grid(~year) +
  coord_equal() +
  ggthemes::theme_map() +
  theme(legend.position = "bottom")