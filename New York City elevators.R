library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(leaflet)

# Loading the data set

tt <- tt_load("2022-12-06")

# Creating a data frame

original <- tt$elevators

# Checking the data types

glimpse(original)

# Checking the number of NA values in columns using purrr::map

map(original, ~ sum(is.na(.)))

# Checking for outliers

ggplot(original, aes(LONGITUDE, LATITUDE))+
  geom_point() # New York/Coordinates should be 40.7128° N, 74.0060° W

original %>% 
  filter(LONGITUDE < -75) %>% 
  View() # BIN is 3129738, 40.11239 -77.51958, the correct coordinates should be 40.62630 -73.98158 (searching by 3129738 11204 1845 52 Street Brooklyn on https://www.latlong.net/)

# Checking the row number that should be updated with the correct coordinates

which(original$BIN == 3129738)

# Replacing values with the correct coordinates in a new data frame 

raw <- original
raw[64009, 28] = 40.62630
raw[64009, 29] = -73.98158

# Cross checking whether the outlier is removed

ggplot(raw, aes(LONGITUDE, LATITUDE))+
  geom_point()

# Cleaning the data by fixing zip codes, changing data types

cleaned <- raw %>%
  clean_names() %>% 
  mutate(zip_code = str_sub(as.character(na_if(zip_code, 0)), 1,5),  
         dv_speed_fpm = as.numeric(dv_speed_fpm),
         dv_floor_to = as.numeric(dv_floor_to),
         dv_floor_from = as.numeric(dv_floor_from),
         across(ends_with("_date"), ymd)) %>% 
  select(longitude, latitude, dv_device_number, device_type, dv_device_status_description,
         bin, dv_floor_from, dv_floor_to, house_number, street_name, zip_code, borough, dv_manufacturer,
         dv_speed_fpm, dv_capacity_lbs, dv_approval_date, dv_lastper_insp_date)
cleaned

# Checking the new data frame

glimpse(cleaned)
summary(cleaned)

# The summary shows that this data set contains errors in dv_floor_from, dv_floor_to and dv_speed_fpm columns.
# The building that has the highest number of floors in NYC has 104 floors while this data claims there is an elevator starts from the 300th floor and one elevator goes up until 912th floor.

# 1) What kind of elevator devices are in New York and what their distribution is?

# Distribution of device statuses

s <- cleaned %>% 
  count(dv_device_status_description, sort = TRUE) %>% 
  mutate(percent = (n/sum(n)) * 100) 

# Distribution of devices per borough

b <- cleaned %>% 
  drop_na(borough) %>% 
  count(borough, sort = TRUE) %>% 
  mutate(percent = (n/sum(n)) * 100)

# Distribution of devices types

d <-cleaned %>% 
  count(device_type, sort = TRUE) %>% 
  mutate(percent = (n/sum(n)) * 100)
  
# Creating a plot for elevator devices per borough

cleaned %>% 
  drop_na() %>% 
  ggplot(aes(longitude, latitude, color = device_type, aplha = 0.3),
         position = "identity") +
  geom_point() +
  labs( title = "Distribution of elevator devices per borough",
        caption = "source: Elevators data package by EmilHvitfeldt and NYC Department of Buildings",
        color = "Device type") +
  facet_wrap(~ borough, scales = "free") +
  scale_colour_brewer(palette = "Set1") 

# 2)	Who are the most popular elevator manufacturers in NYC?

# Checking the distribution of manufacturers.

cleaned %>% 
  filter(!is.na(dv_manufacturer)) %>% 
  count(dv_manufacturer, sort = TRUE) 

# Regrouping the manufacturers

top_manufacturers <- cleaned %>%
  mutate(manufacturer = recode(dv_manufacturer, 
                               "OTIS" = "OTIS",
                               "HOUSING AUTHORITY" = "HOUSING AUTHORITY",
                               "PRIVATE RESIDENCE" = "PRIVATE RESIDENCE",
                               "MRL ELEVATOR" = "MRL ELEVATOR",
                               "STALEY" = "STALEY",
                               "CURTIS" = "CURTIS",
                               "ARMOR" = "ARMOR",
                               "WESTINGHOUSE" = "WESTINGHOUSE",
                               "SEABERG" = "SEABERG",
                               "OTIS ELEV CO" = "OTIS",
                               .default = "Other")) 
top_manufacturers

# Distribution of top manufacturers without those ones who falls into Other category

top_manufacturer_counted <- top_manufacturers %>% 
  drop_na(dv_manufacturer) %>% 
  filter(!manufacturer %in% c("Other")) %>%  
  count(manufacturer, name = 'count', sort = TRUE)
top_manufacturer_counted

# Plotting the distribution of top manufacturers

ggplot(top_manufacturer_counted, aes(x = manufacturer, y = count, fill = count > 1000)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = count), hjust = 1.3, colour = "white", fontface = "bold") +
  scale_fill_manual(values = c("#E7B800", "#FC4E07")) +
  labs(title = "Top 9 manufacturers per devices",
       caption = "source: source: Elevators data package by EmilHvitfeldt and NYC Department of Buildings") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") 

# Distribution of top manufacturers per borough (without Other)

top_by_borough_counted <- top_manufacturers %>% 
  group_by(manufacturer, borough) %>% 
  drop_na(dv_manufacturer, borough) %>% 
  filter(!manufacturer %in% c("Other")) %>%  
  count(manufacturer, name = 'count', sort = TRUE)

top_mf_wider <- pivot_wider(top_by_borough_counted,
                            names_from = borough,
                            values_from = count)
top_mf_wider

# Plotting the distribution of top manufacturers per borough

top_by_borough_counted %>% 
  drop_na(borough) %>% 
  ggplot() +
  geom_bar(aes(x = manufacturer, y = count, fill = manufacturer), stat = 'identity') +
  labs(title = "Top 9 popular manufacturers per borough",
       caption = "source: Elevators data package by EmilHvitfeldt and NYC Department of Buildings") +
  facet_wrap(~ borough) +
  scale_fill_manual(values = c("#999999", "#000000", "#E69F00", "#56B4E9",
                               "#009E73", "#F0E442", "#0072B2", "#D55E00",
                               "#CC79A7")) +
  coord_flip() +
  theme_minimal() 

# 3) What is the relationship between elevator capacity, speed per device type?

# Capacity lbs per device type

top_manufacturers %>% 
  drop_na(dv_capacity_lbs, device_type) %>% 
  ggplot(aes(x= device_type, y= dv_capacity_lbs, fill = device_type)) + 
  geom_boxplot() +
  labs(title = "Capacity lbs per device type",
       caption = "source: Elevators data package by EmilHvitfeldt and NYC Department of Buildings",
       x = "capacity lbs", y = "device type") +
  coord_flip() +
  theme_minimal()

# Speed fpm per device type

top_manufacturers %>% 
  drop_na(dv_speed_fpm, device_type) %>% 
  ggplot(aes(x= device_type, y= dv_speed_fpm, fill = device_type)) + 
  geom_boxplot() +
  labs(title = "Speed fpm per device type",
       caption = "source: Elevators data package by EmilHvitfeldt and NYC Department of Buildings",
       x = "device type", y = "speed fpm") +
  coord_flip() +
  theme_minimal()

# Plotting the relationship between capacity and speed per top manufacturer

top_manufacturers %>%
  filter(!manufacturer %in% c("Other") & !is.na(manufacturer)) %>% 
  ggplot(aes(x = dv_capacity_lbs, y = dv_speed_fpm, color = manufacturer)) +
  geom_jitter() +
  labs(title = "Relationship between capacity and speed per top manufacturer",
       caption = "source: Elevators data package by EmilHvitfeldt and NYC Department of Buildings",
       x = "capacity lbs", y = "speed fpm") +
  scale_colour_brewer(palette = "Set1") +
  theme_minimal()

# Creating a map for active freight elevator devices above 40000 lbs capacity with known manufacturer

pal <- colorNumeric("darkgreen", domain = top_manufacturers$dv_capacity_lbs)
biggest_f <- top_manufacturers %>% 
  filter(dv_device_status_description == "ACTIVE",
         device_type == "Freight (F)",
         dv_capacity_lbs > 40000) 
leaflet(biggest_f) %>% 
  addTiles() %>% 
  addCircles(color = ~ pal(dv_capacity_lbs))
                           
# Creating a map for active passenger elevator devices above 2000 fmp speed with known manufacturer      

pal2 <- colorNumeric("red", domain = top_manufacturers$dv_speed_fpm)
biggest_f <- top_manufacturers %>% 
  filter(dv_device_status_description == "ACTIVE",
         device_type == "Passenger Elevator (P)",
         dv_speed_fpm > 2000) 
leaflet(biggest_f) %>% 
  addTiles() %>% 
  addCircles(color = ~ pal2(dv_speed_fpm))

# Creating a map for dismantled elevator devices with known manufacturer
  
dismantled <- top_manufacturers %>% 
  filter(!manufacturer %in% c("Other") & !is.na(manufacturer)) %>% 
  filter(dv_device_status_description == "DISMANTLED")
leaflet(dismantled) %>% 
  addTiles() %>% 
  addCircles(lng = ~ longitude, lat = ~ latitude)






