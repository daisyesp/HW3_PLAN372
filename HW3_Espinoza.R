## 
## HW3
## Daisy Espinoza
## PLAN372
##


# Loading in packages, and data
install.packages("sf")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("ggspatial")

library(sf)
library(ggplot2)
library(tidyverse)
library(ggspatial)

highways <- st_read("hw3-data/triangle_highways_osm.shp")
groups <- st_read("hw3-data/orange_durham_wake_block_groups.shp")
supermarkets <- st_read("hw3-data/triangle_supermarkets_osm.shp")

### Part I. Generating Map of Supermarkets and Food Deserts.

# To begin, I created a buffer that has the distance of 1609 meters.
supermarket_buffer <- st_buffer(supermarkets, dist = 1609)
  
st_crs(highways)
st_crs(wake_groups)

# For the food desert, I used the following to create an area that
# Before I create the food desert, I have made sure the CRS of group matches with supermarket
supermarket_buffer <- supermarket_buffer %>% 
  st_transform(st_crs(groups)) 

st_crs(groups) == st_crs(supermarket_buffer)

#To create the areas selected as food desert, I select groups to filter from. The length takes the intersections of froups and the buffer created. This is equaled to 0 so that it shows areas not in the buffer zone. 
food_desert <- groups %>% 
  filter(lengths(st_intersects(groups, st_union(supermarket_buffer))) == 0)

#FIPS code for Wake County.
wake_groups <- groups %>% 
  filter(COUNTYFP == "183")

wake_groups <- st_transform(wake_groups, crs = 4326)


# Making them all the same CRS
groups <- st_transform(groups, crs = 4326)
highways <- st_transform(highways, crs = 4326)
supermarkets <- st_transform(supermarkets, crs = 4326)
supermarket_buffer <- st_transform(supermarket_buffer, crs = 4326)
wake_groups <- st_transform(wake_groups, crs = 4326)

# ggplot map
png("food_desert_map.png", width = 800, height = 600)
ggplot()+
  geom_sf(data = highways, color = "black", alpha = 0.2)+
  geom_sf(data = wake_groups, color ="black", fill ="lightgrey")+
  geom_sf(data = supermarkets, color = "blue", size = 1)+
  geom_sf(data = supermarket_buffer, color = "green", alpha = 0.2) +
  theme_minimal() +
  labs(title = "Map of Supermarkets with 1 Mile Radius in Wake County") +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering())
dev.off()


### Part II. Identifying Areas Outside Buffer
intersection <- st_intersects(wake_groups, supermarket_buffer)

food_desert_groups <- wake_groups %>% 
  filter(lengths(intersection) == 0)

# Mapping the groups outside of the buffer
png("food_desert_buffer_map.png", width = 800, height = 600)
ggplot() +
  geom_sf(data = highways, color = "black", size = 1, alpha = 0.2) +
  geom_sf(data = food_desert_groups, color = "black", fill ="red") +
  geom_sf(data = wake_groups, color = "black", fill = "grey", alpha = 0.6) +
  geom_sf(data = supermarkets, color = "blue", size = 1)+
  geom_sf(data = supermarket_buffer, color = "green", alpha = 0.2) +
  theme_minimal()+
  labs(title = "Map of Supermarkets and Food Deserts(Shaded Red) in Wake County") +    annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering())
dev.off()

# This map shows the groups within the wake county that are located in a block that is determined as a food desert. The map locates the supermarkets (marked as blue dots) with their respective 1 mile buffer (green buffers). These markers remain so that it is more clearly noticeable that these red blocks showcase their food desert status.

### Part III. Percentage of Population in Wake County that reside in food deserts.

triangle_census <- read.csv("hw3-data/triangle_census.csv") 

#before I join the two datasets, I need to make sure that the GEOID are both in characters, because it is a numeric value in the triangle_census dataset.
triangle_census <- triangle_census %>% 
  mutate(GEOID = as.character(GEOID))

#joining the triangle census with food_desert_groups
food_desert_data <- food_desert_groups %>% 
  left_join(triangle_census, by = "GEOID")

#Calculating population percentages
#This generates the total population of groups in food deserts
food_desert_population <- sum(food_desert_data$total_population, na.rm = TRUE)

#This will generate the percentage
food_desert_percentage <- (food_desert_population/1069079)
food_desert_percentage
# The results tells us that the percentage out of the Wake County population that resides in the food desert blocks is 9.814%. This includes groups that did not touch any area of the supermarket buffers. Therefore the result can and may appear low, but this is due to the way I have decided to determine who is and is not part of the food desert status

### Part 4. Demographics

#The first section will calculate percentages for FOOD DESERT groups only.
#To begin, I will calculate the percentage of households without vehicles.

#To begin, I will calculate percentage of households with no vehicle: To do this, I evaluate the sum of households with NO vehicle, and divide it by the total number of households.
no_vehicle_percentage <- sum(food_desert_data$zero_vehicle_households, na.rm = TRUE)/sum(food_desert_data$total_households, na.rm = TRUE)
# The result is 3.90%

#Now to calculate the percentage of households with low income
low_income_percentage <- sum(food_desert_data$households_income_less_than_35k)/sum(food_desert_data$total_households)
# The result of this shows that the percentage of low income households for food desert groups is 21.398%.

#Now to calculate the percentages for the whole county of Wake:
#I need to filter the triangle_census file to just contain information on Wake County.
wake_county_census <- triangle_census %>% 
  filter(substr(GEOID, 1, 5) == "37183")

#This is the percentage of households with no vehicles for the Wake county.
wake_no_vehicle_percentage <- sum(wake_county_census$zero_vehicle_households)/sum(wake_county_census$total_households)
# This results to 3.967%.

#Percentage of low income households in Wake County.
wake_low_income_percentage <- sum(wake_county_census$households_income_less_than_35k)/sum(wake_county_census$total_households)
#This results to 18.80%.

### Part 5. Addition of a Supermarket

#To visualize the different percentages of demographics I will add a column that contains the percentage of low income for Wake County. 
food_desert_data <- food_desert_data %>% 
  mutate(
    percentage_lowincome = (households_income_less_than_35k/total_households)*100)

#Map of the low income households with percentages
png("food_desert_low_income.png", width = 800, height = 600)
ggplot()+
  geom_sf(data = food_desert_data, aes(fill = percentage_lowincome)) +
  scale_fill_viridis_c(name = "Percentage of Low Income Households") +
  labs(title = "Low Income Households in Wake County")+
  theme_minimal() +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering())
dev.off()

#Adding column that contains the percentage of no-vehicle households in Wake County
food_desert_data <- food_desert_data %>% 
  mutate(
    percentage_novehicle = (zero_vehicle_households/total_households)*100)



#Map of no-vehicle households with percentages (Wake County)
png("food_desert_no_vehicle_map.png", width = 800, height = 600)

ggplot(data = wake_county_census)+
  geom_sf(aes(fill = percentage_novehicle))+
  scale_fill_viridis_c() +
  labs(title = "Percentage of Households with No Vehicles in Food Deserts", fill = "No Vehicle Percentage") +
  theme_minimal()+
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering())
  
dev.off()




