# MS Information Capstone 
# Sebastian Deimen
# STEM opportunities in the US
# University of Arizona - Spring 2021
#
# Purpose: Plotting maps

library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
library(stringr)


load("data/opps.RData")
# for inspecting purposes I removed the three text attributes "description", excerpt" and "title"
opps_inspect <- select(opps, -title, -excerpt, -description)


# Maps ----

# install.packages(c("maps", "mapdata"))
# install.packages("Rtools")
# devtools::install_github("dkahle/ggmap")


# load mainland US, states and counties map
usa <- map_data("usa")
states <- map_data("state")
counties <- map_data("county")

# plot mainland US with the opportunities locations
ggusa <- ggplot() +
  geom_polygon(data = usa, aes(x=long, y=lat, group=group)) +
  coord_fixed(1.3)
ggusa +
  geom_point(data= opps_inspect, aes(x = locationLongitude, y = locationLatitude), color = "red", size = 1)



# opportunity 18 based in Stockholm, remove, 
opps_inspect <- opps_inspect[-c(filter(opps_inspect, locationLongitude > 0)[1,1]),]

# plot with State boundaries
ggstates <- ggplot() + 
  geom_polygon(data = states, aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend
ggstates + 
  geom_point(data= opps_inspect, aes(x = locationLongitude, y = locationLatitude), color = "red", size = 1) 

# the other two outliers are Honululu and San Jose, but not removing them, just zooming in:
# Final US map:

ggstates <- ggplot() + 
  geom_polygon(data = states, aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend
ggstates + 
  geom_point(data= opps_inspect, aes(x = locationLongitude, y = locationLatitude), color = "red", size = 1) +
  coord_fixed(xlim = c(-124, -69),  ylim = c(25, 49), ratio = 1.3)


# possibility to zoom in on US mape
  ggstates + 
  geom_point(data= opps_inspect, aes(x = locationLongitude, y = locationLatitude), color = "red", size = 2,inherit.aes = FALSE) +
  coord_fixed(xlim = c(-83, -68),  ylim = c(33, 46), ratio = 1.3)




# Plotting function, state name HAS to be a lowercase string ----
base_plots <- function(state)  {
  # get the counties
  opps_state <- filter(opps_inspect, locationState == str_to_title(state))
  state_county <- subset(counties, region == state)
  ggplot(data = state_county, mapping = aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) + 
    geom_polygon(color = "black", fill = "gray") +
    geom_point(data= opps_state, aes(x = locationLongitude, y = locationLatitude), color = "royalblue1", size = 2,inherit.aes = FALSE) +
    ggtitle(str_to_title(state))
  # zoom in a bit - longs and lats are too specific to use in a function
  # zoom_state + coord_fixed(xlim =  c(-80, -77),  ylim = c(35, 36), ratio = 1.3)
  
}
base_plots("arizona")  


# Try some plots by hand, result is the function above ----
# zoom in to a couple of states
# California ----
california_base <- subset(states, region %in% c("california"))


ggcali <- ggplot(data = california_base) +
  geom_polygon(aes(x=long, y=lat, group = group), fill = "sandybrown", color="black") + 
  coord_fixed(1.3)
# filter opps in California
opps_california <- filter(opps_inspect, locationState == "California")
ggcali + 
  geom_point(data= opps_california, aes(x = locationLongitude, y = locationLatitude), color = "royalblue1", size = 2)
# get the counties

ca_county <- subset(counties, region == "california")
ggcali_county <- ggplot(data = ca_county, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

#zoom in a bit
zoom_ca <- ggcali_county + 
  geom_point(data= opps_california, aes(x = locationLongitude, y = locationLatitude), color = "royalblue1", size = 2,inherit.aes = FALSE)
zoom_ca


zoom_ca + coord_fixed(xlim = c(-122.8, -121.5),  ylim = c(36.8, 38), ratio = 1.3)








# Ohio ----
ohio_base <- subset(states, region %in% c("ohio"))

ggohio <- ggplot() +
  geom_polygon(data = ohio_base, aes(x=long, y=lat, group = group), fill = "purple", color="black") + 
  coord_fixed(1.3)
# filter opps in Ohio
ggohio
opps_ohio <- filter(opps_inspect, locationState == "Ohio")
ggohio + 
  geom_point(data= opps_ohio, aes(x = locationLongitude, y = locationLatitude), color = "orange", size = 2)


# get the counties

ohio_county <- subset(counties, region == "ohio")
ggohio_county <- ggplot(data = ohio_county, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")


zoom_ohio <- ggohio_county + 
  geom_point(data= opps_ohio, aes(x = locationLongitude, y = locationLatitude), color = "royalblue1", size = 2,inherit.aes = FALSE)
zoom_ohio
#zoom in a bit
zoom_ohio + coord_fixed(xlim = c(-85, -84),  ylim = c(39, 40), ratio = 1.3)












# Bringing in some extra data ----

nc_pov <- read.csv("data/north_carolina_poverty.csv", colClasses=c("NULL", NA, NA))
# Colnames are named wrong, redo it
colnames(nc_pov) <- c("subregion", "percentage")
# convert the counties data to Title case
nc_counties <- subset(counties, region %in% c("north carolina"))
nc_counties$subregion <- str_to_title(nc_counties$subregion)
opps_north_carolina <- filter(opps_inspect, locationState == "North Carolina")

# inner_join
nc_complete <- inner_join(nc_counties, nc_pov, by ="subregion")
               
nc_complete



# plot
nc_base <- ggplot(data = nc_counties, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "dodgerblue4")

nc_pov_plot <- nc_base + 
  geom_polygon(data = nc_complete, aes(fill = percentage), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ggtitle(str_to_title("North caroliona by Poverty Rate")) + # adding the blue/white color 
  geom_point(data= opps_north_carolina, aes(x = locationLongitude, y = locationLatitude), color = "white", size = 1,inherit.aes = FALSE)

nc_pov_plot + scale_fill_gradientn(colours = rev(rainbow(7))) + # overwriting with easier to read color set
  geom_point(data= opps_north_carolina, aes(x = locationLongitude, y = locationLatitude), color = "black", size = 2,inherit.aes = FALSE)

# as we can see, the red/yellow and green counties with the highest poverty rates have the least offerings of opportunities


