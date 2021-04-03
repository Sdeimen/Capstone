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

# opportunity 18 based in Stockholm, remove, the other two outliers are Honululu and San Jose
opps_inspect <- opps_inspect[-c(18),]

# plot with State boundaries
ggstates <- ggplot() + 
  geom_polygon(data = states, aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend
ggstates + 
  geom_point(data= opps_inspect, aes(x = locationLongitude, y = locationLatitude), color = "red", size = 1)


# possibility to zoom in on US mape
zoom_us <- ggstates + 
  geom_point(data= opps_inspect, aes(x = locationLongitude, y = locationLatitude), color = "royalblue1", size = 2,inherit.aes = FALSE)
zoom_us
zoom_us + coord_fixed(xlim = c(-83, -68),  ylim = c(33, 46), ratio = 1.3)


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




# Plotting function, state name HAS to be a lowercase string ----
base_plots <- function(state)  {
  base <- subset(states, region %in% state)
  ggstate <- ggplot() +
    geom_polygon(data = base, aes(x=long, y=lat, group = group), fill = "purple", color="black") + 
    coord_fixed(1.3)
  # filter opps in Ohio
  opps_state <- filter(opps_inspect, locationState == str_to_title(state))
  gg_color <- ggstate + 
                geom_point(data= opps_state, aes(x = locationLongitude, y = locationLatitude), color = "orange", size = 2)
  gg_color
  
  # get the counties
  
  state_county <- subset(counties, region == state)
  ggstate_county <- ggplot(data = state_county, mapping = aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) + 
    geom_polygon(color = "black", fill = "gray")
  
  zoom_state <- ggstate_county + 
    geom_point(data= opps_state, aes(x = locationLongitude, y = locationLatitude), color = "royalblue1", size = 2,inherit.aes = FALSE)
  zoom_state
  # zoom in a bit - longs and lats are too specific to use in a function
  # zoom_state + coord_fixed(xlim =  c(-80, -77),  ylim = c(35, 36), ratio = 1.3)
  return(c(gg_color, zoom_state))
  }


