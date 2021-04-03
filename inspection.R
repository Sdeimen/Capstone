# MS Information Capstone 
# Sebastian Deimen
# STEM opportunities in the US
# University of Arizona - Spring 2021
#
# Purpose: Inspecting the chosen variables

setwd("H:/MyDropbox/Dropbox/A_UofA/INFO 698/Capstone")
# install.packages("rockchalk")
# loading packages
library(tidyverse)
library(rockchalk) # for combinig levels

load("data/opps.RData")

# for inspecting purposes I removed the three text attributes "description", excerpt" and "title"
opps_inspect <- select(opps, -title, -excerpt, -description)
glimpse(opps_inspect)

# checking for incomplete cases
nrow(filter(opps_inspect, !complete.cases(opps_inspect)))
# it looks like 2 cases are incomplete
filter(opps_inspect, !complete.cases(opps_inspect))
# and those seems to have missing Longs and Lats, which is not super important, so I will keep those
# There are a couple of other missing data, such as "attention", but for now I will keep those as well


# adding a duration column ----
# transform the fromDate and toDate into a Date format to calculate the difference 
opps_inspect$fromDate <- as.POSIXct(opps_inspect$fromDate, format = "%Y-%m-%d %H:%M") 
#check 
opps_inspect$fromDate
opps_inspect$toDate <- as.POSIXct(opps_inspect$toDate, format = "%Y-%m-%d %H:%M")
# check
opps_inspect$toDate
#  creating the duration column
opps_inspect$duration <- as.integer(opps$toDate - opps$fromDate)
(opps_inspect$duration)
ggplot(opps_inspect, aes(x=duration)) +
  geom_histogram()

# Costs ----
# exploring costs - Intervalls: 10 Levels, two duplicates due to typos, combining them togehter
opps_inspect$cost <-  combineLevels(opps_inspect$cost, c("More than $1,000","More than $1000"), newLabel = "More than $1,000")
opps_inspect$cost <-  combineLevels(opps_inspect$cost, c("$25 or less","$25 or Less"), newLabel = "$25 or less")
opps_inspect$cost <-  combineLevels(opps_inspect$cost, c("","Free"), newLabel = "Free")

# order the levels for plotting
opps_inspect_cost_ordered <- opps_inspect
opps_inspect_cost_ordered$cost <- factor(opps_inspect_cost_ordered$cost, levels =c("Free", "$25 or less", "$26-$50", "$51-$100", "$101-$500", "$501-$1,000", "More than $1,000"))

# plot to see the distribution of opportunity costs
ggplot(opps_inspect_cost_ordered, aes(cost, fill = cost)) +
  geom_bar() +
  theme(axis.title = element_text(size = rel(1.8))) +
  labs(x = "Cost intervals") +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))

# see if costs and scholarship are somehow related

opps_inspect_cost_ordered$scholarship <-  combineLevels(opps_inspect_cost_ordered$scholarship, c("","False"), newLabel = "False")
cost_scholars <- select(opps_inspect_cost_ordered, cost, scholarship)
table(cost_scholars)


ggplot(cost_scholars, aes(cost, fill = cost, alpha = scholarship)) + 
  geom_bar() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))
# as we can see, only the price range "$100 to $500" offers a lot of scholarships




# Ages ----

ages <- opps_inspect$ages
ages

