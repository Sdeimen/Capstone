# MS Information Capstone 
# Sebastian Deimen
# STEM opportunities in the US
# University of Arizona - Spring 2021
#
#
# loading packages
library(tidyverse)
library(dplyr)

# data cleaning and attribute selection ---- 

# bring in the data
opps_total <- read.csv("data/all_opportunities_df.csv")

# check if it is a data frame 
is.data.frame(opps_total)

# check
glimpse(opps_total)
colnames(opps_total)

# save the colnames for commenting on attribute selection
write.csv(colnames(opps_total),"colnames_opps_total.csv", row.names = FALSE)

# selecting columns - not keeping affiliations for now -> talk about it in the report
opps <- select(opps_total, title, description, fromDate, toDate, cost, costInt, ages, reach, scholarship, attention, language, locationCity, locationState, locationPostalCode, locationLatitude, locationLongitude, areaOfInterest, typeOfOpportunity, programStatus,excerpt)

# check 
colnames(opps)

# removing duplicated items and adding the OpportunityID column
opps <- distinct(opps)
opps <- tibble::rowid_to_column(opps, "OpportunityID")
glimpse(opps)

# Save opps_total and opps ----
save(opps_total, opps, file = "data/opps.RData")
# To load the data again and check
load("data/opps.RData")
glimpse(opps)
summary(opps)
View(opps)
# Next step: inspecting each attribute and decide how to take car of missing values and so forth ----
# Summmary showed a couple of double appearances of "titles" - after closer inspection it is the same opportunity but with different dates
