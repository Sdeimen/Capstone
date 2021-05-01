# MS Information Capstone 
# Sebastian Deimen
# STEM opportunities in the US
# University of Arizona - Spring 2021
#
# Purpose: Getting data and clean it


# loading packages
library(tidyverse)

# data cleaning and attribute selection ---- 

# bring in the data
opps_total <- read.csv("data/all_opportunities_df_cleaned.csv")
opps_total <- select(opps_total, -c(X))
# check if it is a data frame 
is.data.frame(opps_total)

# check
glimpse(opps_total)
colnames(opps_total)

# save the colnames for commenting on attribute selection
write.csv(colnames(opps_total),"colnames_opps_total.csv", row.names = FALSE)

# checking on affiliations
table(opps_total$affiliation)
nrow(opps_total)

# selecting columns - not keeping affiliations for now -> talk about it in the report
opps <- select(opps_total, title, description, fromDate, toDate, cost, ages, reach, scholarship, attention, language, locationCity, locationState, locationPostalCode, locationLatitude, locationLongitude, areaOfInterest, typeOfOpportunity, excerpt)

opps_for_ohe <- select(opps_total, title, description, fromDate, toDate, cost, reach, scholarship, locationCity, locationState, locationPostalCode, locationLatitude, locationLongitude, typeOfOpportunity, excerpt, contains(c("Ages_","Att_","L_","AoI_")))
  
  
# check 
colnames(opps)

# removing duplicated items and adding the OpportunityID column
opps <- distinct(opps)
opps_for_ohe <- distinct(opps_for_ohe)
opps <- tibble::rowid_to_column(opps, "OpportunityID")
opps_for_ohe <- tibble::rowid_to_column(opps_for_ohe, "OpportunityID")
glimpse(opps)
glimpse(opps_for_ohe)

# Save opps_total and opps ----
save(opps_total, opps, opps_for_ohe, file = "data/opps.RData")
# Load the data again and check
load("data/opps.RData")
glimpse(opps)
summary(opps)
View(opps)


