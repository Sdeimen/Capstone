# MS Information Capstone 
# Sebastian Deimen
# STEM opportunities in the US
# University of Arizona - Spring 2021
#
# Purpose: Finding assosiations in the data set

library(tidyverse)
library(arules)
library(arulesViz)

setwd("H:/MyDropbox/Dropbox/A_UofA/INFO 698/Capstone")

load("data/opps.RData")
glimpse(opps_for_ohe)
# Rule mining preprocessing ----

# Selecting non-correlated and 
opps_trans_pre <- select(opps_for_ohe, -c(cost, locationState,reach, locationPostalCode, OpportunityID, title, description, excerpt, fromDate, toDate, locationLongitude, locationLatitude))

opps_trans_pre <- cbind(opps_trans_pre, duration = opps_inspect$duration_in_h_group, cost = opps_inspect$cost)
glimpse(opps_trans_pre)
# factorize
opps_trans_fct <- data.frame(lapply(opps_trans_pre, as.factor))
glimpse(opps_trans_fct)

opps_trans <- as(opps_trans_fct, "transactions")
colnames(opps_trans)

inspect(opps_trans[1:10])

# check on a plot with a minsup of 0.5.
itemFrequencyPlot(opps_trans, support=0.5, cex.names=0.8)
# Result: most of the items in there are not interesting because the majority is the "not there"-0-value, which does not give any information. 

# -> try sth: try to delete all columns ending in "=0":
opps_trans <- select(opps_trans, -"Ages_All.ages=0")
# does NOT work


# Mine Sets ----

opps_sets <- apriori(opps_trans, parameter = list(support=0.6, confidence=0.9, target="closed frequent itemsets"))
summary(opps_sets)                 
         
inspect(head(opps_sets, n = 5, by = "support"))

is.closed(opps_sets)
# yes, all TRUE

subset(opps_sets, subset=(items %in% "L_English=1"))
 
# Mine Rules ----
# crashes on a win10 16gb ram machine with no limit adjustment, runs on 32gb win10 machine

# maybe de-selecting all the '=0' columns may help
# making a list of all column names NOT containing "...=0"
ishot <- as.list(colnames(opps_trans))
ishot  <- ishot[grep("=0", ishot, invert=TRUE)] 

# Rule mining with this restricted set
opps_rules <- apriori(opps_trans, parameter = list(support=0.2, confidence=0.6, target="rules", minlen=2), appearance = list(both=ishot))
# it does help, choosing lower support and confidence 

inspect(head(opps_rules, n=5, by="support"))

opps_max_rules<- subset(opps_rules, subset=is.maximal(opps_rules))

# safe some storage
rm(opps_rules)

inspect(head(subset(opps_max_rules, subset=(rhs %in% "Att_Boys=1")))) # | rhs %in% "race=H"),  by="lift")))






inspect(rules[1:10])
maximal_sets <- apriori(opps_trans, parameter = list(support=0.05, target="maximally frequent itemsets"))
summary(maximal_sets)
inspect(head(maximal_sets, n = 5, by = "support")) 
