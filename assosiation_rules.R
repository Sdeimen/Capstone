# MS Information Capstone 
# Sebastian Deimen
# STEM opportunities in the US
# University of Arizona - Spring 2021
#
# Purpose: Finding associations in the data set

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
opps_rules <- apriori(opps_trans, parameter = list(support=0.05, confidence=0.8, target="rules", minlen=2), appearance = list(both=ishot))
# it does help, choosing lower support and confidence 
summary(opps_rules)
inspect(head(opps_rules, n=10, by="support"))

opps_max_rules<- subset(opps_rules, subset=is.maximal(opps_rules))
# safe some storage
rm(opps_rules)

# adding Kulczynski as measure 
quality(opps_max_rules) <- cbind(quality(opps_max_rules),
                                 kulc = interestMeasure(opps_max_rules, measure = "kulczynski",
                                                        transactions = opps_trans),
                                 imbalance = interestMeasure(opps_max_rules, measure ="imbalance",
                                 transactions = opps_trans))

# sort ALL rules 

inspect(head((sort(opps_max_rules, decreasing = TRUE, by = c("kulc", "imbalance"))),n=15))
# the first 12 rules all point to four_to_seven days period

## Checking the Rules, lots of room for interpretation ############################################################
# search for SRDoS- rules:
inspect(head(subset(opps_max_rules, subset=(lhs %in% "Att_SRDoS=1" )),n=15,by="lift"))
# no rules at all for Students with risk dropping out of school with the setting support=0.2 and conf = 0.8. Lowering the support threshold to 0.05 
# gives 20 rules. Those show a Kulczynski close to 0.5 and a positive correlated lift but an imbalance close to 1, which means 
# we have very skewed data and our SRDoS is probably a small class
# -> this shows that there are no rules focusing solely on Disability Students, it only appears together with all other five attention groups
inspect(head(subset(opps_max_rules, subset=(lhs %in% "Att_SRDoS=1" & !(rhs %in% "L_English=1"))),n=15, by="lift"))

# it appears that SRDoS appears togehter with Girls more often than with boys
inspect(head(subset(opps_max_rules, subset=(lhs %in% "Att_SRDoS=1" & !(lhs %in% c("Att_Girls=1")))),n=15,by="lift"))

# checking on girls, excluding boys on the rhs
inspect(head(subset(opps_max_rules, subset=(lhs %in% "Att_Girls=1" & !(rhs %in% "Att_Boys=1"))),n=10))


# creating an Areo of Interest vector 
aoi <- as.vector(unlist(ishot[248:267]))
aoi
# checking on girls, focusing the rhs on Area Of Interest:
inspect(head(subset(opps_max_rules, subset=(lhs %in% "Att_Girls=1" & rhs %in% aoi)),n=15))

inspect(head(subset(opps_max_rules, subset=(lhs %in% "Att_Girls=1" & !(lhs %in% aoi) & rhs %in% aoi)),n=15))

# see if there are any rules explicitly for girls
inspect(head(subset(opps_max_rules, subset=(lhs %in% "Att_Girls=1" & !(lhs %in% "Att_Boys=1") & !(lhs %in% "Att_GTS=1") & rhs %in% aoi)),n=15))
# looks like there area bunch of rules emphasizing that girls are combined with Coding/ComputerScience among others. Excluding AoI on the lhs gives no results. 

inspect(head(subset(opps_max_rules, subset=(lhs %in% "Att_SwDis=1")),n=15, by="lift"))
# -> this shows again that there are no rules focusing solely on Disability Students, it only appears together with all other five attention groups

inspect(head(subset(opps_max_rules, subset=(rhs %in% "Att_SwDis=1")),n=15, by="lift"))

# Plotting some rules
girls_sub <-  subset(opps_max_rules, subset=(lhs %in% "Att_Girls=1" & rhs %in% aoi))
girls_sub_no_aoi <- subset(opps_max_rules, subset=(lhs %in% "Att_Girls=1" & !(lhs %in% aoi) & rhs %in% aoi))

plot(girls_sub, measure = "imbalance", shading="kulc", method = 'grouped')
plot(girls_sub, measure = "confidence", shading="kulc", method = 'grouped')

############## example on how to check on rule length
#select rules based on rule length
inspect(head(subset(rules, subset= (size(lhs)<5 & size(lhs) >1) & !(rhs %in% "gender=M")), 10, by="support"))






