# MS Information Capstone 
# Sebastian Deimen
# STEM opportunities in the US
# University of Arizona - Spring 2021
#
# Purpose: Finding important variables with decission trees

library(rpart)
install.packages("DMwR2")
library(DMwR2)
install.packages("rpart.plot")
library(rpart.plot)

setwd("H:/MyDropbox/Dropbox/A_UofA/INFO 698/Capstone/")

load("data/opps.RData")

glimpse(opps_for_ohe)

opps_tree <- opps_for_ohe[,c(-1,-2,-3,-4,-5,-6,-7,-9,-12,-13,-15)]

# bind duration and cost back 
opps_tree <- cbind(opps_tree, duration = opps_inspect$duration_in_h_group, cost = opps_inspect$cost)

# as factors:
opps_tree <- data.frame(lapply(opps_tree, as.factor))
glimpse(opps_tree)

# Store data
save(opps_total, opps, opps_for_ohe, opps_inspect, opps_tree,  file = "data/opps.RData")

# Splitting the data 75/25 in training and testing sets
set.seed(28281)
train_rows = sample(1:nrow(opps_tree), 0.75*nrow(opps_tree))
opps_tree_train = opps_tree[train_rows, ]
opps_tree_test = opps_tree[-train_rows, ]


table(opps_tree$Att_Girls)/sum(table(opps_tree$Att_Girls))
# roughly 53% of the opportunities have Girls as audience, 50% Boys, 45% GTS  whereas only 8 % have SRDoS and SwDis. 



