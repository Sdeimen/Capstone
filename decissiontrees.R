# MS Information Capstone 
# Sebastian Deimen
# STEM opportunities in the US
# University of Arizona - Spring 2021
#
# Purpose: Finding important variables with decision trees
library(stringr)
library(tidyverse)
library(rpart)
library(DMwR2)
library(rpart.plot)
library(rockchalk) # for combining levels
library(caret) # for data partition, if needed, not sure yet
library(randomForest)
setwd("H:/MyDropbox/Dropbox/A_UofA/INFO 698/Capstone/")

load("data/opps.RData")
glimpse(opps_for_ohe)
glimpse(opps_tree)

#to get the score of the tree, use this function: 
get_score <- function(model, data){ p = predict(model, data, type="class")
  #compare predicted Girls with the true Girls
  result <- table(p, data$Att_Girls)
  accurracy <- sum(diag(result))/sum(result)
  return (accurracy)
}




# The first tree first split on a first tree was on Att_None, which is not interesting and two splits on locationPostalCode, but Postalcode is a high
# cardinality attribute, so delete as well
opps_tree <- opps_for_ohe[,c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-12,-13,-15,-11,-26)]

# Code some of the TypeOfOpportunities, the tree seems to like them and they are hard to read -> Note: did not solve the issue, but still leave it
opps_tree$typeOfOpportunity <-  combineLevels(opps_tree$typeOfOpportunit, c("Afterschool","Afterschool Program"), newLabel = "Afterschool")
opps_tree$typeOfOpportunity <-  as.factor(str_replace_all(opps_tree$typeOfOpportunity, c("Citizen Science" = "CitSci",
                                                                              "Community Event" = "CommEvt",
                                                                              "Conference/Workshop" = "Conf/WS",
                                                                              "Fair/Festival" = "F/F",
                                                                              "Online Learning" = "Online")))

# bind discretized and cleaned duration and cost back 
opps_tree <- cbind(opps_tree, duration = opps_inspect$duration_in_h_group, cost = opps_inspect$cost)

# as factors:
opps_tree <- data.frame(lapply(opps_tree, as.factor))
glimpse(opps_tree)

# Store data
save(opps_total, opps, opps_for_ohe, opps_inspect, opps_tree,  file = "data/opps.RData")


# Splitting the data 75/25 in training and testing sets
#split_index <- createDataPartition(opps_tree$Att_Girls, p= 0.75, list = F)
#opps_tree_train <- opps_tree[split_index,]
#opps_tree_test <- opps_tree[-split_index, !(colnames(opps_tree) %in% c("Att_Girls"))]
#opps_tree_target <- opps_tree[-split_index, "Att_Girls"]


# second splitting approach
set.seed(28281)
train_rows = sample(1:nrow(opps_tree), 0.75*nrow(opps_tree))
opps_tree_train = opps_tree[train_rows, ]
opps_tree_test = opps_tree[-train_rows, ]

# looking at the distribution -------------------------------------------------------------- use this code for other distribution lookings
table(opps_tree$Att_Girls)/sum(table(opps_tree$Att_Girls))
# roughly 53% of the opportunities have Girls as audience, 50% Boys, 45% GTS  whereas only 8 % have SRDoS and SwDis. 


# growing tree ----
# trying to classify for Att_Girls ----
full_tree_girls <- rpart(formula = Att_Girls ~ ., data    = opps_tree_train, cp   = 0.005,  minsplit = 2,  method  = "class")
plotcp(full_tree_girls)

get_score(full_tree_girls, opps_tree_test)
# That is a pretty high score. Almost too good to be true

prp(full_tree_girls, type=1, extra=102, roundint = FALSE)
full_tree_girls$variable.importance

# try the to built a different, pruned tree
tree2 <- rpartXse(Att_Girls ~ ., opps_tree_train, cp=0.005)
prp(tree2, type = 1, extra = 103, roundint = FALSE)
# this second tree, which is a pruned tree gives only one splitting point on Boys/NotBoys to predict the class Girls=1.
# I think this is not very helpful, so I will go with a very low cp and unpruned tree from the original rpart funtion. 

# Tree for Students with risk droping out of school ----
full_tree_srdos <- rpart(formula = Att_SRDoS ~ ., data = opps_tree_train, cp= 0.05,  minsplit =2, method="class")
prp(full_tree_srdos, type=2, extra=108, roundint = FALSE)
full_tree_srdos$variable.importance
# as with the Girls trees first split seems to be the boys_att, the student dropping tree seems to think, that SwDis is most important. 
# In a next version, I will delete all Att_ attributes besides the one I am looking for:



# tree function ----
opps_tree_attgirls <- opps_tree[,c(-10,-11,-13,-14)]
set.seed(28281)
opps_tree_train_girls = opps_tree_attgirls[train_rows, ]
opps_tree_test_girls = opps_tree_attgirls[-train_rows, ]

full_tree_only_girls <- rpart(formula = Att_Girls ~., data = opps_tree_train_girls, cp =0.005, method = "class")
prp(full_tree_only_girls, type=1, extra=103, roundint = FALSE)
full_tree_only_girls$variable.importance


opps_tree_attsrdos <- opps_tree[,c(-10,-11,-12,-14)]
set.seed(28281)
opps_tree_train_srdos = opps_tree_attsrdos[train_rows, ]
opps_tree_test_girls = opps_tree_attsrdos[-train_rows, ]

full_tree_only_srdos <- rpart(formula = Att_SRDoS ~., data = opps_tree_train_srdos, cp =0.005, method = "class")
prp(full_tree_only_srdos, type=1, extra=103, roundint = FALSE)
full_tree_only_srdos$variable.importance

#make one more with SwDis....



# I have tried to write a tree-making function, but ran into an error I have not solved yet. 
# Try a forest
rf_tree <- randomForest(Att_Girls ~ ., opps_tree_attgirls, ntree=500, importance=TRUE, na.action = na.omit )
rf_tree$confusion
rf_tree$importance
