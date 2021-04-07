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
#  creating the duration_in_hour column
opps_inspect$duration_in_h <- as.integer(opps_inspect$toDate - opps_inspect$fromDate)/3600

# check graphically
ggplot(opps_inspect, aes(x=duration_in_h)) +
  geom_histogram(bins = 50)


boxplot(opps_inspect$duration_in_h)
summary(opps_inspect$duration_in_h)

# need to bin duration
opps_inspect$duration_in_h_group <- cut(opps_inspect$duration_in_h,
                                        breaks =  c(0,1,12,96,168,336,9504),
                                        labels = c("under 1 hour","under one day","one to four days","four to seven days","seven to 14 days","over 14 days"))

ggplot(opps_inspect, aes(x=duration_in_h_group, fill=duration_in_h_group)) +
  geom_histogram(stat="count") +
  theme(axis.title = element_text(size = rel(1.8))) +
  labs(x = "Duration including not specified duration (NA)") +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))

# -> Four to seven days is the most common duration, but a lot of duration times are not specified (NA)


# Costs ----
# exploring costs - Intervalls: 10 Levels, two duplicates due to typos, combining them togehter
opps_inspect$cost <-  combineLevels(opps_inspect$cost, c("More than $1,000","More than $1000"), newLabel = "More than $1,000")
opps_inspect$cost <-  combineLevels(opps_inspect$cost, c("$25 or less","$25 or Less"), newLabel = "$25 or less")
opps_inspect$cost <-  combineLevels(opps_inspect$cost, c("","Free"), newLabel = "Free")

# order the levels for plotting
opps_inspect$cost <- factor(opps_inspect$cost, levels =c("Free", "$25 or less", "$26-$50", "$51-$100", "$101-$500", "$501-$1,000", "More than $1,000"))

# plot to see the distribution of opportunity costs
ggplot(opps_inspect, aes(cost, fill = cost)) +
  geom_bar() +
  theme(axis.title = element_text(size = rel(1.8))) +
  labs(x = "Cost intervals") +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 270))

# see if costs and scholarship are somehow related

opps_inspect$scholarship <-  combineLevels(opps_inspect$scholarship, c("","False"), newLabel = "False")
cost_scholars <- select(opps_inspect, cost, scholarship)
table(cost_scholars)

# making two plots, not sure which one to keep:
ggplot(cost_scholars, aes(cost, fill = cost, alpha = scholarship)) + 
  geom_bar() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 270))

ggplot(cost_scholars, aes(cost, fill = scholarship)) + 
  geom_bar() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 270))
# as we can see, only the price range "$100 to $500" offers a lot of scholarships


# Save opps_total and opps ----
save(opps_total, opps, opps_for_ohe, opps_inspect, file = "data/opps.RData")

# Ages ----
glimpse(opps_inspect)
# getting only the OHE ages
opps_inspect_ages <- select(opps_for_ohe, contains(c("Ages_")))

# built a count
opps_inspect_ages <- rbind(opps_inspect_ages, colSums(opps_inspect_ages))


# pivot longer
plot_opps_ages <- opps_inspect_ages[842,] %>% pivot_longer(cols=c("Ages_All.Ages","Ages_Pre.K", "Ages_Elementary..K.3.","Ages_Elementary..4.6.","Ages_Grades.4.9","Ages_Middle.School","Ages_High.School","Ages_Post.Secondary" ),
                                                names_to = "layer", values_to ="item")

# versus gather 
plot_opps_ages <- gather(opps_inspect_ages[842,], key=Age_Groups, value=counts)

plot_opps_ages_sort <- plot_opps_ages[c(1,8,3,2,4,6,5,7),]
rownames(plot_opps_ages_sort) <- 1:nrow(plot_opps_ages_sort)

plot_opps_ages_sort <- plot_opps_ages_sort[-5,]
# delete Elementary 4,9 which looks like a typo and add one to elementary 4,6
rownames(plot_opps_ages_sort) <- 1:nrow(plot_opps_ages_sort)
plot_opps_ages_sort[4,2] <- 458


ggplot(plot_opps_ages_sort, aes(Age_Groups, counts, fill=Age_Groups)) +
  geom_bar(stat="identity") +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 270))

# distribution of enrollment in the US in 2017
enroll_numbers <- c(4676,16250,12172,8000,16840,18400)
enroll_groups <- c("Ages_Pre.K", "Ages_Elementary..K.3.","Ages_Elementary..4.6.","Ages_Middle.School","Ages_High.School","Ages_Post.Secondary" )
enroll_distr <- data.frame(enroll_groups = enroll_groups, enroll_numbers=enroll_numbers)

enroll_distr
ggplot(enroll_distr, aes(enroll_groups, enroll_numbers, fill=enroll_groups)) +
  geom_bar(stat="identity") +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 270))






