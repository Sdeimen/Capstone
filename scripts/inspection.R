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
library(rockchalk) # for combining levels

load("data/opps.RData")

# for inspecting purposes I removed the three text attributes "description", excerpt" and "title"
opps_inspect <- select(opps, -title, -excerpt, -description)
glimpse(opps_inspect)

# checking for incomplete cases
nrow(filter(opps_inspect, !complete.cases(opps_inspect)))
# it looks like 2 cases are incomplete
filter(opps_inspect, !complete.cases(opps_inspect))
# and those seems to have missing Longs and Lats, Berkely and Middelbury, IN, put those in manually
# There are a couple of other missing data, such as "attention", but for now I will keep those as well

opps_inspect$locationLatitude[33] <- 41.675328
opps_inspect$locationLongitude[33]  <-  -85.7061011
opps_inspect$locationLatitude[219]  <- 37.87159
opps_inspect$locationLongitude[219]  <- -122.27275



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
  geom_histogram(bins = 25) +
  theme_bw() +
  ggtitle("Histogram of duration in hours")


boxplot(opps_inspect$duration_in_h)
summary(opps_inspect$duration_in_h)

# need to bin duration
opps_inspect$duration_in_h_group <- cut(opps_inspect$duration_in_h,
                                        breaks =  c(0,1,12,96,168,336,9504),
                                        labels = c("under 1 hour","under one day","one to four days","four to seven days","seven to 14 days","over 14 days"))

ggplot(opps_inspect, aes(x=duration_in_h_group, fill=duration_in_h_group)) +
  geom_bar(stat="count") +
  theme(axis.title = element_text(size = rel(1.8))) +
  labs(x = "Duration including not specified duration (NA)") +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 270, size=18)) +
  ggtitle("Distribution of Duration")

# -> Four to seven days is the most common duration, but a lot of duration times are not specified (NA)


# is there a cluster of days?
# for to seven days:
dates <- opps_inspect %>% select(fromDate, duration_in_h_group) %>% filter(duration_in_h_group =="four to seven days")
dates$fromDate <- as.Date(dates$fromDate)

dates_sort <- dates[order(dates$fromDate),]

date_table <- as.data.frame(table(dates_sort))

date_table <- date_table %>% filter(duration_in_h_group =="four to seven days")

ggplot(date_table, aes(x=fromDate, y = Freq, fill = Freq)) + 
  geom_bar(stat="identity", width= 0.4) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 270, size=16)) +
  ggtitle("When happens the most of 'four to seven days'")






dates <- opps_inspect %>% select(fromDate, duration_in_h_group) %>% filter(duration_in_h_group =="under one day")
dates$fromDate <- as.Date(dates$fromDate)

dates_sort <- dates[order(dates$fromDate),]

date_table <- as.data.frame(table(dates_sort))

date_table <- date_table %>% filter(duration_in_h_group =="under one day")

ggplot(date_table, aes(x=fromDate, y = Freq, fill = Freq)) + 
  geom_bar(stat="identity", width= 0.4) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 270, size=16)) +
  ggtitle("When happens the most of 'under one day'")




# general overview
# make a frame without na
dates <- opps_inspect$fromDate
dates <- na.omit(opps_inspect$fromDate)
#dates$fromDate <- as.POSIXct(dates$fromDate, format = "%Y-%M-%d")
dates <- as.Date(dates)

#dates_sort <- as.data.frame(dates[order(dates$fromDate)])
dates_sort <- dates[order(dates)]

date_table <- as.data.frame(table(dates_sort))

ggplot(date_table, aes(x=dates_sort, y = Freq, fill = Freq)) + 
  geom_bar(stat="identity", width= 0.4) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 270)) +
  ggtitle("When are there the most offerings")




# Costs ----
# exploring costs - Intervals: 10 Levels, two duplicates due to typos, combining them together
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
  theme(legend.position = "none", axis.text.x = element_text(angle = 270, size = 16))

# see if costs and scholarship are somehow related

opps_inspect$scholarship <-  combineLevels(opps_inspect$scholarship, c("","False"), newLabel = "False")
cost_scholars <- select(opps_inspect, cost, scholarship, duration_in_h_group)
table(cost_scholars)

# making two plots, not sure which one to keep:
ggplot(cost_scholars, aes(cost, fill = cost, alpha = scholarship)) + 
  geom_bar() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 270))

ggplot(cost_scholars, aes(cost, fill = scholarship)) + 
  geom_bar() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 270, size=14)) +
  ggtitle("Cost for the opportunities over available scholarships")
# as we can see, only the price range "$100 to $500" offers a lot of scholarships
cost_scholars_nan <- na.omit(cost_scholars)

ggplot(cost_scholars_nan, aes(cost, fill = duration_in_h_group)) + 
  geom_bar() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 270, size=14)) +
  ggtitle("Cost for the opportunities over the duration","(duration NaN's deleted)")
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
plot_opps_ages <- opps_inspect_ages[842,] %>% pivot_longer(cols=c("Ages_All.Ages","Ages_Pre.K", "Ages_Elementary..K.3.","Ages_Elementary..4.6.","Ages_Middle.School","Ages_High.School","Ages_Post.Secondary" ),
                                                names_to = "layer", values_to ="item")

# versus gather 
plot_opps_ages <- gather(opps_inspect_ages[842,], key=Age_Groups, value=counts)

plot_opps_ages$Age_Groups <- as.factor(plot_opps_ages$Age_Groups)
plot_opps_ages$Age_Groups <- factor(plot_opps_ages$Age_Groups,levels=c("Ages_All.Ages","Ages_Pre.K", "Ages_Elementary..K.3.","Ages_Elementary..4.6.","Ages_Middle.School","Ages_High.School","Ages_Post.Secondary"))


ggplot(plot_opps_ages[-1,], aes(Age_Groups, counts)) +
  geom_bar(stat="identity", color= "black", fill ="#e60047", width = 0.35) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 270, size =14)) +
  ggtitle("Distribution of Offerings by Age Group","Not Normalized")



# normalize the data do plot a comparisson to enrollment students
normalize_ages <- function(x){x /sum(plot_opps_ages$counts)}
ages_norm <- lapply(plot_opps_ages$counts,normalize )
plot_opps_ages$norm <-  lapply(plot_opps_ages$counts,normalize_ages )

ggplot(plot_opps_ages[-1,], aes(Age_Groups, norm, fill=Age_Groups)) +
  geom_bar(stat="identity") +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 270, size = 14)) +
  ggtitle("Distribution of Offerings by Age Group","Normalized")
  
  

# distribution of enrollment in the US in 2017
enroll_numbers <- c(4676,16250,12172,8000,16840,18400)
enroll_groups <- c("Ages_Pre.K", "Ages_Elementary..K.3.","Ages_Elementary..4.6.","Ages_Middle.School","Ages_High.School","Ages_Post.Secondary" )
enroll_distr <- data.frame(Age_Groups = enroll_groups, enroll_numbers=enroll_numbers)
enroll_distr
enroll_distr$Age_Groups <- factor(enroll_distr$Age_Groups,levels=c("Ages_All.Ages","Ages_Pre.K", "Ages_Elementary..K.3.","Ages_Elementary..4.6.","Ages_Middle.School","Ages_High.School","Ages_Post.Secondary"))



# normalize 
normalize_enrolls <- function(x){x/sum(enroll_distr$enroll_numbers)}
enroll_distr$norm <- lapply(enroll_distr$enroll_numbers, normalize_enrolls)  

# plot non norm
ggplot(enroll_distr, aes(Age_Groups, enroll_numbers, fill=enroll_groups)) +
  geom_bar(stat="identity") +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 270)) +
  ggtitle("Distribution of US wide enrollments","Not Normalized")

# plot norm
ggplot(enroll_distr, aes(Age_Groups, norm, fill=enroll_groups)) +
  geom_bar(stat="identity") +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 270)) +
  ggtitle("Distribution of US wide enrollments","Normalized")



# bind enrolls and ages together, to plot in one plot
ages_enrolls <- inner_join(plot_opps_ages, enroll_distr, by="Age_Groups")
colnames(ages_enrolls) <- c("Age_Groups", "age_counts","age_norm","enroll_counts","enroll_norm")
# make sure, the levels are in the right order
ages_enrolls$Age_Groups <- factor(ages_enrolls$Age_Groups, levels = c("Ages_Pre.K", "Ages_Elementary..K.3.",
                                                                      "Ages_Elementary..4.6.","Ages_Middle.School","Ages_High.School",
                                                                      "Ages_Post.Secondary" ))
# and order it and select only the norms
ages_enrolls <- ages_enrolls[order(ages_enrolls$Age_Groups),] %>% select(Age_Groups,age_norm, enroll_norm)
ages_enrolls$age_norm <- unlist(ages_enrolls$age_norm)
ages_enrolls$enroll_norm <- unlist(ages_enrolls$enroll_norm)
ages_enrolls


ages_enroll_long <- pivot_longer(ages_enrolls, cols=c("age_norm","enroll_norm"), names_to ="Category", values_to = "numbers")



# plot ages_groups and enrollments in one
ggplot(ages_enroll_long, aes(Age_Groups, numbers, fill=Category)) +
  geom_bar(position="dodge", stat="identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 270, size=14)) +
  scale_fill_manual(labels = c("Opportunities", "Enrollments"),
                    values=c("#e69f00", "#d6ebfb"))+
  labs(y = "Normalized Counts") +
  ggtitle("Comparison", "of Opportunities offered by AgeGroups and Enrollments - Normalized")












# Attention - this is the audience ----

opps_inspect_att <- select(opps_for_ohe, contains("Att_")) %>% select(-Att_None)
opps_inspect_att <- lapply(opps_inspect_att, as.numeric)
opps_inspect_att <- as.data.frame(opps_inspect_att)
# finding numbers of observations solely for one audience:
nrow(subset(opps_inspect_att, Att_Boys==0 & Att_Girls == 0 & Att_GTS ==0 & Att_SRDoS ==0 & Att_SwDis ==0))
nrow(subset(opps_inspect_att, Att_Boys==1 & Att_Girls == 0 & Att_GTS ==0 & Att_SRDoS ==0 & Att_SwDis ==0))
nrow(subset(opps_inspect_att, Att_Boys==0 & Att_Girls == 1 & Att_GTS ==0 & Att_SRDoS ==0 & Att_SwDis ==0))
nrow(subset(opps_inspect_att, Att_Boys==0 & Att_Girls == 0 & Att_GTS ==1 & Att_SRDoS ==0 & Att_SwDis ==0))
nrow(subset(opps_inspect_att, Att_Boys==0 & Att_Girls == 0 & Att_GTS ==0 & Att_SRDoS ==1 & Att_SwDis ==0))
nrow(subset(opps_inspect_att, Att_Boys==0 & Att_Girls == 0 & Att_GTS ==0 & Att_SRDoS ==0 & Att_SwDis ==1))

nrow(subset(opps_inspect_att, Att_Boys==0 & Att_Girls == 0 & Att_GTS ==0 & Att_SRDoS ==1 & Att_SwDis ==1))


nrow(subset(opps_inspect_att, Att_Girls == 1))

# If we subtract all Nans from the dataset, we have girls appear in all but 10 opportunites, boys in 24 less. 
# We have 20 solely for girls and only 1 solely for SwDis

# GTS : gifted and talented studends
# SRDos: Students with Risk of dropping out of school
# SwDis: Students with disabilities 

# built a count
opps_inspect_att <- rbind(opps_inspect_att, colSums(opps_inspect_att))
opps_inspect_att[842,]

# add the sum row
plot_opps_att <- gather(opps_inspect_att[842,], key=audience, value=counts)

ggplot(plot_opps_att, aes(audience, counts, fill=audience)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 270, size=14)) +
  ggtitle("Distribution of desired audience")

# This is interesting, it looks like boys and girls are equally likely, but the GTS do get way more attention than underrepresented like Students with risk droping out of school or with disabilities

# Area of Interest ----

opps_inspect_aoi <- select(opps_for_ohe, contains("AoI_"))
opps_inspect_aoi

# built a count
opps_inspect_aoi <- rbind(opps_inspect_aoi, colSums(opps_inspect_aoi))
opps_inspect_aoi[842,]

plot_opps_aoi <- gather(opps_inspect_aoi[842,], key=areaofinterest, value=counts)

ggplot(plot_opps_aoi, aes(areaofinterest, counts, fill=areaofinterest)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(legend.position = "none", axis.text=element_text(size=10), axis.text.x = element_text(angle = 270, size=12)) +
  labs(x = "Area of Interest") +
  ggtitle("Overview of Area of Interests")

cs <- opps_inspect_aoi$AoI_CS
cp <- opps_inspect_aoi$AoI_Coding.Programming

# checking if Computer Science and Coding.Programming are the same, because the counts look like
opps_inspect_aoi[opps_inspect_aoi$AoI_CS != opps_inspect_aoi$AoI_Coding.Programming,]
# but they are not

glimpse(opps_inspect)

# type of opportunity ----
opps_inspect$typeOfOpportunity <-  combineLevels(opps_inspect$typeOfOpportunity, c("Afterschool","Afterschool Program"), newLabel = "Afterschool")
opps_inspect$typeOfOpportunity <- factor(opps_inspect$typeOfOpportunity, levels = c("Afterschool","Citizen Science","Community Event","Competition",
                                         "Conference/Workshop", "Drop-in","Fair/Festival","Field Trip","In School", "Online Learning",
                                         "Other","Overnight","Stargazing","Summer"))

ggplot(opps_inspect, aes(typeOfOpportunity, fill = duration_in_h_group)) +
  geom_bar() +
  theme_classic() +
  theme(axis.text=element_text(size=10), axis.text.x = element_text(angle = 270,size=12)) +
  labs(x = "Type of Opportunity") +
  ggtitle("Type of Opportuntiy by Duration")

ggplot(opps_inspect, aes(typeOfOpportunity, fill = cost)) +
  geom_bar() +
  theme_classic() +
  theme(axis.text=element_text(size=10), axis.text.x = element_text(angle = 270,size=12)) +
  labs(x = "Type of Opportunity") +
  ggtitle("Type of Opportuntiy by Cost")

# try to find girls only

# language

opps_inspect_l <- select(opps_for_ohe, contains("L_"))
opps_inspect_l

# built a count
opps_inspect_l <- rbind(opps_inspect_l, colSums(opps_inspect_l)) %>% select(-"L_None")
opps_inspect_l[842,]

plot_opps_l <- gather(opps_inspect_l[842,], key=Language, value=counts)

ggplot(plot_opps_l, aes(Language, counts, fill=Language)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(legend.position = "none", axis.text=element_text(size=10), axis.text.x = element_text(angle = 270)) +
  labs(x = "Area of Interest") +
  ggtitle("Overview of Languages")
