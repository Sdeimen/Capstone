# MS Information Capstone 
# Sebastian Deimen
# STEM opportunities in the US
# University of Arizona - Spring 2021
#
# Purpose: Finding Clusters in the data


library(tidyverse)
library(cluster)


setwd("C:/Users/Sebas/Dropbox/A_UofA/INFO 698/Capstone/")
load("data/opps.RData")

glimpse(opps_for_ohe)

# getting rid of the attention groups (23:28) to see, if there are clusters resembling the attention groups. 
opps_cluster <- opps_for_ohe[,-c(1:15,23:28 )] 

glimpse(opps_cluster)

# try tow different  distance measures, one Gower and one manhatten
# Gower ----
gower_dist_1 <- daisy(opps_cluster,
                      metric = "gower",
                     )
summary(gower_dist_1)
gower_mat_1 <- as.matrix(gower_dist_1)

# checking for number of clusters using silhouette coefficient

sil <- c(NA)
 
for(i in 2:60){
   pam_fit <- pam(gower_mat_1, diss = TRUE, k = i)
   
   sil[i] <- pam_fit$silinfo$avg.width
 }
 
saveRDS(sil, 'sil.rds')
sil <- readRDS('sil.rds')

# make nice plots
sil_to_plot <- data.frame(index = seq(1:40), sil = sil)

ggplot(sil_to_plot, aes(x=index,y = sil)) +     # "#069680","#960664","#380696")
  geom_point(color = "#069680", size =3) +
  geom_line(color = "#D35612") +
  geom_vline(xintercept = 18, linetype="dotted", color = "#380696", size=1) +
  geom_text(aes(18,0.22,label = "k = 18", vjust = 3, hjust = 1.5))+
  labs(x = "Number of clusters", y = "Silhouette Width") +
  ylim(0.1,0.65) +
  theme_classic() +
  theme(axis.title = element_text(size = rel(1.8)))

# there seems to be an endless improvement in the Silhouette, so I'll stop here and do not go further with gower

# Manhattan ----

manh_dist_1 <- daisy(opps_cluster,
                      metric = "manhattan",
)
summary(manh_dist_1)
manh_mat_1 <- as.matrix(manh_dist_1)

# checking for number of clusters using silhouette coefficient

sil <- c(NA)

for(i in 2:60){
  pam_fit <- pam(manh_mat_1, diss = TRUE, k = i)
  
  sil[i] <- pam_fit$silinfo$avg.width
}

saveRDS(sil, 'sil.rds')
sil <- readRDS('sil.rds')

# make nice plots
sil_to_plot <- data.frame(index = seq(1:60), sil = sil)

ggplot(sil_to_plot, aes(x=index,y = sil)) +     # "#069680","#960664","#380696")
  geom_point(color = "#069680", size =3) +
  geom_line(color = "#D35612") +
  geom_vline(xintercept = 18, linetype="dotted", color = "#380696", size=1) +
  geom_text(aes(18,0.22,label = "k = 18", vjust = 3, hjust = 1.5))+
  labs(x = "Number of clusters", y = "Silhouette Width") +
  ylim(0.1,0.65) +
  theme_classic() +
  theme(axis.title = element_text(size = rel(1.8))) +
  ggtitle("Silhouette Coefficient","PAM clustering with gower matrix")


# the plots of manhattan and gower doesn't seem to be very different. Out of curiosity I will cluster anyway....


# pick k = 24 as best value, but this is definitely arguable 

pam_fit_1 <- pam(gower_mat_1, k = 18, diss = TRUE)

summary(pam_fit_1)
pam_fit_1$medoids
pam_fit_1$clustering

# add the cluster to the original data
opps_with_cluster <- data.frame(opps_for_ohe[,-c(2,3,15,4,5,6)], pam_fit_1$clustering) #deselect text columns
opps_with_cluster[pam_fit_1$medoids,]

opps_cluster_grouped <- opps_with_cluster %>% 
  mutate(cluster = pam_fit_1.clustering)
  

# show the medoids on the map ----

library(ggmap)
library(maps)
usa <- map_data("usa")
states <- map_data("state")
counties <- map_data("county")
# base US-state plot
ggstates <- ggplot() + 
  geom_polygon(data = states, aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend

# base US plot
ggus <- ggplot() + 
  geom_polygon(data = usa, aes(x = long, y = lat,group = group, col= "red"), fill = "grey",  color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend

ggus + 
  geom_point(data= opps_with_cluster[pam_fit_1$medoids,], aes(x = locationLongitude, y = locationLatitude), color = "black", size = 2) +
  ggtitle("Medoids of the clusters") +
  theme(plot.title = element_text(hjust = 0.5))

ggus + 
  geom_point(data = opps_cluster_grouped, aes(x = locationLongitude, y = locationLatitude, color = cluster)) +
  geom_point(data= opps_with_cluster[pam_fit_1$medoids,], aes(x = locationLongitude, y = locationLatitude), color = "red", size = 2) +
  ggtitle("Cluster Medoids in Red") +
  coord_fixed(xlim = c(-125, -68),  ylim = c(25, 50), ratio = 1.3)


# counting the number of Att_girls=1 in each cluster
opps_cluster_grouped <- opps_with_cluster %>% 
  mutate(cluster = pam_fit_1.clustering) %>%
  group_by(cluster) %>% count(Att_Girls)

View(opps_cluster_grouped)
# I think this approach it is kind of pointless. With almost arbitrarily picked 18 clusters, there are a couple of opportunities, where att_girls
# is higher than not being present. 





# last clustering attempt is going to be aglomerative hierarchical clustering: ----

opps_hier <- select(opps_tree, -"locationState")
glimpse(opps_hier)
library(caret)

# OHE the cost, duration and typeofopportunity variable
dummy <- dummyVars(" ~ cost", data=opps_hier)
opps_hier <- data.frame(opps_hier,predict(dummy, newdata = opps_hier)) %>% select(-cost)

dummy <- dummyVars(" ~ duration", data=opps_hier)
opps_hier <- data.frame(opps_hier, predict(dummy, newdata = opps_hier)) %>% select(-duration)

dummy <- dummyVars("~ typeOfOpportunity", data = opps_hier)
opps_hier <- data.frame(opps_hier, predict(dummy, newdata = opps_hier)) %>% select(-typeOfOpportunity)
#opps_hier$scholarship <- as.integer(opps_hier$scholarship)

glimpse(opps_hier)



# creating a dist matrix
# two approaches: one, everything as factor, run daisy with gower
# second: everything as number run euclidian

opps_hier_fct <- data.frame(lapply(opps_hier, as.factor)) %>% na.omit() #%>% select(-scholarship)
#opps_hier_int <- data.frame(lapply(opps_hier, as.integer)) %>% na.omit() # %>% select(-scholarship)
# save all the data I have so far
save(opps_total, opps_inspect, opps, opps_tree, opps_for_ohe,opps_cluster, opps_hier, opps_hier_fct, opps_hier_int,file = "data/opps.RData")

dist_fct <- daisy(opps_hier_fct, metric = "gower")
dist_int <- daisy(opps_hier_int, metric= "euclidean")

dist_fct <- as.matrix(dist_fct)
dist_int <- as.matrix(dist_int)

aggl_fct <- agnes(dist_fct, method = "average")
plot(aggl_fct,
     main = "Agglomerative, average linkages")


# NOT DONE YET, installing dendextend library if time
#aggl_fct <- color_branches(aggl_fct, h = 3)

aggl_int <- agnes(dist_int, method = "average")
plot(aggl_int,
     main = "Agglomerative, int , average linkages")

#aggl_fct <- agnes(dist_fct, method = "complete")
#plot(aggl_fct,
#     main = "Agglomerative, fct, complete linkages")

#aggl_int <- agnes(dist_int, method = "complete")
#plot(aggl_int,
#     main = "Agglomerative, int , complete linkages")

h <- hclust(dist_fct, method="average")
plot(h, hang=-0.1)

clus6 <- cutree(aggl_int, 6)
clus6


# bind clusters back to data
opps_hier_fct_clusters <- cbind(opps_hier_fct, cluster = clus6)
# get some counts on attention groups
opps_hier_fct_clusters_girls <- opps_hier_fct_clusters %>% group_by(cluster) %>% count(Att_Girls)
opps_hier_fct_clusters_boys <- opps_hier_fct_clusters %>% group_by(cluster) %>% count(Att_Boys)
opps_hier_fct_clusters_risk <- opps_hier_fct_clusters %>% group_by(cluster) %>% count(Att_SRDoS)
opps_hier_fct_clusters_soso <- opps_hier_fct_clusters %>% group_by(cluster) %>% count(cluster)

# count/data check
opps_hier_fct_clusters_risk
opps_hier_fct_clusters_girls
opps_hier_fct_clusters_boys
opps_hier_fct_clusters_soso

# preparing and plotting distrubution of cluster no 6
cluster_no_6 <- opps_hier_fct_clusters %>% filter(cluster ==6) %>% select(-cluster)

cluster_no_6 <- data.frame(lapply(cluster_no_6, as.character))
cluster_no_6 <- data.frame(lapply(cluster_no_6, as.numeric))
cluster_no_6 <- rbind(cluster_no_6, colSums(cluster_no_6))
plot_cluster6 <- gather(cluster_no_6[263,], key=attributes, value=counts)

glimpse(cluster_no_6)

ggplot(plot_cluster6, aes(attributes, counts, fill=attributes)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 270)) +
  ggtitle("Distribution of Attributes in Cluster No 6")

# checking on the order of AoI in cluster 6
o <- cluster_no_6[263,] %>% select(contains(c("AoI_"))) %>% order()
t <- cluster_no_6[263,] %>% select(contains(c("AoI_")))
t[o]





# Playground ----

library(dbscan)
library(lsa)
# removing typeofopportunity, causes some trouble with NANs
opps_hier_int <- select(opps_hier_int, -contains(c("typeOf")))
glimpse(opps_hier_int)
opps_cos <- cosine(as.matrix(opps_hier_int))
# cluster with DBSCAN
res <- dbscan(opps_cos, eps= 0.1, minPts = 2)
res
plot(opps_cos, col=res$cluster, pch=res$cluster)
# gives me: 
# The clustering contains 5 cluster(s) and 31 noise points.
#
#  0  1  2  3  4  5 
# 31  2  3 18  2  2 
# that clustering looks like a dead end to me...