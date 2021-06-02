###################
#Written by Rohith#
###################

rm(list=ls())

####################
#a

d1<-dist(USArrests, method = "euclidean")
cluster<-hclust(d1,method="complete")
plot(cluster, cex=0.8, hang = -1)

#####################
#b

cluster_x <- cutree(cluster,3)

cluster_1 <- cluster_x[cluster_x == 1]
cluster_2 <- cluster_x[cluster_x == 2] 
cluster_3 <- cluster_x[cluster_x == 3]

names(cluster_1)
names(cluster_2)
names(cluster_3)

#####################
#c

sd_data<-scale(USArrests)
d2<-dist(sd_data,method = "euclidean")
sd_clustered<-hclust(d2,method="complete")
plot(sd_clustered,cex = 1,hang=-1)

cluster_y <- cutree(sd_clustered,3)

#####################
#d

table(cluster_x,cluster_y)
