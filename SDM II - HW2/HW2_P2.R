###################
#Written by Rohith#
###################

rm(list=ls())

###################
#a)

data<-read.csv("C:/Users/X/Desktop/Ch10Ex11.csv", header = F)
summary(data)

###################
#b)

d1<- 1 - cor(data)
d2<- as.dist(d1)

cluster_x<-hclust(d2,method = "single")
cluster_y<-hclust(d2,method = "average")
cluster_z<-hclust(d2,method = "complete")

plot(cluster_x)
plot(cluster_y)
plot(cluster_z)

###################
#c)

apply(genes_data , 2, mean)
apply(genes_data , 2, var)

pr_x = prcomp(data, scale=TRUE)

biplot(pr_x,scale=0)

head(pr_x$rotation)
