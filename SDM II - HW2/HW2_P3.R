###################
#Written by Rohith#
###################

rm(list=ls())
data<-read.delim(choose.files())
data_y<-data
data_x<-data
data_x$Seed.Group<-NULL
data_x<-scale(data)

###################
#a)

d <- dist(data, method = "euclidean")

#Clustering using single, average and complete
hc1 <- hclust(d, method = "single")
hc2 <- hclust(d, method = "average")
hc3 <- hclust(d, method = "complete")

plot(hc1,hang=-1)
plot(hc2,hang=-1)
plot(hc3,hang=-1)

#Grouping the data

hc_x <- cutree(hc1,3)
hc_y <- cutree(hc2,3)
hc_z <- cutree(hc3,3)

table(data_x, data_y$Seed.Group)

#Kmeans
km= kmeans(data_x, centers=3,nstart = 10)
plot()
table(km$cluster, data_y$Seed.Group)
adj.rand.index(km$cluster, as.numeric(seeddata$Seed.Group))
