#####################
# Written by Rohith #
#####################

rm(list=ls())
graphics.off()

#####################

library(kohonen)
library(ISLR)
data("USArrests")
head(USArrests)
scaled_data <- scale(USArrests)

#####################
#a)

d1 <- dist(scaled_data, method = "euclidean")
cluster <- hclust(d1, method = "complete")
x11()
plot(cluster, cex=0.9, hang = -1)

cluster_f <- cutree(cluster,3)
cluster_1 <- cluster_f[cluster_f == 1]
cluster_2 <- cluster_f[cluster_f == 2]
cluster_3 <- cluster_f[cluster_f == 3]

names(cluster_1)
names(cluster_2)
names(cluster_3)

#####################
#b)

set.seed(369)
som_grid <- somgrid(xdim = 4, ydim = 4, topo = "hexagonal")
arrests.som <- som(scaled_data, grid = som_grid, rlen = 4000)

codes <- arrests.som$codes[[1]]

x11()
plot(arrests.som, main = "scaled arrests data")

x11()
plot(arrests.som, type = "changes")

x11()
plot(arrests.som, type = "count")

x11()
plot(arrests.som, type = "mapping")

#umatrix plot
#using the coolbluehotred color palette

coolbluehotred <- function(n, alpha = 1){rainbow(n, end=4/6, alpha = alpha)[n:1]}

x11()
plot(arrests.som, type="dist.neighbours", palette.name = coolbluehotred)

for (i in 1:4){
  x11()
  plot(arrests.som, type = "property", property=codes[,i], main = colnames(codes)[i])
}

d <- dist(codes)
hc <- hclust(d)

x11()
plot(hc)

som_cluster_x <- cutree(hc, 3)

#Plotting the SOM with the clusters

new_palette <- c("red", "blue", "green")
new_bgcol <- new_palette[som_cluster_x]

x11()
plot(arrests.som, type = "mapping", col = "black", bgcol = new_bgcol)
add.cluster.boundaries(arrests.som, som_cluster_x)

