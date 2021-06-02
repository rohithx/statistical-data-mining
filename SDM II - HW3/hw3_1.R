#####################
# Written by Rohith #
#####################

rm(list=ls())
graphics.off()

#####################

library(kohonen)
library(ElemStatLearn)
data(nci)
head(nci)

#####################
set.seed(333)
som_grid <- somgrid(xdim=7, ydim=7, topo = "hexagonal")
nci.som <- som(nci, grid = som_grid, rlen = 1000)

codes <- nci.som$codes[[1]]

x11()
plot(nci.som, main = " nci data")

x11()
plot(nci.som, type = "changes")

x11()
plot(nci.som, type = "count")

x11()
plot(nci.som, type = "mapping")

#umatrix plot
#using the coolbluehotred color palette

coolbluehotred <- function(n, alpha = 1){rainbow(n, end=4/6, alpha = alpha)[n:1]}

x11()
plot(nci.som, type="dist.neighbours", palette.name = coolbluehotred)

for (i in 1:12){
  x11()
  plot(nci.som, type = "property", property=codes[,i], main = colnames(codes)[i])
}

d <- dist(codes)
hc <- hclust(d)

x11()
plot(hc,cex = 0.7)

som_cluster_x <- cutree(hc,3)

new_palette <- coolbluehotred(3)
new_bgcol <- new_palette[som_cluster_x]

x11()
plot(nci.som, type = "mapping", col = "black", bgcol = new_bgcol)
add.cluster.boundaries(nci.som, som_cluster_x)

cluster_1 <- som_cluster_x[som_cluster_x == 1]
cluster_2 <- som_cluster_x[som_cluster_x == 2]
cluster_3 <- som_cluster_x[som_cluster_x == 3]
