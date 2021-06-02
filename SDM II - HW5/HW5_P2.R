rm(list = ls())
graphics.off()

##################
library(gRain)
library(Rgraphviz)
library(gRbase)
library(ggm)
##################

g <- list(~W, ~X, ~Y|W:X, ~Z|Y)
x <- dagList(g)
plot(x)

##################
#Identifying d-sep
##################

dSep(as(x, "matrix"), "W", "X", NULL) 
dSep(as(x, "matrix"), "W", "Z", "X") 
dSep(as(x, "matrix"), "Z", "W", "Y")
dSep(as(x, "matrix"), "W", "Y", NULL)
dSep(as(x, "matrix"), "X", "Y", NULL) 
dSep(as(x, "matrix"), "W", "X", "Z")
dSep(as(x, "matrix"), "X", "Z", c("W","Y"))

