rm(list = ls())
graphics.off()

##################
library(gRain)
library(Rgraphviz)
library(gRbase)
library(ggm)
##################

g <- list(~A, ~B, ~C|A, ~D|A:B, ~E|B, ~F|A:C:E, ~G|D:E, ~H|F:G)
x <- dagList(g)

##################
#Identifying d-sep
##################

dSep(as(x, "matrix"), "C", "G", NULL)
dSep(as(x, "matrix"), "C", "E", NULL)
dSep(as(x, "matrix"), "C", "E", "G")
dSep(as(x, "matrix"), "A", "G", c("D","E")) 
dSep(as(x, "matrix"), "A", "G", "D") 