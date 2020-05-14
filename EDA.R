library(RODBC)
library(DBI)
library(HelpersMG)
library(stringr)
library(dplyr)
library(ggplot2)
library(lattice)
library(mechkar)

library('fastcluster')
library(matrixStats)


setwd("C://bb//airbnb")

load("C://bb//airbnb//data//FF_train_clean.RData")    



ggplot(data=df) +
  geom_density(aes(log(revenue), group=orig_lang2, color=orig_lang2))

