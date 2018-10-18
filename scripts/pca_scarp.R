## KS selection of samples from SCaRP fractions dataset

#libraries
library(tidyverse)
library(prospectr)
library(stats)
library(ggfortify)

#set wd etc
setwd("~/DATASCHOOL/scarp_pca/")

proc<-read_csv("data/working/FractionSelection.csv")

autoplot(prcomp(proc[2:8]), data = proc, size = 4, colour = 'SOC_Cont', loadings = TRUE, loadings.label = TRUE)

ks <- kenStone(proc[2:8], 250, 'euclid')
ks
ks2<-as_data_frame(ks[["model"]])

write_csv(ks2, "data/working/ks250.csv")
