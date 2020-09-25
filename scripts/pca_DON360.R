## KS selection of samples from DON360 MIR dataset for onward fractionation

#libraries
library(tidyverse)
library(prospectr)
library(stats)
library(ggfortify)

#import data
proc <- read_csv("data/SIP-DON_Candidates_MIR.csv")
head(proc)

#make factors
proc <- proc %>% 
  mutate(across(c("State", "LandUse"), factor))
head(proc)

#z-standardise
z.fn <- function(x) {
  (x-mean(x))/sd(x)
}

proc_s <- proc %>% mutate(across(8:1408, z.fn))
head(proc_s)


autoplot(prcomp(proc[8:1408]), data = proc, size = 4, loadings = FALSE, loadings.label = FALSE)

ks <- kenStone(proc[2:8], 250, 'euclid')
ks
ks2<-as_data_frame(ks[["model"]])

write_csv(ks2, "data/working/ks250.csv")
