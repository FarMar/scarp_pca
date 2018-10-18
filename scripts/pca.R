## First attempt at PCA in R

##Steps taken from https://www.r-bloggers.com/how-to-perform-pca-on-r/

##https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html

##Data is SCaRP for sample selection

# Load packages

library(stats)
library(tidyverse)
library(soil.spec)
library(prospectr)
library(ggfortify)

#set wd etc
setwd("~/DATASCHOOL/scarp_pca/")
all<-read_csv("data/working/scarp_pca.csv")
ratio<-read_csv("data/working/aoa-r.csv")
str(all)

#reorder
allr <- select(all, 1,8,2,3,4,5,6,7,9,10,11)

#join
joined <- inner_join(allr, ratio)
str(joined)

#histograms
hist(ratio$aoa_ratio)
hist(joined$SOC_Cont)

hist(log10(ratio$aoa_ratio))
hist((joined$Udepth))

#PCA plots
autoplot(prcomp(all[3:11]))
autoplot(prcomp(allr[3:11]), data = allr, colour = 'a_oa_C-a_oa_F', loadings = TRUE, loadings.label = TRUE)

#better colours, omit NA
#na.omit(joined, cols=12)
autoplot(prcomp(joined[3:11]), data = joined, size = 4, colour = 'SOC_Cont', loadings = TRUE, loadings.label = TRUE)


#Stoning kennard - prospectr looks to be a better bet: http://antoinestevens.github.io/prospectr/
#soil.spec is from ICRAF: http://soilspec.r-forge.r-project.org/

install.packages("soil.spec")
install.packages("prospectr")
library(soil.spec)
library(prospectr)

# kenStone command follows: https://www.rdocumentation.org/packages/prospectr/versions/0.1.3/topics/kenStone
# Helpfully, it outputs in "priority" order
#Unfelpfully, it only outputs row numbers, so some stitching back together will be required

ks <- kenStone(joined[3:11], 250, 'euclid')
ks
ks2<-as_data_frame(ks[["model"]])

write_csv(ks2, "data/working/ks.csv")

