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

#PCA
autoplot(prcomp(proc[8:1408]), data = proc, size = 4, colour = 'LandUse', loadings = FALSE, loadings.label = FALSE)
autoplot(prcomp(proc_s[8:1408]), data = proc, size = 4, colour = 'LandUse', loadings = FALSE, loadings.label = FALSE)


pc2model <- prcomp(proc[8:1408])

plot(pc2model$x, col = rgb(0, 0, 0, 0.3), pch = 19, main = "Kennard-Stone (synthetic)")
grid()

#KS
ks <- kenStone(proc[8:1408], 20, 'euclid')
ks

points(pc2model$x[ks$model, ], col = "red", pch = 19, cex = 1.4)

#standardised
#pc2smodel <- prcomp(proc_s[8:1408])

#plot(pc2smodel$x, col = rgb(0, 0, 0, 0.3), pch = 19, main = "Kennard-Stone (synthetic)")
#grid()


#ks_s <- kenStone(proc_s[8:1408], 20, 'euclid')
#ks_s

#points(pc2smodel$x[ks_s$model, ], col = "red", pch = 19, cex = 1.4)


#outputting
##add row ID
procID <- mutate(proc, ID = row_number())

#add ranks to KS selection
ks_vec <- as.vector(ks$model)
rank <- as.vector(1:20)
ks_out <- tibble(ID = ks_vec, KS_rank = rank)

#filter full df by KS selection
ks_df <- procID %>% 
  filter(ID %in% ks_out$ID)

#Add in KS ranks for easy sorting
ks_df <- inner_join(ks_out, ks_df)

#output .csv
write_csv(ks_df, "ks20.csv")
