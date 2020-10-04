library(tidyverse)
library(broom)
source("https://raw.githubusercontent.com/jogaudard/common/master/fun-fluxes.R")


slopesCO2 <- flux.calc(co2conc_bio102) %>% 
  write_csv("fluxes_bio102.csv")
#could add filter here if needed to filter out fluxes with an r.squared < 0.7