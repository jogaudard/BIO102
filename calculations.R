library(tidyverse)
library(broom)
source("https://raw.githubusercontent.com/jogaudard/common/master/fun-fluxes.R")


# create a new datafram with the slope of the trendline of the flux, R squared of the trendline, the mean of air temp and PAR
#https://dplyr.tidyverse.org/reference/group_map.html group_map?
#https://www.r-bloggers.com/2016/09/running-a-model-on-separate-groups/
# co2conc_bio102 %>% 
#   group_by(ID) %>% 
#   group_map()
slopesCO2 <- flux.calc(co2conc) %>% 
  write_csv("fluxes_bio102_final.csv")
