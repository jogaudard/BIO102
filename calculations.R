library(tidyverse)
library(broom)

# Import CO2 concentration df


# create a new datafram with the slope of the trendline of the flux, R squared of the trendline, the mean of air temp and PAR
#https://dplyr.tidyverse.org/reference/group_map.html group_map?
#https://www.r-bloggers.com/2016/09/running-a-model-on-separate-groups/
# co2conc_bio102 %>% 
#   group_by(ID) %>% 
#   group_map()
slopesCO2 <- co2conc_bio102 %>% 
  # group_by(ID) %>% 
  nest(-ID) %>% 
  mutate(
    data = map(data, ~.x %>% 
                 mutate(time = difftime(Datetime[1:length(Datetime)],Datetime[1] , units = "secs"), #add a column with the time difference between each measurements and the beginning of the measurement. Usefull to calculate the slope.
                        PARavg = mean(PAR, na.rm = TRUE), #mean value of PAR for each flux
                        Temp_airavg = mean(Temp_air, na.rm = TRUE)  #mean value of Temp_air for each flux
                        + 273.15 #transforming in kelvin for calculation
                        )), 
    fit = map(data, ~lm(CO2 ~ time, data = .)), #fit is a new column in the tibble with the slope of the CO2 concentration vs time (in secs^(-1))
    # slope = map_dbl(fit, "time")
    results = map(fit, glance), #to see the coefficients of the model
    slope = map(fit, tidy) #creates a tidy df with the coefficients of fit
    ) %>% 

  unnest(results, slope) %>% 
  unnest(data) %>% 
  filter(term == 'time') %>%  #filter the estimate of time only. That is the slope of the CO2 concentration. We need that to calculate the flux.
           # & r.squared >= 0.1) %>% #keeping only trendline with an r.squared above or equal to 0.7. Below that it means that the data are not good quality enough
  select(ID, Site, Type, Replicate, Remarks, Date, PARavg, Temp_airavg, r.squared, adj.r.squared, p.value, estimate) %>% #select the column we need, dump the rest
  distinct() #remove duplicate
  

vol = 25 #volume of the chamber + tubings in L
P = 1 #atmospherique pressure, assumed at 1 atm
R = 0.082057 #gas constant, in L*atm*K^(-1)*mol^(-1)
A = 0.0625 #area of plot in m^2

#calculate fluxes using the trendline and the air temperature

fluxes_bio102 <- slopesCO2 %>% 
  mutate(flux = (estimate * P * vol)/(R * Temp_airavg * A) #gives flux in micromol/s/m^2
         *3600 #secs to hours
         /1000 #micromol to mmol
  ) %>%  #flux is now in mmol/m^2/h, which is more common
write_csv("fluxes_bio102.csv")
