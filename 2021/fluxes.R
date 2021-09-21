library(tidyverse)
library("dataDownloader") #details here https://github.com/Between-the-Fjords/dataDownloader

#fluxes
get_file(node = "3qhdj",
         file = "BIO102_c-flux_2021.csv",
         path = "2021/data",
         remote_path = "2021")
# NDVI
get_file(node = "3qhdj",
         file = "BIO102_NDVI_2021.csv",
         path = "2021/data",
         remote_path = "raw_data/2021")
# Soil moisture
get_file(node = "3qhdj",
         file = "BIO102_soil-moisture_2021.csv",
         path = "2021/data",
         remote_path = "raw_data/2021")

# Calculate GEP
fluxes2021 <- read_csv("2021/data/BIO102_c-flux_2021.csv") %>% 
  pivot_wider(id_cols = c(site, plot, date), values_from = c(flux,PARavg), names_from = type) %>% 
  rename(
    ER = flux_ER,
    NEE = flux_NEE
  ) %>% 
  mutate(
    GEP = NEE - ER
  ) %>% 
  pivot_longer(c(ER, NEE, GEP), names_to = "type", values_to = "flux") %>% 
  mutate(
    PAR = case_when(
      type == "ER" ~ PARavg_ER,
      type == "NEE" ~ PARavg_NEE,
      type == "GEP" ~ NA_real_
    )
  ) %>% 
  select(!c(PARavg_ER, PARavg_NEE))
# intersting to see that GEP for forest is positive. Looking at soil and air temperatures, it seems that NEE measurements were warmer, maybe it triggered more soil decomposition?

# graph NEE, ER and GEP for the four sites

# will need to show soil moisture, NDVI and PAR together with the fluxes...