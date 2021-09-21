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
    ),
    site = case_when(
      site == "grassland" ~ "meadow", #decided to group meadow and grassland for simplification
      site == "forest" ~ "forest_floor", #we are talking about the forest floor, we did not include the trees in the flux chamber!!
      TRUE ~ site
    )
  ) %>% 
  select(!c(PARavg_ER, PARavg_NEE))
# intersting to see that GEP for forest is positive. Looking at soil and air temperatures, it seems that NEE measurements were warmer, maybe it triggered more soil decomposition?

# graph NEE, ER and GEP for the four sites
# first we need to summarize the data
# fluxes2021_summary <- fluxes2021 %>% 
#   group_by(site, type) %>% 
#   summarise(flux_avg = mean(flux), flux_se = )
theme_set(theme_grey(base_size = 10)) 

ggplot(fluxes2021, aes(type, flux)) +
  geom_boxplot() +
  # geom_col() +
  # stat_summary(fun = "mean", geom = "bar") +
  # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  facet_wrap(vars(site)) +
  xlab("Flux type") +
  ylab(bquote(CO[2]~"flux,"~mmol/m^2/h))


# will need to show soil moisture, NDVI and PAR together with the fluxes...

ndvi <- read_csv("2021/data/BIO102_NDVI_2021.csv") %>% 
  mutate(
    site = case_when(
      site == "grassland" ~ "meadow", #decided to group meadow and grassland for simplification
      site == "forest" ~ "forest_floor", #we are talking about the forest floor, we did not include the trees in the flux chamber!!
      TRUE ~ site
    )
  )

ggplot(ndvi, aes(site, NDVI)) +
  geom_boxplot() +
  # geom_col() +
  # stat_summary(fun = "mean", geom = "bar") +
  # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  # facet_wrap(vars(site)) +
  xlab("Site") +
  ylab("NDVI")

soil_moisture <- read_csv("2021/data/BIO102_soil-moisture_2021.csv") %>% 
  mutate(
    site = case_when(
      site == "grassland" ~ "meadow", #decided to group meadow and grassland for simplification
      site == "forest" ~ "forest_floor", #we are talking about the forest floor, we did not include the trees in the flux chamber!!
      TRUE ~ site
    ),
    soil_moisture = case_when(
      soil_moisture == "above" ~ "100", #I will assume that "above water table" means we have a saturated soil
      TRUE ~ soil_moisture
    ),
    soil_moisture = as.numeric(soil_moisture)
  )

ggplot(soil_moisture, aes(site, soil_moisture)) +
  geom_boxplot() +
  # geom_col() +
  # stat_summary(fun = "mean", geom = "bar") +
  # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  # facet_wrap(vars(site)) +
  xlab("Site") +
  ylab("Soil moisture, %vol")

ggplot(fluxes2021, aes(site, PAR, fill = type)) +
  geom_boxplot() +
  # geom_col() +
  # stat_summary(fun = "mean", geom = "bar") +
  # stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  # facet_wrap(vars(site)) +
  xlab("Site") +
  ylab(bquote("PAR"~mol/m^2/s))
