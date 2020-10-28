#Installing and importing tidyverse 
#install.packages("tidyverse")
library(tidyverse)
library(dbplyr)
library(RColorBrewer)
library(multcomp)

#Installing and getting devtools and datadownloader
#install.packages("devtools") #Install this if you don't have it on your computer already
library(devtools)
devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader") #check https://github.com/Between-the-Fjords/dataDownloader it is a bit tricky to install
# datadownloader allows you to download data from OSF. It will check if the datas you have locally are older than the one on OSF. If needed, the new data will be downloaded.

get_file(node = "3qhdj",
         file = "fluxes_bio102.csv", #this is the file on OSF
         remote_path = "")

get_file(node = "3qhdj",
         file = "NDVI_raw.csv", #this is the file on OSF
         remote_path = "raw_data")

#opening data files 
# library(readr) #readr is part of tidyverse, no need to call it again
fluxes <- read_csv("fluxes_bio102.csv") %>% #give an obvious name to your objects, so that you know what it is
  filter ( #you can directly filter the data here
    r.squared >= 0.7 #removing r.square belov 0.7, because data is not good enough for analyses
    & ID != 11 #removed because we suspect someone breath in the chamber
    & p.value <= 0.05 #we filter the p.value at that step. The non significant data should not be removed during the cleaning (my mistake, sorry)
    ) %>% 
  mutate(
    Site = Plot_ID # the functions from fun-flux use Plot_ID to designate the plots. Now we can rename them Site again for more clarity
  )

summary(fluxes)

# plotting everything together
ggplot(fluxes, aes(x= Site, y = flux)) +
  geom_boxplot(aes(fill = Site)) +
  scale_fill_brewer(palette = "Set1") +
  geom_jitter(shape = 16, colour = "black") +
  scale_x_discrete() + xlab("Site") + ylab("Flux (mmol/m2/h") + 
  facet_wrap(~Type) + # makes wraps per type
  theme_minimal() 


#anova on the different type of flux
anova.fluxes <- fluxes %>%
  group_by(Type) %>% 
  nest() %>% 
  mutate(
    aov_results =  
      map(data, ~summary(aov(flux ~ Site,.)))
    )
anova.fluxes[[3]] # will print the results of the anova

fluxes <- mutate_at(fluxes, c("Site"), as_factor) #changing Site as a factor (necessary for linear model)

tukey_fun <- function(df){
  fit.lm <- lm(flux~Site, data=df)
  mc <- glht(fit.lm, linfct=mcp(Site="Tukey"), data=df)
  return(summary(mc))
} #creating a function doing a tukey test
  
  
tukey.fluxes <- fluxes %>%
  group_by(Type) %>% 
  nest() %>% 
  mutate(
    tukey.results = 
      map(data,tukey_fun)
  ) #running the tukey_fun function on the nested fluxes
tukey.fluxes[[3]] #shows the summary of the tukey test. First tibble is NEE and second is ER.


#average per site of NEE and ER : grouping the data (tidyverse) and calculating GEP
fluxes.avg <- fluxes %>%
  group_by(Site, Type) %>% # data are grouped by Site and Type
  summarise( # I just discovered summarise, this is awesome!! It does just what we need
    flux.avg = mean(flux) #creating a new column called flux.avg with the mean of the flux
    ) %>% 
  pivot_wider(names_from = Type, values_from = flux.avg) %>%  # pivoting the tibble to have NEE and ER as columns
  mutate(
    GEP = NEE - ER #creating GEP column
  ) %>% 
  pivot_longer(!Site, names_to = "Type", values_to = "flux.avg") # making a tidy tibble again

#barplot of fluxes
# fluxes.avg <- mutate_at(fluxes.avg, c("Site"), as.character)
fluxes.avg.plot <- ggplot(fluxes.avg, aes(x = Type, y = flux.avg, fill = Site, order = Site)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ylab("CO2 flux")
  # ggsave("fluxes_all_sites.jpg") #to save the graph as an image. Check ?ggsave for options




# Barplot of all the fluxes, ideally showing error bars . 
#Include the asphalt site. 
#Can inlude NDVI in the same graph as points with a different vertical axis (0 to 1)

all_fluxes <- read_csv("fluxes_bio102.csv") %>%
  filter ( #you can directly filter the data here
    ID != 11 #removed because we suspect someone breath in the chamber
    & ID != 7 #removed because there was a loose tube
    & ID != 19 #removed because no fan no pump 
    & ID != 24 # removed because r square is significantly lower then the other two leaves_ER measurements 
    ) %>% 
  mutate(Site = Plot_ID)

  
#average per site of NEE and ER : grouping the data (tidyverse) and calculating GEP
allfluxes.avg <- all_fluxes %>%
  group_by(Site, Type) %>% # data are grouped by Site and Type
  summarise( # I just discovered summarise, this is awesome!! It does just what we need
    allflux.avg = mean(flux) #creating a new column called flux.avg with the mean of the flux
  ) %>% 
  pivot_wider(names_from = Type, values_from = allflux.avg) %>%  # pivoting the tibble to have NEE and ER as columns
  mutate(
    GEP = NEE - ER #creating GEP column
  ) %>% 
  pivot_longer(!Site, names_to = "Type", values_to = "allflux.avg") # making a tidy tibble again

#reading NDVI file and calculating the average from it 
NDVI <- read_csv("NDVI_raw.csv") %>% 
  group_by(Site) %>% 
  summarise(
    NDVI.avg = mean(NDVI)
            ) 


#Plotting everything together
ggplot(allfluxes.avg, aes(x=Site, y = allflux.avg, fill = Site)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  scale_fill_brewer(palette = "Set1") + #Can't find out why it won't colour the bars... 
  facet_wrap(~Type) + #makes wraps per type 
  theme_minimal()

ggplot(NDVI, aes(x = Site, y = NDVI.avg)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  scale_fill_brewer(palette = "Set1") + #adding colour to the barplot
  theme_minimal()
#make a barplot beside of the NDVI 

#Working with the asphalt
fluxes.asphalt <- read_csv("fluxes_bio102.csv") %>% #give an obvious name to your objects, so that you know what it is
  filter ( #you can directly filter the data here
    Plot_ID == "asphalt"
    # r.squared >= 0.7 #removing r.square belov 0.7, because data is not good enough for analyses
    & ID != 11 #removed because we suspect someone breath in the chamber
    & p.value <= 0.05 #we filter the p.value at that step. The non significant data should not be removed during the cleaning (my mistake, sorry)
  ) %>% 
  rename(
    Site = Plot_ID # the functions from fun-flux use Plot_ID to designate the plots. Now we can rename them Site again for more clarity
  )

asphalt.avg <- fluxes.asphalt %>%
  group_by(Site, Type) %>% # data are grouped by Type
  summarise( 
    flux.avg = mean(flux) #creating a new column called flux.avg with the mean of the flux
  ) %>% 
  pivot_wider(names_from = Type, values_from = flux.avg) %>%  # pivoting the tibble to have NEE and ER as columns
  mutate(
    GEP = ER - NEE #creating GEP column
  ) %>% 
  pivot_longer(!Site, names_to = "Type", values_to = "flux.avg") # making a tidy tibble again

fluxes.with.asphalt <- bind_rows(asphalt.avg, fluxes.avg)


fluxes.with.asphalt <- mutate_at(fluxes.with.asphalt, c("Site"), as_factor)
fluxes.with.asphalt.plot <- ggplot(fluxes.with.asphalt, aes(x = Type, y = flux.avg, fill = Site)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ylab("CO2 flux")
  # ggsave("fluxes_all_sites_asphalt.jpg")

#some manipulation to keep the same colours for each site
# my_colors <- c("red", "blue", "green", "yellow", "purple")
my_colors <- brewer.pal(5,"Set2")
names(my_colors) <- levels(fluxes.with.asphalt$Site)
my_scale <- scale_fill_manual(name = "Site", values = my_colors)

fluxes.with.asphalt.plot + my_scale +
  ggsave("fluxes_all_sites_asphalt.jpg")
  
fluxes.avg.plot + my_scale +
  ggsave("fluxes_all_sites.jpg")
  


#Making a new table showing fluxes and r-squares
fluxes_rsquare <- select(all_fluxes, Site, Type, r.squared, p.value, flux)

