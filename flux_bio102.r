#Installing and importing tidyverse 
#install.packages("tidyverse")
library(tidyverse)
library(dbplyr)
library(RColorBrewer)

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



#average per site of NEE and ER : grouping the data (tidyverse) and calculating GEP
fluxes.avg <- fluxes %>%
  group_by(Site, Type) %>% # data are grouped by Site and Type
  summarise( # I just discovered summarise, this is awesome!! It does just what we need
    flux.avg = mean(flux) #creating a new column called flux.avg with the mean of the flux
    ) %>% 
  pivot_wider(names_from = Type, values_from = flux.avg) %>%  # pivoting the tibble to have NEE and ER as columns
  mutate(
    GEP = ER - NEE #creating GEP column
  ) %>% 
  pivot_longer(!Site, names_to = "Type", values_to = "flux.avg") # making a tidy tibble again




# Barplot of all the fluxes, ideally showing error bars . 
#Include the asphalt site. 
#Can inlude NDVI in the same graph as points with a different vertical axis (0 to 1)

all_fluxes <- read_csv("fluxes_bio102.csv") %>%
  filter ( #you can directly filter the data here
    ID != 11
    & ID != 7
    & ID != 19 #removed because we suspect: (11) someone breath in the chamber. (7) loose tube. (19) No fan no pump 
    ) %>% 
  mutate(Site = Plot_ID)

#Plotting everything together
ggplot(all_fluxes, aes(x=Site, y = flux)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  scale_fill_brewer(palette = "Set1") + #Can't find out why it won't colour the bars... 
  facet_wrap(~Type) + #makes wraps per type 
  theme_minimal()

#calculate the median for each site 
# Add the GEP in the bar plot (and not error bar)
