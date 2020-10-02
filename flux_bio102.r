#Installing and importing tidyverse 
#install.packages("tidyverse")
library(tidyverse)
library("dataDownloader") #check https://github.com/Between-the-Fjords/dataDownloader it is a bit tricky to install
# datadownloader allows you to download data from OSF. It will check if the datas you have locally are older than the one on OSF. If needed, the new data will be downloaded.

get_file(node = "3qhdj",
         file = "fluxes_bio102.csv", #this is the file on OSF
         remote_path = "")

#opening data files 
# library(readr) #readr is part of tidyverse, no need to call it again
fluxes <- read_csv("fluxes_bio102.csv") %>% #give an obvious name to your objects, so that you know what it is
  filter( #you can directly filter the data here
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
  geom_boxplot(fill = "grey80", colour = "black") +
  geom_jitter(shape = 15, colour = "blue") +
  scale_x_discrete() + xlab("Site") + ylab("Flux (mmol/m2/h") + 
  facet_wrap(~Type) + # makes wraps per type
  theme_classic() # detail: figure is not super pretty, maybe something can be done here


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

