#Installing and importing tidyverse 
#install.packages("tidyverse")
library(tidyverse)

#opening data files 
library(readr)
data.1 <- read_csv("fluxes_bio102_final.csv")
View(data.1)


# Filter data 
data.filtered <- data.1 %>% 
  filter(r.squared >= 0.7 #removing r.square belov 0.7, because data is not good enough for analyses
         & ID != 11 #removed because we suspect someone breath in the chamber
  )


#Summarizing the filtered data using a pipe 
data.filtered %>% 
  summary()

#Divide the data into measurment type:  NEE and ER 
data.NEE <- data.filtered %>% 
  filter(Type == "NEE")
data.ER <- data.filtered %>% 
  filter(Type == "ER")

#plotting the flux data according to site 
ggplot(data.NEE, aes(x= Site, y = flux)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  geom_jitter(shape = 15, colour = "blue") +
  scale_x_discrete() + xlab("Site") + ylab("Flux (mmol/m2/h") + 
  theme_classic() 


ggplot(data.ER, aes(x= Site, y = flux)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  geom_jitter(shape = 15, colour = "blue") +
  scale_x_discrete() + xlab("Site") + ylab("Flux (mmol/m2/h") + 
  theme_classic() 

# Anova
anova.NEE <- aov(flux ~ Site , data.NEE)
summary(anova.NEE)

anova.ER <- aov(flux ~ Site, data.ER)
summary(anova.ER)



#average site of NEE and ER : nesting the data (tidyverse)
nest(data.filtered, )
groupe.data <- data.filtered %>% 
  group_by(Site)
group_by(data.filtered, Site)