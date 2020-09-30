library(tidyverse)
source("https://raw.githubusercontent.com/jogaudard/common/master/fun-fluxes.R")
library(lubridate, warn.conflicts = FALSE)
library("dataDownloader")

measurement <- 120 #the length of the measurement taken on the field in seconds
startcrop <- 30 #how much to crop at the beginning of the measurement in seconds
endcrop <- 0 #how much to crop at the end of the measurement in seconds

#import data from OSF

get_file(node = "3qhdj",
         file = "BIO102_fluxes_raw.csv",
         remote_path = "raw_data")

get_file(node = "3qhdj",
         file = "Field_data_BIO102.csv",
         remote_path = "raw_data")

#import flux data from the logger
fluxes <- 
  read_csv("BIO102_fluxes_raw.csv", na = c("#N/A"), col_types = "ctcnnnnn") %>%
  rename(CO2 = "CO2 (ppm)", PAR = "PAR (umolsm2)", Temp_air = "Temp_air ('C)") %>%  #rename the columns to get something more practical without spaces
  mutate(Date = dmy(Date), #convert date in POSIXct
          Datetime = as_datetime(paste(Date, Time))) %>%  #paste date and time in one column
  select(Datetime,CO2,PAR,Temp_air)

#import field data from the data sheet
field_data <- read_csv("Field_data_BIO102.csv", na = c(""), col_types = "cnctDcn") %>% 
  mutate(
    Start = as_datetime(paste(Date, Starting_time)), #converting the date as posixct, pasting date and starting time together
    # Datetime = Start, #useful for left_join
    End = Start + measurement - endcrop, #creating column End and cropping the end of the measurement
    Start = Start + startcrop #cropping the start
    ) %>%  
  select(Site,Type,Replicate,Starting_time,Date,Remarks,Start,End)

co2conc_bio102 <- match.flux(fluxes, field_data)


write_csv(co2conc_bio102, "co2conc_bio102.csv")

#graph CO2 fluxes to visually check the data


ggplot(co2conc_bio102, aes(x=Datetime, y=CO2)) +
  # geom_point(size=0.005) +
  geom_line(size = 0.1, aes(group = ID)) +
  coord_fixed(ratio = 10) +
  scale_x_datetime(date_breaks = "30 min") +
  # geom_line(size=0.05)
  ggsave("bio102.png", height = 20, width = 40, units = "cm")