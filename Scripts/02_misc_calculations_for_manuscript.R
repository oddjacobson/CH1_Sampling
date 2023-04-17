library(tidyverse)
library(sf)
library(ctmm)

## FIND MEAN AND RANGE OF CONSECUTIVE DAYS IN ENTIRE DATASET

# get data 
data <- readRDS("Data/trk.rds") %>% 
  mutate(date = as.Date(date)) %>%
  group_by(group) %>% 
  distinct(date, .keep_all = TRUE) %>%
  ungroup() %>% 
  st_drop_geometry() %>% 
  dplyr::select(group,date) 

data_sum <- data %>% 
  group_by(group, cumsum(c(0, diff(date) - 1))) %>%
  summarise(sequences = paste(first(date), last(date), sep = ' - '),
            length    = n()) %>%
  filter(length > 1) %>%
  select(group, sequences, length)

range(data_sum$length)
mean(data_sum$length)

## FIND MINIMUM HOURS IN A DAY IN COMPLETE SEGMENTS
comp_data <- readRDS("Data/CH1_GPS_data.rds")


### Other crossing times and sampling schedules

## BUFFALO ---------------------
data("buffalo")
cilla <- buffalo$Cilla

#Calculate variogram 
vg.cilla <- variogram(cilla) 

#Plot up to 50% of the maximum lag in the data 
plot(vg.cilla) 

#Zoom in on the shortest lags 
plot(vg.cilla, fraction=0.005)

# get guess
GUESS <- ctmm.guess(cilla, interactive = FALSE, variogram = vg.cilla)

# get fitted mods
fitted.mods <- ctmm.select(data = cilla, CTMM = GUESS, trace = 2)

### COATI -------------
data("coati")
aleja <- coati[[1]]

#Calculate variogram 
vg.aleja <- variogram(aleja) 

#Plot up to 50% of the maximum lag in the data 
plot(vg.aleja) 

#Zoom in on the shortest lags 
plot(vg.aleja, fraction=0.005)

# get guess
GUESS <- ctmm.guess(aleja, interactive = FALSE, variogram = vg.aleja)

# get fitted mods
fitted.mods <- ctmm.select(data = aleja, CTMM = GUESS, trace = 2)
