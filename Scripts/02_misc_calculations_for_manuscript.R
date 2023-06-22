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

# calculate percentage of incomplete tracks ---------------------------------------

# timestamp data comes from ctmm formatted dataframe
DATA <- read_rds("Data/30min_trkpts_formatted") %>% 
  mutate(name = str_c(str_sub(individual.local.identifier, 1,2),
                      as.character(date(timestamp)), sep = "_"))

# non timestamp data comes from orignal dataframe
DATA2 <- readRDS("Data/trk.rds") %>% 
  mutate(date = as.Date(date)) %>% 
  st_drop_geometry() 

# calculate counts of 5min time periods
old_counts <- DATA2 %>% 
  filter(year <= 2012) %>% # before or equal to 2012
  mutate(group_date = str_c(group,date, sep = "_")) %>% 
  group_by(group_date) %>% 
  summarize(count = n())

# calculate counts of 30min time periods
counts <- DATA %>% 
  filter(year > 2012) %>% # after 2012
  group_by(name) %>% 
  summarize(count=n()) 

# total proportion of tracks with at least 10 hours
# at 30 min sampling rate from ctmm formatted data post 2012, needs 20 points for 10 hours
# older data pre 2012 did not have timestamps, 
# but original data should have 120 points if at least 10 hours because there was a 5 minute sampling rate
(nrow(old_counts[old_counts$count>=120,]) + nrow(counts[counts$count>=20,]))/(nrow(old_counts) + nrow(counts))

# proportion that is at least 5 hours:
(nrow(old_counts[old_counts$count>=60,]) + nrow(counts[counts$count>=10,]))/(nrow(old_counts) + nrow(counts))

#...................
##
# Compare occurence and range distribution
#

# read in data
DATA <- readRDS("Data/30min_trkpts_formatted") %>% 
  filter(year=="2016",
         individual.local.identifier ==  "FF_dry") %>% 
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

SVF <- variogram(DATA)
GUESS <- ctmm.guess(DATA,interactive=FALSE, variogram = SVF)
FITS <- ctmm.select(DATA,GUESS,trace=2)
UD <- akde(DATA,FITS, weights = TRUE, grid=list(dr=10,align.to.origin=TRUE))
OD <- occurrence(DATA,FITS, grid=list(dr=10,align.to.origin=TRUE))

# plots
png("Figures/range_occurence.png", width = 2000, height = 1100, pointsize = 30)
par(mfrow=c(1,2))
plot(DATA, UD=UD, main = "Range", lwd = 2, cex = 3, ext = extent(UD))
plot(DATA, UD=OD, main = "Occurence", lwd = 2, cex = 3, ext = extent(UD))
dev.off()


#.............................................

### Below seems liek nonsense, not sure what I was trying to do ...

## FIND MINIMUM HOURS IN A DAY IN COMPLETE SEGMENTS
comp_data <- readRDS("Data/CH1_GPS_data.rds")


### Other crossing times and sampling schedules

## BUFFALO 
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
