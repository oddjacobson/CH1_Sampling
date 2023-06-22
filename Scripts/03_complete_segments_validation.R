# load packages
library(tidyverse)
library(ctmm)
library(ctmmweb)
library(sf)
library(lubridate)

# load in home ranges from complete segments
AA <- readRDS("Intermediate/ctmm/AKDE_aa.rds")[[1]]
AA2 <- readRDS("Intermediate/ctmm/AKDE_aa2.rds")[[1]]
CE <- readRDS("Intermediate/ctmm/AKDE_ce.rds")[[1]]
FL <- readRDS("Intermediate/ctmm/AKDE_fl.rds")[[1]]
RR <- readRDS("Intermediate/ctmm/AKDE_rr.rds")[[1]]
SP <- readRDS("Intermediate/ctmm/AKDE_sp.rds")[[1]]


# read in data for each group 
data <- readRDS("Data/30min_trkpts_formatted") %>% 
  mutate(group = str_sub(individual.local.identifier, 1, 2))

## get future data for sunsequent 3 years for each group and convert to tele object

# aa

aa_future <- data %>%
  filter(group == "AA",
         year %in% c("2014", "2015"),
         as_date(timestamp) > as_date("2014-02-28"),
         as_date(timestamp) < as_date("2015-03-01")) %>%
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

aa_past <- data %>%
  filter(group == "AA",
         year %in% c("2013")) %>%
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# aa_past_future <- data %>% 
#   filter(group == "AA", 
#          year %in% c("2014", "2015", "2016", "2017"),
#          as_date(timestamp) > as_date("2014-02-28"),
#          as_date(timestamp) < as_date("2017-03-01")) %>% 
#   dplyr::select(-individual.local.identifier) %>% 
#   rename(individual.local.indentifier = group) %>% 
#   arrange(timestamp) %>% 
#   as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# aa2
aa2_future <- data %>% 
  filter(group == "AA", 
         year %in% c("2014")) %>% 
  dplyr::select(-individual.local.identifier) %>% 
  rename(individual.local.indentifier = group) %>% 
  arrange(timestamp) %>% 
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

aa2_past <- data %>% 
  filter(group == "AA", 
         year %in% c("2012", "2013"),
         as_date(timestamp) < as_date("2013-11-01"),
         as_date(timestamp) > as_date("2012-10-31")) %>% 
  dplyr::select(-individual.local.identifier) %>% 
  rename(individual.local.indentifier = group) %>% 
  arrange(timestamp) %>% 
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# rr
rr_future <- data %>% 
  filter(group == "RR", 
         year %in% c("2010", "2011"),
         as_date(timestamp) > as_date("2010-05-30"),
         as_date(timestamp) < as_date("2011-06-01")) %>% 
  dplyr::select(-individual.local.identifier) %>% 
  rename(individual.local.indentifier = group) %>% 
  arrange(timestamp) %>% 
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

rr_past <- data %>% 
  filter(group == "RR", 
         year %in% c("2009", "2010"),
         as_date(timestamp) < as_date("2010-04-01"),
         as_date(timestamp) > as_date("2009-03-31")) %>% 
  dplyr::select(-individual.local.identifier) %>% 
  rename(individual.local.indentifier = group) %>% 
  arrange(timestamp) %>% 
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# ce
ce_future <- data %>% 
  filter(group == "CE", 
         year %in% c("2017", "2018"),
         as_date(timestamp) > as_date("2017-09-30"),
         as_date(timestamp) < as_date("2018-10-01")) %>% 
  dplyr::select(-individual.local.identifier) %>% 
  rename(individual.local.indentifier = group) %>% 
  arrange(timestamp) %>% 
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

ce_past <- data %>% 
  filter(group == "CE", 
         year %in% c("2016", "2017"),
         as_date(timestamp) < as_date("2017-07-01"),
         as_date(timestamp) > as_date("2016-06-30")) %>% 
  dplyr::select(-individual.local.identifier) %>% 
  rename(individual.local.indentifier = group) %>% 
  arrange(timestamp) %>% 
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# fl
fl_future <- data %>% 
  filter(group == "FL", 
         year %in% c("2014", "2015"),
         as_date(timestamp) > as_date("2014-02-28"),
         as_date(timestamp) < as_date("2015-03-01")) %>% 
  dplyr::select(-individual.local.identifier) %>% 
  rename(individual.local.indentifier = group) %>% 
  arrange(timestamp) %>% 
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

fl_past <- data %>% 
  filter(group == "FL", 
         year %in% c("2012", "2013"),
         as_date(timestamp) < as_date("2013-11-01"),
         as_date(timestamp) > as_date("2012-10-31")) %>% 
  dplyr::select(-individual.local.identifier) %>% 
  rename(individual.local.indentifier = group) %>% 
  arrange(timestamp) %>% 
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# sp
sp_future <- data %>% 
  filter(group == "SP", 
         year %in% c("2011")) %>% 
  dplyr::select(-individual.local.identifier) %>% 
  rename(individual.local.indentifier = group) %>% 
  arrange(timestamp) %>% 
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

sp_past <- data %>% 
  filter(group == "SP", 
         year %in% c("2009", "2010"),
         as_date(timestamp) < as_date("2010-07-01"),
         as_date(timestamp) > as_date("2009-06-30")) %>% 
  dplyr::select(-individual.local.identifier) %>% 
  rename(individual.local.indentifier = group) %>% 
  arrange(timestamp) %>% 
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")


#...............................
#
## MAP FUTURE POINTS OVER HOME RANGE CONTOURS
#

par(mfrow = c(2,3))
plot(x = list(aa_future, aa_past), UD = AA, main= "AA", col = c("red", "yellow"))
plot(x = list(aa2_future, aa2_past), UD = AA2, main= "AA2", col = c("red", "yellow"))
plot(x = list(ce_future, ce_past), UD = CE, main= "CE", col = c("red", "yellow"))
plot(x = list(fl_future, fl_past), UD = FL, main= "FL", col = c("red", "yellow"))
plot(x = list(rr_future, rr_past), UD = RR, main= "RR", col = c("red", "yellow"))
plot(x = list(sp_future, sp_past), UD = SP, main= "SP", col = c("red", "yellow"))
