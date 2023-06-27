# load packages
library(tidyverse)
library(ctmm)
library(ctmmweb)
library(sf)
library(sp)
library(lubridate)
library(mapview)
library(patchwork)
library(cowplot)

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

# double check CE weird points are real -- look in database
ce <- data %>% 
  filter(group == "CE") %>% 
  st_as_sf(coords = c(3,4), crs = "+proj=longlat +datum=WGS84 +no_defs")
mapview(ce)

# clean bad points from FL group
fl <- data %>% 
  filter(group == "FL") %>% 
  st_as_sf(coords = c(3,4), crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  slice(-c(6180:6196)) %>% # clean some bad points
  bind_cols(.,st_coordinates(.) %>% as_tibble()) %>%
  as_tibble() %>% 
  dplyr::select(-geometry) %>%
  dplyr::select(individual.local.identifier,
                location.long = X,
                location.lat = Y,
                timestamp,
                season,
                year,
                group) %>%
  arrange(individual.local.identifier, timestamp)

# add back to main data frame
data <- data %>% 
  filter(!group == "FL") %>% 
  rbind(fl)


## get data within season, within year, past and future year

# aa

# aa_season <- data %>% 
#   filter(group == "AA",
#          year=="2014",
#          individual.local.identifier=="AA_dry",
#          !month(timestamp) %in% c(1,2)) %>% 
#   dplyr::select(-individual.local.identifier) %>%
#   rename(individual.local.indentifier = group) %>%
#   arrange(timestamp) %>%
#   as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

aa_year <- data %>% 
  filter(group == "AA",
         year=="2014",
         #!individual.local.identifier=="AA_dry",
         !month(timestamp) %in% c(1,2)) %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

aa_next_year <- data %>% 
  filter(group == "AA",
         year=="2015") %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

aa_prev_year <- data %>% 
  filter(group == "AA",
         year=="2013") %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")


# aa2
# aa2_season <- data %>% 
#   filter(group == "AA",
#          year=="2013",
#          individual.local.identifier=="AA_wet",
#          !month(timestamp) %in% c(11,12)) %>% 
#   dplyr::select(-individual.local.identifier) %>%
#   rename(individual.local.indentifier = group) %>%
#   arrange(timestamp) %>%
#   as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

aa2_year <- data %>% 
  filter(group == "AA",
         year=="2013",
         #!individual.local.identifier=="AA_wet",
         !month(timestamp) %in% c(11,12)) %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

aa2_next_year <- data %>% 
  filter(group == "AA",
         year=="2014") %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

aa2_prev_year <- data %>% 
  filter(group == "AA",
         year=="2012") %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# rr
# rr_season <- data %>% 
#   filter(group == "RR",
#          year=="2010",
#          #individual.local.identifier=="RR_dry",
#          !month(timestamp) %in% c(4,5)) %>% 
#   dplyr::select(-individual.local.identifier) %>%
#   rename(individual.local.indentifier = group) %>%
#   arrange(timestamp) %>%
#   as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

rr_year <- data %>% 
  filter(group == "RR",
         year=="2010",
         #!individual.local.identifier=="RR_dry",
         !month(timestamp) %in% c(4,5)) %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

rr_next_year <- data %>% 
  filter(group == "RR",
         year=="2011") %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

rr_prev_year <- data %>% 
  filter(group == "RR",
         year=="2009") %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# ce
# ce_season <- data %>% 
#   filter(group == "CE",
#          year=="2017",
#          individual.local.identifier=="CE_wet",
#          !month(timestamp) %in% c(7,8,9)) %>% 
#   dplyr::select(-individual.local.identifier) %>%
#   rename(individual.local.indentifier = group) %>%
#   arrange(timestamp) %>%
#   as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

ce_year <- data %>% 
  filter(group == "CE",
         year=="2017",
         #!individual.local.identifier=="CE_wet",
         !month(timestamp) %in% c(7,8,9)) %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

ce_next_year <- data %>% 
  filter(group == "CE",
         year=="2018") %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

ce_prev_year <- data %>% 
  filter(group == "CE",
         year=="2016") %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# fl
# fl_season <- data %>% 
#   filter(group == "FL",
#          year=="2013",
#          individual.local.identifier=="FL_wet",
#          !month(timestamp) %in% c(11,12)) %>% 
#   dplyr::select(-individual.local.identifier) %>%
#   rename(individual.local.indentifier = group) %>%
#   arrange(timestamp) %>%
#   as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

fl_year <- data %>% 
  filter(group == "FL",
         year=="2013",
         #!individual.local.identifier=="FL_wet",
         !month(timestamp) %in% c(11,12)) %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

fl_next_year <- data %>% 
  filter(group == "FL",
         year=="2014") %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

fl_prev_year <- data %>% 
  filter(group == "FL",
         year=="2012") %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# sp

# splinters complete segment already includes the full season so no points left out

sp_year <- data %>% 
  filter(group == "SP",
         year=="2010",
         !individual.local.identifier=="SP_wet") %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

sp_next_year <- data %>% 
  filter(group == "SP",
         year=="2011") %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

sp_prev_year <- data %>% 
  filter(group == "SP",
         year=="2009") %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# put into list for proportion calculations
sp_dlist <- list(sp_year, sp_next_year, sp_prev_year)
names(sp_dlist) <- c("sp_year", "sp_next_year", "sp_prev_year")

aa_dlist <- list(aa_year, aa_next_year, aa_prev_year)
names(aa_dlist) <- c("aa_year", "aa_next_year", "aa_prev_year")

fl_dlist <- list(fl_year, fl_next_year, fl_prev_year)
names(fl_dlist) <- c("fl_year", "fl_next_year", "fl_prev_year")

#.....................................
#
# Calculate proportion of points within HR for AA and SP
AAt <- AAp <-  AAp_low <- AAp_high <- AAf <-  AAf_low <- AAf_high <- list()
for(i in 1:length(aa_dlist)){
  AAt[[i]] <- SpatialPoints.telemetry(aa_dlist[i]) %over% SpatialPolygonsDataFrame.UD(AA ,level.UD=0.95) %>% 
    table(useNA = "always") %>% # keep number of points that fell outside upper CI
    data.frame()  %>% 
    mutate(Prop = round(Freq/sum(Freq), digits = 3), # calculates proportion from frequency
           Freq = Freq); 
  AAf[[i]] <- AAt[[i]]$Freq[1] + AAt[[i]]$Freq[3]; # takes the proportion that fell within lower CI plus proportion that fell between mean and lower
  AAf_low[[i]] <- AAt[[i]]$Freq[3]; # proportion only within lower bound
  AAf_high[[i]] <- AAt[[i]]$Freq[1] + AAt[[i]]$Freq[2] + AAt[[i]]$Freq[3]; # all combined to get upper bound
  AAp[[i]] <- AAt[[i]]$Prop[1] + AAt[[i]]$Prop[3]; # takes the proportion that fell within lower CI plus proportion that fell between mean and lower
  AAp_low[[i]] <- AAt[[i]]$Prop[3]; # proportion only within lower bound
  AAp_high[[i]] <- AAt[[i]]$Prop[1] + AAt[[i]]$Prop[2] + AAt[[i]]$Prop[3]
}
names(AAt) <- names(AAp_low) <- names(AAp_high) <- names(AAp) <- names(AAf_low) <- names(AAf_high) <- names(AAf) <- names(aa_dlist)


SPt <- SPp <-  SPp_low <- SPp_high <- SPf <-  SPf_low <- SPf_high <- list()
for(i in 1:length(sp_dlist)){
  SPt[[i]] <- SpatialPoints.telemetry(sp_dlist[i]) %over% SpatialPolygonsDataFrame.UD(SP ,level.UD=0.95) %>% 
    table(useNA = "always") %>% # keep number of points that fell outside upper CI
    data.frame()  %>% 
    mutate(Prop = round(Freq/sum(Freq), digits = 3), # calculates proportion from frequency
           Freq = Freq); 
  SPf[[i]] <- SPt[[i]]$Freq[1] + SPt[[i]]$Freq[3]; # takes the proportion that fell within lower CI plus proportion that fell between mean and lower
  SPf_low[[i]] <- SPt[[i]]$Freq[3]; # proportion only within lower bound
  SPf_high[[i]] <- SPt[[i]]$Freq[1] + SPt[[i]]$Freq[2] + SPt[[i]]$Freq[3]; # all combined to get upper bound
  SPp[[i]] <- SPt[[i]]$Prop[1] + SPt[[i]]$Prop[3]; # takes the proportion that fell within lower CI plus proportion that fell between mean and lower
  SPp_low[[i]] <- SPt[[i]]$Prop[3]; # proportion only within lower bound
  SPp_high[[i]] <- SPt[[i]]$Prop[1] + SPt[[i]]$Prop[2] + SPt[[i]]$Prop[3]
}
names(SPt) <- names(SPp_low) <- names(SPp_high) <- names(SPp) <- names(SPf_low) <- names(SPf_high) <- names(SPf) <- names(sp_dlist)

FLt <- FLp <-  FLp_low <- FLp_high <- FLf <-  FLf_low <- FLf_high <- list()
for(i in 1:length(fl_dlist)){
  FLt[[i]] <- SpatialPoints.telemetry(fl_dlist[i]) %over% SpatialPolygonsDataFrame.UD(FL ,level.UD=0.95) %>% 
    table(useNA = "always") %>% # keep number of points that fell outside upper CI
    data.frame()  %>% 
    mutate(Prop = round(Freq/sum(Freq), digits = 3), # calculates proportion from frequency
           Freq = Freq); 
  FLf[[i]] <- FLt[[i]]$Freq[1] + FLt[[i]]$Freq[3]; # takes the proportion that fell within lower CI plus proportion that fell between mean and lower
  FLf_low[[i]] <- FLt[[i]]$Freq[3]; # proportion only within lower bound
  FLf_high[[i]] <- FLt[[i]]$Freq[1] + FLt[[i]]$Freq[2] + FLt[[i]]$Freq[3]; # all combined to get upper bound
  FLp[[i]] <- FLt[[i]]$Prop[1] + FLt[[i]]$Prop[3]; # takes the proportion that fell within lower CI plus proportion that fell between mean and lower
  FLp_low[[i]] <- FLt[[i]]$Prop[3]; # proportion only within lower bound
  FLp_high[[i]] <- FLt[[i]]$Prop[1] + FLt[[i]]$Prop[2] + FLt[[i]]$Prop[3]
}
names(FLt) <- names(FLp_low) <- names(FLp_high) <- names(FLp) <- names(FLf_low) <- names(FLf_high) <- names(FLf) <- names(fl_dlist)

props_df <- tibble(ID = names(aa_dlist),
                   Group = "AA",
                   Prop = as.numeric(unlist(AAp)),
                   Prop_low = as.numeric(unlist(AAp_low)),
                   Prop_high = as.numeric(unlist(AAp_high)),
                   Freq = as.numeric(unlist(AAf)),
                   Freq_low = as.numeric(unlist(AAf_low)),
                   Freq_high = as.numeric(unlist(AAf_high))) %>%
  rbind(tibble(ID = names(sp_dlist),
               Group = "SP",
               Prop = as.numeric(unlist(SPp)),
               Prop_low = as.numeric(unlist(SPp_low)),
               Prop_high = as.numeric(unlist(SPp_high)),
               Freq = as.numeric(unlist(SPf)),
               Freq_low = as.numeric(unlist(SPf_low)),
               Freq_high = as.numeric(unlist(SPf_high)))) %>% 
  rbind(tibble(ID = names(fl_dlist),
               Group = "FL",
               Prop = as.numeric(unlist(FLp)),
               Prop_low = as.numeric(unlist(FLp_low)),
               Prop_high = as.numeric(unlist(FLp_high)),
               Freq = as.numeric(unlist(FLf)),
               Freq_low = as.numeric(unlist(FLf_low)),
               Freq_high = as.numeric(unlist(FLf_high))))


#...............................
#
## MAP FUTURE POINTS OVER HOME RANGE CONTOURS
#

par(mfrow = c(2,3))
plot(x = list(aa_future, aa_past), UD = AA, main= "AA", col = c("red", "yellow"))
plot(x = list(aa2_future, aa2_past), UD = AA2, main= "AA2", col = c("red", "yellow"))
plot(x = list(ce_future, ce_past), UD = CE, main= "CE", col = c("red", "yellow"))
plot(x = list(fl_future, fl_past), UD = FL, main= "FL", lwd = 2, col = c("red", "yellow"))
plot(x = list(rr_future, rr_past), UD = RR, main= "RR", lwd = 2, col = c("red", "yellow"))
plot(x = list(sp_future, sp_past), UD = SP, main= "SP", col = c("red", "yellow"))

# in season, in year, next year
plot(x = list(aa_prev_year, aa_season, aa_next_year), UD = AA, main= "AA", lwd = 3,col = c("red", "yellow", "green"))
plot(x = list(aa2_prev_year, aa2_year, aa2_next_year), UD = AA2, main= "AA2", lwd = 3,col = c("red", "yellow", "green"))
plot(x = list(ce_prev_year, ce_year, ce_next_year), UD = CE, main= "CE", lwd = 3,col = c("red", "yellow", "green"))
plot(x = list(rr_prev_year, rr_year, rr_next_year), UD = RR, main= "RR", lwd = 3,col = c("red", "yellow", "green"))
plot(x = list(fl_prev_year, fl_year, fl_next_year), UD = FL, main= "FL", lwd = 3,col = c("red", "yellow", "green"))
plot(x = list(sp_prev_year, sp_year, sp_next_year), UD = SP, main= "SP", lwd = 3,col = c("red", "yellow", "green"))

## GGPLOT

# transform mean boundries to sf
mean_hr_sf <- AA %>% 
  ctmm::as.sf(level.UD = 0.95) %>% 
  filter(str_detect(row.names(.), "est")) %>% 
  rename(id = name) %>% 
  mutate(id = "aa") %>% 
  # rbind(AA2 %>% 
  #         ctmm::as.sf(level.UD = 0.95) %>% 
  #         filter(str_detect(row.names(.), "est")) %>% 
  #         rename(id = name) %>% 
  #         mutate(id = "aa2")) %>% 
  # rbind(CE %>% 
  #         ctmm::as.sf(level.UD = 0.95) %>% 
  #         filter(str_detect(row.names(.), "est")) %>% 
  #         rename(id = name) %>% 
  #         mutate(id = "ce")) %>% 
  rbind(FL %>%
          ctmm::as.sf(level.UD = 0.95) %>%
          filter(str_detect(row.names(.), "est")) %>%
          rename(id = name) %>%
          mutate(id = "fl")) %>%
  # rbind(RR %>% 
  #         ctmm::as.sf(level.UD = 0.95) %>% 
  #         filter(str_detect(row.names(.), "est")) %>% 
  #         rename(id = name) %>% 
  #         mutate(id = "rr")) %>% 
  rbind(SP %>% 
          ctmm::as.sf(level.UD = 0.95) %>% 
          filter(str_detect(row.names(.), "est")) %>% 
          rename(id = name) %>% 
          mutate(id = "sp")) %>% 
  st_transform("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0")

# transform confidence intervals to sf
ci_hr_sf <- AA %>% 
  ctmm::as.sf(level.UD = 0.95) %>% 
  filter(!str_detect(row.names(.), "est")) %>% 
  rename(id = name) %>% 
  mutate(id = "aa") %>% 
  # rbind(AA2 %>% 
  #         ctmm::as.sf(level.UD = 0.95) %>% 
  #         filter(!str_detect(row.names(.), "est")) %>% 
  #         rename(id = name) %>% 
  #         mutate(id = "aa2")) %>% 
  # rbind(CE %>% 
  #         ctmm::as.sf(level.UD = 0.95) %>% 
  #         filter(!str_detect(row.names(.), "est")) %>% 
  #         rename(id = name) %>% 
  #         mutate(id = "ce")) %>% 
  rbind(FL %>%
          ctmm::as.sf(level.UD = 0.95) %>%
          filter(!str_detect(row.names(.), "est")) %>%
          rename(id = name) %>%
          mutate(id = "fl")) %>%
  # rbind(RR %>% 
  #         ctmm::as.sf(level.UD = 0.95) %>% 
  #         filter(!str_detect(row.names(.), "est")) %>% 
  #         rename(id = name) %>% 
  #         mutate(id = "rr")) %>% 
  rbind(SP %>% 
          ctmm::as.sf(level.UD = 0.95) %>% 
          filter(!str_detect(row.names(.), "est")) %>% 
          rename(id = name) %>% 
          mutate(id = "sp")) %>% 
  st_transform("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0")

# transform data in year to sf
data_year_sf <- aa_year %>% 
  ctmm::as.sf() %>% 
  rename(id = identity) %>% 
  mutate(id = "aa") %>%
  # rbind(aa2_year %>% 
  #         ctmm::as.sf() %>% 
  #         rename(id = identity) %>% 
  #         mutate(id = "aa2")) %>%
  # rbind(ce_year %>% 
  #         ctmm::as.sf() %>% 
  #         rename(id = identity) %>% 
  #         mutate(id = "ce")) %>%
  rbind(fl_year %>%
          ctmm::as.sf() %>%
          rename(id = identity) %>%
          mutate(id = "fl")) %>%
  # rbind(rr_year %>% 
  #         ctmm::as.sf() %>% 
  #         rename(id = identity) %>% 
  #         mutate(id = "rr")) %>%
  rbind(sp_year %>% 
          ctmm::as.sf() %>% 
          rename(id = identity) %>% 
          mutate(id = "sp")) %>%
  mutate(scale = "Within Year") %>% 
  st_transform("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0")

# transfrom future data to sf
data_future_sf <- aa_next_year %>% 
  ctmm::as.sf() %>% 
  rename(id = identity) %>% 
  mutate(id = "aa") %>%
  # rbind(aa2_next_year %>% 
  #         ctmm::as.sf() %>% 
  #         rename(id = identity) %>% 
  #         mutate(id = "aa2")) %>%
  # rbind(ce_next_year %>% 
  #         ctmm::as.sf() %>% 
  #         rename(id = identity) %>% 
  #         mutate(id = "ce")) %>%
  rbind(fl_next_year %>%
          ctmm::as.sf() %>%
          rename(id = identity) %>%
          mutate(id = "fl")) %>%
  # rbind(rr_next_year %>% 
  #         ctmm::as.sf() %>% 
  #         rename(id = identity) %>% 
  #         mutate(id = "rr")) %>%
  rbind(sp_next_year %>% 
          ctmm::as.sf() %>% 
          rename(id = identity) %>% 
          mutate(id = "sp")) %>%
  mutate(scale = "Following Year") %>% 
  st_transform("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0")

data_past_sf <- aa_prev_year %>% 
  ctmm::as.sf() %>% 
  rename(id = identity) %>% 
  mutate(id = "aa") %>%
  # rbind(aa2_prev_year %>% 
  #         ctmm::as.sf() %>% 
  #         rename(id = identity) %>% 
  #         mutate(id = "aa2")) %>%
  # rbind(ce_prev_year %>% 
  #         ctmm::as.sf() %>% 
  #         rename(id = identity) %>% 
  #         mutate(id = "ce")) %>%
  rbind(fl_prev_year %>%
          ctmm::as.sf() %>%
          rename(id = identity) %>%
          mutate(id = "fl")) %>%
  # rbind(rr_prev_year %>% 
  #         ctmm::as.sf() %>% 
  #         rename(id = identity) %>% 
  #         mutate(id = "rr")) %>%
  rbind(sp_prev_year %>% 
          ctmm::as.sf() %>% 
          rename(id = identity) %>% 
          mutate(id = "sp")) %>%
  mutate(scale = "Previous Year") %>% 
  st_transform("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0")

# combine into one dataframe
df_sf <- rbind(data_year_sf, data_past_sf, data_future_sf)
aa_sf <- df_sf %>% 
  filter(id == "aa") %>% 
  mutate(scale = str_c("AA Data", scale, sep = " "),
         prop_mean = ifelse(scale == "AA Data Within Year",
                              props_df$Prop[props_df$ID=="aa_year"],
                              ifelse(scale == "AA Data Previous Year",
                                     props_df$Prop[props_df$ID=="aa_prev_year"],
                                     props_df$Prop[props_df$ID=="aa_next_year"])),
         prop_low = ifelse(scale == "AA Data Within Year",
                           props_df$Prop_low[props_df$ID=="aa_year"],
                           ifelse(scale == "AA Data Previous Year",
                                  props_df$Prop_low[props_df$ID=="aa_prev_year"],
                                  props_df$Prop_low[props_df$ID=="aa_next_year"])),
         prop_high = ifelse(scale == "AA Data Within Year",
                            props_df$Prop_high[props_df$ID=="aa_year"],
                            ifelse(scale == "AA Data Previous Year",
                                   props_df$Prop_high[props_df$ID=="aa_prev_year"],
                                   props_df$Prop_high[props_df$ID=="aa_next_year"])),
         prop = str_c( "Proportion Within = ", as.character(prop_mean)))

sp_sf <- df_sf %>% 
  filter(id == "sp") %>% 
  mutate(scale = str_c("SP Data", scale, sep = " "),
         prop_mean = ifelse(scale == "SP Data Within Year",
                            props_df$Prop[props_df$ID=="sp_year"],
                            ifelse(scale == "SP Data Previous Year",
                                   props_df$Prop[props_df$ID=="sp_prev_year"],
                                   props_df$Prop[props_df$ID=="sp_next_year"])),
         prop_low = ifelse(scale == "SP Data Within Year",
                           props_df$Prop_low[props_df$ID=="sp_year"],
                           ifelse(scale == "SP Data Previous Year",
                                  props_df$Prop_low[props_df$ID=="sp_prev_year"],
                                  props_df$Prop_low[props_df$ID=="sp_next_year"])),
         prop_high = ifelse(scale == "SP Data Within Year",
                            props_df$Prop_high[props_df$ID=="sp_year"],
                            ifelse(scale == "SP Data Previous Year",
                                   props_df$Prop_high[props_df$ID=="sp_prev_year"],
                                   props_df$Prop_high[props_df$ID=="sp_next_year"])),
         prop = str_c( "Proportion Within = ", as.character(prop_mean)))

fl_sf <- df_sf %>% 
  filter(id == "fl") %>% 
  mutate(scale = str_c("FL Data", scale, sep = " "),
         prop_mean = ifelse(scale == "FL Data Within Year",
                            props_df$Prop[props_df$ID=="fl_year"],
                            ifelse(scale == "FL Data Previous Year",
                                   props_df$Prop[props_df$ID=="fl_prev_year"],
                                   props_df$Prop[props_df$ID=="fl_next_year"])),
         prop_low = ifelse(scale == "FL Data Within Year",
                           props_df$Prop_low[props_df$ID=="fl_year"],
                           ifelse(scale == "FL Data Previous Year",
                                  props_df$Prop_low[props_df$ID=="fl_prev_year"],
                                  props_df$Prop_low[props_df$ID=="fl_next_year"])),
         prop_high = ifelse(scale == "FL Data Within Year",
                            props_df$Prop_high[props_df$ID=="fl_year"],
                            ifelse(scale == "FL Data Previous Year",
                                   props_df$Prop_high[props_df$ID=="fl_prev_year"],
                                   props_df$Prop_high[props_df$ID=="fl_next_year"])),
         prop = str_c( "Proportion Within = ", as.character(prop_mean)))


## ggplot

# get colors
colors_aa <- c("AA Data Previous Year" = "#fca50a", "AA Data Within Year" = "#c13a50", "AA Data Following Year" = "#4c0c6b")
colors_sp <- c("SP Data Previous Year" = "#fca50a", "SP Data Within Year" = "#c13a50", "SP Data Following Year" = "#4c0c6b")
colors_fl <- c("FL Data Previous Year" = "#fca50a", "FL Data Within Year" = "#c13a50", "FL Data Following Year" = "#4c0c6b")
labels_aa <- c("AA Data Previous Year", "AA Data Within Year", "AA Data Following Year")
labels_sp <- c("SP Data Previous Year", "SP Data Within Year", "SP Data Following Year")
labels_fl <- c("FL Data Previous Year", "FL Data Within Year", "FL Data Following Year")

p1 <- ggplot() +
  geom_sf(data = aa_sf, 
          aes(color = scale),
          inherit.aes = FALSE, alpha = 1.5, shape = 1) +
  geom_sf(data = ci_hr_sf[ci_hr_sf$id=="aa",], # HR CIs
          inherit.aes = FALSE, 
          fill = NA, linetype = "dotted", alpha = 0.8, linewidth = 0.2) +
  geom_sf(data = mean_hr_sf[mean_hr_sf$id=="aa",], # HR mean boundary
          inherit.aes = FALSE, 
          fill =NA , alpha = 0.06, linewidth = 0.8) +
  geom_text(data = aa_sf, aes(x = 677.8, y = 1163.9, label = prop), size = 6) +
  ggspatial::annotation_scale(location = "bl", 
                              width_hint = 0.3,
                              height = unit(10,'pt'),
                              style = 'ticks') +
  coord_sf(datum = st_crs("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0"),
           xlim=c(676.200,680.500), ylim=c(1161.000, 1164.000)) +
  scale_color_manual(values = colors_aa, labels = labels_aa, name = "") +
  labs( x = "Easting (km)", y = "Northing (km)") +
  theme(legend.position="none", 
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 25),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5),
        strip.background = element_rect(color = "black", linewidth = 0.5),
        panel.spacing = unit(2, "lines")) +
  facet_wrap(~factor(scale, levels = c("AA Data Previous Year", "AA Data Within Year", "AA Data Following Year"))) 

p2 <- ggplot() +
  geom_sf(data = sp_sf, 
          aes(color = scale),
          inherit.aes = FALSE, alpha = 1.5, shape = 1) +
  geom_sf(data = ci_hr_sf[ci_hr_sf$id=="sp",], # HR CIs
          inherit.aes = FALSE, 
          fill = NA, linetype = "dotted", alpha = 0.8, linewidth = 0.2) +
  geom_sf(data = mean_hr_sf[mean_hr_sf$id=="sp",], # HR mean boundary
          inherit.aes = FALSE, 
          fill =NA , alpha = 0.06, linewidth = 0.8) +
  geom_text(data = sp_sf, aes(x = 677.3, y = 1162.4, label = prop), size = 6) +
  ggspatial::annotation_scale(location = "bl", 
                              width_hint = 0.3,
                              height = unit(10,'pt'),
                              style = 'ticks') +
  coord_sf(datum = st_crs("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
           xlim=c(676.250,679.000), ylim=c(1159.500, 1162.500)) +
  scale_color_manual(values = colors_sp, labels = labels_sp, name = "") +
  labs( x = "Easting (km)", y = "Northing (km)") +
  theme(legend.position="none", 
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 25),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5),
        strip.background = element_rect(color = "black", linewidth = 0.5),
        panel.spacing = unit(2, "lines")) +
  facet_wrap(~factor(scale, levels = c("SP Data Previous Year", "SP Data Within Year", "SP Data Following Year"))) 

p3 <- ggplot() +
  geom_sf(data = fl_sf, 
          aes(color = scale),
          inherit.aes = FALSE, alpha = 1.5, shape = 1) +
  geom_sf(data = ci_hr_sf[ci_hr_sf$id=="fl",], # HR CIs
          inherit.aes = FALSE, 
          fill = NA, linetype = "dotted", alpha = 0.8, linewidth = 0.2) +
  geom_sf(data = mean_hr_sf[mean_hr_sf$id=="fl",], # HR mean boundary
          inherit.aes = FALSE, 
          fill =NA , alpha = 0.06, linewidth = 0.8) +
  geom_text(data = fl_sf, aes(x = 677.2, y = 1166.8, label = prop), size = 6) +
  ggspatial::annotation_scale(location = "bl", 
                              width_hint = 0.3,
                              height = unit(10,'pt'),
                              style = 'ticks') +
  coord_sf(datum = st_crs("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
           xlim=c(675.500,680.000), ylim=c(1163.000, 1167.000)) +
  scale_color_manual(values = colors_fl, labels = labels_fl, name = "") +
  labs( x = "Easting (km)", y = "Northing (km)") +
  theme(legend.position="none", 
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 25),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5),
        strip.background = element_rect(color = "black", linewidth = 0.5),
        panel.spacing = unit(2, "lines")) +
  facet_wrap(~factor(scale, levels = c("FL Data Previous Year", "FL Data Within Year", "FL Data Following Year"))) 

p4 <- plot_grid(p1, p2, p3, ncol = 1, align = "hv") & theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

# Save plot
file.name <- paste0("complete_seg_validation_",
                                format(Sys.time(), "%Y_%m_%d_%H%M%S"), ".jpg")

ggsave(here::here("Figures", file.name),
       plot = p4,
       width = 4800,
       height = 4200,
       units = "px")



#.........................................
## FINAL ANALYSIS -----------
#

UDs_annual <- readRDS("C:/Users/ojacobson/Documents/PhD/R/CH2_Group_size/Intermediate/ctmm/HR/gps_1990-2019_AKDEs.rds")
UDs_seasonal <- readRDS("C:/Users/ojacobson/Documents/PhD/R/CH2_Group_size/Intermediate/ctmm/HR/gps_1990-2019_AKDEs_seasonal.rds")
DATA_annual <- readRDS("C:/Users/ojacobson/Documents/PhD/R/CH2_Group_size/Intermediate/ctmm/HR/gps_1990-2019_DATA.rds")
DATA_seasonal <- readRDS("C:/Users/ojacobson/Documents/PhD/R/CH2_Group_size/Intermediate/ctmm/HR/gps_1990-2019_DATA_seasonal.rds")

aa_annual <- UDs_annual[["aa_2014"]]
aa_wet <- UDs_seasonal[["aa_wet_2014"]]
aa_dry <- UDs_seasonal[["aa_dry_2014"]]

par(mfrow = c(2,3))
plot(DATA_annual[["aa_2013"]], UD =aa_annual, col.grid = NA)
plot(DATA_annual[["aa_2015"]], UD =aa_annual, col.grid = NA)
plot(DATA_annual[["aa_2016"]], UD =aa_annual, col.grid = NA)
plot(DATA_annual[["aa_2013"]], 
     UD =list(aa_dry, aa_wet), 
     col.grid = NA , col.DF = c("red", "blue"))
plot(DATA_annual[["aa_2015"]], 
     UD =list(aa_dry, aa_wet), 
     col.grid = NA, col.DF = c("red", "blue"))
plot(DATA_annual[["aa_2016"]], 
     UD =list(aa_dry, aa_wet), 
     col.grid = NA, col.DF = c("red", "blue"))

par(mfrow = c(2,3))
rr_annual <- UDs_annual[["rr_2012"]]
rr_wet <- UDs_seasonal[["rr_wet_2012"]]
rr_dry <- UDs_seasonal[["rr_dry_2012"]]

plot(DATA_annual[["rr_2011"]], UD =rr_annual, col.grid = NA)
plot(DATA_annual[["rr_2013"]], UD =rr_annual, col.grid = NA)
plot(DATA_annual[["rr_2014"]], UD =rr_annual, col.grid = NA)
plot(DATA_annual[["rr_2011"]], 
     UD =list(rr_dry, rr_wet), 
     col.grid = NA , col.DF = c("red", "blue"))
plot(DATA_annual[["rr_2013"]], 
     UD =list(rr_dry, rr_wet), 
     col.grid = NA, col.DF = c("red", "blue"))
plot(DATA_annual[["rr_2014"]] , 
     UD =list(rr_dry, rr_wet), 
     col.grid = NA, col.DF = c("red", "blue"))


ce_wet <- UDs_seasonal[["ce_wet_2015"]]
ce_dry <- UDs_seasonal[["ce_dry_2015"]]

par(mfrow = c(2,3))
plot(DATA_annual[["ce_2014"]], UD =ce_annual, col.grid = NA)
plot(DATA_annual[["ce_2016"]], UD =ce_annual, col.grid = NA)
plot(DATA_annual[["ce_2017"]], UD =ce_annual, col.grid = NA)
plot(DATA_annual[["ce_2014"]], 
     UD =list(ce_dry, ce_wet), 
     col.grid = NA , col.DF = c("red", "blue"))
plot(DATA_annual[["ce_2016"]], 
     UD =list(ce_dry, ce_wet), 
     col.grid = NA, col.DF = c("red", "blue"))
plot(DATA_annual[["ce_2017"]], 
     UD =list(ce_dry, ce_wet), 
     col.grid = NA, col.DF = c("red", "blue"))

# put into list
UD_list <- list(aa_annual,aa_wet,aa_dry, rr_annual, rr_wet, rr_dry)
names(UD_list) <- c("aa_annual", "aa_wet", "aa_dry", "rr_annual", "rr_wet", "rr_dry")

data_names <- c("aa_2013","aa_2015","aa_2016","rr_2011", "rr_2013" , "rr_2014" )
DATA_list <- DATA_annual[data_names]

#................................
##
## CALCULATE PROPORTIONS
#

aa_dlist <- DATA_list[1:3]
names(aa_dlist) <- c("aa_prev_year", "aa_next_year", "aa_next_2year")

rr_dlist <- DATA_list[4:6]
names(rr_dlist) <- c("rr_prev_year", "rr_next_year", "rr_next_2year")

#.....................................
#
# Calculate proportion of points within HR for AA and SP
AAt <- AAp <-  AAp_low <- AAp_high <- AAf <-  AAf_low <- AAf_high <- list()
for(i in 1:length(aa_dlist)){
  AAt[[i]] <- SpatialPoints.telemetry(aa_dlist[i]) %over% SpatialPolygonsDataFrame.UD(UD_list[[1]] ,level.UD=0.95) %>% 
    table(useNA = "always") %>% # keep number of points that fell outside upper CI
    data.frame()  %>% 
    mutate(Prop = round(Freq/sum(Freq), digits = 3), # calculates proportion from frequency
           Freq = Freq); 
  AAf[[i]] <- AAt[[i]]$Freq[1] + AAt[[i]]$Freq[3]; # takes the proportion that fell within lower CI plus proportion that fell between mean and lower
  AAf_low[[i]] <- AAt[[i]]$Freq[3]; # proportion only within lower bound
  AAf_high[[i]] <- AAt[[i]]$Freq[1] + AAt[[i]]$Freq[2] + AAt[[i]]$Freq[3]; # all combined to get upper bound
  AAp[[i]] <- AAt[[i]]$Prop[1] + AAt[[i]]$Prop[3]; # takes the proportion that fell within lower CI plus proportion that fell between mean and lower
  AAp_low[[i]] <- AAt[[i]]$Prop[3]; # proportion only within lower bound
  AAp_high[[i]] <- AAt[[i]]$Prop[1] + AAt[[i]]$Prop[2] + AAt[[i]]$Prop[3]
}
names(AAt) <- names(AAp_low) <- names(AAp_high) <- names(AAp) <- names(AAf_low) <- names(AAf_high) <- names(AAf) <- names(aa_dlist)

# rr
RRt <- RRp <-  RRp_low <- RRp_high <- RRf <-  RRf_low <- RRf_high <- list()
for(i in 1:length(rr_dlist)){
  RRt[[i]] <- SpatialPoints.telemetry(rr_dlist[i]) %over% SpatialPolygonsDataFrame.UD(UD_list[[4]] ,level.UD=0.95) %>% 
    table(useNA = "always") %>% # keep number of points that fell outside upper CI
    data.frame()  %>% 
    mutate(Prop = round(Freq/sum(Freq), digits = 3), # calculates proportion from frequency
           Freq = Freq); 
  RRf[[i]] <- RRt[[i]]$Freq[1] + RRt[[i]]$Freq[3]; # takes the proportion that fell within lower CI plus proportion that fell between mean and lower
  RRf_low[[i]] <- RRt[[i]]$Freq[3]; # proportion only within lower bound
  RRf_high[[i]] <- RRt[[i]]$Freq[1] + RRt[[i]]$Freq[2] + RRt[[i]]$Freq[3]; # all combined to get upper bound
  RRp[[i]] <- RRt[[i]]$Prop[1] + RRt[[i]]$Prop[3]; # takes the proportion that fell within lower CI plus proportion that fell between mean and lower
  RRp_low[[i]] <- RRt[[i]]$Prop[3]; # proportion only within lower bound
  RRp_high[[i]] <- RRt[[i]]$Prop[1] + RRt[[i]]$Prop[2] + RRt[[i]]$Prop[3]
}
names(RRt) <- names(RRp_low) <- names(RRp_high) <- names(RRp) <- names(RRf_low) <- names(RRf_high) <- names(RRf) <- names(rr_dlist)

# put in dataframe
props_df <- tibble(ID = names(aa_dlist),
                   Group = "AA",
                   Prop = as.numeric(unlist(AAp)),
                   Prop_low = as.numeric(unlist(AAp_low)),
                   Prop_high = as.numeric(unlist(AAp_high)),
                   Freq = as.numeric(unlist(AAf)),
                   Freq_low = as.numeric(unlist(AAf_low)),
                   Freq_high = as.numeric(unlist(AAf_high))) %>%
  rbind(tibble(ID = names(rr_dlist),
               Group = "RR",
               Prop = as.numeric(unlist(RRp)),
               Prop_low = as.numeric(unlist(RRp_low)),
               Prop_high = as.numeric(unlist(RRp_high)),
               Freq = as.numeric(unlist(RRf)),
               Freq_low = as.numeric(unlist(RRf_low)),
               Freq_high = as.numeric(unlist(RRf_high)))) 

### convert to sf objects for plotting
# transform mean boundries to sf
mean_hr_sf <- UD_list %>% 
  purrr::map(ctmm::as.sf, level.UD = 0.95) %>% 
  reduce(rbind) %>% 
  filter(str_detect(row.names(.), "est")) %>% 
  rename(id = name) %>%
  mutate(id = gsub( " .*$", "", id ),
         group = str_sub (id, 1,2)) %>% 
  st_transform("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0")

# transform confidence intervals to sf
ci_hr_sf <- UD_list %>% 
  purrr::map(ctmm::as.sf, level.UD = 0.95) %>% 
  reduce(rbind) %>% 
  filter(!str_detect(row.names(.), "est")) %>% 
  rename(id = name) %>%
  mutate(id = gsub( " .*$", "", id ),
         group = str_sub (id, 1,2)) %>% 
  st_transform("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0")

# transform data to sf
data_sf <- DATA_list %>% 
  purrr::map(ctmm::as.sf, level.UD = 0.95) %>% 
  reduce(rbind) %>% 
  rename(id = identity) %>%
  mutate(group = str_sub (id, 1,2),
         season = ifelse(month(timestamp) %in% c(1:6), "Dry Season", "Wet Season")) %>% 
  st_transform("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0")

# seperate groups and add scale column
aa_data_sf <- data_sf %>%
  filter(group == "aa") %>% 
  mutate(scale = ifelse(year(timestamp) == 2013,
                        "AA Data Previous Year",
                        ifelse(year(timestamp) == 2015,
                               "AA Data Following Year",
                               "AA Data 2 Years After")),
         prop_mean = ifelse(scale == "AA Data Previous Year",
                            props_df$Prop[props_df$ID=="aa_prev_year"],
                            ifelse(scale == "AA Data Following Year",
                                   props_df$Prop[props_df$ID=="aa_next_year"],
                                   props_df$Prop[props_df$ID=="aa_next_2year"])),
         prop = str_c( "Prop = ", as.character(prop_mean)))

rr_data_sf <- data_sf %>%
  filter(group == "rr") %>% 
  mutate(scale = ifelse(year(timestamp) == 2011,
                        "RR Data Previous Year",
                        ifelse(year(timestamp) == 2013,
                               "RR Data Following Year",
                               "RR Data 2 Years After")),
         prop_mean = ifelse(scale == "RR Data Previous Year",
                            props_df$Prop[props_df$ID=="rr_prev_year"],
                            ifelse(scale == "RR Data Following Year",
                                   props_df$Prop[props_df$ID=="rr_next_year"],
                                   props_df$Prop[props_df$ID=="rr_next_2year"])),
         prop = str_c( "Prop = ", as.character(prop_mean)))

colors <- c("Wet Season" = "#0072b2", "Dry Season" = "#d55e00")

# ggplot
p1 <- ggplot() +
  geom_sf(data = aa_data_sf, 
          aes(color = season),
          inherit.aes = FALSE, alpha = 1.5, shape = 1) +
  scale_color_manual(values = colors, labels = labels, name = "") +
  # geom_sf(data = ci_hr_sf[ci_hr_sf$id=="aa_wet_2014",], # HR CIs
  #         inherit.aes = FALSE, 
  #         fill = NA, linetype = "dotted", alpha = 0.8, linewidth = 0.2, color = "#0072b2") +
  # geom_sf(data = ci_hr_sf[ci_hr_sf$id=="aa_dry_2014",], # HR CIs
  #         inherit.aes = FALSE, 
  #         fill = NA, linetype = "dotted", alpha = 0.8, linewidth = 0.2, color = "#d55e00") +
  geom_sf(data = ci_hr_sf[ci_hr_sf$id=="aa_2014",], # HR CIs
          inherit.aes = FALSE,
          fill = NA, linetype = "dotted", alpha = 0.8, linewidth = 0.2) +
  # geom_sf(data = mean_hr_sf[mean_hr_sf$id=="aa_wet_2014",], # HR mean boundary
  #         inherit.aes = FALSE, 
  #         fill =NA , alpha = 0.06, linewidth = 0.8, color = "#0072b2") +
  # geom_sf(data = mean_hr_sf[mean_hr_sf$id=="aa_dry_2014",], # HR mean boundary
  #         inherit.aes = FALSE, 
  #         fill =NA , alpha = 0.06, linewidth = 0.8, color = "#d55e00") +
  geom_sf(data = mean_hr_sf[mean_hr_sf$id=="aa_2014",], # HR mean boundary
          inherit.aes = FALSE,
          fill =NA , alpha = 0.06, linewidth = 0.8) +
  geom_text(data = aa_data_sf, aes(x = 679.8, y = 1161.0, label = prop), size = 4) +
  ggspatial::annotation_scale(location = "bl",
                              width_hint = 0.3,
                              height = unit(4,'pt'),
                              style = 'ticks') +
  coord_sf(datum = st_crs("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0"),
           xlim=c(676.200,680.500), ylim=c(1161.000, 1164.000)) +
  # coord_sf(datum = st_crs("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0"),
  #          xlim=c(676.500,680.100), ylim=c(1161.000, 1164.000)) +
  #scale_color_manual(values = colors, labels = labels) +
  labs( x = "Easting (km)", y = "Northing (km)") +
  theme(legend.position="none", 
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        legend.key=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 12),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5),
        strip.background = element_rect(color = "black", linewidth = 0.5)) +
  guides(color = guide_legend(override.aes = list(size=5))) +
  facet_wrap(~factor(scale, levels = c("AA Data Previous Year", "AA Data Following Year", "AA Data 2 Years After")))

p2 <- ggplot() +
  geom_sf(data = rr_data_sf, 
          aes(color = season),
          inherit.aes = FALSE, alpha = 1.5, shape = 1) +
  scale_color_manual(values = colors, name = "") +
  # geom_sf(data = ci_hr_sf[ci_hr_sf$id=="aa_wet_2014",], # HR CIs
  #         inherit.aes = FALSE, 
  #         fill = NA, linetype = "dotted", alpha = 0.8, linewidth = 0.2, color = "#0072b2") +
  # geom_sf(data = ci_hr_sf[ci_hr_sf$id=="aa_dry_2014",], # HR CIs
  #         inherit.aes = FALSE, 
  #         fill = NA, linetype = "dotted", alpha = 0.8, linewidth = 0.2, color = "#d55e00") +
  geom_sf(data = ci_hr_sf[ci_hr_sf$id=="rr_2012",], # HR CIs
          inherit.aes = FALSE,
          fill = NA, linetype = "dotted", alpha = 0.8, linewidth = 0.2) +
  # geom_sf(data = mean_hr_sf[mean_hr_sf$id=="aa_wet_2014",], # HR mean boundary
  #         inherit.aes = FALSE, 
  #         fill =NA , alpha = 0.06, linewidth = 0.8, color = "#0072b2") +
  # geom_sf(data = mean_hr_sf[mean_hr_sf$id=="aa_dry_2014",], # HR mean boundary
  #         inherit.aes = FALSE, 
  #         fill =NA , alpha = 0.06, linewidth = 0.8, color = "#d55e00") +
  geom_sf(data = mean_hr_sf[mean_hr_sf$id=="rr_2012",], # HR mean boundary
          inherit.aes = FALSE,
          fill =NA , alpha = 0.06, linewidth = 0.8) +
  geom_text(data = rr_data_sf, aes(x = 679.8, y = 1159.9, label = prop), size = 4) +
  ggspatial::annotation_scale(location = "bl", 
                              width_hint = 0.3,
                              height = unit(4,'pt'),
                              style = 'ticks') +
  # coord_sf(datum = st_crs("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0"),
  #          xlim=c(677.000,680.000), ylim=c(1160.000, 1163.500)) +
  coord_sf(datum = st_crs("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0"),
          xlim=c(676.500,680.500), ylim=c(1159.900, 1162.750)) +
  labs( x = "Easting (km)", y = "Northing (km)") +
  theme(legend.position="bottom", 
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        legend.key=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 12),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5),
        strip.background = element_rect(color = "black", linewidth = 0.5)) +
  guides(color = guide_legend(override.aes = list(size=5))) +
  facet_wrap(~factor(scale, levels = c("RR Data Previous Year", "RR Data Following Year", "RR Data 2 Years After")))


p3 <- egg::ggarrange(p1,p2)

# Save plot
file.name <- paste0("stationarity_plots",
                    format(Sys.time(), "%Y_%m_%d_%H%M%S"), ".jpg")

ggsave(here::here("Figures", file.name),
       plot = p3,
       width = 3000,
       height = 2000,
       units = "px")

####
# Variogram
#

# read in data for each group 
data <- readRDS("Data/30min_trkpts_formatted") %>% 
  mutate(group = str_sub(individual.local.identifier, 1, 2))

aa_data_long <- data %>% 
  filter(group == "AA",
         year %in% c("2013","2014", "2015", "2016")) %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

rr_data_long <- data %>% 
  filter(group == "RR",
         year %in% c("2011","2012", "2013", "2014")) %>% 
  dplyr::select(-individual.local.identifier) %>%
  rename(individual.local.indentifier = group) %>%
  arrange(timestamp) %>%
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

SVF_aa <- variogram(aa_data_long, dt = c(1,40) %#% "hour") 
SVF_rr <- variogram(rr_data_long, dt = c(1,50) %#% "hour") 

par(mfrow= c(1,2))
plot(SVF_aa, main = "AA", fraction = 1)
plot(SVF_rr, main = "RR", fraction = 1)
