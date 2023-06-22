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

ce <- data %>% 
  filter(group == "CE") %>% 
  st_as_sf(coords = c(3,4), crs = "+proj=longlat +datum=WGS84 +no_defs")

fl <- data %>% 
  filter(group == "FL") %>% 
  st_as_sf(coords = c(3,4), crs = "+proj=longlat +datum=WGS84 +no_defs")
mapview::mapview(ce)
mapview::mapview(fl)
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

## GGPLOT

# transform mean boundries to sf
mean_hr_sf <- AA %>% 
  ctmm::as.sf(level.UD = 0.95) %>% 
  filter(str_detect(row.names(.), "est")) %>% 
  rename(id = name) %>% 
  mutate(id = "aa") %>% 
  rbind(AA2 %>% 
          ctmm::as.sf(level.UD = 0.95) %>% 
          filter(str_detect(row.names(.), "est")) %>% 
          rename(id = name) %>% 
          mutate(id = "aa2")) %>% 
  rbind(CE %>% 
          ctmm::as.sf(level.UD = 0.95) %>% 
          filter(str_detect(row.names(.), "est")) %>% 
          rename(id = name) %>% 
          mutate(id = "ce")) %>% 
  rbind(FL %>% 
          ctmm::as.sf(level.UD = 0.95) %>% 
          filter(str_detect(row.names(.), "est")) %>% 
          rename(id = name) %>% 
          mutate(id = "fl")) %>% 
  rbind(RR %>% 
          ctmm::as.sf(level.UD = 0.95) %>% 
          filter(str_detect(row.names(.), "est")) %>% 
          rename(id = name) %>% 
          mutate(id = "rr")) %>% 
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
  rbind(AA2 %>% 
          ctmm::as.sf(level.UD = 0.95) %>% 
          filter(!str_detect(row.names(.), "est")) %>% 
          rename(id = name) %>% 
          mutate(id = "aa2")) %>% 
  rbind(CE %>% 
          ctmm::as.sf(level.UD = 0.95) %>% 
          filter(!str_detect(row.names(.), "est")) %>% 
          rename(id = name) %>% 
          mutate(id = "ce")) %>% 
  rbind(FL %>% 
          ctmm::as.sf(level.UD = 0.95) %>% 
          filter(!str_detect(row.names(.), "est")) %>% 
          rename(id = name) %>% 
          mutate(id = "fl")) %>% 
  rbind(RR %>% 
          ctmm::as.sf(level.UD = 0.95) %>% 
          filter(!str_detect(row.names(.), "est")) %>% 
          rename(id = name) %>% 
          mutate(id = "rr")) %>% 
  rbind(SP %>% 
          ctmm::as.sf(level.UD = 0.95) %>% 
          filter(!str_detect(row.names(.), "est")) %>% 
          rename(id = name) %>% 
          mutate(id = "sp")) %>% 
  st_transform("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0")

# transform past data to sf
data_past_sf <- aa_past %>% 
  ctmm::as.sf() %>% 
  rename(id = identity) %>% 
  mutate(id = "aa") %>%
  rbind(aa2_past %>% 
          ctmm::as.sf() %>% 
          rename(id = identity) %>% 
          mutate(id = "aa2")) %>%
  rbind(ce_past %>% 
          ctmm::as.sf() %>% 
          rename(id = identity) %>% 
          mutate(id = "ce")) %>%
  rbind(fl_past %>% 
          ctmm::as.sf() %>% 
          rename(id = identity) %>% 
          mutate(id = "fl")) %>%
  rbind(rr_past %>% 
          ctmm::as.sf() %>% 
          rename(id = identity) %>% 
          mutate(id = "rr")) %>%
  rbind(sp_past %>% 
          ctmm::as.sf() %>% 
          rename(id = identity) %>% 
          mutate(id = "sp")) %>%
  st_transform("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0")

# transfrom future data to sf
data_future_sf <- aa_future %>% 
  ctmm::as.sf() %>% 
  rename(id = identity) %>% 
  mutate(id = "aa") %>%
  rbind(aa2_future %>% 
          ctmm::as.sf() %>% 
          rename(id = identity) %>% 
          mutate(id = "aa2")) %>%
  rbind(ce_future %>% 
          ctmm::as.sf() %>% 
          rename(id = identity) %>% 
          mutate(id = "ce")) %>%
  rbind(fl_future %>% 
          ctmm::as.sf() %>% 
          rename(id = identity) %>% 
          mutate(id = "fl")) %>%
  rbind(rr_future %>% 
          ctmm::as.sf() %>% 
          rename(id = identity) %>% 
          mutate(id = "rr")) %>%
  rbind(sp_future %>% 
          ctmm::as.sf() %>% 
          rename(id = identity) %>% 
          mutate(id = "sp")) %>%
  st_transform("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0")

## ggplot

# get colors
colors <- c("future data" = "#de4968", "past data" = "#40bd72")
labels <- c("future data", "past data")

ggplot() +
  geom_sf(data = data_future_sf, 
          aes(color = "future data"),
          inherit.aes = FALSE, alpha = 1.5, shape = 1) +
  geom_sf(data = data_past_sf, 
          aes(color = "past data"),
          inherit.aes = FALSE, alpha = 0.6, shape = 1) +
  geom_sf(data = mean_hr_sf, # HR CIs
          inherit.aes = FALSE, 
          fill = NA, linetype = "dashed", alpha = 0.3, size = 0.3) +
  geom_sf(data = ci_hr_sf, # HR mean boundary
          inherit.aes = FALSE, 
          fill =NA , alpha = 0.06, size = 0.8) +
  ggspatial::annotation_scale(location = "bl", 
                              width_hint = 0.3,
                              height = unit(4,'pt'),
                              style = 'ticks') +
  coord_sf(datum = st_crs("+proj=utm +zone=16 +north +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
           xlim=c(676.500,680.000), ylim=c(1161.000, 1164.000)) +
  scale_color_manual(values = colors, labels = labels, name = "") +
  # scale_x_continuous(labels = scales::label_number(accuracy = .1)) +
  # scale_y_continuous(labels = scales::label_number(accuracy = .1, big.mark = "")) +
  # guides(color=guide_legend(override.aes=list(size = 8)))+
  labs( x = "Easting (km)", y = "Northing (km)") +
  theme_bw(base_family = "ArcherPro Book") +
  theme(legend.position="bottom", 
        legend.title = element_blank(),
        legend.text=element_text(size=14),
        #axis.text.x = element_text(angle=45, hjust = 1),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 15)) +
  facet_wrap(~id, nrow = 2) 
