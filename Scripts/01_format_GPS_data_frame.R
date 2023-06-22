library(tidyverse)
library(ctmm)
library(sf)
library(lubridate)
library(ggmap)


# get all complete segments
a1 <- read_rds("Data/30min_trkpts_formatted") %>% 
  filter(year=="2014",
         individual.local.identifier=="AA_dry",
         month(timestamp) %in% c(1,2)) %>% # 43 unique days
  mutate(group = str_sub(individual.local.identifier,1,2),
         individual.local.identifier = "all",
         date = date(timestamp)) %>% 
  dplyr::select(group, individual.local.identifier, timestamp, location.long, location.lat)

r1 <-  read_rds("Data/30min_trkpts_formatted") %>% 
  filter(year=="2010",
         individual.local.identifier=="RR_dry",
         month(timestamp) %in% c(4,5)) %>%  # 39 unique days
  mutate(group = str_sub(individual.local.identifier,1,2),
         individual.local.identifier = "all") %>% 
  dplyr::select(group, individual.local.identifier, timestamp, location.long, location.lat)

c1 <- read_rds("Data/30min_trkpts_formatted") %>% 
  filter(year=="2017",
         individual.local.identifier=="CE_wet",
         month(timestamp) %in% c(7,8,9)) %>% 
  mutate(group = str_sub(individual.local.identifier,1,2),
         individual.local.identifier = "all") %>% 
  dplyr::select(group, individual.local.identifier, timestamp, location.long, location.lat)

aa1 <- read_rds("Data/30min_trkpts_formatted") %>% 
  filter(year=="2013",
         individual.local.identifier=="AA_wet",
         month(timestamp) %in% c(11,12),
         date(timestamp) < date("2013-12-31")) %>% # 42 unique days
  mutate(group = "AA2",
         individual.local.identifier = "all") %>% 
  dplyr::select(group, individual.local.identifier, timestamp, location.long, location.lat)

s1 <- read_rds("Data/30min_trkpts_formatted") %>% 
  filter(year=="2010",
         individual.local.identifier=="SP_wet",
         date(timestamp) < date("2010-12-31")) %>% # 48 unique days
  mutate(group = str_sub(individual.local.identifier,1,2),
         individual.local.identifier = "all",
         date = date(timestamp))  %>% 
  dplyr::select(group, individual.local.identifier, timestamp, location.long, location.lat)

f1 <- read_rds("Data/30min_trkpts_formatted") %>% 
  mutate(group = str_sub(individual.local.identifier, 1,2),
         season_year = str_c(season,year, sep = "_")) %>% 
  filter(season_year %in% c("wet_2013", "dry_2014"),
         group=="FL",
         month(timestamp) %in% c(11,12,1,2),
         date(timestamp) > date("2013-11-02")) %>% # 64 unique days
  mutate(individual.local.identifier = "all",
         date = date(timestamp)) %>%
  arrange(timestamp) %>% 
  dplyr::select(group, individual.local.identifier, timestamp, location.long, location.lat)

# combine into sf dataframe to save as shapefile
df <- rbind(a1,r1,c1,aa1,s1,f1) %>% 
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
           coords = c("location.long", "location.lat"))

# save as shapefile
st_write(df, "Data/CH1_GPS_data.shp")

# combine as ctmm formatted dataframe
df_ctmm <- rbind(a1,r1,c1,aa1,s1,f1)

# Save table and clean up
write.csv(df_ctmm,
          here::here("Data/CH1_GPS_data.csv"),
          na = "",
          row.names = FALSE)

# save as RDS
saveRDS(df_ctmm, "Data/CH1_GPS_data.rds" )


## make map of data -----------

#register API Key for google maps
register_google(key = "AIzaSyDeh4DbtfywpRnUuSZIqht15nDRvHDZ4L4")

# get satellite map
map <- get_map(location = c(left = -85.42, bottom = 10.47, right = -85.32, top = 10.56),
               maptype = "satellite", source = "google")

p1 <- ggmap(map) + 
  geom_sf(data = df, 
          aes(color = group),
          inherit.aes = FALSE,
          alpha = 0.3, 
          size = 1.5) +
  coord_sf(xlim=c(-85.40,-85.35), ylim=c(10.487, 10.545)) +
  #labs(title = "AA pre-2007") +
  #guides(color=guide_legend(override.aes=list(size = 3)))+
  theme_bw(base_family = "ArcherPro Book")

# Save map
file.name <- paste0("CH1_GPS_data_",
                    format(Sys.time(), "%Y_%m_%d_%H%M%S"), ".jpg")

ggsave(here::here("Figures", file.name),
       plot = p1,
       width = 3000,
       height = 3000,
       units = "px")
