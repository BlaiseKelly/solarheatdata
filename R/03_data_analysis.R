library(dplyr)
library(purrr)
library(sf)
library(lubridate)
library(stringr)
library(tmap)
library(openair)

voroni <- st_read('shp/voroni.shp')

site_ids <- paste0('outputs/', unique(voroni$id), '.csv')

read_c <- function(x){
  tryCatch({
  f <- read.csv(x)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

all_dat <- map_dfr(site_ids, read_c)

sites <- all_dat %>% 
  select(-X) %>% 
  mutate(date = dmy_hm(str_sub(date, 0, -7)),
         solar_heat_MWh = as.numeric(gsub(",", ".", solar_heat_MWh)),
         id = as.character(site_num)) %>% 
  arrange(id, date) %>% 
  mutate(difz = solar_heat_MWh - lag(solar_heat_MWh, default = solar_heat_MWh[1]),
         ID = seq(1:NROW(id)))

## some sites have the same values for heat generated for multiple hours, notably during the night
bad_dat <- sites[sites$solar_heat_MWh > 0 & sites$difz == 0 & sites$difz - lag(sites$difz, default = sites$difz[1]) == 0,]  

sites <- sites %>% filter(!ID %in% bad_dat$ID) %>% select(-ID)

timeVariation(sites2, "solar_heat_MWh")

site_tot <- sites %>% 
  mutate(yr = year(date)) %>% 
  group_by(yr, id) %>% 
  summarise(tot = sum(solar_heat_MWh)) %>%  
  filter(!is.na(tot))

all_tot_yr <- sites %>% 
  mutate(yr = year(date)) %>% 
  group_by(yr) %>% 
  summarise(tot = sum(solar_heat_MWh)) %>%  
  filter(!is.na(tot))

all_tot_hr <- sites %>% 
  mutate(yr = year(date)) %>% 
  group_by(date) %>% 
  summarise(tot = sum(solar_heat_MWh)) %>%  
  filter(!is.na(tot))

all_tot_hr_2020 <- filter(all_tot_hr, date > "2018-12-31 23:00:00" & date < "2020-01-01 00:00:00")

timeVariation(all_tot_hr_2020, "tot", group = "year")

voroni_tot <- voroni %>% 
  left_join(site_tot, by = 'id') %>%  
  filter(!is.na(yr),
         yr == '2020') %>% 
  mutate('total generation (MWh)' = tot)

bks <- c(0, 2500, 5000, 7500, 10000, 15000, 
         20000, 30000, 40000, Inf)

## palette generated from R base graphics plot which is not clearly defined
me_pal <- c("#0000b3", "#0000eb", "#1d00ff", "#4a00ff", "#7600ff", "#a211ee", "#cf2ed1", "#fb4ab5", 
            "#ff6798", "#ff837c", "#ff9f60", "#ffbc43", "#ffd827", "#fff50a")

tm1 <- tm_shape(voroni)+
  tm_polygons()+
  tm_shape(voroni_tot) +
  tm_polygons('total generation (MWh)', palette = me_pal, border.col = 'black', breaks = bks)+
  tm_layout(legend.outside = TRUE, frame = FALSE)+
  tm_facets(along = "yr", free.coords = FALSE)

tmap_save(tm1, "plots/dk_heat_2020.png") 

voroni_tot <- voroni %>% 
  left_join(site_tot, by = 'id') %>%  
  filter(!is.na(yr)) %>% 
  mutate('total generation (MWh)' = tot)

bks <- c(0, 2500, 5000, 7500, 10000, 15000, 
         20000, 30000, 40000, Inf)

## palette generated from R base graphics plot which is not clearly defined
me_pal <- c("#0000b3", "#0000eb", "#1d00ff", "#4a00ff", "#7600ff", "#a211ee", "#cf2ed1", "#fb4ab5", 
            "#ff6798", "#ff837c", "#ff9f60", "#ffbc43", "#ffd827", "#fff50a")

tm1 <- tm_shape(voroni)+
  tm_polygons()+
  tm_shape(voroni_tot) +
  tm_polygons('total generation (MWh)', palette = me_pal, border.col = 'black', breaks = bks)+
  tm_layout(legend.outside = TRUE, frame = FALSE)+
  tm_facets(along = "yr", free.coords = FALSE)
tm1
tmap_animation(tm1, filename = "plots/dk_heat.gif", delay = 100)

## municipality
dk_kom <- st_read("http://data.information.dk/open/dagi/geojson/dagi-ref-kommuner.geojson")
dk <- select(dk_kom, KOMKODE) %>% st_transform(4326)
sites_muni <- readRDS("sites_muni.RDS")
st_geometry(sites_muni) <- NULL
sites <- left_join(sites, sites_muni, by = "id")

muni_tot_yr <- sites %>% 
  mutate(yr = year(date)) %>% 
  group_by(yr, muni) %>% 
  summarise(tot = sum(solar_heat_MWh)) %>%  
  filter(!is.na(tot))

muni_tot_geo <- left_join(dk, muni_tot_yr, by = c("KOMKODE" = "muni")) %>% filter(!is.na(yr)) %>% 
  mutate('total generation (MWh)' = tot)

tm2 <- tm_shape(dk)+
  tm_polygons()+
  tm_shape(muni_tot_geo) +
  tm_polygons('total generation (MWh)', palette = me_pal, border.col = 'black', breaks = bks)+
  tm_layout(legend.outside = TRUE, frame = FALSE)+
  tm_facets(along = "yr", free.coords = FALSE)
tm2

tmap_animation(tm2, filename = "plots/dk_muni_heat.gif", delay = 100)

muni_tot_geo_2021 <- left_join(dk, muni_tot_yr, by = c("KOMKODE" = "muni")) %>% filter(!is.na(yr) & yr == "2021") %>% 
  mutate('total generation (MWh)' = tot)

tm3 <- tm_shape(dk)+
  tm_polygons()+
  tm_shape(muni_tot_geo_2021) +
  tm_polygons('total generation (MWh)', palette = me_pal, border.col = 'black', breaks = bks)+
  tm_layout(legend.outside = TRUE, frame = FALSE)
tm3

tmap_save(tm3, "plots/dk_heat_muni_2021.png") 
