library(dplyr)
library(purrr)
library(sf)
library(lubridate)
library(stringr)
library(tmap)

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
         id = as.character(site_num))

site_tot <- sites %>% 
  mutate(yr = year(date)) %>% 
  group_by(yr, id) %>% 
  summarise(tot = sum(solar_heat_MWh)) %>%  
  filter(!is.na(tot))

voroni_tot <- voroni %>% 
  left_join(site_tot, by = 'id') %>%  
  filter(!is.na(yr),
         !yr == '2021') %>% 
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
  

tmap_animation(tm1, filename = "plots/dk_heat.gif", delay = 100)
