library(reshape2)
library(tidyverse)
library(sf)

##define coordinate systems
latlong = "+init=epsg:4326"
rdnew = "+init=epsg:28992"

site_names <- c("Strandby","Taars","Vraa","Jerslev", "Saeby", "Saeby 2","Jetsmark","Aabybro","Hjallerup", 
                "Dronninglund","Asaa","Ulsted", "Mou", "Snedsted", "Durup","Hadsund","Gjerlev","Ramsing-Lem-Lihme", "Haderup",
                "Feldborg","Frederiks","Karup" ,"Silkeborg", "Rye","Braedstrup 2","Braedstrup","Ejstrupholm", 
                "Torring" , "Torring2","Vildbjerg","Ornhoj-Gronbjerg", "Tim", "Ringkjobing", "Egtved" ,"Hejnsvig", 
                "Skovlund", "Tistrup" ,"Sig" , "Oksbol" , "Gording", "Holsted" , "Christiansfeld", "Gram" ,"Vojens",
                "Toftlund","Logumkloster", "Grasten" ,"Broager", "St. Rise","Aeroskobing","Marstel","Sydlangeland",
                "Tommerup","Ringe", "Lolland","Oster", "Sydfalster","Refa-Gedser","Stege", "Sandved-Tornemark", "Fuglebjerg", 
                "Haslev" ,"Hvidebaek", "Svebolle","Smorum", "Skuldelev","Jaegerspris","Nykobings", "Hundested", "Vejby", 
                "Helsinge")

site_ids <- c("4", "79","45","51","14","115","49","110","81","37","40","3","32","47","114","54","53","112","44","24","30", 
"31", "100", "43", "99", "1", "16",  "5",  "86", "42", "25", "27", "11", "76", "12", "23", "15", "34", "13", "19",  "98",  "28", 
"6", "18", "36","50", "22", "10", "39", "8", "2",  "33", "82","116", "97", "93","17", "102", "83", "94", "96","103", "35",
"29", "104", "48", "9",  "41",  "46","21", "20") 

site_latlons <- c("57.488519, 10.486884", "57.392246, 10.120313","57.361012, 9.947647",  "57.284072, 10.096013", "57.317312, 10.502039",
                  "57.315924, 10.501388", "57.204279, 9.670758",  "57.146419, 9.762861",  "57.175564, 10.151121", "57.162937, 10.253277",
                  "57.155180, 10.394182", "57.078654, 10.252327", "56.962971, 10.214036", "56.888538, 8.538094",  "56.741672, 8.963102",
                  "56.736111, 10.112385", "56.591002, 10.136062", "56.591399, 8.791363", "56.392732, 8.990445",  "56.336781, 8.940749",
                  "56.337432, 9.245461",  "56.311003, 9.163035",  "56.208234, 9.546451",  "56.069370, 9.692441", "55.977873, 9.623761",
                  "55.978695, 9.629253",  "55.980475, 9.293377",  "55.855816, 9.492632",  "55.854990, 9.496065",  "56.206777, 8.773864",
                  "56.204275, 8.573454",  "56.190090, 8.312271",  "56.094087, 8.284403",  "55.620152, 9.323732",  "55.695530, 9.007244",
                  "55.742032, 8.702813", "55.717618, 8.613441",  "55.667168, 8.565599",  "55.619889, 8.288348",  "55.476738, 8.803819",
                  "55.506467, 8.903544",  "55.368032, 9.488627", "55.276828, 9.049574",  "55.241101, 9.285634",  "55.199975, 9.080764",
                  "55.051287, 8.960624",  "54.932625, 9.604330",  "54.884324, 9.676783", "54.852033, 10.408747", "54.883866, 10.415010",
                  "54.852505, 10.506659", "54.798816, 10.710182", "55.329109, 10.199913", "55.252603, 10.496850", "54.825794, 11.171034",
                  "54.758955, 11.818087", "54.718146, 11.923282", "54.577268, 11.935755", "55.000648, 12.291542", "55.254072, 11.522817",
                  "55.298998, 11.535034", "55.333998, 11.970144", "55.613226, 11.209546", "55.654732, 11.292038", "55.729510, 12.307020",
                  "55.775250, 12.016991", "55.829130, 12.000715", "55.917183, 11.651009", "55.963456, 11.880701", "56.069852, 12.152346",
                  "56.009482, 12.205189")

sites_sf <- data.frame(site = site_names, id = site_ids, latlon = site_latlons) %>% 
  dplyr::mutate(colsplit(latlon, ",", c("lon", "lat"))) %>% 
  st_as_sf(coords = c("lat", "lon"), crs = 4326) %>% 
  st_transform(28992) %>% 
  dplyr::select(-latlon)

## IMPORT SHAPEFILES TO CREATE DETAILED DANISH COAST OUTLINE
## define url of dk boundary file
url <- 'https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/denmark-shapefile/at_download/file'

##download file
download.file(url, destfile ='dk.zip', mode='wb')

## unzip downloaded file
unzip(zipfile = 'dk.zip', exdir = 'shp')

## read in 1km file, join all polygons together and reduce by 14km (as EEA file is oversize) and transform to latlong
dk_shp <- st_read('shp/dk_1km.shp') %>% 
  st_union %>% 
  st_transform(28992) %>%
  st_buffer(-14000) %>% ## negative buffer as shape file overlaps sweden and germany
  st_transform(4326)

  
## define url EU coastline
europe_url <- 'https://www.eea.europa.eu/data-and-maps/data/eea-coastline-for-analysis-1/gis-data/europe-coastline-shapefile/at_download/file'
##download
download.file(europe_url, destfile ='eu.zip', mode='wb')

## unzip
unzip(zipfile = 'eu.zip', exdir = 'shp')

## read in eu coastline shp and convert to latlong
eu_shp <- st_read('shp/Europe_coastline_poly.shp') %>% 
  st_transform(latlong)

unlink('eu.zip')
unlink('dk.zip')

## trim eu coastline to Danish boundaries, define it as a multipolygon and join all polygons,  transform to rdnew
dk_rd <- eu_shp %>% 
  st_intersection(dk_shp) %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_union() %>% 
  st_transform(rdnew)

## combine sites
d <- st_union(sites_sf)
## create voroni
v <- st_voronoi(d)

## trim the plot to the country shapefile
p1 <- st_as_sf(st_intersection(st_cast(v), dk_rd), crs = 28992)

## tell R that the geometry is polygons
p1 <- st_cast(p1, "MULTIPOLYGON")

p2 <- sites_sf[unlist(st_intersects(p1, sites_sf)),]
p1$site <- p2$site
p1$id <- p2$id
st_write(p1, 'shp/voroni.shp')

## mapview makes it easy to see the plot on a zoomable map
library(mapview)
##plot the voroni and original sites
mapview(p1)

## group by municipality
## load in map
dk_kom <- st_read("http://data.information.dk/open/dagi/geojson/dagi-ref-kommuner.geojson")
dk <- select(dk_kom, KOMKODE) %>% st_transform(4326)

sites_sf <- data.frame(site = site_names, id = site_ids, latlon = site_latlons) %>% 
  dplyr::mutate(colsplit(latlon, ",", c("lon", "lat"))) %>% 
  st_as_sf(coords = c("lat", "lon"), crs = 4326)

sites_sf$muni <- dk$KOMKODE[st_nearest_feature(sites_sf, dk)]

saveRDS(sites_sf, "sites_muni.RDS")
