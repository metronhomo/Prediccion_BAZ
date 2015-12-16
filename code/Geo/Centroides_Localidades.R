# Centroides_Localidades.R

library(rgdal)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(geosphere)

# cves_edo_mun <- read.csv("../Claves Estados.csv")

# Localidades urbanas
loc_urb_map <- readOGR("../../data/Geo/Localidades urbanas_2013/", 
                       layer = "Localidades urbanas_2013") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

loc_urb_map@data <- loc_urb_map@data %>% 
  mutate(id = paste(CVE_ENT, CVE_MUN, CVE_LOC, sep=""))

# Localidades rurales
loc_rur_map <- readOGR("../../data/Geo/Localidades rurales_2013/", 
                       layer = "Localidades rurales_2013") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

loc_rur_map@data <- loc_rur_map@data %>% 
  mutate(id = paste(CVE_ENT, CVE_MUN, CVE_LOC, sep=""))

# Pasar de shapefile a dataframe los datos de comunidades urbanas
loc_urb_gg <- fortify(loc_urb_map, region = "id")

# Para las comunidades rurales no se tienen los shapes, solo los puntos, así que se pasan a dataframe
loc_rur_df <- as.data.frame(loc_rur_map)
names(loc_rur_df)[7:9] <- c("id_loc", "long", "lat")

# Id's de localidades urbanas
loc_urb_id <- unique(loc_urb_gg$id)
lista <- lapply(loc_urb_id, 
                function(x) {
                  filter(loc_urb_gg, id == x)[,c("long", "lat")]
                  })
names(lista) <- loc_urb_id

Centroides_localidades_urbanas <- t(sapply(lista, centroid)) %>% as.data.frame()
Centroides_localidades_urbanas$id_loc <- names(lista)
names(Centroides_localidades_urbanas) <- c("long", "lat", "id_loc")

saveRDS(loc_urb_gg, file = "../../output/Geo/localidades_urbanas_map_dataframe.RDS")
saveRDS(loc_rur_df, file = "../../output/Geo/localidades_rurales_puntos_dataframe.RDS")
saveRDS(Centroides_localidades_urbanas, file = "../../output/Geo/Centroides_localidades_urbanas.RDS")
saveRDS(lista, file = "../../output/Geo/lista_localidades_urbanas_coord.RDS")

#ggplot() + geom_point(data = loc_rur_df, aes(long, lat), size = 0.3, alpha = 0.5, color = 'grey') + geom_point(data=Centroides_localidades_urbanas, aes(x = long, y = lat), color = 'blue', size = 0.8) + theme_map()



