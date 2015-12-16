# Centroides_AGEB.R

library(rgdal)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(rgeos)

#cves_edo_mun <- read.csv("../../data/Geo/Claves Estados.csv")

# Lee los shapefiles de los AGEBs y los convierte a dataframe
agebs_map <- readOGR("../../data/Geo/agebs_urbanos_2013/", layer = "agebs_urbanos_2013") %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>% 
  fortify(region = 'CVEGEO')


# Hace una lista en la que cada elemento es un CP que contiene a las 
# coordenadas del polígono que lo define en un objeto de tipo SpatialPoints
agebs_long_lat <- agebs_map %>%
  select(long, lat, id)
lista_agebs <- with(agebs_long_lat, split(c(long, lat), id, drop = T)) 
#lista_navteq <- lapply(lista_navteq, function(x) as.data.frame(matrix(x, ncol = 2)))
lista_agebs <- lapply(lista_agebs, function(x) {
  SpatialPoints(
    matrix(x, ncol = 2) ,
    CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
})

# Calcula centroides de las AGEBs
Centroides_AGEBS <- t(sapply(lista_agebs, function(x){
  a <- gCentroid(x)
  a@coords })) %>% 
  as.data.frame()
Centroides_AGEBS$id_AGEB <- names(lista_agebs)
names(Centroides_AGEBS) <- c("long", "lat", "id_AGEB")

saveRDS(agebs_map, file = "../../output/Geo/agebs_map.RDS")
saveRDS(Centroides_AGEBS, file = "../../output/Geo/Centroides_AGEBS.RDS")
saveRDS(lista_agebs, file = "../../output/Geo/lista_AGEBs_coord.RDS")




