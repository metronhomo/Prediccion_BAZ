library(rgdal)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(geosphere)

colonias_map <- readOGR("../Colonias Diego Valle/", layer = "Colonias")

colonias <- select(colonias_map@data, OBJECTID, POSTALCODE, ST_NAME, MUN_NAME, SETT_NAME, SETT_TYPE) %>%
  mutate(OBJECTID = as.character(OBJECTID))

colonias_map <- fortify(colonias_map, region = "OBJECTID")

colonias_map <- colonias_map %>%
  mutate(OBJECTID = id) %>%
  left_join(colonias)

## Solo DF
# colonias_map_completo <- colonias_map
# colonias_map <- colonias_map %>% filter(ST_NAME == "DISTRITO FEDERAL") 

# CPs_colonias <- unique(as.character(colonias_map$POSTALCODE))
# lista <- lapply(CPs_colonias, function(x) filter(colonias_map, POSTALCODE == x)[,c("long", "lat")])
# names(lista) <- CPs_colonias

CPs_col <- colonias_map %>%
  select(long, lat, POSTALCODE) %>%
  mutate(POSTALCODE = as.character(POSTALCODE))
lista <- with(CPs_col, split(c(long, lat), POSTALCODE, drop = T)) 
lista <- lapply(lista, function(x) as.data.frame(matrix(x, ncol = 2)))

Centroides_CP <- t(sapply(lista, centroid)) %>% as.data.frame()
Centroides_CP$CP <- names(lista)
names(Centroides_CP) <- c("long", "lat", "CP")

# Cols <- colonias_map %>%
#   select(long, lat, id) %>%
#   mutate(id = as.character(id))
# lista2 <- with(Cols, split(c(long, lat), id, drop = T)) 
# lista2 <- lapply(lista2, function(x) as.data.frame(matrix(x, ncol = 2)))
# 
# Centroides_cols <- t(sapply(lista2, centroid)) %>% as.data.frame()
# Centroides_cols$id <- names(lista2)
# names(Centroides_cols) <- c("long", "lat", "id")

saveRDS(colonias_map, file = "./Out/colonias_map.RDS")
saveRDS(Centroides_CP, file = "./Out/Centroides_CP.RDS")
saveRDS(lista, file = "./Out/lista_CPs_coord.RDS")


