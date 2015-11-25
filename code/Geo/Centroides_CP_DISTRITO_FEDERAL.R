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

colonias_map_completo <- colonias_map
colonias_map <- colonias_map %>% filter(ST_NAME == "DISTRITO FEDERAL") 

CPs_colonias <- unique(colonias_map$POSTALCODE)
lista <- lapply(CPs_colonias, function(x) filter(colonias_map, POSTALCODE == x)[,c("long", "lat")])
names(lista) <- CPs_colonias

Centroides_CP <- t(sapply(lista, centroid)) %>% as.data.frame()
Centroides_CP$CP <- names(lista)
names(Centroides_CP) <- c("long", "lat", "CP")

saveRDS(colonias_map, file = "./Out/colonias_map_DF.RDS")
saveRDS(Centroides_CP, file = "./Out/Centroides_CP_DF.RDS")



