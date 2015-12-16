# Centroides_AGEB.R

library(rgdal)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(geosphere)

cves_edo_mun <- read.csv("../Claves Estados.csv")

agebs_map <- readOGR("../agebs_urbanos_2013/", layer = "agebs_urbanos_2013")
agebs_map <- spTransform(agebs_map, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

agebs_map <- fortify(agebs_map, region = "CVEGEO")

# Solo DF
# agebs_map_completo <- fortify(agebs_map, region = "CVEGEO")
# agebs_map <- agebs_map_completo %>%
#   filter(substr(id, 1, 2) == "09")

AGEBS <- unique(agebs_map$id)
lista <- lapply(AGEBS, function(x) filter(agebs_map, id == x)[,c("long", "lat")])
names(lista) <- AGEBS

Centroides_AGEBS <- t(sapply(lista, centroid)) %>% as.data.frame()
Centroides_AGEBS$id_AGEB <- names(lista)
names(Centroides_AGEBS) <- c("long", "lat", "id_AGEB")

saveRDS(agebs_map, file = "./Out/agebs_map.RDS")
saveRDS(Centroides_AGEBS, file = "./Out/Centroides_AGEBS.RDS")
saveRDS(lista, file = "./Out/lista_AGEBs_coord.RDS")




