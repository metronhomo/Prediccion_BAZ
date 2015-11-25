library(foreign)
library(dplyr)
library(ggplot2)
library(rgdal)

# Checar si se quiere para todo Mex o solo DF
composicion <- read.dbf("../Regiones socioeconómicas de México INEGI/Composición de los estratos/COMAGB.dbf")
agebs_map <- readRDS(file = "./Out/agebs_map.RDS")
Centroides_AGEBS <- readRDS(file = "./Out/Centroides_AGEBS.RDS")
estados_map <- readOGR("../Entidades_2010_5A/", layer = "Entidades_2010_5A") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
estados_map <- fortify(estados_map, region = "NOM_ENT")
colonias_map <- readRDS(file = "./Out/colonias_map.RDS")
Centroides_CP <- readRDS(file = "./Out/Centroides_CP.RDS")

composicion <- composicion %>%
  filter(ENT == "09") %>%
  mutate(id = paste0(ENT, MUN, LOC, AGEB))

agebs_map <- agebs_map %>%
  left_join(composicion[,c("id", "E_AGEB", "POB")])

# Mapa con AGEBS con el color dependiendo del tipo de región socioeconómica según INEGI
ggplot() + 
  geom_polygon(data = filter(estados_map, id == "Distrito Federal"), aes(long, lat, group=group), colour = 'black', fill = 'white') +
  geom_polygon(data = agebs_map, aes(long, lat, group=group, fill = factor(E_AGEB)), colour = 'grey30') +
  scale_fill_discrete(na.value = "grey90") +
  coord_fixed() +
  coord_map(projection="mercator")

# Mapa con AGEBS pintados de acuerdo a tipo  de región socioeconómica según INEGI con los polígonos de los CP encima
ggplot() + 
  geom_polygon(data = filter(estados_map, id == "Distrito Federal"), aes(long, lat, group=group), colour = 'black', fill = 'white') +
  geom_polygon(data = agebs_map, aes(long, lat, group=group, fill = factor(E_AGEB)), colour = 'grey60') +
  geom_polygon(data = colonias_map, aes(long, lat, group=group), colour = 'grey20', fill = NA) +
  scale_fill_discrete(na.value = "grey90") +
  coord_map(projection = "mercator")


