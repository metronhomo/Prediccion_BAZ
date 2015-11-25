library(rgdal)
library(dplyr)
library(ggplot2)
library(ggthemes)

# Checar si se quiere para todo Mex o solo DF
colonias_map <- readRDS(file = "./Out/colonias_map.RDS")
Centroides_CP <- readRDS(file = "./Out/Centroides_CP.RDS")
agebs_map <- readRDS(file = "./Out/agebs_map.RDS")
Centroides_AGEBS <- readRDS(file = "./Out/Centroides_AGEBS.RDS")

estados_map <- readOGR("../Entidades_2010_5A/", layer = "Entidades_2010_5A") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
estados_map <- fortify(estados_map, region = "NOM_ENT")

#Polígonos de AGEBs
ggplot() + 
  geom_polygon(data = filter(estados_map, id == "Distrito Federal"), aes(long, lat, group=group), colour = 'black', fill = 'white') +
  geom_polygon(data = agebs_map, aes(long, lat, group=group), colour = 'red', fill = "red", alpha = 0.2) +
  coord_fixed() +
  coord_map(projection="mercator")

#Polígonos de colonias y CPs
ggplot() + 
  geom_polygon(data = filter(estados_map, id == "Distrito Federal"), aes(long, lat, group=group), colour = 'black', fill = 'white') +
  geom_polygon(data = colonias_map, aes(long, lat, group=group), colour = 'blue', fill = "blue", alpha = 0.2) +
  coord_fixed() +
  coord_map(projection="mercator")

# Políonos de AGEBs y colonias juntos
ggplot() + 
  geom_polygon(data = filter(estados_map, id == "Distrito Federal"), aes(long, lat, group=group), colour = 'black', fill = 'white') +
  geom_polygon(data = colonias_map, aes(long, lat, group=group), colour = 'blue', fill = "blue", alpha = 0.2) +
  geom_polygon(data = agebs_map, aes(long, lat, group=group), colour = 'red', fill = "red", alpha = 0.2) +
  coord_fixed() +
  coord_map(projection="mercator")

# Políonos de AGEBs y colonias juntos con respectivos centroides
ggplot() + 
  geom_polygon(data = filter(estados_map, id == "Distrito Federal"), aes(long, lat, group=group), colour = 'black', fill = 'white') +
  geom_polygon(data = colonias_map, aes(long, lat, group=group), colour = 'blue', fill = "blue", alpha = 0.2) +
  geom_polygon(data = agebs_map, aes(long, lat, group=group), colour = 'red', fill = "red", alpha = 0.2) +
  geom_point(data = Centroides_AGEBS, aes(x = long, y = lat), colour = 'red') +
  geom_point(data = Centroides_CP, aes(x = long, y = lat), colour = 'blue') +
  coord_map(projection="mercator")

# Centroides de AGEBs y Colonias
ggplot() + 
  geom_polygon(data = filter(estados_map, id == "Distrito Federal"), aes(long, lat, group=group), colour = 'black', fill = 'white') +
  geom_point(data = Centroides_AGEBS, aes(x = long, y = lat), colour = 'red') +
  geom_point(data = Centroides_CP, aes(x = long, y = lat), colour = 'blue') +
  coord_fixed() +
  coord_map(projection="mercator")




