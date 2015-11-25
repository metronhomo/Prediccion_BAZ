# compute_nearest_available_AGEB.R

library(spatstat)
library(fields)

Centroides_CP <- readRDS(file = "./Out/Centroides_CP.RDS")
Centroides_AGEBS <- readRDS(file = "./Out/Centroides_AGEBS.RDS")
composicion <- read.dbf("../Regiones socioeconómicas de México INEGI/Composición de los estratos/COMAGB.dbf") %>%
  mutate(id = paste0(ENT, MUN, LOC, AGEB))

Centroides_AGEBS_con_info <- Centroides_AGEBS %>%
  filter(id_AGEB %in% composicion$id)

pp_AGEB <- as.ppp(Centroides_AGEBS_con_info[,c("long", "lat")], c(min(Centroides_AGEBS_con_info$long), 
                                                         max(Centroides_AGEBS_con_info$long),
                                                         min(Centroides_AGEBS_con_info$lat),
                                                         max(Centroides_AGEBS_con_info$lat)))
pp_CP <- as.ppp(Centroides_CP[,c("long", "lat")], c(min(Centroides_CP$long), 
                                                    max(Centroides_CP$long),
                                                    min(Centroides_CP$lat),
                                                    max(Centroides_CP$lat)))

nncross_CP_AGEB <- nncross(pp_CP, pp_AGEB)
# nncross_CP_AGEB <- nncross(pp_CP, pp_AGEB, k = 1:3) # Regresa los 3 vecinos más cercanos

distancias <- data.frame(CP = Centroides_CP$CP, 
                         CP_long = Centroides_CP$long, 
                         CP_lat = Centroides_CP$lat,
                         nearest_AGEB = Centroides_AGEBS_con_info$id_AGEB[nncross_CP_AGEB$which],
                         AGEB_long = Centroides_AGEBS_con_info$long[nncross_CP_AGEB$which],
                         AGEB_lat = Centroides_AGEBS_con_info$lat[nncross_CP_AGEB$which],
                         distance = nncross_CP_AGEB$dist)

distancias2 <- distancias %>%
  mutate(id = nearest_AGEB,
         distance_km = rdist.earth.vec(cbind(CP_long, CP_lat), cbind(AGEB_long, AGEB_lat), R = 6371, miles = F)) %>%
  left_join(composicion[,c("id", "E_AGEB", "TIPO")])


write.csv(distancias2, file = "./Out/mapeo_CP_AGEB_con_info_socioeconomica.csv", row.names = F)

