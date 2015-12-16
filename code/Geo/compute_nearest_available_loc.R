# compute_nearest_available_loc.R

library(spatstat)
library(fields)

Centroides_CP <- readRDS(file = "../../output/Geo/Centroides_CP.RDS")
Centroides_loc_urb <- readRDS(file = "../../output/Geo/Centroides_localidades_urbanas.RDS")
Puntos_loc_rur <- readRDS(file = "../../output/Geo/localidades_rurales_puntos_dataframe.RDS")

Centroides_loc_urb$Tipo_Localidad <- "U"
Puntos_loc_rur$Tipo_Localidad <- "R"

puntos_localidades <- rbind(Centroides_loc_urb, Puntos_loc_rur[,c("id_loc", "long", "lat", "Tipo_Localidad")])

ind_marg <- read.csv("../../data/Geo/Índice de marginación CONAPO/basecompleta.csv")
ind_marg <- ind_marg %>% mutate(id = paste(sprintf("%02d", ENT), 
                                           sprintf("%03d", MUN),
                                           sprintf("%04d", LOC),
                                           sep = ""))

puntos_localidades_con_info <- puntos_localidades %>%
  filter(id_loc %in% ind_marg$id)

pp_loc <- as.ppp(puntos_localidades_con_info[,c("long", "lat")], c(min(puntos_localidades_con_info$long), 
                                                                  max(puntos_localidades_con_info$long),
                                                                  min(puntos_localidades_con_info$lat),
                                                                  max(puntos_localidades_con_info$lat)))
pp_CP <- as.ppp(Centroides_CP[,c("long", "lat")], c(min(Centroides_CP$long), 
                                                    max(Centroides_CP$long),
                                                    min(Centroides_CP$lat),
                                                    max(Centroides_CP$lat)))

nncross_CP_loc <- nncross(pp_CP, pp_loc)
# nncross_CP_AGEB <- nncross(pp_CP, pp_AGEB, k = 1:3) # Regresa los 3 vecinos más cercanos

distancias <- data.frame(CP = Centroides_CP$CP, 
                         CP_long = Centroides_CP$long, 
                         CP_lat = Centroides_CP$lat,
                         nearest_loc = puntos_localidades_con_info$id_loc[nncross_CP_loc$which],
                         loc_long = puntos_localidades_con_info$long[nncross_CP_loc$which],
                         loc_lat = puntos_localidades_con_info$lat[nncross_CP_loc$which],
                         distance = nncross_CP_loc$dist)

distancias2 <- distancias %>%
  mutate(id = nearest_loc,
         distance_km = rdist.earth.vec(cbind(CP_long, CP_lat), cbind(loc_long, loc_lat), R = 6371, 
                                       miles = F)) %>%
  left_join(ind_marg)


write.csv(distancias2, file = "../../output/Geo/mapeo_CP_localidad.csv", row.names = F)

