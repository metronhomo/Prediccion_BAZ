library(spatstat)

Centroides_CP <- readRDS(file = "./Out/Centroides_CP.RDS")
Centroides_AGEBS <- readRDS(file = "./Out/Centroides_AGEBS.RDS")

pp_AGEB <- as.ppp(Centroides_AGEBS[,c("long", "lat")], c(min(Centroides_AGEBS$long), 
                                                         max(Centroides_AGEBS$long),
                                                         min(Centroides_AGEBS$lat),
                                                         max(Centroides_AGEBS$lat)))
pp_CP <- as.ppp(Centroides_CP[,c("long", "lat")], c(min(Centroides_CP$long), 
                                                    max(Centroides_CP$long),
                                                    min(Centroides_CP$lat),
                                                    max(Centroides_CP$lat)))

nncross_CP_AGEB <- nncross(pp_CP, pp_AGEB)
# nncross_CP_AGEB <- nncross(pp_CP, pp_AGEB, k = 1:3) # Regresa los 3 vecinos más cercanos

distancias <- data.frame(CP = Centroides_CP$CP, 
                         CP_long = Centroides_CP$long, 
                         CP_lat = Centroides_CP$lat,
                         nearest_AGEB = Centroides_AGEBS$id_AGEB[nncross_CP_AGEB$which],
                         AGEB_long = Centroides_AGEBS$long[nncross_CP_AGEB$which],
                         AGEB_lat = Centroides_AGEBS$lat[nncross_CP_AGEB$which],
                         distance = nncross_CP_AGEB$dist)

write.csv(distancias, file = "./Out/mapeo_CP_AGEB.csv", row.names = F)

