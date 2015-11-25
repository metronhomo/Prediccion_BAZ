library(dplyr)
library(ggplot2)
library(geosphere)

CP_map <- readRDS("../../output/Geo/dataframe_mapas_CP_datos_gob.RDS")

CPs_col <- CP_map %>%
  select(long, lat, CP) %>%
  mutate(CP = as.character(CP))
lista <- with(CPs_col, split(c(long, lat), CP, drop = T)) 
lista <- lapply(lista, function(x) as.data.frame(matrix(x, ncol = 2)))

Centroides_CP <- t(sapply(lista, centroid)) %>% as.data.frame()
Centroides_CP$CP <- names(lista)
names(Centroides_CP) <- c("long", "lat", "CP")
Centroides_CP2 <- Centroides_CP %>%
  filter(CP != "00000", long != Inf, lat != Inf) %>%
  left_join(unique(CP_map[,c("CP", "Estado")]))

# Cols <- colonias_map %>%
#   select(long, lat, id) %>%
#   mutate(id = as.character(id))
# lista2 <- with(Cols, split(c(long, lat), id, drop = T)) 
# lista2 <- lapply(lista2, function(x) as.data.frame(matrix(x, ncol = 2)))
# 
# Centroides_cols <- t(sapply(lista2, centroid)) %>% as.data.frame()
# Centroides_cols$id <- names(lista2)
# names(Centroides_cols) <- c("long", "lat", "id")

saveRDS(Centroides_CP2, file = "../../output/Geo/Centroides_CP.RDS")
saveRDS(lista, file = "../../output/Geo/lista_CPs_coord.RDS")


