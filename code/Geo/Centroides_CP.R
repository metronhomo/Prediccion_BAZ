library(dplyr)
library(rgdal)
library(ggplot2)
library(geosphere)

# Lee el dataframe preprocesado que contiene los shapefiles de los CPs de México
# Información original: KML descargado de datos.gob.mx
CP_map <- readRDS("../../output/Geo/dataframe_mapas_CP_datos_gob.RDS")

# Lee el OGR con shapefiles de colonias obtenido de http://www.numeroslocos.com/2013/07/10/shapefile/
# Originalmente son de NAVTEQ
colonias_map <- readOGR("../../data/Geo/Colonias_NAVTEQ/", layer = "Colonias")

# Guarda en un dataframe la información sobre las colonias
colonias <- select(colonias_map@data, OBJECTID, POSTALCODE, ST_NAME, MUN_NAME, SETT_NAME, SETT_TYPE) %>%
  mutate(OBJECTID = as.character(OBJECTID))

# Guarda en CPs_faltantes_datosgob los CPs que están en la base de numeroslocos.com.mx y no están
# en la base de datos.gob.mx
CPs_faltantes_datosgob <- setdiff(unique(colonias_map@data$POSTALCODE), unique(CP_map$CP))

# Filtrar el shp para quedarnos solo con las colonias de CPs_faltantes_datosgob
idx <- as.character(colonias_map$POSTALCODE) %in% CPs_faltantes_datosgob
temp <- colonias_map[idx,]

# Convierte a dataframe y le pega la información sobre las colonias
colonias_map <- fortify(temp, region = "OBJECTID") %>%
  mutate(OBJECTID = id) %>%
  left_join(colonias)

# Hace una lista en la que cada elemento es un CP que contiene a las coordenadas del polígono que lo define
CPs_col <- colonias_map %>%
  select(long, lat, POSTALCODE) %>%
  mutate(CP = as.character(POSTALCODE))
lista_navteq <- with(CPs_col, split(c(long, lat), CP, drop = T)) 
lista_navteq <- lapply(lista_navteq, function(x) as.data.frame(matrix(x, ncol = 2)))

# Calcula los centroides
Centroides_CP_navteq <- t(sapply(lista_navteq, centroid)) %>% as.data.frame()
Centroides_CP_navteq$CP <- names(lista_navteq)
names(Centroides_CP_navteq) <- c("long", "lat", "CP")
Centroides_CP_navteq2 <- Centroides_CP_navteq %>%
  filter(CP != "00000", long != Inf, lat != Inf) 
Centroides_CP_navteq2$fuente <- "NAVTEQ"

# Calcular los centroides para todos los datos de datos.gob.mx
CPs_col <- CP_map %>%
  select(long, lat, CP) %>%
  mutate(CP = as.character(CP))
lista <- with(CPs_col, split(c(long, lat), CP, drop = T)) 
lista <- lapply(lista, function(x) as.data.frame(matrix(x, ncol = 2)))

Centroides_CP <- t(sapply(lista, centroid)) %>% as.data.frame()
Centroides_CP$CP <- names(lista)
names(Centroides_CP) <- c("long", "lat", "CP")
Centroides_CP2 <- Centroides_CP %>%
  filter(CP != "00000", long != Inf, lat != Inf)
Centroides_CP2$fuente <- "datos.gob.mx"

# Cols <- colonias_map %>%
#   select(long, lat, id) %>%
#   mutate(id = as.character(id))
# lista2 <- with(Cols, split(c(long, lat), id, drop = T)) 
# lista2 <- lapply(lista2, function(x) as.data.frame(matrix(x, ncol = 2)))
# 
# Centroides_cols <- t(sapply(lista2, centroid)) %>% as.data.frame()
# Centroides_cols$id <- names(lista2)
# names(Centroides_cols) <- c("long", "lat", "id")

# Agrupar en un solo dataframe
Centroides_CP <- rbind(Centroides_CP2, Centroides_CP_navteq2)


# Ahora agregamos la información de CPs que no están ni en NAVTEQ ni en datos.gob.mx, que se obtuvo
# usando la API de Google
CPs_google <- readRDS(
  '../../output/postal_code_google_api/zips_geocode_dataframe_status_ok_not_repeated2.RDS')

#Dejar solo los CPs de google que no están en datos.gob.mx ni NAVTEQ y unir
Centroides_CP2 <- CPs_google %>% filter(!(CP %in% Centroides_CP$CP)) %>% 
  mutate(long = as.numeric(as.character(lon)),
         lat = as.numeric(as.character(lat))) %>% 
  mutate(fuente = 'Google API') %>% 
  select(long, lat, CP, fuente) %>% 
  rbind(Centroides_CP)

saveRDS(Centroides_CP2, file = "../../output/Geo/Centroides_CP.RDS")
# saveRDS(lista, file = "../../output/Geo/lista_CPs_coord.RDS")


