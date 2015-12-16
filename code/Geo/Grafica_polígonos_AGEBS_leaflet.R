# Grafica_polígonos_AGEBS_leaflet.R
# Grafica polígonos de la clasificacion socioeconomica de los AGEBs segun la INEGI

library(ggplot2)
library(dplyr)
library(leaflet)
library(foreign)
library(rgdal)

#Clasificación de los AGEBs
composicion <- read.dbf("../../data/Geo/Regiones socioeconómicas de México INEGI/Composición de los estratos/COMAGB.dbf")
# Data frame con mapa de los AGEBs
agebs_map <- readRDS(file = "../../output/Geo/agebs_map.RDS")

# Filtrar EdoMex y DF
composicion_DF <- composicion %>%
  filter(ENT == "09" | ENT == "15") %>%
  mutate(id = paste0(ENT, MUN, LOC, AGEB))

# Filtra poligonos de AGEBs del DF y le hace left join a la informacion de la clasificacion de los AGEBs
agebs_map_DF <- agebs_map %>%
  mutate(sub_id = substr(id, 1, 2)) %>%
  filter(sub_id == "09" | sub_id == "15") %>%
  select(-sub_id) %>%
  left_join(composicion_DF[,c("id", "E_AGEB", "POB")]) %>%
  filter(!is.na(E_AGEB))

# Encontrado en:
# http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/
# Convertir agebs_map_DF a SpatialPolygonsDataFrame
polyFunc<-function(groupname, dat){
  poly<-filter(dat, id==groupname) %>% 
    select(long, lat)
  return(Polygons(list(Polygon(poly)), groupname))
}

agebs <- distinct(agebs_map_DF, id, E_AGEB)
agebname <- unique(agebs_map_DF$id)
polygons<-lapply(agebname, function(x) polyFunc(x, dat=agebs_map_DF)) 
sp.polygon<-SpatialPolygons(polygons)
df.polygon<-SpatialPolygonsDataFrame(sp.polygon, 
                                     data=data.frame(row.names=agebname, agebs))
df.polygon <- df.polygon[order(df.polygon$E_AGEB),]

popup <- paste0("GEOID: ", df.polygon$id, "<br>", 
                "Clasificacion de AGEB: ", df.polygon$E_AGEB)

# Paleta de colores
pal <- colorFactor(
  palette = topo.colors(15),
  domain = unique(df.polygon$E_AGEB)
)

# Mapa en Leaflet
leaflet(df.polygon) %>% 
  addTiles() %>% 
  addPolygons( 
    fillColor = ~pal(E_AGEB), 
    #color = ~(E_AGEB), # you need to use hex colors
    fillOpacity = 0.7, 
    weight = 0.3, 
    smoothFactor = 0.2,
    popup = popup) %>%
  addLegend(pal = pal, 
            values = df.polygon$E_AGEB, 
            position = "bottomright", 
            title = "Clasificación AGEB") 

  
  