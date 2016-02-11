# Lee los archivos KML que tienen los shapefiles de los CPs de México y los convierte a una lista y a un
# dataframe.
# Los archivos KML fueron descargados en datos.gob.mx

library(rgdal)
library(ggplot2)

archivos <- c("CP_Ags.kml",
              "CP_BC.kml",
              "CP_BCS.kml",
              "CP_Camp.kml",
              "CP_Chih.kml",
              "CP_Chis.kml",
              "CP_Coah.kml",
              "CP_Col.kml",
              "CP_DF.kml",
              "CP_Dgo.kml",
              "CP_Gro.kml",
              "CP_Gto.kml",
              "CP_Hgo.kml",
              "CP_Jal.kml",
              "CP_Mex.kml",
              "CP_Mich.kml",
              "CP_Mor.kml",
              "CP_Nay.kml",
              "CP_NL.kml",
              "CP_Oax.kml",
              "CP_Pue.kml",
              "CP_Qro.kml",
              "CP_Sin.kml",
              "CP_SLP.kml",
              "CP_Son.kml",
              "CP_Tab.kml",
              "CP_Tamps.kml",
              "CP_Tlax.kml",
              "CP_Ver.kml",
              "CP_Yuc.kml",
              "CP_Zac.kml")

#archivos <- archivos[1:9]

n_char <- sapply(archivos, nchar)
estados <- substr(archivos, 4, n_char-4)

shapes <- lapply(archivos, function(x){
  ruta <- paste0("../../data/Geo/CPs_datos.gob.mx/", x)
  lyr <- ogrListLayers(ruta)[1]
  readOGR(ruta, layer = lyr)
})
names(shapes) <- estados

mapas <- lapply(shapes, fortify)
mapas2 <- lapply(1:length(mapas), function(x){
  df <- data.frame(id = as.character(1:nrow(shapes[[x]]@data)), CP = shapes[[x]]@data$d_cp, Estado = names(shapes)[x])
  mapas[[x]] <- left_join(mapas[[x]], df)
})
names(mapas2) <- names(mapas)

mapas_dataframe <- rbind_all(mapas2)

#sapply(shapes, function(x) length(unique(x@data$d_cp)))
sapply(shapes, function(x) length((x@data$d_cp)))
sapply(mapas, function(x) length(unique(x$id)))
#sapply(mapas, function(x) length(unique(x$group)))

sapply(shapes, function(x) length(unique(x@data$d_cp)))
sapply(mapas2, function(x) length(unique(x$CP)))

saveRDS(mapas2, file = "../../output/Geo/lista_mapas_CP_datos_gob.RDS")
saveRDS(mapas_dataframe, file = "../../output/Geo/dataframe_mapas_CP_datos_gob.RDS")
