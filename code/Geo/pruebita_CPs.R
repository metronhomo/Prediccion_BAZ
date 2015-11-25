library(rgdal)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(geosphere)

colonias <- readOGR("./Colonias Diego Valle/", layer = "Colonias")


CPs <- read.csv("./CPdescarga.txt", sep = "|")

unique(colonias@data$POSTALCODE)  %>% length
unique(CPs$d_codigo) %>% length()

sum(unique(colonias@data$POSTALCODE) %in% unique(CPs$d_codigo))

colonias_df <- fortify(colonias, region = "OBJECTID")

cols <- select(colonias@data, OBJECTID, POSTALCODE, ST_NAME, MUN_NAME, SETT_NAME, SETT_TYPE) %>%
  mutate(OBJECTID = as.character(OBJECTID))

rm(colonias)

colonias_df2 <- colonias_df %>%
  mutate(OBJECTID = id) %>%
  left_join(cols)

colonias_df2 %>% 
  # filter(MUN_NAME == "TLALPAN" | MUN_NAME == "COYOACÁN") %>%
  filter(ST_NAME == "DISTRITO FEDERAL") %>% 
  ggplot(aes(long, lat, group=group, fill = MUN_NAME)) + 
  geom_polygon(colour='black') +
  coord_map(projection="mercator") 

colonias_df2 %>% 
  ggplot(aes(long, lat, group=group, fill = ST_NAME)) + 
  geom_polygon(colour = 'black') +
  coord_map(projection="mercator") 

# Centroides <- colonias_df2 %>%
#   filter(ST_NAME == "DISTRITO FEDERAL") %>% 
#   group_by(POSTALCODE) %>%
#   summarise(Centroide = centroid(select(., long, lat)))

CPs_colonias <- unique(filter(colonias_df2, ST_NAME == "DISTRITO FEDERAL")$POSTALCODE)
CPs_colonias <- unique(colonias_df2$POSTALCODE)
lista <- lapply(CPs_colonias, function(x) filter(colonias_df2, POSTALCODE == x)[,c("long", "lat")])
names(lista) <- CPs_colonias

Centroides <- t(sapply(lista, centroid)) %>% as.data.frame()

colonias_df2 %>% 
  # filter(MUN_NAME == "TLALPAN" | MUN_NAME == "COYOACÁN") %>%
  filter(ST_NAME == "DISTRITO FEDERAL") %>% 
  ggplot() + 
  geom_polygon(aes(long, lat, group=group, fill = MUN_NAME), colour='grey26') +
  geom_point(data = Centroides, aes(x = V1, y = V2)) +
  coord_map(projection="mercator") 



