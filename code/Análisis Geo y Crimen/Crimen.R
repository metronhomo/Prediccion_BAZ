library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(corrplot)
library(rgdal)
library(ggthemes)

crimen <- read.csv("data/Crimen/fuero-comun-municipios.csv",header = T)
saveRDS(crimen,"data/Crimen/Crimen.RDS")
#Solo tomamos datos a parti de 2013 porque desde ese año tenemos transacciones
crimen <- crimen %>%
  filter(as.integer(substr(date,1,4))>=2013)

#Vemos cómo se distribuyen los crimenes en la base
modalidades <- crimen %>% 
  select(modalidad) %>% 
  table() %>% 
  prop.table() %>% 
  data.frame() %>% 
  select_("modalidad"=".","n"="Freq") %>%
  ungroup() %>% 
  arrange(desc(n))

#Creamos la base con la que calcuremos los índices
aux <- crimen %>% 
  group_by(state,state_code,mun_code,municipio,modalidad) %>% 
  summarise(n=sum(count,na.rm=T)) %>% 
  mutate(log=ifelse(n==0,0,log(n))) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>%
  select(-n) %>% 
  spread(modalidad,log)
names(aux) <- str_replace_all(names(aux)," ","_")

#Hacemos índices con PCA
x<-cor(aux[,-(1:2)],method = c("spearman"),use="pairwise.complete.obs")
corrplot(x, method = "circle")

comp<-prcomp(aux[,-(1:4)],center=T,scale=T)
summary(comp)
#Con las primeras cuatro componentes explicamos el 92% de la variabilidad en los datos
comp
saveRDS(comp,"data/Crimen/Componentes.RDS")

#Hacemos una función para calcular los índices en cada municipio
crea_indice <- function(i){
  indice <- apply(aux[,-(1:4)],1,function(r){
    sum(r*t(comp$rotation[,i]),na.rm=T)}) %>%
    as.data.frame()
  #Normalizamos el índice
  names(indice)<-"indice"
  indice <- indice %>%
    mutate(indice_normalizado = (indice-min(indice))/(max(indice)-min(indice))) %>% 
    select(indice_normalizado)
    return(indice)
}

#Creamos los índices de cada una de las dimensiones
#Le restamos uno porque el índice es todo negativo
indice_crimen <- 1 - crea_indice(1)
qplot(indice_crimen$indice_normalizado,stat="density")
quantile(indice_crimen$indice_normalizado,seq(0,1,.1))

indice_crimen_no_violento <- crea_indice(2)
qplot(indice_crimen_no_violento$indice_normalizado,stat="density")
quantile(indice_crimen_no_violento$indice_normalizado,seq(0,1,.1))

indice_crimen_violento <- crea_indice(3)
qplot(indice_crimen_violento$indice_normalizado,stat="density")
quantile(indice_crimen_violento$indice_normalizado,seq(0,1,.1))

indice_secuestro <- crea_indice(4)
qplot(indice_secuestro$indice_normalizado,stat="density")
quantile(indice_secuestro$indice_normalizado,seq(0,1,.1))

aux2 <- data.frame(aux,
                     dimension_crimen=indice_crimen$indice_normalizado,
                     dimension_crimen_no_violento=indice_crimen_no_violento$indice_normalizado,
                     dimension_crimen_violento=indice_crimen_violento$indice_normalizado,
                     dimension_secuestro=indice_secuestro$indice_normalizado)
saveRDS(aux2,"data/Crimen/base_con_indices.RDS")

aux2 <- readRDS("data/Crimen/base_con_indices.RDS") %>% 
  mutate(mun_code=as.character(mun_code),
    mun_code=ifelse(nchar(mun_code)==1,paste("00",mun_code,sep=""),
           ifelse(nchar(mun_code)==2,paste("0",mun_code,sep=""),mun_code)),
    state_code=as.character(state_code),
    state_code=ifelse(nchar(state_code)==1,paste("0",state_code,sep=""),state_code))

#Pintamos los índices sobre el mapa de México
#Acomodamos el shape
municipios_map_f <- readRDS(file = "data/Geografía/municipios_map_f.RDS") %>% 
  left_join(aux2,by=c("CVE_ENT" = "state_code","CVE_MUN" = "mun_code"))

saveRDS(municipios_map_f,"data/Crimen/datos_mapa.RDS")

municipios_map_f <- readRDS("data/Crimen/datos_mapa.RDS") %>% 
  mutate(dimension_crimen.C=cut(dimension_crimen,breaks=c(-Inf,.25,.5,.75,Inf),
                               labels=c("Bajo","Moderado","Alto","Muy Alto")),
         dimension_crimen_no_violento.C=cut(dimension_crimen_no_violento,breaks=c(-Inf,.25,.5,.75,Inf),
                                labels=c("Bajo","Moderado","Alto","Muy Alto")),
         dimension_crimen_violento.C=cut(dimension_crimen_violento,breaks=c(-Inf,.25,.5,.75,Inf),
                                labels=c("Bajo","Moderado","Alto","Muy Alto")),
         dimension_secuestro.C=cut( dimension_secuestro,breaks=c(-Inf,.25,.5,.75,Inf),
                                labels=c("Bajo","Moderado","Alto","Muy Alto")))



mapa <- function(df,color,title){
  mp <- ggplot() + 
    geom_polygon(data = df, 
                 aes_string(x="long", y="lat", group="group", 
                     fill=color),
                 colour="gray45") +
    scale_fill_gradient2(low="forestgreen",
                         mid="gold1",
                         high="firebrick1",
                         midpoint = .5,
                         limits=c(0,1)) +
    theme_map() +
    coord_equal() +
    ggtitle(title)
  return(mp)
}

print(mapa(municipios_map_f,"dimension_crimen","Mapa de Crimen"))
print(mapa(municipios_map_f,"dimension_crimen_no_violento","Mapa de Crimen"))
print(mapa(municipios_map_f,"dimensio_crimen_violento","Mapa de Crimen"))
print(mapa(municipios_map_f,"dimension_secuestro","Mapa de Crimen"))

print(mapa("dimension_crimen.C","Mapa de Crimen"))
print(mapa("dimension_crimen_no_violento.C","Mapa de Crimen"))
print(mapa("dimensio_crimen_violento.C","Mapa de Crimen"))
print(mapa("dimension_secuestro.C","Mapa de Crimen"))


prueba <- municipios_map_f %>% 
  filter(CVE_ENT %in% c('20'))

print(mapa(prueba,"dimension_crimen","Mapa de Crimen"))



