library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(tidyr)
library(readxl)
#Un poco más sobre el ingreso sacado por AGEB y el crimen

#Leemos datos del NSE que reporta la AMAI
nse_amai <- read_excel("../../data/Geo/NSE_AMAI_EDOS.xlsx",sheet = 1,col_names = T)
names(nse_amai) <- c("ESTADOS","AB","C_MAS","C","C_MENOS","D_MAS","D","E")    

nse_amai <- nse_amai %>%
  group_by(ESTADOS) %>% 
  mutate(alto=AB,
         medio=sum(C_MAS,C),
         bajo=sum(C_MENOS,D_MAS,D,E)) %>% 
  ungroup()

#Leemos los datos de los clientes con ahorro y ahorro crédito
datos <- readRDS("../../output/SFTP/crédito y captación.RDS")
names(datos) <- gsub(" ", "_", names(datos))

#Hacemos algunos cambios a la base original
datos$credit_or_savings <- as.integer(is.na(datos$zip_code))
datos$zip_code2 <- as.character(datos$zip_code)
idx <- is.na(datos$zip_code)
datos$zip_code2[idx] <- datos$cred_zip_code[idx]
datos2 <- datos %>%
  mutate(zip_code = gsub('"',"",zip_code2),
         gender = gsub("(.*)M(.*)", "M", as.character(gender)),
         gender = gsub("(.*)F(.*)", "F", gender),
         credit_or_savings = gsub("1", "credit", credit_or_savings),
         credit_or_savings = gsub("0", "savings", credit_or_savings),
         savings_aplication_date = as.Date(savings_aplication_date,"%d/%m/%y"),
         age_C=cut(age,
                   breaks=c(0,17,25,35,45,55,65,Inf),
                   labels=c(NA,"18-25","26-35","36-45","46-55","56-65",">65"))) %>%
  select(-zip_code2, -cred_zip_code)
idx <- datos2$zip_code == "00000"
datos2$zip_code[idx] <- NA

#Leemos los datos de CP
cp <- readRDS("../../output/Geo/Detalle_CP.RDS") %>% 
  select(d_codigo,
         d_tipo_asenta,
         D_mnpio,
         d_estado,
         d_zona) %>% 
  as.data.frame() %>% 
  mutate(d_codigo = as.integer(d_codigo)) %>% 
  filter(!duplicated(d_codigo)) #Quitamos los repetidos por tipo de asentamiento (asumimos q el primero es
                                # el bueno)

#Leemos los datos del mapeo AGEB CP
mapeo <- read.csv("../../output/Geo/mapeo_CP_AGEB_con_info_socioeconomica.csv") %>%
  select(-id, -distance) %>% 
  left_join(cp,by=c("CP"="d_codigo"))

#Pegamos los datos de mapeo a la base de clientes
datos3 <- datos2 %>%
  mutate(CP = as.integer(zip_code)) %>%
  left_join(mapeo[,c("CP", "E_AGEB","d_tipo_asenta","D_mnpio","d_estado","d_zona")]) %>%
  mutate(d_estado=toupper(d_estado),
         D_mnpio=toupper(D_mnpio)) %>% 
  select(-CP)

rm(datos,datos2)

#Quitamos los acentos de las varaibles Estado y municipio
unwanted_array = list('Á'='A','É'='E','Í'='I','Ó'='O','Ú'='U')

quita_acentos <- function(string){
  st <- chartr(paste(names(unwanted_array), collapse=''),
         paste(unwanted_array, collapse=''),
         string)
  return(st)
}

datos3$d_estado <- sapply(as.character(datos3$d_estado),function(s) quita_acentos(s))
datos3$D_mnpio <- sapply(as.character(datos3$D_mnpio),function(s) quita_acentos(s))

#El 28% de las personas no tiene estado. 
#Esto se debe a que hay 517138 personas que tienen un CP inválido
#En particular hay 90152 personas que no tienen CP

#Este df lo hacemos para poder colorear la gráfica de cred/ahorro por nivel del AGEB
colores <- datos3 %>% 
  group_by(d_estado,E_AGEB) %>% 
  tally() %>% 
  group_by(d_estado) %>% 
  mutate(total_estado=sum(n,na.rm=T)) %>% 
  ungroup() %>%
  mutate(p=n/total_estado) %>% 
  select(-n,-total_estado) %>% 
  as.data.frame() %>% 
  spread(E_AGEB,p) %>% 
  select_("d_estado",
          Nivel_1="2",
          Nivel_2="3",
          Nivel_3="4",
          Nivel_4="5",
          Nivel_5="6",
          Nivel_6="7",
          Nivel_7="8") %>% 
  filter(!is.na(d_estado)) %>% 
  group_by(d_estado) %>% 
  mutate(pauperrimo=sum(Nivel_1,Nivel_2,Nivel_3,na.rm=T),
         medio=sum(Nivel_4,Nivel_5,na.rm=T),
         alto=sum(Nivel_6,Nivel_7,na.rm=T)) %>% 
  mutate(bienestar=ifelse(max(sum(Nivel_1,Nivel_2,Nivel_3,Nivel_4,na.rm=T),sum(Nivel_5,Nivel_6,na.rm=T),sum(Nivel_7,na.rm=T))==sum(Nivel_1,Nivel_2,Nivel_3,Nivel_4,na.rm=T),"Bajo",
                          ifelse(max(sum(Nivel_1,Nivel_2,Nivel_3,Nivel_4,na.rm=T),sum(Nivel_5,Nivel_6,na.rm=T),sum(Nivel_7,na.rm=T))==sum(Nivel_5,Nivel_6,na.rm=T),"Medio",
                                 "Alto"))) %>% 
  select(d_estado,bienestar)

#Obtenemos el df de ahorro para hacer la gráfica
ahorro <- datos3 %>% 
  group_by(d_estado,credit_or_savings) %>% 
  tally() %>% 
  group_by(d_estado) %>% 
  mutate(total_edo=sum(n)) %>% 
  filter(complete.cases(d_estado),
         credit_or_savings=="savings") %>% 
  arrange(desc(total_edo)) %>% 
  ungroup() %>% 
  mutate(d_estado=factor(d_estado,levels=unique(as.character(d_estado))),
         p=n/total_edo) %>% 
  filter(d_estado!="GUERRERO")
#Ajustamos la recta de regresión que va a aparecer en la gráfica
reg2 <- data.frame(reg=lm(formula = p~log10(total_edo),
                          data = ahorro[ahorro$d_estado!="GUERRERO",])$fitted.values)
#Pegamos los colores 
ahorro <- ahorro %>% 
  bind_cols(reg2) %>% 
  left_join(colores) 
#Reordenamos los factores
ahorro$bienestar <- factor(ahorro$bienestar,levels = c("Alto","Medio","Bajo"))

#Hacemos una gráfica en la que dibujamos los estados. Una recta de regresión para 
#saber cuáles están por encima de lo esperado y finalmente pintamos los puntos del estado
#de acuerdo al nivel de ingreso del ageb en el que está
p1 <- ahorro %>% 
  ggplot() +
  geom_area(aes(x=log10(total_edo),y=reg),alpha=.3) +
  geom_point(aes(x=log10(total_edo),y=p,colour=bienestar,fill=bienestar),size=3) +
  geom_smooth(aes(x=log10(total_edo),y=p),method='lm',formula=y~x,colour="red",fill=NA) +
  geom_text(aes(x=log10(total_edo),y=p+.01,label=d_estado),size=3.5) +
  geom_text(x=3.75,y=.7,label="Estados con porcentaje de ahorro \n por encima de lo esperado",colour="red",size=4) +
  geom_text(x=5.25,y=.45,label="Estados con porcentaje de ahorro \n por debajo de lo esperado",colour="red",size=4) +
  coord_cartesian(ylim=c(.4,.75)) +
  xlab("Log10 del número de clientes") +
  ylab("Porcentaje de clientes con ahorro")

#Obtenemos el df de crédito
cred <- datos3 %>% 
  group_by(d_estado,credit_or_savings) %>% 
  tally() %>% 
  group_by(d_estado) %>% 
  mutate(total_edo=sum(n)) %>% 
  filter(complete.cases(d_estado),
         credit_or_savings=="credit") %>% 
  arrange(desc(total_edo)) %>% 
  ungroup() %>% 
  mutate(d_estado=factor(d_estado,levels=unique(as.character(d_estado))),
         p=n/total_edo) %>% 
  filter(d_estado!="GUERRERO")
#Ajustamos la recta de regresión para este caso
reg <- data.frame(reg=lm(formula = p~log10(total_edo),
                         data = cred[cred$d_estado!="GUERRERO",])$fitted.values)
#Pegamos la recta de regresión y tmb los colores
cred <- cred %>% 
  bind_cols(reg) %>% 
  left_join(colores)
#Cambiamos el orden de los niveles
cred$bienestar <- factor(cred$bienestar,levels = c("Alto","Medio","Bajo"))
#Esta es la misma gráfica que p1 pero para los datos de crédito
p2<-cred %>% 
  ggplot() +
  geom_area(aes(x=log10(total_edo),y=reg),alpha=.3) +
  geom_point(aes(x=log10(total_edo),y=p,colour=bienestar,fill=bienestar),size=3) +
  geom_smooth(aes(x=log10(total_edo),y=p),method='lm',formula=y~x,colour="red",fill=NA) +
  geom_text(aes(x=log10(total_edo),y=p+.01,label=d_estado),size=3.5) +
  geom_text(x=3.75,y=.65,label="Estados con porcentaje de crédito \n por encima de lo esperado",colour="red",size=4) +
  geom_text(x=5.25,y=.3,label="Estados con porcentaje de crédito \n por debajo de lo esperado",colour="red",size=4) +
  coord_cartesian(ylim=c(.25,.7)) +
  xlab("Log10 del número de clientes") +
  ylab("Porcentaje de clientes con crédito")
#Pegamos las gráficas p1 y p2 en una sola
grid.arrange(p1,p2)
#Creamos el df con el que vamos a graficar la distribución de los niveles de AGEB por estado
edo_ageb <- datos3 %>% 
  group_by(d_estado,E_AGEB,credit_or_savings) %>% 
  tally() %>% 
  group_by(d_estado) %>% 
  mutate(total_estado=sum(n,na.rm=T)) %>% 
  group_by(E_AGEB) %>% 
  mutate(total_ageb=sum(n)) %>% 
  group_by(d_estado,E_AGEB) %>% 
  mutate(total_ageb_estado=sum(n,na.rm=T)) %>% 
  group_by(d_estado,credit_or_savings) %>% 
  mutate(total_c_s=sum(n,na.rm=T)) %>% 
  filter(complete.cases(d_estado)) %>% 
  as.data.frame() %>% 
  arrange(desc(total_estado)) %>% 
  mutate(p=total_ageb_estado/total_estado,
         d_estado=factor(d_estado,levels=unique(as.character(d_estado)))) %>%
  group_by(d_estado,E_AGEB,total_estado,p) %>%
  tally() %>% 
  filter(d_estado!="GUERRERO") 
#En esta gráfica se muestran los estados ordenados de mayor a menor número de clientes
#En las barras mostramos la distribución por ageb de los niveles
#En el número mostramos el % de personas con crédito sobre el total de personas de cada estado
p3 <- ggplot() +
  geom_bar(data=edo_ageb,aes(x=E_AGEB,y=p),stat="identity") +
  geom_text(data=edo_ageb,aes(x=E_AGEB,y=p + .08,label=round(p*100))) +
  geom_text(data=cred,aes(x=2,y=.6,label=paste(round(p*100),"%",sep="")),colour="red") +
  scale_x_continuous(breaks=seq(1,7,1)) +
  facet_wrap(~d_estado,ncol = 5) +
  scale_y_continuous(labels=percent)
#Pegamos las gráficas p3 y p1 en una sola  
grid.arrange(p3,p2,ncol=2)

#Crimen
#Leemos los datos de crimen
municipios_map_f <- readRDS("../../output/EDA_markdown_preprocessing/datos_mapa.RDS") 

levels(municipios_map_f$state)[c(7,16,30)] <- c("COAHUILA DE ZARAGOZA",
                                                "MICHOACAN DE OCAMPO",
                                                "VERACRUZ DE IGNACIO DE LA LLAVE")

municipios_map_f <- municipios_map_f %>% 
  select(state,municipio,25:28) %>% 
  group_by(state,
           municipio,
           dimension_crimen,
           dimension_crimen_no_violento,
           dimension_crimen_violento,
           dimension_secuestro) %>% 
  tally() %>% 
  ungroup() %>% 
  select(-n)
#Pegamos los datos del crimen a la base de personas y agregamos variables categóricas creadas con 
#los índices.
datos4 <- datos3 %>% 
  left_join(municipios_map_f,by = c("d_estado"="state","D_mnpio"="municipio")) %>% 
  mutate(dimension_crimen.C=cut(dimension_crimen,breaks=c(-Inf,.25,.5,.75,Inf),
                                labels=c("Bajo","Moderado","Alto","Muy Alto")),
         dimension_crimen_no_violento.C=cut(dimension_crimen_no_violento,breaks=c(-Inf,.25,.5,.75,Inf),
                                            labels=c("Bajo","Moderado","Alto","Muy Alto")),
         dimension_crimen_violento.C=cut(dimension_crimen_violento,breaks=c(-Inf,.25,.5,.75,Inf),
                                         labels=c("Bajo","Moderado","Alto","Muy Alto")),
         dimension_secuestro.C=cut( dimension_secuestro,breaks=c(-Inf,.25,.5,.75,Inf),
                                    labels=c("Bajo","Moderado","Alto","Muy Alto")))
  
#Hacemos graficas de barras para mirar la distribución de los índices de crimen en las personas 
#de nuestra base. Estas gráficas las cortamos para diferenciar a los clientes de ahorro y crédito
p1 <- ggplot(datos4,aes(dimension_crimen)) +
  geom_histogram(aes(y=..count../sum(..count..)),binwidth=.025) +
  facet_wrap(~credit_or_savings,ncol = 1) +
  ylab("") +
  xlab("") +
  ggtitle("Dimensión Crimen")

p2 <- ggplot(datos4,aes(dimension_crimen_no_violento)) +
  geom_histogram(aes(y=..count../sum(..count..)),binwidth=.025) +
  facet_wrap(~credit_or_savings,ncol = 1) +
  ylab("") +
  xlab("") +
  ggtitle("Dimensión Crimen No Violento")

p3 <- ggplot(datos4,aes(dimension_crimen_violento)) +
  geom_histogram(aes(y=..count../sum(..count..)),binwidth=.025) +
  facet_wrap(~credit_or_savings,ncol = 1) +
  ylab("") +
  xlab("") +
  ggtitle("Dimensión Crimen Violento")

p4 <- ggplot(datos4,aes(dimension_secuestro)) +
  geom_histogram(aes(y=..count../sum(..count..)),binwidth=.025) +
  facet_wrap(~credit_or_savings,ncol = 1) +
  ylab("") +
  xlab("") +
  ggtitle("Dimensión Secuestro")
#Pegamos las distribuciones de todos los índices en una sola gráfica
grid.arrange(p1,p2,p3,p4,ncol=4)
#Hacemos los dfs auxiliares que vamos a ocupar para replicar las gráficas p1 y p2, pero ahora 
#coloreando por el nivel de crimen más abundante en el estado

#Función para obtener el máximo nivel de crimen
regresa_max <- function(df,rg){
  mx <- names(df)[2+which(df[rg,3:7]==max(df[rg,3:7],na.rm = T))]
  return(mx)
}
#Función para calcular los dfs q vamos a ocupar en las gráficas
calcula_df1 <- function(dimension){
  df1 <- datos4 %>% 
    select_("d_estado","credit_or_savings","dimension"=dimension) %>% 
    group_by(d_estado,credit_or_savings,dimension) %>% 
    tally() %>% 
    group_by(d_estado,credit_or_savings) %>% 
    mutate(total_edo_crimen=sum(n)) %>% 
    group_by(d_estado) %>% 
    mutate(total_edo=sum(n)) %>%   
    filter(complete.cases(d_estado)) %>% 
    ungroup() %>% 
    arrange(desc(total_edo)) %>% 
    mutate(d_estado=factor(d_estado,levels=unique(as.character(d_estado))),
           p=n/total_edo_crimen) %>% 
    filter(d_estado!="GUERRERO")
  return(df1)
}
#Esta función agrega la variable del máximo nivel de crimen
calcula_df2 <- function(df,nombre){
  #En este data frame vamos a obtener el nivel de crimen más popular por estado 
  df <- df %>% 
    select(d_estado,credit_or_savings,dimension,p) %>% 
    as.data.frame() %>% 
    spread(dimension,p)
  #Aplicamos la función para cada uno de los estados de la base
  mx <- sapply(1:(nrow(df)),function(r){regresa_max(df,r)})
  #Pegamos el resultado
  cadena <- paste0("df$",nombre," <- mx")
  eval(parse(text=cadena))
  
  return(df)
}

completa_cred <- function(dimension,nombre,cred){
  aux1_d1 <- calcula_df1(dimension)
  aux2_d1 <- calcula_df2(aux1_d1,nombre)
  
  #Pegamos el resultado en la base aux1 (con estos datos vamos a hacer la gráfica)
  aux1_d1 <- aux1_d1 %>% 
    left_join(aux2_d1[,c(1,2,8)],by=c("d_estado","credit_or_savings"))
  
  #Pegamos los datos auxiliares en la gráfica de crédito y posteriormente en la de ahorro
  cadena <- paste0('df <- cred %>% 
    left_join(aux1_d1[,c(1,2,8)],by=c("d_estado","credit_or_savings")) %>% 
    filter(!duplicated(c(d_estado))) %>% 
    mutate(',nombre,'=factor(',nombre,',levels=c("Bajo","Moderado","Alto","Muy Alto")))')
  
  eval(parse(text=cadena))
  return(df)
}

cred2 <- completa_cred("dimension_crimen.C","d1",cred)
cred3 <- completa_cred("dimension_crimen_no_violento.C","d2",cred2)
cred4 <- completa_cred("dimension_crimen_violento.C","d3",cred3)
cred5 <- completa_cred("dimension_secuestro.C","d4",cred4)

cred <- cred5
rm(cred2,cred3,cred4,cred5)

#Replicamos las gráficas de estado pero pintadas por crimen

graf_crimen_reg <- function(dimension){
  cadena<-paste0('p2<-cred %>% ggplot() +
                 geom_area(aes(x=log10(total_edo),y=reg),alpha=.3) +
                 geom_point(aes(x=log10(total_edo),y=p,colour=',
                 dimension,
                 ',fill=',
                 dimension,
                 '),size=3) +
                 geom_smooth(aes(x=log10(total_edo),y=p),method="lm",formula=y~x,colour="red",fill=NA) +
                 geom_text(aes(x=log10(total_edo),y=p+.01,label=d_estado),size=2) +
                 geom_text(x=3.75,y=.65,label="Estados con porcentaje de crédito \n por encima de lo esperado",colour="red",size=4) +
                 geom_text(x=5.25,y=.3,label="Estados con porcentaje de crédito \n por debajo de lo esperado",colour="red",size=4) +
                 coord_cartesian(ylim=c(.25,.7)) +
                 xlab("Log10 del número de clientes") +
                 ylab("Porcentaje de clientes con crédito") +
                 theme(legend.position="none")')
  eval(parse(text=cadena))
  
  return(p2)
}

p1 <- graf_crimen_reg("d1")
p2 <- graf_crimen_reg("d2")
p3 <- graf_crimen_reg("d3")
p4 <- graf_crimen_reg("d4")

grid.arrange(p1,p2,p3,p4,ncol=2)

