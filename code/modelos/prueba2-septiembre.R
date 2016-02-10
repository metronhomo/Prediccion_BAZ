library(ggplot2)
library(lubridate)
library(dplyr)
library(caret)
library(kernlab)

#Miramos la distribución de pedidos en los clientes q van a entrar a la prueba----
datos <- read.table("cu_num_trans_sept_prueba2.csv",header=F,sep=",",stringsAsFactors = F) %>% 
  select (HASH_MAP_CU = V1,
          CUENTA = V2)
datos$CUENTA <- as.numeric(datos$CUENTA)

#Vemos los cuantiles de la distribución de pedidos----
quantile(datos$V2,probs = seq(0,1,.01),na.rm = T)

#Nos quedamos con los atípicos para dsps filtrarlos en sqlite
datos2 <- datos %>% 
  filter(CUENTA > 500)

write.table(datos2,file="atipicos_prueba2.csv",sep=",", row.names = F, col.names = F)

#Hacemos una gráfica una vez que sabemos en dónde está el 99% de la distribución 
ggplot(datos, aes(x=V2)) + 
  geom_histogram(aes(y=..count../sum(..count..))) +
  scale_x_continuous(limits=c(0,500))

#Algunas funciones que vamos a ocupar---

#Obtenemos el total de abonos (pos) o retiros (neg)
cuenta_pos_neg <- function(col, cond){
  if(cond)
    res <- length(col[col>0])
  else
    res <- length(col[col<0])
  return(res)
}
#Sacamos la suma dependiendo de abonos (pos) o retiros (neg)
sum_pos_neg <- function(col, cond){
  if(cond)
    res <- sum(col[col>0])
  else
    res <- sum(col[col<0])
  return(res)
}
#Sacamos el máximo dependiendo de si fue abono (pos) o retiro (neg)
max_pos_neg <- function(col, cond){
  if(cond){
    col <- col[col>0]
    res <- ifelse(length(col) == 0, 0, max(col))
  }
  else{
    col <- col[col<0]
    res <- ifelse(length(col) == 0, 0, -max(-col))
  }
  return(res)
}
#Sacamos el mínimo dependiendo de si fue abono (pos) o retiro (neg)
min_pos_neg <- function(col, cond){
  if(cond){
    col <- col[col>0]
    res <- ifelse(length(col) == 0, 0, min(col))
  }
  else{
    col <- col[col<0]
    res <- ifelse(length(col) == 0, 0, -min(-col))
  }
  return(res)
}
#Sacamos la mediana dependiendo si son abonos (pos) o retiros (neg)
median_pos_neg <- function(col, cond){
  if(cond){
    col <- col[col>0]
    res <- ifelse(length(col) == 0, 0, median(col))
  }
  else{
    col <- col[col<0]
    res <- ifelse(length(col) == 0, 0, median(col))
  }
  return(res)
}

#Esta función genera texto. Este texto dsps lo vamos a evaluar y así obtenemos las variables que 
#queremos con los nombres que queremos. Recibe como argumento los nombres que queremos tengan las 
#variables y el df
gen_txt <- function(df,ventana,dias_dif_max,dias_dif_min,num_trans,num_abonos,num_retiros,med_gral,med_abonos,
                    med_retiros,max_abono,max_retiro,min_abono,min_retiro,sum_abono,sum_retiro,
                    coc_abonos_retiro,coc_max_abono,coc_max_retiro,coc_abono_gral,coc_retiro_gral){
  texto <- paste0(
    "
    fecha_max_abono <- transac %>% 
    filter(difftime(FECHA_PRIM_SURT2, T071_DAT_OPERATION, units = 'days') <=", ventana,",
    difftime(FECHA_PRIM_SURT2, T071_DAT_OPERATION, units = 'days') >= 0) %>% 
    group_by(HASH_MAP_CU) %>% 
    filter(T071_AMOUNT > 0) %>% 
    filter(T071_AMOUNT == max(T071_AMOUNT)) %>% 
    select(HASH_MAP_CU, T071_DAT_OPERATION, FECHA_PRIM_SURT2) %>% 
    mutate(T071_DAT_OPERATION = max(T071_DAT_OPERATION)) %>% 
    unique() %>% 
    mutate(",dias_dif_max, "= as.integer(difftime(FECHA_PRIM_SURT2, T071_DAT_OPERATION, unit = 'days'))) %>% 
    select(HASH_MAP_CU,", dias_dif_max,")
    
    fecha_min_abono <- transac %>% 
    filter(difftime(FECHA_PRIM_SURT2, T071_DAT_OPERATION, units = 'days') <=", ventana,",
    difftime(FECHA_PRIM_SURT2, T071_DAT_OPERATION, units = 'days') >= 0) %>% 
    group_by(HASH_MAP_CU) %>% 
    filter(T071_AMOUNT > 0) %>%
    filter(T071_AMOUNT == min(T071_AMOUNT)) %>% 
    select(HASH_MAP_CU, T071_DAT_OPERATION, FECHA_PRIM_SURT2) %>% 
    mutate(T071_DAT_OPERATION = max(T071_DAT_OPERATION)) %>% 
    unique() %>% 
    mutate(",dias_dif_min,"= as.integer(difftime(FECHA_PRIM_SURT2, T071_DAT_OPERATION, unit = 'days'))) %>% 
    select(HASH_MAP_CU,", dias_dif_min,")
    ",
    df, 
    "<- transac %>% 
    filter(difftime(FECHA_PRIM_SURT2, T071_DAT_OPERATION, units = 'days') <= ",ventana,",
    difftime(FECHA_PRIM_SURT2, T071_DAT_OPERATION, units = 'days') >= 0) %>% 
    group_by(HASH_MAP_CU) %>% 
    summarise(",
    num_trans, "= n(),",
    num_retiros,"= cuenta_pos_neg(T071_AMOUNT,FALSE),",
    num_abonos,"= cuenta_pos_neg(T071_AMOUNT,TRUE),",
    med_gral, "= median(T071_AMOUNT),",
    med_abonos, "= median_pos_neg(T071_AMOUNT, TRUE),",
    med_retiros," = median_pos_neg(T071_AMOUNT, FALSE),",
    max_abono," = max_pos_neg(T071_AMOUNT, TRUE),",
    max_retiro," = max_pos_neg(T071_AMOUNT, FALSE),",
    min_abono, "= min_pos_neg(T071_AMOUNT, TRUE),",
    min_retiro," = min_pos_neg(T071_AMOUNT, FALSE),",
    sum_abono, "= sum_pos_neg(T071_AMOUNT, TRUE),",
    sum_retiro, "= sum_pos_neg(T071_AMOUNT, FALSE),",
    coc_abonos_retiro, "= ifelse(",sum_retiro," == 0, 0,", sum_abono,"/",sum_retiro,"),",
    coc_max_abono, "= ifelse(",med_abonos,"==0,0,",max_abono,"/",med_abonos,"),",
    coc_max_retiro, "= ifelse(",med_retiros,"==0,0,",max_retiro,"/",med_retiros,"),",
    coc_abono_gral, "= ifelse(",med_gral,"==0,0,",med_abonos,"/",med_gral,"),",
    coc_retiro_gral, "= ifelse(",med_gral,"==0,0,",med_retiros,"/",med_gral,")) %>% 
    left_join(fecha_min_abono) %>% 
    left_join(fecha_max_abono)")
  
  return(texto)
}

#Esta función genera los nombres de las variables y del df con los que alimentamos la función de arriba
#Básicamente lo q hacemos es poner como subíndice el tamaño de la ventana.
gen_vrbs <- function(n){
  texto <- gen_txt(paste0("df_",n), 
                   n,
                   paste0("dias_dif_max_",n),
                   paste0("dias_dif_min_",n),
                   paste0("num_trans_",n),
                   paste0("num_retiros_",n),
                   paste0("num_abonos_",n),
                   paste0("med_gral_",n),
                   paste0("med_abonos_",n),
                   paste0("med_retiros_",n),
                   paste0("max_abono_",n),
                   paste0("max_retiro_",n),
                   paste0("min_abono_",n),
                   paste0("min_retiro_",n),
                   paste0("sum_abono_",n),
                   paste0("sum_retiro_",n),
                   paste0("coc_abono_retiro_",n),
                   paste0("coc_max_abono_",n),
                   paste0("coc_max_retiro_",n),
                   paste0("coc_abono_gral_",n),
                   paste0("coc_retiro_gral_",n))
  return(texto)
}

# Función para hacer curva ROC en ggplot ----------------------------------
rocdata <- function(grp, pred){
  # Produces x and y co-ordinates for ROC curve plot
  # Arguments: grp - labels classifying subject status
  #            pred - values of each observation
  # Output: List with 2 components:
  #         roc = data.frame with x and y co-ordinates of plot
  #         stats = data.frame containing: area under ROC curve, p value, upper and lower 95% confidence interval
  
  grp <- as.factor(grp)
  if (length(pred) != length(grp)) {
    stop("The number of classifiers must match the number of data points")
  } 
  
  if (length(levels(grp)) != 2) {
    stop("There must only be 2 values for the classifier")
  }
  
  cut <- unique(pred)
  tp <- sapply(cut, function(x) length(which(pred > x & grp == levels(grp)[2])))
  fn <- sapply(cut, function(x) length(which(pred < x & grp == levels(grp)[2])))
  fp <- sapply(cut, function(x) length(which(pred > x & grp == levels(grp)[1])))
  tn <- sapply(cut, function(x) length(which(pred < x & grp == levels(grp)[1])))
  tpr <- tp / (tp + fn)
  fpr <- fp / (fp + tn)
  roc = data.frame(x = fpr, y = tpr)
  roc <- roc[order(roc$x, roc$y),]
#   
#   i <- 2:nrow(roc)
#   auc <- (roc$x[i] - roc$x[i - 1]) %*% (roc$y[i] + roc$y[i - 1])/2
#   
#   pos <- pred[grp == levels(grp)[2]]
#   neg <- pred[grp == levels(grp)[1]]
#   q1 <- auc/(2-auc)
#   q2 <- (2*auc^2)/(1+auc)
#   se.auc <- sqrt(((auc * (1 - auc)) + ((length(pos) -1)*(q1 - auc^2)) + ((length(neg) -1)*(q2 - auc^2)))/(length(pos)*length(neg)))
#   ci.upper <- auc + (se.auc * 0.96)
#   ci.lower <- auc - (se.auc * 0.96)
#   
#   se.auc.null <- sqrt((1 + length(pos) + length(neg))/(12*length(pos)*length(neg)))
#   z <- (auc - 0.5)/se.auc.null
#   p <- 2*pnorm(-abs(z))
#   
#   stats <- data.frame (auc = auc,
#                        p.value = p,
#                        ci.upper = ci.upper,
#                        ci.lower = ci.lower
#   )
  
  # return (list(roc = roc, stats = stats))+
  return(roc)
}

# Comienza propiamente los cálculos para hacer la prueba hasta septiembre-----

#Con la distribución de pedidos vamos a crear un catálogo para quedarnos con clientes 
#que tengan hasta 500 transacciones
cat_clientes_prueba <- datos %>% 
  #Quitamos a todas las personas que tuvieron más de 500 transaccions (> percentil 99)
  filter(CUENTA <= 500)

#Leemos el catálogo de clientes (aquí están los que tienen más y menos de 500 transacciones)
#y le ponemos nombre a las variables
cat_todos_prueba <- read.table("cat_client_sept.csv",sep = ",",header = F,stringsAsFactors = F) %>% 
  select(HASH_MAP_CU = V1,
         FECHA_PRIM_SURT = V2)

#Nos quedamos solo con las personas que tienen menos de 500 transacciones
catalogo <- cat_clientes_prueba %>% 
  inner_join(cat_todos_prueba)

saveRDS(catalogo,"catalogo_prueba_sept.RDS")
rm(cat_todos_prueba,cat_clientes_prueba,datos)

catalogo <- readRDS("catalogo_prueba_sept.RDS")

#Leemos los datos de las transacciones de los clientes que entran a la prueba---
df <- read.table("trans_prueba_sept6_v2.csv",header=F,sep=",",stringsAsFactors = F)
saveRDS(df,"datos_prueba_septiembre.RDS")

#Leemos y nombramos los datos de las transacciones
transac <- readRDS("datos_prueba_septiembre.RDS") %>% 
  select(HASH_MAP_CU = V1,
         TO71_AMOUNT = V2,
         T071_DAT_OPERATION = V3) 

#Nos quedamos solo con las variables que nos interesan del catálogo
catalogo2 <- catalogo %>% select(HASH_MAP_CU,FECHA_PRIM_SURT)

write.table(catalogo2,"catalogo_join_prueba_sept.csv",sep=",",row.names = F,col.names = F)

#Queremos hacer un join entre las transacciones y el catálogo. R no es tan bueno para hacer esto...
transac <- trans %>% 
  inner_join(catalogo2) %>% 
  mutate(FECHA_PRIM_SURT2 = as.Date(FECHA_PRIM_SURT, format = "%Y-%m-%d"),
         T071_DAT_OPERATION = as.Date(T071_DAT_OPERATION, format = "%Y-%m-%d")) %>% 
  filter(T071_AMOUNT != 0)

############################################################################################################
## Como lo de arriba era muy compicado de hacerlo en R lo hicimos desde bash                              ##
#                                                                                                         ##
## Primer hicimos:                                                                                        ##
## dos2unix -n archivo_viejo.csv archivo_nuevo.csv                                                        ##
## lo anterior para que tuviera el formato adecuado. Esto para el archivo con las transacciones           ##
#                                                                                                         ##
## Una vez que teníamos los formatos adecuados corrimos la siguiente línea que hace el join en bash:      ##
## join -t ',' trans_prueba_sept7.csv catalogo_join_prueba_sept_sort_v2.csv > join_trans_prueba_sept2.csv ##
############################################################################################################

#Una vez que tenemos la tabla con todos los datos que necesitamos (los del join) creamos las variables
#que faltan y filtramos las transacciones que son cero                                                  
transac <- read.table("join_trans_prueba_sept2.csv",header = F,sep=",",stringsAsFactors = F) 
names(transac) <- c("HASH_MAP_CU","T071_AMOUNT","T071_DAT_OPERATION","FECHA_PRIM_SURT")

transac <- transac %>% 
  mutate(FECHA_PRIM_SURT2 = as.Date(FECHA_PRIM_SURT, format = "%Y-%m-%d"),
         T071_DAT_OPERATION = as.Date(T071_DAT_OPERATION, format = "%Y-%m-%d")) %>% 
  filter(T071_AMOUNT != 0)

saveRDS(transac,"trans_prueba_sept.RDS")

transac <- readRDS("trans_prueba_sept.RDS")

#Creamos las variables para el último mes (30 días), para los últimos tres meses (90 días), para los 
#últimos seis meses (180 días) y para el último año (360 días).

#Hay que correrlo i por i si no se acaba la RAM y todo muere ):
i <- 360

for(i in c(30,90,180,360)){
  eval(parse(text = gen_vrbs(i)))
}
#Guardamos los df's
saveRDS(df_360,"df_360_prueba_sept.RDS")
saveRDS(fecha_max_abono,"fecha_max_abono_360.RDS")
saveRDS(fecha_min_abono,"fecha_min_abono_360.RDS")

rm(df_360,fecha_max_abono,fecha_min_abono)

rm(transac)
#Leemos los df's que acabamos de crear
df_30 <- readRDS("df_30_prueba_sept.RDS")
df_90 <- readRDS("df_90_prueba_sept.RDS")
df_180 <- readRDS("df_180_prueba_sept.RDS")
df_360 <- readRDS("df_360_prueba_sept.RDS")

#Juntamos los df
df <- df_30 %>% 
  full_join(df_90,by=c("HASH_MAP_CU")) %>% 
  full_join(df_180,by=c("HASH_MAP_CU")) %>% 
  full_join(df_360,by=c("HASH_MAP_CU")) %>% 
  mutate_each(funs(replace(., which(is.na(.)), 0))) %>% 
  as.data.frame()
#Creamos las variables de interacción entre tiempo que hacían falta
df2 <- df %>% 
  mutate(coc_90_180_gral=ifelse(med_gral_180==0,0,med_gral_90/med_gral_180),
         coc_90_180_abonos=ifelse(med_abonos_180==0,0,med_abonos_90/med_abonos_180),
         coc_90_180_retiros=ifelse(med_retiros_180==0,0,med_retiros_90/med_retiros_180))

df <- df2;rm(df2)

saveRDS(df,"df_prueba_sept_final.RDS")

df2 <- readRDS("df_prueba_sept_final.RDS")

#Leemos los datos de las personas que sí son de crédito
#Seleccionamos a las personas que tienen una fecha de solicitud adecuada
convertidos <- read.table("convertidos2.csv",sep=",",header = T,stringsAsFactors = F) %>% 
  mutate(credito = 1,
         FDFECSOL = as.Date(FDFECSOL,"%d/%m/%Y"),
         ventana = ifelse(FDFECSOL > as.Date("31/08/2015","%d/%m/%Y"),1,0))

#Leemos el catálogo de clientes que entraron a la prueba
catalogo <- read.table("join_no_atipicos.csv",header = F,sep = ",",stringsAsFactors = F)
names(catalogo) <- c("HASH_MAP_CU","Cuenta","NPAIS","NCANAL","NSUCURSAL","NFOLIO")

#Al catálogo le pegamos los datos de los que sí son crédito
catalogo2 <- catalogo %>% 
  left_join(convertidos %>% select(NPAIS,NCANAL,NSUCURSAL,NFOLIO,credito,ventana)) %>% 
  mutate_each(funs(replace(., which(is.na(.)), 0)))

#Pegamos el dato de quién tiene crédito en la prueba
df2 <- df %>% 
  left_join(catalogo2 %>% select(HASH_MAP_CU,ventana))

df <- df2;rm(df2)

saveRDS(df,"df_prueba_sept_final.RDS")

df2 <- readRDS("df_prueba_sept_final.RDS")

#Cargamos los modelos
rf2_model <- readRDS("./Modelo_Bosque_2.RDS")
log_model <- readRDS("./Modelo_reg_log.RDS")
svm_model <- readRDS("./Modelo_SVM.RDS")
gb_model <- readRDS("./Modelo_GB.RDS")

#Probamos los modelos en la base nueva
rf_pred <- predict(rf2_model,df2,type="prob")
rf_pred$pred <- 0
rf_pred[(which(rf_pred[,1] < rf_pred[,2])),3] <- 1
confusionMatrix(rf_pred$pred, df2$ventana)

log_pred <- predict(log_model,df2,type="response")
log_pred <- as.data.frame(log_pred)
log_pred$pred <- 0
log_pred[(which(log_pred>.5)),2] <- 1
confusionMatrix(log_pred$pred, df2$ventana)

svm_pred <- predict(svm_model,df2)
svm_pred <- as.data.frame(svm_pred)
svm_pred$pred <- 0
svm_pred[(which(svm_pred[,1] < svm_pred[,2])),3] <- 1

gb_pred <- predict(gb_model,df2,type="prob")
length(which(gb_pred[,1] < gb_pred[,2]))
gb_pred$pred <- 0
gb_pred[(which(gb_pred[,1] < gb_pred[,2])),3] <- 1
confusionMatrix(gb_pred$pred, df2$ventana)

#Hacemos un df con las predicciones de cada uno de los modelos
predicciones <- data.frame(HASH_MAP_CU=df2$HASH_MAP_CU,
                           rf=rf_pred[,3],
                           gb=gb_pred[,3],
                           log=log_pred[,2],
                           credito=df2$credito,
                           ventana=df2$ventana)
#Combinamos respuestas
predicciones2 <- predicciones %>% 
  mutate(comb = ifelse(log + gb + rf >=1,1,0))

confusionMatrix(predicciones2$comb, predicciones2$ventana)

# Curva ROC ---------------------------------------------------------------
df <- readRDS("df_prueba_sept_final.RDS")

rf_pred <- predict(rf2_model,select(df,-HASH_MAP_CU,-ventana,-credito),type="prob")
saveRDS(rf_pred,"pred_prueba_sept_rf.RDS")
rf_prob <- data.frame(rf_pred=rf_pred[,2])
plotdata_rf <- rocdata(df$ventana, rf_prob$rf_pred)

log_pred <- predict(log_model,select(df,-HASH_MAP_CU,-ventana,-credito),type="response")
saveRDS(log_pred,"pred_prueba_sept_log.RDS")
log_prob <- data.frame(log_pred=as.numeric(log_pred))
plotdata_log <- rocdata(df$ventana, log_prob$log_pred)

svm_pred <- predict(svm_model,select(df,-HASH_MAP_CU,-ventana,-credito),type="prob")
saveRDS(svm_pred,"pred_prueba_sept_svm.RDS")
svm_prob <- data.frame(svm_pred=svm_pred[,2])
plotdata_svm <- rocdata(df$ventana, svm_prob$svm_pred)

gb_pred <- predict(gb_model,select(df,-HASH_MAP_CU,-ventana,-credito),type="prob")
saveRDS(gb_pred,"pred_prueba_sept_gb.RDS")
gb_prob <- data.frame(gb_pred=as.numeric(gb_pred[,2]))
plotdata_gb <- rocdata(df$ventana, gb_prob$gb_pred)

# Gráfica con la curva ROC ------------------------------------------------

graf_roc <- ggplot() +
  geom_line(data=plotdata_rf$roc, aes(x = x, y = y,colour = "Random Forest"),size=.8) +
  geom_line(data=plotdata_gb$roc, aes(x = x, y = y,colour = "Gradient Boosting"),size=.8) +
  geom_line(data=plotdata_log$roc, aes(x = x, y = y,colour = "Logistic Regression"),size=.8) +
  geom_line(data=plotdata_svm$roc, aes(x = x, y = y,colour = "SVM"),size=.8) +
  geom_abline (intercept = 0, slope = 1,size=1.5) +
  scale_x_continuous("False Positive Rate (1-Specificity)") +
  scale_y_continuous("True Positive Rate (Sensitivity)") +
  scale_colour_manual(name="Modelos",
                      values = c("Random Forest"="brown1",
                                 "Logistic Regression"="darkorchid4",
                                 "Gradient Boosting"="aquamarine4",
                                 "SVM" = "darkgoldenrod2")) +
  ggtitle("Desempeño de Modelos") + 
  theme(
    axis.text.x=element_text(size=22, colour = 'black'),
    axis.title.x=element_text(size=22, colour = 'black'),
    axis.text.y=element_text(size=22, colour = 'black'),
    axis.title.y=element_text(size=22, colour = 'black'),
    plot.title=element_text(size=22, colour = 'black'),
    panel.background=element_rect(fill='grey95'),
    strip.background=element_rect(fill="#2c3e50"),
    panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
    strip.text.x = element_text(colour = 'white', size = 22),
    strip.text.y = element_text(colour = 'white', size = 22),
    legend.text=element_text(size=20),
    legend.title=element_text(size=20),
    panel.grid.major = element_line(colour = 'grey70', linetype = 'dashed'),
    panel.grid.minor = element_line(colour = 'grey70', linetype = 'dashed'))

saveRDS(graf_roc,"Graf_curva_roc_4_modelos_prueba_sept.RDS")

#Miramos relaciones entre las variabes-----

#Hacemos muestra de entrenamiento y validación
set.seed(108176)

df_entrenamiento <- df[sample(nrow(df),size = round(nrow(df)*.7,0)),]
df_prueba <- df %>% 
  anti_join(df_entrenamiento,by="HASH_MAP_CU")

corrs <-  cor(df_prueba[,-1],use="complete.obs",method="spearman")

library(corrplot)

corrplot(corrs,tl.cex=.5,tl.col="black",order="AOE")
