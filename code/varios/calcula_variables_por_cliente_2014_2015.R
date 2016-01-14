# Requiere la base de datos BAZ.db de 160 GB

library(dplyr)
library(lubridate)
library(RSQLite)

con = dbConnect(drv = SQLite(), dbname = "BAZ.db")
catalogo <- dbGetQuery(con, "SELECT 
  npais, ncanal, nsucursal, nfolio, hash_map_cu, hash_map_cta, fecha_prim_surt 
  from claves_2014_2015")

catalogo_capta_hash <- dbGetQuery(con, "SELECT 
                       npais, ncanal, nsucursal, nfolio, hash_map_cu, hash_map_cta 
                       from codigos_capta_hash")
dbDisconnect(con)

catalogo_hash <- catalogo %>% 
  select(NPAIS, NCANAL, NSUCURSAL, NFOLIO, HASH_MAP_CU) %>% 
  mutate(respuesta=1) %>% 
  unique()


catalogo_capta_hash <- catalogo_capta_hash %>%
  select(NPAIS, NCANAL, NSUCURSAL, NFOLIO, HASH_MAP_CU) %>% 
  mutate(respuesta=0) %>% 
  unique()


catalogo <- catalogo_hash %>% 
  bind_rows(catalogo_capta_hash)

rm(catalogo_capta_hash,catalogo_hash)

# Obtenemos los datos trans de las personas de captación 
# con = dbConnect(drv = SQLite(), dbname = "BAZ.db")
# trans_capta <- dbGetQuery(con, "SELECT 
#                           npais,ncanal,nsucursal,nfolio,hash_map_cu,hash_map_cta,
#                           t071_num_operation,t071_code,t071_amount,t071_dat_operation,
#                           t071_tim_operation
#                           from transacciones_capta")
# dbDisconnect(con)
# 
# trans_capta <- trans_capta %>% 
#   group_by(HASH_MAP_CU) %>%
#   mutate(FECHA_PRIM_SURT=max(T071_DAT_OPERATION))
# 
# saveRDS(trans_capta,"transac_capta.RDS")


##################################################################
# Gers
th <- read.table('./TH_clientes_capta_credito.txt', header = T, sep=",")
cap_pag_ini <- read.table("Base_800_CapPagInic_PrimerCompra_3.csv", sep = ",",
  col.names = 
  c("NPAIS","NCANAL","NSUCURSAL","NFOLIO","FECHA_PRIM_SURT","FIUNIDADNEGOCIO","FCDESCLARGA","FDCSALDOCAPITAL","FILCRACTIVA","CAP_ACTUAL","CAPDISP_ACTUAL","CAPPAGO_INI","MARCA")) %>% 
  select(NPAIS, NCANAL, NSUCURSAL, NFOLIO, FECHA_PRIM_SURT)
aux <- cap_pag_ini %>% 
  left_join(th) %>% 
  left_join(catalogo_hash) %>% 
  mutate(prim_surt=as.Date(FECHA_PRIM_SURT,"%d/%m/%y"),
         cred_prim_surt=as.Date(CRED_DMINFDSURT,"%d/%m/%y"),
         diferencia=as.integer(prim_surt-cred_prim_surt),
         anio1=year(prim_surt),
         anio2=year(cred_prim_surt)) %>% 
  filter(anio2 %in% c(2014,2015))

##################################################################
trans_capta <- readRDS("transac_capta.RDS") %>% 
  group_by(HASH_MAP_CU) %>% 
  mutate(FECHA_PRIM_SURT2 = as.Date(min(FECHA_PRIM_SURT), format = "%Y-%m-%d"),
         T071_DAT_OPERATION = as.Date(T071_DAT_OPERATION, format = "%Y-%m-%d"))


transac <- readRDS("./transacciones_2014_2015.RDS") %>% 
  left_join(catalogo) %>% 
  group_by(HASH_MAP_CU) %>% 
  mutate(FECHA_PRIM_SURT2 = as.Date(min(FECHA_PRIM_SURT), format = "%Y-%m-%d"),
         T071_DAT_OPERATION = as.Date(T071_DAT_OPERATION, format = "%Y-%m-%d")) %>% 
  filter(HASH_MAP_CU %in% aux$HASH_MAP_CU,
         T071_AMOUNT != 0)

transac2 <- rbind(trans_capta,transac)

transac <- transac2;rm(transac2,trans_capta)

saveRDS(transac,"trans_cred_2014_2015_capta.RDS")
# a <- transac %>% 
#   select(HASH_MAP_CU, HASH_MAP_CTA) %>% 
#   unique %>% 
#   group_by(HASH_MAP_CU) %>% 
#   tally() %>%
#   ungroup() %>% 
#   arrange(desc(n))


# Variables para 6 meses --------------------------------------------------
cuenta_pos_neg <- function(col, cond){
  if(cond)
    res <- length(col[col>0])
  else
    res <- length(col[col<0])
  return(res)
}

sum_pos_neg <- function(col, cond){
  if(cond)
    res <- sum(col[col>0])
  else
    res <- sum(col[col<0])
  return(res)
}

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


# 
# fecha_max_abono_6M <- transac %>% 
#   filter(difftime(FECHA_PRIM_SURT2, T071_DAT_OPERATION, units = "days") <= 180,
#          difftime(FECHA_PRIM_SURT2, T071_DAT_OPERATION, units = "days") >= 0) %>% 
#   group_by(HASH_MAP_CU) %>% 
#   filter(T071_AMOUNT > 0,
#          T071_AMOUNT == max(T071_AMOUNT)) %>% 
#   select(HASH_MAP_CU, T071_DAT_OPERATION, FECHA_PRIM_SURT2) %>% 
#   mutate(T071_DAT_OPERATION = max(T071_DAT_OPERATION)) %>% 
#   unique() %>% 
#   mutate(dias_dif_max_6M = as.integer(difftime(FECHA_PRIM_SURT2, T071_DAT_OPERATION, unit = "days"))) %>% 
#   select(HASH_MAP_CU, dias_dif_max_6M)
# 
# fecha_min_abono_6M <- transac %>% 
#   filter(difftime(FECHA_PRIM_SURT2, T071_DAT_OPERATION, units = "days") <= 180,
#          difftime(FECHA_PRIM_SURT2, T071_DAT_OPERATION, units = "days") >= 0) %>% 
#   group_by(HASH_MAP_CU) %>% 
#   filter(T071_AMOUNT > 0,
#          T071_AMOUNT == min(T071_AMOUNT)) %>% 
#   select(HASH_MAP_CU, T071_DAT_OPERATION, FECHA_PRIM_SURT2) %>% 
#   mutate(T071_DAT_OPERATION = max(T071_DAT_OPERATION)) %>% 
#   unique() %>% 
#   mutate(dias_dif_min_6M = as.integer(difftime(FECHA_PRIM_SURT2, T071_DAT_OPERATION, unit = "days"))) %>% 
#   select(HASH_MAP_CU, dias_dif_min_6M)
# 

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

for(i in c(30,90,180,360)){
  eval(parse(text = gen_vrbs(i)))
}

#Juntamos los df

df <- df_30 %>% 
  full_join(df_90,by=c("HASH_MAP_CU")) %>% 
  full_join(df_180,by=c("HASH_MAP_CU")) %>% 
  full_join(df_360,by=c("HASH_MAP_CU")) %>% 
  mutate_each(funs(replace(., which(is.na(.)), 0))) %>% 
  as.data.frame()

df2 <- df %>% 
  mutate(coc_90_180_gral=ifelse(med_gral_180==0,0,med_gral_90/med_gral_180),
         coc_90_180_abonos=ifelse(med_abonos_180==0,0,med_abonos_90/med_abonos_180),
         coc_90_180_retiros=ifelse(med_retiros_180==0,0,med_retiros_90/med_retiros_180))

df <- df2;rm(df2)

saveRDS(df,"df_solo_resumen_trans.RDS")

df <- readRDS("df_solo_resumen_trans.RDS")
# CPs --------------------------------------------------

datos <- readRDS("./crédito y captación.RDS")
names(datos) <- gsub(" ", "_", names(datos))
datos <- datos %>% 
  select(NPAIS, NCANAL, NSUCURSAL, NFOLIO, zip_code, cred_zip_code) %>% 
  mutate(zip_code2 = as.character(zip_code))
idx <- is.na(datos$zip_code)
datos$zip_code2[idx] <- datos$cred_zip_code[idx]
datos <- datos %>%
  mutate(zip_code = as.integer(gsub('\\D', '', zip_code2))) %>%
  select(NPAIS, NCANAL, NSUCURSAL, NFOLIO, zip_code)

datos <- datos %>%
  inner_join(unique(select(catalogo, NPAIS, NCANAL, NSUCURSAL, NFOLIO, HASH_MAP_CU,respuesta))) %>% 
  select(-NPAIS, -NCANAL, -NSUCURSAL, -NFOLIO) %>% 
  filter(complete.cases(HASH_MAP_CU))
idx <- datos$zip_code == "00000"
datos$zip_code[idx] <- NA


# Juntando datasets --------------------------------------------------

df2 <- df %>% 
  left_join(datos)

df <- df2;rm(df2)

cod_municipios <- read.table("CPdescarga.txt",sep="|",header = T) %>% 
  group_by(d_codigo,D_mnpio,c_mnpio,d_estado,c_estado) %>% 
  tally() %>% 
  select(-n)

df2 <- df %>% 
  mutate(zip_code=as.integer(zip_code)) %>% 
  left_join(cod_municipios,by=c("zip_code"="d_codigo"))

df <- df2;rm(df2)

saveRDS(df,"df_resumen_variables_transaccionales.RDS")

# Crimen ---------------------

crimen <- readRDS("base_con_indices.RDS")

df2 <- df %>% 
  inner_join(crimen,by=c("c_estado"="state_code" ,"c_mnpio"="mun_code"))

df <- df2;rm(df2)

saveRDS(df,"df_resumen_variables_transaccionales_crimen.RDS")

# Pobreza -----------------------------------------------------------------

pobreza <- readRDS("mapeo_AGEB_CP.RDS")

df2 <- df %>% 
  inner_join(pobreza,by=c("zip_code"="CP"))

df <- df2;rm(df2)

saveRDS(df,"df_resumen_variables_transaccionales_crimen_pobreza.RDS")


# Seleccionamos a las personas de captación ---------------------------------------------
# 
# capta <- read.table("Base_muestra1_500mil_salida.txt",sep="|",header = T)
# 
# prueba <- capta %>% 
#   group_by(NPAIS,NCANAL,NSUCURSAL,NFOLIO) %>% 
#   summarise(anio=max(ANIO_OPERA)) %>% 
#   filter(anio!=2013)
# 
# muestra <- sample(nrow(prueba),size = 60000)
# 
# muestra2 <- prueba[muestra,] %>% 
#   select(-anio)
# 
# write.table(muestra2,"codigos_capta.csv",sep=",",row.names = F,col.names = F)
# 
# rm(capta)

