############################################################################################
# HACIENDO ESTO NOS DIMOS CUENTA QUE EL ERROR EN LA PRUEBA ESTABA EN TOMAR EL MÍNIMO DE LA #
# FECHA DE PRIMER SURTIMIENTO                                                              #
############################################################################################

library(dplyr)

transac <- read.table("join_trans_prueba_sept2.csv",header = F,sep=",",stringsAsFactors = F)
names(transac) <- c("HASH_MAP_CU","T071_AMOUNT","T071_DAT_OPERATION","FECHA_PRIM_SURT")

transac <-  transac%>% 
  #Aquí hay un error queremos que la fecha de surtimiento sea la máxima...no la mínima
  mutate(FECHA_PRIM_SURT2 = as.Date(min(FECHA_PRIM_SURT), format = "%Y-%m-%d"),
         T071_DAT_OPERATION = as.Date(T071_DAT_OPERATION, format = "%Y-%m-%d"))

#Hacemos un df con la persona, su fecha de surtimiento y todas sus fechas de transacciones en una 
#ventana de 30 días para revisar porqué hay personas q no tienen transacciones en los 30 días
rev1 <- transac %>% 
  mutate(dias_dif=difftime(FECHA_PRIM_SURT2, T071_DAT_OPERATION, units = 'days')) %>% 
  group_by(HASH_MAP_CU) %>% 
  mutate(num_trans = n())

saveRDS(rev1,"Rev1.RDS")
