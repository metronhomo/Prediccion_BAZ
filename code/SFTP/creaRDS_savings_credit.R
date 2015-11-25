library(dplyr)

datos_orig1 <- read.csv("./Datos/SALDOS_ACUMULADOS_MENSUALES_500MIL_UNO.txt")
datos_orig2 <- read.csv("./Datos/SALDOS_ACUMULADOS_MENSUALES_500MIL_DOS.txt")
credit_original <- read.csv("./Datos/SALDOS_ACUMULADOS_MENSUALES_800MIL.txt")

savings_original <- rbind(datos_orig1, datos_orig2) %>%
  mutate(id = paste(
    FIPAIS, 
    FICANAL, 
    FISUCURSAL,
    FIFOLIO, 
    sep = "_")) %>%
  select(-FIPAIS, -FICANAL, -FISUCURSAL, -FIFOLIO)

rm(datos_orig1)
rm(datos_orig2)

# savings_original$id <- paste(sprintf("%02d", savings_original$FIPAIS), sprintf("%02d", savings_original$FICANAL), sprintf("%04d", savings_original$FISUCURSAL), sprintf("%06d", savings_original$FIFOLIO), sep = "")
# savings_original$id <- paste(savings_original$FIPAIS, savings_original$FICANAL, savings_original$FISUCURSAL, savings_original$FIFOLIO, sep = "_")

credit_original <- credit_original %>%
  mutate(id = paste(
    FIPAIS, 
    FICANAL, 
    FISUCURSAL,
    FIFOLIO, 
    sep = "_")) %>%
  select(-FIPAIS, -FICANAL, -FISUCURSAL, -FIFOLIO)

saveRDS(savings_original, file = "./Datos/savings_original.RDS")
saveRDS(credit_original, file = "./Datos/credit_original.RDS")
