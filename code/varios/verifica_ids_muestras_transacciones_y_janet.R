#Verifica que los ids de la muestra transaccional mensual (1,434,148 clientes) estén en la base con 70 variables (1,859,441 clientes)
# (Sí están)

library(dplyr)

df <- read.table("../../output/Preprocesamiento/datos.txt",
                 sep = "|",
                 col.names = c(
                   "Credito",
                   "ANIO_OPERA",
                   "MES_OPERA",
                   "NPAIS",
                   "NCANAL",
                   "NSUCURSAL",
                   "NFOLIO",
                   "FIID_CLIENTE_UNICO",
                   "ABONO_VENTANILLA",
                   "MONTO_ABONO_VENTANILLA",
                   "RETIRO_VENTANILLA",
                   "MONTO_RETIRO_VENTANILLA",
                   "RETIRO_ATM_PROPIO",
                   "MONTO_RETIRO_ATM_PROPIO",
                   "RETIRO_ATM_AJENO",
                   "MONTO_RETIRO_ATM_AJENO",
                   "ABONO_TEF",
                   "MONTO_ABONO_TEF",
                   "RETIRO_TEF",
                   "MONTO_RETIRO_TEF",
                   "ABONO_CUENTAS_BAZ",
                   "MONTO_ABONO_CUENTAS_BAZ",
                   "RETIRO_CUENTAS_BAZ",
                   "MONTO_RETIRO_CUENTAS_BAZ",
                   "ABONO_SPEI",
                   "MONTO_ABONO_SPEI",
                   "RETIRO_SPEI",
                   "MONTO_RETIRO_SPEI",
                   "ABONO_PARED",
                   "MONTO_ABONO_PARED",
                   "RETIRO_PARED",
                   "MONTO_RETIRO_PARED"
                 ),
                 colClasses = c(
                   "integer",
                   "integer",
                   "integer",
                   "integer",
                   "integer",
                   "integer",
                   "integer",
                   "integer",
                   "integer",
                   "numeric",
                   "integer",
                   "numeric",
                   "integer",
                   "numeric",
                   "integer",
                   "numeric",
                   "integer",
                   "numeric",
                   "integer",
                   "numeric",
                   "integer",
                   "numeric",
                   "integer",
                   "numeric",
                   "integer",
                   "numeric",
                   "integer",
                   "numeric",
                   "integer",
                   "numeric",
                   "integer",
                   "numeric"
                 ))

df2 <- df %>% select(FIID_CLIENTE_UNICO, NPAIS, NCANAL, NSUCURSAL, NFOLIO) %>% unique()

rm(df)

ids1 <- paste(sprintf("%02d", df2$NPAIS), 
                sprintf("%02d", df2$NCANAL), 
                sprintf("%04d", df2$NSUCURSAL), 
                sprintf("%06d", df2$NFOLIO), 
                sep = "")



datos <- readRDS("../../output/SFTP/crédito y captación.RDS")
ids2 <- paste(sprintf("%02d", datos$NPAIS), 
                  sprintf("%02d", datos$NCANAL), 
                  sprintf("%04d", datos$NSUCURSAL), 
                  sprintf("%06d", datos$NFOLIO), 
                  sep = "")

ids1 %in% ids2 %>% sum()

