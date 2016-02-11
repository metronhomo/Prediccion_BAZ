# Comprueba que del catalogo de clientes de septiembre con los que se hace la prueba del modelo
# no esten en el catalogo de clientes de credito

library(RSQLite)
library(dplyr)

catalogo_capta_sept <- read.csv("./catalogo_no_atipicos.csv", header = F)

con = dbConnect(drv = SQLite(), dbname = "BAZ_cp.db")
catalogo_credito <- dbGetQuery(con, 
  "SELECT NPAIS, NCANAL, NSUCURSAL, NFOLIO from prim_surt_cred")
dbDisconnect(con)

names(catalogo_capta_sept) <- names(catalogo_credito)

vector_credito <- paste(catalogo_credito$NPAIS, 
                         catalogo_credito$NCANAL, 
                         catalogo_credito$NSUCURSAL, 
                         catalogo_credito$NFOLIO,
                         sep = '_')

vector_capta_sep <- paste(catalogo_capta_sept$NPAIS, 
                         catalogo_capta_sept$NCANAL, 
                         catalogo_capta_sept$NSUCURSAL, 
                         catalogo_capta_sept$NFOLIO,
                         sep = '_')

sum(vector_capta_sep %in% vector_credito)


