# Necesita el archivo de la base de datos de SQLite (BAZ.db)

library(RSQLite)
library(dplyr)

con = dbConnect(drv = SQLite(), dbname = "/media/mario/HD1/Dorado/BAZ.db")
prim_surt <- dbGetQuery(con, "SELECT 
                       NPAIS, NCANAL, NSUCURSAL, NFOLIO, FECHA_PRIM_SURT 
                       FROM prim_surt_cred;") %>% 
  mutate(FECHA_PRIM_SURT = as.Date(FECHA_PRIM_SURT, format = "%Y-%m-%d"))
dbDisconnect(con)

saveRDS(prim_surt, "../../output/Varios/catalogo_primer_surtimiento.RDS")
