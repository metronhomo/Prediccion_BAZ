#Convierte la columna de fecha de la tabla prim_surt_cred de formato DD/MM/YY en YYYY-MM-DD

library(RSQLite)

con = dbConnect(drv = SQLite(), dbname = "BAZ.db")
prim_surt_cred <- dbGetQuery(con, "SELECT * FROM prim_surt_cred")
prim_surt_cred$FECHA_PRIM_SURT <- as.character(as.Date(prim_surt_cred$FECHA_PRIM_SURT, format = "%d/%m/%y"))
dbWriteTable(con, "prim_surt_cred2", prim_surt_cred)
