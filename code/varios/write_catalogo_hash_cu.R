# Requiere la base de datos BAZ.db

library(RSQLite)
library(readr)

con = dbConnect(drv = SQLite(), dbname = "BAZ_cp.db")

catalogo_hash <- dbGetQuery(con, "SELECT distinct
                       Pais, Canal, Sucursal, Folio, HASH_MAP_CU
                       from catalogo_hash")
dbDisconnect(con)

write_csv(catalogo_hash, 'catalogo_hash_unique.csv')
