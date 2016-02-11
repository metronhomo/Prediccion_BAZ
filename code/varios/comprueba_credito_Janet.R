library(RSQLite)
library(readxl)
library(dplyr)

con = dbConnect(drv = SQLite(), dbname = "BAZ_cp.db")
catalogo_credito <- dbGetQuery(con, 
                               "SELECT NPAIS, NCANAL, NSUCURSAL, NFOLIO from prim_surt_cred")
dbDisconnect(con)

datos_j <- read_excel("./Informacion_Ctes_noAtipicos_sociodemo.xlsx")
catalogo_capta_sept <- read.csv("./catalogo_no_atipicos.csv", header = F)

vector_claves_cat <- paste(catalogo_credito$NPAIS, 
                           catalogo_credito$NCANAL, 
                           catalogo_credito$NSUCURSAL, 
                           catalogo_credito$NFOLIO,
                           sep = '_') %>% 
  sort()

vector_claves_j <- paste(datos_j$NPAIS, 
                           datos_j$NCANAL, 
                           datos_j$NSUCURSAL, 
                           datos_j$NFOLIO,
                           sep = '_') %>% 
  sort()

vector_capta_sep <- paste(catalogo_capta_sept$NPAIS, 
                          catalogo_capta_sept$NCANAL, 
                          catalogo_capta_sept$NSUCURSAL, 
                          catalogo_capta_sept$NFOLIO,
                          sep = '_') %>% 
  sort()

sum(vector_claves_cat %in% vector_claves_j)
sum(vector_claves_j %in% vector_claves_cat)
sum(vector_capta_sep %in% vector_claves_j)
sum(vector_claves_j %in% vector_capta_sep)
sum(vector_claves_cat %in% vector_capta_sep)


