library(dplyr)
library(data.table)
library(digest)

datos_orig <- readRDS("./Datos/crédito y captación.RDS")

datos_orig <- datos_orig %>%
  mutate(id = paste(
    NPAIS, 
    NCANAL, 
    NSUCURSAL,
    NFOLIO, 
    sep = "_")) %>%
  select(-NPAIS, -NCANAL, -NSUCURSAL, -NFOLIO)

# Anonymize function
anonymize <- function(x, algo="md5"){
  unq_hashes <- vapply(unique(x), function(object) digest(object, algo=algo), FUN.VALUE="", USE.NAMES=TRUE)
  unname(unq_hashes[x])
}

datos_encoded <- datos_orig %>%
  mutate(id = anonymize(id))


saveRDS(datos_encoded, file = "./Datos/credit_and_savings_encoded.RDS")

write.csv(datos_encoded, file = "./Datos/credit_and_savings_encoded.csv", row.names = FALSE)



