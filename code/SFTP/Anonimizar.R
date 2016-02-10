# Source: http://www.r-bloggers.com/data-anonymization-in-r/

library(data.table)
library(digest)
library(dplyr)


# Anonymize function
anonymize <- function(x, algo="md5"){
  unq_hashes <- vapply(unique(x), function(object) digest(object, algo=algo), FUN.VALUE="", USE.NAMES=TRUE)
  unname(unq_hashes[x])
}

savings_original <- readRDS(file = "./Datos/savings_original.RDS")
credit_original <- readRDS(file = "./Datos/credit_original.RDS")

savings_encoded <- savings_original %>%
  mutate(id = anonymize(id))

rm(savings_original)

credit_encoded <- credit_original %>%
  mutate(id = anonymize(id))

rm(credit_original)

saveRDS(savings_encoded, file = "./Datos/savings_encoded.RDS")
saveRDS(credit_encoded, file = "./Datos/credit_encoded.RDS")

write.csv(savings_encoded, file = "./Datos/savings_encoded.csv", row.names = FALSE)
write.csv(credit_encoded, file = "./Datos/credit_encoded.csv", row.names = FALSE)



