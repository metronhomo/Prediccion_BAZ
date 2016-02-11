datos <- readRDS("../../output/SFTP/crédito y captación.RDS")
names(datos) <- gsub(" ", "_", names(datos))

datos$Credito <- as.integer(is.na(datos$zip_code))
datos$zip_code2 <- as.character(datos$zip_code)
idx <- is.na(datos$zip_code)
datos$zip_code2[idx] <- datos$cred_zip_code[idx]
datos <- datos %>%
  mutate(zip_code = substr(zip_code2, 2, 6)) %>%
  select(-zip_code2, -cred_zip_code)
idx <- datos$zip_code == "00000"
datos$zip_code[idx] <- NA
