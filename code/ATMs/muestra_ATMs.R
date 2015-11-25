library(readxl)
library(dplyr)

set.seed(124362)

datos <- read_excel("../../data/ATMs/Base cajeros cierre semana 47.xlsx", sheet = "ok")

datos2 <- datos %>%
  filter(Estado %in% c("DISTRITO FEDERAL", "ESTADO DE MEXICO", "NUEVO LEON", "JALISCO", "GUANAJUATO"))

prop_datos <- datos2 %>% 
  group_by(Estado, `Municipio ó Delegación`) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(Estado) %>% 
  mutate(n2 = sum(n), prop = n/n2)

muestra <- sample_n(datos2, 200)

prop_muestra <- muestra %>% 
  group_by(Estado, `Municipio ó Delegación`) %>% 
  summarise(n_muestra = n()) %>% 
  ungroup() %>% 
  group_by(Estado) %>% 
  mutate(n2_muestra = sum(n_muestra), prop_muestra = n_muestra/n2_muestra)

prop_datos_muestra <- prop_datos %>% left_join(prop_muestra)

name = paste0("../../output/ATMs/muestra_", nrow(muestra), "_ATMs.csv")
write.csv(muestra, file = name, row.names = FALSE)
