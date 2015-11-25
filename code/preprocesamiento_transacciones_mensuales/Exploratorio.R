library(dplyr)
library(tidyr)
library(ggplot2)

train <- readRDS("data/train_sample.RDS")

train %>% group_by(FIID_CLIENTE_UNICO) %>% tally() %>% ungroup() %>% 
  rename(num_de_registros_por_cliente = n) %>% 
  group_by(num_de_registros_por_cliente) %>% 
  summarise(num_clientes = n()) %>% 
  ggplot() + 
  geom_bar(aes(x = num_de_registros_por_cliente, y = num_clientes), stat='identity')



train_num_meses <- train %>% 
  group_by(FIID_CLIENTE_UNICO) %>% 
  summarise(
    Num_meses_registrados = n())

train_suma <- train %>%
  select(-Credito, -ANIO_OPERA, -MES_OPERA, -NPAIS, -NCANAL, -NSUCURSAL, -NFOLIO) %>%
  group_by(FIID_CLIENTE_UNICO) %>%
  summarise_each(funs(sum))
names(train_suma) <- c("FIID_CLIENTE_UNICO", paste0("Suma_", names(train_suma)[-1]))

numerador = seq(3, 25, 2)
denominador = seq(2, 24, 2)

train_mean <- as.data.frame(train_suma[,numerador]/train_suma[,denominador])
train_mean[is.na(train_mean)] <- 0
names(train_mean) <- paste0("Mean", substr(names(train_mean), 5, 100))
train_mean$FIID_CLIENTE_UNICO <- train_suma$FIID_CLIENTE_UNICO

# train_mean <- train %>%
#   select(-Credito, -ANIO_OPERA, -MES_OPERA, -NPAIS, -NCANAL, -NSUCURSAL, -NFOLIO, -ABONO_VENTANILLA, -RETIRO_VENTANILLA, -RETIRO_ATM_PROPIO, -RETIRO_ATM_AJENO, -ABONO_TEF, -RETIRO_TEF, -ABONO_CUENTAS_BAZ, -RETIRO_CUENTAS_BAZ, -ABONO_SPEI, -RETIRO_SPEI, -ABONO_PARED, -RETIRO_PARED) %>%
#   group_by(FIID_CLIENTE_UNICO) %>%
#   summarise_each(funs(mean))
# names(train_mean) <- c("FIID_CLIENTE_UNICO", paste0("Mean_", names(train_mean)[-1]))

train_summary <- train_suma %>%
  left_join(train_num_meses) %>%
  left_join(train_mean) %>%
  left_join(unique(select(train, FIID_CLIENTE_UNICO, Credito)))
  
sapply(train_summary, summary)  

train_summary %>%
  group_by(Credito) %>%
  summarise(Suma_MONTO_ABONO_VENTANILLA = sum(Suma_MONTO_ABONO_VENTANILLA)) %>%
  ggplot() +
  geom_bar(aes(x = factor(Credito), y = Suma_MONTO_ABONO_VENTANILLA), stat = 'identity')
  
train_summary %>%
  group_by(Credito) %>%
  summarise(Suma_MONTO_RETIRO_VENTANILLA = sum(abs(Suma_MONTO_RETIRO_VENTANILLA))) %>%
  ggplot() +
  geom_bar(aes(x = factor(Credito), y = Suma_MONTO_RETIRO_VENTANILLA), stat = 'identity')

train_summary %>%
  group_by(Credito) %>%
  summarise(Prom_MONTO_RETIRO_VENTANILLA = mean(abs(Mean_MONTO_RETIRO_VENTANILLA))) %>%
  ggplot() +
  geom_bar(aes(x = factor(Credito), y = Prom_MONTO_RETIRO_VENTANILLA), stat = 'identity')

saveRDS(train_summary, file = "./data/train_summary.RDS")


test <- readRDS("./data/test_sample.RDS")

test %>% group_by(FIID_CLIENTE_UNICO) %>% tally() %>% ungroup() %>% 
  rename(num_de_registros_por_cliente = n) %>% 
  group_by(num_de_registros_por_cliente) %>% 
  summarise(num_clientes = n()) %>% 
  ggplot() + 
  geom_bar(aes(x = num_de_registros_por_cliente, y = num_clientes), stat='identity')



test_num_meses <- test %>% 
  group_by(FIID_CLIENTE_UNICO) %>% 
  summarise(
    Num_meses_registrados = n())

test_suma <- test %>%
  select(-Credito, -ANIO_OPERA, -MES_OPERA, -NPAIS, -NCANAL, -NSUCURSAL, -NFOLIO) %>%
  group_by(FIID_CLIENTE_UNICO) %>%
  summarise_each(funs(sum))
names(test_suma) <- c("FIID_CLIENTE_UNICO", paste0("Suma_", names(test_suma)[-1]))

numerador = seq(3, 25, 2)
denominador = seq(2, 24, 2)

test_mean <- as.data.frame(test_suma[,numerador]/test_suma[,denominador])
test_mean[is.na(test_mean)] <- 0
names(test_mean) <- paste0("Mean", substr(names(test_mean), 5, 100))
test_mean$FIID_CLIENTE_UNICO <- test_suma$FIID_CLIENTE_UNICO

test_summary <- test_suma %>%
  left_join(test_num_meses) %>%
  left_join(test_mean) %>%
  left_join(unique(select(test, FIID_CLIENTE_UNICO, Credito)))

saveRDS(test_summary, file = "./data/test_summary.RDS")
