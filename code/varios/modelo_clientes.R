library(dplyr)
library(randomForest)
library(caret)
library(doParallel)
library(kernlab)
library(corrplot)
library(gbm)

df <- readRDS("df_resumen_variables_transaccionales_crimen_pobreza.RDS") %>%
  select(-c(81,83:98,103:110,112)) %>% 
  mutate(respuesta = as.factor(respuesta))

df <- df[sample(nrow(df)),]

set.seed(108176)

df_entrenamiento <- df[sample(nrow(df),size = round(nrow(df)*.7,0)),]
df_prueba <- df %>% 
  anti_join(df_entrenamiento,by="HASH_MAP_CU")


# Selección de Variables --------------------------------------------------

cluster <- makeCluster(6)
registerDoParallel(cluster)
rf_model <- train(respuesta ~ .,
                data=select(df_entrenamiento,-HASH_MAP_CU),
                method="rf",
                importance = T,
                ntree=30,
                trControl=trainControl(method="cv",number=5),
                allowParallel=TRUE)
stopCluster(cluster)

saveRDS(rf_model,"Modelo_Bosque_1.RDS")

varImp(rf_model,scale=F)

rf_model <- readRDS("Modelo_Bosque_1.RDS")

pred_entrena <- predict(rf_model,select(df_entrenamiento,-HASH_MAP_CU,-respuesta))
df_entrenamiento$preds_rf <- round(pred_entrena ,0) 
confusionMatrix(df_entrenamiento$preds_rf,df_entrenamiento$respuesta)

pred_prueba <- predict(rf_model,select(df_prueba,-HASH_MAP_CU,-respuesta))
df_prueba$preds_rf <- round(pred_prueba,0)
confusionMatrix(df_prueba$preds_rf,df_prueba$respuesta)

# Bosque usando las variables seleccionadas ------------------------------------------

cluster <- makeCluster(6)
registerDoParallel(cluster)
rf2_model <- train(respuesta ~ .,
                data = select(df_entrenamiento, 
                            respuesta,             
                            num_trans_30,
                            num_trans_360,
                            coc_abono_retiro_90,
                            dias_dif_max_360,
                            coc_abono_retiro_180,
                            dias_dif_min_360,
                            dias_dif_min_30,
                            min_abono_360,
                            max_abono_360,
                            coc_abono_retiro_30,
                            med_abonos_360,
                            dias_dif_max_30,
                            num_retiros_90,
                            coc_max_abono_180,
                            num_abonos_180,
                            dimension_crimen,
                            max_abono_180,
                            med_abonos_30,
                            sum_abono_360,
                            coc_max_retiro_90),
                method="rf",
                importance = T,
                ntree=90,
                trControl=trainControl(method="cv",number=10),
                allowParallel=TRUE)
stopCluster(cluster)

saveRDS(rf2_model,"Modelo_Bosque_2.RDS")

varImp(rf2_model,scale=F)

rf2_model <- readRDS("Modelo_Bosque_2.RDS")

pred_entrena2 <- predict(rf2_model,select(df_entrenamiento,-HASH_MAP_CU,-respuesta))
df_entrenamiento$preds_rf2 <- round(pred_entrena2 ,0) 
confusionMatrix(df_entrenamiento$preds_rf2,df_entrenamiento$respuesta)

pred_prueba2 <- predict(rf2_model,select(df_prueba,-HASH_MAP_CU,-respuesta))
df_prueba$preds_rf2 <- round(pred_prueba2,0)
confusionMatrix(df_prueba$preds_rf2,df_prueba$respuesta)


# SVM ---------------------------------------------------------------------

svm_model <- ksvm(as.factor(respuesta) ~ ., 
                  data = select(df_entrenamiento, 
                                respuesta,             
                                num_trans_30,
                                num_trans_360,
                                coc_abono_retiro_90,
                                dias_dif_max_360,
                                coc_abono_retiro_180,
                                dias_dif_min_360,
                                dias_dif_min_30,
                                min_abono_360,
                                max_abono_360,
                                coc_abono_retiro_30,
                                med_abonos_360,
                                dias_dif_max_30,
                                num_retiros_90,
                                coc_max_abono_180,
                                num_abonos_180,
                                dimension_crimen,
                                max_abono_180,
                                med_abonos_30,
                                sum_abono_360,
                                coc_max_retiro_90), 
         cross=10)
 
saveRDS(svm_model,"Modelo_SVM.RDS")

svm_model <- readRDS("Modelo_SVM.RDS")

pred_entrena3 <- predict(svm_model,select(df_entrenamiento,-HASH_MAP_CU,-respuesta))
df_entrenamiento$preds_svm <- pred_entrena3
confusionMatrix(df_entrenamiento$preds_svm,df_entrenamiento$respuesta)

pred_prueba3 <- predict(svm_model,select(df_prueba,-HASH_MAP_CU,-respuesta))
df_prueba$preds_svm <- pred_prueba3
confusionMatrix(df_prueba$preds_svm,df_prueba$respuesta)


# Reg Logística -----------------------------------------------------------

modelo_log <- glm(respuesta ~ ., data = select(df_entrenamiento, 
                                               respuesta,             
                                               num_trans_30,
                                               num_trans_360,
                                               coc_abono_retiro_90,
                                               dias_dif_max_360,
                                               coc_abono_retiro_180,
                                               dias_dif_min_360,
                                               dias_dif_min_30,
                                               min_abono_360,
                                               max_abono_360,
                                               coc_abono_retiro_30,
                                               med_abonos_360,
                                               dias_dif_max_30,
                                               num_retiros_90,
                                               coc_max_abono_180,
                                               num_abonos_180,
                                               dimension_crimen,
                                               max_abono_180,
                                               med_abonos_30,
                                               sum_abono_360,
                                               coc_max_retiro_90),
                  family = binomial)

pred_prueba <- predict(modelo_log, newdata = select(df_prueba,           
                                                    num_trans_30,
                                                    num_trans_360,
                                                    coc_abono_retiro_90,
                                                    dias_dif_max_360,
                                                    coc_abono_retiro_180,
                                                    dias_dif_min_360,
                                                    dias_dif_min_30,
                                                    min_abono_360,
                                                    max_abono_360,
                                                    coc_abono_retiro_30,
                                                    med_abonos_360,
                                                    dias_dif_max_30,
                                                    num_retiros_90,
                                                    coc_max_abono_180,
                                                    num_abonos_180,
                                                    dimension_crimen,
                                                    max_abono_180,
                                                    med_abonos_30,
                                                    sum_abono_360,
                                                    coc_max_retiro_90),
                       type = 'response')

confusionMatrix(round(modelo_log$fitted.values), df_entrenamiento$respuesta)
confusionMatrix(round(pred_prueba), df_prueba$respuesta)

# GB ---------------------------------------------------------------------

model_gb <- train(respuesta ~ . , 
                  data = select(df_entrenamiento, 
                                respuesta,             
                                num_trans_30,
                                num_trans_360,
                                coc_abono_retiro_90,
                                dias_dif_max_360,
                                coc_abono_retiro_180,
                                dias_dif_min_360,
                                dias_dif_min_30,
                                min_abono_360,
                                max_abono_360,
                                coc_abono_retiro_30,
                                med_abonos_360,
                                dias_dif_max_30,
                                num_retiros_90,
                                coc_max_abono_180,
                                num_abonos_180,
                                dimension_crimen,
                                max_abono_180,
                                med_abonos_30,
                                sum_abono_360,
                                coc_max_retiro_90), 
                  method="gbm", 
                  distribution="bernoulli")

pred_prueba_gb <- predict(model_gb, newdata = select(df_prueba,           
                                                     num_trans_30,
                                                     num_trans_360,
                                                     coc_abono_retiro_90,
                                                     dias_dif_max_360,
                                                     coc_abono_retiro_180,
                                                     dias_dif_min_360,
                                                     dias_dif_min_30,
                                                     min_abono_360,
                                                     max_abono_360,
                                                     coc_abono_retiro_30,
                                                     med_abonos_360,
                                                     dias_dif_max_30,
                                                     num_retiros_90,
                                                     coc_max_abono_180,
                                                     num_abonos_180,
                                                     dimension_crimen,
                                                     max_abono_180,
                                                     med_abonos_30,
                                                     sum_abono_360,
                                                     coc_max_retiro_90))

saveRDS(model_gb,"Modelo_GB.RDS")

confusionMatrix(pred_prueba_gb, df_prueba$respuesta)
