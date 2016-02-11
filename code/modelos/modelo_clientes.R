library(dplyr)
library(randomForest)
library(caret)
library(doParallel)
library(kernlab)
library(corrplot)
library(gbm)
library(ROCR)
library(ggplot2)

df <- readRDS("df_resumen_variables_transaccionales_crimen_pobreza.RDS") %>%
  select(-c(81,83:98,103:110,112)) %>% 
  mutate(respuesta = as.factor(respuesta))

df <- df[sample(nrow(df)),]

set.seed(108176)

df_entrenamiento <- df[sample(nrow(df),size = round(nrow(df)*.7,0)),]
df_prueba <- df %>% 
  anti_join(df_entrenamiento,by="HASH_MAP_CU")

saveRDS(df_entrenamiento,"Entrenamiento.RDS")
saveRDS(df_prueba,"Prueba.RDS")

df2 <- df_prueba %>% 
  select(2:81) %>% 
  mutate(respuesta = as.numeric(respuesta))

corrs <-  cor(df2,use="complete.obs",method="spearman")
library(corrplot)

corrplot(corrs,tl.cex=.5,tl.col="black",order="AOE")
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

rf_model <- readRDS("Modelo_Bosque_1.RDS")
x <- varImp(rf_model,scale=F)

pred_entrena <- predict(rf_model,select(df_entrenamiento,-HASH_MAP_CU,-respuesta))
confusionMatrix(pred_entrena,df_entrenamiento$respuesta)

pred_prueba <- predict(rf_model,select(df_prueba,-HASH_MAP_CU,-respuesta))
confusionMatrix(pred_prueba,df_prueba$respuesta)

# Bosque usando las variables seleccionadas ------------------------------------------

cluster <- makeCluster(6)
registerDoParallel(cluster)
rf2_model <- train(as.factor(respuesta) ~ .,
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
                            #dimension_crimen,
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

rf2_model <- readRDS("Modelo_Bosque_2.RDS")
varImp(rf2_model,scale=F)

pred_entrena2 <- predict(rf2_model,select(df_entrenamiento,-HASH_MAP_CU,-respuesta))
confusionMatrix(pred_entrena2,df_entrenamiento$respuesta)

pred_prueba2 <- predict(rf2_model,select(df_prueba,-HASH_MAP_CU,-respuesta))
confusionMatrix(pred_prueba2,df_prueba$respuesta)


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
                  prob.model  = T, 
                  C = 5,
                  cross=10)
 
saveRDS(svm_model,"Modelo_SVM.RDS")

svm_model <- readRDS("Modelo_SVM.RDS")

pred_entrena3 <- predict(svm_model,select(df_entrenamiento,-HASH_MAP_CU,-respuesta))
confusionMatrix(pred_entrena3,df_entrenamiento$respuesta)

pred_prueba3 <- predict(svm_model, select(df_prueba, 
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
                        type = "prob")

confusionMatrix(pred_prueba3,df_prueba$respuesta)


# Reg Logística -----------------------------------------------------------

log_model <- glm(respuesta ~ ., data = select(df_entrenamiento, 
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

pred_prueba4 <- predict(modelo_log, newdata = select(df_prueba,           
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

saveRDS(log_model,"Modelo_reg_log.RDS")

confusionMatrix(round(pred_prueba4), df_prueba$respuesta)

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

# Función para hacer curva ROC en ggplot ----------------------------------

rocdata <- function(grp, pred){
  # Produces x and y co-ordinates for ROC curve plot
  # Arguments: grp - labels classifying subject status
  #            pred - values of each observation
  # Output: List with 2 components:
  #         roc = data.frame with x and y co-ordinates of plot
  #         stats = data.frame containing: area under ROC curve, p value, upper and lower 95% confidence interval
  
  grp <- as.factor(grp)
  if (length(pred) != length(grp)) {
    stop("The number of classifiers must match the number of data points")
  } 
  
  if (length(levels(grp)) != 2) {
    stop("There must only be 2 values for the classifier")
  }
  
  cut <- unique(pred)
  tp <- sapply(cut, function(x) length(which(pred > x & grp == levels(grp)[2])))
  fn <- sapply(cut, function(x) length(which(pred < x & grp == levels(grp)[2])))
  fp <- sapply(cut, function(x) length(which(pred > x & grp == levels(grp)[1])))
  tn <- sapply(cut, function(x) length(which(pred < x & grp == levels(grp)[1])))
  tpr <- tp / (tp + fn)
  fpr <- fp / (fp + tn)
  roc = data.frame(x = fpr, y = tpr)
  roc <- roc[order(roc$x, roc$y),]
  
  i <- 2:nrow(roc)
  auc <- (roc$x[i] - roc$x[i - 1]) %*% (roc$y[i] + roc$y[i - 1])/2
  
  pos <- pred[grp == levels(grp)[2]]
  neg <- pred[grp == levels(grp)[1]]
  q1 <- auc/(2-auc)
  q2 <- (2*auc^2)/(1+auc)
  se.auc <- sqrt(((auc * (1 - auc)) + ((length(pos) -1)*(q1 - auc^2)) + ((length(neg) -1)*(q2 - auc^2)))/(length(pos)*length(neg)))
  ci.upper <- auc + (se.auc * 0.96)
  ci.lower <- auc - (se.auc * 0.96)
  
  se.auc.null <- sqrt((1 + length(pos) + length(neg))/(12*length(pos)*length(neg)))
  z <- (auc - 0.5)/se.auc.null
  p <- 2*pnorm(-abs(z))
  
  stats <- data.frame (auc = auc,
                       p.value = p,
                       ci.upper = ci.upper,
                       ci.lower = ci.lower
  )
  
  return (list(roc = roc, stats = stats))
}

# Curva ROC ---------------------------------------------------------------
rf_pred <- predict(rf2_model,select(df_prueba,-HASH_MAP_CU,-respuesta),type="prob")
rf_prob <- rf_pred[,2]
plotdata_rf <- rocdata(df_prueba$respuesta, rf_prob)

log_pred <- predict(log_model,select(df_prueba,-HASH_MAP_CU,-respuesta),type="response")
log_prob <- log_pred
plotdata_log <- rocdata(df_prueba$respuesta, log_prob)

svm_pred <- predict(svm_model,select(df_prueba,-HASH_MAP_CU,-respuesta),type="prob")
svm_prob <- svm_pred[,2]
plotdata_svm <- rocdata(df_prueba$respuesta, svm_prob)

gb_pred <- predict(gb_model,select(df_prueba,-HASH_MAP_CU,-respuesta),type="prob")
gb_prob <- gb_pred[,2]
plotdata_gb <- rocdata(df_prueba$respuesta, gb_prob)

# Gráfica con la curva ROC ------------------------------------------------

graf_roc <- ggplot() +
  geom_line(data=plotdata_rf$roc, aes(x = x, y = y,colour = "Random Forest"),size=.8) +
  geom_line(data=plotdata_gb$roc, aes(x = x, y = y,colour = "Gradient Boosting"),size=.8) +
  geom_line(data=plotdata_log$roc, aes(x = x, y = y,colour = "Logistic Regression"),size=.8) +
  geom_line(data=plotdata_svm$roc, aes(x = x, y = y,colour = "SVM"),size=.8) +
  geom_abline (intercept = 0, slope = 1,size=1.5) +
  scale_x_continuous("False Positive Rate (1-Specificity)") +
  scale_y_continuous("True Positive Rate (Sensitivity)") +
  scale_colour_manual(name="Modelos",
                      values = c("Random Forest"="brown1",
                                 "Logistic Regression"="darkorchid4",
                                 "Gradient Boosting"="aquamarine4",
                                 "SVM" = "darkgoldenrod2")) +
  ggtitle("Desempeño de Modelos") + 
  theme(
    axis.text.x=element_text(size=22, colour = 'black'),
    axis.title.x=element_text(size=22, colour = 'black'),
    axis.text.y=element_text(size=22, colour = 'black'),
    axis.title.y=element_text(size=22, colour = 'black'),
    plot.title=element_text(size=22, colour = 'black'),
    panel.background=element_rect(fill='grey95'),
    strip.background=element_rect(fill="#2c3e50"),
    panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
    strip.text.x = element_text(colour = 'white', size = 22),
    strip.text.y = element_text(colour = 'white', size = 22),
    legend.text=element_text(size=20),
    legend.title=element_text(size=20),
    panel.grid.major = element_line(colour = 'grey70', linetype = 'dashed'),
    panel.grid.minor = element_line(colour = 'grey70', linetype = 'dashed'))

saveRDS(graf_roc,"Graf_curva_roc_4_modelos.RDS")

