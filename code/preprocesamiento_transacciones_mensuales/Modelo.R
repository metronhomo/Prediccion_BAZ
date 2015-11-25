# Modelo
library(caret)

train <- readRDS("./data/train_summary.RDS")
test <- readRDS("./data/test_summary.RDS")

mylogit <- glm(Credito ~ Suma_ABONO_VENTANILLA + Suma_MONTO_ABONO_VENTANILLA + Suma_RETIRO_VENTANILLA + Suma_MONTO_RETIRO_VENTANILLA + Suma_RETIRO_ATM_PROPIO + Suma_MONTO_RETIRO_ATM_PROPIO + Suma_RETIRO_ATM_AJENO + Suma_MONTO_RETIRO_ATM_AJENO + Suma_ABONO_TEF + Suma_MONTO_ABONO_TEF + Suma_RETIRO_TEF + Suma_MONTO_RETIRO_TEF + Suma_ABONO_CUENTAS_BAZ + Suma_MONTO_ABONO_CUENTAS_BAZ + Suma_RETIRO_CUENTAS_BAZ + Suma_MONTO_RETIRO_CUENTAS_BAZ + Suma_ABONO_SPEI + Suma_MONTO_ABONO_SPEI + Suma_RETIRO_SPEI + Suma_MONTO_RETIRO_SPEI + Suma_ABONO_PARED + Suma_MONTO_ABONO_PARED + Num_meses_registrados + Mean_MONTO_ABONO_VENTANILLA + Mean_MONTO_RETIRO_VENTANILLA + Mean_MONTO_RETIRO_ATM_PROPIO + Mean_MONTO_RETIRO_ATM_AJENO + Mean_MONTO_ABONO_TEF + Mean_MONTO_RETIRO_TEF + Mean_MONTO_ABONO_CUENTAS_BAZ + Mean_MONTO_RETIRO_CUENTAS_BAZ + Mean_MONTO_ABONO_SPEI + Mean_MONTO_RETIRO_SPEI + Mean_MONTO_ABONO_PARED , 
               data = train, family = "binomial")

test2 <- test %>% select(Suma_ABONO_VENTANILLA, Suma_MONTO_ABONO_VENTANILLA, Suma_RETIRO_VENTANILLA, Suma_MONTO_RETIRO_VENTANILLA, Suma_RETIRO_ATM_PROPIO, Suma_MONTO_RETIRO_ATM_PROPIO, Suma_RETIRO_ATM_AJENO, Suma_MONTO_RETIRO_ATM_AJENO, Suma_ABONO_TEF, Suma_MONTO_ABONO_TEF, Suma_RETIRO_TEF, Suma_MONTO_RETIRO_TEF, Suma_ABONO_CUENTAS_BAZ, Suma_MONTO_ABONO_CUENTAS_BAZ, Suma_RETIRO_CUENTAS_BAZ, Suma_MONTO_RETIRO_CUENTAS_BAZ, Suma_ABONO_SPEI, Suma_MONTO_ABONO_SPEI, Suma_RETIRO_SPEI, Suma_MONTO_RETIRO_SPEI, Suma_ABONO_PARED, Suma_MONTO_ABONO_PARED, Num_meses_registrados, Mean_MONTO_ABONO_VENTANILLA, Mean_MONTO_RETIRO_VENTANILLA, Mean_MONTO_RETIRO_ATM_PROPIO, Mean_MONTO_RETIRO_ATM_AJENO, Mean_MONTO_ABONO_TEF, Mean_MONTO_RETIRO_TEF, Mean_MONTO_ABONO_CUENTAS_BAZ, Mean_MONTO_RETIRO_CUENTAS_BAZ, Mean_MONTO_ABONO_SPEI, Mean_MONTO_RETIRO_SPEI, Mean_MONTO_ABONO_PARED)

pred <- predict(mylogit, newdata = test2, type = "response")
pred2 <- as.integer(pred>0.5)

mean(test$Credito == pred2)

confusionMatrix(pred2, test$Credito)

# Encontrado en: http://stats.stackexchange.com/questions/25389/obtaining-predicted-values-y-1-or-0-from-a-logistic-regression-model-fit
perf = function(cut, mod, y)
{
  yhat = (mod$fit>cut)
  w = which(y==1)
  sensitivity = mean( yhat[w] == 1 ) 
  specificity = mean( yhat[-w] == 0 ) 
  c.rate = mean( y==yhat ) 
  d = cbind(sensitivity,specificity)-c(1,1)
  d = sqrt( d[1]^2 + d[2]^2 ) 
  out = t(as.matrix(c(sensitivity, specificity, c.rate,d)))
  colnames(out) = c("sensitivity", "specificity", "c.rate", "distance")
  return(out)
}

s = seq(.01,.99,length=1000)
OUT = matrix(0,1000,4)
for(i in 1:1000) OUT[i,]=perf(s[i],mylogit,test$Credito)
plot(s,OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
lines(s,OUT[,4],col="darkred",lwd=2)
box()
legend(0,.25,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Classification Rate","Distance"))
