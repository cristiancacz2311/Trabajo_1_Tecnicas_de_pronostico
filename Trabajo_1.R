{r RStyle, echo=TRUE}
knitr::opts_chunk$set(echo = F, fig.align = "center",
                      fig.height = 4.5, fig.pos = "H")

library(tidyverse)
library(tseries)
library(dplyr)
library(gridExtra)
library(tidyr)
library(cowplot)
library(kableExtra)
library(GGally)
library(knitr)
library(strucchange)
library(Metrics)
library(forecast)
library(Metrics)

codensa = read.table("codensa.diaria.dat", header = TRUE, stringsAsFactors = FALSE) 
codensa$date <- as.Date(codensa$date)

Crear un gráfico de la serie de tiempo usando ggplot2 con un color personalizado (por ejemplo, rojo)
ggplot(data = codensa, aes(x = date, y = y)) +
  geom_line(color = "458B74") +   Cambia "red" al color que prefieras
labs(x = "Fecha", y = "Valores") +
  theme_minimal() +
  ggtitle("Serie de Tiempo de Codensa")

summary(codensa)

medidas = function(m,y,k){
  m = objeto producido con lm()
  y = variable dependiente
  k = número de coeficientes beta
  T = length(y)
  yest = fitted(m)
  sse = sum((yest-y)^2)
  ssr = sum((y-mean(y))^2) 
  mse = sse/(T-k)
  R2 = 1 - sse/ssr
  Ra2 = 1 - (T-1)*(1-R2)/(T-k)
  aic = log((T-k)*exp(2*k/T)*mse/T)
  bic = log(T^(k/T)*(T-k)*mse/T)
  M = c(mse,sqrt(mse),Ra2,  aic, bic)
  names(M) = c("mse","rmse","R2-ad","log.aic","log.bic")
  return(M)
}

medidas.hw = function(y,yest,k){
  y = serie, m = modelo, k = numero parametros
  T = length(y)
  sse = sum((yest-y)^2)
  ssr = sum((y-mean(y))^2) 
  mse = sse/(T-k)
  R2 = 1 - sse/ssr
  Ra2 = 1 - (T-1)*(1-R2)/(T-k)
  aic = log((T-k)*exp(2*k/T)*mse/T)
  bic = log(T^(k/T)*(T-k)*mse/T)
  
  M = c(mse,sqrt(mse),Ra2,  aic, bic)
  names(M) = c("mse","rmse","R2-ad","log.aic","log.bic")
  return(M)
}

codensa_1<-ts(codensa$y, frequency = 7, start = c(1995,7))
m<-15
n<-length(codensa_1)-m
t<-1:n
yf=ts(codensa_1[(n-m+1):n], frequency=7)
T=length(codensa_1)

codensa_y<-ts(codensa_1[t], freq=7, start = c(1995,7))
It<-seasonaldummy(codensa_y)

modelo_cuadratico <- lm(codensa_y ~ t + I(t^2) + It)

summary(modelo_cuadratico)

medidas(modelo_cuadratico,codensa_y, 8)

modelo_cubico <- lm(codensa_y ~ t + I(t^2) + I(t^3)+ It)
summary(modelo_cubico)

medidas(modelo_cubico,codensa_y, 8)

modelo_exponencial <- lm(log(codensa_y) ~ t+ It)

summary(modelo_exponencial)

medidas(modelo_exponencial,log(codensa_y), 7)

modelo_exponencial_cuad<-lm(log(codensa_y) ~ t + I(t^2)+ It)
summary(modelo_exponencial_cuad)

medidas(modelo_exponencial_cuad,log(codensa_y), 8)

Convertimos los datos en un objeto tipo ts
train_data <- codensa[1:(nrow(codensa) - 15), ]

y = ts(train_data$y,frequency=7,start=c(1955,07))
modelo_holt<- hw(y, damped=TRUE )
summary(modelo_holt)
yhat_holt<-modelo_holt$model$fitted

medidas.hw(y,yhat_holt,5)

modelo_neuronal= nnetar( y,lambda =0 )
print (modelo_neuronal)
yhat_nnar = fitted(modelo_neuronal)
yhat_nnar = na.omit(yhat_nnar)

NNAR(28,1,14)[12]: 
  q=12  nodos en la capa oculta, 
p=28  nodos de entrada autoregresivos,
s = 1  nodo estacional
k = 1 + 2*(p+1)+ q*1 + q*s

medidas.hw(y,yhat_nnar,k)

Crear una matriz con los resultados
resultados <- matrix(0, nrow = 6, ncol = 5)
rownames(resultados) <- c("Modelo Cuadrático", "Modelo Cúbico", "Modelo Exponencial Lineal", "Modelo Exponencial Cuadrático", "Modelo Holt-Winters Amortiguado", "Modelo Red Neuronal NNAR")
colnames(resultados) <- c("MSE", "RMSE", "R-cuadrado ajustado","AIC", "BIC")

Rellenar la matriz con los resultados
resultados[1, ] <- medidas(modelo_cuadratico, codensa_y, 8)
resultados[2, ] <- medidas(modelo_cubico, codensa_y, 8)
resultados[3, ] <- medidas(modelo_exponencial, log(codensa_y), 7)
resultados[4, ] <- medidas(modelo_exponencial_cuad, log(codensa_y), 8)
resultados[5, ] <- medidas.hw(y, yhat_holt, 5)
resultados[6, ] <- medidas.hw(y, yhat_nnar, k)

kable(resultados, caption = "Tabla de Resultados de metricas para los Modelos", format = "markdown")

tt=seq((T-15+1),T,1)
tt2=tt*tt
tt3=tt*tt*tt
Itf = seasonaldummy(codensa_y,15)
xtf=cbind(rep(1,15),tt,Itf)
xtf2=cbind(rep(1,15),tt, tt2,Itf)

calcular_metricas <- function ( predichos , observados ) {
  rmse <- sqrt ( mean (( predichos - observados ) ^2) )
  mae <- mean (abs( predichos - observados ) )
  mape <- mean ( abs (( predichos - observados ) / observados ) ) * 100
  u_theil <- sqrt ( mean (( predichos - observados ) ^2) ) / sqrt ( mean ( observados
                                                                           ^2) )
  resultados <- data.frame ( RMSE = rmse , MAE = mae , MAPE = mape ,
                             U_Theil = u_theil )
  return ( resultados )
}

Pronósticos para el modelo cuadrático
pred_cuadratico <- predict(modelo_cuadratico, data.frame(t=tt, t2=tt2, It=I(Itf)))

Pronósticos para el modelo cúbico
pred_cubico <- predict(modelo_cubico, data.frame(t=tt, t2=tt2, t3=tt3, It=I(Itf)))

Pronósticos para el modelo exponencial
pred_exponencial <- predict(modelo_exponencial, data.frame(xt=I(xtf)))

Pronósticos para el modelo exponencial cuadratico
pred_exponencial_cuad <- predict(modelo_exponencial_cuad, data.frame(xt=I(xtf2)))

Pronósticos para el modelo Holt-Winters amortiguado
pred_holt <- forecast(modelo_holt,h=15)$mean

Pronósticos para el modelo red neuronal NNAR
pred_neural <- forecast(modelo_neuronal,h=15)$mean

resultado_cuadratico <- calcular_metricas ( pred_cuadratico,data.frame(t=tt, t2=tt2, It=I(Itf)) )
resultado_cubico <- calcular_metricas ( pred_cubico ,data.frame(t=tt, t2=tt2, t3=tt3, It=I(Itf)) )
resultado_exponencial <- calcular_metricas ( pred_exponencial,data.frame(xt=I(xtf)) )

resultado_exponenecial_cuad <- calcular_metricas ( pred_exponencial_cuad, data.frame(xt=I(xtf2)) )
resultado_holt <- calcular_metricas (pred_holt, yf )
resultado_neuronal <- calcular_metricas ( pred_neural , yf )
predichos <-rbind ( resultado_cuadratico, resultado_cubico , resultado_exponencial ,
                    resultado_exponenecial_cuad , resultado_holt , resultado_neuronal)

Función para calcular MAPE
calculate_mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

Métricas para el modelo cuadrático
rmse_cuadratico <- rmse(yf, pred_cuadratico)
mae_cuadratico <- mae(yf, pred_cuadratico)
mape_cuadratico <- calculate_mape(yf, pred_cuadratico)
u_theil_cuadratico <- 1 / (length(yf)) * sum(((yf - pred_cuadratico) ^ 2) / (yf * pred_cuadratico))

Métricas para el modelo cúbico
rmse_cubico <- rmse(yf, pred_cubico)
mae_cubico <- mae(yf, pred_cubico)
mape_cubico <- calculate_mape(yf, pred_cubico)
u_theil_cubico <- 1 / (length(yf)) * sum(((yf - pred_cubico) ^ 2) / (yf * pred_cubico))

Métricas para el modelo exponencial
rmse_exponencial <- rmse(yf, pred_exponencial)
mae_exponencial <- mae(yf, pred_exponencial)
mape_exponencial <- calculate_mape(yf, pred_exponencial)
u_theil_exponencial <- 1 / (length(yf)) * sum(((yf - pred_exponencial) ^ 2) / (yf * pred_exponencial))

Métricas para el modelo exponencial cuadrático
rmse_exponencial_cuad <- rmse(yf, pred_exponencial_cuad)
mae_exponencial_cuad <- mae(yf, pred_exponencial_cuad)
mape_exponencial_cuad <- calculate_mape(yf, pred_exponencial_cuad)
u_theil_exponencial_cuad <- 1 / (length(yf)) * sum(((yf - pred_exponencial_cuad) ^ 2) / (yf * pred_exponencial_cuad))

Métricas para el modelo Holt-Winters amortiguado
rmse_holt <- rmse(yf, pred_holt)
mae_holt <- mae(yf, pred_holt)
mape_holt<- calculate_mape(yf, pred_holt)
u_theil_holt <- 1 / (length(yf)) * sum(((yf - pred_holt) ^ 2) / (yf * pred_holt))

Métricas para el modelo red neuronal NNAR
pred_neural<-as.numeric(pred_neural)

rmse_neural <- rmse(yf, pred_neural)
mae_neural <- mae(yf, pred_neural)
mape_neural<- calculate_mape(yf, pred_neural)
u_theil_neural <- 1 / (length(yf)) * sum(((yf - pred_neural) ^ 2) / (yf * pred_neural))


metricas <- data.frame(
  Modelo = c("Cuadrático", "Cúbico", "Exponencial","Exponencial Cuadratico","Holt-Winters amortiguado","red neuronal NNAR"),
  RMSE = c(rmse_cuadratico, rmse_cubico, rmse_exponencial,
           rmse_exponencial_cuad,rmse_holt,rmse_neural),
  MAE = c(mae_cuadratico, mae_cubico, mae_exponencial,
          mae_exponencial_cuad,mae_holt,mae_neural),
  MAPE = c(mape_cuadratico, mape_cubico, mape_exponencial,
           mape_exponencial_cuad,mape_holt,mape_neural),
  U_Theil = c(u_theil_cuadratico, u_theil_cubico, u_theil_exponencial,
              u_theil_exponencial_cuad,u_theil_holt,u_theil_neural))
kable(metricas, caption = "Tabla de Resultados de metricas para los Modelos")


Gráfica de Datos Observados vs. Estimados y Pronosticados para el Modelo Red Neuronal NNAR
library(ggplot2)

ggplot(data = test_data, aes(x = t)) +
  geom_line(aes(y = y, color = "Observados"), size = 1) +
  geom_line(aes(y = pred_neural, color = "Estimados"), size = 1, linetype = "dashed") +
  geom_line(aes(y = pred_neural, color = "Pronosticados"), size = 1, linetype = "dotted") +
  labs(
    x = "Tiempo",
    y = "Valor",
    title = "Comparación de Datos Observados, Estimados y Pronosticados (Modelo Red Neuronal NNAR)"
  ) +
  scale_color_manual(
    name = "Series",
    values = c(Observados = "blue", Estimados = "red", Pronosticados = "green")
  ) +
  theme_minimal()


par(mfrow=c(1,2))
prueba.cusum1 = efp(codensa_y ~ t + I(t^2) + I(t^3)+ It, type = "Rec-CUSUM")
plot(prueba.cusum1)

prueba.cusum2 = efp(codensa_y ~ t + I(t^2) + I(t^3)+ It, type = "OLS-CUSUM")
plot(prueba.cusum2)

sctest(prueba.cusum1)
sctest(prueba.cusum2)