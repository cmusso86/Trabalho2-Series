setwd("~/Estudos/Séries temporais/Listas/Trabalho 2")
#install.packages("pacman")
pacman::p_load(Mcomp, tseries, tidyverse, forecast, xtable)
data(M3)
id=1893 # id da série 1893 1931

# MSTL ----
M3[[id]]$description
serie <- M3[[id]]$x
plot(serie,
     main=M3[[id]]$description,
     ylab="", xlab="Ano")

# Decomposição via mstl
mstl(serie) %>% autoplot(xlab="Ano",
                     main="") + theme_bw()

# Seleção modelo NORMAL ----
par(mfrow=c(1,2))
acf(serie, lag.max=12*5)
pacf(serie, lag.max=12*5)
dev.off()

serie %>% ndiffs()

dx <- serie %>% diff() #%>% diff(lag=12)
ndiffs(dx)
nsdiffs(dx)
plot(dx)

cbind('Série Original' = serie, 'Séria Diferenciada' = dx) %>%
  autoplot(facets = T) +
  labs(x = "Ano", y = "") +
  scale_x_continuous(breaks = seq(1982,1992,2)) +
  theme_bw()

kpss.test(serie)
kpss.test(dx)

par(mfrow=c(1,2))
pacf(dx, lag.max=12*4) # p 0 1 2 (P = 1)
acf(dx, lag.max=12*4) # q 0 1 2 (Q = 0)
dev.off()

melhor_AICc = Inf
for(p in 0:2){
  for(q in 0:2){
    fit = Arima(serie,order=c(p,1,q),seasonal=c(1,0,0))
    if(fit$aicc < melhor_AICc){
      melhor_AICc = fit$aicc
      cat("p =",p,", q =",q,", AICc =", fit$aicc, "\n")
    }
  }
}

#(1,1,2)x(1,0,0)[12]
auto.arima(serie)
fit_1 = Arima(serie, order=c(1,1,2), seasonal=c(1,0,0))

#Resíduos
par(mfrow=c(1,3))
E <- fit_1$residuals
plot(E)
qqnorm(E) 
qqline(E)
acf(E, lag.max=12*5) #save: 4 9
dev.off()

kpss.test(E) 
Box.test(E, lag=15, type = "Ljung-Box") 
Box.test(E, lag=20, type = "Ljung-Box") 
shapiro.test(E) 

# Seleção modelo BOXCOX----
(lambda_auto <- serie %>% BoxCox.lambda())

par(mfrow=c(2,1))
serie %>% plot(main="Série original")
serie_box <- serie %>% BoxCox(lambda_auto)
serie_box %>% plot(main="Box-Cox")

cbind('Série Original' = serie, 'Série Box-Cox' = serie_box) %>%
  autoplot(facets = T) +
  labs(x = "Ano", y = "") +
  scale_x_continuous(breaks = seq(1982,1992,2)) +
  theme_bw()

mstl(serie_box) %>% autoplot(xlab="Ano",
                             main="") + theme_bw()

par(mfrow=c(1,2))
acf(serie_box, lag.max=12*5)
pacf(serie_box, lag.max=12*5)
dev.off()

serie_box %>% ndiffs()

dx_box <- serie_box %>% diff() #%>% diff(lag=12)
ndiffs(dx_box)
nsdiffs(dx_box)
plot(dx_box)

cbind('Série Diferenciada sem Boxcox' = dx, 'Série Diferenciada com Boxcox' = dx_box) %>%
  autoplot(facets = T) +
  labs(x = "Ano", y = "") +
  scale_x_continuous(breaks = seq(1982,1992,2)) +
  theme_bw()

kpss.test(serie_box)
kpss.test(dx_box)

par(mfrow=c(1,2))
pacf(dx_box, lag.max=12*4) # p 0 1 >2< (P = 0)
acf(dx_box, lag.max=12*4) # q 0 >1< 2 (Q = 1)
dev.off()

melhor_AICc = Inf
for(p in 0:2){
  for(q in 0:2){
    fit = Arima(serie_box,order=c(p,1,q),seasonal=c(1,0,0))
    if(fit$aicc < melhor_AICc){
      melhor_AICc = fit$aicc
      cat("p =",p,", q =",q,", AICc =", fit$aicc, "\n")
    }
  }
}

#(1,1,2)x(1,0,0)[12]
auto.arima(serie_box)

#Ajuste
fit_2 = Arima(serie_box, order=c(1,1,2), seasonal=c(1,0,0))

#Resíduos
par(mfrow=c(1,3))
E <- fit_2$residuals
plot(E)
qqnorm(E) 
qqline(E)
acf(E, lag.max=12*5)
dev.off()

kpss.test(E) 
Box.test(E, lag=15, type = "Ljung-Box") 
Box.test(E, lag=20, type = "Ljung-Box") 
shapiro.test(E) 


# Seleção modelo ETS Normal ----
mstl(serie) %>% plot()
# Resultado de critério de informação ETS sem transformação
fit1<- ets(serie,model = "AAA",damped = FALSE)
fit2<- ets(serie,model = "AAA",damped = TRUE)
fit3<- ets(serie,model = "MAA",damped = FALSE)
fit4<- ets(serie,model = "MAA",damped = TRUE)
#fit5<- ets(serie,model = "AMA",damped = FALSE)
#fit6<- ets(serie,model = "AMA",damped = TRUE)
#fit7<- ets(serie,model = "AAM",damped = FALSE)
#fit8<- ets(serie,model = "AAM",damped = TRUE)
#fit9<- ets(serie,model = "MMA",damped = FALSE)
#fit10<- ets(serie,model = "MMA",damped = TRUE)
fit11<- ets(serie,model = "MAM",damped = FALSE)
fit12<- ets(serie,model = "MAM",damped = TRUE)
#fit13<- ets(serie,model = "AMM",damped = FALSE)
#fit14<- ets(serie,model = "AMM",damped = TRUE)
fit15<- ets(serie,model = "MMM",damped = FALSE)
fit16<- ets(serie,model = "MMM", damped = TRUE)

AIC <- rbind(fit1$aic,fit2$aic,fit3$aic,fit4$aic,
             fit11$aic,fit12$aic,fit15$aic,fit16$aic)
AICc <- rbind(fit1$aicc,fit2$aicc,fit3$aicc,fit4$aicc,
              fit11$aicc,fit12$aicc,fit15$aicc,fit16$aicc)
BIC <- rbind(fit1$bic,fit2$bic,fit3$bic,fit4$bic,
             fit11$bic,fit12$bic,fit15$bic,fit16$bic)

Modelo <- cbind(c("ETS(A,A,A)","ETS(A,Ad,A)","ETS(M,A,A)","ETS(M,Ad,A)",
                  "ETS(M,A,M)","ETS(M,Ad,M)","ETS(M,M,M)","ETS(M,Md,M)"))
d <- data.frame(Modelo,AIC,AICc,BIC)
xtable(d)
plot(fit16) 

#Resíduos
par(mfrow=c(1,3))
E <- fit16$residuals
plot(E)
qqnorm(E) 
qqline(E)
acf(E, lag.max=12*5)
dev.off()

kpss.test(E) 
Box.test(E, lag=15, type = "Ljung-Box") 
Box.test(E, lag=20, type = "Ljung-Box") 
shapiro.test(E) 


# Seleção modelo ETS boxcox ----
mstl(serie_box) %>% plot()
# Resultado de critério de informação ETS sem transformação
fit1_box<- ets(serie_box,model = "AAA",damped = FALSE)
fit2_box<- ets(serie_box,model = "AAA",damped = TRUE)
fit3_box<- ets(serie_box,model = "MAA",damped = FALSE)
fit4_box<- ets(serie_box,model = "MAA",damped = TRUE)
# fit5_box<- ets(serie_box,model = "AMA",damped = FALSE)
# fit6_box<- ets(serie_box,model = "AMA",damped = TRUE)
# fit7_box<- ets(serie_box,model = "AAM",damped = FALSE)
# fit8_box<- ets(serie_box,model = "AAM",damped = TRUE)
# fit9_box<- ets(serie_box,model = "MMA",damped = FALSE)
# fit10_box<- ets(serie_box,model = "MMA",damped = TRUE)
fit11_box<- ets(serie_box,model = "MAM",damped = FALSE)
fit12_box<- ets(serie_box,model = "MAM",damped = TRUE)
# fit13_box<- ets(serie_box,model = "AMM",damped = FALSE)
# fit14_box<- ets(serie_box,model = "AMM",damped = TRUE)
fit15_box<- ets(serie_box,model = "MMM",damped = FALSE)
fit16_box<- ets(serie_box,model = "MMM", damped = TRUE)

AIC <- rbind(fit1_box$aic,fit2_box$aic,fit3_box$aic,fit4_box$aic,
             fit11_box$aic,fit12_box$aic,fit15_box$aic,fit16_box$aic)
AICc <- rbind(fit1_box$aicc,fit2_box$aicc,fit3_box$aicc,fit4_box$aicc,
              fit11_box$aicc,fit12_box$aicc,fit15_box$aicc,fit16_box$aicc)
BIC <- rbind(fit1_box$bic,fit2_box$bic,fit3_box$bic,fit4_box$bic,
             fit11_box$bic,fit12_box$bic,fit15_box$bic,fit16_box$bic)

Modelo <- cbind(c("ETS(A,A,A)","ETS(A,Ad,A)","ETS(M,A,A)","ETS(M,Ad,A)",
                  "ETS(M,A,M)","ETS(M,Ad,M)","ETS(M,M,M)","ETS(M,Md,M)"))
d <- data.frame(Modelo,AIC,AICc,BIC)
xtable(d)
plot(fit1_box)

#Resíduos
par(mfrow=c(1,3))
E <- fit1_box$residuals
plot(E)
qqnorm(E) 
qqline(E)
acf(E, lag.max=12*5)
dev.off()

kpss.test(E) 
Box.test(E, lag=15, type = "Ljung-Box") 
Box.test(E, lag=20, type = "Ljung-Box") 
shapiro.test(E) 

# Desempenho preditivo -----
# Sarima
f_arima <- function(y, h){
  fit = Arima(y, order=c(1,1,2), seasonal=c(1,0,0))
  forecast(fit, h)
}
# Sarima com transformação
f_arima_boxcox <- function(y, h){
  fit = Arima(y, order=c(1,1,2), seasonal=c(1,0,0), lambda = 0.3165242)
  forecast(fit, h)
}
# ETS
f_ets <- function(y, h){
  fit = ets(y, model = "MMM", damped = TRUE)
  forecast(fit, h)
}
# ETS com transformação
f_ets_boxcox <- function(y, h){
  fit = ets(y, model="AAA", damped = FALSE, lambda = 0.3165242)
  forecast(fit, h)
}


# Tamanho da série
n <- length(serie)
# Erros de previsão
CV_arima = tsCV(y=serie, forecastfunction=f_arima, h=5, initial=n-14)
CV_arima_boxcox = tsCV(y=serie, forecastfunction=f_arima_boxcox,
                       h=5, initial=n-14)
CV_ets = tsCV(y=serie, forecastfunction=f_ets, h=5, initial=n-14)
CV_ets_boxcox = tsCV(y=serie, forecastfunction=f_ets_boxcox,
                     h=5, initial=n-14)

# Cálculo do erro absoluto médio (MAE) para cada horizonte de previsão
MAE_arima = CV_arima %>% abs() %>% colMeans(na.rm=T)
MAE_arima_boxcox = CV_arima_boxcox %>% abs() %>% colMeans(na.rm=T)
MAE_ets = CV_ets %>% abs() %>% colMeans(na.rm=T)
MAE_ets_boxcox = CV_ets_boxcox %>% abs() %>% colMeans(na.rm=T)
tab = cbind(as.numeric(MAE_arima), as.numeric(MAE_ets),as.numeric(MAE_arima_boxcox), as.numeric(MAE_ets_boxcox))
colnames(tab) <- c('ARIMA', 'ETS', 'ARIMA com Box-Cox', 'ETS com Box-Cox')
xtable(tab)

# Gráfico das médias dos resultados dos erros
# Sem transformação <- as.numeric(tab)
plot.ts(tab,plot.type='s',col=c("#06d6a0","#1b9aaa","#ef476f","#ffc43d"),lwd=2,xlab="h",ylab="MAE",
        main=bquote('Gráfico dos erros de previsão para cada horizonte'))
legend('topleft', legend=c('ARIMA', 'ETS','ARIMA com Box-Cox', 'ETS com Box-Cox'), 
       col=c("#06d6a0","#1b9aaa","#ef476f","#ffc43d"), lwd=2)


# Previsão pontual e intervalar ARIMA

serie <- M3[[id]]$x
M3[[id]]$h

fit_1 = Arima(serie, order=c(1,1,2), seasonal=c(1,0,0))
fit_2 = Arima(serie_box, order=c(1,1,2), seasonal=c(1,0,0))

prev_fit_1 <- fit_1 %>% forecast(h=18, level = 95)
df1 <- data.frame(prev_fit_1)
colnames(df1) <- c("Previsão Pontual", "LI 95", "LS 95")
xtable(df1)
prev_fit_1 %>%  plot(main= "Previsão de ARIMA(1,1,2)(1,0,0)[12]", cex.main=0.9, cex.axis=0.8)

prev_fit_2 <- fit_2 %>% forecast(h=18, level = 95)
df2 <- data.frame(prev_fit_2)
colnames(df2) <- c("Previsão Pontual", "LI 95", "LS 95")
xtable(df2)
prev_fit_2 %>%  plot(main= "Previsão de ARIMA(1,1,2)(1,0,0)[12] com Box-Cox", cex.main=0.9, cex.axis=0.8)

# Previsão pontual e intervalar ETS

fit16<- ets(serie,model = "MMM", damped = TRUE)
fit1_box<- ets(serie_box,model = "AAA",damped = FALSE)

prev_fit16 <- fit16 %>% forecast(h=18, level=95)
df3 <- data.frame(prev_fit16)
colnames(df3) <- c("Previsão Pontual", "LI 95", "LS 95")
xtable(df3)
prev_fit16 %>% plot(main= "Previsão de ETS(M,Md,M)", cex.main=0.9, cex.axis=0.8)


prev_fit1_box <- fit1_box %>% forecast(h=18, level=95)
df4 <- data.frame(prev_fit1_box)
colnames(df4) <- c("Previsão Pontual", "LI 95", "LS 95")
xtable(df4)
prev_fit1_box %>% plot(main= "Previsão de ETS(A,A,A) com Box-Cox", cex.main=0.9, cex.axis=0.8)

# MAE

dados_teste <- M3[[id]]$xx
(lambda_auto <- serie %>% BoxCox.lambda())
dados_teste_box <- dados_teste %>% BoxCox(lambda_auto)

ar <- accuracy(prev_fit_1, dados_teste)[[6]]
ar_bc <- accuracy(prev_fit_2, dados_teste_box ) [[6]]
ets <- accuracy(prev_fit16, dados_teste)[[6]]
ets_bc <- accuracy(prev_fit1_box, dados_teste_box )[[6]]
auto_arima <- accuracy(forecast(auto.arima(serie), h=18, level =95), dados_teste)[[6]]
ses <- accuracy(ses(serie, h=18, level =95), dados_teste)[[6]]
holt <- accuracy(holt(serie, h=18, level =95), dados_teste)[[6]]
ets <- accuracy(forecast(ets(serie), h=18, level =95), dados_teste)[[6]]
stlf <- accuracy(stlf(serie, h=18, level =95), dados_teste)[[6]]
bats <- accuracy(forecast(bats(serie), h=18, level =95), dados_teste)[[6]]
tbats<- accuracy(forecast(tbats(serie), h=18, level =95), dados_teste)[[6]]

df_modelos <- data.frame(c("Arima", "Arima c/ Box-Cox", "ETS", "ETS c/ Box-Cox"),
                         c(ar, ar_bc, ets, ets_bc))
colnames(df_modelos) <- c('Modelo', 'MAE')
xtable(df_modelos)

df_outros <- data.frame(c( "auto.arima()", "ses()", "holt()", "ets()", "stlf()", "bats()",
                           "tbats()"), c(auto_arima, ses, holt, ets, stlf, bats, tbats))
colnames(df_outros) <- c('Benchmarks', 'MAE')
xtable(df_outros)
