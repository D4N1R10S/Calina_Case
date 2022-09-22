library(readxl)
library(magrittr)
library(ggplot2)
library(fpp2)
library(forecast)
library(urca)
library(lubridate)
library(MLmetrics) 

dds <- read_excel("C:/Users/danie/OneDrive/Área de Trabalho/Calina - R/Ciência de Dados.xlsx",
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric"))
#DADOS EM 52
#head(dds$Week)

#b<-as.Date(dds$Week,format="%Y-%m-%d")
#class(b)

#transformando Week de Posixc em Date

dados <- dds[-1]

#dados["week"] <- b  



ts = ts(dados, start=c(2018,1,1), frequency = 52)

# Series Temporais: Geral, A, B, C e Receita
A.ts = ts(dados[,1],start = c(2018,1,1), frequency = 52) 
B.ts = ts(dados[,2], start = c(2018,1,1), frequency = 52)
C.ts = ts(dados[,3], start = c(2018,1,1), frequency = 52)
Receita.ts = ts(dados[,4], start = c(2018,1,1), frequency = 52)
R.ts <- Receita.ts
####Plot simples de cada modelo

summary(ts)

ts.plot(A.ts, main="Mídia A", ylab="Receita da Mídia A (R$)", xlab="Tempo (Semanas)")
ts.plot(B.ts, main="Mídia B", ylab="Receita da Mídia B (R$)", xlab="Tempo (Semanas)")
ts.plot(C.ts, main="Mídia C", ylab="Receita da Mídia C (R$)", xlab="Tempo (Semanas)")
ts.plot(Receita.ts, main=" Receita Total", "Tempo (Semanas)")


#### Time Series Forecasting Example in RStudio ####
# https://www.youtube.com/watch?v=dBNy_A6Zpcc

# Seasonal method
DY.A <- diff(A.ts)
fit <-snaive(DY.A)
print(summary(fit)) #Residual SD = 3717.5295; p-value = 0.0008886
checkresiduals(fit) 
# no grafico ACF barras estão saindo do intervalo de confiança de 95%, o que significa que o modelo nao esta usando os dados de forma eficiente, procuramos um modelo melhor


#Fit ETS Method - Modelo de suavização exponencial(?)

fit_ets.A <- ets(A.ts)
print(summary(fit_ets.A))
checkresiduals(fit_ets.A) #Residual SD =  2280.247, Significa melhor ajuste

# Modelo ARIMA

arima.R <- auto.arima(R.ts, d=1, D=1, stepwise = FALSE, 
                      approximation = FALSE, trace=TRUE)

# Best model: ARIMA(0,1,1)(0,1,0)[52]
print(summary(arima.R)) # Residual [SD sqrt(9558187)] = 3091.632
checkresiduals(arima.R) # ARIMA deu SD Residual pior q o modelo anterior
#ta sobrando mais algumas autocorrelações

#Prevendo as 4 semanas depois
fcst.A <- forecast(arima.R, h=4) 
autoplot(fcst.R)
print(summary(fcst.R))

#1 Semana 531731; 80% - entre 324626 e 738836; 
#2 Semana 532222; 80% - entre 238337 e 82610; 
#3 Semana 532715; 80% - entre 171566 e 12432;   
#4 Semana 535207; 80% - entre 114791 e 12432; 
#Intervalo de Precisão







##### Introduction To Time Series In R Basic Models #####

#https://www.youtube.com/watch?v=8cKeAH2aGVI

#Melhorando com: https://www.youtube.com/watch?v=PvaKMQBR6nA

#Previsão de Receita 
#3 Separando modelos Teste 
ts.A.Train <- window(A.ts, end = c(2018,30))
ts.A.Test  <- window(A.ts, start = c(2018,31))


#tslm
#Treinando os modelos

#5 fazer que ele preveja 30 dados
meann <- meanf(ts.A.Train, h=10)
naivem <- naive(ts.A.Train, h=10)
driftm <- rwf(ts.A.Train, h=10, drift = T)
snaivem<- snaive(ts.A.Train, h=10)



#6
plot(meann, plot.conf = F, main="", xlab="Tempo (Semanas)", ylab = "Receita da Mídia A (R$)")
lines(naivem$mean, col=3, lty=1)
lines(driftm$mean, col=2, lty=1)
lines(naivem$mean, col=6, lty=1)
lines(A.ts, lty=5)
#Legenda em cima do grafico
legend("topleft", lty =1, col=c(1,3,2,6),legend=c("Mean Method","Naive Method","Drift Mehod","Season Naive"))

#7 HORRIVEL ;plot(snaivem, main=""); lines(ts.A.Test, col=7, lty=1, lwd=3)

#8 erro absoluto percentual medio
accuracy(meann, ts.A.Test)
accuracy(naivem, ts.A.Test)
accuracy(driftm, ts.A.Test)
accuracy(snaivem, ts.A.Test)

moving_avarege3 = forecast(ma(ts.A.Train, order = 3), h=20)



dm.test(residuals(fit), residuals(fit3), alternative = "greater")


























decomp <- decompose(R.ts)# perquisar
decomp$figure
plot(decomp$figure,
     type='b', 
     xlab = 'week',
     ylab= 'Índice de sazonalidade',
     col='blue',
     las=2)
plot(decomp)


R.ts %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Ano") +
  ggtitle("Decomposição Multiplicativa Clássica da Variável Renda Total")
  

 
#RMARK
#Pare escolhermos o melhor modelo usaremos a função que  implementa o teste modificado proposto por Harvey, Leybourne e Newbold (1997). A hipótese nula é que os dois métodos têm a mesma precisão de previsão. Para alternativa="greater", a hipótese alternativa é que o método 2 é mais preciso que o método 1. Escolhemos os modelos que possuem os maiores BICs.
dm.test(residuals(fit3), residuals(fit4), alternative = "greater")
#Os modelos possuem a mesma precisão de previsão.



dados2["BF"] <- rep(0, 152)
dados2[47,5] <- 1
dados2[100,4]<- 1





