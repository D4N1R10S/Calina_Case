##### Forecast #####
library(readxl)
library(magrittr)
library(ggplot2)
library(fpp2)
library(forecast)
library(GGally)
library(lubridate)

options(scipen = 10000000)
##### Lendo os Dados #####

dds <- read_excel("C:/Users/danie/OneDrive/Área de Trabalho/Calina - R/Ciência de Dados.xlsx",
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric"))



#transformando Week de Posixc em Date
dados <- dds[-1]


ts = ts(dados, start=c(2018, 1), frequency = 52)
R.ts = ts(dados[4], start=c(2018, 1), frequency = 52)



#decompose para cada variavel
#sacionaliocidade para cada variavel
#medidas resumo de cada variavel
#Modelos:


# https://otexts.com/fpp2/regression-intro.html 
##### 2.6 #####
autoplot(ts, facets=TRUE) +
  ylab("Receitas (R$)") + xlab("Tempo (Semanas)")

##### 3.1 #####
#Average method - Testes simples com a variavel Receita
R.mean <- meanf(R.ts, h=4)
plot(R.mean)
summary(R.mean)
checkresiduals(R.mean)

#Naive method
R.naive<-naive(R.ts, h=4)
rwf(R.ts, h=4) # Equivalent alternative
plot(R.naive)
summary(R.naive)
checkresiduals(R.naive)

#Seasonal naïve method

R.snaive <- snaive(R.ts, h=4)
plot(R.snaive)
summary(R.snaive)
checkresiduals(R.snaive)

#Drift Method

R.drift <- rwf(R.ts, h=4, drift=TRUE)
autoplot(R.drift)
summary(R.drift)
checkresiduals(R.drift)

#Prevendo as 4 semanas depois
fcst.R <- forecast(R.drift, h=4) 
autoplot(fcst.R) # grafico mais bonito que o simples Plot
print(summary(fcst.R))

autoplot(R.ts) +
  autolayer(meanf(R.ts, h=30), series = "Mean", PI=F) +
  autolayer(rwf(R.ts, h=30), series = "Naive", PI=F) +
  autolayer(rwf(R.ts, h=30, drift = T), series = "Drift", PI=F) +
  ggtitle("Previsões da Receita A") +
  xlab("Tempo(Semanas)") + ylab("Receita (R$)")+
  guides(colour=guide_legend(title="Forecast"))


## Ler direito 3.4 ##
#Modelo R ~ A + B + C


#### 3.6 ####
forecast(R.ts, h=4) #n se deve usar assim, achar um bom modelo





##### 5.1 #####


autoplot(ts[,c("A","B","C")]) + 
  xlab("Tempo em Semanas") +
  ylab("Receita (R$)")

# Figura x: Dispersão entre as Receita Total vs Receita de A
ts %>%
  as.data.frame() %>%
  ggplot(aes(x=Receita, y=A)) +
  ylab("Receita de A (R$)") +
  xlab("Receita de Total (R$) ") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)



#Gráfico de correlação e dispersão das ST

ts %>%
  as.data.frame() %>%
  ggpairs()

#Receita B tem o menor investimento da receita
fit <- tslm(
  Receita ~ A + B + C,
  data=ts)
summary(fit)



#pag 137 - Receita Total do Cliente Real vs Previsão da Receita Total do Cliente
autoplot(ts[,'Receita'], series="Data") +
  autolayer(fitted(fit), series="Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour=guide_legend(title=" "))

# pag 139 - arrumar mais tarde

cbind(Data = ts[,"Receita"],
      Fitted = fitted(fit)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  xlab("Fitted (predicted values)") +
  ylab("Data (actual values)") +
  ggtitle("Percent change in US consumption expenditure") +
  geom_abline(intercept=0, slope=1)

#pag 143 - Analysing the residuals from a regression model for US quarterly consumption.

checkresiduals(fit)

# pag 145 - Scatterplots of residuals versus each predictor.
library(gridExtra)

df <- as.data.frame(ts)
df[,"Residuals"] <- as.numeric(residuals(fit))
p1 <- ggplot(df, aes(x=Receita, y=Residuals)) +
  geom_point()
p2 <- ggplot(df, aes(x=A, y=Residuals)) +
  geom_point()
p3 <- ggplot(df, aes(x=B, y=Residuals)) +
  geom_point()
p4 <- ggplot(df, aes(x=C, y=Residuals)) +
  geom_point()
grid.arrange(p1, p2, p3, p4, nrow=2)


#pag 154
fit3 <- window(R.ts, start = c(2018,30))
autoplot(fit3) + xlab("Semanas") + ylab("Receita(R$)")

fitQ <- tslm(fit3 ~ trend + season)
summary(fitQ)

#pag 156

autoplot(fit3, series="Data") +
  autolayer(fitted(fit), series="Fitted") +
  xlab("Ano") + ylab("Receita em Reais") +
  ggtitle("Receita Semanalmente")

#pagina 156/157 - ta feio(fourier tendendo nada)

fourier.beer <- tslm(fit3 ~ trend + fourier(fit3, K=12))
summary(fourier.beer)

#5.5 - selecionando os preditores
# 162 

CV(fit)

#5.6 - Forecasting with regression
#pagina 170

fit <- window(R.ts, start=c(2018,31))
fit.R <- tslm(fit ~ trend + season)
fcast <- forecast(fit.R)
autoplot(fcast) +
  ggtitle("Forecasts of beer production using regression") +
  xlab("Year") + ylab("Receita(R$)")


summary(forecast(fit.R, h=4))  


  #pagina 172 - deu ruim

fit.consBest <- tslm(
  Receita ~ A + B + C,
  data = ts)
h <- 4
newdata <- data.frame(
  A = c(1, 1, 1, 1),
  B = c(0.5, 0.5, 0.5, 0.5),
  C = c(0, 0, 0, 0))
fcast.up <- forecast(fit.consBest, newdata = newdata)
newdata <- data.frame(
  A = rep(-1, h),
  B = rep(-0.5, h),
  C = rep(0, h))
fcast.down <- forecast(fit.consBest, newdata = newdata)

autoplot(ts[, 1]) +
  ylab("Renda Semanal(R$)") +
  autolayer(fcast.up, PI = TRUE, series = "increase") +
  autolayer(fcast.down, PI = TRUE, series = "decrease") +
  guides(colour = guide_legend(title = "Scenario"))

