library(readxl)
library(magrittr)
library(ggplot2)
library(fpp2)
library(forecast)
library(GGally)
library(lubridate)
library(MLmetrics) 

options(scipen = 10000000) #Tirar a notação científica dos gráficos

##### Lendo os Dados #####

dds <- read_excel("C:/Users/danie/OneDrive/Área de Trabalho/Calina - R/Ciência de Dados.xlsx",
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric"))


dados <- dds[-1]

  #Criando as Séries Temporais (ts)
ts = ts(dados, 
        start=c(2018, 1), 
        frequency = 52)

R.ts = ts(dados[,4], 
          start = c(2018,1,1), 
          frequency = 52)


#### Análise descritivas Simples das ST ####

  #Plot das ST das Receitas A, B e C - 'Sobrepostas'
autoplot(ts[,-4], xlab = "Ano",
         ylab="Receita (R$)",
         main = "Séries Temporais das Receitas A, B e C")



  #Plot das ST das Receitas A, B e C - 'Lado a Lado'
autoplot(ts[,-4], facets=TRUE, 
         main="Receitas das Séries lado a lado") +
         ylab("Receitas (R$)") +
         xlab("Tempo(Semanas)")


#### Vendo as correlações das séries ####
ts %>%
  as.data.frame() %>%
  ggpairs()

  #Plotando o comportamento da ST Receita
autoplot(R.ts, xlab = "Ano", 
         ylab = "Receita Total (R$)",
         main="Série Temporal da Variável Receita Total")


  #Plot das Séries da Receita Total Separada por Ano
ggseasonplot(R.ts, xlab="Semana", 
             main="Renda Total Separada por ano") 


#### Decomposição da ST Renda####
R.ts %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Ano") +
  ggtitle("Decomposição da Variável Renda Total")



#### Propondo Modelos ####

fit  <- tslm(Receita ~ A + B + C, data = ts)
fit2 <- tslm(Receita ~ A + C, data = ts)
fit3 <- tslm(Receita ~ trend + season, data = ts)
fit4 <- tslm(Receita ~ season, data = ts)

#### Escolhendo o Modelo ####

CV(fit)  
CV(fit2)
CV(fit3) 
CV(fit4)


#### Modelo de Teste ####
          
    #Pergunta Principal: Quanto o Modelo erra?

  #Prevendo a Última Semana
ts.train <-ts(dados[1:147,], 
              start=c(2018,1,1), 
              frequency = 52)

train <- tslm(Receita ~ season, 
              data = ts.train)

forecast(train, h=1)

  #Calculando o MAPE
e1 = MAPE(526964.7, 531238.9) # Semana 4 - 26/10/2020
e1

  
  #Prevendo as 2 últimas Semanas
ts.train <-ts(dados[1:146,], 
              start=c(2018,1,1), frequency = 52)


train <- tslm(Receita ~ season, 
              data = ts.train)


forecast(train, h=2)

  #Calculando os MAPEs
e2 = MAPE(526478.3, 519165.6) #Semana 3 -19/10/2020
#e2 

e3 = MAPE(526964.7, 531238.9) #Semana 4 - 26/10/2020
#e3

  #Prevendo as 3 últimas Semanas
ts.train <-ts(dados[1:145,], 
              start=c(2018,1,1), frequency = 52)


train <- tslm(Receita ~ season, 
              data = ts.train)

forecast(train, h=3)
  
  #Calculando os MAPEs
e4 = MAPE(542231.8, 516503.7) #Semana 2  - 12/10/2020
#e4

e5 = MAPE(526478.3, 519165.6) #Semana 3  - 19/10/2020 
#e5

e6 = MAPE(526964.7, 531238.9) #Semana 4  - 26/10/2020
#e6

  
  #Prevendo as 3 últimas Semanas
ts.train <-ts(dados[1:144,], 
              start=c(2018,1,1), 
              frequency = 52)


train <- tslm(Receita ~ season, 
              data = ts.train)


forecast(train, h=4)

  #Calculando os MAPEs
e7 = MAPE(479803.6 , 443026.8) #Semana 1 - 05/10/2020
#e7

e8 = MAPE(542231.8, 516503.7) #Semana 2 - 12/10/2020
#e8

e9 = MAPE(526478.3, 519165.6) #Semana 3 - 19/10/2020
#e9

e10 = MAPE(526964.7, 531238.9) #Semana 4 - 26/10/2020
#e10

mape <- c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10)
 
  #Erro médio das previsões é? (2.6%)
mean(mape)


#### Modelo Final - Prevendo as 4 Semanas ####
fcst.R <- forecast(fit4, h=4)

autoplot(fcst.R, xlab="Ano", 
     ylab="Receita Total(R$)",
     main="Regressão do Modelo")

summary(fcst.R)



#### Modelo Final Ajustado - Trocando os valores ####
  
dados1<-dados
dados1[47,4] <- 823460.9
dados1[48,4] <- 1311737.6

  #ST do dados1
ts = ts(dados1, start=c(2018, 1), frequency = 52)
R.ts = ts(dados1[,4], start = c(2018,1,1), frequency = 52)

  #Plot das Séries da Renda Total Separada por Ano
ggseasonplot(R.ts, xlab="Semana", 
             main="Renda Total Separada por ano")

fit4 <- tslm(Receita ~ season, data = ts)


#### Modelo Final Ajustado - Prevendo as 4 Semanas ####
fcst.R <- forecast(fit4, h=4)
summary(fcst.R)

  #plotando as previsões das prox. 4 semanas
plot(fcst.R)


#### Adendo ####

  #Variável Binária - Black Friday
dados2<- dados
dados2["BF"] <- rep('0', 148)
dados2[47,5] <- '1'
dados2[100,5]<- '1'



linha <- data.frame(A=c(0,0,0,0), 
                    B=c(0,0,0,0) ,
                    C=c(0,0,0,0), 
                    Receita=c(0,0,0,0), 
                    BF=c('0','0','0','1'))


dados2 <- rbind(dados2, linha)


  #ST com a variável BF
ts = ts(dados2[,c(4,5)], 
        start=c(2018, 1), 
        frequency = 52)


  #janela de dados da semana 1 a semana 148

ts.w <- window(ts, end = c(2020,44))

  #Vendo o comportamento da ST para as 4 semanas

  #Prevendo as 4 próx semanas
fcst<-forecast(ts.window, h=4)
fcst
plot(fcst)


  #Usando o Modelo fit 4
fit4 <- tslm(Receita ~ season, data = ts.window)

  #Vereficando o BIC
CV(fit4)

  #Prevendo as 4 próximas semanas
fcst<-forecast(fit4, h=4)
fcst

  #plot das prox 4 semanas
plot(fcst)


#Feliz Natal e Próspero Ano Novo, até mais ver!

