library(urca)
library(forecast)

##### Teste de Estacionariedade no R ##### - RUIM
# https://www.youtube.com/watch?v=INTlHrmbfXw

x <- ur.kpss(A.ts) # n�o � estacion�ria
print(x)
#processo de diferencia��o
z <- diff(A.ts)
x <- ur.kpss(z)
print(x) #agora � estacionaria

ndiffs(A.ts)