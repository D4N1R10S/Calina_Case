library(urca)
library(forecast)

##### Teste de Estacionariedade no R ##### - RUIM
# https://www.youtube.com/watch?v=INTlHrmbfXw

x <- ur.kpss(A.ts) # não é estacionária
print(x)
#processo de diferenciação
z <- diff(A.ts)
x <- ur.kpss(z)
print(x) #agora é estacionaria

ndiffs(A.ts)