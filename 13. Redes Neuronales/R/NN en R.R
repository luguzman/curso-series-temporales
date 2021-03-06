
# Importando los datos
library(readr)
APTelectric <- read_csv("APTelectricity.csv", 
                        col_types = cols(X1 = col_skip()))

# Objeto ts
# Frequency = 288 ya que los watts se toman en intervalos de 5 min,
# entonces dado que hay 1440 min en un d�a 1440/5=288
myts = ts(APTelectric$watt, frequency = 288)
plot(myts)

# Ajuste del modelo
library(forecast)
fit = nnetar(myts)

# Predicciones
# h: horizonte temporal -> en este caso de 400 periodos que es 
# razonable dado que en nuestro caso una ventana estacional completa
# ser�an 288 periodos, esto ser�a un d�a entero, entonces nosotros
# vamos a obtener m�s de un d�a para ver si se mantiene ese patron.
# PI: se refiere al intervalo de predicci�n. Si F=False, obtenemos 
# uan estimaci�n puntual. Si T=True, obtenemos un intervalo de conf
# del 80% y de 95%. Esto no es muy recomendable al menos que tengamos
# un ordenadro de mucha potencia, por lo menos en este caso que tenemos
# una frecuencia tan peque�a por lo que son muchos datos, por lo que
# tardar�a mucho calculandolo.
nnetforecast <- forecast(fit, h = 400, PI = F)
library(ggplot2)
autoplot(nnetforecast)

# Podemos observar que la predicci�n es m�s suve a los datos reales,esto
# ocurre ya que el modelo esta tomando en total 14 retrasos de periodos
# anteriores, m�s uno del ciclo, en total 15. Y como pasa en el exponential
# smoothing, mientras m�s par�metros, m�s suave ser� la predicci�n osea
# menos variaci�n

# Usando una variable ex�gena
fit2 = nnetar(myts, xreg = APTelectric$appliances)


# Definiendo los pron�sticos de la variable ex�gena para 10 horas
# Dado que queremos predecir 10 hrs necesitamos ver cauntos intervalos
# de 5 min hay en 1hr => 60/5=12
y =rep(2, times = 12*10)
nnetforecast <- forecast(fit2, xreg = y, PI = F)
autoplot(nnetforecast)


# Definiendo los pron�sticos de la variable ex�gena para 30 horas
y =rep(2, times = 12*30)
nnetforecast <- forecast(fit2, xreg = y, PI = F)
autoplot(nnetforecast)

#Probando otras cosas
fit3 = nnetar(myts, 6)
nnetforecast3 <- forecast(fit3, h = 400, PI = F)
autoplot(nnetforecast3)

fit3 = nnetar(myts, 3, xreg = APTelectric$appliances)
y =c(rep(2, times = 12*10),  sample(2:8, 120, replace= TRUE))
nnetforecast3 <- forecast(fit3, xreg = y, PI = F)
autoplot(nnetforecast3)
