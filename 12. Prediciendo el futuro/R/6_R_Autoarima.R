library(forecast)

# Dataset de linces 
# Número de linces atrapados por año en el período 1821-1934 en Canadá
length(lynx)

plot(lynx)

# Parece estacionario pero probablemente haya autocorrelación 
# Si se capturan muchos linces un año, se espera que al ño sgte hayan menos porque hay menos para reproducirse.
# Vemos un pulso cíclico que son los puntos máximos, no hay estacionalidad pero hay un patrón

# Es estacionaria
adf.test(lynx)



tsdisplay(lynx) # autoregresion?

#Por lo menos un AR(2)

auto.arima(lynx) # version basica

auto.arima(lynx, trace = T)

# Mejor modelo
myarima=auto.arima(lynx, trace = T, 
           stepwise = F, 
           approximation = F)
### ARIMA Forecast

# Forecast de 10 años
arimafore <- forecast(myarima, h = 10)

plot(arimafore)

# Valores estimados del futuro
arimafore$mean

# Ultimas observaciones y forecast
plot(arimafore, xlim = c(1930, 1944))

############################### Tarea ########################
# Obtén el valor del logaritmo de la verosimilitud (LL) y el AIC 
# para el modelo AR(4) que estimamos con auto.arima.
myarima = arima(lynx, order = c(4,0,0))
myarima

myarima=auto.arima(lynx, trace = T, 
                   max.order = 8,
                   max.p = 8,
                   stepwise = F, 
                   approximation = F)

myarima = arima(lynx, order = c(8,0,0))
myarima
