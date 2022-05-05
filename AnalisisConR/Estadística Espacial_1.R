# Instalaci?n y carga de paquetes

# install.packages (c("spdep","gstat","geoR","mapview", "raster","leaflet","RColorBrewer",
#                     "PerformanceAnalytics","ggplot2","caret","parallel","nlme","sf","stars"))

library(spdep)
library(gstat)
library(geoR)
library(mapview)
library(raster)
library(leaflet)
library(RColorBrewer)
library(PerformanceAnalytics)
library(ggplot2)
library(caret)
library(parallel)
library(nlme)
library(sf)
library(tmap)
library(stars)

# Seleccion de directorio de trabajo
#setwd(choose.dir(getwd(), "Seleccione Directorio de Trabajo"))

# Carga de base de datos
datos <- read.table("datosSF_depurados.txt", header = T)
head(datos)

# datos y posibles variables explicativas d_ son distancias a distintas cosas 


# Graficos Exploratorios
chart.Correlation(datos[, 1:9], histogram = TRUE, pch = 19)
chart.Correlation(datos[, c(1, 9:18)], histogram = TRUE, pch = 19)

# En estos graficos podemos ver la correlacion, distribuciones, si es asimetrica o no, relacion con otras variables 
# Vemos tendencias lineales y no lineales, descrimir como es la relacion, polinomio de primer segundo grado.

#Transformaci?n a objeto espacial (sf)
datos <- st_as_sf(datos, coords = c("x", "y"), crs = 22174)
datos

# Transformamos los datos a un conjunto de datos espaciales con la libreria sf, aplicamos la funcion st_as_sf
# cual es la base de datos, coordenadas x e y + crs que asigna coordenadas de referencia. 
# X e y coordenadas planas posgar 98 espg 22174 informacion de antemano.

# Visualizaci?n
plot(datos)
# Plot automatico que muestra variabilidad de cada una de las variables

plot(
  datos["VUT"],
  key.pos = 4,
  axes = TRUE,
  key.width = lcm(1.5),
  key.length = 1,
  pal = terrain.colors
)
# Me concentro en valor unitario de la tierra, y hago plot, VUT es la variable que quiero modelar.

datos %>% ggplot() + geom_sf(aes(color = VUT)) +
  scale_color_viridis_c(direction = -1)
# Otro plot con ggplot. 

cols <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
mapview(datos,
        zcol = "VUT",
        alpha = 0,
        col.regions = cols)
# Plot dinamico, permite seleccionar mapa base, podemos cambiar el fondo.

################################################################
####################### ?ndice de Moran ########################
################################################################
################################################################
# Vamos con el calculo del indice de moran

# Definicion de vecindarios
vecindarios <- dnearneigh(datos, 0, 500)
# Lo primero que debemos hacer es la especificacion del vecindario.
# Nos basamos en la distancia (funcion dnearneigh) todos los puntos a una distancia de 500 m son mis vecinos.
# Lo que se encuentra por fuera de los 500 no es mi vecino.

plot(
  vecindarios,
  datos$geometry,
  col = "#009999",
  pch = 20,
  cex = 1
)

lw <- nb2listw(vecindarios, style = "W", zero.policy = TRUE)
lw[["neighbours"]]
lw[["weights"]]
# Generamos matriz de pesos espaciales, zero.policy, permite incluir vecinos nulos, si no lo incluyo a ese comando no me ejecuta y salta un error.
# Si no me dice que tengo vecindarios vacios, si le pongo TRUE tolera ello. 

# Calculo del ?ndice de autocorrelaci?n espacial de Moran
imoran <- moran.mc(datos$VUT, lw, nsim = 1000, zero.policy = T)
imoran
# Calculamos el indice de moran global.

# C?lculo del IM para multiples distancias

distancias <- function (dmax) {
  vecindarios <- dnearneigh(datos, 0, dmax)
  lw <- nb2listw(vecindarios, style = "W", zero.policy = T)
  i.moran <- moran.mc(datos$VUT, lw, nsim = 999, zero.policy =
                        T)
  tabla <- data.frame("Distancia" = dmax, "MI" = i.moran$statistic)
  tabla
}

tablad <-
  do.call(rbind, lapply(c(seq(100, 1000, 100), seq(1200, 2000, 200)), distancias))
tablad

# Generamos una funcion que me calcula el indice de moran a muchas distancias distintas. 
# Tenemos para cada distancia el valor del indice de moran

p <- ggplot(tablad, aes(x = Distancia, y = MI)) +
  geom_point() +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 2200),
                     breaks = seq(0, 2200, 200)) +
  ylab("Moran's I") +
  xlab("Distancia (m)") +
  theme_bw()
p
# Hacmeos un plot y vemos que el valor del indice de moran aumenta hasta los 400 metros, donde alcanza el valor mas alto y luego comienza a disminuir
# Cuando uno hace comparacion y define vecindarios, hay un valor de distancia que maximiza ese parecido entre los datos y se da a los 400 metros. 

################################################################
####################### Ajuste Semivariogramas #################
#######################      Estimaci?n WLS    #################
################################################################

# Ajuste de semivariograma experimetal
semi_exp <- variogram(VUT ~ 1, datos)
plot(semi_exp)
# Eje x distancia e y valores de semivarianza, vemos que a partir de un valor se estabiliza, patron que uno espera que se genere. 
semi_exp
# Nos dice la cantidad de puntos con las cuales se caulcula cada semivarianza.
# No toma la maxima distancia de separacion entre observaciones, toma un tercio de la maxima distancia y esa es la que usa para hacer el ajuste de la maxima distancia.
# Aproximadamente llega a 3500 metros y eso es un tercio de la distancia original que encierra mis puntos de muestreo. 

# Luego esos 3500 los divide en 15 puntitos y nos da el h que hay entre los puntos.
# Vemos como va variando la distancia en dist, ese es el lag o distancia de separacion. 

# Si quisiesemos aumentar esa distancia el argumento que debemos utilizar es el cutoff. Hasta donde yo quiero que calcule el semivariograma.
# Luego de los 3000 metros que se estabilizo el semivariograma, vuelve a caer y eso quiere decirque aumenta la correlacion, no tendria mucho sentido. 
# Cambiamos cutoff y cambia distancia maxima hasta la cual calcula el semivariograma.
# Esto indica que existiria cierto anidamiento o mezclas de escalas, ademas vemos que despues de los 8000, los ultimos valores calculados con pocos numeros de pares, es menor a 30 entonces poco confiable.
# Como minimo 30 pares de observaciones. 
# Si no tocamos cutoff llega hasta un tercio de la distancia maxima y por lo general toma 15 bins para calcular los valores de semivarianza. 


# Cosas a mirar:
# Cantidad de bins con los cuales contruimos el semivariograma. 
# Cantidad de puntos con los cuales se construye cada observacion. Mas de 30 para ser confiables.
# El punto donde se genera la estabilizacion de la semivarianza, llega a su rango. Hasta el rango esperamos tener por lo menos entre 4 y 6 bins o balores de semivarianza. 
# Estariamos con una buena cantidad de puntos para caracterizar la primera parte del semivariograma que es la critica.
# Queremos buen ajuste para puntos que estan por debajo del rango, que es la parte critica, ademas alli observamos nugget tambien. 
# 6 y 8 observaciones antes del rango con un buen N para estimarlos. 

# Tambien pensar en el tamaño de la matriz de datos para realizar el ajuste del semivariograma. Se sostiene que deben existir al menos 100 observaciones, georreferenciados, seria lo minimo para hacer un buen ajuste de semivariograma experimental y teorico.
# Entre 100 y 150 puntos para poder ajustar estas cosas. 
# Otros autores sostienen que cuando trabajam con estimaciones basadas en maxima verosimilitud podemos trabajar con n mas chicos.

# Otro argumento importante es el lag de separacion entre los puntos que puede ser modifcado, con el argumento width=....

# Al aumentar la cantidad de valores de semivarianza que se estima, cambia la cantidad de valores con los que estimo cada uno. 
# Widht igual a 200, toma maxima distancia y la divide en 200 y esos les quedan como cantidad de puntos a estimar. 

# Cantidad de valores estimados antes de que se estabilice.

# Verificar que se trate de una funcion creciente, semivarianzas positivas y que se estabilice en algun lado.
# Si no se estabiliza nunca eso podria hablar de que existe alguna tendencia en la variable que estoy ajustando. 
# Son procesos no estacionarios donde hay una tendencia con las coordenadas x e y. 


# Ajuste de semivariograma te?rico, modelo exponencial
mod_exp <- fit.variogram(semi_exp, vgm(3000000, "Exp", 1500, 10000))
mod_exp
plot(semi_exp , mod_exp)
# Pasamos al ajuste del modelo teorico: 
# En fucion de la fomra en que es el semivariograma muestral, vemos cual curva teorica puede ajustar bien
# Tenemos modelo exponencial, esferico, gaussiano, matteron, lineal, entre otros.
# Por lo general exponencial y esferico son los mas utlizados para ajuste semivariograma teorico. 


# El ajuste lo hacemos sobre el semivariograma experimental, y acontinuacion vgm() define el modelo que estamos ajustando.
# el primer numero, partial sill. 3000000
# Modelo conciderado Exp
# Rango: valor de distancia en la cual se estabiliza. 1500
# Nugget seria como la ordenada al origen en el grafico. 10000.

# Se puede forzar ajuste a que el nugget sea cero, pero por lo general se estima. 

mod_exp
# Informacion del modelo mod_exp
# model      psill    range
# 1   Nug   35520.78    0.000
# 2   Exp 3514832.04 1022.817

# El efecto nugget 35520.78
# El partial sill 3514832.04
# rango 1022.817

# Queremos interpretar rango de modelo exponencial, debemos multiplicarlo por 3.
# La funcion no tiene un rango verdadero pq sigue creciendo asintoticamente.
# Entonces se infroma rango practico *3, 3066 es el valor la cual se alcanza el 95% de la varianza. 


# Le hacemos el plot y entonces queda la curva, en el valor de 3000 maso menos se interpreta que llega al sill de la curva.
# Seria el rango 

# Ajuste de semivariograma te?rico, modelo esf?rico
mod_esf = fit.variogram(semi_exp, vgm(3000000, "Sph", 1500, 10000))
mod_esf
plot(semi_exp , mod_esf)

# Ahora procedemos a ajustar un semivariograma esferico, dandole los mismos parametros inciales.

# Visualizaci?n conjunta
vgLine <-
  rbind(cbind(variogramLine(mod_exp, maxdist = max(semi_exp$dist)), id =
                "Exponencial"),
        cbind(variogramLine(mod_esf, maxdist = max(semi_exp$dist)), id = "Esf?rico"))

grafico <-  ggplot(semi_exp, aes(x = dist, y = gamma, color = id)) +
  geom_line(data = vgLine) +
  geom_point() +
  labs(title = "Semivariograma experimental y te?rico ajustado")  +
  xlab("Distancia") +
  ylab("Semivarianza")  +
  scale_color_discrete(name = "Modelo",
                       labels = c("Esf?rico", "Exponencial", "Experimental"))

grafico

# Vemos la comparacion de ambos ajustes, podemos ver que graficamente el esferico ajusta mejor 
# Puede haber un puntito que esta afectando un poco el grafico, por lo tanto antes del inicio del analizis debemos hacer limpieza de los datos. 
# Semivariograma esferico parece mejor, hacer depuracion previa la analisis, ver si hay outliers, eliminarlos si es necesario y generar distribucion variable simetrica, sin valores extremos mejor. 
# Opcion en argumentos de la funcion variogram(cressie = TRUE), hace estimacion de un variograma robusto a presencia o existencia de valores extremos.
# Cuando hay valores extremos, pero son correctos probamos con parametro cressie = T
# Vemos que no mejora en el modelo exponencial la inclucion del nuevo parametro.


# Suma de cuadrado del error de modelos ajustados
attr(mod_exp, 'SSErr')
attr(mod_esf, 'SSErr')
# Verificando por la suma de cuadrados, el modelo exponencial es el que tiene mejor ajuste,
# En lo visual nos gustaba mas el esferico, pero este criterio de bondad de ajuste apoya al exponencial.
# Hay otras formas de verificar la bondad de ajuste

# Ajuste automatico de modelos
v.fit_todos <- fit.variogram(semi_exp, vgm(c("Exp", "Sph", "Gau")))
plot(semi_exp, v.fit_todos)

# Ahora en vgm solo colocamos los nombres de los modlos que queremos ajustar
# La funcion ajusta todos los modelos ajustando valores inciales de los parametros, razonables pero de forma automatica. 

attr(v.fit_todos, 'SSErr')
# Ahora muestra el modelo que mejor ajuste tubo en funcion de la suma de cuadrados del error.
v.fit_todos
# Solo vemos los parmetros del mejor.
# Esta forma de realizar el ajuste permite automatizar el modelo.


# Ajuste de modelo con tendencia
semi_exp_trend <-
  variogram(VUT ~ g_porc_edi + d_supcom + d_centro + d_indust + d_front, datos)
v.fit_todos_trend <-
  fit.variogram(semi_exp_trend, vgm(c("Exp", "Sph", "Gau")))
plot(semi_exp_trend, v.fit_todos_trend)
attr(v.fit_todos_trend, 'SSErr')
v.fit_todos_trend

# Finalmente hacemos el ajuste de un modelo con tendencia. 
# Veiamos correlacion entre VUT con otras variables, por medio del analisis de correlacion, algunas muy grandes. arriba 0.8
# Incorporamos las variables con las que esta correlacionada VUT, pero no correlacionadas entre ellas. Ahora ponemos como regresoras otras variables.
# Antes poniamos el 1 en vez de las variables, como asumiendo independencia. 
# Vemos que el esferico es el que mejor ajuste tubo, podriamos realizar un corte a los 2000m de distancia, que ya se estabiliza.
# Asi incorporamos la tendencia, a traves de las covariables. 
# Por lo general no tenemos covariables, se evalua la tendencia con la X e Y. 





# Podemos probar agregar o quitar covariables para ver como afecta al ajuste del modelo, pero mas en varosimilitud.
# Se toman variables a priori que se correlacionan con la variable respuesta, se puede hace runa regresion por stepwise y quedarnos con las variables que quedan seleccionadas. 
# Ese grupo seleccionado hacemos ajuste con tendencia. 
# Si vemos tendencia con x e y la incorporamos en el modelo, es lo mas normal, si tenemos tendencia y no la incluimos vemos que el variograma aumenta de forma indeterminada hasta el infinito. 
# Por la forma del semivariograma con 1 en vez de covariables, al infinito.
# Por otra parte incluimos covariables y semivariograma se estabiliza. 

################################################################
####################### Ajuste Semivariogramas #################
#######################      Estimaci?n REML    #################
################################################################

# Ajuste de semivariogramas con libreria geoR
datos_geo <- read.table("datosSF_depurados.txt", header = T)

datos_geo <-
  as.geodata(
    datos_geo,
    coords.col = c("x", "y"),
    data.col = "VUT",
    covar.col = c("g_porc_edi", "d_supcom", "d_centro", "d_indust", "d_front")
  )

ini_val <- c(var(datos_geo$data), 1000)

vut_exp_model <-
  likfit(datos_geo,
         ini = ini_val,
         cov.model = "exponential",
         lik.method = "REML")
summary(vut_exp_model)
# Hace ajuste de modelo exponencial por restricted maximum likelihood

vut_sph_model <-
  likfit(datos_geo,
         ini = ini_val,
         cov.model = "spherical",
         lik.method = "REML")
summary(vut_sph_model)

semiv <- variog(datos_geo) # !!!!
plot(semiv)
lines(vut_sph_model, lty = 2)


# Modelo con covariables
vut_exp_model_cov <-
  likfit(
    datos_geo,
    trend =  ~ g_porc_edi + d_supcom + d_centro + d_indust + d_front,
    ini = ini_val,
    cov.model = "exponential",
    lik.method = "REML"
  )
summary(vut_exp_model_cov)

(difloglik <-
    -2 * (
      vut_exp_model_cov$nospatial$loglik.ns - vut_exp_model_cov$loglik
    ))

(p.val <- pchisq(difloglik, df = 2, lower.tail = FALSE))
# Las covariables son significativas en el modelo exponencial.

vut_sph_model_cov <-
  likfit(
    datos_geo,
    trend =  ~ g_porc_edi + d_supcom + d_centro + d_indust + d_front,
    ini = ini_val,
    cov.model = "spherical",
    lik.method = "REML"
  )
summary(vut_sph_model_cov)
(
  difloglik2 <-
    -2 * (
      vut_sph_model_cov$nospatial$loglik.ns - vut_sph_model_cov$loglik
    )
)
(p.val2 <- pchisq(difloglik2, df = 2, lower.tail = FALSE))
# Las covariables son significativas en el esferico

vut_exp_model_cov[c("AIC", "BIC")]
vut_sph_model_cov[c("AIC", "BIC")]
# Da menor aic y bic con covariables en esferico. 

# Ajuste de semivariogramas con libreria nlme
datos_df <- read.table("datosSF_depurados.txt", header = T)

# Ajuste modelo sin covariables
vut_null_model <-
  gls(VUT ~ 1, datos_df, method = "REML", na.action = na.omit)

vut_exp_model_nlme <- gls(
  VUT ~ 1,
  data = datos_df,
  correlation = corExp(
    form =  ~ x + y,
    metric = "euclidean",
    nugget = T
  ),
  method = "REML",
  na.action = na.omit
)

summary(vut_exp_model_nlme)

vut_sph_model_nlme <- gls(
  VUT ~ 1,
  data = datos_df,
  correlation = corSpher(
    form =  ~ x + y,
    metric = "euclidean",
    nugget = T
  ),
  method = "REML",
  na.action = na.omit
)

summary(vut_sph_model_nlme)

anova(vut_null_model, vut_sph_model_nlme)
# Da diferncias significativas entre ambos. El esferico sin covariables parece mejor.


# Modelo con covariables
vut_null_model_nlme_cov <-
  gls(
    VUT ~ g_porc_edi + d_supcom + d_centro + d_indust + d_front,
    data = datos_df
    ,
    method = "REML",
    na.action = na.omit
  )


vut_sph_model_nlme_cov <-
  gls(
    VUT ~ g_porc_edi + d_supcom + d_centro + d_indust + d_front,
    data = datos_df
    ,
    correlation = corSpher(
      form =  ~ x + y,
      metric = "euclidean",
      nugget = T
    )
    ,
    method = "REML",
    na.action = na.omit
  )


anova(vut_null_model_nlme_cov, vut_sph_model_nlme_cov)
# A un nivel del 5% no significativo. 
# Ya con añadir covariables esta. 