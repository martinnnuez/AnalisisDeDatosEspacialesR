################################################################
####################### Interpolaci?n Kriging  #################
################################################################

# Metodos de interpolacion por krigging.
# Generaci?n de grilla de predicci?n
Limites <- read.table("bordesSF.txt", header = T)
gr <- pred_grid(Limites, by = 100)
plot(gr)
gri <- polygrid(gr, bor = Limites)
plot(gri)
names(gri)[1] <- paste("x")
names(gri)[2] <- paste("y")
gridded(gri) = ~ x + y
plot(gri)

gri <- st_as_sf(gri)
st_crs(gri) <- 22174
gri
mapview(gri)

# Hacemos prediccion y cargamos la grilla en el mapa.

# Carga grilla predicci?n ejes de manzana
ejes <- read.table("ejesSF.txt", dec = ",", header = T)
names(ejes)
names(ejes)[names(ejes) == "g_perc_edif"] <- "g_porc_edi"
names(ejes)[names(ejes) == "g_perc_baldio"] <- "g_por_bald"
names(datos)

ejes <- st_as_sf(ejes, coords = c("x", "y"))
st_crs(ejes) <- 22174
mapview(ejes)

# Kriging Ordinario
v.fit_todos
kriging_grilla <- krige(VUT ~ 1, datos, gri, model = v.fit_todos)

plot(kriging_grilla["var1.pred"],
     main = "Kriging Ordinario: Predicciones",
     pal = terrain.colors,
     pch = 16)

plot(kriging_grilla["var1.var"], main = "Kriging Ordinario: Varianza", pal =
       terrain.colors)

kriging_grilla$DE_pred <- sqrt(kriging_grilla$var1.var)
head(kriging_grilla)

plot(kriging_grilla["DE_pred"],
     main = "Kriging Ordinario: DE",
     pal = terrain.colors,
     pch = 16)


kriging_ejes <- krige(VUT ~ 1, datos, ejes, model = v.fit_todos)

plot(kriging_ejes["var1.pred"],
     main = "Kriging Ordinario: Predicciones (ejes)",
     pal = terrain.colors,
     pch = 16)

plot(kriging_ejes["var1.var"], main = "Kriging Ordinario: Varianza (ejes)", pal =
       terrain.colors)


# Visualizaci?n interactiva
cols <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

muestra <-
  mapview(
    datos,
    zcol = "VUT",
    ceX = "VUT",
    layer.name = "Muestra",
    col.regions = cols,
    alpha = 0
  )

pred_grilla <-
  st_rasterize(kriging_grilla["var1.pred"],
               dx = 100,
               dy = 100,
               value = NA_real_)

plot(pred_grilla)

mapapred_grilla <-
  mapview(
    pred_grilla,
    legend = T,
    col.regions =  cols,
    layer.name = "Predichos Grilla",
    na.color = "transparent"
  )

mapapred_grilla

var_grilla <-
  st_rasterize(kriging_grilla["var1.var"],
               dx = 100,
               dy = 100,
               value = NA_real_)

plot(var_grilla)

mapavar_grilla <-
  mapview(
    var_grilla,
    legend = T,
    col.regions =  cols,
    layer.name = "Varianza Predicci?n Grilla",
    na.color = "transparent"
  )

mapavar_grilla


kriging_ejes <- st_as_sf(kriging_ejes)

mapapred_ejes <-
  mapview(
    kriging_ejes,
    zcol = "var1.pred",
    ceX = "var1.pred",
    col.regions =  cols,
    layer.name = "Predicho Ejes",
    alpha = 0
  )

mapapred_ejes

mapavar_ejes <-
  mapview(
    kriging_ejes,
    zcol = "var1.var",
    ceX = "var1.var",
    col.regions =  cols,
    layer.name = "Varianza Prediccion Ejes",
    alpha = 0
  )

mapavar_ejes

muestra +  mapapred_grilla + mapavar_grilla + mapapred_ejes + mapavar_ejes

##### Opciones visualizaci?n
tmap_mode('view')
mapa_prediccion <-
  tm_basemap(
    c(
      Satelite = "Esri.WorldImagery",
      Politico = "Esri.WorldGrayCanvas",
      Topo = "Esri.WorldTopoMap"
    )
  ) +
  tm_shape(pred_grilla) +
  tm_raster(
    title = "Valor Unitario de la Tierra ($/m2)",
    style = "fixed",
    palette = "Spectral",
    contrast = c(0, 1),
    breaks = c(340, 900, 1000, 1200, 1400, 1600, 2000, 2500, 3000, 3500, 8600)
  ) +
  tm_layout(legend.format = list(
    scientific = TRUE,
    format = "f",
    digits = 0
  ))
mapa_prediccion

tmap_save(mapa_prediccion, "mapa_prediccion.jpg")

mapa_var <-
  tm_basemap(
    c(
      Satelite = "Esri.WorldImagery",
      Politico = "Esri.WorldGrayCanvas",
      Topo = "Esri.WorldTopoMap"
    )
  ) +
  tm_shape(var_grilla) +
  tm_raster(
    title = "Varianza de Predicci?n",
    style = "quantile",
    palette = "YlOrBr",
    n = 5
  ) +
  tm_layout(legend.format = list(
    scientific = TRUE,
    format = "f",
    digits = 0
  ))
mapa_var

mapa_muestra <- tm_shape(datos) +
  tm_dots(
    "VUT",
    title = "Valor Unitario de la Tierra ($/m2)",
    style = "quantile",
    palette = "Spectral",
    n = 10,
    alpha = 0.7,
    size = 0.1,
    popup.vars = T,
    
  ) +
  tm_layout(legend.format = list(
    scientific = TRUE,
    format = "f",
    digits = 0
  ))
mapa_muestra

mapas_todos <- mapa_var + mapa_prediccion + mapa_muestra
mapas_todos

mapas_todos_lf <- tmap_leaflet(mapas_todos)
mapshot(mapas_todos_lf, "Mapas.html")


################################################################
####################### Validaci?n Cruzada Kriging #############
################################################################
mod_exp
mod_esf

attr(mod_exp, 'SSErr')
attr(mod_esf, 'SSErr')

set.seed(17)
kricv_mod_esf <- krige.cv(VUT ~ 1, datos, mod_esf, nfold = 10)
set.seed(17)
kricv_mod_exp <- krige.cv(VUT ~ 1, datos, mod_exp, nfold = 10)

bubble(kricv_mod_esf, "residual", main = "Residuos Esferico")
bubble(kricv_mod_exp, "residual", main = "Residuos Exponencial")

# Error medio de predicci?n (ME), cercano a cero mejor:
mean(kricv_mod_esf$residual)
mean(kricv_mod_exp$residual)

# Error medio absoluto (MAE)
mean(abs(kricv_mod_esf$residual))
mean(abs(kricv_mod_exp$residual))

# Error cuadratico medio de predicci?n (MSE), mas peque?o mejor
mean(kricv_mod_esf$residual ^ 2)
mean(kricv_mod_exp$residual ^ 2)

# Mean squared deviation ratio (MSDR), Error cuadratico medio normalizado, cercano a 1 mejor
mean(kricv_mod_esf$zscore ^ 2)
mean(kricv_mod_exp$zscore ^ 2)

# RMSE relativo a la media
sqrt(mean(kricv_mod_esf$residual ^ 2)) / mean(kricv_mod_esf$observed) *
  100
sqrt(mean(kricv_mod_exp$residual ^ 2)) / mean(kricv_mod_exp$observed) *
  100

# Correlaci?n lineal entre valores observados y predichos
cor(kricv_mod_esf$observed,
    kricv_mod_esf$observed - kricv_mod_esf$residual)
cor(kricv_mod_exp$observed,
    kricv_mod_exp$observed - kricv_mod_exp$residual)

# Correlaci?n lineal entre valores observados y predichos
par(mfrow = c(1, 2))
plot(
  kricv_mod_esf$observed,
  kricv_mod_esf$observed - kricv_mod_esf$residual,
  xlab = "Observados",
  ylab = "Predichos"
)
plot(
  kricv_mod_exp$observed,
  kricv_mod_exp$observed - kricv_mod_exp$residual,
  xlab = "Observados",
  ylab = "Predichos"
)

# Antes de esto no esta clase subida, asique continuo desde donde esta cargada la clase:

################################################################
####################### Kriging con deriva externa  ############
################################################################
# Alternativas de trabajar con otros tipos de krigging.
# Suponemos que el proceso no es estacionario y observamos tendencia con alguna covariable.

# Otro kirgging que sigue la misma idea es el krigging universal.
# Cuando observamos alguna tendencia en el semivariograma, entonces hacemos krigging universal. 

# Universal similar a deriva externa.
# unviersal cuando hay tendencia con x e y .
# deriva externa cuando la tendencia es con otras covariables. 

names(datos)

# Ajuste de semivariograma experimetal del VUT
semiv_ked <-
  variogram(VUT ~ g_porc_edi + d_supcom + d_centro + d_indust + d_front , datos)
plot(semiv_ked)
# Ajustamos semivariograma con tendencia repetimos.
# Ajustamos semivariograma experimental.

semiv_ked  <-
  variogram(
    VUT ~ g_porc_edi + d_supcom + d_centro + d_indust + d_front ,
    datos,
    cutoff = 2300,
    width = 250
  )
plot(semiv_ked)
# Especificamos algunos parametros. Acotamos distancia maxima con cutoff y cambiamos el lag o h entre los puntos.

# Ajuste automatico de modelos teorico a VUT en funcion a co-variables
v.fit_vut_ked <-
  fit.variogram(semiv_ked , vgm(c("Exp", "Sph", "Gau")))
plot(semiv_ked , v.fit_vut_ked)
# Vemos cual es el mejor modelo o semivariograma teorico que ajusta al semivariograma. 
v.fit_vut_ked
# Vemos que el esferico es el indicado. Selecciona el mejor modelo, el que tiene menor suma de cuadrados del error. 
# Guarda el que mejor ajusta, el criterio de seleccion es por minima suma de cuadrados del error y elije el de menor.

# Alternativamente podemos hacer este proceso de seleccion por validacion cruzada, no se basa en minimizar suma de cuadrado del error para seleccionar el modelo teorico
# Si no que se basa en el error de prediccion.


# Kriging con dervia externa
kged <-
  krige(VUT ~ g_porc_edi + d_supcom + d_centro + d_indust + d_front,
        datos,
        ejes,
        model = v.fit_vut_ked)

# Utilizamos la funcion krige que permite realizar la interpolacion krigging. 
# Definimos formula, datos, ejes que es la base de datos sobre la cual haremos la prediccion. 
# La clase pasada realizamos la prediccion sobre 2 bases de datos
# Una que era una grilla regular con distancia de separacion entre celdas a predecir de 100 metros
# Ejes representa el punto que esta situado a la mitad de la manzana de la ciudad
# Y finalmente el modelo teorico ajustado.
# La formula tiene que se la misma que pusimos en el ajuste del semivariograma.
# Ejes ya es un objeto espacial cargado al principio de la clase.

# Dice que usa kriging universal, porque expresamos una tendencia para la variable respuesta. 
# Engloba todos los krigging con tendencia como kriging universal.
# Con deriva externa cuadno las variables son distintas a las cordenadas. 


# Generamos el kriging con deriva externa teniendo en cuenta esas variables

# Hacemos el plot de las predicciones con deriva externa.
plot(kged["var1.pred"],
     main = "Kriging con deriva externa: Predicciones",
     pal = terrain.colors,
     pch = 16)

# Aqui ploteamos la varianza del kirgging con deriva externa.
plot(kged["var1.var"], main = "Kriging con deriva externa: Varianza", pal =
       terrain.colors)

# Visualizacion interactiva
mapapredejesKED <-
  mapview(
    kged,
    zcol = "var1.pred",
    ceX = "var1.pred",
    col.regions =  cols,
    layer.name = "Predicho Ejes KED",
    alpha = 0
  )
mapavarpredejesKED <-
  mapview(
    kged,
    zcol = "var1.var",
    ceX = "var1.var",
    col.regions =  cols,
    legend = TRUE,
    layer.name = "Varianza Predicci?n Ejes KED",
    alpha = 0
  )

muestra + mapapred_ejes + mapapredejesKED

# Hacemos visualizaciones.
# Recordar cuando vemos tendencia con covariables, tiene que ser muy marcara relacion con variable respuesta a traves de correlacion grande o diagrama dispersion, debe ser grande
# Siempre pensando en relacion lineal entre covariable y variable respuesta.

# Algo importante la base de datos ejes no solo teine que tener los x e y si no tambien el valor de las covariables que empleo en el modelo, si no no puedo ejecutarlo
# Es un requisito bastante importante.
# Para predecir en sitios no muestreado no tengo variable respuesta, pero si necesito tener el valor de las variables explicativas.
# Esto pone ciertas restricciones a la prediccion ya que tengo que tener valores observados en sitios donde quiero predecir, es decir que no conozco respuesta.

################################################################
####################### Kriging Regresi?n ######################
################################################################

# Otra metodologia muy relacionada, krigging regresion hermano gemelo de kriging con deriva externa
# Hacemos todo lo de tener en cuenta tendencias, en 2 pasos similar al kriging con deriva externa.


# Ajuste de modelo de RLM
mlr <-
  lm(VUT ~ g_porc_edi + d_supcom + d_centro + d_indust + d_front , datos)
# Ajustamos un modelo lineal en funcion de las covariables 
# Genero objeto de regresion en mlr hay informacion de la regresion para interpretar los coeficientes, en este caso casi todos significativos. 
# Podemos interpetar mas en detalle sobre como es interaccion con variable respuesta.


# Incorporamos los residuos del MLR a la base de datos
datos$residuos <- mlr$residuals
names(datos)
# Guardamos los residuos de la regresion, es decir que es como que extraemos la tendencia con las covariables de los datos y nos quedamos directamente con los residuos.
# Procedemos en el analisis a anlizar los residuos. 


# Ajuste de semivariograma experimetal y te?rico a los reiuduos
semiv_rk <- variogram(residuos ~ 1 , datos)
plot(semiv_rk)
# Semivariograma teorico sobre los residuos

semiv_rk <-
  variogram(residuos ~ 1 , datos, cutoff = 2300, width = 250)
plot(semiv_rk)
# Semivariograma experimental sobre los residuos

# Vemos que los plots recientemente ajustados, son muy parecidos a cuando hicimos el ajsute del semivariograma con tendencia.
# Varaible con correlacion es similar a covariable habiendo extraido tendencias es decir los residuos. 

v.fit_vut_rk <-
  fit.variogram(semiv_rk , vgm(c("Exp", "Sph", "Gau")))
plot(semiv_rk , v.fit_vut_rk)
# Ajustamos los teoricos y nos quedamos con el esferico. 
# En este caso hacemos lo mismo sobre los residuos.

# En estos residuos suponemos que no hay tendencia entonces aplicamos krigging ordinario a los residuos.

# Kriging sobre residuos
kgres <- krige(residuos ~ 1, datos, ejes, model = v.fit_vut_rk)

# Hago el plot d ela prediccion de los residuos, tienen valores positivos y negativos, concentrados en cero.
# Vemos que en el centro hay parches con valores de residuos muy altos o muy bajos y vemos que no estan ordenados de forma aleatoria.
# Vemos que hay un cierto tipo de correlacion espacial. 
# Hay correlacion espacial que debemos incluir en el modelo, no solo es tendencia con covariables, debemos tener en cuenta la espacial. 

plot(kgres["var1.pred"],
     main = "Kriging Residual: Predicciones",
     pal = terrain.colors,
     pch = 16)

# Predicci?n final
ejes$RK_pred <- predict(mlr, newdata = ejes) + kgres$var1.pred
# Componemos prediccion final con el modelo de regresion lineal multiple,
# Uso el modelo para predecir en los ejes, y a esa prediccion le sumo el krigging de los residuos.
# El krigging de los residuos guarda la correlacion espacial.

# Este procedimiento es similar al krigging con extrnal drift, pero en dos pasos. 
# Hacemos prediccion final, componiendola

plot(ejes["RK_pred"],
     main = "Predicci?n RK",
     pal = terrain.colors,
     pch = 16)
# Vemos el plot de la prediccion vemos valores menores que 0, que no es lo mas apropiado para los datos, pero es lo que sucede cuando usamos regresion y tenemos valores fuera del dominio.

# Mapa interactivo. 
mapapred_ejesRK <-
  mapview(
    ejes,
    zcol = "RK_pred",
    ceX = "RK_pred",
    col.regions =  cols,
    layer.name = "Predicho Ejes RK",
    alpha = 0
  )

muestra +  mapapred_grilla + mapapred_ejes + mapapred_ejesRK

# Que lectura hacemos sobre el mapa, ganamos conocimiento sobre el mapa, vemos que los metodos generan mayores predicciones en el area central.
# Como que el valor unitario de la tierra es mayor en el centro. 
# Cercanos avias principale tambien hay diferencia de valor. 
# Esto se comienza a ver cuando incluimos covariables.
# En kriging ordinario sin tener en ceunta covariables no se puede apreciar esto.

# Si uno quiere tener alguna idea mas del error de prediccion podemos hacer una validacion cruzada, cuantficamos el error de prediccion usando distintos metodos de krigging.
# Entonces seleccionamos modelo.
# No solo resalta diferencias qeu esperaria en estos datos, si no que tambien presenta menor error de prediccion. 
# Ajuste comparativo entre modelos importante.

# Ventaja de cual u otro esquema.
# La idea de modelar la tendencia, luego extraerla y guardar los residuos y hacer predicicon componiendola.
# Tenemos la libertad de que en el ajuste de tendencia podemos implementar otros algoritmos para extraer tendencia.
# Random forest, SVM, arbol, hacemos lo mismo.
# Puede haber otros algoritmos que rescaten mejor estas relaciones no lineales que podemos ver en el valor de la tierra.
# Tenemos mucha mas flexibilidad, podemos emplear otro algoritmo de aprendizaje automatico. 
# Relaciones del tipo no lineales. Generar predicciones de otra forma.

################################################################
####################### Random Forest Kriging ##################
################################################################
# Aqui ajustamos un random forest para extraer tendencia de las covariables. 

datos <- read.table("datosSF_depurados.txt", header = T)

# Ajuste del RF con librer?a caret
seed <- 7

# grilla de valores para mtry a evaluar
# Grilla de hiperparametro del mtry: es la cantidad de variables a seleccionar aleatoriamente en cada arbol.
mtry <- expand.grid(mtry = seq(2, 5, 1))
# De esta manera rompe estructura de correlacion. El numero de variables que toma aleatorias es constante y es mtry.


# opciones de validaci?n
# 10 fold cross validation
fitControl <-
  trainControl(method = "cv",
               number = 10,
               allowParallel = T)


# opciones para praralelizado para cuando trabajamos con bases de datos grandes.
# library(parallel)
# library(doParallel)
# cluster <- makeCluster(detectCores() - 1)
# registerDoParallel(cluster)

# ajuste del modelo de RF
set.seed(seed)
train_rf <-
  train(
    VUT ~ g_porc_edi + d_supcom + d_centro + d_indust + d_front,
    data = datos,
    method = "rf",# Cambiamos este parametro method y puedo ajustar otros SVM y asi.....
    importance = T, # Permite hacer variable importance. 
    tuneGrid = mtry, # Grilla tuneo hiperparametros. Caret solo permite mtry.
    trControl = fitControl
  )

# Aca se hace entrenamiento y vemos los resultados.
# nos quedamos con mtry=3 minimiza la RMSE. Y queda ajustado con ese modelo.
# Entrenamos el random forest con libreria caret, caret tiene muchos paquetes y librerias, entonces podemos hacer de una forma facil de muchas librerias distintas. 
# Caret esta muy piola y es intuitiva. 
# ml3 otra libreria mas nuevita conocida.

# Incorporamos los residuos del MLR a la base de datos
datos$residuosRF <- datos$VUT - predict(train_rf, newdata = datos)
# Obtenemos residuos del modelo, datos - prediccion de cada uno. 
# Ahi me quedo con los resiudos que guardan la correlacion espacial

# Ajuste de semivariograma experimetal y te?rico a los residuos del RF
datos <- st_as_sf(datos, coords = c("x", "y"), crs = 22174)

# Ajusto semivariograma experimental a los resiudos
semiv_RFk <- variogram(residuosRF ~ 1 , datos)
plot(semiv_RFk)

# Aqui lo hago con ciertos parametros
semiv_RFk <-
  variogram(residuosRF ~ 1 , datos, cutoff = 2300, width = 250)
plot(semiv_RFk)

v.fit_vut_RFk <-
  fit.variogram(semiv_RFk , vgm(c("Exp", "Sph", "Gau")))
plot(semiv_RFk , v.fit_vut_RFk)
# Ajusto los teoricos y me quedo con el esferico. 

# Kriging sobre residuos del RF
kgresRF <- krige(residuosRF ~ 1, datos, ejes, model = v.fit_vut_RFk)

plot(kgresRF["var1.pred"],
     main = "Kriging Residual (RF): Predicciones",
     pal = terrain.colors,
     pch = 16)
# Puedo ver estructura de correlacion de menor importancia pero se muestra. 

# Prediccion final RF, hago la composicion de prediccion + los residuos
ejes$RFK_pred <-
  predict(train_rf, newdata = ejes) + kgresRF$var1.pred
# Prediccion final random forest +  krigging predicho.

plot(ejes["RFK_pred"],
     main = "Predicci?n RFK",
     pal = terrain.colors,
     pch = 16)

mapapredejes_RFK <-
  mapview(
    ejes,
    zcol = "RFK_pred",
    ceX = "RFK_pred",
    col.regions =  cols,
    layer.name = "Predicho Ejes RFK",
    alpha = 0
  )

muestra +  mapapred_grilla + mapapred_ejes +  mapapred_ejesRK + mapapredejes_RFK

# Vemos una clara concentracion de los valores altos en el centro. 
# krigging y regresion krigging muy parecidos. 
# RF muestra mucha concentracion en el centro son diferencias visuales entre ellos que uno podria ver. 

################################################################
####################### Validaci?n Cruzada         #############
################################################################
# Funcion larga hecha por el, donde hace una validacion de tres metodos.
# FOLD, BASE DE DATOS Y VARIBALE REPSUESTA

# hace ajuste kriggging ordinario, kriging regresion y kriging random forest.

validacion <- function (fold, base, var.y) {
  require(caret)
  require(gstat)
  require(sp)
  
  datos <- read.table(base, head = T)
  names(datos)[names(datos) == var.y] <- "Y"
  
  if (base == "petrel.txt") {
    names(datos)[names(datos) == 'long'] <- "x"
    names(datos)[names(datos) == 'lat'] <- "y"
  }
  
  seed <- 7
  
  set.seed(seed)
  datos$id <-
    sample(rep(1:10, nrow(datos), length.out = nrow(datos)))
  
  list <- 1:10
  prediccion <- data.frame()
  testset <- data.frame()
  
  training <- subset(datos, id %in% list[-fold])
  testing <- subset(datos, id %in% c(fold))
  
  # Kriging Ordinario
  train_ko = training
  test_ko = testing
  coordinates(train_ko) <-  ~ x + y
  coordinates(test_ko) <-  ~ x + y
  vario <- variogram(Y ~ 1, train_ko)
  VF_vut_KO <- fit.variogram(vario, vgm(c("Sph", "Exp", "Gau")))
  KO <- krige(Y ~ 1, train_ko, test_ko, VF_vut_KO)
  
  # Regression Kriging
  train_ko = training
  test_ko = testing
  
  coordinates(train_ko) <-  ~ x + y
  coordinates(test_ko) <-  ~ x + y
  
  mlr <- lm(Y ~ . - x - y - id, training)
  
  pred_mlr = predict(mlr, newdata = test_ko)
  
  inside_rk <- predict(mlr, newdata = train_ko)
  train_ko$error_rk <- training$Y - inside_rk
  
  vario_rk <- variogram(error_rk ~ 1, train_ko) #, cutoff = 2300, width=250 ponemos estos parametros pq eran con los que veniamos trabajando.
  model_rk_ko <-
    fit.variogram(vario_rk, vgm(c("Sph", "Exp", "Gau")))
  
  test_k <- krige(error_rk ~ 1 , train_ko, test_ko, model_rk_ko)
  test_rk_ko <- pred_mlr + test_k$var1.pred
  
  # Random Forest
  #fitControl <- trainControl(method = "cv", number = 10)
  fitControl <- trainControl(method = "none")
  #mtry <-data.frame(mtry=2)
  set.seed(seed)
  
  rf <- train(
    Y ~ . - x - y - id,
    data = training,
    method = "rf",
    #tuneGrid =mtry,
    trControl = fitControl,
    verbose = FALSE
  )
  
  test_rf <- predict(rf, newdata = testing)
  
  # Random Forest + Kriging Ordinario
  inside_rf <- predict(rf, newdata = training)
  
  train_ko = training
  test_ko = testing
  
  coordinates(train_ko) <-  ~ x + y
  coordinates(test_ko) <-  ~ x + y
  
  train_ko$error_rf <- training$Y - inside_rf
  vario_rf <- variogram(error_rf ~ 1, train_ko) #, cutoff = 2300
  model_rf_ko <-
    fit.variogram(vario_rf, vgm(c("Sph", "Exp", "Gau")))
  test_ko <- krige(error_rf ~ 1 , train_ko, test_ko, model_rf_ko)
  test_rf_ko <- test_rf + test_ko$var1.pred
  
  
  # Tabla observados y predichos
  testset <- rbind(testset, as.data.frame(testing[, "Y"]))
  result <- data.frame(
    data.frame(
      "x" = testing$x,
      "y" = testing$y,
      "k-fold" = fold,
      "Observado" = testset[, 1],
      "KO" = KO$var1.pred,
      "RK" = test_rk_ko,
      "RF" = test_rf,
      "RF_KO" = test_rf_ko
    )
  )
  
  return(result)
  
}

# correr validacion cruzada
#resultados <- do.call(rbind,lapply(1:10, validacion))

# correr validacion cruzada paralelizado
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
system.time(resultados <-
              do.call(
                rbind,
                parLapply(cl, 1:10, validacion, base = "datosSF_depurados.txt", var.y =
                            "VUT")
              ))

# Aca paraleliza y ejecuta

# Comparaci?n de m?todos
head(resultados)
# Pone prediccion RF y no suma nadam y RF_KO con kriging ordinario.
# RK regresion kriging

tabla <- resultados[, 4:8]
resumen <- function (j) {
  ME <- mean(tabla [, j] - tabla[, "Observado"])
  MAE <- mean(abs(tabla [, j] - tabla[, "Observado"]))
  MAPE <-
    mean(abs(tabla [, j] - tabla[, "Observado"]) / tabla[, "Observado"]) *
    100
  MSE <- mean((tabla [, j] - tabla[, "Observado"]) ^ 2)
  RMSE <- sqrt(mean((tabla [, j] - tabla[, "Observado"]) ^ 2))
  nRMSE <- sqrt(MSE) / mean(tabla[, "Observado"]) * 100
  rLM <- lm(tabla [, j] ~ tabla[, "Observado"])
  R2 <- as.matrix(summary(rLM)$adj.r.squared)
  mx <- mean(tabla[, "Observado"])
  my <- mean(tabla [, j])
  s2x <- var(tabla[, "Observado"])
  s2y <- var(tabla [, j])
  sxy <- mean((tabla[, "Observado"] - mx) * (tabla [, j] - my))
  resumen <-
    data.frame("Modelo" = names(tabla [j]), ME, MAE, MAPE, MSE, RMSE, nRMSE, R2)
  return(resumen)
}

tablafinal <- do.call("rbind", lapply(2:5, resumen))
tablafinal
# Obtiene tabla final resumen con errores de prediccion o metricas comparacion desempeño de los modelos ejecutados. 
# Modelo       ME      MAE     MAPE       MSE     RMSE    nRMSE        R2
# 1     KO 23.83025 574.8569 43.29535 1031592.3 1015.673 48.17457 0.5984712
# 2     RK 24.37174 633.4445 47.28254 1277532.7 1130.280 53.61050 0.5344878
# 3     RF 19.25174 565.9473 43.83119  956053.8  977.780 46.37725 0.6148372
# 4  RF_KO 15.50887 573.0723 43.49481 1022661.7 1011.267 47.96560 0.5929816

# RF solo ya es muy bueno comparado a RF_KO no hay mucha mejoria RMSE estoy viendo. 
# nRMSE prediccion promedio.
# Modelo de solo random forest sin tener en cuenta krigging sobre los resiudos tubo mejor desempeño. 

# Otras bases de datos para aplicar los mismos metodos. 
datosAP <- read.table("datosAP.txt", head = T)
names(datosAP)
# corre la ultima funcion en esta base de datos.
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
system.time(resultados <-
              do.call(
                rbind,
                parLapply(cl, 1:10, validacion, base = "datosAP.txt", var.y =
                            "RindeSoja")
              ))

# Modelo          ME       MAE      MAPE       MSE      RMSE    nRMSE        R2
# 1     KO 0.002223275 0.2567968  9.323514 0.1164713 0.3412788 10.57276 0.9278274
# 2     RK 0.005098428 0.3020968 11.543750 0.1587911 0.3984861 12.34503 0.9017745
# 3     RF 0.004747305 0.4999943 20.320726 0.4176076 0.6462256 20.01996 0.7473294
# 4  RF_KO 0.002104196 0.3782188 14.807369 0.2485983 0.4985963 15.44643 0.8503696

# Estos son los resultados. Menor RMSE kriging ordinario, el random forest individual el mas flojito.
# Comparando RF con RF_KO, el RF_KO tiene una mejoria. 


datos_meuse <- read.table("meuse.txt", head = T)
names(datos_meuse)

datos_petrel <- read.table("petrel.txt", head = T)
names(datos_petrel) # mud es asimetrica!!!


# Nos concentramos bien en obetivo geoestadistica objetivo predictivo. 
# Predecir donde no hemos medido.
# Tecnica mas conocida krigging, actualmente aprendizaje automatico y predicciones, tiene buenos resultados.
# Tiene muchas ventajas y desventajas, cada base de datos tiene un metodo que mejor ajusta no hay uno por defecto.

# Podemos complicar con prediccion con metodos bayesianos un lineamiento mas complejo y nuevo,
# Son otros paradigmas y con mas restricciones, no tan flexibles como esto.
# Mucho mas costo computacional.