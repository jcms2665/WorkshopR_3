
library(rgdal)
library(sp)
library(GISTools)
library(RColorBrewer)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(foreign)
library(spdep)



# Lista de librerías:
list.of.packages <- c("rgdal", "sp", "GISTools", "RColorBrewer", "ggplot2",
                      "reshape2", "grid", "gridExtra")
# Ver qué no está instalado
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Si falta algo, instalarlo
if (length(new.packages)) install.packages(new.packages)




# Cambiar directorio de trabajo

setwd("C:/Users/jmartinez/Desktop/ANA/teTra-Red-master/teTra-Red-master")

robos <- readOGR("data", "robos")

colnames(robos@data)

# Definir márgenes para ocupar todo el espacio
par(mar = c(0, 0, 1.5, 0))

# Definir un esquema para colorear el mapa de acuerdo a desviaciones
# estándar
shades <- auto.shading(robos$N_robos, cutter = sdCuts, n = 6, cols = rev(brewer.pal(6,"RdYlBu")))

# Definimos el mapa temático
choropleth(robos, robos$N_robos, shades)

# Agregar título
title(main = "Número de robos",cex.main = 0.75)




#2. Índice de Mooran


# Librerías:
list.of.packages <- c("spdep")

# Ver qué no está instalado
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,
                                                                              "Package"])]
# Si falta algo, instalarlo
if (length(new.packages)) install.packages(new.packages)

#Agregar una capa
frontera <- readOGR("data", "BOUNDARY")

# Calcular los vecinos más cercanos 
k1 <- knn2nb(knearneigh(robos, k = 1, longlat = TRUE))

# Calcular distancias de los vecinos más cercanos
distancia <- max(unlist(nbdists(k1, robos)))

# Encontrar vecinos
vecinos <- dnearneigh(robos, 0, distancia)

# Lista de pesos
robos.lw <- nb2listw(vecinos) 

# Estandarizar valores 
robos$zScaled <- scale(robos$N_robos)

# Calcular la variable de retraso y salvarla
robos$lagZ <- lag.listw(robos.lw, robos$N_robos)

# Diagrama de dispersión
plot(robos$zScaled, robos$lagZ)

# Ejes que pasan por el origen
abline(h = 0, v = 0)

# Recta de ajuste lineal entre las dos variables
abline(lm(robos$lagZ ~ robos$zScaled), lty = 2, lwd = 2, col = "red")

title("Diagrama de dispersión de Moran")

# Con variables estandarizadas
plot(robos$zScaled,robos$lagZ-mean(robos$lagZ))

# o bien, plot(robos$zScaled,scale(robos$lagZ))
abline(h = 0, v = 0)
abline(lm(robos$lagZ-mean(robos$lagZ) ~ robos$zScaled), lty = 2, lwd = 2, col = "red")
title("Diagrama de dispersión de Moran")

#Índice de Mooran
moran.test(robos$N_robos, robos.lw)


# Calcular la I de Moran local
lmoran <- localmoran(robos$N_robos, robos.lw)

# Mapa de cúmulos
# Definir vector de cuadrante
cuadrante <- vector(mode = "numeric", length = nrow(lmoran))

# Definir significancia
significancia <- 0.05

# Centrar la variable de interés alrededor de su media
centerZ <- scale(robos$N_robos) #Es lo mismo que (robos$Z-mean(robos$Z))/sd(robos$Z)

# Centrar el valor de la I de Moran local alrededor de su media
centerLag <- robos$lagZscaled


# Colores para las significancias
colorsPValue <- c(rgb(0.74, 0.74, 0.74), rgb(0.22, 0.98, 0.3), rgb(0, 0.75,
                                                                   0), rgb(0, 0.44, 0), rgb(0, 0, 0))
# gris, verde claro, verde medio, verde oscuro, negro


# Segundo mapa Definir márgenes para ocupar todo el espacio
par(mar = c(0, 0, 1, 0))

# Definir vector de significancias
pValues <- vector(mode = "numeric", length = nrow(lmoran))

# Definir niveles de significancia
pValues[(lmoran[, 5] > 0.05)] <- 0 
pValues[(lmoran[, 5] <= 0.05)] <- 4
pValues[(lmoran[, 5] <= 0.01)] <- 3
pValues[(lmoran[, 5] <= 0.001)] <- 2
pValues[(lmoran[, 5] <= 0.0001)] <- 1


plot(frontera)
# Plot not significant
plot(robos[pValues == 0, ], col = rgb(0.74, 0.74, 0.74, alpha = 0.2), add = T,
     pch = 16, cex = 0.75)
# Plot 0.05
plot(robos[pValues == 1, ], col = rgb(0.22, 0.98, 0.3, alpha = 0.2), add = T,
     pch = 16, cex = 0.75)
# Plot 0.01
plot(robos[pValues == 2, ], col = rgb(0, 0.75, 0, alpha = 0.2), add = T, pch = 16,
     cex = 0.75)
# Plot 0.001
plot(robos[pValues == 3, ], col = rgb(0, 0.44, 0, alpha = 0.2), add = T, pch = 16,
     cex = 0.75)
# Plot 0.0001
plot(robos[pValues == 4, ], col = rgb(0, 0, 0, alpha = 0.75), add = T, pch = 16,
     cex = 0.75)
legend("right", legend = c("No significativo", "p = 0.05", "p = 0.01", "p = 0.001",
                           "p = 0.0001"), fill = colorsPValue, bty = "n", cex = 0.7, y.intersp = 1,
       x.intersp = 1)
title("p-value Local")
