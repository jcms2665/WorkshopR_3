library(foreign)
library(TraMineR)
library(cluster)
library(stats)
library(base)
library(questionr)
library(data.table)


# Datos
rm(list = ls())
setwd("C:/Users/jmartinez/Desktop/ANA/teTra-Red-master/teTra-Red-master/data")

# Cargar la base de datos
bar <- read.csv("Barandilla.csv", header=TRUE)

# Seleccionar las variables
variables<-c("sancion","sexo")

#Crear una nueva variable
mini_bar<-bar[,variables]


#Análisis de los datos

cor(mini_bar, use="complete.obs", method="kendall") 


variables<-c("sancion","edad_recod", "resolucion")
mini_bar2<-bar[,variables]

cor(mini_bar2, use="complete.obs", method="kendall") 

