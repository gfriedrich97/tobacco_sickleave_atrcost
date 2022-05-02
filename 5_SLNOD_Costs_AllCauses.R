#Script análisis base de datos - Tesis Epidemiología 
#Autor: Gabriela Friedrich
#Fecha última modificación: 6/12/2021

rm(list=ls())

library(tidyverse) #Manipulacion y visualización de data
library(magrittr) #Manipulación de data
library(lubridate) #Manejo de fechas
library(plotly) #Graficos interactivos
library(summarytools) #Exploración de datos y reporte sencillo
library(dlookr) #Si no funciona reinstalar "rmarkdown". Para diagnosticos generales
library(xtable) #Output en formato latex
library(gtsummary) #Reporte de estadísticas
library(finalfit) #Análisis de datos missing
library(naniar) #Análisis de datos missing
library(reshape2) #Separar columnas


#Cargar base de datos
data<-read.csv('C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 3/Proyecto de grado I/Datos/Datos R/consolidado.csv')

data %<>%
  mutate(costo_real = ifelse(AÃ.O_VIGENCIA_PAGO==2016, VALOR_INCAPACIDAD/0.88273, ifelse(AÃ.O_VIGENCIA_PAGO==2017, VALOR_INCAPACIDAD/0.94805, VALOR_INCAPACIDAD/0.94805)))

sum(data$costo_real)

cost_denominador <- data %>% group_by(AÃ.O_VIGENCIA_PAGO) %>%
  summarise(costo_denominador = sum(costo_real), n=n(), N = length(unique(ANONIMO_IDE)))

cost_denominador_2016 <- filter(cost_denominador,AÃ.O_VIGENCIA_PAGO=="2016")
cost_denominador_2017 <- filter(cost_denominador,AÃ.O_VIGENCIA_PAGO=="2017")
cost_denominador_2018 <- filter(cost_denominador,AÃ.O_VIGENCIA_PAGO=="2018")


weight_2016 <- sum(cost_denominador_2016$costo_denominador)/sum(sum(cost_denominador_2016$costo_denominador)+sum(cost_denominador_2017$costo_denominador)+sum(cost_denominador_2018$costo_denominador))
weight_2017 <- sum(cost_denominador_2017$costo_denominador)/sum(sum(cost_denominador_2016$costo_denominador)+sum(cost_denominador_2017$costo_denominador)+sum(cost_denominador_2018$costo_denominador))
weight_2018 <- sum(cost_denominador_2018$costo_denominador)/sum(sum(cost_denominador_2016$costo_denominador)+sum(cost_denominador_2017$costo_denominador)+sum(cost_denominador_2018$costo_denominador))

cost_denominador_2016$weight_2016<-weight_2016
cost_denominador_2017$weight_2016<-weight_2017
cost_denominador_2018$weight_2016<-weight_2018

cost_denominador_2016 %<>% mutate(cost_denominador_weighted = costo_denominador*weight_2016)
cost_denominador_2017 %<>% mutate(cost_denominador_weighted = costo_denominador*weight_2017)
cost_denominador_2018 %<>% mutate(cost_denominador_weighted = costo_denominador*weight_2018)

#Mean annual total cost
sum(cost_denominador_2016$cost_denominador_weighted)+sum(cost_denominador_2017$cost_denominador_weighted)+sum(cost_denominador_2018$cost_denominador_weighted)



