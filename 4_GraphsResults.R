#-----------------------
#Results
#Graphs
#-----------------------

#Cargar librerias
#------------------------------------------------------------------------------------------------
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
library(car) #Para density plot
library(data.table) #Para analizar datos duplicados
library(ggExtra) #Para realizar gráficos más complejos con ggplot
library(viridis) #Colores histograma
library(missForest) #Imputación random forest
library(hrbrthemes) #Graficas
library(dplyr)
library(dvmisc)#dividir en quantiles
library(fastDummies)#Crear dummies

#------------------------------------------------------------------------------------------------


rm(list=ls())

#Data
#------------------------------------------------------------------------------------------------
#Datos para los modelos
All_data <- read_csv("~/MEPI/Semestre 4/PG2/Datos/All_data.csv") %>% select(-"X1")
colnames(All_data)[1:3]<-c("sexo","grupo_edad","enfermedad")

#------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------

results <- All_data %>% select(sexo,grupo_edad,enfermedad,Denom_Cost_weighted,
                               Mean_Atr_Cost_weighted,Lwr_Atr_Cost_weighted,Upr_Atr_Cost_weighted)


#Resultados - Total
results_total <- results %>% summarise(lwr=sum(Lwr_Atr_Cost_weighted), 
                                       mean=sum(Mean_Atr_Cost_weighted),
                                       upr=sum(Upr_Atr_Cost_weighted),
                                       denominador = sum(Denom_Cost_weighted),
                                       percentage = mean/denominador)


#Gráficos - costos por sexo y edad
results_age_sex <- results %>% group_by(sexo, grupo_edad) %>% summarise(lwr=sum(Lwr_Atr_Cost_weighted), 
                                                                        mean=sum(Mean_Atr_Cost_weighted),
                                                                        upr=sum(Upr_Atr_Cost_weighted),
                                                                        denominador = sum(Denom_Cost_weighted),
                                                                        percentage = mean/denominador)

#Gráficos - costos por enfermedad
results_enfermedad <- results %>% group_by(enfermedad) %>% summarise(lwr=sum(Lwr_Atr_Cost_weighted), 
                                                                     mean=sum(Mean_Atr_Cost_weighted),
                                                                     upr=sum(Upr_Atr_Cost_weighted),
                                                                     denominador = sum(Denom_Cost_weighted),
                                                                     percentage = mean/denominador)

#Resultados - costos por sexo
results_sexo <- results %>% group_by(sexo) %>% summarise(lwr=sum(Lwr_Atr_Cost_weighted), 
                                                         mean=sum(Mean_Atr_Cost_weighted),
                                                         upr=sum(Upr_Atr_Cost_weighted),
                                                         denominador = sum(Denom_Cost_weighted),
                                                         percentage = mean/denominador)

#Resultados - costos por edad
results_edad <- results %>% group_by(grupo_edad) %>% summarise(lwr=sum(Lwr_Atr_Cost_weighted), 
                                                               mean=sum(Mean_Atr_Cost_weighted),
                                                               upr=sum(Upr_Atr_Cost_weighted),
                                                               denominador = sum(Denom_Cost_weighted),
                                                               percentage = mean/denominador)

#------------------------------------------------------------------------------------------------


#Gráficos
library(data.table)
long_mean <- results_age_sex %>% select(grupo_edad,sexo,mean)
colnames(long_mean) <- c("grupo_edad","sexo","costo")
long_den <- results_age_sex %>% select(grupo_edad,sexo,denominador)
colnames(long_den) <- c("grupo_edad","sexo","costo")
long <- rbind(long_mean,long_den)
long$id_tipo_cost <- rep(c("mean","denominador"),each=18)
long_lwr <- c(results_age_sex$lwr,rep(0, each=18))
long_upr <- c(results_age_sex$upr,rep(0, each=18))
long$lwr <- long_lwr
long$upr <- long_upr
long$id_tipo_cost %<>% as.factor()

#Gráficos
pl <- ggplot(data = long,aes(x=sexo, y = costo, fill = factor(id_tipo_cost, levels=c("denominador","mean"))))
pl <- pl + geom_bar(stat="identity",color = "black")
pl <- pl+scale_fill_manual(values = c("azure2","cadetblue3"))
pl<- pl+theme_bw()
pl<- pl+theme(legend.position = "none")
#pl <- pl+  theme(panel.grid.minor = element_blank()) 
#pl <- pl + geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
#position=position_stack(vjust=0.5), colour="white", size = 2)
pl <- pl  + ylab("SLNOD financial cost (2020 Billion COP)")
pl <- pl + facet_grid(.~grupo_edad)
pl

pl + geom_errorbar(aes(ymin=lwr, ymax=upr), size=0.5,   
                   width=.25,position=position_dodge(.1)) +
  facet_grid(~grupo_edad)


#Mismo gráfico para diseases:
library(data.table)
long_mean <- results_enfermedad %>% select(enfermedad,mean)
colnames(long_mean) <- c("enfermedad","costo")
long_den <- results_enfermedad %>% select(enfermedad,denominador)
colnames(long_den) <- c("enfermedad","costo")
long <- rbind(long_mean,long_den)
long$id_tipo_cost <- rep(c("mean","denominador"),each=8)
long_lwr <- c(results_enfermedad$lwr,rep(0, each=8))
long_upr <- c(results_enfermedad$upr,rep(0, each=8))
long$lwr <- long_lwr
long$upr <- long_upr
long$id_tipo_cost %<>% as.factor()

library(scales)
level_order = c("Pancreatic cancer","Cervical Cancer","Tracheal, bronchus, and lung cancer","Chronic obstructive pulmonary disease","Stomach cancer","Lower respiratory infections","Stroke","Ischemic heart disease")
#Gráficos
pl <- ggplot(data = long,aes(x=factor(enfermedad, level=level_order), y = costo, fill = factor(id_tipo_cost, levels=c("denominador","mean"))))
pl <- pl + geom_bar(stat="identity",color = "black")
pl <- pl+scale_fill_manual(values = c("azure2","cadetblue3"))
pl<- pl+theme_bw()
pl<- pl+theme(legend.position = "none")
#pl <- pl+  theme(panel.grid.minor = element_blank()) 
#pl <- pl + geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
#position=position_stack(vjust=0.5), colour="white", size = 2)
pl <- pl  + ylab("SLNOD financial cost (2020 Billion COP)")
pl<-pl + geom_errorbar(aes(ymin=lwr, ymax=upr), size=0.5,   
                       width=.25,position=position_dodge(.1))
pl<-pl +scale_x_discrete(guide = guide_axis(n.dodge=1))
pl <- pl +coord_flip()
pl <- pl+scale_y_continuous(labels = scientific)
pl
