#-----------------------
#Results
#Graphs
#-----------------------

#Cargar librerias
#------------------------------------------------------------------------------------------------
library(tidyverse) #Manipulacion y visualizaci�n de data
library(magrittr) #Manipulaci�n de data
library(lubridate) #Manejo de fechas
library(plotly) #Graficos interactivos
library(summarytools) #Exploraci�n de datos y reporte sencillo
library(dlookr) #Si no funciona reinstalar "rmarkdown". Para diagnosticos generales
library(xtable) #Output en formato latex
library(gtsummary) #Reporte de estad�sticas
library(finalfit) #An�lisis de datos missing
library(naniar) #An�lisis de datos missing
library(car) #Para density plot
library(data.table) #Para analizar datos duplicados
library(ggExtra) #Para realizar gr�ficos m�s complejos con ggplot
library(viridis) #Colores histograma
library(missForest) #Imputaci�n random forest
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
results_total <- results %>% summarise(lwr=sum(Lwr_Atr_Cost_weighted)/(3694.8*1000000), 
                                       mean=sum(Mean_Atr_Cost_weighted)/(3694.8*1000000),
                                       upr=sum(Upr_Atr_Cost_weighted)/(3694.8*1000000),
                                       denominador = sum(Denom_Cost_weighted)/(3694.8*1000000),
                                       percentage = mean/denominador)


#Gr�ficos - costos por sexo y edad
results_age_sex <- results %>% group_by(sexo, grupo_edad) %>% summarise(lwr=sum(Lwr_Atr_Cost_weighted)/(3694.8*1000000), 
                                                                        mean=sum(Mean_Atr_Cost_weighted)/(3694.8*1000000),
                                                                        upr=sum(Upr_Atr_Cost_weighted)/(3694.8*1000000),
                                                                        denominador = sum(Denom_Cost_weighted)/(3694.8*1000000),
                                                                        percentage = mean/denominador)

#Gr�ficos - costos por enfermedad
results_enfermedad <- results %>% group_by(enfermedad) %>% summarise(lwr=sum(Lwr_Atr_Cost_weighted)/(3694.8*1000000), 
                                                                     mean=sum(Mean_Atr_Cost_weighted)/(3694.8*1000000),
                                                                     upr=sum(Upr_Atr_Cost_weighted)/(3694.8*1000000),
                                                                     denominador = sum(Denom_Cost_weighted)/(3694.8*1000000),
                                                                     percentage = mean/denominador)

#Resultados - costos por sexo
results_sexo <- results %>% group_by(sexo) %>% summarise(lwr=sum(Lwr_Atr_Cost_weighted)/(3694.8*1000000), 
                                                         mean=sum(Mean_Atr_Cost_weighted)/(3694.8*1000000),
                                                         upr=sum(Upr_Atr_Cost_weighted)/(3694.8*1000000),
                                                         denominador = sum(Denom_Cost_weighted)/(3694.8*1000000),
                                                         percentage = mean/denominador)

#Resultados - costos por edad
results_edad <- results %>% group_by(grupo_edad) %>% summarise(lwr=sum(Lwr_Atr_Cost_weighted)/(3694.8*1000000), 
                                                               mean=sum(Mean_Atr_Cost_weighted)/(3694.8*1000000),
                                                               upr=sum(Upr_Atr_Cost_weighted)/(3694.8*1000000),
                                                               denominador = sum(Denom_Cost_weighted)/(3694.8*1000000),
                                                               percentage = mean/denominador)

#------------------------------------------------------------------------------------------------


#Gr�ficos
library(data.table)
results_age_sex$denominador<-results_age_sex$denominador-results_age_sex$mean
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

#Gr�ficos
pl <- ggplot(data = long,aes(x=sexo, y = costo, fill = factor(id_tipo_cost, levels=c("denominador","mean"))))
pl <- pl + geom_bar(stat="identity",color = "black")
pl <- pl+scale_fill_manual(values = c("azure2","cadetblue3"))
pl<- pl+theme_bw()
pl<- pl+theme(legend.position = "none")
#pl <- pl+  theme(panel.grid.minor = element_blank()) 
#pl <- pl + geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
#position=position_stack(vjust=0.5), colour="white", size = 2)
pl <- pl  + ylab("SLNOD financial cost (2020 million USD)")
pl <- pl + facet_grid(.~grupo_edad)
pl

pl + geom_errorbar(aes(ymin=lwr, ymax=upr), size=0.5,   
                   width=.25,position=position_dodge(.1)) +
  facet_grid(~grupo_edad)


#Mismo gr�fico para diseases:
library(data.table)
results_enfermedad$denominador<-results_enfermedad$denominador-results_enfermedad$mean
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
level_order = c("Pancreatic cancer","Tracheal, bronchus, and lung cancer","Cervical Cancer","Chronic obstructive pulmonary disease","Stomach cancer","Lower respiratory infections","Stroke","Ischemic heart disease")
#Gr�ficos
pl <- ggplot(data = long,aes(x=factor(enfermedad, level=level_order), y = costo, fill = factor(id_tipo_cost, levels=c("denominador","mean"))))
pl <- pl + geom_bar(stat="identity",color = "black")
pl <- pl+scale_fill_manual(values = c("azure2","cadetblue3"))
pl<- pl+theme_bw()
pl<- pl+theme(legend.position = "none")
#pl <- pl+  theme(panel.grid.minor = element_blank()) 
#pl <- pl + geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
#position=position_stack(vjust=0.5), colour="white", size = 2)
pl <- pl  + ylab("SLNOD financial cost (2020 million USD)")
pl<-pl + geom_errorbar(aes(ymin=lwr, ymax=upr), size=0.5,   
                       width=.25,position=position_dodge(.1))
pl<-pl +scale_x_discrete(guide = guide_axis(n.dodge=1))
pl <- pl +coord_flip()
pl <- pl+scale_y_continuous(labels = scientific)
pl
