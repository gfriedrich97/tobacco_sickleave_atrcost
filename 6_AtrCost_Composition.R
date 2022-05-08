#-----------------------------
#Composition attributable costs by disease 
#2016, 2017, 2018
#-----------------------------

#Limpiar memoria
rm(list=ls())

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

#Data
#------------------------------------------------------------------------------------------------
#Datos para los modelos
model_data_2016 <- read_csv("~/MEPI/Semestre 4/PG2/Datos/model_data_2016.csv")
length(unique(model_data_2016$anonimo_ide))
model_data_2017 <- read_csv("~/MEPI/Semestre 4/PG2/Datos/model_data_2017.csv")
length(unique(model_data_2017$anonimo_ide))
model_data_2018 <- read_csv("~/MEPI/Semestre 4/PG2/Datos/model_data_2018.csv")
length(unique(model_data_2018$anonimo_ide))

model_data <- rbind(model_data_2016,model_data_2017,model_data_2018)
#------------------------------------------------------------------------------------------------





#Días incapacidad totales por enfermedad al año
#------------------------------------------------------------------------------------------------
summary(model_data_2016$diastotales)

#Boxplot días - 2016 a 2018
par(cex.axis=0.5)
boxplot(diastotales ~ enfermedad, data = model_data,outline=FALSE,horizontal = F, ylim=c(3,290),
        col = c("#A6D13A","#DAF7A6", "#FFC300","#F57C00","#FF5733","#C70039","#900C3F","#581845","#F57C00"))

#remotes::install_github("R-CoderDotCom/ridgeline@main")
library(ridgeline)
ridgeline(model_data$diastotales, model_data$enfermedad)
#Percentage bar char: 
#3-5 días
#6-13 días
#14-30 días
#> 30 días

model_data %<>%
  mutate(cat_diastotales=ifelse(diastotales <=5, "3-5 días",
                                ifelse(diastotales >5 & diastotales<=13, "6-13 días",
                                ifelse(diastotales >13 & diastotales <=30, "14-30 días","> 30 días"))))
freq(model_data$cat_diastotales)


stacked_days <-model_data %>%
  group_by(enfermedad, cat_diastotales) %>%
  tally() %>%
  mutate(prop = prop.table(n)) 



#Check codigo
#model_data %>% filter(enfermedad=="Cervical cancer") %>% freq(cat_diastotales)
par(cex.axis=0.2)
ggplot(stacked_days, aes(fill=factor(cat_diastotales, levels=c("> 30 días","14-30 días","6-13 días","3-5 días")),
                   x=factor(enfermedad,levels=c("Lower respiratory infections","Chronic obstructive pulmonary disease","Ischemic heart disease","Stroke","Cervical cancer","Tracheal, bronchus, and lung cancer","Pancreatic cancer","Stomach cancer")), y=prop
                   )) + 
  geom_bar(position="fill", stat="identity")+scale_x_discrete(labels = abbreviate)+
  scale_fill_manual("legend", values = c("#FFC300","#F57C00","#FF5733","#C70039"))

#------------------------------------------------------------------------------------------------


#Ingreso por enfermedad
#------------------------------------------------------------------------------------------------
#Boxplot días - 2016 a 2018
par(cex.axis=0.5)
boxplot(ibc ~ enfermedad, data = model_data,outline=FALSE,horizontal = F,
        col = c("#A6D13A","#DAF7A6", "#FFC300","#F57C00","#FF5733","#C70039","#900C3F","#581845","#F57C00"))

#remotes::install_github("R-CoderDotCom/ridgeline@main")
library(ridgeline)
ridgeline(model_data$ibc, model_data$enfermedad)

stacked_wage <-model_data %>%
  group_by(enfermedad, rango_smlv) %>%
  tally() %>%
  mutate(prop = prop.table(n)) 

#Check codigo
#model_data %>% filter(enfermedad=="Cervical cancer") %>% freq(rango_smlv)
par(cex.axis=0.2)
ggplot(stacked_wage, aes(fill=factor(rango_smlv, levels=c(">  5  SMLMV","ENTRE  2  y  5  SMLMV","<  2  SMLMV")),
                   x=factor(enfermedad,levels=c("Lower respiratory infections","Chronic obstructive pulmonary disease","Ischemic heart disease","Stroke","Cervical cancer","Tracheal, bronchus, and lung cancer","Pancreatic cancer","Stomach cancer")), y=prop
)) + 
  geom_bar(position="fill", stat="identity")+scale_x_discrete(labels = abbreviate)+
  scale_fill_manual("legend", values = c("#FFC300","#F57C00","#FF5733","#C70039"))

#------------------------------------------------------------------------------------------------


