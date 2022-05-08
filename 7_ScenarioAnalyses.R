#-----------------------------
#Scenario Analysis
#By wage and days
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


#Bootstrap estratificado cuartil más bajo del ingreso base de cotización
#-------------------------------------------------------------------------------------------------



disease <- c("Ischemic heart disease","Chronic obstructive pulmonary disease","Lower respiratory infections","Stroke","Tracheal, bronchus, and lung cancer","Stomach cancer","Pancreatic cancer","Cervical cancer")
disease_df <- c("Ischemic heart disease","Chronic obstructive pulmonary disease","Lower respiratory infections","Stroke","Tracheal, bronchus, and lung cancer","Stomach cancer","Pancreatic cancer")
age_group <- c("30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70+ years")
sex <- c("Male","Female")

data_frame_costo <- data.frame(Disease = c(rep(disease_df,each=18), rep("Cervical Cancer",9)), Age_group = c(rep(rep(age_group,each=2),7),age_group), Sex = c(rep(sex,7*9),rep("Female",9)))
data_frame_dias <- data.frame(Disease = c(rep(disease_df,each=18), rep("Cervical Cancer",9)), Age_group = c(rep(rep(age_group,each=2),7),age_group), Sex = c(rep(sex,7*9),rep("Female",9)))

columna<-0

c <- matrix(0,135,50)
d <- matrix(0,135,50)


for(l in disease){
  for(j in age_group){
    if(l == "Cervical cancer"){
      set.seed(123)
      grupo <- data_id %>% filter(enfermedad == l & grupo_edad == j & sexo == "Female")
      grupo_q <- grupo %>% filter(quartile==1) #lowest
      paf_val <- pafs_2016 %>% filter(cause_name == l & age_name == j & sex_name == "Female") %>% select(val)
      sample_n <- as.numeric(floor(paf_val*nrow(grupo)))
      prueba_valor<-lapply(1:50, function(i) sample(grupo_q$valor_incapacidad_sum,sample_n,replace=T))
      valor <- sapply(prueba_valor, sum)
      columna=columna+1
      print(c(columna,l,j,k))
      #Organizar en el data.frame
      c[columna,]<- valor
    }
    else{
      for(k in sex){
        set.seed(123)
        grupo <- data_id %>% filter(enfermedad == l & grupo_edad == j & sexo == k)
        grupo_q <- grupo %>% filter(quartile==1) #lowest
        paf_val <- pafs_2016 %>% filter(cause_name == l & age_name == j & sex_name == k) %>% select(val)
        sample_n <- as.numeric(floor(paf_val*nrow(grupo)))
        prueba_valor<-lapply(1:50, function(i) sample(grupo_q$valor_incapacidad_sum,sample_n,replace=TRUE))
        valor <- sapply(prueba_valor, sum)
        columna=columna+1
        print(c(columna,l,j,k))
        #Organizar en el data.frame
        c[columna,]<- valor
      }
    }
  }
}

#set.seed(123)
#grupo <- data_id %>% filter(enfermedad == "Ischemic heart disease" & grupo_edad == "30 to 34" & sexo == "Male")
#paf_val <- pafs_2016 %>% filter(cause_name == "Ischemic heart disease" & age_name == "30 to 34" & sex_name == "Male") %>% select(val)
#prueba_valor<-lapply(1:50, function(i) sample(grupo$valor_incapacidad_sum,as.numeric(floor(paf_val*nrow(grupo))),replace=FALSE))
#valor <- sapply(prueba_valor, sum)
#valor

#Unir a df creados anteriormente
costo_df_q1 <- c
costo_df_q1 %<>% as.data.frame()



data_frame_costo_q1 <- cbind(data_frame_costo,costo_df_q1)


#Guardar dataframes
write.csv(data_frame_costo_q1, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/data_frame_costo_q1.csv')


#-------------------------------------------------------------------------------------------------


#Bootstrap estratificado cuartil más alto del ingreso base de cotización
#-------------------------------------------------------------------------------------------------



disease <- c("Ischemic heart disease","Chronic obstructive pulmonary disease","Lower respiratory infections","Stroke","Tracheal, bronchus, and lung cancer","Stomach cancer","Pancreatic cancer","Cervical cancer")
disease_df <- c("Ischemic heart disease","Chronic obstructive pulmonary disease","Lower respiratory infections","Stroke","Tracheal, bronchus, and lung cancer","Stomach cancer","Pancreatic cancer")
age_group <- c("30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70+ years")
sex <- c("Male","Female")

data_frame_costo <- data.frame(Disease = c(rep(disease_df,each=18), rep("Cervical Cancer",9)), Age_group = c(rep(rep(age_group,each=2),7),age_group), Sex = c(rep(sex,7*9),rep("Female",9)))
data_frame_dias <- data.frame(Disease = c(rep(disease_df,each=18), rep("Cervical Cancer",9)), Age_group = c(rep(rep(age_group,each=2),7),age_group), Sex = c(rep(sex,7*9),rep("Female",9)))

columna<-0

c <- matrix(0,135,50)
d <- matrix(0,135,50)


for(l in disease){
  for(j in age_group){
    if(l == "Cervical cancer"){
      set.seed(123)
      grupo <- data_id %>% filter(enfermedad == l & grupo_edad == j & sexo == "Female")
      grupo_q <- grupo %>% filter(quartile==4) #highest
      paf_val <- pafs_2016 %>% filter(cause_name == l & age_name == j & sex_name == "Female") %>% select(val)
      sample_n <- as.numeric(floor(paf_val*nrow(grupo)))
      if(nrow(grupo_q)>0){
        prueba_valor<-lapply(1:50, function(i) sample(grupo_q$valor_incapacidad_sum,sample_n,replace=T))
        valor <- sapply(prueba_valor, sum)
        columna=columna+1
        print(c(columna,l,j,k))
        #Organizar en el data.frame
        c[columna,]<- valor
      }
      else{
        columna=columna+1
        print(c(columna,l,j,k))
        c[columna,]<- 0
      }
    }
    else{
      for(k in sex){
        set.seed(123)
        grupo <- data_id %>% filter(enfermedad == l & grupo_edad == j & sexo == k)
        grupo_q <- grupo %>% filter(quartile==4) #highest
        paf_val <- pafs_2016 %>% filter(cause_name == l & age_name == j & sex_name == k) %>% select(val)
        sample_n <- as.numeric(floor(paf_val*nrow(grupo)))
        if(nrow(grupo_q)>0){
          prueba_valor<-lapply(1:50, function(i) sample(grupo_q$valor_incapacidad_sum,sample_n,replace=TRUE))
          valor <- sapply(prueba_valor, sum)
          columna=columna+1
          print(c(columna,l,j,k))
          #Organizar en el data.frame
          c[columna,]<- valor
        }
        else{
          columna=columna+1
          print(c(columna,l,j,k))
          c[columna,]<- 0
        }
      }
    }
  }
}

#set.seed(123)
#grupo <- data_id %>% filter(enfermedad == "Ischemic heart disease" & grupo_edad == "30 to 34" & sexo == "Male")
#paf_val <- pafs_2016 %>% filter(cause_name == "Ischemic heart disease" & age_name == "30 to 34" & sex_name == "Male") %>% select(val)
#prueba_valor<-lapply(1:50, function(i) sample(grupo$valor_incapacidad_sum,as.numeric(floor(paf_val*nrow(grupo))),replace=FALSE))
#valor <- sapply(prueba_valor, sum)
#valor

#Unir a df creados anteriormente
costo_df_q4 <- c
costo_df_q4 %<>% as.data.frame()



data_frame_costo_q4 <- cbind(data_frame_costo,costo_df_q4)


#Guardar dataframes
write.csv(data_frame_costo_q4, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/data_frame_costo_q4.csv')

#-------------------------------------------------------------------------------------------------