#-----------------------------
#Atributable costs 
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
model_data_2017 <- read_csv("~/MEPI/Semestre 4/PG2/Datos/model_data_2017.csv")
model_data_2018 <- read_csv("~/MEPI/Semestre 4/PG2/Datos/model_data_2018.csv")

#Fracciones poblacionales atribuibles - gráficos 2016
pafs <- read.csv("C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/PAFS_2008_2016_2018/IHME-GBD_2019_DATA-253b79f4-1.csv")
pafs %<>% filter(metric_name=="Percent")
pafs %<>% select(sex_name, age_name, cause_name, year, val, upper, lower) 

pafs %<>% filter(cause_name=="Lower respiratory infections" | cause_name=="Chronic obstructive pulmonary disease" | 
                   cause_name=="Cervical cancer" | cause_name == "Ischemic heart disease" | cause_name=="Pancreatic cancer" | 
                   cause_name=="Stomach cancer" | cause_name =="Stroke" | cause_name =="Tracheal, bronchus, and lung cancer")

pafs_2016 <- filter(pafs, year==2016)
#Smoking: https://ghdx.healthdata.org/gbd-results-tool?params=gbd-api-2019-permalink/a6ff27feb447f3b05e05f3dd9002e05c
#Deaths general: https://ghdx.healthdata.org/gbd-results-tool?params=gbd-api-2019-permalink/8721c469f41e354da35e4f29b4bae82d 
#------------------------------------------------------------------------------------------------


#Check number of observations by sampling groups
#------------------------------------------------------------------------------------------------
n_grupos_2016 <- model_data_2016 %>% group_by(enfermedad, grupo_edad, sexo) %>% summarise(n=n())
min(n_grupos_2016$n)
n_grupos_2017 <- model_data_2017 %>% group_by(enfermedad, grupo_edad, sexo) %>% summarise(n=n())
min(n_grupos_2017$n)
n_grupos_2018 <- model_data_2018 %>% group_by(enfermedad, grupo_edad, sexo) %>% summarise(n=n())
min(n_grupos_2018$n)
#------------------------------------------------------------------------------------------------


#Select atributable cases - 2016
#------------------------------------------------------------------------------------------------
disease <- c("Ischemic heart disease","Chronic obstructive pulmonary disease","Lower respiratory infections","Stroke","Tracheal, bronchus, and lung cancer","Stomach cancer","Pancreatic cancer","Cervical cancer")
disease_df <- c("Ischemic heart disease","Chronic obstructive pulmonary disease","Lower respiratory infections","Stroke","Tracheal, bronchus, and lung cancer","Stomach cancer","Pancreatic cancer")
age_group <- c("30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70+ years")
sex <- c("Male","Female")

data_frame_costo <- data.frame(Disease = c(rep(disease_df,each=18), rep("Cervical Cancer",9)), Age_group = c(rep(rep(age_group,each=2),7),age_group), Sex = c(rep(sex,7*9),rep("Female",9)))
data_frame_dias <- data.frame(Disease = c(rep(disease_df,each=18), rep("Cervical Cancer",9)), Age_group = c(rep(rep(age_group,each=2),7),age_group), Sex = c(rep(sex,7*9),rep("Female",9)))

columna<-0

c <- matrix(0,135,1000)
d <- matrix(0,135,1000)
n_atr <- matrix(0,135,1000)


for(l in disease){
  for(j in age_group){
    if(l == "Cervical cancer"){
      set.seed(123)
      grupo <- model_data_2016 %>% filter(enfermedad == l & grupo_edad == j & sexo == "Female")
      paf_val <- pafs_2016 %>% filter(cause_name == l & age_name == j & sex_name == "Female") %>% select(val)
      n_select <- as.numeric(unlist(floor(paf_val*nrow(grupo))))
      if(n_select==0){
        valor <- 0
        dias<-0 
        columna=columna+1
        print(c(columna,l,j,k))
        c[columna,]<- valor
        d[columna,]<- dias
        n_atr[columna,]<-n_select
      }else{
      prueba_valor<-lapply(1:1000, function(i) sample(grupo$valor_incapacidad_sum,n_select,replace=FALSE))  
      prueba_dias<-lapply(1:1000, function(i) sample(grupo$diastotales,n_select,replace=FALSE))
      valor <- sapply(prueba_valor, sum)
      dias <- sapply(prueba_dias, sum)
      columna=columna+1
      print(c(columna,l,j,k))
      c[columna,]<- valor
      d[columna,]<- dias
      n_atr[columna,]<-n_select
      #Organizar en el data.frame
      #print(paf_val,n,valor,dias)
      }
    }
    else{
      for(k in sex){
        set.seed(123)
        grupo <- model_data_2016 %>% filter(enfermedad == l & grupo_edad == j & sexo == k)
        paf_val <- pafs_2016 %>% filter(cause_name == l & age_name == j & sex_name == k) %>% select(val)
        n_select <- as.numeric(unlist(floor(paf_val*nrow(grupo))))
        if(n_select==0){
          valor <- 0
          dias<-0 
          columna=columna+1
          print(c(columna,l,j,k))
          c[columna,]<- valor
          d[columna,]<- dias
          n_atr[columna,]<-n_select
        }else{
          prueba_valor<-lapply(1:1000, function(i) sample(grupo$valor_incapacidad_sum,n_select,replace=FALSE))  
          prueba_dias<-lapply(1:1000, function(i) sample(grupo$diastotales,n_select,replace=FALSE))
          valor <- sapply(prueba_valor, sum)
          dias <- sapply(prueba_dias, sum)
          columna=columna+1
          print(c(columna,l,j,k))
          c[columna,]<- valor
          d[columna,]<- dias
          n_atr[columna,]<-n_select
          #Organizar en el data.frame
          #print(paf_val,n,valor,dias)
        }
      }
    }
  }
}

#set.seed(123)
#grupo <- model_data_2016 %>% filter(enfermedad == "Ischemic heart disease" & grupo_edad == "30 to 34" & sexo == "Male")
#paf_val <- pafs_2016 %>% filter(cause_name == "Ischemic heart disease" & age_name == "30 to 34" & sex_name == "Male") %>% select(val)
#prueba_valor<-lapply(1:50, function(i) sample(grupo$valor_incapacidad_sum,as.numeric(floor(paf_val*nrow(grupo))),replace=FALSE))
#valor <- sapply(prueba_valor, sum)
#valor

#Unir a df creados anteriormente
costo_df <- c
costo_df %<>% as.data.frame()
dias_df <- d
dias_df %<>% as.data.frame()
n_atr %<>% as.data.frame()

data_frame_costos <- cbind(data_frame_costo,costo_df)
data_frame_dias <- cbind(data_frame_dias,dias_df)
n_atr_bind <- cbind(data_frame_costo,n_atr)

#Guardar dataframes
write.csv(data_frame_costos, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/atr_cost_2016.csv')
write.csv(data_frame_dias, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/atr_days_2016.csv')
write.csv(n_atr_bind, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/atr_cases_2016.csv')

#------------------------------------------------------------------------------------------------


#Select atributable cases - 2017
#------------------------------------------------------------------------------------------------
disease <- c("Ischemic heart disease","Chronic obstructive pulmonary disease","Lower respiratory infections","Stroke","Tracheal, bronchus, and lung cancer","Stomach cancer","Pancreatic cancer","Cervical cancer")
disease_df <- c("Ischemic heart disease","Chronic obstructive pulmonary disease","Lower respiratory infections","Stroke","Tracheal, bronchus, and lung cancer","Stomach cancer","Pancreatic cancer")
age_group <- c("30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70+ years")
sex <- c("Male","Female")

data_frame_costo <- data.frame(Disease = c(rep(disease_df,each=18), rep("Cervical Cancer",9)), Age_group = c(rep(rep(age_group,each=2),7),age_group), Sex = c(rep(sex,7*9),rep("Female",9)))
data_frame_dias <- data.frame(Disease = c(rep(disease_df,each=18), rep("Cervical Cancer",9)), Age_group = c(rep(rep(age_group,each=2),7),age_group), Sex = c(rep(sex,7*9),rep("Female",9)))

columna<-0

c <- matrix(0,135,1000)
d <- matrix(0,135,1000)
n_atr <- matrix(0,135,1000)


for(l in disease){
  for(j in age_group){
    if(l == "Cervical cancer"){
      set.seed(123)
      grupo <- model_data_2017 %>% filter(enfermedad == l & grupo_edad == j & sexo == "Female")
      paf_val <- pafs_2016 %>% filter(cause_name == l & age_name == j & sex_name == "Female") %>% select(val)
      n_select <- as.numeric(unlist(floor(paf_val*nrow(grupo))))
      if(n_select==0){
        valor <- 0
        dias<-0 
        columna=columna+1
        print(c(columna,l,j,k))
        c[columna,]<- valor
        d[columna,]<- dias
        n_atr[columna,]<-n_select
      }else{
        prueba_valor<-lapply(1:1000, function(i) sample(grupo$valor_incapacidad_sum,n_select,replace=FALSE))  
        prueba_dias<-lapply(1:1000, function(i) sample(grupo$diastotales,n_select,replace=FALSE))
        valor <- sapply(prueba_valor, sum)
        dias <- sapply(prueba_dias, sum)
        columna=columna+1
        print(c(columna,l,j,k))
        c[columna,]<- valor
        d[columna,]<- dias
        n_atr[columna,]<-n_select
        #Organizar en el data.frame
        #print(paf_val,n,valor,dias)
      }
    }
    else{
      for(k in sex){
        set.seed(123)
        grupo <- model_data_2017 %>% filter(enfermedad == l & grupo_edad == j & sexo == k)
        paf_val <- pafs_2016 %>% filter(cause_name == l & age_name == j & sex_name == k) %>% select(val)
        n_select <- as.numeric(unlist(floor(paf_val*nrow(grupo))))
        if(n_select==0){
          valor <- 0
          dias<-0 
          columna=columna+1
          print(c(columna,l,j,k))
          c[columna,]<- valor
          d[columna,]<- dias
          n_atr[columna,]<-n_select
        }else{
          prueba_valor<-lapply(1:1000, function(i) sample(grupo$valor_incapacidad_sum,n_select,replace=FALSE))  
          prueba_dias<-lapply(1:1000, function(i) sample(grupo$diastotales,n_select,replace=FALSE))
          valor <- sapply(prueba_valor, sum)
          dias <- sapply(prueba_dias, sum)
          columna=columna+1
          print(c(columna,l,j,k))
          c[columna,]<- valor
          d[columna,]<- dias
          n_atr[columna,]<-n_select
          #Organizar en el data.frame
          #print(paf_val,n,valor,dias)
        }
      }
    }
  }
}

#set.seed(123)
#grupo <- model_data_2016 %>% filter(enfermedad == "Ischemic heart disease" & grupo_edad == "30 to 34" & sexo == "Male")
#paf_val <- pafs_2016 %>% filter(cause_name == "Ischemic heart disease" & age_name == "30 to 34" & sex_name == "Male") %>% select(val)
#prueba_valor<-lapply(1:50, function(i) sample(grupo$valor_incapacidad_sum,as.numeric(floor(paf_val*nrow(grupo))),replace=FALSE))
#valor <- sapply(prueba_valor, sum)
#valor

#Unir a df creados anteriormente
costo_df <- c
costo_df %<>% as.data.frame()
dias_df <- d
dias_df %<>% as.data.frame()
n_atr %<>% as.data.frame()

data_frame_costos <- cbind(data_frame_costo,costo_df)
data_frame_dias <- cbind(data_frame_dias,dias_df)
n_atr_bind <- cbind(data_frame_costo,n_atr)

#Guardar dataframes
write.csv(data_frame_costos, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/atr_cost_2017.csv')
write.csv(data_frame_dias, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/atr_days_2017.csv')
write.csv(n_atr_bind, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/atr_cases_2017.csv')

#------------------------------------------------------------------------------------------------


#Select atributable cases - 2018
#------------------------------------------------------------------------------------------------
disease <- c("Ischemic heart disease","Chronic obstructive pulmonary disease","Lower respiratory infections","Stroke","Tracheal, bronchus, and lung cancer","Stomach cancer","Pancreatic cancer","Cervical cancer")
disease_df <- c("Ischemic heart disease","Chronic obstructive pulmonary disease","Lower respiratory infections","Stroke","Tracheal, bronchus, and lung cancer","Stomach cancer","Pancreatic cancer")
age_group <- c("30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70+ years")
sex <- c("Male","Female")

data_frame_costo <- data.frame(Disease = c(rep(disease_df,each=18), rep("Cervical Cancer",9)), Age_group = c(rep(rep(age_group,each=2),7),age_group), Sex = c(rep(sex,7*9),rep("Female",9)))
data_frame_dias <- data.frame(Disease = c(rep(disease_df,each=18), rep("Cervical Cancer",9)), Age_group = c(rep(rep(age_group,each=2),7),age_group), Sex = c(rep(sex,7*9),rep("Female",9)))

columna<-0

c <- matrix(0,135,1000)
d <- matrix(0,135,1000)
n_atr <- matrix(0,135,1000)


for(l in disease){
  for(j in age_group){
    if(l == "Cervical cancer"){
      set.seed(123)
      grupo <- model_data_2018 %>% filter(enfermedad == l & grupo_edad == j & sexo == "Female")
      paf_val <- pafs_2016 %>% filter(cause_name == l & age_name == j & sex_name == "Female") %>% select(val)
      n_select <- as.numeric(unlist(floor(paf_val*nrow(grupo))))
      if(n_select==0){
        valor <- 0
        dias<-0 
        columna=columna+1
        print(c(columna,l,j,k))
        c[columna,]<- valor
        d[columna,]<- dias
        n_atr[columna,]<-n_select
      }else{
        prueba_valor<-lapply(1:1000, function(i) sample(grupo$valor_incapacidad_sum,n_select,replace=FALSE))  
        prueba_dias<-lapply(1:1000, function(i) sample(grupo$diastotales,n_select,replace=FALSE))
        valor <- sapply(prueba_valor, sum)
        dias <- sapply(prueba_dias, sum)
        columna=columna+1
        print(c(columna,l,j,k))
        c[columna,]<- valor
        d[columna,]<- dias
        n_atr[columna,]<-n_select
        #Organizar en el data.frame
        #print(paf_val,n,valor,dias)
      }
    }
    else{
      for(k in sex){
        set.seed(123)
        grupo <- model_data_2018 %>% filter(enfermedad == l & grupo_edad == j & sexo == k)
        paf_val <- pafs_2016 %>% filter(cause_name == l & age_name == j & sex_name == k) %>% select(val)
        n_select <- as.numeric(unlist(floor(paf_val*nrow(grupo))))
        if(n_select==0){
          valor <- 0
          dias<-0 
          columna=columna+1
          print(c(columna,l,j,k))
          c[columna,]<- valor
          d[columna,]<- dias
          n_atr[columna,]<-n_select
        }else{
          prueba_valor<-lapply(1:1000, function(i) sample(grupo$valor_incapacidad_sum,n_select,replace=FALSE))  
          prueba_dias<-lapply(1:1000, function(i) sample(grupo$diastotales,n_select,replace=FALSE))
          valor <- sapply(prueba_valor, sum)
          dias <- sapply(prueba_dias, sum)
          columna=columna+1
          print(c(columna,l,j,k))
          c[columna,]<- valor
          d[columna,]<- dias
          n_atr[columna,]<-n_select
          #Organizar en el data.frame
          #print(paf_val,n,valor,dias)
        }
      }
    }
  }
}

#set.seed(123)
#grupo <- model_data_2016 %>% filter(enfermedad == "Ischemic heart disease" & grupo_edad == "30 to 34" & sexo == "Male")
#paf_val <- pafs_2016 %>% filter(cause_name == "Ischemic heart disease" & age_name == "30 to 34" & sex_name == "Male") %>% select(val)
#prueba_valor<-lapply(1:50, function(i) sample(grupo$valor_incapacidad_sum,as.numeric(floor(paf_val*nrow(grupo))),replace=FALSE))
#valor <- sapply(prueba_valor, sum)
#valor

#Unir a df creados anteriormente
costo_df <- c
costo_df %<>% as.data.frame()
dias_df <- d
dias_df %<>% as.data.frame()
n_atr %<>% as.data.frame()

data_frame_costos <- cbind(data_frame_costo,costo_df)
data_frame_dias <- cbind(data_frame_dias,dias_df)
n_atr_bind <- cbind(data_frame_costo,n_atr)

#Guardar dataframes
write.csv(data_frame_costos, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/atr_cost_2018.csv')
write.csv(data_frame_dias, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/atr_days_2018.csv')
write.csv(n_atr_bind, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/atr_cases_2018.csv')

#------------------------------------------------------------------------------------------------





