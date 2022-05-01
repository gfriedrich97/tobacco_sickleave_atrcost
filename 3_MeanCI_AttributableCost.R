#---------------------------------------
#Analyzes of attributable cost estimates 
#Distribution
#Mean + CI
#---------------------------------------

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
atr_cost_2016 <- read_csv("~/MEPI/Semestre 4/PG2/Datos/atr_cost_2016.csv") %>% select(-"X1")
atr_cost_2017 <- read_csv("~/MEPI/Semestre 4/PG2/Datos/atr_cost_2017.csv") %>% select(-"X1")
atr_cost_2018 <- read_csv("~/MEPI/Semestre 4/PG2/Datos/atr_cost_2018.csv") %>% select(-"X1")
#------------------------------------------------------------------------------------------------


#Distribution of atr cost by age-sex-disease group - 2016
#------------------------------------------------------------------------------------------------
hist_2016<-atr_cost_2016 %>% pivot_longer(cols = -c("Disease","Age_group","Sex"))
  
hist_2016 %>% filter(Disease=="Ischemic heart disease") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Ischemic heart disease",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2016 %>% filter(Disease=="Chronic obstructive pulmonary disease") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Chronic obstructive pulmonary disease",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2016 %>% filter(Disease=="Lower respiratory infections") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Lower respiratory infections",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2016 %>% filter(Disease=="Stroke") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Stroke",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2016 %>% filter(Disease=="Tracheal, bronchus, and lung cancer") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Tracheal, bronchus, and lung cancer",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2016 %>% filter(Disease=="Stomach cancer") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Stomach cancer",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2016 %>% filter(Disease=="Pancreatic cancer") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Pancreatic cancer",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")



hist_2016 %>% filter(Disease=="Cervical Cancer") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Cervical cancer",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")



#------------------------------------------------------------------------------------------------

#Distribution of atr cost by age-sex-disease group - 2017
#------------------------------------------------------------------------------------------------
hist_2017<-atr_cost_2017 %>% pivot_longer(cols = -c("Disease","Age_group","Sex"))

hist_2017 %>% filter(Disease=="Ischemic heart disease") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Ischemic heart disease",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2017 %>% filter(Disease=="Chronic obstructive pulmonary disease") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Chronic obstructive pulmonary disease",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2017 %>% filter(Disease=="Lower respiratory infections") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Lower respiratory infections",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2017 %>% filter(Disease=="Stroke") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Stroke",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2017 %>% filter(Disease=="Tracheal, bronchus, and lung cancer") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Tracheal, bronchus, and lung cancer",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2017 %>% filter(Disease=="Stomach cancer") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Stomach cancer",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2017 %>% filter(Disease=="Pancreatic cancer") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Pancreatic cancer",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")



hist_2017 %>% filter(Disease=="Cervical Cancer") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Cervical cancer",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")



#------------------------------------------------------------------------------------------------

#Distribution of atr cost by age-sex-disease group - 2018
#------------------------------------------------------------------------------------------------
hist_2018<-atr_cost_2018 %>% pivot_longer(cols = -c("Disease","Age_group","Sex"))

hist_2018 %>% filter(Disease=="Ischemic heart disease") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Ischemic heart disease",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2018 %>% filter(Disease=="Chronic obstructive pulmonary disease") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Chronic obstructive pulmonary disease",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2018 %>% filter(Disease=="Lower respiratory infections") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Lower respiratory infections",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2018 %>% filter(Disease=="Stroke") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Stroke",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2018 %>% filter(Disease=="Tracheal, bronchus, and lung cancer") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Tracheal, bronchus, and lung cancer",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2018 %>% filter(Disease=="Stomach cancer") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Stomach cancer",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")

hist_2018 %>% filter(Disease=="Pancreatic cancer") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group+Sex, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Pancreatic cancer",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")



hist_2018 %>% filter(Disease=="Cervical Cancer") %>% ggplot(aes(value)) +
  facet_wrap(~Age_group, scales = "free") +
  geom_histogram()+
  labs(title = "Smoking-attributable costs: Cervical cancer",
       y = "Count", x = "Smoking-attributable costs (2020 Billion COP)")



#------------------------------------------------------------------------------------------------


#Media, Mediana y p2.5 y p97.5 2016
#-------------------------------------------------------------------------------------------------

sum_atr_cost_2016 <- atr_cost_2016
sum_atr_cost_2016$p025 <- 
  apply(sum_atr_cost_2016[ ,grep("V", names(sum_atr_cost_2016))], #select just `VNNN` columns
        1, # row-wise calcs
        quantile, probs=0.025)
sum_atr_cost_2016$p50 <- 
  apply(sum_atr_cost_2016[ ,grep("V", names(sum_atr_cost_2016))], #select just `VNNN` columns
        1, # row-wise calcs
        quantile, probs=0.50)
sum_atr_cost_2016$p975 <- 
  apply(sum_atr_cost_2016[ ,grep("V", names(sum_atr_cost_2016))], #select just `VNNN` columns
        1, # row-wise calcs
        quantile, probs=0.975)
sum_atr_cost_2016$mean <- rowMeans(sum_atr_cost_2016[,4:1003])
sum_atr_cost_2016 %<>% select("Disease","Age_group","Sex","p025","p50","mean","p975")

#sum(sum_atr_cost_2016$mean)
#5.707.563.039
#-------------------------------------------------------------------------------------------------


#Media, Mediana y p2.5 y p97.5 2017
#-------------------------------------------------------------------------------------------------
sum_atr_cost_2017 <- atr_cost_2017
sum_atr_cost_2017$p025 <- 
  apply(sum_atr_cost_2017[ ,grep("V", names(sum_atr_cost_2017))], #select just `VNNN` columns
        1, # row-wise calcs
        quantile, probs=0.025)
sum_atr_cost_2017$p50 <- 
  apply(sum_atr_cost_2017[ ,grep("V", names(sum_atr_cost_2017))], #select just `VNNN` columns
        1, # row-wise calcs
        quantile, probs=0.50)
sum_atr_cost_2017$p975 <- 
  apply(sum_atr_cost_2017[ ,grep("V", names(sum_atr_cost_2017))], #select just `VNNN` columns
        1, # row-wise calcs
        quantile, probs=0.975)
sum_atr_cost_2017$mean <- rowMeans(sum_atr_cost_2017[,4:1003])
sum_atr_cost_2017 %<>% select("Disease","Age_group","Sex","p025","p50","mean","p975")

#sum(sum_atr_cost_2017$mean)
#4.916.762.017
#-------------------------------------------------------------------------------------------------

#Media, Mediana y p2.5 y p97.5 2017
#-------------------------------------------------------------------------------------------------
sum_atr_cost_2018 <- atr_cost_2018
sum_atr_cost_2018$p025 <- 
  apply(sum_atr_cost_2018[ ,grep("V", names(sum_atr_cost_2018))], #select just `VNNN` columns
        1, # row-wise calcs
        quantile, probs=0.025)
sum_atr_cost_2018$p50 <- 
  apply(sum_atr_cost_2018[ ,grep("V", names(sum_atr_cost_2018))], #select just `VNNN` columns
        1, # row-wise calcs
        quantile, probs=0.50)
sum_atr_cost_2018$p975 <- 
  apply(sum_atr_cost_2018[ ,grep("V", names(sum_atr_cost_2018))], #select just `VNNN` columns
        1, # row-wise calcs
        quantile, probs=0.975)
sum_atr_cost_2018$mean <- rowMeans(sum_atr_cost_2018[,4:1003])
sum_atr_cost_2018 %<>% select("Disease","Age_group","Sex","p025","p50","mean","p975")

#sum(sum_atr_cost_2018$mean)
#6.722.827.139
#-------------------------------------------------------------------------------------------------

#Save data
#-------------------------------------------------------------------------------------------------

write.csv(sum_atr_cost_2016, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/sum_atr_cost_2016.csv')
write.csv(sum_atr_cost_2017, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/sum_atr_cost_2017.csv')
write.csv(sum_atr_cost_2018, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/sum_atr_cost_2018.csv')

#-------------------------------------------------------------------------------------------------

#Mean costs, denominators and weights by year: 
#------------------------------------------------------------------------------------------------
data_select_age <- read_csv("~/MEPI/Semestre 4/PG2/Datos/data_select_age.csv")
cost_denominador <- data_select_age %>% group_by(anio_vigencia_pago,sexo, grupo_edad, enfermedad) %>%
  summarise(costo_denominador = sum(costo_real))
cost_denominador$enfermedad[cost_denominador$enfermedad=="Cervical cancer"] <- "Cervical Cancer"


cost_denominador_2016 <- filter(cost_denominador,anio_vigencia_pago=="2016")
cost_denominador_2017 <- filter(cost_denominador,anio_vigencia_pago=="2017")
cost_denominador_2018 <- filter(cost_denominador,anio_vigencia_pago=="2018")


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

#----------------------------------------------------------------------------------------------

#Bind weights and denominator to attr cost data
#------------------------------------------------------------------------------------------------

N_paf_meanCost_IC_2016 <- merge(sum_atr_cost_2016, cost_denominador_2016, by.x=c("Sex","Age_group","Disease"), by.y=c("sexo", "grupo_edad", "enfermedad"))
N_paf_meanCost_IC_2017 <- merge(sum_atr_cost_2017, cost_denominador_2017, by.x=c("Sex","Age_group","Disease"), by=c("sexo", "grupo_edad", "enfermedad"))
N_paf_meanCost_IC_2018 <- merge(sum_atr_cost_2018, cost_denominador_2018, by.x=c("Sex","Age_group","Disease"), by=c("sexo", "grupo_edad", "enfermedad"))

N_paf_meanCost_IC_2016 %<>%
  mutate(Mean_Atr_Cost_2016_weighted = mean*weight_2016) %>%
  mutate(P50_Atr_Cost_2016_weighted = p50*weight_2016) %>%
  mutate(Lwr_Atr_Cost_2016_weighted = p025*weight_2016) %>%
  mutate(Upr_Atr_Cost_2016_weighted = p975*weight_2016)

N_paf_meanCost_IC_2016 %<>% mutate(percentage = Mean_Atr_Cost_2016_weighted/cost_denominador_weighted)

N_paf_meanCost_IC_2017 %<>%
  mutate(Mean_Atr_Cost_2017_weighted = mean*weight_2017) %>%
  mutate(P50_Atr_Cost_2017_weighted = p50*weight_2017) %>%
  mutate(Lwr_Atr_Cost_2017_weighted = p025*weight_2017) %>%
  mutate(Upr_Atr_Cost_2017_weighted = p975*weight_2017)

N_paf_meanCost_IC_2017 %<>% mutate(percentage = Mean_Atr_Cost_2017_weighted/cost_denominador_weighted)


N_paf_meanCost_IC_2018 %<>%
  mutate(Mean_Atr_Cost_2018_weighted = mean*weight_2018) %>%
  mutate(P50_Atr_Cost_2018_weighted = p50*weight_2018) %>%
  mutate(Lwr_Atr_Cost_2018_weighted = p025*weight_2018) %>%
  mutate(Upr_Atr_Cost_2018_weighted = p975*weight_2018)

N_paf_meanCost_IC_2018 %<>% mutate(percentage = Mean_Atr_Cost_2018_weighted/cost_denominador_weighted)

#------------------------------------------------------------------------------------------------

#Save data
#-------------------------------------------------------------------------------------------------

write.csv(N_paf_meanCost_IC_2016, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/N_paf_meanCost_IC_2016.csv')
write.csv(N_paf_meanCost_IC_2017, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/N_paf_meanCost_IC_2017.csv')
write.csv(N_paf_meanCost_IC_2018, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/N_paf_meanCost_IC_2018.csv')

#-------------------------------------------------------------------------------------------------

#Unir
#------------------------------------------------------------------------------------------------
All_data <- merge(N_paf_meanCost_IC_2016, N_paf_meanCost_IC_2017, by=c("Sex", "Age_group", "Disease"))
All_data <- merge(All_data, N_paf_meanCost_IC_2018, by=c("Sex", "Age_group", "Disease"))

All_data %<>% mutate(Mean_Atr_Cost_weighted = Mean_Atr_Cost_2018_weighted+Mean_Atr_Cost_2017_weighted+Mean_Atr_Cost_2016_weighted)
All_data %<>% mutate(Lwr_Atr_Cost_weighted = Lwr_Atr_Cost_2018_weighted+Lwr_Atr_Cost_2017_weighted+Lwr_Atr_Cost_2016_weighted)
All_data %<>% mutate(Upr_Atr_Cost_weighted = Upr_Atr_Cost_2018_weighted+Upr_Atr_Cost_2017_weighted+Upr_Atr_Cost_2016_weighted)
All_data %<>% mutate(Denom_Cost_weighted = cost_denominador_weighted+cost_denominador_weighted.x+cost_denominador_weighted.y)

#Check:
sum(All_data$Denom_Cost_weighted)
sum(All_data$Mean_Atr_Cost_weighted)
sum(All_data$Lwr_Atr_Cost_weighted)
sum(All_data$Upr_Atr_Cost_weighted)
#------------------------------------------------------------------------------------------------

#Save data
#-------------------------------------------------------------------------------------------------
write.csv(All_data, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/All_data.csv')

#-------------------------------------------------------------------------------------------------
