#------------------------------------------------------------------------
#Database cleaning and formating variables
#Selection of the elegible population
#------------------------------------------------------------------------

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


#----------------


#Clean workspace
rm(list=ls())
#Load data
data<-read.csv('C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 3/Proyecto de grado I/Datos/Datos R/consolidado.csv')


#----------------

#Filter by ICD10 codes of interest
data$DIAG %<>% toupper() #Upper case format

data %<>% 
  mutate(vl_ch = grepl("[[:punct:]]", data$DIAG)) #Eliminate patterns

data %>% filter(vl_ch==TRUE) #Check

# if (data$vl_ch==T){
#   dat$DIAG %<>% str_remove_all(DIAG, "[[:punct:]]") #Limpiando código de DIAGnósticos si tienen algún pattern/puntuación rara
# }


cie_10 <- data %>% select(ANONIMO_IDE, DIAG)
cie_10_split <- colsplit(cie_10$DIAG, "(?<=\\p{L})(?=[\\d+$])", c("char", "digit"))


#Select ICD10 code letters: 
cie_10_split %<>% filter(char=="I" | char=="G" | char=="C" | char=="J" | char=="A" | char=="B" | char=="P" | char=="U")

#table(cie_10_split %>% filter(char =="U")) %>% kable() %>% kable_paper()
#I: 20,22,23,24,25,200:216,219:259, 60, 61, 62, 63, 64, 64X, 600:620, 629:640,641,65,66,67,68,69,650:699
#G: 45,45X,46,450:468
#C: 16, 160:169, 25, 250:259, 33X, 34, 340:349, 53, 530:539
#J: 41, 42X, 410:424, 43, 44, 430:449, 09X, 10,11,12,13,13X,14,14X,15,16,17,18,10:182, 188:189, 19, 20, 21, 22, 22X, 196:229, 851, 91, 91X, 
#A: 481, 70
#B: 960, 961, 972, 974:976
#P: 23, 230:239
#U: 04, 049

I_eic<-c("20",22,23,24,25,200:216,219:259)
I_stroke <-c(60,61,62,63,64,"64X",600:620,629:640,641,65,66,67,68,69,650:699)
G_stroke <- c(45,"45X",46,450:468)
C_pulmon <- c("33X",34,340:349)
C_estomago <- c("16",160:169)
C_pancreas <- c("25",250:259)
C_cervix <- c("53",530:539)
J_EPOC <- c(41, "42X", 410:424, 43, 44, 430:449)
J_irvb <- c("09X", 10,11,12,13,"13X",14,"14X",15,16,17,18,10:182, 188:189, 19, 20, 21, 22, "22X", 196:229, 851, 91, "91X", "91*")
A_irvb<-c("481", 70)
B_irvb <-c("960", 961, 972, 974:976)
P_irvb <-c("23", 230:239)
U_irvb <-c("04", 049)

I_eic<-gsub(" ","",paste("I",I_eic))
I_stroke <- gsub(" ","",paste("I",I_stroke))
G_stroke<-gsub(" ","",paste("G",G_stroke))
C_pulmon<-gsub(" ","",paste("C",C_pulmon))
C_estomago<-gsub(" ","",paste("C",C_estomago))
C_pancreas<-gsub(" ","",paste("C",C_pancreas))
C_cervix<-gsub(" ","",paste("C",C_cervix))
J_EPOC<-gsub(" ","",paste("J",J_EPOC))
J_irvb<-gsub(" ","",paste("J",J_irvb))
A_irvb<-gsub(" ","",paste("A",A_irvb))
B_irvb<-gsub(" ","",paste("B",B_irvb))
P_irvb<-gsub(" ","",paste("P",P_irvb))
U_irvb<-gsub(" ","",paste("U",U_irvb))


cie_10_interes <-c(I_eic,I_stroke,G_stroke,C_pulmon,C_estomago,C_pancreas,C_cervix,J_EPOC,J_irvb,A_irvb,B_irvb,P_irvb,U_irvb)

data_filter <- data[data$DIAG %in% cie_10_interes,]

data_filter %<>%
  mutate(Enfermedad = ifelse(DIAG %in% I_eic, "Enfermedad isquemica del corazon",
                             ifelse((DIAG %in% I_stroke) | (DIAG %in% G_stroke), "Accidente cerebrovascular", 
                                    ifelse(DIAG %in% C_pulmon, "Cancer de traquea, bronquios y pulmnón",
                                           ifelse(DIAG %in% C_estomago, "Cancer de estomago",
                                                  ifelse(DIAG %in% C_cervix, "Cancer de cervix",
                                                         ifelse(DIAG %in% C_pancreas, "Cancer de pancreas",
                                                                ifelse(DIAG %in% J_EPOC, "EPOC","Infeccion respiratoria de las vias bajas"))))))))




#-------------------------------------------



#Fix date format
#Function
multidate <- function(data, formats){
  a<-list()
  for(i in 1:length(formats)){
    a[[i]]<- as.Date(data,format=formats[i])
    a[[1]][!is.na(a[[i]])]<-a[[i]][!is.na(a[[i]])]
  }
  a[[1]]
}


#Date of birth
data_filter$fecha_nac_original <- data_filter$FECHA_NAC
data_filter$fecha_nacimiento <- data_filter$FECHA_NAC
data_filter$fecha_nacimiento2 <- data_filter$FECHA_NAC
data_filter$FECHA_NAC <- multidate(data_filter$fecha_nacimiento, c("%d-%m-%Y","%Y-%m-%d", "%m/%d/%Y"))
data_filter$fecha_nacimiento2 = as.Date(data_filter$fecha_nacimiento2, format ="%d/%m/%Y")
data_filter$FECHA_NAC[is.na(data_filter$FECHA_NAC)] <- data_filter$fecha_nacimiento2[is.na(data_filter$FECHA_NAC)]

#Check
data_filter %>% 
  filter(is.na(FECHA_NAC)==T) %>% 
  select(FECHA_NAC,fecha_nacimiento,fecha_nacimiento2)

#Find missing 
data_filter %>%
  filter(FECHA_NAC == "1900-01-01")

#Date: start sick leave
data_filter$FECHA_INI_RECT %<>% ymd()

#Date: end sick leave
data_filter$FECHA_FIN_RECT %<>% ymd()

#Date: payment sick leave
data_filter$FECHA_PAGO_RECT %<>% ymd()






#-------------------------------------




#Age: birth date - sick leave start date 

#Create variable age
data_filter %<>%
  mutate(Edad = year(FECHA_INI_RECT)-year(FECHA_NAC))

#fig <- plot_ly(x = ~data$Edad, type = "histogram", name="Edad")
#fig

summary(data_filter$Edad)

#Check
prueba_edad <- data_filter %>% select(Edad, FECHA_NAC, fecha_nac_original, FECHA_INI_RECT) %>% filter(Edad > 100)
pos <- which(data_filter$Edad > 100)
data_filter$FECHA_NAC[pos] = as.Date(data_filter$fecha_nac_original[pos], format ="%d-%m-%Y")

#Fix date
data_filter %<>%
  mutate(Edad = year(FECHA_INI_RECT)-year(FECHA_NAC))

#Check
prueba_edad <- data_filter %>% select(Edad, FECHA_NAC, fecha_nac_original, FECHA_INI_RECT) %>% filter(Edad > 100)


#Filter by age criteria
data_filter_edad <- data_filter %>% filter(Edad >= 30)


#Create age groups

data_clean <- data_filter_edad
data_clean %<>%
  mutate(Grupo_Edad = ifelse(Edad >= 30 & Edad < 35, "30 to 34", 
                             ifelse(Edad >= 35 & Edad < 40, "35 to 39",
                                    ifelse(Edad >=40 & Edad <45,"40 to 44",
                                           ifelse(Edad >=45 & Edad <50, "45 to 49",
                                                  ifelse(Edad >= 50 & Edad < 55, "50 to 54",
                                                         ifelse(Edad>=55 & Edad <60, "55 to 59",
                                                                ifelse(Edad >=60 & Edad <65, "60 to 64",
                                                                       ifelse(Edad >=65 & Edad <70, "65 to 69", "70+ years")))))))))


#Character to factor
data_clean$Grupo_Edad %<>% as.factor()




#--------------------------------------

#Recode, reformat variables

#Sex:
data_clean$SEXO %<>%as.factor()

#Tipo cotizante
data_clean$TIPO_COTIZANTE<-factor(data_clean$TIPO_COTIZANTE,levels=c("1","2","3","4","12","16","18","19","20","21","22","30","31","32","33","34","35","36","40","42","43","44","45","47","52","53","54","55","56", "57","58","59"))
levels(data_clean$TIPO_COTIZANTE) <- c("Dependiente","Servicio doméstico","Independiente","Madre sustituta","Aprendices en etapa electiva","Independiente agremiado o asociado","Funcionarios públicos sin tope máximo en el IBC","Aprendices etapa productiva","Estudiantes (Régimen especial - Ley 789/2002)","Estudiantes de postgrado en salud","Profesor de establecimiento particular","Dependiente entidades o universidades públicas de los regímenes especial y de excepción","Cooperados o precooperativas de trabajo asociado","Cotizante miembro de la carrera diplomática o consular de un país extranjero o funcionario de organismo multilateral, no sometido a la legislación colombiana","Beneficiario del Fondo de Solidaridad Pensional","Concejal o edil de Junta Administradora Local del Distrito Capital de Bogotá amparado por póliza de salud", "Concejal o edil de Junta Administradora Local del Distrito Capital de Bogotá amparado por póliza de salud","Concejal municipal o distrital no amparado con póliza de salud","Concejal municipal o distrital no amparado con póliza de salud beneficiario del Fondo de Solidaridad Pensional","Beneficiario UPC adicional","Cotizante independiente pago solo salud", "Cotizante a pensiones con pago a tercero","Cotizante dependiente de empleo de emergencia con duración mayor o igual a un mes","Cotizante dependiente de empleo de emergencia con duración menor a un mes","Trabajador dependiente de entidad beneficiaria del Sistema General de Participaciones- Aportes Patronales", "Beneficiario del mecanismo de protección al cesante","Afiliado participe","Prepensionado de entidad en liquidación","Afiliado participe dependiente","Prepensionado con aporte voluntario a salud","Independiente voluntario al Sistema de Riesgos laborales","Estudiantes de prácticas laborales en el sector público","Independientes con contrato de prestación de servicios superior a un mes")

#RANGO_SMLV
data_clean$RANGO_SMLV %<>%as.factor()  

#CONTINUIDAD
data_clean$CONTINUIDAD <- factor(data_clean$CONTINUIDAD, levels=c("1","2","3","4"))
levels(data_clean$CONTINUIDAD) <-c("Inicial","Continuidad","Mayores a 180 días hasta 540 días","Mayores a 540 días")

#Estado incapacidad
data_clean$ESTADO_INCAPACIDAD <- factor(data_clean$ESTADO_INCAPACIDAD, levels=c("1","2","3"))
levels(data_clean$ESTADO_INCAPACIDAD) <-c("Pagada","Glosada","En trámite")


#Codigo municipio
data_clean$COD_MUNI<-stringr::str_pad(data_clean$COD_MUNI, 5, side="left", pad="0") #Agregar 0 si el código de municipio solo tiene 4 dígitos

#DEPARTAMENTO
data_clean$DEPARTAMENTO <- gsub("Ã©", "é", data_clean$DEPARTAMENTO)
data_clean$DEPARTAMENTO <- gsub("Ã³", "ó", data_clean$DEPARTAMENTO)
data_clean$DEPARTAMENTO <- gsub("Ã¡", "á", data_clean$DEPARTAMENTO) 
data_clean$DEPARTAMENTO<- gsub("Ã±", "ñ", data_clean$DEPARTAMENTO)
data_clean$DEPARTAMENTO <- gsub("Ã", "í", data_clean$DEPARTAMENTO)

#Factor
$DEPARTAMENTO %<>% as.factor()

#Department code
data_clean %<>%
  mutate(cod_departamento = substr(COD_MUNI, start = 1, stop = 2))

#Factor
data_clean$COD_MUNI %<>%as.factor()
data_clean$cod_departamento %<>% as.factor()


#--------------------------------------

#Guardar base de datos final 
#write.csv(data_clean, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 3/Proyecto de grado I/Datos filtrados/base_filtrada.csv')
#analyze with stata
#--------------------------------------




#--------------------------------------

#Database stata with names assigned to ICD10 codes
data_stata<-read.csv('C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 3/Proyecto de grado I/Datos filtrados/base_filtrada_stata.csv')


#DEPARTAMENTO
data_stata$departamento <- gsub("Ã©", "é", data_stata$departamento)
data_stata$departamento <- gsub("Ã³", "ó", data_stata$departamento)
data_stata$departamento <- gsub("Ã¡", "á", data_stata$departamento) 
data_stata$departamento<- gsub("Ã±", "ñ", data_stata$departamento)
data_stata$departamento <- gsub("Ã­Â", "í", data_stata$departamento)

freq(data_stata$departamento)

#Regions: 

data_stata %<>%
  mutate(Region = ifelse(departamento == "La Guajira" | departamento == "Cesar" | departamento == "Magdalena" | departamento =="Atlántico" | departamento =="Bolí­var" | departamento =="Sucre" |  departamento =="Córdoba", "Atlántica", 
                         ifelse(departamento == "Caldas" | departamento =="Quindí­o" | departamento == "Risaralda" | departamento =="Tolima" | departamento =="Huila" | departamento == "Caquetá", "Central",
                                ifelse(departamento == "Bogotá D.C.", "Bogotá",
                                       ifelse(departamento == "Antioquia", "Antioquia",
                                              ifelse(departamento =="Archipiélago de San Andrés, Providencia y Santa Catalina", "San Andrés",
                                                     ifelse(departamento == "Cauca" | departamento == "Chocó" | departamento == "Nariño", "Pacífico",
                                                            ifelse(departamento == "Valle del Cauca", "Valle del Cauca",
                                                                   ifelse(departamento == "Boyacá" | departamento == "Cundinamarca" | departamento == "Meta" | departamento == "Norte de Santander" | departamento == "Santander","Oriental","Orinoquía - Amazonía")))))))))
freq(data_stata$Region)


#Llenar descripción de códigos CIE-10 incompleta fuente: http://idsn.gov.co/site/web2/images/documentos/RIPS/CIE-10.pdf & https://www.imss.gob.mx/sites/all/statics/profesionalesSalud/economia/Libro-GRD2017.pdf


#C33X Tumor maligno de la tráquea
#G45X Ataques de isquemia cerebral transitoria y síndromes afines
#I64X Accidente vascular encefálico agudo, no especificado como hemorrágico o isquémico
#J09X Influenza debida a virus de la influenza aviar identificado
#J13X Neumonía debida a Streptococcus pneumoniae
#J14X Neumonía debida a Haemophilus influenzae
#J22X Infección aguda no especificada de las vías respiratorias inferiores
#J42X Bronquitis crónica no especificada
#J91* Derrame pleural en afecciones clasificadas en otra parte
#J91X Derrame pleural en afecciones clasificadas en otra parte


data_stata %<>%
  mutate(descr_cie10 = ifelse(diag == "C33X", "C33X Tumor maligno de la tráquea",
                              ifelse(diag=="G45X","G45X Ataques de isquemia cerebral transitoria y síndromes afines",
                                     ifelse(diag=="I64X","I64X Accidente vascular encefálico agudo, no especificado como hemorrágico o isquémico",
                                            ifelse(diag=="J09X","J09X Influenza debida a virus de la influenza aviar identificado",
                                                   ifelse(diag=="J13X","J13X Neumonía debida a Streptococcus pneumoniae",
                                                          ifelse(diag=="J14X","J14X Neumonía debida a Haemophilus influenzae",
                                                                 ifelse(diag=="J22X","J22X Infección aguda no especificada de las vías respiratorias inferiores",
                                                                        ifelse(diag=="J42X", "J42X Bronquitis crónica no especificada",
                                                                               ifelse(diag=="J91*" | diag == "J91X", "J91X Derrame pleural en afecciones clasificadas en otra parte", descr))))))))))


#Agrupar tipo cotizante
data_stata %<>% 
  mutate(cat_tipo_cotizante = ifelse(tipo_cotizante=="Dependiente", "Dependiente", 
                                     ifelse(tipo_cotizante=="Independiente", "Independiente", "Otro")))
data_stata$cat_tipo_cotizante %<>% as.factor()
#--------------------------------------


#--------------------------------------

#Guardar base de datos final 
write.csv(data_stata, 'C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 3/Proyecto de grado I/Datos filtrados/base_filtrada_clean.csv')
#--------------------------------------------------------------------------------------------------------------------





#--------------------------------------

#Cargar base de datos final 
data<-read.csv('C:/Users/gaga1/OneDrive/Documents/MEPI/Semestre 4/PG2/Datos/base_filtrada_clean.csv')

#Arreglar nombre de columnas
nombres <- colnames(data)
nombres[nombres=="aÃ.o_vigencia_pago"] <- "anio_vigencia_pago"
colnames(data) <- nombres

#Drop individuals with inplausible data
data<-data[!data$anonimo_ide %in% c("0B6FE854-56D2-4544-875F-949BC0868605","195E6681-7F67-41DC-9DA2-8B321A191D97","3716D1DB-AA45-4553-B459-B64E78FB22A0","46929439-1939-47BA-96A1-B299E9BE4A09","4E7BF391-65DF-43BA-B83D-B094DAAF53C6","78489344-6F44-4FFF-97E3-A8441F08B735","7F7978DC-F687-413F-A2E6-D85EBC9C49E8","AE162BAD-48DF-49ED-918A-156E54E68C33","C361B568-E8D0-455A-AEED-AFDCDC3149E6","D8C7315F-6E7F-4B89-B6F8-E88A91CB4535"),]


#Change cause name to english
data$enfermedad[data$enfermedad=="Enfermedad isquemica del corazon"]<-"Ischemic heart disease" 
data$enfermedad[data$enfermedad=="Accidente cerebrovascular"]<-"Stroke" 
data$enfermedad[data$enfermedad=="Cancer de traquea, bronquios y pulmnÃ³n"]<- "Tracheal, bronchus, and lung cancer" 
data$enfermedad[data$enfermedad=="Cancer de estomago"]<- "Stomach cancer" 
data$enfermedad[data$enfermedad=="Cancer de cervix"]<- "Cervical cancer" 
data$enfermedad[data$enfermedad=="Cancer de pancreas"]<- "Pancreatic cancer" 
data$enfermedad[data$enfermedad=="EPOC"]<- "Chronic obstructive pulmonary disease" 
data$enfermedad[data$enfermedad=="Infeccion respiratoria de las vias bajas"]<- "Lower respiratory infections" 

data$sexo[data$sexo=="M"]<-"Male"
data$sexo[data$sexo=="F"]<-"Female"

data$rango_smlv %<>% as.factor()

data_select <- data %>% select(anonimo_ide, anio_vigencia_pago, fecha_ini_rect, fecha_fin_rect, enfermedad, grupo_edad, sexo, valor_incapacidad, dias_totales,rango_smlv, ibc)

#--------------------------------------



#--------------------------------------

#Adjust by IPC
#Variable valor incapacidad a costo real (2020 COP)
#Crear columna costos modificados por IPC
data_select %<>%
  mutate(costo_real = ifelse(anio_vigencia_pago==2016, valor_incapacidad/0.88273, ifelse(anio_vigencia_pago==2017, valor_incapacidad/0.94805, valor_incapacidad/0.94805)))


#--------------------------------------



#Agrupar base de datos por ID
#--------------------------------------

#Dejar edad inicial para pacientes que cambian de grupo de edad en las recurrencias de una misma enfermedad
data_recurrence <- data_select %>% group_by(anonimo_ide,enfermedad) %>%
  summarise(n = n())
data_recurrence_disease <- data_recurrence %>% filter(n>1)

data_select %<>%
  mutate(Age_order = ifelse(grupo_edad=="30 to 34",1,
                            ifelse(grupo_edad =="35 to 39",2,
                                   ifelse(grupo_edad=="40 to 44",3,
                                          ifelse(grupo_edad=="45 to 49",4,
                                                 ifelse(grupo_edad=="50 to 54",5,
                                                        ifelse(grupo_edad=="55 to 59",6,
                                                               ifelse(grupo_edad=="60 to 64",7,
                                                                      ifelse(grupo_edad=="65 to 69",8,9)))))))))


pos <- 1:nrow(data_recurrence_disease)
for(i in pos){
  p<-data_select %>% subset(anonimo_ide==data_recurrence_disease$anonimo_ide[i] & enfermedad==data_recurrence_disease$enfermedad[i], select=Age_order)
  data_recurrence_disease$age_min[i] <- min(p)
  print(min(p))
}

#Replace age_min with real category
data_recurrence_disease %<>%
  mutate(min_age_group = ifelse(age_min==1,"30 to 34",
                                ifelse(age_min ==2,"35 to 39",
                                       ifelse(age_min==3,"40 to 44",
                                              ifelse(age_min==4,"45 to 49",
                                                     ifelse(age_min==5,"50 to 54",
                                                            ifelse(age_min==6,"55 to 59",
                                                                   ifelse(age_min==7,"60 to 64",
                                                                          ifelse(age_min==8,"65 to 69","70+ years")))))))))


#Replace age in dataset:
data_select_age <- data_select
#find positions and change
for(i in pos){
  id <- data_recurrence_disease$anonimo_ide[i]
  disease<-data_recurrence_disease$enfermedad[i]
  data_select_age$grupo_edad[data_select_age$anonimo_ide == id & data_select_age$enfermedad==disease] <- data_recurrence_disease$min_age_group[data_recurrence_disease$anonimo_ide == id & data_recurrence_disease$enfermedad==disease]
}

#Observación 73 control
#data_select$grupo_edad[data_select$anonimo_ide == "005CDC34-C03F-4C4B-A455-ADD822E0CEAB" & data_select$enfermedad=="Stroke"]
#data_select_age$grupo_edad[data_select_age$anonimo_ide == "005CDC34-C03F-4C4B-A455-ADD822E0CEAB" & data_select_age$enfermedad=="Stroke"]

model_data_2016 <- data_select_age %>% filter(anio_vigencia_pago=="2016") %>% group_by(anonimo_ide, sexo, grupo_edad, enfermedad) %>% 
summarise(valor_incapacidad_sum = sum(costo_real), rango_smlv=calculate_mode(rango_smlv), diastotales = sum(dias_totales), tipo_cotizante=calculate_mode(tipo_cotizante), region=calculate_mode(Region), ibc = median(ibc), recurrencia=n())

model_data_2017 <- data_select_age %>% filter(anio_vigencia_pago=="2017") %>% group_by(anonimo_ide, sexo, grupo_edad, enfermedad) %>% 
summarise(valor_incapacidad_sum = sum(costo_real), rango_smlv=calculate_mode(rango_smlv), diastotales = sum(dias_totales), tipo_cotizante=calculate_mode(tipo_cotizante), region=calculate_mode(Region), ibc = median(ibc), recurrencia=n())

model_data_2018 <- data_select_age %>% filter(anio_vigencia_pago=="2018") %>% group_by(anonimo_ide, sexo, grupo_edad, enfermedad) %>% 
summarise(valor_incapacidad_sum = sum(costo_real), rango_smlv=calculate_mode(rango_smlv), diastotales = sum(dias_totales), tipo_cotizante=calculate_mode(tipo_cotizante), region=calculate_mode(Region), ibc = median(ibc), recurrencia=n())

#--------------------------------------