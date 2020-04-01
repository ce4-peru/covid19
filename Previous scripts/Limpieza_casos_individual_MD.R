### LIMPIEZA DE BASE DE DATOS DE CASOS POR INDIVIDUOS ###

setwd("~/covid19PE-LIEZ/")

## Call data
covid_ind <- read.csv("~/covid19PE-LIEZ/data/crudas/covidPE_IND_20200326_MD.csv")

## Check data
names(covid_ind)
# [1] "Caso_ID"            "Fecha"              "RegiÃ³n"           
# [4] "Provincia"          "Distrito"           "Direccion"         
# [7] "Edad_anhos"         "SexoM1H0"           "HospitalizadoS1N0" 
# [10] "AisladoDomicilS1N0" "Contacto"           "Enlaces"           
# [13] "X"                  "X.1"  
# Sacar los "S1N0 del nombre de la variable
# Averiguar que son X y X.1
# Cambiar "Edad_anhos" por "Edad_A"
# Todas las variables con mayúscula
# Todas las variables sin tilde
# Dividir la variable contacto como se indicó en googlesheet.
# Enlaces al menos para el análisis no nos sirve y quita espacio, 
# pero que se mantenga a menos que se indique lo contrario.

str(covid_ind) # 395 obs. of  14 variables
head(covid_ind) # ok


## Check variables

# ID del caso
str(covid_ind$Caso_ID) # int
unique(covid_ind$Caso_ID)
# En el google spreadsheet checar que los números de los casos 
# corresponden a los números de las observaciones.
# Ej. caso nro 44 figura en la observación nro 50.
# Completar n[umero de casos en google spreadsheet.

## Fecha 
str(covid_ind$Fecha) # Factor w/ 18 levels
unique(covid_ind$Fecha)
# 17/3/2020 estan en formato incorrecto. 
# no lo encuentro en el google sheet ni en el csv. Obs 106.
# Transformar fecha en R.
# [1] 06/03/2020 07/03/2020 09/03/2020 10/03/2020 11/03/2020 12/03/2020
# [7] 13/03/2020 14/03/2020 15/03/2020 16/03/2020 17/03/2020 17/3/2020 
# [13] 18/03/2020 19/03/2020 20/03/2020 21/03/2020 22/03/2020 23/03/2020

## Region
str(covid_ind$RegiÃ³n) # factor
# Cambiar nombre de variable en googlesheet
levels(covid_ind$RegiÃ³n)
# [1] "Ancash"        "Arequipa"      "Callao"        "Cusco"        
# [5] "HuÃ¡nuco"      "Ica"           "Junin"         "La libertad"  
# [9] "Lambayeque"    "Lima"          "LIma"          "Lima "        
# [13] "Loreto"        "Madre de Dios" "Piura"         "San Martin"  
# Pareciera que no lee pero si lee.
# "HuÃ¡nuco" con tilde
# "Lima", "LIma" and "Lima ". Convertir todo en mayúscula.

## Provincia
str(covid_ind$Provincia) # Factor w/ 20 levels
levels(covid_ind$Provincia)
# [1] ""           "Arequipa"   "Callao"     "Callao "    "Chiclayo"  
# [6] "Chincha"    "Cusco"      "FerreÃ±afe" "Huancayo"   "Huanuco"   
# [11] "Ica"        "Lima"       "LIma"       "Maynas"     "Piura"     
# [16] "San Martin" "Santa"      "Sullana"    "Tambopata"  "Trujillo"
# "FerreÃ±afe" y "". Quitar la ñ y la otra marcar como NA.

## Region
str(covid_ind$Distrito) # Factor w/ 31 levels
levels(covid_ind$Distrito)
# [1] ""                              "Amarilis"                     
# [3] "Carmen de la Legua"            "Castilla"                     
# [5] "Cerro Colorado"                "Chilca"                       
# [7] "Chincha baja"                  "Cusco"                        
# [9] "Huancayo"                      "Huanchaco"                    
# [11] "Ica"                           "Indiana"                      
# [13] "Iquitos"                       "Jose Luis Bustamante y Ribero"
# [15] "La Banda de Shilcayo"          "La Punta"                     
# [17] "La Victoria"                   "Las lomas"                    
# [19] "Lince"                         "Marcavelica"                  
# [21] "Mariano Melgar"                "Nuevo Chimbote"               
# [23] "Piura"                         "Santa"                        
# [25] "Surco"                         "Tambo"                        
# [27] "Tambopata"                     "Trujillo"                     
# [29] "Veintiseis de Octubre"         "Villa Maria del Triunfo"      
# [31] "Yanahuara"  
# "" convertir a NA

## Direccion
str(covid_ind$Direccion) # logi [1:395]
unique(covid_ind$Direccion) # NA

## Edad
str(covid_ind$Edad_anhos) # Factor w/ 42 levels
covid_ind$Edad_A_n <- as.numeric(as.character(covid_ind$Edad_anhos)) # NA
str(covid_ind$Edad_A_n)# ok
table(covid_ind$Edad_A_n) # ok, from 7 to 96.

## Sexo
str(covid_ind$SexoM1H0) # int
table(covid_ind$SexoM1H0) # 0 and 1
# 0  1 
# 44 41

## Hospitalizado
str(covid_ind$HospitalizadoS1N0) # int
table(covid_ind$HospitalizadoS1N0) # 0 and 1
# 0  1 
# 61 17 

## AisladoDomicil
str(covid_ind$AisladoDomicilS1N0) # int
table(covid_ind$AisladoDomicilS1N0) # 0 and 1
# 0  1 
# 15 61  

## AisladoDomicil
str(covid_ind$Contacto) # Factor w/ 37 levels
unique(covid_ind$Contacto) 
# [1] Europa (Recorrio Francia, EspaÃ±a y Republica Checa)
# [2] Familiar del caso 1                                 
# [3] Amigo del caso 1                                    
# [4] Reino Unido (Londres, Inglaterra)                   
# [5] Entorno del caso 1                                  
# [6] Espana                                              
# [7] Espana e Italia                                     
# [8] Familiar del caso 6                                 
# [9]                                                     
# [10] Italia                                              
# [11] Alemania y Espana                                   
# [12] Espana y Londres                                    
# [13] EEUU (Washington DC)                                
# [14] Espana (Madrid)                                     
# [15] EspaÃ±a e Italia                                    
# [16] EspaÃ±a                                             
# [17] Estados Unidos                                      
# [18] Contacto con extranjeros                            
# [19] Contacto con turistas                               
# [20] Relacionado al primer caso de Acash                 
# [21] Hermana del caso del 17/03/2020                     
# [22] Familiar del caso del 17/03/2020                    
# [23] Lima                                                
# [24] Contacto con turistas holandeses                    
# [25] Turista aleman                                      
# [26] Turista                                             
# [27] Contacto con caso del 17/03/2020                    
# [28] Relacionado al caso 3 de Ancash                     
# [29] EspaÃ±a y Francia                                   
# [30] Ecuador                                             
# [31] En investigaciÃ³n                                   
# [32] Familiar del caso 1 de las lomas                    
# [33] Contacto con familiar que vino del extrangero       
# [34] Contacto con el caso 1 de las Lomas                 
# [35] Madrid y Barcelona                                  
# [36] Francia (Paris)                                     
# [37] Contacto con turistas europeo 

# Dividir en 3 o 4 columnas esta inforamci[on como se indic[o en el googlesheet.
