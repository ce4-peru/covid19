library(data.table)
library(dplyr)
library(tidyr)
# library(tidyverse)
library(lubridate)
library(ggplot2)
library(directlabels)
library(reshape)
library(colorspace)
library(plotly)
#### si no tienes el paquete bbplot 
library(devtools)
library(rlang)
library(digest)
library(glue)

# install devtools
library(devtools)

#devtools::install_github('bbc/bbplot')
#devtools::install_github('JLutils')# Esto no va a funcionar

#install.packages('JLutils', repos = 'https://cloud.r-project.org')

library(bbplot)
#library(bbc)# Me parece que esta libreria esta por las puras
#library(JLutils)

library(scales)
library("gridExtra")
library(cowplot)
######### curvas nacionales de positivos y pruebas totales ##################
### leer esta data de los puntos de corte para ambos dibujos ####
d=data.frame(Fecha=as.Date(c("2020-03-06", "2020-03-11", "2020-03-12", "2020-03-15","2020-03-16", "2020-03-26","2020-03-31","2020-04-02","2020-04-07")), event=c(" Primer Caso", " Cierre colegios", "Cierre universidades", " Inicio de cuarentena","Cierre de aeropuertos", " Extensión cuarentena", " Restricción por sexo","Toque de queda 18pm-5am","Extensión cuarentena"))


# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200406_MD_clean.csv") 
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200407_CA_clean.csv.csv") 
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200410_MD_clean.csv") 
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200411_CA_clean.csv") 
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200412_MD_clean.csv") 
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200413_CA_clean.csv") 
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200414_CA_clean.csv") 
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200415_GF_clean.csv") 
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200416_CA_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200418_MD_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200419_GF_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200420_MD_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200421_CA_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200422_GF_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200423_CA_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200424_AL_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200426_GF_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200427_MD_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200428_AL_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200429_GF_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200430_AL_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200501_AL_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200502_MD_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200504_MD_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200505_CA_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200506_GF_clean.csv")
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200507_CA_clean.csv")
acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200509_MD_clean.csv")

setnames(acumulado,"TOTAL_POSITIVOS","Casos positivos")
setnames(acumulado,"TOTAL_PRUEBAS","Pruebas realizadas")

acumulado1<-melt(acumulado,id="FECHA")
acumulado1$value<-as.numeric(acumulado1$value)
target <- c("Casos positivos", "Pruebas realizadas")
acumulado1<-filter(acumulado1, variable %in% target)  
acumulado1<-acumulado1 %>%
  mutate(Fecha = as.Date(FECHA))

setwd("~/covid19/outputs_covid19/")
# 
#Cambiar el nombre
png(filename="20200509_pruebas_vs_positivos_COVID19.png", width=1100, height=600)
ggplot() + 
  geom_line(data = acumulado1, aes(Fecha, value, colour=variable,group=variable),alpha =1.6) +
  scale_colour_manual(values=c("red", "blue"))+
  scale_x_date(labels = date_format("%d-%b"),
               breaks = as.Date(c("2020-03-06", "2020-03-11", "2020-03-12", 
                                  "2020-03-15","2020-03-16" ,"2020-03-26","2020-03-31",
                                  "2020-04-02","2020-04-07")),
               minor_breaks = NULL)+
  # scale_x_date() +
  geom_vline(data=d, mapping=aes(xintercept=Fecha), color="gray") +
  geom_text(data=d, mapping=aes(x=Fecha, y =0,label=event), size=4, angle=90, vjust=-0.4, hjust=0)+
  # theme_minimal()+
        theme(panel.background = element_rect(fill = "white"),
              legend.position = c(0.35,0.95),
              legend.justification = c("right","top"),
            axis.text.x = element_text(vjust =0.60,angle=60,face = "bold.italic", size = 10))+
  labs(title="SARS-CoV-2 en Per�", y="Pruebas positivas y pruebas realizadas", 
       x="Fecha", caption="Source :varios")+
  theme(plot.title = element_text(face = "bold.italic",size = 5) )+
  bbc_style()
  #print(p)
dev.off()





# # ######## dibujando graficos  casos por departamentos  #########
# # install.packages('devtools')
# 
# df_dep<-fread("~/covid19/data/modificadas/covidPE_DEP_20200416_CA_clean.csv") 
# df_dep<-df_dep %>%
#   mutate(Fecha = as.Date(Fecha1))
# 
# # 
# # palM <- c("maroon", "darkorange3", "goldenrod3", "darkgreen", "navy", "plum4", "black",
# #            "lightcoral", "peachpuff2", "khaki1", "olivedrab3", "steelblue1", "thistle", "snow2",
# #            "turquoise", "orange", "gold", "springgreen4", "dodgerblue3", "mediumpurple", "gray")
# #  
# palM1 <- c("red", "blue", "yellow", "purple", "green", "gray15", "gray34",
#           "blue3", "pink", "green3", "brown", "orange2", "skyblue2", "gray33",
#           "turquoise", "orange3", "gold", "springgreen", "dodgerblue", "purple", "gray32","black",
#           "blue2","red4","green3")
# 
#  # pal1 <- palM[1:length(unique(df_dep$REGION))]
#  pal2 <- palM1[1:length(unique(df_dep$REGION))]
# 
# ggplot()+
#   geom_line( data=df_dep,mapping = aes(x = Fecha,y = CASOS,colour=REGION,group=REGION)) +
#   scale_colour_manual(values=pal2)+ 
#   scale_x_date(labels = date_format("%d-%b"),breaks = as.Date(c("2020-03-06", "2020-03-11", "2020-03-12", "2020-03-15", "2020-03-26","2020-03-31","2020-04-02","2020-04-07")),
#                minor_breaks = NULL)+
#   geom_vline(data=d, mapping=aes(xintercept=Fecha), color="gray") +
#    geom_text(data=d, mapping=aes(x=Fecha, y =0,label=event), size=4, angle=85, vjust=-0.4, hjust=0)+
#    theme(panel.background = element_rect(fill = "white"),
#          legend.position = "right",
#          legend.justification = c("right","top"),
#   axis.text.x = element_text(vjust =0.60,angle=60,face = "bold.italic", size = 10))+
#   
#    # theme(axis.text.x = element_text(angle=90),
#    #       )+
#    labs(title="SARS-CoV-2 en Perú", y="Casos por región", 
#         x="Fecha", caption="Source :varios")+
#   bbc_style()
# #### haciendolo interactivo ########
# # ggplotly(p)
# 
# 
# ####### ploteando solo lima y solo provincias por separado ########
# 
# target <- c("LIM")
# df_otros<-filter(df_dep,! REGION %in% target) 
# df_lima<-filter(df_dep,REGION%in% target)
# 
# p1<-ggplot()+
#   geom_line( data=df_otros,mapping = aes(x = Fecha,y = CASOS,colour=REGION,group=REGION)) +
#    scale_colour_manual(values=pal2)+ 
#   scale_x_date(labels = date_format("%d-%b"),breaks = as.Date(c("2020-03-06", "2020-03-11", "2020-03-12", "2020-03-15", "2020-03-26","2020-03-31","2020-04-02","2020-04-07")),
#                minor_breaks = NULL)+
#   geom_vline(data=d, mapping=aes(xintercept=Fecha), color="gray") +
#   geom_text(data=d, mapping=aes(x=Fecha, y =0,label=event), size=4, angle=90, vjust=-0.4, hjust=0)+
#   theme(panel.background = element_rect(fill = "white"),
#         legend.position = "right",
#         legend.justification = c("right","top"),
#         axis.text.x = element_text(vjust =0.60,angle=60,face = "bold.italic", size = 10))+
#   # theme(axis.text.x = element_text(angle=90),
#   #       )+
#   labs(title="SARS-CoV-2 en regiones", y="Casos por región", 
#        x="Fecha", caption="Source :varios")+
#   bbc_style()
# 
# p2<-ggplot()+
#   geom_line( data=df_lima,mapping = aes(x = Fecha,y = CASOS,colour=REGION,group=REGION)) +
#   scale_colour_manual(values=pal2)+ 
#   scale_x_date(labels = date_format("%d-%b"),breaks = as.Date(c("2020-03-06", "2020-03-11", "2020-03-12", "2020-03-15", "2020-03-26","2020-03-31","2020-04-02","2020-04-07")),
#                minor_breaks = NULL)+
#   geom_vline(data=d, mapping=aes(xintercept=Fecha), color="gray") +
#   geom_text(data=d, mapping=aes(x=Fecha, y =0,label=event), size=4, angle=90, vjust=-0.4, hjust=0)+
#   theme(panel.background = element_rect(fill = "white"),
#         legend.position = "right",
#         legend.justification = c("right","top"),
#         axis.text.x = element_text(vjust =0.60,angle=60,face = "bold.italic", size = 10))+
#   # theme(axis.text.x = element_text(angle=90),
#   #       )+
#   labs(title="SARS-CoV-2 en Lima", y="Casos", 
#        x="Fecha", caption="Source :varios")+
# bbc_style()
# 
# 
# ##### poniendo los dos plots en grupo #######
# png(filename="20200416_pruebas_vs_positivos_COVID19_lima_vs_provincias.png", width=1100, height=600)
# grid.arrange(p1, p2, 
#              ncol = 2, nrow = 2)
# dev.off()
# 


  
  
  
  
  