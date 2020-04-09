library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(directlabels)
library(reshape)
library(colorspace)
######## ploting national cases with overlaping line ##########

# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200406_MD_clean.csv") 
# acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200407_CA_clean.csv.csv") 
acumulado<-fread("~/covid19/data/modificadas/covidPE_PORdia_20200408_MD_clean.csv") 

acumulado1<-melt(acumulado,id="FECHA")
acumulado1$value<-as.numeric(acumulado1$value)
target <- c("TOTAL_POSITIVOS", "TOTAL_PRUEBAS")
acumulado1<-filter(acumulado1, variable %in% target)  


setwd("~/covid19/outputs_covid19/")
# cambio n[umero
png(filename="20200408_pruebas_vs_positivos_COVID19.png")
ggplot(acumulado1, aes(FECHA, value, colour=variable,group=variable)) + 
  geom_line() +
  scale_colour_manual(values=c("red", "blue"))+
  theme(legend.position = c(0.35,0.95),
        legend.justification = c("right","top"))+
  theme(axis.text.x = element_text(angle=90))+
  labs(title="SARS-CoV2 en Peru", y="Pruebas positivas y pruebas realizadas", 
       x="Fecha", caption="Source :varios")+
  geom_vline(xintercept = "2020-03-06") +
  geom_vline(xintercept = "2020-03-11") +
  geom_vline(xintercept = "2020-03-12") +
  geom_vline(xintercept = "2020-03-15") +
  geom_vline(xintercept = "2020-03-26") +
  geom_vline(xintercept = "2020-03-31") +
  geom_vline(xintercept = "2020-04-02") +
  annotate("text", x = "2020-03-07", y = 3000,
           label = " Primer Caso", size = 2.5)+
  annotate("text", x = "2020-03-11", y = 4000,
           label = " Cierre colegios", size = 2.5)+
  annotate("text", x = "2020-03-12", y = 5000,
           label = " Cierre universidades", size = 2.5)+
  annotate("text", x = "2020-03-15", y = 6000,
           label = " inicio de cuarentena", size = 2.5)+
  annotate("text", x = "2020-03-26", y = 7000,
           label = " Extension cuarentena", size = 2.5)+
  annotate("text", x = "2020-04-02", y = 9000,
           label = " Restriccion por sexo", size = 2.5)+
  annotate("text", x = "2020-03-31", y = 8000,
           label = " toque de queda 18pm-5am", size = 2.5)

dev.off()



      
# ggplot() +
#   theme_classic()+
#   # scale_fill_manual(values = pal1)+
#   # scale_x_date(breaks=seq(min(acumulado$FECHA)-1, max(acumulado$FECHA)+1, 1))+
#   geom_line(data = acumulado, aes(x= acumulado$FECHA,y=acumulado$TOTAL_POSITIVOS,group=1), color="red")+
#   geom_line(data = acumulado , aes( x=acumulado$FECHA,y =acumulado$TOTAL_PRUEBAS,group=1 ), color = "blue")+
#   #  annotate("text", x = "2020-03-26", y = 2000,
#   #           label = " Casos positivos", size = 2.5,color="red")+
#   #  annotate("text", x = "2020-03-22", y = 8000,
#   #           label = " Pruebas totales", size = 2.5,color="blue")+
#   # scale_color_manual(values = c("Casos positivos " = "red")) +
#   # scale_color_manual(values = c("Total Pruebas" = "blue")) +
#   # 
#   # 
#   
#   
#   # scale_y_continuous(name="Casos positivos", sec.axis=sec_axis(~./1, name="Pruebas totales")) +
# # scale_x_date(name = "Fecha" )
# #   theme(
# #   axis.title.y.left=element_text(color="red"),
# #   axis.text.y.left=element_text(color="red"),
# #   axis.title.y.right=element_text(color="blue"),
# #   axis.text.y.right=element_text(color="blue")
# # )+
# labs(title="SARS-CoV2 en Peru", y="Pruebas positivas y pruebas realizadas", 
#      x="Fecha", caption="Source :varios")+
#   # theme(axis.line.y.right = element_line(color = "red"), 
#   #       axis.ticks.y.right = element_line(color = "red"),
#   #       axis.text.y.right = element_text(color = "red"), 
#   #       axis.title.y.right = element_text(color = "red")
#   # )+
#   geom_vline(xintercept = "2020-03-06") +
#   geom_vline(xintercept = "2020-03-11") +
#   geom_vline(xintercept = "2020-03-12") +
#   geom_vline(xintercept = "2020-03-15") +
#   geom_vline(xintercept = "2020-03-26") +
#   geom_vline(xintercept = "2020-03-31") +
#   geom_vline(xintercept = "2020-04-02") +
#   annotate("text", x = "2020-03-07", y = 3000,
#            label = " Primer Caso", size = 2.5)+
#   annotate("text", x = "2020-03-11", y = 4000,
#            label = " Cierre colegios", size = 2.5)+
#   annotate("text", x = "2020-03-12", y = 5000,
#            label = " Cierre universidades", size = 2.5)+
#   annotate("text", x = "2020-03-15", y = 6000,
#            label = " inicio de cuarentena", size = 2.5)+
#   annotate("text", x = "2020-03-26", y = 7000,
#            label = " Extension cuarentena", size = 2.5)+
#   annotate("text", x = "2020-04-02", y = 9000,
#            label = " Restriccion por sexo", size = 2.5)+
#   annotate("text", x = "2020-03-31", y = 8000,
#            label = " toque de queda 18pm-5am", size = 2.5)+
#   # annotate("text", x = "2020-04-02", y = 7000,
#   #          label = " Restriccion por sexo", size = 2.5)+
#   #   # ylab("Casos Positivos") +
#   # theme_minimal() +
#   theme(legend.position = c(0.95,0.95),
#         legend.justification = c("right","top"))+
#   theme(axis.text.x = element_text(angle=90)
#   )
















