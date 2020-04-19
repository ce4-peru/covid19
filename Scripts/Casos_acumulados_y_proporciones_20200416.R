
###### haciendo curvas de casos positivos nuevos y acumulados en el pais  #####
################codigo adaptado de Gabriel Carrazco ###############################

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
library(JLutils)
#### si no tienes el paquete bbplot 
# devtools::install_github('bbc/bbplot')
library(bbplot)
library(scales)
library("gridExtra")
library(cowplot)

######## ploting national cases with overlaping line ##########
d=data.frame(Fecha=as.Date(c("2020-03-06", "2020-03-11", "2020-03-12", "2020-03-15", "2020-03-26","2020-03-31","2020-04-02","2020-04-07")), event=c(" Primer Caso", " Cierre colegios", "Cierre universidades", " Inicio de cuarentena", " Extensión cuarentena", " Restricción por sexo","Toque de queda 18pm-5am","Extensión cuarentena"))

dat_acumulado<-fread("../data/modificadas/covidPE_PORdia_20200414_CA_clean.csv") 


###### data sets para hacer la grafica de casos positivos y pruebas negativas nacionales 
dat_acumulado<-dat_acumulado %>%
  mutate(Fecha = as.Date(FECHA))
dat1_acumulado <- dat_acumulado %>%
  mutate(pos_new = TOTAL_POSITIVOS-lag(TOTAL_POSITIVOS,default = 0),
         des_new = TOTAL_DESCARTADOS-lag(TOTAL_DESCARTADOS,default = 0),
         tot_pruebas = pos_new+des_new) 

#### sumarizando por FECHA pero asegurate que lees primero  
## el paquete dyplyr y despues tydir si no no funciona
dat1_acumulado<-dat1_acumulado%>%group_by(Fecha) %>%
  summarise(pos_new = sum(pos_new), des_new = sum(des_new)) %>%
  mutate(cum_pos = cumsum(pos_new),
         tot_pruebas = pos_new+des_new)


##### data set dos para hacer grafico de proporciones de positivos y negativos 
dat2_acumulado <- dat1_acumulado %>%
  mutate(neg_new = tot_pruebas-pos_new) %>%
  dplyr::select(Fecha, pos_new, neg_new) 
setnames(dat2_acumulado,old = "pos_new",new = "Positivo")
setnames(dat2_acumulado,old = "neg_new",new = "Negativo")
dat2_acumulado<-dat2_acumulado%>%gather(res, count, -Fecha) %>%
  uncount(count)


###### graficando casos totales y pruebas negativas nacionales#####

f1 <- dat1_acumulado %>%
  ggplot(aes(x = Fecha)) +
  geom_bar(aes(y = pos_new, fill = "Nuevos"), stat = "identity", alpha=.5) + 
  geom_line(aes(y = cum_pos, col = "Acumulados"), size=1) +
  geom_point(aes(y = cum_pos), col = "#8B1C62") +
  # geom_text(aes(y = pos_new, label = pos_new), vjust = -0.5, col = "#43CD80") +
  # geom_text(aes(y = cum_pos, label = cum_pos), vjust = -0.5, col = "#8B1C62") +
  labs(y = "Numero de casos reportados", color = " Casos", fill = " ", 
       title = paste0("Numero de casos confirmados \nhasta: ", Sys.Date())) +
  scale_fill_manual(values = c("Nuevos" = "#43CD80")) +
  scale_color_manual(values = c("Acumulados" = "#8B1C62")) +
  scale_x_date(labels = date_format("%d-%m")) +
  scale_y_continuous(sec.axis = sec_axis(~ .)) +
  theme_minimal() +
  theme(legend.position="bottom")

#### GUARDANDO EL GRAFICO INTERACTIVO COMO HTML ###### 
inter1<-ggplotly(f1)
htmlwidgets::saveWidget(inter,"~/covid19/outputs_covid19/grafico_interactivo_casos_totales.html")



#### Graficando proporcciones negativos y positivos#########  
f2 <- dat2_acumulado %>%
  ggplot(aes(x = Fecha, fill = res)) +
  geom_bar(position = "fill") +
  stat_fill_labels(size=2) +
  scale_fill_discrete_sequential(palette="BluGrn") +
  labs(y = "proportion", fill = "Resultado",
       title = paste0("Proporcion de positivos del total \nde muestras analizadas hasta: ", Sys.Date())) +
  theme_bw() +
  theme(legend.position="bottom")
#### guradando el grafico interactivo como HTML#####
inter2<-ggplotly(f2)
htmlwidgets::saveWidget(inter,"~/covid19/outputs_covid19/grafico_interactivo_casos_totales.html")

### guardando como png #######
png(filename="20200415__COVID19_casos_confirmados_nacional_y proprociones.png", width=1100, height=600)
# grid.arrange(p1, p2, 
#              ncol = 2, nrow = 2)
plot_grid(f1,f2, rel_widths = c(4,3), labels = c("A","B"))
dev.off()



