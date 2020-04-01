library(JLutils)
library(magrittr)
library(ggthemr)
library(ggplot2)
library('incidence')
library('outbreaks')

setwd('~/covid19PE-LIEZ/outputs_covid19/')
####casos_acumulado por semana#####
### weekly incidense #####
acumulado_dep<-fread("~/covid19PE-LIEZ/data/crudas/covidPE_IND_20200326_MD.csv")
acumulado_dep <- mutate_if(acumulado_dep, is.character, toupper)
acumulado_dep<-acumulado_dep %>%
  mutate(Fecha = as.Date(as.character(Fecha), format = "%d/%m/%y"))
str(acumulado_dep, strict.width = "cut", width = 76)

i.7.group <- incidence(acumulado_dep$Fecha, interval = 7, groups = acumulado_dep$Región)
my_theme <- theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black"))

png(filename="20202803_weekly_incid_COVID19.png")
plot(i.7.group, border = "white") +
  my_theme +
  # theme(legend.position = "topleft")
 theme(legend.position = c(-1.0, -1.0))
dev.off()


#### drawing cases per day aggregate per region #####inci

###  daily incidence  
data_acumulada_diaria<- fread('~/covid19PE-LIEZ/data/crudas/covidPE_acumulado_20200327_CA.csv')
data_acumulada_diaria <- mutate_if(data_acumulada_diaria, is.character, toupper)
data_acumulada_diaria<-data_acumulada_diaria %>%
  mutate(Fecha = as.Date(as.character(Fecha), format = "%d/%m/%y"))  
data_acumulada_diaria[is.na(data_acumulada_diaria)] <- 0

i.group <- as.incidence(x = data_acumulada_diaria[, 3:16], dates = data_acumulada_diaria$Fecha)
i.pooled <- pool(i.group)

png(filename="20202803_daily_incid_COVID19.png")
plot(i.group, border = "white") + my_theme + theme(legend.position = c(0.9, 0.7))
dev.off()

  



