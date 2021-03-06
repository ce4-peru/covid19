## 11 june 2018 ##
## This script explore the samples analised at the INS-AQP

library(ggplot2)
library(data.table)
library(raster)
#library(car)
library(dplyr)
library(plyr)
#install.packages("tmap")
library(tmap)
library(ggmap)
library(rgeos)
library(rgdal)

# Import the csv files.
setwd("C:/Users/Micaela/Documents/Rabies")
#data <- read.csv("Casos_INS/Scripts/data/Base_perrospositivos_2015-2018_20Feb18.csv")
# i added new cases. check information.
data <- read.csv("Casos_INS/Scripts/data/Base_perrospositivos_2015-2018_23Mar18.csv")
paper.data <- read.csv("Casos_INS/Scripts/outputs/casos_papertorrenteras_2coord.csv")
AQP_AQP_districts <- readOGR("Casos_INS/Scripts/outputs/AQP_AQP_province_districts.shp", 
                             "AQP_AQP_province_districts")
AQP_prov <- readOGR("Casos_INS/Scripts/outputs/AQP_region_provinces.shp", 
                             "AQP_region_provinces")

## FULL DATA ##
data2014_full <- read.csv("Casos_INS/Scripts/data/Muestras_INS_2014.csv")
data2015_full <- read.csv("Casos_INS/Scripts/data/Muestras_INS_2015.csv")
data2016_full <- read.csv("Casos_INS/Scripts/data/Muestras_INS_2016.csv")
data2017_full <- read.csv("Casos_INS/Scripts/data/Muestras_INS_2017.csv")








names(data)
# [1] "CODIGO"          "F..RECEPCION"    "F..OBTENCION"    "F..MUERTE"      
# [5] "DEPARTAMENTO"    "PROVINCIA"       "DISTRITO"        "LOCALIDAD"      
# [9] "ESPECIE"         "EESS"            "IFD"             "FECHA.RESULTADO"
# [13] "RESULTADO.INS"   "INOC.RATONES"   

head(data)
str(data) # 140 obs. of  14 variables:

# Create year and month variables
# solve an issue
data[25,2] # 17/01/1900
data[25,2] <- "17/02/2016" # replace for info in column 3: F..OBTENCION

data$year <- substr(data$F..RECEPCION, 7, 10)
data$month <- substr(data$F..RECEPCION, 4, 5)
class(data$year)
class(data$month)

# Create numeric variables
data$nyear <- as.numeric(data$year)
data$nmonth <- as.numeric(data$month)

# Create monthly Timesteps
data$nmonth_s <- (12*(data$nyear-2015)) + data$nmonth

# Create quarters
data$quarter<- data$nmonth
class(data$quarter) # [1] "numeric"
data$quarter[data$quarter == 2] <- 1
data$quarter[data$quarter == 3] <- 1
data$quarter[data$quarter == 4] <- 2
data$quarter[data$quarter == 5] <- 2
data$quarter[data$quarter == 6] <- 2
data$quarter[data$quarter == 7] <- 3
data$quarter[data$quarter == 8] <- 3
data$quarter[data$quarter == 9] <- 3
data$quarter[data$quarter == 10] <- 4
data$quarter[data$quarter == 11] <- 4
data$quarter[data$quarter == 12] <- 4

# Create quarterly timesteps
data$quarter_s <- (4*(data$nyear-2015)) + data$quarter


# Quadrimester
data$term <- data$nmonth
data$term[data$term == 1] <- "Dic-Mar"
data$term[data$term == 2] <- "Dic-Mar"
data$term[data$term == 3] <- "Dic-Mar"
data$term[data$term == 4] <- "Abr-Jul"
data$term[data$term == 5] <- "Abr-Jul"
data$term[data$term == 6] <- "Abr-Jul"
data$term[data$term == 7] <- "Abr-Jul"
data$term[data$term == 8] <- "Ago-Nov"
data$term[data$term == 9] <- "Ago-Nov"
data$term[data$term == 10] <-"Ago-Nov"
data$term[data$term == 11] <-"Ago-Nov"
data$term[data$term == 12] <-"Dic-Mar"

# Season
data$season <- data$nmonth
data$season[data$season == 1] <- "Verano" # 
data$season[data$season == 2] <- "Verano"
data$season[data$season == 3] <- "Oto�o"
data$season[data$season == 4] <- "Oto�o"
data$season[data$season == 5] <- "Oto�o"
data$season[data$season == 6] <- "Invierno"
data$season[data$season == 7] <- "Invierno"
data$season[data$season == 8] <- "Invierno"
data$season[data$season == 9] <- "Primavera"
data$season[data$season == 10] <-"Primavera"
data$season[data$season == 11] <-"Primavera"
data$season[data$season == 12] <-"Verano"

# season2
data$season2 <- data$nmonth
data$season2[data$season2 == 1] <- "Lluvias" # 
data$season2[data$season2 == 2] <- "Lluvias"
data$season2[data$season2 == 3] <- "Lluvias"
data$season2[data$season2 == 4] <- "Seca"
data$season2[data$season2 == 5] <- "Seca"
data$season2[data$season2 == 6] <- "Seca"
data$season2[data$season2 == 7] <- "Seca"
data$season2[data$season2 == 8] <- "Seca"
data$season2[data$season2 == 9] <- "Seca"
data$season2[data$season2 == 10] <-"Lluvias"
data$season2[data$season2 == 11] <-"Lluvias"
data$season2[data$season2 == 12] <-"Lluvias"


### CHECK DISTRITO ###
table(data$DISTRITO) # some repeated
levels(data$DISTRITO)
# [1] " LOS GIRASOLES Q13 ZNAA"    "A.S.A"                      "A.S.A."                    
# [4] "C. COLORADO"                "CAYMA"                      "CERCADO"                   
# [7] "HUNTER"                     "J. L. B Y R"                "M. MELGAR"                 
# [10] "MIRAFLORES"                 "PAUCARPATA"                 "SACHACA"                   
# [13] "SOCABAYA"                   "UCHUMAYO"                   "URB C. DE JESUS H9SOCABAYA"
# [16] "YURA"                       "YURA "                

# Obtains rows of  1 and 15. 1 should be plot and 15 is SOCABAYA
names(data)
issue1 <- data[data$DISTRITO == " LOS GIRASOLES Q13 ZNAA",]
issue2 <- data[data$DISTRITO == "URB C. DE JESUS H9SOCABAYA",]

# Change issue 1
data[8,7] <- "SOCABAYA"

# Change issue 2
issue2.xy <- paper.data[7,]
# find out district
map <- get_map(location = c(lon = -71.5, lat = -16.375),     
               zoom = 14,                # 5 to set a higher scale.
               maptype = "roadmap",     # setting easy to visualise
               source = "google")       # the source to load the map from
ggmap(map) + xlab("Longitude") + ylab("Latitude") +
  geom_point(data = issue2.xy, # Add points for sites using latitude and longitude.
             aes(x = long, y = lat), # Set longitudes and latitudes on axes.
             color = "red", size = 5)
# It belongs to Miraflores
data[7,7] <- "MIRAFLORES"
data$DISTRITO <- factor(data$DISTRITO)

# Replace when necessary
levels(data$DISTRITO)
# [1] "A.S.A"       "A.S.A."      "C. COLORADO" "CAYMA"       "CERCADO"     "HUNTER"     
# [7] "J. L. B Y R" "M. MELGAR"   "MIRAFLORES"  "PAUCARPATA"  "SACHACA"     "SOCABAYA"   
# [13] "UCHUMAYO"    "YURA"        "YURA " 

# change
data$DISTRITO[data$DISTRITO == "A.S.A"] <- "A.S.A."
data$DISTRITO[data$DISTRITO == "YURA "] <- "YURA"
data$DISTRITO <- factor(data$DISTRITO)
levels(data$DISTRITO)
# [1] "A.S.A."      "C. COLORADO" "CAYMA"       "CERCADO"     "HUNTER"     
# [6] "J. L. B Y R" "M. MELGAR"   "MIRAFLORES"  "PAUCARPATA"  "SACHACA"    
# [11] "SOCABAYA"    "UCHUMAYO"    "YURA"

###########################################################################

## PER YEAR ##
table(data$nyear)
# 2015 2016 2017 2018 
# 19   59   48    14 

## Sum the number of cases in each year
rabies.y <- hist(data$nyear, breaks=2014:2018, plot=FALSE)$counts

## Plot the graph
png("Casos_INS/Scripts/outputs/Cases_years_scp1.png", width=8, height=4, 
    units="in", res=300)
plot.y <- plot(rabies.y, ylab ="N�mero de perros positivos",
               type="o", col="red", axes= FALSE, 
               main="Casos de rabia canina en AQP",
               lwd=2, pch=19, ylim=c(0,60), ann=FALSE)
axis(1, at=1:4, lab=c(2015:2018)) ## Set axes and title
axis(2, at = seq(0, 60, by = 10), las=2)
box()
title(main="Casos de rabia canina en AQP", xlab="A�os", ylab="N�mero de perros positivos")
mtext("*hasta marzo", line = -13.5, adj=1.02, cex = 0.75)
#abline(h=15, col="red", lty=2, lwd=1.5)
dev.off()

########################################################################
#### Number of cases per month 
table(data$nmonth)
# 1  2  3  4  5  6  7  8  9 10 11 12 
# 10 13 23 11  8  9 13  6 11 14 11 11 

## Sum the number of cases per quarter
rabies.m <- hist(data$nmonth, breaks=0:12, plot=FALSE)$counts

## Plot the graph
png("Casos_INS/Scripts/outputs/Cases_monthss_bp1.png", 
    width=7, height=4, units="in", res=300)
bp <- barplot(rabies.m, axes=FALSE, axisnames = FALSE,
              main="Acumulaci�n de casos de rabia canina en Arequipa \npor mes del a�o desde el inicio del brote", 
              ylim = c(0, 25))
axis(2, ylim=c(0,25), cex=1.2, las=2)
text(bp, par("usr")[3], labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                   "Jul", "Ago", "Set", "Oct", "Nov", "Dic"), 
     adj = c(0.5,1.2), xpd = TRUE, cex=1.2)
#mtext("N�mero de perros positivos", side=2,line=4, cex=1.3)
# More cases in the first quarter
dev.off()


########################################################################
#### Number of cases per monthly timesteps 
table(data$nmonth_s)
# 3  4  5  7  8  9 10 11 12 13 14 15 16 17 18 19 21 22 23 24 25 26 27 28 29 30 31 32 33 
# 5  2  1  1  3  2  1  3  1  1  5  6  5  3  4  7  6 10  4  8  5  3  7  4  4  5  5  3  3 
# 34 35 36 37 38 39 
# 3  4  2  4  5  5 

## Sum number of cases per quarter timestep (Qmarize cases by quarter.
rabies.qs <- hist(data$nmonth_s, breaks=0:39, plot=FALSE)$counts

## Plot the lineplot
png("Casos_INS/Scripts/outputs/Cases_mseries_scp2.png", width=15, height=5, 
    units="in", res=300)
plot.qs <- plot(rabies.qs,
                ylab ="Nuevos perros positivos",
                type="o", col="red", axes=FALSE, 
                main="Casos de rabia canina por mes secuencialmente \ndesde el inicio del brote",
                lwd=1.5, pch=19, ann=FALSE, cex=0.7)
axis(1, lab=c(1:39), at=1:39, cex=1)
axis(2, at = seq(0, 10, by = 1), cex=1.2, las=2)
mtext("2015", line = -18.7, adj=0.16, cex = 0.75)
mtext("2016", line = -18.7, adj=0.46, cex = 0.75)
mtext("2017", line = -18.7, adj=0.76, cex = 0.75)
mtext("2018", line = -18.7, adj=0.95, cex = 0.75)
box()
title(main="Casos de rabia canina por mes secuencialmente \ndesde el inicio del brote", 
      xlab="Meses secuenciales",
      ylab="N�mero de casos",
      cex=1.3)
dev.off()
# We still see the peaks at first quarters.


####
#### Number of cases per quarter, peruvian calendar quarters
table(data$quarter) #2015-2018
# 1  2  3  4 
# 41 28 30 36  

# Delete 2018 just now
data1 <- data[data$nyear != 2018,]
table(data1$quarter) #2016-2018
# 1  2  3  4 
# 32 28 30 36 

# Delete 2018 just now
table(data$quarter) #2015-2018
# 1  2  3  4 
# 41 28 30 36 


## Sum the number of cases per quarter
rabies.q <- hist(data1$quarter, breaks=0:4, plot=FALSE)$counts

## Plot the graph
png("Casos_INS/Scripts/outputs/rabies_quarters_bp_2015-2017.png", width=6, height=4, units="in", res=300)
bp <- barplot(rabies.q, axes=FALSE, axisnames = FALSE,
              main="Arequipa 2015 - 2017", ylim = c(0, 40))
axis(2, ylim=c(0,40), cex=1.2, las=2)
text(bp, par("usr")[3], labels = c("Ene-Mar", "Abr-Jun", 
                                   "Jul-Set", "Oct-Dic"), 
     adj = c(0.5,1.2), xpd = TRUE, cex=1.2)
mtext("N�mero de perros positivos", side=2,line=4, cex=1)
# More cases in the first quarter
dev.off()

## Sum the number of cases per quarter
rabies.q <- hist(data$quarter, breaks=0:4, plot=FALSE)$counts

## Plot the graph
png("Casos_INS/Scripts/outputs/rabies_quarters_bp_2015-2018.png", width=6, height=4, units="in", res=300)
bp <- barplot(rabies.q, axes=FALSE, axisnames = FALSE,
              main="Arequipa 2015 - 2018", ylim = c(0, 40))
axis(2, ylim=c(0,40), cex=1.2, las=2)
text(bp, par("usr")[3], labels = c("Ene-Mar", "Abr-Jun", 
                                   "Jul-Set", "Oct-Dic"), 
     adj = c(0.5,1.2), xpd = TRUE, cex=1.2)
mtext("N�mero de perros positivos", side=2,line=4, cex=1)
# More cases in the first quarter
dev.off()


#### Number of cases per term - quadrimester 
table(data$term)
# Abr-Jul Ago-Nov Dic-Mar 
# 37      33      46 

# Delete 2018 just now
data1 <- data[data$nyear != 2018,]
table(data1$term) #2016-2018
# Abr-Jul Ago-Nov Dic-Mar 
# 37      33      37 

## Sum the number of cases per quarter
rabies.t <- table(data1$term)

## Plot the graph
png("Casos_INS/Scripts/outputs/rabies_term_bp.png", width=6, height=4, units="in", res=300)
bp <- barplot(rabies.t, axes=FALSE, axisnames = FALSE,
              main="Arequipa 2015 - 2017", ylim = c(0, 40))
axis(2, ylim=c(0,40), cex=1.2, las=2)
text(bp, par("usr")[3], labels = c("Abr-Jul", "Ago-Nov", 
                                   "Dic-Mar"), 
     adj = c(0.5,1.2), xpd = TRUE, cex=1.2)
mtext("N�mero de perros positivos", side=2,line=4, cex=1)
# More cases in the first quarter
dev.off()


#### Number of cases per season
table(data$season)
# Invierno Oto�o Primavera    Verano 
# 24        29        30        33 

# Delete 2018 just now
data1 <- data[data$nyear != 2018,]
table(data1$season) #2016-2018
# Invierno     Oto�o Primavera    Verano 
# 24        29        30        24

## Sum the number of cases per season
rabies.s <- table(data1$season)

## Plot the graph
png("Casos_INS/Scripts/outputs/rabies_season.png", width=6, height=4, units="in", res=300)
bp <- barplot(rabies.s, axes=FALSE, axisnames = FALSE,
              main="Arequipa 2015 - 2017", ylim = c(0, 40))
axis(2, ylim=c(0,40), cex=1.2, las=2)
text(bp, par("usr")[3], labels = c("Invierno", "Oto�o", 
                                   "Primavera", "Verano"), 
     adj = c(0.5,1.2), xpd = TRUE, cex=1.2)
mtext("N�mero de perros positivos", side=2,line=4, cex=1)
# More cases in the first quarter
dev.off()


#### Number of cases per simple season
table(data$season2)
# Lluvias    Seca 
# 67      49 

## Sum the number of cases per season
rabies.s2 <- table(data$season2)

## Plot the graph
png("Casos_INS/Scripts/outputs/rabies_season2.png", width=6, height=4, units="in", res=300)
bp <- barplot(rabies.s2, axes=FALSE, axisnames = FALSE,
              main="Arequipa 2016 - 2018", ylim = c(0, 70))
axis(2, ylim=c(0,70), cex=1.2, las=2)
text(bp, par("usr")[3], labels = c("Lluvias", "Seca"), 
     adj = c(0.5,1.2), xpd = TRUE, cex=1.2)
mtext("N�mero de perros positivos", side=2,line=4, cex=1)
dev.off()
# More cases in the first quarter


# MIXES
table(data$year, data$quarter)
#       1  2  3  4
# 2015  5  3  6  5
# 2016 12 12 13 22
# 2017 15 13 11  9
# 2018  9  0  0  0

table(data$year, data$season)
#        Invierno Oto�o Primavera Verano
# 2015        4     8         6      1
# 2016       11    14        20     14
# 2017       13    15        10     10
# 2018        0     0         0      9

table(data$year, data$term)
#        Abr-Jul Ago-Nov Dic-Mar
# 2015       4       9       6
# 2016      19      20      20
# 2017      18      13      17
# 2018       0       0       9

table(data$year, data$season2)
#        Lluvias Seca
# 2015      10    9
# 2016      34   25
# 2017      24   24
# 2018       9    0



########################################################################
#### Number of cases per country in WE ####
table(data$DISTRITO)
# A.S.A. C. COLORADO       CAYMA     CERCADO      HUNTER J. L. B Y R   M. MELGAR 
# 7          59           7           2           1           5          30 
# MIRAFLORES  PAUCARPATA     SACHACA    SOCABAYA    UCHUMAYO        YURA 
# 11           4           1           2           2           9 

## Sum the number of cases found in each country
A <- table(data$DISTRITO)
A_df <- data.frame(A)
names(A_df) # [1] "Var1" "Freq"

# Order df
A_df_order <- sort(A_df$Freq, decreasing = TRUE)
A_df_order <- A_df[with(A_df, order(Freq, decreasing = TRUE)), ]

# Extract districts for labels
labels <- unique(A_df_order$Var1)

## Plot the graph
png("Casos_INS/Scripts/outputs/rabies_districts_bp3.png", 
    width=11, height=6, units="in", res=300)
plot.c <-barplot(A_df_order$Freq,
                 ylab ="N�mero de perros positivos",
                 ylim = c(0,60),
                 col = "grey", cex.lab=1.3,
                 cex.axis = 1.2)

text(plot.c, par("usr")[3], 
     labels = labels, #A_df$Var1, 
     adj = c(1, 1.5), xpd = TRUE, cex=1, srt = 50)
abline(h=20, col="red", lty=2, lwd=2)
dev.off()
# Cerro colorado by far.


### STACKED PLOT DISTRICTS AND YEAR
table(data$DISTRITO, data$year)

## Summarize in a dataframe
data$number <- 1

sum.dy <- tapply(data$number, 
                 list(data$DISTRITO, data$year), sum)
sum.rabies.dy <- data.frame(sum.dy) # dataframe
print(sum.dy)
class(sum.dy) # matrix

## Change structure of the dataframe.
# Makes easier to plot later
df <- melt(sum.dy,  id.vars = sum.rabies.dy$year,
           variable.name = sum.rabies.dy$DISTRITO)

# Change NAs with 0s
df[is.na(df)] <- 0

png("Casos_INS/Scripts/outputs/rabies_districts_years_scp1.png", width=11, height=6, units="in", res=300)
ggplot(df, aes(x = Var2, y = value, fill = Var1)) + 
  geom_area(position = 'stack') +
  labs(x = "A�os", y = "N�mero de casos") +
  scale_x_continuous(breaks=seq(2015,2018,1)) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text.y = element_text(size = 12)) + 
  ggtitle("Casos de rabia canina en AQP (2015 - marzo 2018)") +
  #theme(legend.text = element_text(size = 23)) +
  guides(fill=guide_legend(title=NULL))
#+ scale_linetype_discrete(name = "Country") + facet_grid(. ~ V2)
dev.off()


### STACKED PLOT DISTRICTS AND MONTHS
table(data$DISTRITO, data$nmonth_s)

## Summarize in a dataframe
data$number <- 1
sum.dm <- tapply(data$number, 
                 list(data$DISTRITO, data$nmonth_s), sum)
sum.rabies.dm <- data.frame(sum.dm) # dataframe
print(sum.dm)
class(sum.dm) # matrix

## Change structure of the dataframe.
df <- melt(sum.dm,  id.vars = sum.rabies.dm$nmonth_s,
           variable.name = sum.rabies.dm$DISTRITO)

# Change NAs with 0s
df[is.na(df)] <- 0

png("Casos_INS/Scripts/outputs/rabies_districts_months_scp.png", width=11, height=6, units="in", res=300)
ggplot(df, aes(x = Var2, y = value, fill = Var1)) + 
  geom_area(position = 'stack') +
  labs(x = "Meses-timesteps", y = "Incidencia de rabia canina") +
  scale_x_continuous(breaks=seq(1,26,1)) +
  scale_y_continuous(breaks=seq(1,10,2)) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text.y = element_text(size = 12)) + 
  #theme(legend.title = element_text(size=16, face="bold")) +
  #theme(legend.text = element_text(size = 23)) +
  guides(fill=guide_legend(title=NULL))
#+ scale_linetype_discrete(name = "Country") + facet_grid(. ~ V2)
dev.off()


### STACKED PLOT DISTRICTS AND QUARTERS
table(data$DISTRITO, data$quarter_s)

## Summarize in a dataframe
data$number <- 1
sum.dm <- tapply(data$number, 
                 list(data$DISTRITO, data$quarter_s), sum)
sum.rabies.dm <- data.frame(sum.dm) # dataframe
print(sum.dm)
class(sum.dm) # matrix

## Change structure of the dataframe.
df <- melt(sum.dm,  id.vars = sum.rabies.dm$quarter_s,
           variable.name = sum.rabies.dm$DISTRITO)

# Change NAs with 0s
df[is.na(df)] <- 0

png("Casos_INS/Scripts/outputs/rabies_districts_quarters_scp.png", width=11, height=6, units="in", res=300)
ggplot(df, aes(x = Var2, y = value, fill = Var1)) + 
  geom_area(position = 'stack') +
  labs(x = "Trimestres-timesteps", y = "Incidencia de rabia canina") +
  scale_x_continuous(breaks=seq(1,9,1)) +
  scale_y_continuous(breaks=seq(1,25,2)) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text.y = element_text(size = 12)) + 
  #theme(legend.title = element_text(size=16, face="bold")) +
  #theme(legend.text = element_text(size = 23)) +
  guides(fill=guide_legend(title=NULL))
#+ scale_linetype_discrete(name = "Country") + facet_grid(. ~ V2)
dev.off()


#### MAP ####
# Call shapefiles
### DISTRICT AND BEYOND...
#install.packages("getData")

# #library(getData)
# tdcero <- tempdir() # save the directory
# peru0 <- getData('GADM', country=c('PER'), level=0, path=tdcero)
# peru1 <- getData('GADM', country=c('PER'), level=1, path=tdcero)
# peru2 <- getData('GADM', country=c('PER'), level=2, path=tdcero)
# peru3 <- getData('GADM', country=c('PER'), level=3, path=tdcero)
# 
# # plot
# plot(peru0)
# plot(peru1)
# plot(peru2)
# plot(peru3)
# 
# # get df
# peru0_df <- peru0@data
# peru1_df <- peru1@data
# peru2_df <- peru2@data
# peru3_df <- peru3@data
# 
# # check variables in gadm2
# names(peru2@data)
# # [1] "OBJECTID"  "ID_0"      "ISO"       "NAME_0"    "ID_1"      "NAME_1"   
# # [7] "ID_2"      "NAME_2"    "HASC_2"    "CCN_2"     "CCA_2"     "TYPE_2"   
# # [13] "ENGTYPE_2" "NL_NAME_2" "VARNAME_2"
# head(peru2@data, 3)
# # OBJECTID ID_0 ISO NAME_0 ID_1   NAME_1 ID_2      NAME_2   HASC_2 CCN_2 CCA_2
# # 1        1  178 PER   Peru    1 Amazonas    1       Bagua PE.AM.BG    NA      
# # 2        2  178 PER   Peru    1 Amazonas    2     Bongar� PE.AM.BN    NA      
# # 3        3  178 PER   Peru    1 Amazonas    3 Chachapoyas PE.AM.CP    NA      
# # TYPE_2 ENGTYPE_2 NL_NAME_2 VARNAME_2
# # 1 Provincia  Province                    
# # 2 Provincia  Province             Bongart
# # 3 Provincia  Province  
# 
# ## Keep Arequipa only. It has Arequipa region with its 8 provinces
# AQP_prov <-  peru2[peru2$ID_1==4,]
# AQP_prov_df <- AQP_prov@data
# 
# 
# ## check variables in gadm3
# names(peru3@data)
# head(peru3@data, 3)
# # OBJECTID ID_0 ISO NAME_0 ID_1   NAME_1 ID_2 NAME_2 ID_3   NAME_3 CCN_3 CCA_3
# # 1        1  178 PER   Peru    1 Amazonas    1  Bagua    1 Aramango    NA      
# # 2        2  178 PER   Peru    1 Amazonas    1  Bagua    2 Copallin    NA      
# # 3        3  178 PER   Peru    1 Amazonas    1  Bagua    3 El Parco    NA      
# # TYPE_3 ENGTYPE_3 NL_NAME_3 VARNAME_3
# # 1 Distrito  District                    
# # 2 Distrito  District                    
# # 3 Distrito  District   
# 
# # Keep ONLY districts in Arequipa province
# AQP_districts <-  peru3[peru3$ID_1==4,]
# AQP_AQP_districts <- AQP_districts[AQP_districts$ID_2==35,]
# AQP_AQP_districts_df <- AQP_AQP_districts@data
# str(AQP_AQP_districts_df)
# AQP_AQP_districts_df$fNAME_3 <- factor(AQP_AQP_districts_df$NAME_3)
# levels(AQP_AQP_districts_df$fNAME_3)
# # [1] "Alto Selva Alegre"             "Arequipa"                     
# # [3] "Cayma"                         "Cerro Colorado"               
# # [5] "Characato"                     "Chiguata"                     
# # [7] "Jacobo Hunter"                 "Jose Luis Bustamante Y Rivero"
# # [9] "La Joya"                       "Laguna Loriscota"             
# # [11] "Mariano Melgar"                "Miraflores"                   
# # [13] "Mollebaya"                     "Paucarpata"                   
# # [15] "Pocsi"                         "Polobaya"                     
# # [17] "Queque�a"                      "Sabandia"                     
# # [19] "Sachaca"                       "San Juan de Siguas"           
# # [21] "San Juan de Tarucani"          "Santa Isabel de Siguas"       
# # [23] "Santa Rita de Siguas"          "Socabaya"                     
# # [25] "Tiabaya"                       "Uchumayo"                     
# # [27] "Vitor"                         "Yanahuara"                    
# # [29] "Yarabamba"                     "Yura"  
# 
# # Plotear
# plot(AQP_AQP_districts, col="ivory2")
# plot(AQP_prov, add=TRUE, lwd=2)
# plot(peru0, add=TRUE, lwd=3)
# box()
# 
# Square for plotthe delimitated extent
square_AQP <- crop(AQP_prov, extent(-71.9, -71.25, -15.9, -16.6))
square_AQP <- crop(AQP_prov, extent(-71.84, -71.36, -15.9, -16.55))

plot(AQP_AQP_districts)
plot(square_AQP, border="red", col="white")

# Calculate area
areas <- sapply(slot(AQP_AQP_districts, "polygons"), slot, "area")
areas.km <-areas*10000 # transform to km
areas.km #
AQP_AQP_districts@data$area <- round(areas.km, digits = 2)
AQP_AQP_districts_df <- AQP_AQP_districts@data

# Recode names
AQP_AQP_districts$NAME_3 <- recode(AQP_AQP_districts$NAME_3,  
                                 `Alto Selva Alegre` = "A.S.A.", 
                                 `Cayma` = "CAYMA", 
                                 `Cerro Colorado` = "C. COLORADO", 
                                 `Jacobo Hunter` = "HUNTER", 
                                 `Jose Luis Bustamante Y Rivero` = "J. L. B Y R", 
                                 `Mariano Melgar` = "M. MELGAR", 
                                 `Miraflores` = "MIRAFLORES", 
                                 `Paucarpata` = "PAUCARPATA",
                                 `Sachaca` = "SACHACA", 
                                 `Socabaya` = "SOCABAYA",
                                 `Uchumayo` = "UCHUMAYO",
                                 `Yura` = "YURA",
                                 `Arequipa` = "AREQUIPA",
                                 `Characato` = "CHARACATO",
                                 `Chiguata` = "CHIGUATA",
                                 `La Joya` = "LA JOYA",
                                 `Laguna Loriscota` = "L. LORISCOTA",
                                 `Mollebaya` = "MOLLEBAYA",
                                 `Pocsi` = "POCSI",
                                 `Polobaya` = "POLOBAYA",
                                 `Queque�a` = "QUEQUE�A",
                                 `Sabandia` = "SABANDIA",
                                 `San Juan de Siguas` = "S.J. Siguas",
                                 `San Juan de Tarucani` = "S.J. Tarucani",
                                 `Santa Isabel de Siguas` = "S.I. de Siguas",
                                 `Santa Rita de Siguas` = "S.R. de Siguas",
                                 `Tiabaya` = "TIABAYA",
                                 `Vitor` = "VITOR",
                                 `Yanahuara` = "YANAHUARA",
                                 `Yarabamba` = "YARABAMBA")
AQP_AQP_districts_df <- AQP_AQP_districts@data


### CREATE A SUMMARY WITH CASES IN 2017 ###
# data2017 <- data[data$nyear == 2017,]
# districts2017 <- as.data.frame(table(data2017$DISTRITO))
# # Changes names for them to match
# names(districts2017) # [1] "Var1" "Freq"
# colnames(districts2017)[1] <- "NAME_3" # change names
# colnames(districts2017)[2] <- "CASES"
# names(districts2017) # [1] "NAME_3" "CASES" 
# districts2017$NAME_3
# levels(districts2017$NAME_3)
# # [1] "A.S.A."      "C. COLORADO" "CAYMA"       "HUNTER"      "J. L. B Y R"
# # [6] "M. MELGAR"   "MIRAFLORES"  "PAUCARPATA"  "SACHACA"     "SOCABAYA"   
# # [11] "UCHUMAYO"    "YURA" 
# # 12 DISTRITOS
# 
# # create a safety copy
# districts2017tb <- districts2017
# class(districts2017tb) # df
# 
# ## Merge data
# tmp1 <- AQP_AQP_districts 
# tmp1_map <- tmp1
# tmp1_map@data <- join(AQP_AQP_districts@data, districts2017tb, by="NAME_3")
# tmp1_map_df <- tmp1_map@data
# tmp1_map@data$CASES[is.na(tmp1_map@data$CASES)]<-0 #
# tmp1_map@data$brks <- cut(tmp1_map@data$CASES, 
#                           breaks=c(-0.1, 0.9, 1.9, 5, 10, 20, 30, 35), 
#                           labels=c("0", "1", "2 - 5", "6 - 10", 
#                                    "11 - 20", "21 - 30", "> 30"))
# names(tmp1_map@data) # check variables
# tmp1_map_df <- tmp1_map@data
# 
# # Create the map qtm
# col <- c("ivory2", "lightcyan1", "lightblue2", #"lightskyblue", 
#          "royalblue", "blue2", "navyblue", "black")
# # try snow2 too
# qtm <- qtm(square_AQP, fill="white", bb=FALSE) + 
#   qtm(tmp1_map, fill = "brks", fill.palette = #"Blues"
#         col, fill.title="Incidencia") + 
#   tm_text("NAME_3", size="CASES", scale=.9,
#           #size="area",
#           root=3, 
#           size.lowerbound = .2, 
#           bg.color="white", bg.alpha = .15, 
#           #auto.placement = 1, 
#           legend.size.show = FALSE
#   ) +
#   tm_layout("Arequipa 2017", legend.title.size = 1.1, legend.text.size = .8,
#             #legend.outside = TRUE, 
#             legend.bg.color = "white")
# print(qtm)
# # save_tmap(qtm, paste("Casos_INS/Scripts/output/Rabia_AQP_20173.png", sep=""), 
# #           width=1920, height=1380)
# 
# 
# ### CREATE A SUMMARY WITH CASES IN 2016 ###
# data2016 <- data[data$nyear == 2016,]
# districts2016 <- as.data.frame(table(data2016$DISTRITO))
# # Changes names for them to match
# colnames(districts2016)[1] <- "NAME_3" # change names
# colnames(districts2016)[2] <- "CASES"
# districts2016$NAME_3
# levels(districts2016$NAME_3)
# # create a safety copy
# districts2016tb <- districts2016
# 
# ## Merge data
# tmp1 <- AQP_AQP_districts 
# tmp1_map <- tmp1
# tmp1_map@data <- join(AQP_AQP_districts@data, districts2016tb, by="NAME_3")
# tmp1_map_df <- tmp1_map@data
# tmp1_map@data$CASES[is.na(tmp1_map@data$CASES)]<-0 #
# tmp1_map@data$brks <- cut(tmp1_map@data$CASES, 
#                           breaks=c(-0.1, 0.9, 1.9, 5, 10, 20, 30, 35), 
#                           labels=c("0", "1", "2 - 5", "6 - 10", 
#                                    "11 - 20", "21 - 30", "> 30"))
# names(tmp1_map@data) # check variables
# tmp1_map_df <- tmp1_map@data
# 
# # Create the map qtm
# col <- c("ivory2", "lightcyan1", "lightblue2", #"lightskyblue", 
#          "royalblue", "blue2", "navyblue", "black")
# # try snow2 too
# qtm <- qtm(square_AQP, fill="white", bb=FALSE) + 
#   qtm(tmp1_map, fill = "brks", fill.palette = #"Blues"
#         col, fill.title="Incidencia") + 
#   tm_text("NAME_3", size="CASES", scale=.9,
#           #size="area",
#           root=3, 
#           size.lowerbound = .2, 
#           bg.color="white", bg.alpha = .15, 
#           #auto.placement = 1, 
#           legend.size.show = FALSE
#   ) +
#   tm_layout("Arequipa 2016", legend.title.size = 1.1, legend.text.size = .8,
#             #legend.outside = TRUE, 
#             legend.bg.color = "white")
# print(qtm)
# # save_tmap(qtm, paste("Casos_INS/Scripts/output/Rabia_AQP_20183.png", sep=""), 
# #           width=1920, height=1380)
# 
# 
# ##### 2018 #####
# data2018 <- data[data$nyear == 2018,]
# districts2018 <- as.data.frame(table(data2018$DISTRITO))
# # Changes names for them to match
# colnames(districts2018)[1] <- "NAME_3" # change names
# colnames(districts2018)[2] <- "CASES"
# districts2018$NAME_3
# levels(districts2018$NAME_3)
# 
# # create a safety copy
# districts2018tb <- districts2018
# 
# ## Merge data
# tmp1 <- AQP_AQP_districts 
# tmp1_map <- tmp1
# tmp1_map@data <- join(AQP_AQP_districts@data, districts2018tb, by="NAME_3")
# tmp1_map_df <- tmp1_map@data
# tmp1_map@data$CASES[is.na(tmp1_map@data$CASES)]<-0 #
# tmp1_map@data$brks <- cut(tmp1_map@data$CASES, 
#                       breaks=c(-0.1, 0.9, 1.9, 5, 10, 20, 30, 35), 
#                       labels=c("0", "1", "2 - 5", "6 - 10", 
#                                "11 - 20", "21 - 30", "> 30"))
# names(tmp1_map@data) # check variables
# tmp1_map_df <- tmp1_map@data
# 
# # Create the map qtm
# col <- c("ivory2", "lightcyan1", "lightblue2", #"lightskyblue", 
#          "royalblue", "blue2", "navyblue", "black")
# # try snow2 too
# qtm <- qtm(square_AQP, fill="white", bb=FALSE) + 
#   qtm(tmp1_map, fill = "brks", fill.palette = #"Blues"
#              col, fill.title="Incidencia") + 
#   tm_text("NAME_3", size="CASES", scale=.9,
#           #size="area",
#           root=3, 
#           size.lowerbound = .2, 
#           bg.color="white", bg.alpha = .15, 
#           #auto.placement = .8, 
#           legend.size.show = FALSE
#           ) +
#   tm_layout("Arequipa 2018", legend.title.size = 1.1, legend.text.size = .8,
#             #legend.outside = TRUE, 
#             legend.bg.color = "white")
# print(qtm)
# # save_tmap(qtm, paste("Casos_INS/Scripts/output/Rabia_AQP_20183.png", sep=""), 
# #           width=1920, height=1380)
# 
# # https://cran.r-project.org/web/packages/tmap/vignettes/tmap-nutshell.html


#### MAP PER YEARS #### 
year.i <- unique(sort(data$nyear))

for(i in year.i)
  {
  print(i)
  data.i <- data[data$nyear == i,]
  districts.i <- as.data.frame(table(data.i$DISTRITO))
  
  # Changes names for them to match
  colnames(districts.i)[1] <- "NAME_3" # change names
  colnames(districts.i)[2] <- "CASES"
  districts.i$NAME_3
  levels(districts.i$NAME_3)
  # create a safety copy
  districts.itb <- districts.i
  
  ## Merge data
  tmp1 <- AQP_AQP_districts 
  tmp1_map <- tmp1
  tmp1_map@data <- join(AQP_AQP_districts@data, districts.itb, by="NAME_3")
  tmp1_map_df <- tmp1_map@data
  tmp1_map@data$CASES[is.na(tmp1_map@data$CASES)]<-0 #
  tmp1_map@data$brks <- cut(tmp1_map@data$CASES, 
                            breaks=c(-0.1, 0.9, 1.9, 5, 10, 20, 30, 35), 
                            labels=c("0", "1", "2 - 5", "6 - 10", 
                                     "11 - 20", "21 - 30", "> 30"))
  names(tmp1_map@data) # check variables
  tmp1_map_df <- tmp1_map@data
  
  # Create the map qtm
  col <- c("ivory2", "lightcyan1", "lightblue2", #"lightskyblue", 
           "royalblue", "blue2", "navyblue", "black")
  # try snow2 too
  if(i > 2017){
    
  qtm <- qtm(square_AQP, fill="white", bb=FALSE) + 
    qtm(tmp1_map, fill = "brks", fill.palette = col, fill.title="Incidencia") + 
    tm_text("NAME_3", size="CASES", scale=.9, root=3, 
            size.lowerbound = .2, bg.color="white", bg.alpha = .15, 
            legend.size.show = FALSE) +
    tm_layout(paste("Arequipa", i, "- hasta Marzo"), legend.title.size = 1.1, 
              legend.text.size = .8, legend.bg.color = "white")
  print(qtm)
  save_tmap(qtm, paste("Casos_INS/Scripts/outputs/Rabia_AQP_", i,"_full1.png", sep=""), 
             width=1920, height=1380)
  
  } else {
    qtm <- qtm(square_AQP, fill="white", bb=FALSE) + 
      qtm(tmp1_map, fill = "brks", fill.palette = col, fill.title="Incidencia") + 
      tm_text("NAME_3", size="CASES", scale=.9, root=3, 
              size.lowerbound = .2, bg.color="white", bg.alpha = .15, 
              legend.size.show = FALSE) +
      tm_layout(paste("Arequipa", i), legend.title.size = 1.1, 
                legend.text.size = .8, legend.bg.color = "white")
    print(qtm)
    save_tmap(qtm, paste("Casos_INS/Scripts/outputs/Rabia_AQP_", i,"_full1.png", sep=""), 
              width=1920, height=1380)
  }}


#### MAP PER YEARS #### NAME ACCORDING TO SIZE
year.i <- unique(sort(data$nyear))

for(i in year.i)
{
  print(i)
  data.i <- data[data$nyear == i,]
  districts.i <- as.data.frame(table(data.i$DISTRITO))
  
  # Changes names for them to match
  colnames(districts.i)[1] <- "NAME_3" # change names
  colnames(districts.i)[2] <- "CASES"
  districts.i$NAME_3
  levels(districts.i$NAME_3)
  # create a safety copy
  districts.itb <- districts.i
  
  ## Merge data
  tmp1 <- AQP_AQP_districts 
  tmp1_map <- tmp1
  tmp1_map@data <- join(AQP_AQP_districts@data, districts.itb, by="NAME_3")
  tmp1_map_df <- tmp1_map@data
  tmp1_map@data$CASES[is.na(tmp1_map@data$CASES)]<-0 #
  tmp1_map@data$brks <- cut(tmp1_map@data$CASES, 
                            breaks=c(-0.1, 0.9, 1.9, 5, 10, 20, 30, 35), 
                            labels=c("0", "1", "2 - 5", "6 - 10", 
                                     "11 - 20", "21 - 30", "> 30"))
  names(tmp1_map@data) # check variables
  tmp1_map_df <- tmp1_map@data
  
  # Create the map qtm
  col <- c("ivory2", "lightcyan1", "lightblue2", #"lightskyblue", 
           "royalblue", "blue2", "navyblue", "black")
  # try snow2 too
  
  if(i > 2017){
    qtm <- qtm(square_AQP, fill="white", bb=FALSE) + 
      qtm(tmp1_map, fill = "brks", fill.palette = col, 
          fill.title="Casos") + 
      tm_text("NAME_3", size="area", scale=1, root=4, 
              size.lowerbound = .1, bg.color="white", bg.alpha = .15, 
              legend.size.show = FALSE) +
      tm_layout(paste("Rabia canina en Arequipa", i, "- hasta marzo"), 
                title.size=1,
                legend.title.size = 1.1, 
                legend.text.size = .8, legend.bg.color = "white")
    print(qtm)
    save_tmap(qtm, paste("Casos_INS/Scripts/outputs/Rabia_AQP_", i,"_full_allnames3.png", sep=""), 
              width=1920, height=1380)    
    
  } else {
    qtm <- qtm(square_AQP, fill="white", bb=FALSE) + 
      qtm(tmp1_map, fill = "brks", fill.palette = col, 
          fill.title="Casos") + 
      tm_text("NAME_3", size="area", scale=1, root=4, 
              size.lowerbound = .1, bg.color="white", bg.alpha = .15, 
              legend.size.show = FALSE) +
      tm_layout(paste("Rabia canina en Arequipa", i), title.size=.83,
                legend.title.size = 1.1, 
                legend.text.size = .8, legend.bg.color = "white")
    print(qtm)
    save_tmap(qtm, paste("Casos_INS/Scripts/outputs/Rabia_AQP_",
                         i,"_full_allnames3.png", sep=""), 
              width=1920, height=1380)
  }
}  




# PER MONTH, sequence timestep - simple
names(data)
month.i <- unique(sort(data$nmonth_s))
#  [1]  1  2  3  4  5  6  7  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26

for(i in month.i){
  print(i)
  data.i <- data[data$nmonth_s == i,]
  districts.i <- as.data.frame(table(data.i$DISTRITO)) # data
  
  # Changes names for them to match
  colnames(districts.i)[1] <- "NAME_3" # change names
  colnames(districts.i)[2] <- "CASES"
  districts.i$NAME_3
  levels(districts.i$NAME_3)
  # create a safety copy
  districts.itb <- districts.i
  
  ## Merge data
  tmp1 <- AQP_AQP_districts 
  tmp1_map <- tmp1
  tmp1_map@data <- join(AQP_AQP_districts@data, districts.itb, by="NAME_3") # shp with data
  tmp1_map_df <- tmp1_map@data
  tmp1_map@data$CASES[is.na(tmp1_map@data$CASES)]<-0 #
  tmp1_map@data$brks <- cut(tmp1_map@data$CASES, 
                            breaks=c(-0.1, 0.9, 1.9, 5, 10, 20, 30, 35), 
                            labels=c("0", "1", "2 - 5", "6 - 10", 
                                     "11 - 20", "21 - 30", "> 30"))
  names(tmp1_map@data) # check variables
  tmp1_map_df <- tmp1_map@data
  
  # Create the map qtm
  col <- c("ivory2", "lightcyan1", "lightblue2", #"lightskyblue", 
           "royalblue", "blue2", "navyblue", "black")
  # try snow2 too
  qtm <- qtm(square_AQP, fill="white", bb=FALSE) + 
    qtm(tmp1_map, fill = "brks", fill.palette = col, fill.title="Incidencia") + 
    tm_text("NAME_3", size="area", scale=1, root=4, 
            size.lowerbound = .1, bg.color="white", bg.alpha = .15, 
            legend.size.show = FALSE) +
    tm_layout(paste("Arequipa", month.i[i]), legend.title.size = 1.1, 
              legend.text.size = .8, legend.bg.color = "white")
  print(qtm)
  save_tmap(qtm, paste("Casos_INS/Scripts/outputs/Rabia_AQP_month_", i,".png", sep=""), 
            width=1920, height=1380)
}  




# not needed now
# library(rgeos)
# gArea(tmp1_map)
# gArea(tmp1_map, byid = TRUE)
# areas <- sapply(slot(tmp1_map, "polygons"), slot, "area") 
# areas.km <-areas*10000 # transform to km
# areas.km #
# tmp1_map@data$area <- round(areas.km, digits = 2)
# tmp1_map_df <- tmp1_map@data


# Try to increase line width
# # Try to ADD borders
# plot(AQP_prov, add=TRUE, lwd=2)
# plot(peru0, add=TRUE, lwd=3)


# not needed now


# not needed anymore
# # Recode names
# districts2017tb$NAME_3 <- recode(districts2017tb$NAME_3,  
#                                  `A.S.A.` = "Alto Selva Alegre", 
#                                  `CAYMA` = "Cayma", 
#                                  `C. COLORADO` = "Cerro Colorado", 
#                                  `HUNTER` = "Jacobo Hunter", 
#                                  `J. L. B Y R` = "Jose Luis Bustamante Y Rivero", 
#                                  `M. MELGAR` = "Mariano Melgar", 
#                                  `MIRAFLORES` = "Miraflores", 
#                                  `PAUCARPATA` = "Paucarpata",
#                                  `SACHACA` = "Sachaca", 
#                                  `SOCABAYA` = "Socabaya",
#                                  `UCHUMAYO` = "Uchumayo",
#                                  `YURA` = "Yura")
