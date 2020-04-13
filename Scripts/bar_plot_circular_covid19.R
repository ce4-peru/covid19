# Libraries
library(tidyverse)
library(data.table)
library(plyr)
######### abriendo los datos ########
setwd("~/covid19/data/modificadas")
    df0 <- read.csv("covidPE_IND_20200411_CA_clean.csv")
table(df0$REGION)
#Format data frame
df0 <- df0[2:17]
colnames(df0) <- c("ID", "Date", "Day", "Month", "Year", "Region", "Provence", "Disctrict",  
                   "Direction", "Age1", "Age", "Sex", "Hospitilized", "AISLADODOMICILS1N0", 
                   "Contact","Imported") #KEY: Sex:M=1,F=0|Hospitilized:Yes=1,No=0|Imported:Yes=1,No=0)
#df0$Date <- as.Date(df0$Date, "%m/%d/%Y") #Fix date format
df0$Date <- as.Date(df0$Date) 

df0 <- df0[1:6]
df0$count<-unlist(1)

dfag<- aggregate(count ~ Region, data = df0, sum)
dfag$id=seq(1,25)
target <- c("LIMA")
dfag<-filter(dfag, !Region %in% target) 


# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data <- data
label_data<- dfag
# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
label_data$per<- label_data$count*100/sum(label_data$count)
options(digits=2)
        label_data$Region <- revalue(label_data$Region, 
                                    c("ANCASH"="ANC", 
                                      "AREQUIPA"="AQP",
                                      "AYACUCHO" ="AYA",
                                      "CAJAMARCA"="CAJ",
                                      "CALLAO"="CAL",
                                      "CUSCO" ="CUS",
                                      "HUANUCO" ="HCO",
                                      "ICA" ="ICA",
                                      "JUNIN" ="JUN",
                                      "LA LIBERTAD" ="LAL",
                                      "LAMBAYEQUE" ="LAM",
                                      "LIMA" ="LIM",
                                      "LORETO"="LOR",
                                      "MADRE DE DIOS"="MDD",
                                      "PASCO" ="PAS",
                                      "PIURA"="PIU",
                                      "SAN MARTIN"="SAM", 
                                      "TACNA" ="TAC",
                                      "TUMBES" ="TUM", 
                                      "AMAZONAS" ="AMA", 
                                      "MOQUEGUA"="MOQ", 
                                      "APURIMAC"="APU", 
                                      "HUANCAVELICA"="HUV", 
                                      "PUNO"="PUN", 
                                      "UCAYALI"="UCA"))
label_data$Region <- paste(label_data$Region, " (",label_data$count,")", sep="")

# ----- ------------------------------------------- ---- #
##### ploteando los barplots circulares 
p <- ggplot(dfag, aes(x=as.factor(id), y=label_data$count)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("green", 0.7)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,350) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=count+10, label=Region, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p







