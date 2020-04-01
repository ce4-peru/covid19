# This script
setwd("C:/Users/Micaela/Documents/Rabies")

## Data INS
data <- read.csv("Casos_INS/Scripts/data/Base_perrospositivos_2015-2018_20Feb18.csv")
names(data)
data.paper <- data[c(1:36), ]
unique(data.paper$CODIGO)
# [1]  53  73  92 101 103 114 118 166 367 496 507 521 560 609 653 696 699 704 711  10  22  32  36
# [24]  43  44  69  72  80  87  93 108 130 150 153 160

## Coordinates
coordxy <- read.csv("rabia/bd/Casos_Rabia_2015_16 I.csv")
names(coordxy)
unique(coordxy$ident)
# [1]  53  73  92 101 103 114 118 166 367 496 507 521 560 609 653 696 699 704 711  10  22  32  36
# [24]  43  44  69  72  80  87  93 108 130 150 153 160
# Same codes
data.coordxy <-  coordxy[, c(4:5)]

# Merge
final.df <- cbind(data.paper, data.coordxy)
write.csv(final.df, "Casos_INS/Scripts/outputs/casos_papertorrenteras_2coord.csv", 
          row.names = FALSE)
