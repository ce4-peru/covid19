##'**LAURIE'S PROJECT**
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'**MICAELA DE LA PUENTE L.**
##'**This script shows subset of rabies data**

######################################################
# FIRST SET WORKING DIRECTORY IN RSTUDIO
# i.e. Session > Set Working Directory > To Source File Location
rm(list=ls())

######################################################
library(foreign)
library(maptools)
library(sp)
library(rgdal)
require(rgeos)
######################################################

### FOLDER: DATA/ADMINISTRATIVE EUROPE

### Check shapefile of Administrative Units level
nuts_l0 <-readOGR("output/nuts_l0_subset.shp", 
                             "nuts_l0_subset") 
length(levels(unique(nuts_l0@data$CODE))) # 54

# Subset Germanym Czech Rep and Poland
nuts_l0_subset <- subset(nuts_l0, CODE %in% c("DE", "CZ", "PL"))
nuts_l0_subset$CODE <- factor(nuts_l0_subset$CODE)
str(nuts_l0_subset@data) # $ CODE: Factor w/ 3 levels "CZ","DE","PL": 1 2 3

png("output/Subset_Level0_nuts.png", width=5, height=5, units="in", res=300)
plot(nuts_l0_subset, main="Level0 in Germany, Poland and Czech Republic")
dev.off()



## Level 1

nuts_l1 <-readOGR("output/nuts_l1_subset.shp", 
                  "nuts_l1_subset") 
length(levels(unique(nuts_l1@data$ID))) # 130
levels(unique(nuts_l1@data$ID))
# Germany: "DE1" "DE2" "DE3" "DE4" "DE5" "DE6" "DE7" "DE8" "DE9" "DEA" "DEB" "DEC" "DED"
#         "DEE" "DEF" "DEG"
# Czech Republic: "CZ0"
# Poland:  "PL1" "PL2" "PL3" "PL4" "PL5" "PL6"

# Subset
nuts_l1_subset <- subset(nuts_l1, ID %in% c("DE1", "DE2", "DE3", "DE4", "DE5", "DE6",
                                            "DE7", "DE8", "DE9", "DEA", "DEB", "DEC",
                                            "DED", "DEE", "DEF", "DEG", "CZ0", "PL1", 
                                            "PL2", "PL3", "PL4", "PL5", "PL6"))
nuts_l1_subset$ID <- factor(nuts_l1_subset$ID)
str(nuts_l1_subset@data) # $ ID: Factor w/ 23 levels

png("output/Subset_Level1_nuts.png", width=5, height=5, units="in", res=300)
plot(nuts_l1_subset, main="Level1 in Germany, Poland and Czech Republic")
dev.off()



## Level 2

nuts_l2 <-readOGR("output/nuts_l2_subset.shp", 
                  "nuts_l2_subset") 
length(levels(unique(nuts_l2@data$ID))) # 322
levels(unique(nuts_l2@data$ID))
# Germany: "DE11" "DE12" "DE13" "DE14" "DE21" "DE22" "DE23" "DE24" "DE25" "DE26" "DE27"
# "DE30" "DE41" "DE42" "DE50" "DE60" "DE71" "DE72" "DE73" "DE80" "DE91" "DE92" "DE93"
# "DE94" "DEA1" "DEA2" "DEA3" "DEA4" "DEA5" "DEB1" "DEB2" "DEB3" "DEC0" "DED1" "DED2"
# "DED3" "DEE1" "DEE2" "DEE3" "DEF0" "DEG0"
# Czech Republic: "CZ01" "CZ02" "CZ03" "CZ04" "CZ05" "CZ06" "CZ07" "CZ08"
# Poland:  "PL11" "PL12" "PL21" "PL22" "PL31" "PL32" "PL33" "PL34" "PL41" "PL42" "PL43" 
# "PL51" "PL52" "PL61" "PL62" "PL63"

# Subset
nuts_l2_subset <- subset(nuts_l2, ID %in% c("DE11", "DE12", "DE13", "DE14", "DE21", "DE22",
                                            "DE23", "DE24", "DE25", "DE26", "DE27", "DE30",
                                            "DE41", "DE42", "DE50", "DE60", "DE71", "DE72",
                                            "DE73", "DE80", "DE91", "DE92", "DE93", "DE94",
                                            "DEA1", "DEA2", "DEA3", "DEA4", "DEA5", "DEB1",
                                            "DEB2", "DEB3", "DEC0", "DED1", "DED2", 
                                            "DED3", "DEE1", "DEE2", "DEE3", "DEF0", "DEG0",
                                            "CZ01", "CZ02", "CZ03", "CZ04", "CZ05", "CZ06", 
                                            "CZ07", "CZ08", "PL11", "PL12", "PL21", "PL22",
                                            "PL31", "PL32", "PL33", "PL34", "PL41", "PL42",
                                            "PL43", "PL51", "PL52", "PL61", "PL62", "PL63"))
nuts_l2_subset$ID <- factor(nuts_l2_subset$ID)
str(nuts_l2_subset@data) # $ ID: Factor w/ 65 levels

png("output/Subset_Level2_nuts.png", width=5, height=5, units="in", res=300)
plot(nuts_l2_subset, main="Level2 in Germany, Poland and Czech Republic")
dev.off()



## Level 3

nuts_l3 <-readOGR("output/nuts_l3_subset.shp", 
                  "nuts_l3_subset") 
length(levels(unique(nuts_l3@data$CODE))) # 53
levels(unique(nuts_l3@data$CODE))
# Germany: "DE" 
# Czech Republic: "CZ"
# Poland:  "PL"

# Subset
nuts_l3_subset <- subset(nuts_l3, CODE %in% c("DE", "CZ", "PL"))
nuts_l3_subset$CODE <- factor(nuts_l3_subset$CODE)
str(nuts_l3_subset@data) # $ ID: Factor w/ 65 levels

png("output/Subset_Level3_nuts.png", width=5, height=5, units="in", res=300)
plot(nuts_l3_subset, main="Level3 in Germany, Poland and Czech Republic")
dev.off()



# Save all subsets
writeOGR(nuts_l0_subset, dsn=".", layer="output/nuts_l0_subset_three", 
         driver="ESRI Shapefile")
writeOGR(nuts_l1_subset, dsn=".", layer="output/nuts_l1_subset_three", 
         driver="ESRI Shapefile")
writeOGR(nuts_l2_subset, dsn=".", layer="output/nuts_l2_subset_three", 
         driver="ESRI Shapefile")
writeOGR(nuts_l3_subset, dsn=".", layer="output/nuts_l3_subset_three", 
         driver="ESRI Shapefile")


#### JUST GERMANY ####


# Add nuts3 shp
nuts_l3 <-readOGR("output/nuts_l3_subset.shp", "nuts_l3_subset") 
names(nuts_l3) # [1] "ID"         "Name"       "Name_ASCII" "CODE"  
head(nuts_l3@data)
levels(unique(nuts_l3@data$CODE)) # Germany: "DE" 

subset <- nuts_l3[which(nuts_l3$CODE == "DE"),]
str(subset@data)
subset@data$CODE <- factor(subset@data$CODE)
subset@data$Name <- factor(subset@data$Name)
subset@data$Name_ASCII <- factor(subset@data$Name_ASCII)
subset@data$ID <- factor(subset@data$ID)
str(subset@data)
#$ ID        : Factor w/ 429 levels "01001","01002",..: 1 2 3 4 5 6 7 8 9 10 ...
#$ Name      : Factor w/ 429 levels "LK Aachen","LK Ahrweiler",..: 347 373 376 392 59 110 181 201 209 210 ...
#$ Name_ASCII: Factor w/ 429 levels "LK Aachen","LK Ahrweiler",..: 346 372 381 391 57 109 180 201 209 210 ...
#$ CODE      : Factor w/ 1 level "DE": 1 1 1 1 1 1 1 1 1 1 ...
str(subset) # projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Save it
#writeOGR(subset, dsn=".", layer="output/nuts3_germany", 
#         driver="ESRI Shapefile", overwrite_layer = TRUE)
