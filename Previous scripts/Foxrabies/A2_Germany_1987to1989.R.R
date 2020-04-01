##'**LAURIE'S PROJECT**
##'**FOX RABIES DYNAMIC**
##'**MICAELA DE LA PUENTE L.**        
##'**This script add german data from 1987 to 1989 to the general dataset** 
##########################################################################

library(rgdal)
library(rgeos)

##########################################################################

# Call the germany dataset
germany <-readOGR("C:/Users/Micaela/Dropbox/Fox Rabies_MD/FLI/Data/Cases/German rabies dataset/Germany_1982_2014.shp",
                  "Germany_1982_2014")

names(germany)
#[1] "JAHR"     "LFD_NR"   "UA_SCHL"  "UA_NAME"  "GEB_SCHL" "GEB_NAME" "TA_SCHL" 
#[8] "TA_NAME"  "EG_SCHL"  "EG_NAME"  "ALTER"    "DATUM"    "OTC"      "IFT"     
#[15] "PCR"      "RFFIT"

# JAHR / year
# TA_NAME / species
# DATUM / fecha


head(germany, 3)
#  coordinates          JAHR  LFD_NR  UA_SCHL  UA_NAME     GEB_SCHL  GEB_NAME  TA_SCHL
#1 (8.785678, 51.36405) <NA>  0000073   99    <unbekannt>  06000000   Hessen   890
#2 (11.41794, 53.88943) 1982  1000001   01     Rostock     13074087   Wismar   331
#3 (11.88272, 54.02313) 1982  1000002   01     Rostock     13072093    Satow   313
#         TA_NAME       EG_SCHL EG_NAME ALTER   DATUM     OTC IFT   PCR   RFFIT
#1 nicht spezifiziert    <NA>    <NA>  <NA>       <NA>     <NA>   - <NA>  <NA>
#2              Fuchs    <NA>    <NA>  <NA>   1982/01/15   <NA>   + <NA>  <NA>
#3            Rehwild    <NA>    <NA>  <NA>   1982/01/15   <NA>   + <NA>  <NA>
  
table(germany$JAHR)
# 1900  1901  1982  1983  1984  1985  1986  1987  1988  1989  1990  1991  1992 
#3    17 19772 22742 22550 21313 20309 19744 24109 31370 30952 27123 26157 
#1993  1994  1995  1996  1997  1998  1999  2000  2001  2002  2003  2004  2005 
#24508 18648 23737 38983 36178 37568 38064 31390 31259 24450 24351 24322 22599 
#2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2200  2201  2203 
#14792 14858 16735 18949 23010 10219  8953  6864  4995    52     1     2     1 
#2204  2205  2207  2213 
#3     1     2     1 

# 1900 and 1901?
# Figure out what are column 2, 3, 5, 7, 9, 10, 11
# Figure out level of UA in column 4 and 6
# Figure out tests and levels of columns 13, 14, 15, 16


# Subset foxes only
germany_subset <- subset(germany, TA_NAME == "Fuchs")
sort(unique(germany_subset$JAHR))
#[1] 1900 1901 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997
#[19] 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015
#[37] 2200 2201 2203 2204 2205 2207
# 43 Levels: 1900 1901 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 ... 2213
# 1900, 1901, 2200, 2201, 2203, 2204, 2205, 2207!!!

# Select years from 1987 and 1989 only
germany_subset1 <- subset(germany_subset, JAHR %in%  1987:1989)
subset <- as.data.frame(germany_subset1)
str(germany_subset1@data) # 11399 obs. 

# For Micas project
rabies_CRS <- CRS("+proj=lcc +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
proj4string(germany_subset1) <- rabies_CRS
str(germany_subset1)

# Subset IFT positive only
str(germany_subset1$IFT)
#  Factor w/ 4 levels "-","?","+","U":
germany_subset2 <- subset(germany_subset1, IFT == "+")
germany_subset2$IFT <- factor(germany_subset2$IFT)
str(germany_subset2$IFT) #  Factor w/ 1 level "+":


# Tidy the column names
names(germany_subset2)
#[1] "JAHR"     "LFD_NR"   "UA_SCHL"  "UA_NAME"  "GEB_SCHL" "GEB_NAME" "TA_SCHL"  "TA_NAME" 
#[9] "EG_SCHL"  "EG_NAME"  "ALTER"    "DATUM"    "OTC"      "IFT"      "PCR"      "RFFIT" 

germany_add <- germany_subset2[,c(1,12,6,8)] # eliminate unnecessary columns
names(germany_add)
#[1] "JAHR"    "DATUM"   "GEB_NAME"  "TA_NAME"
  
colnames(germany_add@data)[1] <- "YEAR"
colnames(germany_add@data)[2] <- "QUARTER"
colnames(germany_add@data)[3] <- "REGION"
colnames(germany_add@data)[4] <- "SPECIES"

names(germany_add)
#[1] "YEAR"    "QUARTER" "REGION" "SPECIES"

# add number and country
len <- length(germany_add)
germany_add@data$NUMBER <- rep(1, len) # column with 0s for the 2006
germany_add@data$COUNTRY <- rep("Germany", len) # column with 0s for the 2006
names(germany_add)
germany_add@data <- germany_add@data[, c(1,2,6,3:5)]
names(germany_add)
# [1] "YEAR"    "QUARTER" "COUNTRY" "REGION"  "SPECIES" "NUMBER" 


## Split the date to keep the month only
germany_add1 <- germany_add
str(germany_add$QUARTER)
germany_add1$QUARTER <- as.character(germany_add1$QUARTER) # has to be chr for the split

list <- strsplit(germany_add1$QUARTER, "/")
library("plyr")
df <- ldply(list)
df <- df[, c(2)]
df1 <- as.data.frame(df)
str(df1)

## Replace the values of months and classify per quarter
library(car)
df2 <- recode(df1$df, 
              '"01" = "1"; "02" = "1"; "03" = "1"; "04" = "2"; "05" = "2"; "06" = "2"; "07" = "3"; "08" = "3"; "09" = "3"; "10" = "4"; "11" = "4"; "12" = "4"')
df2 <- as.data.frame(df2)

# Add the column to the germany data
germany_add@data$QUARTER <- df2$df2 
names(germany_add)
head(germany_add@data)
plot(germany_add)

# Save german data
writeOGR(germany_add, dsn=".", 
         layer="output/germany_cases", 
         driver="ESRI Shapefile")
