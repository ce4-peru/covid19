
##'**LAURIE'S PROJECT**
##'**FOX RABIES DYNAMIC**
##'**MICAELA DE LA PUENTE L.**        
##'**This script creates a csv of german data by month using nuts 3 shapefile** 
##########################################################################
library(rgdal)
library(ggplot2)
library(plyr)
library(rgeos)
library(maptools)
library(raster)
library(dplyr)
library(rgdal)
library(car)
library(tidyr) # 
library(mefa) # this is for rep() function
library(Matrix)
require(reshape2)
library(foreign)
library(sp)
library(GISTools)
##########################################################################

### CALL THE DATASETS
germany <-readOGR("Cases/German rabies dataset/Germany_1982_2014.shp",
                  "Germany_1982_2014")
#ger_one <- readOGR("Administrative Shape Database (Rabigramm)/Europe_Nuts1_WGS84.shp",
#                   "Europe_Nuts1_WGS84") 

tdone <- tempdir() # save the directory
germany1 <- getData('GADM', country=c('DEU'), level=1, path=tdone)
ger_one <- germany1

#####################################################################################

# Make projections equal
str(germany) # NOT OK
str(ger_one) # ok

CRS <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(germany) <- CRS
proj4string(ger_one) <- CRS
str(germany)
str(ger_one)


### PREPARE GERMAN AND EUROPEAN DATASET TO JOIN THEM ###

### GERMAN DATASET ###

# Call the germany dataset

names(germany)
#[1] "JAHR"     "LFD_NR"   "UA_SCHL"  "UA_NAME"  "GEB_SCHL" "GEB_NAME" "TA_SCHL" 
#[8] "TA_NAME"  "EG_SCHL"  "EG_NAME"  "ALTER"    "DATUM"    "OTC"      "IFT"     
#[15] "PCR"      "RFFIT"

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

length(sort(unique(germany$GEB_NAME), decreasing = FALSE))
# [1] 9480

# Subset foxes Germany data only
germany_subset <- subset(germany, TA_NAME == "Fuchs")
sort(unique(germany_subset$JAHR))
#[1] 1900 1901 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997
#[19] 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015
#[37] 2200 2201 2203 2204 2205 2207
# 43 Levels: 1900 1901 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 ... 2213
# 1900, 1901, 2200, 2201, 2203, 2204, 2205, 2207!!!
table(germany_subset$JAHR)

## Split the date to keep the month only
germany_subset$DATUM <- as.character(germany_subset$DATUM) 
# has to be chr for the split
df <- substr(germany_subset$DATUM, 6, 7)
df1 <- as.data.frame(df)
str(df1) # 'data.frame':	477011 obs. of  1 variable:
# df: Factor w/ 12 levels

## Add the column to the germany data
germany_subset@data$MONTH <- df1$df
names(germany_subset) # month appear at the end
head(germany_subset@data)
plot(germany_subset)


###### try pos (lauries code)
g_rabies <- germany_subset
g_rabies$date<-as.POSIXct(g_rabies$DATUM,"%Y-%m-%d",tz="gmt") 
##getting the right date format


## Choose columns of interest
germany_subset1 <- germany_subset[,c(1,17)]
names(germany_subset1)
# [1] "JAHR"     "MONTH"



#### ADD FEDERAL STATE TO DATAFRAMES

names(germany_subset1) # [1] "JAHR"  "MONTH"
length(germany_subset1) # 477011

# Call DEU1
names(ger_one)
# [1] "OBJECTID"  "ID_0"      "ISO"       "NAME_0"    "ID_1"      "NAME_1"    "HASC_1"   
# [8] "CCN_1"     "CCA_1"     "TYPE_1"    "ENGTYPE_1" "NL_NAME_1" "VARNAME_1"
head(ger_one) # NAME_1 IS THE ONE WE NEED

# with nuts 1
# [1] "ID"         "NAME"       "ASCII_NAME"
#ger_one_subset <- subset(ger_one, ID %in% c("DE1", "DE2", "DE3", "DE4", "DE5", "DE6",
#                                            "DE7", "DE8", "DE9", "DEA", "DEB", "DEC",
#                                            "DED", "DEE", "DEF", "DEG"))
##ger_one_subset$ID <- factor(ger_one_subset$ID)
#ger_one_subset$NAME <- factor(ger_one_subset$NAME)
#ger_one_subset$ASCII_NAME <- factor(ger_one_subset$ASCII_NAME)
#str(ger_one_subset@data) # Factor w/ 16 levels


ger_one_subset <- ger_one # only for gadm1
plot(ger_one_subset)

#print(ger_one_subset$NAME) # nuts1
# [1] Baden-Werttemberg      Bayern                 Berlin                
# [4] Brandenburg            Bremen                 Hamburg               
# [7] Hessen                 Mecklenburg-Vorpommern Niedersachsen         
# [10] Nordrhein-Westfalen    Rheinland-Pfalz        Saarland              
# [13] Sachsen                Sachsen-Anhalt         Schleswig-Holstein    
# [16] ThGringen 

print(ger_one_subset$NAME_1) #  gadm1
# [1] "Baden-Württemberg"      "Bayern"                 "Berlin"                
# [4] "Brandenburg"            "Bremen"                 "Hamburg"               
# [7] "Hessen"                 "Mecklenburg-Vorpommern" "Niedersachsen"         
# [10] "Nordrhein-Westfalen"    "Rheinland-Pfalz"        "Saarland"              
# [13] "Sachsen-Anhalt"         "Sachsen"                "Schleswig-Holstein"    
# [16] "Thüringen"      


plot(germany_subset1, add=TRUE, col="red", pch=21, cex=0.2)
plot(ger_one_subset, add=TRUE)
# All Regions with no cases in the german dataset, but we dont know about years

# Overlay, as done before in QGIS
o <- over(germany_subset1, ger_one_subset)
length(germany_subset1) #  477011
length(o$NAME) #  477011
germany_subset2 <- germany_subset1
germany_subset2@data <- cbind(germany_subset1@data,o$NAME)
names(germany_subset2) # [1] "JAHR"  "MONTH" "o$NAME"
colnames(germany_subset2@data)[1] <- "YEAR"
colnames(germany_subset2@data)[3] <- "REGION"

# Create a new column with year and month together
germany_subset2@data$ID <- with(germany_subset2@data, paste0(YEAR, "M", MONTH))
names(germany_subset2) # [1] "YEAR"   "MONTH"  "REGION" "ID"

dim(germany_subset2) # [1] 477011      4
germany_subset3 <- na.exclude(germany_subset2)
dim(germany_subset3) # no NAs

# Create the compiled dataframe, per months
# German cases from 1982 to 2007
ger_cases_monthly <- as.data.frame(table(germany_subset3$REGION,
                                         germany_subset3$YEAR,
                                         germany_subset3$MONTH))
head(ger_cases_monthly) # [1] "Var1" "Var2" "Var3" "Freq"

# Correct column names
colnames(ger_cases_monthly)[1] <- "REGION"
colnames(ger_cases_monthly)[2] <- "YEAR"
colnames(ger_cases_monthly)[3] <- "MONTH"
colnames(ger_cases_monthly)[4] <- "COUNT"

# Create a new column with year and month together
ger_cases_monthly$ID <- with(ger_cases_monthly, paste0(YEAR, "M", MONTH))
names(ger_cases_monthly) # [1] "REGION" "YEAR"   "MONTH"  "COUNT"  "ID"  
# Tidy the dataset
ger_cases_monthly <- ger_cases_monthly[,c(1,5,2:4)]



### CREATE A MONTHLY DATA FRAME WITH ALL UNITS IN GERMANY ###

# Monthly cases shapefile
# Add quarters
ger_cases_monthly$QUARTER <- ger_cases_monthly$MONTH 
library(dplyr) # upload again.
ger_cases_monthly$QUARTER <- recode(ger_cases_monthly$QUARTER, `01` = "1", `02` = "1", 
                                `03` = "1", 
                                `04` = "2", `05` = "2", `06` = "2", `07` = "3",
                                `08` = "3", `09` = "3", `10` = "4", `11` = "4", 
                                `12` = "4")

str(ger_cases_monthly)
ger_cases_monthly <- ger_cases_monthly[,c(1,3,6,4,2,5)]
head(ger_cases_monthly)
ger_cases_monthly$MONTH <- as.numeric(ger_cases_monthly$MONTH)
ger_cases_monthly$QUARTER <- as.numeric(ger_cases_monthly$QUARTER)
ger_cases_monthly$YEAR <- as.numeric(as.character(ger_cases_monthly$YEAR))
str(ger_cases_monthly)
#'data.frame':	8256 obs. of  6 variables:
#  $ REGION : Factor w/ 16 levels "Baden-Werttemberg",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ YEAR   : num  1900 1900 1900 1900 1900 1900 1900 1900 1900 1900 ...
#$ QUARTER: num  1 1 1 2 2 2 3 3 3 4 ...
#$ MONTH  : num  1 2 3 4 5 6 7 8 9 10 ...
#$ ID     : chr  "1900M01" "1900M02" "1900M03" "1900M04" ...
#$ COUNT  : int  0 0 0 0 0 0 0 0 0 0 ...


## SELECT ONLY YEARS THAT MAKES SENSE #
print(sort(unique(ger_cases_monthly$YEAR), decreasing = FALSE))
# [1] 1900 1901 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997
# [19] 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015
# [37] 2200 2201 2203 2204 2205 2207 2213
# Delete 1900, 1901, 2200, 2201, 2203, 2204, 2205, 2207 and 2213 

ger_cases_monthly1 <- ger_cases_monthly[which(ger_cases_monthly$YEAR %in% 1982:2015),]
str(ger_cases_monthly1)
# 'data.frame':	6528 obs. of  6 variables:
# $ REGION : Factor w/ 16 levels "Baden-Werttemberg",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ YEAR   : num  1982 1982 1982 1982 1982 ...
# $ QUARTER: num  1 1 1 1 1 1 1 1 1 1 ...
# $ MONTH  : num  1 1 1 1 1 1 1 1 1 1 ...
# $ ID     : chr  "1982M01" "1982M01" "1982M01" "1982M01" ...
# $ COUNT  : int  0 0 0 54 0 0 0 48 0 0 ...



# This one is empty for COUNT column ##
# Call boundaries shapefile
library(mefa)
str(ger_one_subset@data) # 16 obs. of  3 variables:
names(ger_one_subset)
# [1] "ID"         "NAME"       "ASCII_NAME"
unit.i <- sort(unique(ger_one_subset$NAME), decreasing = FALSE) # 429

### Create an empty dataframe
final_dataframe <- as.data.frame(unit.i)
colnames(final_dataframe)[1] <- "REGION"
nunits <- length(final_dataframe$REGION) # [1] 429

# Expand the data set
nyears<- length(1982:2015) # 34
nmonths<- length(1:12) # 12
nrow_perunit <- nyears*nmonths # 408
nrows <- nrow_perunit*nunits # [1]  6528
final_dataframe1 <- rep(final_dataframe, 408) # mefa package

# Add years
years <- as.data.frame(c(1982:2015))
colnames(years)[1] <- "YEAR"
years1 <- as.data.frame(years[rep(seq_len(nrow(years)), each=nunits*nmonths),])
colnames(years1)[1] <- "YEAR"
final_dataframe1$YEAR <- years1$YEAR

# Add quarters
quarters <- as.data.frame(c(1:4))
colnames(quarters)[1] <- "QUARTER"
quarters1 <- as.data.frame(quarters[rep(seq_len(nrow(quarters)), each=nunits*3),])
# Because three times have to be the same quarter.
colnames(quarters1)[1] <- "QUARTER"
quarters2 <- rep(quarters1, nyears) 
final_dataframe1$QUARTER <- quarters2$QUARTER

# Add months
months <- as.data.frame(c(1:12))
colnames(months)[1] <- "MONTH"
months1 <- as.data.frame(months[rep(seq_len(nrow(months)), each=nunits),])
months2 <- rep(months1, nyears) 
colnames(months2)[1] <- "MONTH"
final_dataframe1$MONTH <- months2$MONTH
str(final_dataframe1$MONTH)
final_dataframe1$MONTH <- as.factor(final_dataframe1$MONTH)
#final_dataframe1$MONTH <- recode(final_dataframe1$MONTH, "'1'='01'; '2'='02'; 
#                         '3'='03'; '4'='04';'5'='05';
#                       '6'='06';'7'='07'; '8'='08'; '9'='09'")
final_dataframe1$MONTH <- recode(final_dataframe1$MONTH, `1` = "01", `2` = "02", 
                                 `3` = "03", 
                                 `4` = "04", `5` = "05", `6` = "06", `7` = "07",
                                 `8` = "08", `9` = "09")

#Clean dataset
final_dataframe1$ID <- with(final_dataframe1, paste0(YEAR, "M", MONTH)) 
# create ID for year plus month.
str(final_dataframe1)
final_dataframe1$YEAR <- as.numeric(final_dataframe1$YEAR)
final_dataframe1$QUARTER <- as.numeric(final_dataframe1$QUARTER)
final_dataframe1$MONTH <- as.numeric(final_dataframe1$MONTH)

# Add am empty column for count
final_dataframe1$COUNT <- NA
str(final_dataframe1) # OK now
# 'data.frame':	6528 obs. of  6 variables:
# $ REGION : Factor w/ 16 levels "Baden-Werttemberg",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ YEAR   : num  1982 1982 1982 1982 1982 ...
# $ QUARTER: num  1 1 1 1 1 1 1 1 1 1 ...
# $ MONTH  : num  1 1 1 1 1 1 1 1 1 1 ...
# $ ID     : chr  "1982M01" "1982M01" "1982M01" "1982M01" ...
# $ COUNT  : logi  NA NA NA NA NA NA ...



##### Merge this empty dataframe with the cases dataframe

str(ger_cases_monthly1) # 6528 obs. of  6 variables:
tmp1 <- merge(final_dataframe1, ger_cases_monthly1, by = c("REGION", "ID"), all.x = TRUE, 
              all.y = TRUE)
tmp1 <- tmp1[,c(1:5,10)]
names(tmp1) # [1] "ADM_UNIT"  "ID"        "YEAR.x"    "QUARTER.x" "MONTH.x"   "COUNT.y" 
tmp1$COUNT.y[is.na(tmp1$COUNT.y)]<-0
colnames(tmp1)[3] <- "YEAR"
colnames(tmp1)[4] <- "QUARTER"
colnames(tmp1)[5] <- "MONTH"
colnames(tmp1)[6] <- "COUNT"
head(tmp1)

# Write it
write.csv(tmp1, file = "output/full_federalstates_posandneg.csv")


# SUBSET federal stated required
print(sort(unique(tmp1$REGION), decreasing = FALSE))
# [1] Baden-Werttemberg      Bayern                 Berlin                 Brandenburg           
# [5] Bremen                 Hamburg                Hessen                 Mecklenburg-Vorpommern
# [9] Niedersachsen          Nordrhein-Westfalen    Rheinland-Pfalz        Saarland              
# [13] Sachsen                Sachsen-Anhalt         Schleswig-Holstein     ThGringen  

# Only "Brandenburg", "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thüringen"
tmp2 <- tmp1[which(tmp1$REGION %in% c("Brandenburg", "Mecklenburg-Vorpommern",
                                      "Sachsen", "Sachsen-Anhalt", "ThGringen")),]
str(tmp2) # 'data.frame':	2040 obs. of  6 variables:
write.csv(tmp2, file = "output/full_federalstates_posandneg_subset.csv")










############# SAME PROCEDURE WITHOUT STRECKE ########

# Upload datasets again and arrange projections

### GERMAN DATASET ###

names(germany)
#[1] "JAHR"     "LFD_NR"   "UA_SCHL"  "UA_NAME"  "GEB_SCHL" "GEB_NAME" "TA_SCHL" 
#[8] "TA_NAME"  "EG_SCHL"  "EG_NAME"  "ALTER"    "DATUM"    "OTC"      "IFT"     
#[15] "PCR"      "RFFIT"

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

length(sort(unique(germany$GEB_NAME), decreasing = FALSE))
# [1] 9480

# Subset foxes Germany data only
germany_subset <- subset(germany, TA_NAME == "Fuchs")
sort(unique(germany_subset$JAHR))
#[1] 1900 1901 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997
#[19] 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015
#[37] 2200 2201 2203 2204 2205 2207
# 43 Levels: 1900 1901 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 ... 2213
# 1900, 1901, 2200, 2201, 2203, 2204, 2205, 2207!!!
table(germany_subset$JAHR)


# Subset positive and negative not strecke only
names(germany_subset)

str(germany_subset$IFT)
#  Factor w/ 4 levels "-","?","+","U":

str(germany_subset$EG_NAME) 
# Factor w/ 7 levels "<keine Angaben>",..: NA NA NA NA NA NA NA NA NA NA ...
sort(unique(germany_subset$EG_NAME))
# [1] Fallwild      Personenkontakt       Strecke    Tollwutverdacht    Unfall         
# [6] Unfall mit PK 

str(germany_subset@data) # 477011 obs. of  16 variables:
germany_subset1 <- germany_subset[-which(germany_subset$IFT=="-" &
                                           germany_subset$EG_NAME == "Strecke"),]
str(germany_subset1@data) # 176388 obs. of  16 variables:
germany_subset1$EG_NAME <- factor(germany_subset1$EG_NAME)
germany_subset1$IFT <- factor(germany_subset1$IFT)
str(germany_subset1@data) 

## Split the date to keep the month only
germany_subset1$DATUM <- as.character(germany_subset1$DATUM) 
# has to be chr for the split
df <- substr(germany_subset1$DATUM, 6, 7)
df1 <- as.data.frame(df)
str(df1) # 'data.frame':	176388 obs. of  1 variable:
# df: Factor w/ 12 levels

## Add the column to the germany data
germany_subset1@data$MONTH <- df1$df
names(germany_subset1) # month appear at the end
head(germany_subset1@data)
plot(germany_subset1)

## Choose columns of interest
germany_subset1 <- germany_subset1[,c(1,17)]
names(germany_subset1)
# [1] "JAHR"     "MONTH"



# Call DEU1
names(ger_one)
# [1] "ID"         "NAME"       "ASCII_NAME"
ger_one_subset <- subset(ger_one, ID %in% c("DE1", "DE2", "DE3", "DE4", "DE5", "DE6",
                                            "DE7", "DE8", "DE9", "DEA", "DEB", "DEC",
                                            "DED", "DEE", "DEF", "DEG"))
ger_one_subset$ID <- factor(ger_one_subset$ID)
ger_one_subset$NAME <- factor(ger_one_subset$NAME)
ger_one_subset$ASCII_NAME <- factor(ger_one_subset$ASCII_NAME)
str(ger_one_subset@data) # Factor w/ 16 levels

plot(ger_one_subset)
print(ger_one_subset$NAME)
# [1] Baden-Werttemberg      Bayern                 Berlin                
# [4] Brandenburg            Bremen                 Hamburg               
# [7] Hessen                 Mecklenburg-Vorpommern Niedersachsen         
# [10] Nordrhein-Westfalen    Rheinland-Pfalz        Saarland              
# [13] Sachsen                Sachsen-Anhalt         Schleswig-Holstein    
# [16] ThGringen 
plot(germany_subset1, add=TRUE, col="red", pch=21, cex=0.2)
plot(ger_one_subset, add=TRUE)

# All Regions with no cases in the german dataset, but we dont know about years


#### ADD FEDERAL STATE TO DATAFRAMES

### To German dataset only ###
names(germany_subset1) # [1] "JAHR"  "MONTH"
length(germany_subset1) # 176388

# Overlay, as done before in QGIS
o <- over(germany_subset1, ger_one_subset)
length(germany_subset1) #  176388
length(o$NAME) #  176388
germany_subset2 <- germany_subset1
germany_subset2@data <- cbind(germany_subset1@data,o$NAME)
names(germany_subset2) # [1] "JAHR"  "MONTH" "o$NAME"
colnames(germany_subset2@data)[1] <- "YEAR"
colnames(germany_subset2@data)[3] <- "REGION"

# Create a new column with year and month together
germany_subset2@data$ID <- with(germany_subset2@data, paste0(YEAR, "M", MONTH))
names(germany_subset2) # [1] "YEAR"   "MONTH"  "REGION" "ID"

dim(germany_subset2) # [1]  176388      4
germany_subset3 <- na.exclude(germany_subset2)
dim(germany_subset3) # no NAs

# Create the compiled dataframe, per months
# German cases from 1982 to 2007
ger_cases_monthly <- as.data.frame(table(germany_subset3$REGION,
                                         germany_subset3$YEAR,
                                         germany_subset3$MONTH))
head(ger_cases_monthly) # [1] "Var1" "Var2" "Var3" "Freq"

# Correct column names
colnames(ger_cases_monthly)[1] <- "REGION"
colnames(ger_cases_monthly)[2] <- "YEAR"
colnames(ger_cases_monthly)[3] <- "MONTH"
colnames(ger_cases_monthly)[4] <- "COUNT"

# Create a new column with year and month together
ger_cases_monthly$ID <- with(ger_cases_monthly, paste0(YEAR, "M", MONTH))
names(ger_cases_monthly) # [1] "REGION" "YEAR"   "MONTH"  "COUNT"  "ID"  
# Tidy the dataset
ger_cases_monthly <- ger_cases_monthly[,c(1,5,2:4)]


### CREATE A MONTHLY DATA FRAME WITH ALL UNITS IN GERMANY ###
# Monthly cases shapefile
str(ger_cases_monthly)
# 'data.frame':	8256 obs. of  5 variables:
#   $ REGION: Factor w/ 16 levels "Baden-Werttemberg",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ ID    : chr  "1900M01" "1900M01" "1900M01" "1900M01" ...
# $ YEAR  : Factor w/ 43 levels "1900","1901",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ MONTH : Factor w/ 12 levels "01","02","03",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ COUNT : int  0 0 1 0 0 0 0 0 0 0 ...

# Add quarters
ger_cases_monthly$QUARTER <- ger_cases_monthly$MONTH 
library(dplyr) # upload again.
ger_cases_monthly$QUARTER <- recode(ger_cases_monthly$QUARTER, `01` = "1", `02` = "1", 
                                `03` = "1", 
                                `04` = "2", `05` = "2", `06` = "2", `07` = "3",
                                `08` = "3", `09` = "3", `10` = "4", `11` = "4", 
                                `12` = "4")

str(ger_cases_monthly)
ger_cases_monthly <- ger_cases_monthly[,c(1,3,6,4,2,5)]
ger_cases_monthly$MONTH <- as.numeric(ger_cases_monthly$MONTH)
ger_cases_monthly$QUARTER <- as.numeric(ger_cases_monthly$QUARTER)
ger_cases_monthly$YEAR <- as.numeric(as.character(ger_cases_monthly$YEAR))
str(ger_cases_monthly)
#'data.frame':	8256 obs. of  6 variables:
#$ REGION : Factor w/ 16 levels "Baden-Werttemberg",..: 1 2 3 4 5 6 7 8 9 10 ...
##$ YEAR   : num  1900 1900 1900 1900 1900 1900 1900 1900 1900 1900 ...
#$ QUARTER: num  1 1 1 1 1 1 1 1 1 1 ...
#$ MONTH  : num  1 1 1 1 1 1 1 1 1 1 ...
#$ ID     : chr  "1900M01" "1900M01" "1900M01" "1900M01" ...
#$ COUNT  : int  0 0 1 0 0 0 0 0 0 0 ...


## SELECT ONLY YEARS THAT MAKES SENSE #
print(sort(unique(ger_cases_monthly$YEAR), decreasing = FALSE))
# [1] 1900 1901 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997
# [19] 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015
# [37] 2200 2201 2203 2204 2205 2207 2213
# Delete 1900, 1901, 2200, 2201, 2203, 2204, 2205, 2207 and 2213 

ger_cases_monthly1 <- ger_cases_monthly[which(ger_cases_monthly$YEAR %in% 1982:2015),]
str(ger_cases_monthly1)
# 'data.frame':	6528 obs. of  6 variables:
# $ REGION : Factor w/ 16 levels "Baden-Werttemberg",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ YEAR   : num  1982 1982 1982 1982 1982 ...
# $ QUARTER: num  1 1 1 1 1 1 1 1 1 1 ...
# $ MONTH  : num  1 1 1 1 1 1 1 1 1 1 ...
# $ ID     : chr  "1982M01" "1982M01" "1982M01" "1982M01" ...
# $ COUNT  : int  0 0 0 54 0 0 0 48 0 0 ...



### CREATE A MONTHLY DATA FRAME WITH ALL UNITS IN GERMANY ###
# This one is empty for COUNT column
library(mefa)

# Call boundaries shapefile
str(ger_one_subset@data) # 16 obs. of  3 variables:
names(ger_one_subset)
# [1] "ID"         "NAME"       "ASCII_NAME"
unit.i <- sort(unique(ger_one_subset$NAME), decreasing = FALSE) # 429

### Create an empty dataframe
final_dataframe <- as.data.frame(unit.i)
colnames(final_dataframe)[1] <- "REGION"
nunits <- length(final_dataframe$REGION) # [1] 429

# Expand the data set
nyears<- length(1982:2015) # 34
nmonths<- length(1:12) # 12
nrow_perunit <- nyears*nmonths # 408
nrows <- nrow_perunit*nunits # [1]  6528
final_dataframe1 <- rep(final_dataframe, 408) # mefa package

# Add years
years <- as.data.frame(c(1982:2015))
colnames(years)[1] <- "YEAR"
years1 <- as.data.frame(years[rep(seq_len(nrow(years)), each=nunits*nmonths),])
colnames(years1)[1] <- "YEAR"
final_dataframe1$YEAR <- years1$YEAR

# Add quarters
quarters <- as.data.frame(c(1:4))
colnames(quarters)[1] <- "QUARTER"
quarters1 <- as.data.frame(quarters[rep(seq_len(nrow(quarters)), each=nunits*3),])
# Because three times have to be the same quarter.
colnames(quarters1)[1] <- "QUARTER"
quarters2 <- rep(quarters1, nyears) 
final_dataframe1$QUARTER <- quarters2$QUARTER

# Add months
months <- as.data.frame(c(1:12))
colnames(months)[1] <- "MONTH"
months1 <- as.data.frame(months[rep(seq_len(nrow(months)), each=nunits),])
months2 <- rep(months1, nyears) 
colnames(months2)[1] <- "MONTH"
final_dataframe1$MONTH <- months2$MONTH
str(final_dataframe1$MONTH)
final_dataframe1$MONTH <- as.factor(final_dataframe1$MONTH)
#final_dataframe1$MONTH <- recode(final_dataframe1$MONTH, "'1'='01'; '2'='02'; 
#                         '3'='03'; '4'='04';'5'='05';
#                       '6'='06';'7'='07'; '8'='08'; '9'='09'")
final_dataframe1$MONTH <- recode(final_dataframe1$MONTH, `1` = "01", `2` = "02", 
                                 `3` = "03", 
                                 `4` = "04", `5` = "05", `6` = "06", `7` = "07",
                                 `8` = "08", `9` = "09")

#Clean dataset
final_dataframe1$ID <- with(final_dataframe1, paste0(YEAR, "M", MONTH)) 
# create ID for year plus month.
str(final_dataframe1)
final_dataframe1$YEAR <- as.numeric(final_dataframe1$YEAR)
final_dataframe1$QUARTER <- as.numeric(final_dataframe1$QUARTER)
final_dataframe1$MONTH <- as.numeric(final_dataframe1$MONTH)

# Add am empty column for count
final_dataframe1$COUNT <- NA
str(final_dataframe1) # OK now
# 'data.frame':	6528 obs. of  6 variables:
# $ REGION : Factor w/ 16 levels "Baden-Werttemberg",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ YEAR   : num  1982 1982 1982 1982 1982 ...
# $ QUARTER: num  1 1 1 1 1 1 1 1 1 1 ...
# $ MONTH  : num  1 1 1 1 1 1 1 1 1 1 ...
# $ ID     : chr  "1982M01" "1982M01" "1982M01" "1982M01" ...
# $ COUNT  : logi  NA NA NA NA NA NA ...


##### Merge this empty dataframe with the cases dataframe

str(ger_cases_monthly1) # 6528 obs. of  6 variables:
tmp1 <- merge(final_dataframe1, ger_cases_monthly1, by = c("REGION", "ID"), all.x = TRUE, 
              all.y = TRUE)
tmp1 <- tmp1[,c(1:5,10)]
names(tmp1) # [1] "ADM_UNIT"  "ID"        "YEAR.x"    "QUARTER.x" "MONTH.x"   "COUNT.y" 
tmp1$COUNT.y[is.na(tmp1$COUNT.y)]<-0
colnames(tmp1)[3] <- "YEAR"
colnames(tmp1)[4] <- "QUARTER"
colnames(tmp1)[5] <- "MONTH"
colnames(tmp1)[6] <- "COUNT"
head(tmp1)

# Write it
write.csv(tmp1, file = "output/full_federalstates_posandneg_nonstrecke.csv")


# SUBSET federal stated required
print(sort(unique(tmp1$REGION), decreasing = FALSE))
# [1] Baden-Werttemberg      Bayern                 Berlin                 Brandenburg           
# [5] Bremen                 Hamburg                Hessen                 Mecklenburg-Vorpommern
# [9] Niedersachsen          Nordrhein-Westfalen    Rheinland-Pfalz        Saarland              
# [13] Sachsen                Sachsen-Anhalt         Schleswig-Holstein     ThGringen  

# Only "Brandenburg", "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thüringen"
tmp2 <- tmp1[which(tmp1$REGION %in% c("Brandenburg", "Mecklenburg-Vorpommern",
                                      "Sachsen", "Sachsen-Anhalt", "ThGringen")),]
str(tmp2) # 'data.frame':	2040 obs. of  6 variables:
write.csv(tmp2, file = "output/full_federalstates_posandneg_subset_nonstrecke.csv")






############ just positive ###########
### PREPARE GERMAN AND EUROPEAN DATASET TO JOIN THEM ###

### GERMAN DATASET ###

# Call the germany dataset

names(germany)
#[1] "JAHR"     "LFD_NR"   "UA_SCHL"  "UA_NAME"  "GEB_SCHL" "GEB_NAME" "TA_SCHL" 
#[8] "TA_NAME"  "EG_SCHL"  "EG_NAME"  "ALTER"    "DATUM"    "OTC"      "IFT"     
#[15] "PCR"      "RFFIT"

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

length(sort(unique(germany$GEB_NAME), decreasing = FALSE))
# [1] 9480

# Subset foxes Germany data only
germany_subset <- subset(germany, TA_NAME == "Fuchs")
sort(unique(germany_subset$JAHR))
#[1] 1900 1901 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997
#[19] 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015
#[37] 2200 2201 2203 2204 2205 2207
# 43 Levels: 1900 1901 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 ... 2213
# 1900, 1901, 2200, 2201, 2203, 2204, 2205, 2207!!!
table(germany_subset$JAHR)

germany_subset1 <- germany_subset[which(germany_subset$IFT=="+"),]
str(germany_subset1@data) # 18081 obs. of  16 variables:
germany_subset1$EG_NAME <- factor(germany_subset1$EG_NAME)
germany_subset1$IFT <- factor(germany_subset1$IFT)
germany_subset1$JAHR <- as.numeric(as.character(germany_subset1$JAHR))

str(germany_subset1@data) 

## Split the date to keep the month only
germany_subset1$DATUM <- as.character(germany_subset1$DATUM) 
# has to be chr for the split
df <- substr(germany_subset1$DATUM, 6, 7)
df1 <- as.data.frame(df)
str(df1) # 'data.frame':	31095 obs. of  1 variable:
# df: Factor w/ 12 levels

## Add the column to the germany data
germany_subset1@data$MONTH <- df1$df
names(germany_subset1) # month appear at the end
head(germany_subset1@data)
plot(germany_subset1)

###### try pos (lauries code)
#g_rabies <- germany_subset
#g_rabies$date<-as.POSIXct(g_rabies$DATUM,"%Y-%m-%d",tz="gmt") 
##getting the right date format

## Choose columns of interest
germany_subset1 <- germany_subset1[,c(1,17)]
names(germany_subset1)
# [1] "JAHR"     "MONTH"



#### ADD FEDERAL STATE TO DATAFRAMES

names(germany_subset1) # [1] "JAHR"  "MONTH"
length(germany_subset1) # 18081

# Call DEU1
names(ger_one)
# [1] "OBJECTID"  "ID_0"      "ISO"       "NAME_0"    "ID_1"      "NAME_1"    "HASC_1"   
# [8] "CCN_1"     "CCA_1"     "TYPE_1"    "ENGTYPE_1" "NL_NAME_1" "VARNAME_1"
head(ger_one) # NAME_1 IS THE ONE WE NEED

# with nuts 1
# [1] "ID"         "NAME"       "ASCII_NAME"
#ger_one_subset <- subset(ger_one, ID %in% c("DE1", "DE2", "DE3", "DE4", "DE5", "DE6",
#                                            "DE7", "DE8", "DE9", "DEA", "DEB", "DEC",
#                                            "DED", "DEE", "DEF", "DEG"))
##ger_one_subset$ID <- factor(ger_one_subset$ID)
#ger_one_subset$NAME <- factor(ger_one_subset$NAME)
#ger_one_subset$ASCII_NAME <- factor(ger_one_subset$ASCII_NAME)
#str(ger_one_subset@data) # Factor w/ 16 levels

ger_one_subset <- ger_one # only for gadm1
plot(ger_one_subset)

#print(ger_one_subset$NAME) # nuts1
# [1] Baden-Werttemberg      Bayern                 Berlin                
# [4] Brandenburg            Bremen                 Hamburg               
# [7] Hessen                 Mecklenburg-Vorpommern Niedersachsen         
# [10] Nordrhein-Westfalen    Rheinland-Pfalz        Saarland              
# [13] Sachsen                Sachsen-Anhalt         Schleswig-Holstein    
# [16] ThGringen 

print(ger_one_subset$NAME_1) #  gadm1
# [1] "Baden-Württemberg"      "Bayern"                 "Berlin"                
# [4] "Brandenburg"            "Bremen"                 "Hamburg"               
# [7] "Hessen"                 "Mecklenburg-Vorpommern" "Niedersachsen"         
# [10] "Nordrhein-Westfalen"    "Rheinland-Pfalz"        "Saarland"              
# [13] "Sachsen-Anhalt"         "Sachsen"                "Schleswig-Holstein"    
# [16] "Thüringen"      


plot(germany_subset1, add=TRUE, col="red", pch=21, cex=0.2)
plot(ger_one_subset, add=TRUE)
# All Regions with no cases in the german dataset, but we dont know about years

# Overlay, as done before in QGIS
o <- over(germany_subset1, ger_one_subset)
length(germany_subset1) #  18081
length(o$NAME_1) #  18081
germany_subset2 <- germany_subset1
germany_subset2@data <- cbind(germany_subset1@data,o$NAME_1)
names(germany_subset2) # [1] "JAHR"  "MONTH" "o$NAME_1"
colnames(germany_subset2@data)[1] <- "YEAR"
colnames(germany_subset2@data)[3] <- "REGION"

# Create a new column with year and month together
germany_subset2@data$ID <- with(germany_subset2@data, paste0(YEAR, "M", MONTH))
names(germany_subset2) # [1] "YEAR"   "MONTH"  "REGION" "ID"

dim(germany_subset2) # [1]  18081      4
germany_subset3 <- na.exclude(germany_subset2)
dim(germany_subset3) # no NAs

# Create the compiled dataframe, per months
# German cases from 1982 to 2007
ger_cases_monthly <- as.data.frame(table(germany_subset3$REGION,
                                         germany_subset3$YEAR,
                                         germany_subset3$MONTH))
head(ger_cases_monthly) # [1] "Var1" "Var2" "Var3" "Freq"

# Correct column names
colnames(ger_cases_monthly)[1] <- "REGION"
colnames(ger_cases_monthly)[2] <- "YEAR"
colnames(ger_cases_monthly)[3] <- "MONTH"
colnames(ger_cases_monthly)[4] <- "COUNT"

# Create a new column with year and month together
ger_cases_monthly$ID <- with(ger_cases_monthly, paste0(YEAR, "M", MONTH))
names(ger_cases_monthly) # [1] "REGION" "YEAR"   "MONTH"  "COUNT"  "ID"  
# Tidy the dataset
ger_cases_monthly <- ger_cases_monthly[,c(1,5,2:4)]



### CREATE A MONTHLY DATA FRAME WITH ALL UNITS IN GERMANY ###

# Monthly cases shapefile
# Add quarters
ger_cases_monthly$QUARTER <- ger_cases_monthly$MONTH 
library(dplyr) # upload again.
ger_cases_monthly$QUARTER <- recode(ger_cases_monthly$QUARTER, `01` = "1", `02` = "1", 
                                    `03` = "1", 
                                    `04` = "2", `05` = "2", `06` = "2", `07` = "3",
                                    `08` = "3", `09` = "3", `10` = "4", `11` = "4", 
                                    `12` = "4")

str(ger_cases_monthly)
ger_cases_monthly <- ger_cases_monthly[,c(1,3,6,4,2,5)]
head(ger_cases_monthly)
ger_cases_monthly$MONTH <- as.numeric(ger_cases_monthly$MONTH)
ger_cases_monthly$QUARTER <- as.numeric(ger_cases_monthly$QUARTER)
ger_cases_monthly$YEAR <- as.numeric(as.character(ger_cases_monthly$YEAR))
str(ger_cases_monthly)
#'data.frame':	3900 obs. of  6 variables:
# $ REGION : Factor w/ 13 levels "Baden-Württemberg",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ YEAR   : num  1982 1982 1982 1982 1982 ...
# $ QUARTER: num  1 1 1 1 1 1 1 1 1 1 ...
# $ MONTH  : num  1 1 1 1 1 1 1 1 1 1 ...
# $ ID     : chr  "1982M01" "1982M01" "1982M01" "1982M01" ...
# $ COUNT  : int  0 0 0 36 0 22 0 0 0 0 ...


## SELECT ONLY YEARS THAT MAKES SENSE #
print(sort(unique(ger_cases_monthly$YEAR), decreasing = FALSE))
# [1] 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999
#[19] 2000 2001 2002 2003 2004 2005 2006

# for full
#ger_cases_monthly1 <- ger_cases_monthly[which(ger_cases_monthly$YEAR %in% 1982:2015),]
#str(ger_cases_monthly1)
# 'data.frame':	6528 obs. of  6 variables:
# $ REGION : Factor w/ 16 levels "Baden-Werttemberg",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ YEAR   : num  1982 1982 1982 1982 1982 ...
# $ QUARTER: num  1 1 1 1 1 1 1 1 1 1 ...
# $ MONTH  : num  1 1 1 1 1 1 1 1 1 1 ...
# $ ID     : chr  "1982M01" "1982M01" "1982M01" "1982M01" ...
# $ COUNT  : int  0 0 0 54 0 0 0 48 0 0 ...



# This one is empty for COUNT column ##
# Call boundaries shapefile
library(mefa)
str(ger_one_subset@data) # 16 obs. of  3 variables:
#names(ger_one_subset) # nuts
# [1] "ID"         "NAME"       "ASCII_NAME"
#unit.i <- sort(unique(ger_one_subset$NAME), decreasing = FALSE) # 429

names(ger_one_subset) # gadm_1
#[1] "OBJECTID"  "ID_0"      "ISO"       "NAME_0"    "ID_1"      "NAME_1"    "HASC_1"   
#[8] "CCN_1"     "CCA_1"     "TYPE_1"    "ENGTYPE_1" "NL_NAME_1" "VARNAME_1"
unit.i <- sort(unique(ger_one_subset$NAME_1), decreasing = FALSE) # 429

### Create an empty dataframe
final_dataframe <- as.data.frame(unit.i)
colnames(final_dataframe)[1] <- "REGION"
nunits <- length(final_dataframe$REGION) # [1] 16

# Expand the data set
nyears<- length(1982:2015) # 34
nmonths<- length(1:12) # 12
nrow_perunit <- nyears*nmonths # 408
nrows <- nrow_perunit*nunits # [1]  6528
final_dataframe1 <- rep(final_dataframe, 408) # mefa package

# Add years
years <- as.data.frame(c(1982:2015))
colnames(years)[1] <- "YEAR"
years1 <- as.data.frame(years[rep(seq_len(nrow(years)), each=nunits*nmonths),])
colnames(years1)[1] <- "YEAR"
final_dataframe1$YEAR <- years1$YEAR

# Add quarters
quarters <- as.data.frame(c(1:4))
colnames(quarters)[1] <- "QUARTER"
quarters1 <- as.data.frame(quarters[rep(seq_len(nrow(quarters)), each=nunits*3),])
# Because three times have to be the same quarter.
colnames(quarters1)[1] <- "QUARTER"
quarters2 <- rep(quarters1, nyears) 
final_dataframe1$QUARTER <- quarters2$QUARTER

# Add months
months <- as.data.frame(c(1:12))
colnames(months)[1] <- "MONTH"
months1 <- as.data.frame(months[rep(seq_len(nrow(months)), each=nunits),])
months2 <- rep(months1, nyears) 
colnames(months2)[1] <- "MONTH"
final_dataframe1$MONTH <- months2$MONTH
str(final_dataframe1$MONTH)
final_dataframe1$MONTH <- as.factor(final_dataframe1$MONTH)
#final_dataframe1$MONTH <- recode(final_dataframe1$MONTH, "'1'='01'; '2'='02'; 
#                         '3'='03'; '4'='04';'5'='05';
#                       '6'='06';'7'='07'; '8'='08'; '9'='09'")
final_dataframe1$MONTH <- recode(final_dataframe1$MONTH, `1` = "01", `2` = "02", 
                                 `3` = "03", 
                                 `4` = "04", `5` = "05", `6` = "06", `7` = "07",
                                 `8` = "08", `9` = "09")

#Clean dataset
final_dataframe1$ID <- with(final_dataframe1, paste0(YEAR, "M", MONTH)) 
# create ID for year plus month.
str(final_dataframe1)
final_dataframe1$YEAR <- as.numeric(final_dataframe1$YEAR)
final_dataframe1$QUARTER <- as.numeric(final_dataframe1$QUARTER)
final_dataframe1$MONTH <- as.numeric(final_dataframe1$MONTH)

# Add am empty column for count
final_dataframe1$COUNT <- NA
str(final_dataframe1) # OK now
# 'data.frame':	6528 obs. of  6 variables:
# $ REGION : Factor w/ 16 levels "Baden-Werttemberg",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ YEAR   : num  1982 1982 1982 1982 1982 ...
# $ QUARTER: num  1 1 1 1 1 1 1 1 1 1 ...
# $ MONTH  : num  1 1 1 1 1 1 1 1 1 1 ...
# $ ID     : chr  "1982M01" "1982M01" "1982M01" "1982M01" ...
# $ COUNT  : logi  NA NA NA NA NA NA ...



##### Merge this empty dataframe with the cases dataframe

str(ger_cases_monthly1) # 3900 obs. of  6 variables:
tmp1 <- merge(final_dataframe1, ger_cases_monthly1, by = c("REGION", "ID"), all.x = TRUE, 
              all.y = TRUE)
tmp1 <- tmp1[,c(1:5,10)]
names(tmp1) # [1] "ADM_UNIT"  "ID"        "YEAR.x"    "QUARTER.x" "MONTH.x"   "COUNT.y" 
#tmp1$COUNT.y[is.na(tmp1$COUNT.y)]<-0
tmp1$COUNT.y[tmp1$COUNT.y == 0] <- NA

colnames(tmp1)[3] <- "YEAR"
colnames(tmp1)[4] <- "QUARTER"
colnames(tmp1)[5] <- "MONTH"
colnames(tmp1)[6] <- "COUNT"
head(tmp1)

# Write it
write.csv(tmp1, file = "output/full_federalstates_pos_gadm1.csv")

# SUBSET federal stated required
#print(sort(unique(tmp1$REGION), decreasing = FALSE)) # nut
# [1] Baden-Werttemberg      Bayern                 Berlin                 Brandenburg           
# [5] Bremen                 Hamburg                Hessen                 Mecklenburg-Vorpommern
# [9] Niedersachsen          Nordrhein-Westfalen    Rheinland-Pfalz        Saarland              
# [13] Sachsen                Sachsen-Anhalt         Schleswig-Holstein     ThGringen  

print(sort(unique(tmp1$REGION), decreasing = FALSE)) # gadm
# [1] Baden-Württemberg      Bayern                 Berlin                 Brandenburg           
# [5] Bremen                 Hamburg                Hessen                 Mecklenburg-Vorpommern
# [9] Niedersachsen          Nordrhein-Westfalen    Rheinland-Pfalz        Saarland              
# [13] Sachsen                Sachsen-Anhalt         Schleswig-Holstein     Thüringen      

# Only "Brandenburg", "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thüringen"

tmp2 <- tmp1[which(tmp1$REGION %in% c("Brandenburg", "Mecklenburg-Vorpommern",
                                      "Sachsen", "Sachsen-Anhalt", "Thüringen")),]
str(tmp2) # 'data.frame':	2040 obs. of  6 variables:
write.csv(tmp2, file = "output/full_federalstates_pos_gadm1_subset.csv")
