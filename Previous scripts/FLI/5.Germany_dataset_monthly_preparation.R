##'**LAURIE'S PROJECT**
##'**FOX RABIES DYNAMIC**
##'**MICAELA DE LA PUENTE L.**        
##'**This script subset fox rabies data in the germana dataset and creates a new** 
##'**variable MONTH. Also add missing information of germany regions using the**
##'**european dataset**
##########################################################################

library(rgdal)
library(rgeos)
require(GISTools)

##########################################################################

### GERMAN DATASET ###

# Call the germany dataset
germany <-readOGR("Cases/German rabies dataset/Germany_1982_2014.shp",
                  "Germany_1982_2014")

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

length(sort(unique(germany_subset$GEB_NAME), decreasing = FALSE))
# [1] 8731
length(levels(sort(unique(germany_subset$GEB_NAME), decreasing = FALSE)))
# [1] 9480
str(germany_subset@data$GEB_NAME)
# Factor w/ 9480 levels 

# Subset IFT positive only
str(germany_subset$IFT)
#  Factor w/ 4 levels "-","?","+","U":
germany_subset1 <- subset(germany_subset, IFT == "+")
germany_subset1$IFT <- factor(germany_subset1$IFT)
str(germany_subset1$IFT) #  Factor w/ 1 level "+":
germany_subset1$JAHR <- factor(germany_subset1$JAHR)
table(germany_subset1$JAHR) # Only cases from 1982 to 2006.
length(sort(unique(germany_subset1$GEB_NAME), decreasing = FALSE))
# [1] 2600
length(levels(sort(unique(germany_subset1$GEB_NAME), decreasing = FALSE)))
# [1] 9480
str(germany_subset1@data$GEB_NAME) # Factor w/ 9480 levels 
# Factorize
germany_subset1$GEB_NAME <- factor(germany_subset1$GEB_NAME) 
# Dont do these if want every unit ever infected by some kind of rabies to appear.
str(germany_subset1@data$GEB_NAME) # Factor w/ 2600 levels
germany_subset1$GEB_SCHL <- factor(germany_subset1$GEB_SCHL)
str(germany_subset1@data$GEB_SCHL) # Factor w/ 2651 levels 
### This difference is important! mistake or reality?

table(germany_subset1$JAHR)

## Split the date to keep the month only
germany_subset1$DATUM <- as.character(germany_subset1$DATUM) 
# has to be chr for the split
list <- strsplit(germany_subset1$DATUM, "/")
library("plyr")
df <- ldply(list) #divide the date
df <- df[, c(2)] # second column is the month
df1 <- as.data.frame(df)
str(df1)
# 'data.frame':	18081 obs. of  1 variable:
# df: Factor w/ 12 levels

## Add the column to the germany data
germany_subset1@data$MONTH <- df1$df
names(germany_subset1) # month appear at the end
head(germany_subset1@data)
plot(germany_subset1)

# Set the projection
CRS <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(germany_subset1) <- CRS
str(germany_subset1)

germany_subset1 <- germany_subset1[,c(1,6,12,17)]
names(germany_subset1)
# [1] "JAHR"     "GEB_NAME" "DATUM"    "MONTH"

#write.csv(germany_subset1, file = "output/german_subset_germany.csv")
#writeOGR(germany_subset1, dsn=".", layer="output/german_subset_germany",
#         driver="ESRI Shapefile", overwrite_layer = TRUE)



#### EUROPEAN DATASET #####

# Add the missing cases
rabies.data.single <-readOGR("Cases/Rabies_WGS84_Singlepart.shp", 
                             "Rabies_WGS84_Singlepart") 
names(rabies.data.single) #[1] "YEAR"    "QUARTER" "COUNTRY" "REGION"  "SPECIES" "Qs"      "Ss"
str(rabies.data.single@data) # 'data.frame':	147640 obs. of  6 variables:

cases_subset <- subset(rabies.data.single, COUNTRY == "Germany")
str(cases_subset@data) # 'data.frame':	14702 obs. of  6 variables:
cases_subset$COUNTRY <- factor(cases_subset$COUNTRY)

cases_subset <- subset(cases_subset, SPECIES == "fox")
str(cases_subset@data)
cases_subset$SPECIES <- factor(cases_subset$SPECIES)
cases_subset$REGION <- factor(cases_subset$REGION)
table(cases_subset$YEAR)
# 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 
# 3941 2663 1011  636 1044  636  107   74   86   37  150   35   24   21   27   39    3 


# Call DEU1
ger_one <- readOGR("Administrative Shape Database (Rabigramm)/Europe_Nuts1_WGS84.shp",
                   "Europe_Nuts1_WGS84") 
CRS <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(ger_one) <- CRS
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

# Regions with no cases in the german dataset
#Schleswig-Holstein
#Hamburg
#Niedersachsen
#Bremen

plot(ger_one_subset)
plot(cases_subset, add=TRUE, col="red", pch=21, cex=0.2)
plot(ger_one_subset, add=TRUE)
# Cases in all regions for the European dataset.

# Compare raabies subsets of Germany 
table(germany_subset1$JAHR)
# 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 
#1934 2124 1795 1224  965 1150 1497 2381 2306 1648  323   28    4    7  133   85   92   38 
#2000 2001 2002 2003 2004 2005 2006 
#210   53   24   21   27   10    2 

table(cases_subset$YEAR)
# 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 
# 3941 2663 1011  636 1044  636  107   74   86   37  150   35   24   21   27   39    3 
# European dataset has more cases from 1990 to 1996. Then, from 1997 to 2001, the german
# dataset has more cases. From 2002 to 2004 both have the same number of cases. 
# In 2005 and 2006, the european dataset has more cases again.


# Save spdf with missing cases per year.

cases_subset$YEAR <- as.numeric(cases_subset$YEAR)
year.i <- sort(unique(cases_subset$YEAR), decreasing = FALSE)
class(year.i)
str(cases_subset$YEAR)

eu_subset_clip <- list()

for(j in year.i)
  {
  ger_subset <- subset(germany_subset1, JAHR == j)
  eu_subset <- subset(cases_subset, YEAR == j)
  
  plot(ger_one_subset, main=paste(j, "german dataset"))
  plot(ger_subset, add=TRUE, col="red", pch=21, cex=0.2)
  plot(ger_one_subset, add=TRUE)
  
  plot(ger_one_subset, main=paste(j, "european dataset"))
  plot(eu_subset, add=TRUE, col="red", pch=21, cex=0.2)
  plot(ger_one_subset, add=TRUE)
  
  # Convert the counts to a dataframe
  counts = poly.counts(ger_subset,ger_one_subset)
  df = data.frame(counts)
  
  ger_one_subset1 <- ger_one_subset
  ger_one_subset1@data <- cbind(ger_one_subset1@data, df)
  
  # For fun: Compute densities and map them in a choropleth map
#  choropleth(ger_one_subset,counts/poly.areas(ger_one_subset1))
  
  ger_one_subset2 <- subset(ger_one_subset1, counts == 0)
  eu_subset_clip[[j]] <- eu_subset[ger_one_subset2, ]

  plot(ger_one_subset, main=paste(j, "with added cases"))
  plot(ger_subset, add=TRUE, col="blue", pch=21, cex=0.3)
  plot(eu_subset_clip[[j]], add=TRUE, col="red", pch=21, cex=0.3)
  }

class(eu_subset_clip)
#[1] "list"

class(eu_subset_clip[[1990]])
class(eu_subset_clip[[1991]])
str(eu_subset_clip[[1991]]@data)

tria1 <- do.call("rbind", lapply(eu_subset_clip, as.data.frame))
# convert to a spdf
xy <- tria1[,c(7,8)]
clip_cases <- SpatialPointsDataFrame(coords = xy, data = tria1, 
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

plot(ger_one_subset)
plot(germany_subset1, add=TRUE, col="blue", pch=21, cex=0.3)
plot(clip_cases, add=TRUE, col="red", pch=21, cex=0.3)

head(clip_cases)


#write.csv(clip_cases, file = "output/clip_european_subset_germany.csv")
#writeOGR(clip_cases, dsn=".", layer="output/clip_european_subset_germany",
#         driver="ESRI Shapefile", overwrite_layer = TRUE)
