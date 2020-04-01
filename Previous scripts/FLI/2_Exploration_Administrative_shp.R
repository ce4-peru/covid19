##'**LAURIE'S PROJECT**
##'**FOX RABIES DYNAMIC**
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

### FOLDER: DATA/ADMINISTRATIVE SHAPE (Rabigramm)

### Level 0

## Check shapefile of Administrative Units level
nuts_l0 <-readOGR("Administrative Shape Database (Rabigramm)/Europe_Nuts0_WGS84.shp", 
                    "Europe_Nuts0_WGS84") 

## Check the data attributes
names(nuts_l0)
# [1] "OBJECTID"   "CODE"       "Shape_Leng" "Shape_Area"

## Check how data looks like
head(nuts_l0@data)
#  OBJECTID CODE Shape_Leng  Shape_Area
#0        1   AD   111984.2   439346139
#1        2   AL  1251828.9 27598908394
#2        3   AM  1261830.8 28785023199
#3        4   AT  2527989.4 78478252773

## Check projection
str(nuts_l0)
#..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Find out how countries are identified
levels(unique(nuts_l0$CODE)) # 54 levels, 54 countries
# [1] "AD" "AL" "AM" "AT" "AZ" "BA" "BE" "BG" "BY" "CH" "CY" "CZ" "DE" "DK" "EE" "ES" "FI"
# [18] "FO" "FR" "GE" "GI" "GR" "HR" "HU" "IC" "IE" "IM" "IS" "IT" "LI" "LT" "LU" "LV" "MC"
# [35] "MD" "ME" "MK" "MT" "NL" "NO" "PL" "PT" "RO" "RS" "RU" "SE" "SI" "SJ" "SK" "SM" "TR"
# [52] "UA" "UK" "XK"
# http://www.immigration-usa.com/country_digraphs.html
# Croatia is HR and Sweden is SE.

# Subset all but Croatia and Sweden
nuts_l0_subset <- subset(nuts_l0, !(CODE %in% c("HR", "SE")))
nuts_l0_subset$CODE <- factor(nuts_l0_subset$CODE)
str(nuts_l0_subset@data) # $ CODE: Factor w/ 52 levels "AD","AL","AM",..

# Plot it
png("output/Europe_Level0_nuts.png", width=5, height=5, units="in", res=300)
plot(nuts_l0_subset, main="Europe_Level0")
dev.off()



## Level 1

### Check shapefile of Administrative Units level
nuts_l1 <-readOGR("Administrative Shape Database (Rabigramm)/Europe_Nuts1_WGS84.shp", 
                  "Europe_Nuts1_WGS84") 

## Check the data attributes
names(nuts_l1)
# [1] "ID"         "NAME"       "ASCII_NAME"

head(nuts_l1@data, 3)
#    ID             NAME       ASCII_NAME
# 0 AD          Andorra          Andorra
# 1 AL        Shqiperia        Shqiperia
# 2 AM Armenia/Hayastan Armenia/Hayastan
# Country code as ID

## Check projection
str(nuts_l1)
#..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

levels(unique(nuts_l1$ID)) # 132 levels, 132 countries
# [1] "AD"  "AL"  "AM"  "AT1" "AT2" "AT3" "AZ"  "BA"  "BE1" "BE2" "BE3" "BG3" "BG4" "BY"  "CH" 
# [16] "CY0" "CZ0" "DE1" "DE2" "DE3" "DE4" "DE5" "DE6" "DE7" "DE8" "DE9" "DEA" "DEB" "DEC" "DED"
# [31] "DEE" "DEF" "DEG" "DK0" "EE0" "ES1" "ES2" "ES3" "ES4" "ES5" "ES6" "ES7" "FI1" "FI2" "FO" 
# [46] "FR1" "FR2" "FR3" "FR4" "FR5" "FR6" "FR7" "FR8" "GE"  "GI"  "GR1" "GR2" "GR3" "GR4" "HR0"
# [61] "HU1" "HU2" "HU3" "IC"  "IE0" "IM"  "IS"  "ITC" "ITD" "ITE" "ITF" "ITG" "LI"  "LT0" "LU0"
# [76] "LV0" "MC"  "MD"  "ME"  "MK"  "MT0" "NL1" "NL2" "NL3" "NL4" "NO"  "PL1" "PL2" "PL3" "PL4"
# [91] "PL5" "PL6" "PT1" "PT2" "PT3" "RO1" "RO2" "RO3" "RO4" "RS"  "RU"  "SE0" "SI0" "SJ"  "SK0"
# [106] "SM"  "TR1" "TR2" "TR3" "TR4" "TR5" "TR6" "TR7" "TR8" "TR9" "TRA" "TRB" "TRC" "UA"  "UKC"
# [121] "UKD" "UKE" "UKF" "UKG" "UKH" "UKI" "UKJ" "UKK" "UKL" "UKM" "UKN" "XK" 
# Croatia is HR0 and Sweden is SE0.

# Subset
nuts_l1_subset <- subset(nuts_l1, !(ID %in% c("HR0", "SE0")))
nuts_l1_subset$ID <- factor(nuts_l1_subset$ID)
str(nuts_l1_subset@data) # $  ID: Factor w/ 130 levels "AD","AL","AM"

png("output/Europe_Level1_nuts.png", width=5, height=5, units="in", res=300)
plot(nuts_l1_subset, main="Europe_Level1")
dev.off()



## Level 2

### Check shapefile of Administrative Units level
nuts_l2 <-readOGR("Administrative Shape Database (Rabigramm)/Europe_Nuts2_WGS84.shp", 
                  "Europe_Nuts2_WGS84") 

## Check the data attributes
names(nuts_l2)
# [1] "ID"         "NAME"       "ASCII_NAME"

head(nuts_l2@data, 6)
#    ID             NAME       ASCII_NAME
# 0 AD          Andorra          Andorra
# 1 AL        Shqiperia        Shqiperia
# 2 AM Armenia/Hayastan Armenia/Hayastan
# Country code as ID

## Check projection
str(nuts_l2)
#..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

levels(unique(nuts_l2$ID)) # 333 levels
# [1] "AD"  "AL"  "AM"  "AT1" "AT2" "AT3" "AZ"  "BA"  "BE1" "BE2" "BE3" "BG3" "BG4" "BY"  "CH" 
# [16] "CY0" "CZ0" "DE1" "DE2" "DE3" "DE4" "DE5" "DE6" "DE7" "DE8" "DE9" "DEA" "DEB" "DEC" "DED"
# [31] "DEE" "DEF" "DEG" "DK0" "EE0" "ES1" "ES2" "ES3" "ES4" "ES5" "ES6" "ES7" "FI1" "FI2" "FO" 
# [46] "FR1" "FR2" "FR3" "FR4" "FR5" "FR6" "FR7" "FR8" "GE"  "GI"  "GR1" "GR2" "GR3" "GR4" "HR0"
# [61] "HU1" "HU2" "HU3" "IC"  "IE0" "IM"  "IS"  "ITC" "ITD" "ITE" "ITF" "ITG" "LI"  "LT0" "LU0"
# [76] "LV0" "MC"  "MD"  "ME"  "MK"  "MT0" "NL1" "NL2" "NL3" "NL4" "NO"  "PL1" "PL2" "PL3" "PL4"
# [91] "PL5" "PL6" "PT1" "PT2" "PT3" "RO1" "RO2" "RO3" "RO4" "RS"  "RU"  "SE0" "SI0" "SJ"  "SK0"
# [106] "SM"  "TR1" "TR2" "TR3" "TR4" "TR5" "TR6" "TR7" "TR8" "TR9" "TRA" "TRB" "TRC" "UA"  "UKC"
# [121] "UKD" "UKE" "UKF" "UKG" "UKH" "UKI" "UKJ" "UKK" "UKL" "UKM" "UKN" "XK" 
# Croatia is HR01, HR02, HR03, and Sweden from SE01, SE02, SE04, SE06  to SE09 and SE0A.

# Subset
nuts_l2_subset <- subset(nuts_l2, !(ID %in% c("HR01", "HR02", "HR03", "SE01", "SE02", 
                                              "SE04", "SE06", 
                                              "SE07", "SE08", "SE09", "SE0A")))
nuts_l2_subset$ID <- factor(nuts_l2_subset$ID)
str(nuts_l2_subset@data) # $  ID: Factor w/ 322 levels 

png("output/Europe_Level2_nuts.png", width=5, height=5, units="in", res=300)
plot(nuts_l2_subset, main="Europe_Level2")
dev.off()


## Level 3

### Check shapefile of Administrative Units level
nuts_l3 <-readOGR("Administrative Shape Database (Rabigramm)/Europe_Nuts3_WGS84.shp", 
                  "Europe_Nuts3_WGS84") 

## Check the data attributes
names(nuts_l3)
# [1] "ID"         "NAME"       "ASCII_NAME" "CODE" 

head(nuts_l3@data)
#  ID         Name  Name_ASCII CODE
#0 01        Berat       Berat   AL
#1 02       Dibër       Diber   AL
#2 03      Durrës      Durres   AL
#3 04      Elbasan     Elbasan   AL
#4 05         Fier        Fier   AL
#5 06 Gjirokastër Gjirokaster   AL

## Check projection
str(nuts_l3)
#..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

levels(unique(nuts_l3$Name)) # 2415 levels
levels(unique(nuts_l3$CODE)) # 55 levels,
# Croatia is HR and Sweden is SE.

# Subset
nuts_l3_subset <- subset(nuts_l3, !(CODE %in% c("HR", "SE")))
nuts_l3_subset$CODE <- factor(nuts_l3_subset$CODE)
str(nuts_l3_subset@data) # $  CODE      : Factor w/ 53 levels

png("output/Europe_Level3_nuts.png", width=5, height=5, units="in", res=300)
plot(nuts_l3_subset, main="Europe_Level3")
dev.off()


# Save all subsets without Croatia and Sweden
#writeOGR(nuts_l0_subset, dsn=".", layer="output/nuts_l0_subset", driver="ESRI Shapefile")
#writeOGR(nuts_l1_subset, dsn=".", layer="output/nuts_l1_subset", driver="ESRI Shapefile")
#writeOGR(nuts_l2_subset, dsn=".", layer="output/nuts_l2_subset", driver="ESRI Shapefile")
#writeOGR(nuts_l3_subset, dsn=".", layer="output/nuts_l3_subset", driver="ESRI Shapefile")



### FOLDER: DATA/ADMINISTRATIVE EUROPE

### Check shapefile of Administrative Units level
europe_l1 <-readOGR("Administrative Europe/Europa_Level_1.shp", 
                    "Europa_Level_1") 

## Check the data attributes
names(europe_l1)
# [1] "ID"    "Name"    "ASCII_Name"    "Name_EN"    "Shape_Leng"    "Shape_Area"
levels(unique(europe_l1$Name)) # 53 levels, 53 countries
#[1] "�-sterreich"                    "Ísland"                       
#[3] "Andorra"                        "Armenia/Hayastan"              
#[5] "Azerbaijan"                     "Belarus'"                      
#[7] "Belgique-België"               "Bosna i Hercegovina"           
#[9] "Bulgaria"                       "Ceska Republika"               
#[11] "Channel Islands"                "Danmark"                       
#[13] "Deutschland"                    "Eesti"                         
#[15] "Ellada"                         "España"                       
#[17] "Færørne"                      "France"                        
#[19] "Georgia/Gruzija/Sakartwelo"     "Gibraltar"                     
#[21] "Hrvatska"                       "Ireland"                       
#[23] "Isle of Man"                    "Italia"                        
#[25] "Kypros / Kibris"                "Latvija"                       
#[27] "Liechtenstein"                  "Lietuva"                       
#[29] "Luxembourg (Grand-Duché)"      "Magyarorszag"                  
#[31] "Makedonija"                     "Malta"                         
#[33] "Moldova"                        "Monaco"                        
#[35] "Montenegro"                     "Nederland"                     
#[37] "Norge"                          "Polska"                        
#[39] "Portugal"                       "România"                      
#[41] "Rossija"                        "San Marino"                    
#[43] "Schweiz/Suisse/Svizzera"        "Serbia"                        
#[45] "Shqiperia"                      "Slovenija"                     
#[47] "Slovenska Republika"            "Suomi / Finland"               
#[49] "Svalbard and Jan Mayen Islands" "Sverige"                       
#[51] "Turkiye"                        "Ukraine"                       
#[53] "United Kingdom"                

## Check projection
str(europe_l1)
#..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

png("output/Europe_Level1.png", width=5, height=5, units="in", res=300)
plot(europe_l1, main="Europe_Level1")
dev.off()



## Check shapefile of Administrative Units level
europe_l2 <-readOGR("Administrative Europe/Europa_Level_2.shp", 
                    "Europa_Level_2") 

## Check the data attributes
names(europe_l2)
# [1] "ID"    "Name"    "ASCII_Name"    "Name_EN"    "Shape_Leng"    "Shape_Area"
levels(unique(europe_l2$Name)) # 333 levels, provinces

## Check projection
str(europe_l2)
#..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

png("output/Europe_Level2.png", width=5, height=5, units="in", res=300)
plot(europe_l2, main="Europe_Level2")
dev.off()



### Check shapefile of Administrative Units level 3
europe_l3 <-readOGR("Administrative Europe/Europa_Level_3.shp", 
                    "Europa_Level_3") 

## Check the data attributes
names(europe_l3)
# [1] "ID"    "Name"    "ASCII_Name"    "Name_EN"    "Shape_Leng"    "Shape_Area"
levels(unique(europe_l3$Name)) # 2416 levels, provinces

## Check projection
str(europe_l3)
#..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


png("output/Europe_Level3.png", width=5, height=5, units="in", res=300)
plot(europe_l3, main="Europe_Level3")
dev.off()



### Check shapefile of Adminisrative Units level 4
europe_l4 <-readOGR("Administrative Europe/Europa_Level_4.shp", 
                    "Europa_Level_4") 

## Check the data attributes
names(europe_l4)
# [1] "ID"    "Name"    "ASCII_Name"    "Name_EN"    "Shape_Leng"    "Shape_Area"
levels(unique(europe_l4$Name)) # 8415 levels

## Check projection
str(europe_l4)
#..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


png("output/Europe_Level4.png", width=5, height=5, units="in", res=300)
plot(europe_l4, main="Europe_Level4")
dev.off()



### Check shapefile of Adminisrative Units level 5
europe_l5 <-readOGR("Administrative Europe/Europa_Level_5.shp", 
                    "Europa_Level_5") 
# Error: cannot allocate vector of size 1024 Kb

## Check the data attributes
names(europe_l5)
levels(unique(europe_l5$Name)) # 333 levels, provinces

## Check projection
str(europe_l5)

png("output/Europe_Level5.png", width=5, height=5, units="in", res=300)
plot(europe_l5, main="Europe_Level5")
dev.off()

## Level 5 was not plotted because was no able to be uploaded.

#############################################################

### FOLDER: DATA/Administrative region

region <-readOGR("Administrative region/region_aktuell.shp", 
                 "region_aktuell")
plot(region)

region.alle <-readOGR("Administrative region/region_alle.shp", 
                      "region_alle")
plot(region.alle) # some other countries are here.

# Check the shapefile
names(region)
# [1] "RegionCode" "Name"       "Disabled"   "ESRI_OID"  

names(region.alle)
# [1] "RegionCode" "Name"       "Disabled"   "ESRI_OID"  

head(region@data)
#   RegionCode                     Name Disabled ESRI_OID
#0       BA06          Srednjebosanski        0        1
#1       BA07 Hercegovacko-neretvanski        0        2
#2      AL005             Gjiroikaster        0        3
#3      BE353            Philippeville        0        4
#4      AL001                    Berat        0        5
#5      AL002                   Durres        0        6

# Check number of regions
length(levels(unique(region@data$RegionCode))) # 1793
length(levels(unique(region.a@data$RegionCode))) # 2161

# Look all level of regioes for the actual region shapefile
levels(unique(region@data$RegionCode))

## Copy the regions codes for the countries of interest.
#Albania: "AL001"   "AL002"   "AL003"   "AL004"   "AL005"   "AL006"   "AL007"  
#[9] "AL008"   "AL009"   "AL010"   "AL011"   "AL012"   "AT111"
#Austria: "AT111"   "AT112"   "AT113"  
#[17] "AT121"   "AT122"   "AT123"   "AT124"   "AT125"   "AT126"   "AT127"   "AT13"   
#[25] "AT211"   "AT212"   "AT213"   "AT221"   "AT222"   "AT223"   "AT224"   "AT225"  
#[33] "AT226"   "AT311"   "AT312"   "AT313"   "AT314"   "AT315"   "AT321"   "AT322"  
#[41] "AT323"   "AT331"   "AT332"   "AT333"   "AT334"   "AT335"   "AT341"   "AT342"  
#Belarus: "BY001"   "BY002"  "BY003"   "BY004"   "BY005"  "BY006"   
#Belgium: "BE1"     "BE211"   "BE212"   "BE213"   "BE221"   "BE222"  
#[73] "BE223"   "BE231"   "BE232"   "BE233"   "BE234"   "BE235"   "BE236"   "BE241"  
#[81] "BE242"   "BE251"   "BE252"   "BE253"   "BE254"   "BE255"   "BE256"   "BE257"  
#[89] "BE258"   "BE31"    "BE321"   "BE322"   "BE323"   "BE324"   "BE325"   "BE326"  
#[97] "BE327"   "BE331"   "BE332"   "BE333"   "BE334"   "BE341"   "BE342"   "BE343"  
#[105] "BE344"   "BE345"   "BE351"   "BE352"   "BE353"
#Bulgaria: "BG1"     "BG201"   "BG202"  
#[113] "BG203"   "BG204"   "BG205"   "BG206"   "BG207"   "BG208"   "BG209"   "BG20A"  
#[121] "BG20B"   "BG20C"   "BG20D"   "BG20E"   "BG301"   "BG302"   "BG303"   "BG304"  
#[129] "BG305"   "BG306"   "BG307"   "BG308"   "BG309"   "BG30A"   "BG30B"   "BG30C"  
#[137] "BG30D" 
#Croatia: "HR001"   "HR002"  
#[849] "HR003"   "HR004"   "HR005"   "HR006"   "HR007"   "HR008"   "HR009"   "HR010"  
#[857] "HR011"   "HR012"   "HR013"   "HR014"   "HR015"   "HR016"   "HR017"   "HR018"  
#[865] "HR019"   "HR020"   "HR021" 
#Czech Republic: "CZ01"   
#[177] "CZ02"    "CZ031"   "CZ032"   "CZ041"   "CZ042"   "CZ051"   "CZ052"   "CZ053"  
#[185] "CZ063"   "CZ064"   "CZ071_1" "CZ072"   "CZ080"
#Estonia: "EE0037"  "EE0039" 
#[609] "EE0044"  "EE0049"  "EE0051"  "EE0057"  "EE0059"  "EE0065"  "EE0067"  "EE0070" 
#[617] "EE0074"  "EE0078"  "EE0082"  "EE0084"  "EE0086"
#Finland: "FI131"   "FI132"   "FI133"   "FI134"   "FI141"   "FI142"   "FI143"  
#[681] "FI144"   "FI151"   "FI152"   "FI161"   "FI162"   "FI171"   "FI172"   "FI173"  
#[689] "FI174"   "FI175"   "FI176"   "FI177"   "FI2"
#France: "FR1"     "FR21"   
#[697] "FR22"    "FR23"    "FR24"    "FR25"    "FR26"    "FR3"     "FR41"    "FR42"   
#[705] "FR43"    "FR51"    "FR52"    "FR53"    "FR61"    "FR62"    "FR63"    "FR71"   
#[713] "FR72"    "FR81"    "FR82"    "FR83"    "FR91"    "FR92"    "FR93"    "FR94"   
#[721] "FR95" 
#Germany: "DE111"   "DE112"   "DE113"  
#[193] "DE114"   "DE115"   "DE116"   "DE117"   "DE118"   "DE119"   "DE11A"   "DE11B"  
#[201] "DE11C"   "DE11D"   "DE121"   "DE122"   "DE123"   "DE124"   "DE125"   "DE126"  
#[209] "DE127"   "DE128"   "DE129"   "DE12A"   "DE12B"   "DE12C"   "DE131"   "DE132"  
#[217] "DE133"   "DE134"   "DE135"   "DE136"   "DE137"   "DE138"   "DE139"   "DE13A"  
#[225] "DE141"   "DE142"   "DE143"   "DE144"   "DE145"   "DE146"   "DE147"   "DE148"  
#[233] "DE149"   "DE211"   "DE212"   "DE213"   "DE214"   "DE215"   "DE216"   "DE217"  
#[241] "DE218"   "DE219"   "DE21A"   "DE21B"   "DE21C"   "DE21D"   "DE21E"   "DE21F"  
#[249] "DE21G"   "DE21H"   "DE21I"   "DE21J"   "DE21K"   "DE21L"   "DE21M"   "DE21N"  
#[257] "DE221"   "DE222"   "DE223"   "DE224"   "DE225"   "DE226"   "DE227"   "DE228"  
#[265] "DE229"   "DE22A"   "DE22B"   "DE22C"   "DE231"   "DE232"   "DE233"   "DE234"  
#[273] "DE235"   "DE236"   "DE237"   "DE238"   "DE239"   "DE23A"   "DE241"   "DE242"  
#[281] "DE243"   "DE244"   "DE245"   "DE246"   "DE247"   "DE248"   "DE249"   "DE24A"  
#[289] "DE24B"   "DE24C"   "DE24D"   "DE251"   "DE252"   "DE253"   "DE254"   "DE255"  
#[297] "DE256"   "DE257"   "DE258"   "DE259"   "DE25A"   "DE25B"   "DE25C"   "DE261"  
#[305] "DE262"   "DE263"   "DE264"   "DE265"   "DE266"   "DE267"   "DE268"   "DE269"  
#[313] "DE26A"   "DE26B"   "DE26C"   "DE271"   "DE272"   "DE273"   "DE274"   "DE275"  
#[321] "DE276"   "DE277"   "DE278"   "DE279"   "DE27A"   "DE27B"   "DE27C"   "DE27D"  
#[329] "DE27E"   "DE300"   "DE401_1" "DE402"   "DE403"   "DE404_1" "DE405"   "DE406"  
#[337] "DE407"   "DE408_1" "DE409"   "DE40A"   "DE40B"   "DE40C"   "DE40D"   "DE40E_1"
#[345] "DE40F"   "DE40G"   "DE40H"   "DE40I"   "DE501"   "DE502"   "DE6"     "DE711"  
#[353] "DE712"   "DE713"   "DE714"   "DE715"   "DE716"   "DE717"   "DE718"   "DE719"  
#[361] "DE71A"   "DE71B"   "DE71C"   "DE71D"   "DE71E"   "DE721"   "DE722"   "DE723"  
#[369] "DE724"   "DE725"   "DE731"   "DE732"   "DE733"   "DE734"   "DE735"   "DE736"  
#[377] "DE737"   "DE803"   "DE804"   "DE80J"   "DE80K"   "DE80L"   "DE80M"   "DE80N"  
#[385] "DE80O"   "DE911"   "DE912"   "DE913"   "DE914"   "DE915"   "DE916"   "DE917"  
#[393] "DE918"   "DE919"   "DE91A"   "DE91B"   "DE922"   "DE923"   "DE925"   "DE926"  
#[401] "DE927"   "DE928"   "DE929"   "DE931"   "DE932"   "DE933"   "DE934"   "DE935"  
#[409] "DE936"   "DE937"   "DE938"   "DE939"   "DE93A"   "DE93B"   "DE941"   "DE942"  
#[417] "DE943"   "DE944"   "DE945"   "DE946"   "DE947"   "DE948"   "DE949"   "DE94A"  
#[425] "DE94B"   "DE94C"   "DE94D"   "DE94E"   "DE94F"   "DE94G"   "DE94H"   "DEA11"  
#[433] "DEA12"   "DEA13"   "DEA14"   "DEA15"   "DEA16"   "DEA17"   "DEA18"   "DEA19"  
#[441] "DEA1A"   "DEA1B"   "DEA1C"   "DEA1D"   "DEA1E"   "DEA1F"   "DEA22"   "DEA23"  
#[449] "DEA24"   "DEA26"   "DEA27"   "DEA28"   "DEA29"   "DEA2A"   "DEA2B"   "DEA2C"  
#[457] "DEA2D"   "DEA31"   "DEA32"   "DEA33"   "DEA34"   "DEA35"   "DEA36"   "DEA37"  
#[465] "DEA38"   "DEA41"   "DEA42"   "DEA43"   "DEA44"   "DEA45"   "DEA46"   "DEA47"  
#[473] "DEA51"   "DEA52"   "DEA53"   "DEA54"   "DEA55"   "DEA56"   "DEA57"   "DEA58"  
#[481] "DEA59"   "DEA5A"   "DEA5B"   "DEA5C"   "DEB11"   "DEB12"   "DEB13"   "DEB14"  
#[489] "DEB15"   "DEB16"   "DEB17"   "DEB18"   "DEB19"   "DEB1A"   "DEB1B"   "DEB21"  
#[497] "DEB22"   "DEB23"   "DEB24"   "DEB25"   "DEB31"   "DEB32"   "DEB33"   "DEB34"  
#[505] "DEB35"   "DEB36"   "DEB37"   "DEB38"   "DEB39"   "DEB3A"   "DEB3B"   "DEB3C"  
#[513] "DEB3D"   "DEB3E"   "DEB3F"   "DEB3G"   "DEB3H"   "DEB3I"   "DEB3J"   "DEB3K"  
#[521] "DEC01"   "DEC02"   "DEC03"   "DEC04"   "DEC05"   "DEC06"   "DED21"   "DED2C"  
#[529] "DED2D"   "DED2E"   "DED2F"   "DED41"   "DED42"   "DED43"   "DED44"   "DED45"  
#[537] "DED51"   "DED52"   "DED53"   "DEE01"   "DEE02"   "DEE03"   "DEE04"   "DEE05"  
#[545] "DEE06"   "DEE07"   "DEE08"   "DEE09"   "DEE0A"   "DEE0B"   "DEE0C"   "DEE0D"  
#[553] "DEE0E"   "DEF01"   "DEF02"   "DEF03"   "DEF04"   "DEF05"   "DEF06"   "DEF07"  
#[561] "DEF08"   "DEF09"   "DEF0A"   "DEF0B"   "DEF0C"   "DEF0D"   "DEF0E"   "DEF0F"  
#[569] "DEG01"   "DEG02"   "DEG03"   "DEG04"   "DEG05"   "DEG06"   "DEG07"   "DEG09"  
#[577] "DEG0A"   "DEG0B"   "DEG0C"   "DEG0D"   "DEG0E"   "DEG0F"   "DEG0G"   "DEG0H"  
#[585] "DEG0I"   "DEG0J"   "DEG0K"   "DEG0L"   "DEG0M"   "DEG0N"   "DEG0P"
#Greece: "GR111"   "GR112"   "GR113"   "GR114"   "GR115"  
#[801] "GR121"   "GR122"   "GR123"   "GR124"   "GR125"   "GR126"   "GR127"   "GR131"  
#[809] "GR132"   "GR133"   "GR134"   "GR141"   "GR142"   "GR143"   "GR144"   "GR211"  
#[817] "GR212"   "GR213"   "GR214"   "GR221"   "GR222"   "GR223"   "GR224"   "GR231"  
#[825] "GR232"   "GR233"   "GR241"   "GR242"   "GR243"   "GR244"   "GR245"   "GR251"  
#[833] "GR252"   "GR253"   "GR254"   "GR255"   "GR3"     "GR411"   "GR412"   "GR413"  
#[841] "GR421"   "GR422"   "GR431"   "GR432"   "GR433"   "GR434" 
#Hungary: "HU011"   "HU012"   "HU021"   "HU022"   "HU023"  
#[873] "HU031"   "HU032"   "HU033"   "HU041"   "HU042"   "HU043"   "HU051"   "HU052"  
#[881] "HU053"   "HU061"   "HU062"   "HU063"   "HU071"   "HU072"   "HU073" 
#Italy: "IT111"   "IT112"   "IT113"   "IT114"   "IT115"   "IT116"  
#[905] "IT117"   "IT118"   "IT12"    "IT131"   "IT132"   "IT133"   "IT134"   "IT201"  
#[913] "IT202"   "IT203"   "IT204"   "IT205"   "IT206"   "IT207"   "IT208"   "IT209"  
#[921] "IT20A"   "IT20B"   "IT311"   "IT312"   "IT321"   "IT322"   "IT323"   "IT324"  
#[929] "IT325"   "IT326"   "IT327"   "IT331"   "IT332"   "IT333"   "IT334"   "IT401"  
#[937] "IT402"   "IT403"   "IT404"   "IT405"   "IT406"   "IT407"   "IT408"   "IT409"  
#[945] "IT511"   "IT512"   "IT513"   "IT514"   "IT515"   "IT516"   "IT517"   "IT518"  
#[953] "IT519"   "IT51A"   "IT521"   "IT522"   "IT531"   "IT532"   "IT533"   "IT534"  
#[961] "IT601"   "IT602"   "IT603"   "IT604"   "IT605"   "IT711"   "IT712"   "IT713"  
#[969] "IT714"   "IT721"   "IT722"   "IT801"   "IT802"   "IT803"   "IT804"   "IT805"  
#[977] "IT911"   "IT912"   "IT913"   "IT914"   "IT915"   "IT921"   "IT922"   "IT931"  
#[985] "IT932"   "IT933"   "IT934"   "IT935"   "ITA01"   "ITA02"   "ITA03"   "ITA04"  
#[993] "ITA05"   "ITA06"   "ITA07"   "ITA08"   "ITA09"   "ITB01"   "ITB02"   "ITB03"  
#[1001] "ITB04"
#Kosovo: no kosovo
#Latvia: [1049] "LV001_1" "LV002_1" "LV003_1" "LV004_1" "LV005_1" "LV006"   "LV007"   "LV008"  
#[1057] "LV009"   "LV010"   "LV011"   "LV012"   "LV013"   "LV014"   "LV015"   "LV016"  
#[1065] "LV017"   "LV018"   "LV019"   "LV020"   "LV021"   "LV022"   "LV023"   "LV024"  
#[1073] "LV025"   "LV026"   "LV027"   "LV028"   "LV029"   "LV030"   "LV031"   "LV032"  
#[1081] "LV033"   "LV034"   "LV035"   "LV036"   "LV037"   "LV038"   "LV039"   "LV040"  
#[1089] "LV041"   "LV042"   "LV043"   "LV044"   "LV045"   "LV046"   "LV047"   "LV048"  
#[1097] "LV049"   "LV050"   "LV051"   "LV052"   "LV053"   "LV054"   "LV055"   "LV056"  
#[1105] "LV057"   "LV058"   "LV059"   "LV060"   "LV061"   "LV062"   "LV063"   "LV064"  
#[1113] "LV065"   "LV066"   "LV067"   "LV068"   "LV069"   "LV070"   "LV071"   "LV072"  
#[1121] "LV073"   "LV074"   "LV075"   "LV076"   "LV077"   "LV078"   "LV079"   "LV080"  
#[1129] "LV081"   "LV082"   "LV083"   "LV084"   "LV085"   "LV086"   "LV087"   "LV088"  
#[1137] "LV089"   "LV090"   "LV091"   "LV092"   "LV093"   "LV094" 
#Lithuania: "LT001"   "LT002"   "LT003"  
#[1041] "LT004"   "LT005"   "LT006"   "LT007"   "LT008"   "LT009"   "LT00A"
#Luxembourg : "LU"
#Macedonia: "MK01"    "MK02"    "MK03"    "MK04"    "MK05"    "MK06"   
#[1233] "MK07"    "MK08"    "MK09"    "MK10"    "MK11"    "MK12"    "MK13"    "MK14"   
#[1241] "MK15"    "MK16"    "MK17"    "MK18"    "MK19"    "MK20"    "MK21"    "MK22"   
#[1249] "MK23"    "MK24"    "MK25"    "MK26"    "MK27"    "MK28"    "MK29"    "MK30"   
#[1257] "MK31"    "MK32"    "MK33"    "MK34"    "MK35"    "MK36"    "MK37"    "MK38"   
#[1265] "MK39"    "MK40"    "MK41"    "MK42"    "MK43"    "MK44"    "MK45"    "MK46"   
#[1273] "MK47"    "MK48"    "MK49"    "MK50"    "MK51"    "MK52"    "MK53"    "MK54"   
#[1281] "MK55"    "MK56"    "MK57"    "MK58"    "MK59"    "MK60"    "MK61"    "MK62"   
#[1289] "MK63"    "MK64"    "MK65"    "MK66"    "MK67"    "MK68"    "MK69"    "MK70"   
#[1297] "MK71"    "MK72"    "MK73"    "MK74"    "MK75"    "MK76"    "MK77"    "MK78"   
#[1305] "MK79"    "MK80"    "MK81"    "MK82"    "MK83"    "MK84"
#Monaco: "MC" 
#Montenegro: "ME01"    "ME02"    "ME03"   
#[1209] "ME04"    "ME05"    "ME06"    "ME07"    "ME08"    "ME09"    "ME10"    "ME11"   
#[1217] "ME12"    "ME13"    "ME14"    "ME15"    "ME16"    "ME17"    "ME18"    "ME19"   
#[1225] "ME20"    "ME21"
#Moldova: "MDAN"    "MDBA"    "MDBD"    "MDBR"    "MDBS"    "MDCA"    "MDCL"    "MDCM"   
#[1177] "MDCR"    "MDCS"    "MDCT"    "MDCU"    "MDDO"    "MDDR"    "MDDU"    "MDED"   
#[1185] "MDFA"    "MDFL"    "MDGA"    "MDGL"    "MDHI"    "MDIA"    "MDLE"    "MDNI"   
#[1193] "MDOC"    "MDOR"    "MDRE"    "MDRI"    "MDSD"    "MDSI"    "MDSN"    "MDSO"   
#[1201] "MDST"    "MDSV"    "MDTA"    "MDTE"    "MDUN"
#Netherlands: "NL11" 
#[1313] "NL12"    "NL13"    "NL21"    "NL22"    "NL23"    "NL31"    "NL32"    "NL33"   
#[1321] "NL34"    "NL41"    "NL42"
#Poland: "PL01"    "PL02"   
#[1345] "PL03"    "PL04"    "PL05"    "PL06"    "PL07"    "PL08"    "PL09"    "PL0A"   
#[1353] "PL0B"    "PL0C"    "PL0D"    "PL0E"    "PL0F"    "PL0G" 
#Portugal: "PT111"   "PT112"  
#[1361] "PT113"   "PT114"   "PT115"   "PT116"   "PT117"   "PT118"   "PT121"   "PT122"  
#[1369] "PT123"   "PT124"   "PT125"   "PT126"   "PT127"   "PT128"   "PT129"   "PT12A"  
#[1377] "PT131"   "PT132"   "PT133"   "PT134"   "PT135"   "PT141"   "PT142"   "PT143"  
#[1385] "PT144"   "PT15"    "PT2"     "PT3"
#Romania: "RO011"   "RO012"   "RO013"   "RO014"  
#[1393] "RO015"   "RO016"   "RO021"   "RO022"   "RO023"   "RO024"   "RO025"   "RO026"  
#[1401] "RO031"   "RO032"   "RO033"   "RO034"   "RO035"   "RO036"   "RO037"   "RO041"  
#[1409] "RO042"   "RO043"   "RO044"   "RO045"   "RO051"   "RO052"   "RO053"   "RO054"  
#[1417] "RO061"   "RO062"   "RO063"   "RO064"   "RO065"   "RO066"   "RO071"   "RO072"  
#[1425] "RO073"   "RO074"   "RO075"   "RO076"   "RO081"   "RO082"
#Serbia: "RS00"    "RS01"   
#[1433] "RS02"    "RS03"    "RS04"    "RS05"    "RS06"    "RS07"    "RS08"    "RS09"   
#[1441] "RS10"    "RS11"    "RS12"    "RS13"    "RS14"    "RS15"    "RS16"    "RS17"   
#[1449] "RS18"    "RS19"    "RS20"    "RS21"    "RS22"    "RS23"    "RS24"
#Slovakia: "SK01"   
#[1545] "SK021"   "SK022"   "SK023"   "SK031"   "SK032"   "SK041"   "SK042"
#Slovenia: "SI001"   "SI002"   "SI003"   "SI004_1" "SI005_1"
#[1537] "SI006_1" "SI007_1" "SI008_1" "SI009"   "SI00A"   "SI00B"   "SI00C" 
#Spain: "ES111"   "ES112"   "ES113"  
#[625] "ES114"   "ES12"    "ES13"    "ES211"   "ES212"   "ES213"   "ES22"    "ES23"   
#[633] "ES241"   "ES242"   "ES243"   "ES3"     "ES411"   "ES412"   "ES413"   "ES414"
#[641] "ES415"   "ES416"   "ES417"   "ES418"   "ES419"   "ES421"   "ES422"   "ES423"  
#[649] "ES424"   "ES425"   "ES431"   "ES432"   "ES511"   "ES512"   "ES513"   "ES514"  
#[657] "ES521"   "ES522"   "ES523"   "ES53"    "ES611"   "ES612"   "ES613"   "ES614"  
#[665] "ES615"   "ES616"   "ES617"   "ES618"   "ES62"    "ES63"    "ES64"    "ES701"  
#[673] "ES702" 
#Ukraine: "UA05"    "UA07"    "UA09"    "UA12"    "UA14"    "UA18"    "UA21"   
#[1641] "UA23"    "UA26"    "UA30"    "UA32"    "UA35"    "UA40"    "UA43"    "UA46"   
#[1649] "UA48"    "UA51"    "UA53"    "UA56"    "UA59"    "UA61"    "UA63"    "UA65"   
#[1657] "UA68"    "UA71"    "UA74"    "UA77" 
#Russia: "RU02B"  
#[1457] "RU043"   "RU044"   "RU045"   "RU047"   "RU04A"   "RU04D"   "RU04E"   "RU1103" 
#[1465] "RU1107"  "RU1110"  "RU1111A" "RU1112"  "RU1114"  "RU1115"  "RU1117"  "RU1118" 
#[1473] "RU1119"  "RU1120"  "RU1122"  "RU1124"  "RU1127"  "RU1128"  "RU1129"  "RU1134" 
#[1481] "RU1138"  "RU1140"  "RU1141"  "RU1142"  "RU1145"  "RU1146"  "RU1147"  "RU1149" 
#[1489] "RU1154"  "RU1156"  "RU1158"  "RU1160"  "RU1161"  "RU1163"  "RU1166"  "RU1168" 
#[1497] "RU1170"  "RU1173"  "RU1178"  "RU1179"  "RU1182"  "RU1183"  "RU1185"  "RU1186" 
#[1505] "RU1188"  "RU1189"  "RU1190"  "RU1191"  "RU1196"  "RU1197"


# Subset the regions of the countries of interest only
region1 <- subset(region, RegionCode %in% c("AL001","AL002", "AL003", "AL004", "AL005",
                                            "AL006", "AL007", "AL008", "AL009", "AL010",
                                            "AL011", "AL012", "AT111", "AT112", "AT113",  
                                            "AT121", "AT122", "AT123", "AT124", "AT125",
                                            "AT126", "AT127", "AT13",  "AT211", "AT212",
                                            "AT213", "AT221", "AT222", "AT223", "AT224",
                                            "AT225", "AT226", "AT311", "AT312", "AT313",
                                            "AT314", "AT315", "AT321", "AT322", "AT323",
                                            "AT331", "AT332", "AT333", "AT334", "AT335",
                                            "AT341",  "AT342", "BY001", "BY002", "BY003",
                                            "BY004",   "BY005",  "BY006",   
                                            "BE1", "BE211", "BE212", "BE213", "BE221", 
                                            "BE222", "BE223", "BE231", "BE232", "BE233",
                                            "BE234", "BE235", "BE236", "BE241", "BE242",
                                            "BE251", "BE252", "BE253", "BE254", "BE255",
                                            "BE256", "BE257", "BE258", "BE31", "BE321", 
                                            "BE322", "BE323", "BE324", "BE325", "BE326",  
                                            "BE327", "BE331", "BE332", "BE333", "BE334", 
                                            "BE341", "BE342", "BE343", "BE344", "BE345", 
                                            "BE351", "BE352", "BE353",
                                            "BG1", "BG201", "BG202", "BG203", "BG204", 
                                            "BG205", "BG206", "BG207", "BG208", "BG209",
                                            "BG20A", "BG20B", "BG20C", "BG20D", "BG20E",
                                            "BG301", "BG302", "BG303", "BG304", "BG305",
                                            "BG306", "BG307", "BG308", "BG309", "BG30A",
                                            "BG30B", "BG30C", "BG30D", 
                                            "HR001", "HR002", "HR003", "HR004", "HR005",
                                            "HR006", "HR007", "HR008", "HR009", "HR010",  
                                            "HR011", "HR012", "HR013", "HR014", "HR015",
                                            "HR016", "HR017", "HR018", "HR019", "HR020",
                                            "HR021", 
                                            "CZ01", "CZ02", "CZ031", "CZ032", "CZ041",
                                            "CZ042", "CZ051", "CZ052", "CZ053", "CZ063",
                                            "CZ064", "CZ071_1", "CZ072", "CZ080",
                                            "EE0037", "EE0039", "EE0044", "EE0049", 
                                            "EE0051", "EE0057", "EE0059", "EE0065", 
                                            "EE0067", "EE0070", "EE0074", "EE0078", 
                                            "EE0082", "EE0084", "EE0086",
                                            "FI131", "FI132", "FI133", "FI134", "FI141",
                                            "FI142", "FI143", "FI144", "FI151", "FI152",
                                            "FI161", "FI162", "FI171", "FI172", "FI173",  
                                            "FI174", "FI175", "FI176", "FI177", "FI2",
                                            "FR1", "FR21", "FR22",  "FR23", "FR24", "FR25",
                                            "FR26", "FR3", "FR41",  "FR42", "FR43", "FR51",
                                            "FR52", "FR53", "FR61", "FR62", "FR63", "FR71",   
                                            "FR72", "FR81", "FR82", "FR83", "FR91", "FR92",
                                            "FR93", "FR94", "FR95", 
                                            "DE111", "DE112", "DE113", "DE114", "DE115",
                                            "DE116", "DE117", "DE118", "DE119", "DE11A", 
                                            "DE11B", "DE11C", "DE11D", "DE121", "DE122",
                                            "DE123", "DE124", "DE125", "DE126", "DE127",
                                            "DE128", "DE129", "DE12A", "DE12B", "DE12C",
                                            "DE131", "DE132", "DE133", "DE134", "DE135",
                                            "DE136", "DE137", "DE138", "DE139", "DE13A",
                                            "DE141", "DE142", "DE143", "DE144", "DE145",
                                            "DE146", "DE147", "DE148", "DE149", "DE211",
                                            "DE212", "DE213", "DE214", "DE215", "DE216",
                                            "DE217", "DE218", "DE219", "DE21A", "DE21B",
                                            "DE21C", "DE21D", "DE21E", "DE21F", "DE21G",
                                            "DE21H", "DE21I", "DE21J", "DE21K", "DE21L",
                                            "DE21M", "DE21N", "DE221", "DE222", "DE223",
                                            "DE224", "DE225", "DE226", "DE227", "DE228",
                                            "DE229", "DE22A", "DE22B", "DE22C", "DE231",
                                            "DE232", "DE233", "DE234", "DE235", "DE236",
                                            "DE237", "DE238", "DE239", "DE23A", "DE241",
                                            "DE242", "DE243", "DE244", "DE245", "DE246",
                                            "DE247", "DE248", "DE249", "DE24A", "DE24B",
                                            "DE24C", "DE24D", "DE251", "DE252", "DE253",
                                            "DE254", "DE255", "DE256", "DE257", "DE258",
                                            "DE259", "DE25A", "DE25B", "DE25C", "DE261", 
                                            "DE262", "DE263", "DE264", "DE265", "DE266",
                                            "DE267", "DE268", "DE269", "DE26A", "DE26B",
                                            "DE26C", "DE271", "DE272", "DE273", "DE274",
                                            "DE275", "DE276", "DE277", "DE278", "DE279",
                                            "DE27A", "DE27B", "DE27C", "DE27D", "DE27E",
                                            "DE300", "DE401_1", "DE402", "DE403", "DE404_1",
                                            "DE405", "DE406", "DE407", "DE408_1", "DE409",
                                            "DE40A", "DE40B", "DE40C", "DE40D", "DE40E_1",
                                            "DE40F", "DE40G", "DE40H", "DE40I", "DE501",
                                            "DE502", "DE6", "DE711", "DE712", "DE713", 
                                            "DE714", "DE715", "DE716", "DE717", "DE718", 
                                            "DE719", "DE71A", "DE71B", "DE71C", "DE71D",
                                            "DE71E", "DE721", "DE722", "DE723", "DE724",
                                            "DE725", "DE731", "DE732", "DE733", "DE734",
                                            "DE735", "DE736", "DE737", "DE803", "DE804",
                                            "DE80J", "DE80K", "DE80L", "DE80M", "DE80N",
                                            "DE80O", "DE911", "DE912", "DE913", "DE914",
                                            "DE915", "DE916", "DE917", "DE918", "DE919",
                                            "DE91A", "DE91B", "DE922", "DE923", "DE925",
                                            "DE926", "DE927", "DE928", "DE929", "DE931",
                                            "DE932", "DE933", "DE934", "DE935", "DE936",
                                            "DE937", "DE938", "DE939", "DE93A", "DE93B", 
                                            "DE941", "DE942", "DE943", "DE944", "DE945",
                                            "DE946", "DE947", "DE948", "DE949", "DE94A",
                                            "DE94B", "DE94C", "DE94D", "DE94E", "DE94F", 
                                            "DE94G", "DE94H", "DEA11", "DEA12", "DEA13", 
                                            "DEA14", "DEA15", "DEA16", "DEA17", "DEA18",
                                            "DEA19", "DEA1A", "DEA1B", "DEA1C", "DEA1D",
                                            "DEA1E", "DEA1F", "DEA22", "DEA23", "DEA24",
                                            "DEA26", "DEA27", "DEA28", "DEA29", "DEA2A",
                                            "DEA2B", "DEA2C", "DEA2D", "DEA31", "DEA32", 
                                            "DEA33", "DEA34", "DEA35", "DEA36", "DEA37",
                                            "DEA38", "DEA41", "DEA42", "DEA43", "DEA44", 
                                            "DEA45", "DEA46", "DEA47", "DEA51", "DEA52", 
                                            "DEA53", "DEA54", "DEA55", "DEA56", "DEA57",
                                            "DEA58", "DEA59", "DEA5A", "DEA5B", "DEA5C",
                                            "DEB11", "DEB12", "DEB13", "DEB14", "DEB15",
                                            "DEB16", "DEB17", "DEB18", "DEB19", "DEB1A",
                                            "DEB1B", "DEB21", "DEB22", "DEB23", "DEB24",
                                            "DEB25", "DEB31", "DEB32", "DEB33", "DEB34",
                                            "DEB35", "DEB36", "DEB37", "DEB38", "DEB39",
                                            "DEB3A", "DEB3B", "DEB3C", "DEB3D", "DEB3E",
                                            "DEB3F", "DEB3G", "DEB3H", "DEB3I", "DEB3J",
                                            "DEB3K", "DEC01", "DEC02", "DEC03", "DEC04",
                                            "DEC05", "DEC06", "DED21", "DED2C", "DED2D", 
                                            "DED2E", "DED2F", "DED41", "DED42", "DED43",
                                            "DED44", "DED45", "DED51", "DED52", "DED53",
                                            "DEE01", "DEE02", "DEE03", "DEE04", "DEE05", 
                                            "DEE06", "DEE07", "DEE08", "DEE09", "DEE0A", 
                                            "DEE0B", "DEE0C", "DEE0D", "DEE0E", "DEF01", 
                                            "DEF02", "DEF03", "DEF04", "DEF05", "DEF06",
                                            "DEF07", "DEF08", "DEF09", "DEF0A", "DEF0B",
                                            "DEF0C", "DEF0D", "DEF0E", "DEF0F", "DEG01", 
                                            "DEG02", "DEG03", "DEG04", "DEG05", "DEG06", 
                                            "DEG07", "DEG09", "DEG0A", "DEG0B", "DEG0C",
                                            "DEG0D", "DEG0E", "DEG0F", "DEG0G", "DEG0H",
                                            "DEG0I", "DEG0J", "DEG0K", "DEG0L", "DEG0M",
                                            "DEG0N", "DEG0P",
                                            "GR111", "GR112", "GR113", "GR114", "GR115", 
                                            "GR121", "GR122", "GR123", "GR124", "GR125",  
                                            "GR126", "GR127", "GR131", "GR132", "GR133", 
                                            "GR134", "GR141", "GR142", "GR143", "GR144", 
                                            "GR211", "GR212", "GR213", "GR214", "GR221", 
                                            "GR222", "GR223", "GR224", "GR231", "GR232",
                                            "GR233", "GR241", "GR242", "GR243", "GR244", 
                                            "GR245", "GR251", "GR252", "GR253", "GR254",
                                            "GR255", "GR3", "GR411", "GR412", "GR413", 
                                            "GR421", "GR422", "GR431", "GR432", "GR433",
                                            "GR434",
                                            "HU011", "HU012", "HU021", "HU022", "HU023",
                                            "HU031", "HU032", "HU033", "HU041", "HU042",
                                            "HU043", "HU051", "HU052", "HU053", "HU061",
                                            "HU062", "HU063", "HU071", "HU072", "HU073",
                                            "IT111", "IT112", "IT113", "IT114", "IT115", 
                                            "IT116", "IT117", "IT118", "IT12", "IT131",
                                            "IT132", "IT133", "IT134", "IT201", "IT202",
                                            "IT203", "IT204", "IT205", "IT206", "IT207",
                                            "IT208", "IT209", "IT20A", "IT20B", "IT311", 
                                            "IT312", "IT321", "IT322", "IT323", "IT324",
                                            "IT325", "IT326", "IT327", "IT331", "IT332", 
                                            "IT333", "IT334", "IT401", "IT402", "IT403",
                                            "IT404", "IT405", "IT406", "IT407", "IT408",
                                            "IT409", "IT511", "IT512", "IT513", "IT514", 
                                            "IT515", "IT516", "IT517", "IT518", "IT519", 
                                            "IT51A", "IT521", "IT522", "IT531", "IT532", 
                                            "IT533", "IT534", "IT601", "IT602", "IT603",
                                            "IT604", "IT605", "IT711", "IT712", "IT713",
                                            "IT714", "IT721", "IT722", "IT801", "IT802",
                                            "IT803", "IT804", "IT805", "IT911", "IT912",
                                            "IT913", "IT914", "IT915", "IT921", "IT922",
                                            "IT931", "IT932", "IT933", "IT934", "IT935",
                                            "ITA01", "ITA02", "ITA03", "ITA04", "ITA05", 
                                            "ITA06", "ITA07", "ITA08", "ITA09", "ITB01", 
                                            "ITB02", "ITB03", "ITB04",
                                            #Kosovo: no kosovo
                                            "LV001_1", "LV002_1", "LV003_1", "LV004_1",
                                            "LV005_1", "LV006", "LV007", "LV008", "LV009",
                                            "LV010", "LV011", "LV012", "LV013", "LV014",
                                            "LV015", "LV016", "LV017", "LV018", "LV019",
                                            "LV020", "LV021", "LV022", "LV023", "LV024",
                                            "LV025", "LV026", "LV027", "LV028", "LV029",
                                            "LV030", "LV031", "LV032", "LV033", "LV034",
                                            "LV035", "LV036", "LV037", "LV038", "LV039",
                                            "LV040", "LV041", "LV042", "LV043", "LV044",
                                            "LV045", "LV046", "LV047", "LV048", "LV049", 
                                            "LV050", "LV051", "LV052", "LV053", "LV054", 
                                            "LV055", "LV056", "LV057", "LV058", "LV059",
                                            "LV060", "LV061", "LV062", "LV063", "LV064",
                                            "LV065", "LV066", "LV067", "LV068", "LV069", 
                                            "LV070", "LV071", "LV072", "LV073", "LV074", 
                                            "LV075", "LV076", "LV077", "LV078", "LV079", 
                                            "LV080", "LV081", "LV082", "LV083", "LV084",
                                            "LV085", "LV086", "LV087", "LV088", "LV089", 
                                            "LV090", "LV091", "LV092", "LV093", "LV094", 
                                            "LT001", "LT002", "LT003", "LT004", "LT005",  
                                            "LT006", "LT007", "LT008", "LT009", "LT00A",
                                            "LU",
                                            "MK01", "MK02", "MK03", "MK04", "MK05", "MK06",
                                            "MK07", "MK08", "MK09", "MK10", "MK11", "MK12",
                                            "MK13", "MK14", "MK15", "MK16", "MK17", "MK18",
                                            "MK19", "MK20", "MK21", "MK22", "MK23", "MK24",
                                            "MK31", "MK32", "MK33", "MK34", "MK35", "MK36",
                                            "MK37", "MK38", "MK39", "MK40", "MK41", "MK42",
                                            "MK43", "MK44", "MK45", "MK46", "MK47", "MK48",
                                            "MK49", "MK50", "MK51", "MK52", "MK53", "MK54",
                                            "MK55", "MK56", "MK57", "MK58", "MK59", "MK60",
                                            "MK61", "MK62", "MK63", "MK64", "MK65", "MK66",
                                            "MK67", "MK68", "MK69", "MK70", "MK71", "MK72",
                                            "MK79", "MK80", "MK81", "MK82", "MK83", "MK84",
                                            "MC",
                                            "ME01", "ME02", "ME03",
                                            "ME04", "ME05", "ME06", "ME07", "ME08", "ME09", 
                                            "ME10", "ME11", "ME12", "ME13", "ME14", "ME15",
                                            "ME16", "ME17", "ME18", "ME19", "ME20", "ME21",
                                            "MDAN", "MDBA", "MDBD", "MDBR", "MDBS", "MDCA",
                                            "MDCL", "MDCM", "MDCR", "MDCS", "MDCT", "MDCU",
                                            "MDDO", "MDDR", "MDDU", "MDED", "MDFA", "MDFL",
                                            "MDGA", "MDGL", "MDHI", "MDIA", "MDLE", "MDNI",
                                            "MDOC", "MDOR", "MDRE", "MDRI", "MDSD", "MDSI",
                                            "MDSN", "MDSO", "MDST", "MDSV", "MDTA", "MDTE",
                                            "MDUN",
                                            "NL11", "NL12", "NL13", "NL21", "NL22", "NL23",
                                            "NL31", "NL32", "NL33", "NL34", "NL41", "NL42",
                                            "PL01", "PL02", "PL03", "PL04", "PL05", "PL06",
                                            "PL07", "PL08", "PL09", "PL0A", "PL0B", "PL0C",
                                            "PL0D", "PL0E", "PL0F", "PL0G", "PT111", "PT112",
                                            "PT113", "PT114", "PT115", "PT116", "PT117", 
                                            "PT118", "PT121", "PT122", "PT123", "PT124", 
                                            "PT125", "PT126", "PT127", "PT128", "PT129", 
                                            "PT12A", "PT131", "PT132", "PT133", "PT134", 
                                            "PT135", "PT141", "PT142", "PT143", "PT144",
                                            "PT15", "PT2", "PT3",
                                            "RO011", "RO012", "RO013", "RO014", "RO015", 
                                            "RO016", "RO021", "RO022", "RO023", "RO024", 
                                            "RO025", "RO026", "RO031", "RO032", "RO033", 
                                            "RO034", "RO035", "RO036", "RO037", "RO041", 
                                            "RO042", "RO043", "RO044", "RO045", "RO051", 
                                            "RO052", "RO053", "RO054", "RO061", "RO062",
                                            "RO063", "RO064", "RO065", "RO066", "RO071",
                                            "RO072", "RO073", "RO074", "RO075", "RO076", 
                                            "RO081", "RO082",
                                            "RS00", "RS01", "RS02", "RS03", "RS04", "RS05",
                                            "RS06", "RS07", "RS08", "RS09", "RS10", "RS11",
                                            "RS12", "RS13", "RS14", "RS15", "RS16", "RS17",
                                            "RS18", "RS19", "RS20", "RS21", "RS22", "RS23",
                                            "RS24",
                                            "SK01", "SK021", "SK022", "SK023", "SK031", 
                                            "SK032", "SK041", "SK042", 
                                            "SI001", "SI002", "SI003", "SI004_1", "SI005_1", 
                                            "SI006_1", "SI007_1", "SI008_1", "SI009", 
                                            "SI00A", "SI00B", "SI00C", 
                                            "ES111", "ES112", "ES113", "ES114", "ES12", 
                                            "ES13", "ES211", "ES212", "ES213", "ES22", 
                                            "ES23", "ES241", "ES242", "ES243", "ES3", 
                                            "ES411", "ES412", "ES413", "ES414", "ES415",
                                            "ES416", "ES417", "ES418", "ES419", "ES421",
                                            "ES422", "ES423", "ES424", "ES425", "ES431", 
                                            "ES432", "ES511", "ES512", "ES513", "ES514", 
                                            "ES521", "ES522", "ES523", "ES53",  "ES611",
                                            "ES612", "ES613", "ES614", "ES615", "ES616",
                                            "ES617", "ES618", "ES62", "ES63", "ES64", 
                                            "ES701", "ES702",
                                            "UA05", "UA07", "UA09", "UA12", "UA14", "UA18",
                                            "UA21", "UA23", "UA26", "UA30", "UA32", "UA35",
                                            "UA40", "UA43", "UA46", "UA48", "UA51", "UA53",
                                            "UA56", "UA59", "UA61", "UA63", "UA65", "UA68",
                                            "UA71", "UA74", "UA77",
                                            "RU02B", "RU043", "RU044", "RU045", "RU047",
                                            "RU04A", "RU04D", "RU04E", "RU1103", "RU1107",
                                            "RU1110", "RU1111A", "RU1112", "RU1114", 
                                            "RU1115", "RU1117", "RU1118", "RU1119", 
                                            "RU1120", "RU1122", "RU1124", "RU1127",
                                            "RU1128", "RU1129", "RU1134", "RU1138",
                                            "RU1140", "RU1141", "RU1142", "RU1145",
                                            "RU1146", "RU1147", "RU1149", "RU1154",
                                            "RU1156", "RU1158", "RU1160", "RU1161",
                                            "RU1163", "RU1166", "RU1168", "RU1170", 
                                            "RU1173", "RU1178", "RU1179", "RU1182", 
                                            "RU1183", "RU1185", "RU1186", "RU1188", 
                                            "RU1189", "RU1190", "RU1191", "RU1196", 
                                            "RU1197"))

## Check the new shapefila
region1$RegionCode <- factor(region1$RegionCode)
str(region1@data) # $ ID: Factor w/ 23 levels
# 'data.frame':	1307 obs. of  4 variables:
#$ RegionCode: Factor w/ 1307 levels "AL001","AL002",..: 5 90 1 2 3 4 29 30 31 6 ...
#$ Name      : Factor w/ 1792 levels "* no region specified",..: 525 1188 159 410 432 467 1139 1141 1724 792 ...
#$ Disabled  : int  0 0 0 0 0 0 0 0 0 0 ...
#$ ESRI_OID  : int  3 4 5 6 7 8 9 10 11 14 ..

## Plot the new shapefile
png("output/region_actual.png", width=5, height=5, units="in", res=300)
plot(region1, main="Selected countries")
dev.off()

## Save it
writeOGR(region1, dsn=".", layer="output/region_actual_selection", driver="ESRI Shapefile")
