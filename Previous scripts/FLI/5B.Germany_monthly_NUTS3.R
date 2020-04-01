
##'**LAURIE'S PROJECT**
##'**FOX RABIES DYNAMIC**
##'**MICAELA DE LA PUENTE L.**        
##'**This script creates a csv of german data by month using nuts 3 shapefile** 
##########################################################################

library(rgdal)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(rgeos)
library(maptools)
library(raster)
library(dplyr)
library(rgdal)
library(rgeos)
library(car)
library(raster)
library(tidyr) # 
library(mefa) # this is for rep() function
library(Matrix)
require(reshape2)
library(foreign)
library(sp)
library(ggthemes)
library(ggalt)
library(scales)
library(viridis)
library(tmap)

##########################################################################


#### ADD  THE NUTS 3 UNITS ####

### To German dataset only ###
ger_cases <- readOGR("output/german_subset_germany.shp","german_subset_germany") 
names(ger_cases)
length(sort(unique(ger_cases$GEB_NAME), decreasing=FALSE)) # 2600 units

## Extract ub QGIS the units according to coordinates to avoid spelling problems.
# We have a way now to do it in R with XXXXXXXXX function.
nuts3_unit <-readOGR("output/nuts3_unit.shp", "nuts3_unit") # done in qgis
names(nuts3_unit) # [1] "Name_ASCII"
str(nuts3_unit@data) # 'data.frame':18081 obs. of  1 variable:
# Factor w/ 158 levels 

# Add to the germany data
ger_cases1 <- ger_cases
ger_cases1@data <- cbind(ger_cases1@data, nuts3_unit@data)
names(ger_cases1) # [1] "JAHR"       "GEB_NAME"   "DATUM"      "MONTH"      "Name_ASCII"
head(ger_cases1) 
# JAHR    GEB_NAME      DATUM MONTH             Name_ASCII
# 1 1982      Wismar 1982/01/15    01              SK Wismar
# 2 1982        Binz 1982/01/15    01              LK Ruegen
# 3 1982   Dierhagen 1982/01/15    01      LK Nordvorpommern
# 4 1982   Bernstorf 1982/01/15    01 LK Nordwestmecklenburg
# 5 1982       Eixen 1982/01/15    01      LK Nordvorpommern
# 6 1982 Carinerland 1982/01/15    01         LK Bad Doberan

# Turn into a df
df <- as.data.frame(ger_cases1)
df1 <- df[,c(1,2,4,5)]
colnames(df1)[1] <- "YEAR"
colnames(df1)[2] <- "ADM_UNIT"
colnames(df1)[3] <- "MONTH"
colnames(df1)[4] <- "NUTS3"

# Create the compiled dataframe, per months
# German cases from 1982 to 2007
ger_cases_monthly <- as.data.frame(table(df1$NUTS3, df1$YEAR, df1$MONTH))

# Correct column names
colnames(ger_cases_monthly)[1] <- "NUTS3"
colnames(ger_cases_monthly)[2] <- "YEAR"
colnames(ger_cases_monthly)[3] <- "MONTH"
colnames(ger_cases_monthly)[4] <- "COUNT"

# Create a new column with year and month together (ID)
ger_cases_monthly$ID <- with(ger_cases_monthly, paste0(YEAR, "M", MONTH))
names(ger_cases_monthly) # [1] "NUTS3" "YEAR"  "MONTH" "COUNT" "ID" 
# Tidy the dataset
ger_cases_monthly <- ger_cases_monthly[,c(1,5,2:4)] 


### To European dataset only ####
clip_eu_cases <- readOGR("output/clip_european_subset_germany.shp",
                         "clip_european_subset_germany")  # from QGIS
names(clip_eu_cases)
# [1] "YEAR"      "QUARTER"   "COUNTRY"   "REGION"    "SPECIES"   "IMPORTED"  "coords_x1"
# [8] "coords_x2"
length(sort(unique(clip_eu_cases$REGION), decreasing=FALSE)) # 215 units

## Extract ub QGIS the units according to coordinates to avoid spelling problems.
nuts3_unit_clip <-readOGR("output/nuts3_unit_clip.shp", "nuts3_unit_clip") # done in qgis
names(nuts3_unit_clip) # [1] "Name_ASCII"
str(nuts3_unit_clip@data) # 'data.frame':5423 obs. of  1 variable:
# Factor w/ 201 levels 

# Add to the germany data
clip_eu_cases1 <- clip_eu_cases
clip_eu_cases1@data <- cbind(clip_eu_cases1@data, nuts3_unit_clip@data)
names(clip_eu_cases1)
head(clip_eu_cases1,3) 
# YEAR QUARTER COUNTRY     REGION SPECIES IMPORTED coords_x1 coords_x2    Name_ASCII
# 1 1990       1 Germany   Stormarn     fox        0 10.420389  53.79435   LK Stormarn
# 2 1990       1 Germany Euskirchen     fox        0  6.442152  50.38917 LK Euskirchen
# 3 1990       1 Germany     Hexter     fox        0  9.194825  51.67099    LK Hoexter

# Turn into a df
df_clip <- as.data.frame(clip_eu_cases1)
df1_clip <- df_clip[,c(1,4,2,9)]
colnames(df1_clip)[2] <- "ADM_UNIT"
colnames(df1_clip)[4] <- "NUTS3"

# Find out number of cases per quarter
clip_cases_quarters <- as.data.frame(table(df1_clip$NUTS3,
                                           df1_clip$YEAR,
                                           df1_clip$QUARTER))

# Change names
colnames(clip_cases_quarters)[1] <- "NUTS3"
colnames(clip_cases_quarters)[2] <- "YEAR"
colnames(clip_cases_quarters)[3] <- "QUARTER"
colnames(clip_cases_quarters)[4] <- "COUNT"

# Add ID for years plus quarters
clip_cases_quarters$ID_Q <- with(clip_cases_quarters, paste0(YEAR, "Q", QUARTER))

# Copy each row 2 times, for each to appear 3 times.
clip_cases_quarters <- clip_cases_quarters[rep(seq_len(nrow(clip_cases_quarters)), 
                                               each=3),]

## Add a month column
# Correlative numbers from 1 to 12 are added to the dataframe.
time.i <- sort(unique(clip_cases_quarters$ID_Q), decreasing = FALSE)
unit.i <- sort(unique(clip_cases_quarters$NUTS3), decreasing = FALSE)

library(dplyr) # sometimes is needed to upload the package again

clip.final <- list()

for(j in unit.i){
  clip.i <- subset(clip_cases_quarters, NUTS3 == j) # subset each unit
  clip.i$NUTS3 <- factor(clip.i$NUTS3)
  str(clip.i)
  
  clip.i <- clip.i[order(clip.i$ID_Q),] # tidy subset by ID of year plus quarter
  
  length.ID_Q <- length(unique(clip.i$YEAR)) # find out the number of years in subset
  c <- c(1:12) # create a vector with numbers from 1 to 12
  d <- rep(c, length.ID_Q) # Create a vector repeating c by the number of years.
  
  clip.i$MONTH <- d # add columna with months
  clip.i$MONTH <- as.factor(clip.i$MONTH)
  #clip.i$MONTH1 <- recode(clip.i$MONTH, "'1'='01'; '2'='02'; '3'='03'; 
  #                       '4'='04';'5'='05'; '6'='06';'7'='07'; '8'='08'; 
  #                       '9'='09'; '10'='10'; '11'='11'; '12'='12'")
  # This method works with car package.
  # Sometime previous recode does not work (Error: All replacements must be named)
  # Then, use the following:
  clip.i$MONTH <- recode(clip.i$MONTH, `1` = "01", `2` = "02", `3` = "03", 
                         `4` = "04", `5` = "05", `6` = "06", `7` = "07",
                         `8` = "08", `9` = "09")
  
  clip.i <- clip.i[,c(1,2,6,4)] # eliminate quarters
  clip.i$ID <- with(clip.i, paste0(YEAR, "M", MONTH)) # create ID for year plus month.
  clip.final[[j]] <- clip.i[,c(1,5,2:4)]
}

# Create the df
clip.df <- do.call("rbind", lapply(clip.final, as.data.frame))
# Divide the number of cases in months
clip.df$COUNT <- round(clip.df$COUNT/3, digits = 0) 


##
## Check both dfs
str(ger_cases_monthly)
# 'data.frame':	47400 obs. of  5 variables:
#$ NUTS3: Factor w/ 158 levels "LK Alb-Donau-Kreis",..: 1 2 3 4 5 6 7 8 9 10 ...
#$ ID   : chr  "1982M01" "1982M01" "1982M01" "1982M01" ...
#$ YEAR : Factor w/ 25 levels "1982","1983",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ MONTH: Factor w/ 12 levels "01","02","03",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ COUNT: int  0 0 0 1 0 6 0 0 1 1 ...
ger_cases_monthly$COUNT <- as.numeric(ger_cases_monthly$COUNT)

str(clip.df)
# 'data.frame':	16884 obs. of  5 variables:
#$ NUTS3: Factor w/ 201 levels "LK Alb-Donau-Kreis",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ ID   : chr  "1990M1" "1990M2" "1990M3" "1990M4" ...
#$ YEAR : Factor w/ 7 levels "1990","1991",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ MONTH: Factor w/ 12 levels "01","02","03",..: 1 2 3 4 5 6 7 8 9 10 ...
#$ COUNT: num  1 1 1 1 1 1 1 1 1 2 ...
## Both dataframes look OK.


##### Merge the two datasets ######
tmp <- merge(ger_cases_monthly, clip.df, by = c("NUTS3", "ID"), all.x = TRUE, 
             all.y = TRUE)
tmp$COUNT.x[is.na(tmp$COUNT.x)]<-0
tmp$COUNT.y[is.na(tmp$COUNT.y)]<-0
tmp$COUNT <- tmp$COUNT.x+tmp$COUNT.y
tmp <- tmp[,c(1,2,9)]
head(tmp,3)
# NUTS3      ID COUNT
# 1 LK Alb-Donau-Kreis 1982M01     0
# 2 LK Alb-Donau-Kreis 1982M02     0
# 3 LK Alb-Donau-Kreis 1982M03     0

# Check number of units
length.unit <- length(unique(tmp$NUTS3)) # 307
length.df <- nrow(tmp) #  59916
length.df/length.unit # 195.1661. It is ok, 25 years per 12 months. it is not exact
# The ones that come from european dataset only have until 1990 to 1995 and 2000



### CREATE A MONTHLY DATA FRAME WITH ALL UNITS IN GERMANY ###
# This one is empty for COUNT column

# Call boundaries shapefile
nuts3_ger <-readOGR("output/nuts3_germany.shp", "nuts3_germany") # done in qgis
str(nuts3_ger@data) # 'data.frame':'data.frame':	429 obs. of  4 variables:
names(nuts3_ger)
# [1] "ID"         "Name"       "Name_ASCII" "CODE"
unit.i <- sort(unique(nuts3_ger$Name_ASCII), decreasing = FALSE) # 429

# Monthly cases shapefile
str(tmp)
# 'data.frame':	59916 obs. of  3 variables:
#$ NUTS3: Factor w/ 307 levels "LK Alb-Donau-Kreis",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ ID   : chr  "1982M01" "1982M02" "1982M03" "1982M04" ...
#$ COUNT: num  0 0 0 0 0 0 0 0 0 0 ...
names(tmp)
# [1] "NUTS3" "ID"    "COUNT"

# Divide ID to month and year
units_monthly <- tmp
units_monthly$YEAR <- substring(units_monthly$ID, 1,4)
units_monthly$MONTH <- substring(units_monthly$ID, 6,7)
str(units_monthly) # data.frame':	59916 obs. of  5 variables:
units_monthly$YEAR <- as.numeric(units_monthly$YEAR)
str(units_monthly)
# 'data.frame':	59916 obs. of  5 variables:
#$ NUTS3: Factor w/ 307 levels "LK Alb-Donau-Kreis",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ ID   : chr  "1982M01" "1982M02" "1982M03" "1982M04" ...
#$ COUNT: num  0 0 0 0 0 0 0 0 0 0 ...
#$ YEAR : num  1982 1982 1982 1982 1982 ...
#$ MONTH: chr  "01" "02" "03" "04" ...

# Add quarters
units_monthly$QUARTER <- units_monthly$MONTH 
library(dplyr) # upload again.
#units_monthly$QUARTER <- recode(units_monthly$QUARTER, "'01'='1'; '02'='1'; 
#                         '03'='1'; '04'='2';'05'='2'; '06'='2'; '07'='3'; 
#                                '08'='3'; '09'='3'")
#units_monthly$QUARTER <- recode(units_monthly$QUARTER, 
#                                "'10'='4'; '11'='4'; '12'='4'")
units_monthly$QUARTER <- recode(units_monthly$QUARTER, `01` = "1", `02` = "1", 
                                `03` = "1", 
                                `04` = "2", `05` = "2", `06` = "2", `07` = "3",
                                `08` = "3", `09` = "3", `10` = "4", `11` = "4", 
                                `12` = "4")

str(units_monthly) # 59916 obs. of  6 variables:
units_monthly$ID <- with(units_monthly, paste0(YEAR, "M", MONTH)) 
units_monthly <- units_monthly[,c(1,4,6,5,2,3)]
colnames(units_monthly)[1] <- "ADM_UNIT"
head(units_monthly, 3)
# ADM_UNIT YEAR QUARTER MONTH      ID COUNT
# 1 LK Alb-Donau-Kreis 1982       1    01 1982M01     0
# 2 LK Alb-Donau-Kreis 1982       1    02 1982M02     0
# 3 LK Alb-Donau-Kreis 1982       1    03 1982M03     0

units_monthly$MONTH <- as.numeric(units_monthly$MONTH)
units_monthly$QUARTER <- as.numeric(units_monthly$QUARTER)
str(units_monthly)
# 'data.frame':	59916 obs. of  6 variables:
#$ ADM_UNIT: Factor w/ 307 levels "LK Alb-Donau-Kreis",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ YEAR    : num  1982 1982 1982 1982 1982 ...
#$ QUARTER : num  "1" "1" "1" "2" ...
#$ MONTH   : num  1 2 3 4 5 6 7 8 9 10 ...
#$ ID      : chr  "1982M01" "1982M02" "1982M03" "1982M04" ...
#$ COUNT   : num  0 0 0 0 0 0 0 0 0 0 ...


### Create an empty dataframe
final_dataframe <- as.data.frame(unit.i)
colnames(final_dataframe)[1] <- "ADM_UNIT"
nunits <- length(final_dataframe$ADM_UNIT) # [1] 429

# Expand the data set
nyears<- length(1982:2006) # 25
nmonths<- length(1:12) # 12
nrow_perunit <- nyears*nmonths #300
nrows <- nrow_perunit*nunits # [1] 128700
final_dataframe1 <- rep(final_dataframe, 300)

# Add years
years <- as.data.frame(c(1982:2006))
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
str(final_dataframe1$MONTH) #  int [1:128700] 1 1 1 1 1 1 1 1 1 1 ...
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
str(final_dataframe1) # 128700 obs. of  5 variables:
final_dataframe1$YEAR <- as.numeric(final_dataframe1$YEAR)
final_dataframe1$QUARTER <- as.numeric(final_dataframe1$QUARTER)
final_dataframe1$MONTH <- as.numeric(final_dataframe1$MONTH)

# Add am empty column for count
final_dataframe1$COUNT <- NA
str(final_dataframe1) # OK now
# $ ADM_UNIT: Factor w/ 429 levels "LK Aachen","LK Ahrweiler",..: 1 2 3 4 5 6 7 8 9 10 ...
#$ YEAR    : num  1982 1982 1982 1982 1982 ...
#$ QUARTER : num  1 1 1 1 1 1 1 1 1 1 ...
#$ MONTH   : num  1 1 1 1 1 1 1 1 1 1 ...
#$ ID      : chr  "1982M01" "1982M01" "1982M01" "1982M01" ...
#$ COUNT   : logi  NA NA NA NA NA NA ...



##### Merge this empty dataframe with the cases dataframe

str(units_monthly)
tmp1 <- merge(final_dataframe1, units_monthly, by = c("ADM_UNIT", "ID"), all.x = TRUE, 
             all.y = TRUE)
tmp1 <- tmp1[,c(1:5,10)]
names(tmp1) # [1] "ADM_UNIT"  "ID"        "YEAR.x"    "QUARTER.x" "MONTH.x"   "COUNT.y" 
tmp1$COUNT.y[is.na(tmp1$COUNT.y)]<-0
colnames(tmp1)[3] <- "YEAR"
colnames(tmp1)[4] <- "QUARTER"
colnames(tmp1)[5] <- "MONTH"
colnames(tmp1)[6] <- "COUNT"
head(tmp1)
# #    ADM_UNIT      ID YEAR QUARTER MONTH COUNT
# 1 LK Aachen 1982M01 1982       1     1     0
# 2 LK Aachen 1982M02 1982       1     2     0
# 3 LK Aachen 1982M03 1982       1     3     0

# Write it
#write.csv(tmp1, file = "full_units_nuts_cases.csv")



###### PLOT MONTHLY MAPS #####

units_data <- nuts3_ger 
names(units_data) [1] "ID"         "Name"       "Name_ASCII" "CODE"     
units_data <- units_data[,c(3)]
units_data@data$id <- rownames(units_data@data)
names(units_data) # [1] "Name_ASCII" "id"        

vector <- sort(unique(tmp$ID), decreasing = FALSE)
length(vector) # [1] 300

#pdf("output/germany_monthly_map3.pdf", onefile = TRUE)
for(i in vector) {
  cases_month <- subset(tmp, ID == i)
  cases_month1 <- merge(units_data@data,cases_month,by.x="Name_ASCII",by.y="NUTS3") 
  # plus the id
  
  units_data.df <- fortify(units_data, by="id") # Regions defined for each Polygons
  # i am not sure what this does
  
  map.df <- join(units_data.df,cases_month1, by="id") # what about merge?????
  map.df$COUNT[is.na(map.df$COUNT)]<-0
  
  plot <- ggplot(map.df, color=COUNT) + 
    aes(long, lat, group = group, fill = COUNT) + 
    geom_polygon() + 
    geom_path(color="black") +
    coord_equal() + ggtitle(i) +
    scale_fill_gradient(low = "white", high = "blue", #guide = "legend"), 
                        limits=c(0, 33))  +
    theme_map() + 
    theme(legend.position="left") 
  plot(plot)
  ggsave(paste(i, "_cont", ".png", sep=""), dpi=300, width=6, height=5)
}
#dev.off()


#### discrete ###

#colnames(ger_cases_monthly)[1] <- "Name_ASCII"
for(i in vector) {
  cases_month <- subset(tmp, ID == i)
  cases_month1 <- merge(nuts3_ger@data,cases_month,by.x="Name_ASCII",
                        by.y="NUTS3")  # plus the id and Name
  #  cases_month2 <- rename(cases_month1, Name_ASCII=Name_ASCII) 
  map.df <- nuts3_ger
  #map.df <- fortify(map.df, by="Name_ASCII") # Regions defined for each Polygons
  #map.df@data <- left_join(map.df@data,cases_month1, by="Name_ASCII")
  map.df@data <- join(map.df@data,cases_month1, by="Name_ASCII")
  map.df@data$COUNT[is.na(map.df@data$COUNT)]<-0
  map.df@data$brks <- cut(map.df@data$COUNT, 
                          breaks=c(-0.1, 0.9, 10, 20, 30, 35), 
                          labels=c("0", "1 - 10", "11 - 20", 
                                   "21 - 30", "> 30"))
  #map.df@data$cols <- cut(map.df@data$COUNT, 
  #                        breaks=c(-0.1, 0.9, 10, 20, 30, 35), 
  #                        labels=c("white", "lavanderblush", "red1", 
  #                                 "red3", "red4"))
  
  col <- c("white", "lightblue", "royalblue", "blue2", "navyblue")
  qtm <- qtm(map.df, fill = "brks", fill.palette = #"Blues"
               col, 
             fill.title="Number of cases") + tm_fill(title = "GDP") +
    tm_borders() +
    tm_layout(i, legend.title.size = 1, legend.text.size = 0.6,
              #legend.position = c("right","bottom"),
              legend.outside = TRUE,
              legend.bg.color = "white")
  print(qtm)
  save_tmap(qtm, paste("qtm1_", i, ".png", sep=""), width=1920, height=1380)
}



### AGGREGATE PER QUARTERS ###

tmp1$IDQ <- with(tmp1, paste0(YEAR, "Q", QUARTER))

tmp1_quarters <- tapply(tmp1$COUNT, list(tmp1$ADM_UNIT, tmp1$IDQ), sum)
tmp1_quarters.df <- as.data.frame.table(tmp1_quarters) # dataframe

tmp1_quarters.df$YEAR <- substring(tmp1_quarters.df$Var2, 1,4)
tmp1_quarters.df$QUARTER <- substring(tmp1_quarters.df$Var2, 6,7)

tmp1_quarters.df <- tmp1_quarters.df[,c(1,2,4,5,3)]

colnames(tmp1_quarters.df)[1] <- "ADM_UNIT"
colnames(tmp1_quarters.df)[2] <- "ID"
colnames(tmp1_quarters.df)[5] <- "COUNT"

#write.csv(tmp1_quarters.df, file = "full_units_nuts_cases_QUARTERS.csv")

