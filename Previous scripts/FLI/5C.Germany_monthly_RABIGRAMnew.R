
##'**LAURIE'S PROJECT**
##'**FOX RABIES DYNAMIC**
##'**MICAELA DE LA PUENTE L.**        
##'**This script creates a csv of german data by month using the new** 
##'**rabrigramm shapefile** 
##########################################################################

library(rgdal)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(rgeos)
library(maptools)
library(raster)
library(car)
library(dplyr)
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

adm_units_brd <-readOGR("New_shapefiles_FLI/BRD_Rabigramm/BRD_Rabigramm.shp", 
                        "BRD_Rabigramm")
CRS <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(adm_units_brd) <- CRS
str(adm_units_brd@data)
# 'data.frame':	17575 obs. of  3 variables:
#$ ID      : int  11507 11508 11509 11510 11511 1 2 3 4 5 ...
#$ AreaCode: Factor w/ 17573 levels "01000000","01001000",..: 16540 16541 16542 16543 16544 2 3 4 5 7 ...
#$ Name    : Factor w/ 13981 levels "Aach","Aachen",..: 5416 5458 5606 5895 5941 3319 6147 7394 8562 122 ...

# Delete first big regions
adm_units_brd1 <- subset(adm_units_brd, ID < 17533) # subset each unit
adm_units_brd1$AreaCode <- factor(adm_units_brd1$AreaCode)
adm_units_brd1$Name <- factor(adm_units_brd1$Name)
str(adm_units_brd1@data)

#writeOGR(adm_units_brd1, dsn=".", layer="output/BRD_Rabigramm1", 
#         driver="ESRI Shapefile")

# Delete next
adm_units_brd2 <- subset(adm_units_brd1, ID < 17232) # subset each unit
adm_units_brd2$AreaCode <- factor(adm_units_brd2$AreaCode)
adm_units_brd2$Name <- factor(adm_units_brd2$Name)
str(adm_units_brd2@data)
# 'data.frame':	17231 obs. of  3 variables:
#$ ID      : int  11507 11508 11509 11510 11511 1 2 3 4 5 ...
#$ AreaCode: Factor w/ 17229 levels "01001000","01002000",..: 16212 16213 16214 16215 16216 1 2 3 4 5 ...
#$ Name    : Factor w/ 13801 levels "Aach","Aachen",..: 5363 5405 5553 5840 5886 3280 6092 7332 8477 119 ...

#writeOGR(adm_units_brd2, dsn=".", layer="output/BRD_Rabigramm2", 
#         driver="ESRI Shapefile")


#### ADD  THE RABIGRAMM UNITS ####

### To German dataset only ###
ger_cases <- readOGR("output/german_subset_germany.shp","german_subset_germany") 
names(ger_cases)
length(sort(unique(ger_cases$GEB_NAME), decreasing=FALSE)) # 2600 units

## Extract ub QGIS the units according to coordinates to avoid spelling problems.
rabigramm_unit <-readOGR("output/rabigramm2_units.shp", "rabigramm2_units") # done in qgis
names(rabigramm_unit) # [1] "AreaCode"
str(rabigramm_unit@data) # 'data.frame':18081 obs. of  1 variable:
# data.frame':	18081 obs. of  1 variable:
# $ AreaCode: Factor w/ 4643 levels "03355049","05120000",..: 1337 2132 1816 1891 1837 1387 1863 1936 2019 1833 ...

# Add to the germany data
ger_cases1 <- ger_cases
ger_cases1@data <- cbind(ger_cases1@data, rabigramm_unit@data)
names(ger_cases1)
head(ger_cases1)# First 4 match with code, the other 2 dont.

# Turn into a df
df <- as.data.frame(ger_cases1)
df1 <- df[,c(1,2,4,5)]
colnames(df1)[1] <- "YEAR"
colnames(df1)[2] <- "ADM_UNIT"
colnames(df1)[3] <- "MONTH"
colnames(df1)[4] <- "AreaCode"

# Create the compiled dataframe, per months
# German cases from 1982 to 2007
ger_cases_monthly <- as.data.frame(table(df1$AreaCode,
                                         df1$YEAR,
                                         df1$MONTH))

# Correct column names
colnames(ger_cases_monthly)[1] <- "AreaCode"
colnames(ger_cases_monthly)[2] <- "YEAR"
colnames(ger_cases_monthly)[3] <- "MONTH"
colnames(ger_cases_monthly)[4] <- "COUNT"

# Create a new column with year and month together
ger_cases_monthly$ID <- with(ger_cases_monthly, paste0(YEAR, "M", MONTH))
names(ger_cases_monthly)
# Tidy the dataset
ger_cases_monthly <- ger_cases_monthly[,c(1,5,2:4)]



### To European dataset only ####

clip_eu_cases <- readOGR("output/clip_european_subset_germany.shp",
                         "clip_european_subset_germany") 
names(clip_eu_cases)
length(sort(unique(clip_eu_cases$REGION), decreasing=FALSE)) # 215 units

## Extract ub QGIS the units according to coordinates to avoid spelling problems.
rabigramm2_units_clip <-readOGR("output/rabigramm2_units_clip1.shp", 
                                "rabigramm2_units_clip1") # done in qgis
names(rabigramm2_units_clip) # [1] "AreaCode"
str(rabigramm2_units_clip@data) # 'data.frame':5423 obs. of  1 variable:
# 'data.frame':	5423 obs. of  1 variable:
#$ AreaCode: Factor w/ 2175 levels "01053046","01053093",..: 6 229 275 281 314 314 315 316 316 316 ...

# Add to the germany data
clip_eu_cases1 <- clip_eu_cases
clip_eu_cases1@data <- cbind(clip_eu_cases1@data, rabigramm2_units_clip@data)
names(clip_eu_cases1)
head(clip_eu_cases1) 

# Turn into a df
df_clip <- as.data.frame(clip_eu_cases1)
df1_clip <- df_clip[,c(1,4,2,9)]
colnames(df1_clip)[2] <- "ADM_UNIT"
colnames(df1_clip)[4] <- "AreaCode"

# Find out number of cases per quarter
clip_cases_quarters <- as.data.frame(table(df1_clip$AreaCode,
                                           df1_clip$YEAR,
                                           df1_clip$QUARTER))

# Change names
colnames(clip_cases_quarters)[1] <- "AreaCode"
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
unit.i <- sort(unique(clip_cases_quarters$AreaCode), decreasing = FALSE)

#library(dplyr) # sometimes is needed to upload the package again

clip.final <- list()

for(j in unit.i){
  clip.i <- subset(clip_cases_quarters, AreaCode == j) # subset each unit
  clip.i$AreaCode <- factor(clip.i$AreaCode)
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


## Check both dfs
str(ger_cases_monthly)
# 'data.frame':	1392900 obs. of  5 variables:
#$ AreaCode: Factor w/ 4643 levels "03355049","05120000",..: 1 2 3 4 5 6 7 8 9 10 ...
#$ ID      : chr  "1982M01" "1982M01" "1982M01" "1982M01" ...
#$ YEAR    : Factor w/ 25 levels "1982","1983",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ MONTH   : Factor w/ 12 levels "01","02","03",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ COUNT   : int  0 0 0 0 0 0 0 0 0 0 ...
ger_cases_monthly$COUNT <- as.numeric(ger_cases_monthly$COUNT)

str(clip.df)
# 'data.frame':	182700 obs. of  5 variables:
#$ AreaCode: Factor w/ 2175 levels "01053046","01053093",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ ID      : chr  "1990M01" "1990M02" "1990M03" "1990M04" ...
#$ YEAR    : Factor w/ 7 levels "1990","1991",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ MONTH   : Factor w/ 12 levels "01","02","03",..: 1 2 3 4 5 6 7 8 9 10 ...
#$ COUNT   : num  0 0 0 0 0 0 0 0 0 0 ...



##### Merge the two datasets ####
tmp <- merge(ger_cases_monthly, clip.df, by = c("AreaCode", "ID"), 
             all.x = TRUE, all.y = TRUE)
str(tmp)
head(tmp)
tmp$COUNT.x[is.na(tmp$COUNT.x)]<-0
tmp$COUNT.y[is.na(tmp$COUNT.y)]<-0
tmp$COUNT <- tmp$COUNT.x+tmp$COUNT.y
tmp <- tmp[,c(1,2,9)]

# Check number of units
length.unit <- length(unique(tmp$AreaCode)) # 6668
length.df <- nrow(tmp) #  1563000
length.df/length.unit # 234.4031. It is ok, 25 years per 12 months. it is not exact
# The ones that come from european dataset only have until 1990 to 1995 and 2000


### CREATE A MONTHLY DATA FRAME WITH ALL UNITS IN GERMANY ###
# This one is empty for COUNT column

# Call boundaries shapefile
unit.i <- sort(unique(adm_units_brd2$AreaCode), decreasing = FALSE) #  17229

# Monthly cases shapefile
str(tmp)
# 'data.frame':	1563000 obs. of  3 variables:
#$ AreaCode: Factor w/ 6668 levels "03355049","05120000",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ ID      : chr  "1982M01" "1982M02" "1982M03" "1982M04" ...
#$ COUNT   : num  0 0 0 0 0 0 1 0 0 0 ...

names(tmp)
# [1] "AreaCode" "ID"    "COUNT"

# Divide ID to month and year
units_monthly <- tmp
units_monthly$YEAR <- substring(units_monthly$ID, 1,4)
units_monthly$MONTH <- substring(units_monthly$ID, 6,7)
str(units_monthly)
units_monthly$YEAR <- as.numeric(units_monthly$YEAR)
str(units_monthly)
# 'data.frame':	1563000 obs. of  5 variables:
#$ AreaCode: Factor w/ 6668 levels "03355049","05120000",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ ID      : chr  "1982M01" "1982M02" "1982M03" "1982M04" ...
#$ COUNT   : num  0 0 0 0 0 0 1 0 0 0 ...
#$ YEAR    : num  1982 1982 1982 1982 1982 ...
#$ MONTH   : chr  "01" "02" "03" "04" ...

# Add quarters
units_monthly$QUARTER <- units_monthly$MONTH 
#library(dplyr) # upload again if needed.
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

str(units_monthly)
units_monthly$ID <- with(units_monthly, paste0(YEAR, "M", MONTH)) 
units_monthly <- units_monthly[,c(1,4,6,5,2,3)]
colnames(units_monthly)[1] <- "AreaCode"
head(units_monthly)
units_monthly$MONTH <- as.numeric(units_monthly$MONTH)
units_monthly$QUARTER <- as.numeric(units_monthly$QUARTER)
str(units_monthly)
# 'data.frame':	1563000 obs. of  6 variables:
#$ AreaCode: Factor w/ 6668 levels "03355049","05120000",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ YEAR    : num  1982 1982 1982 1982 1982 ...
#$ QUARTER : num  1 1 1 2 2 2 3 3 3 4 ...
#$ MONTH   : num  1 2 3 4 5 6 7 8 9 10 ...
#$ ID      : chr  "1982M01" "1982M02" "1982M03" "1982M04" ...
#$ COUNT   : num  0 0 0 0 0 0 1 0 0 0 ...


### Create an empty dataframe
final_dataframe <- as.data.frame(unit.i)
colnames(final_dataframe)[1] <- "AreaCode"
nunits <- length(unique(final_dataframe$AreaCode)) # [1] 17229

# Expand the data set
nyears <- length(1982:2006) # 25
nmonths<- length(1:12) # 12
nrow_perunit <- nyears*nmonths #300
nrows <- nrow_perunit*nunits # [1]  5168700
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
str(final_dataframe1$MONTH)
final_dataframe1$MONTH <- as.factor(final_dataframe1$MONTH)
#final_dataframe1$MONTH <- recode(final_dataframe1$MONTH, "'1'='01'; '2'='02'; 
#                         '3'='03'; '4'='04';'5'='05';
#                       '6'='06';'7'='07'; '8'='08'; '9'='09'")

library(dplyr) # upload if needed
library(sp)
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
#'data.frame':	5168700 obs. of  6 variables:
#$ AreaCode: Factor w/ 17229 levels "01001000","01002000",..: 1 2 3 4 5 6 7 8 9 10 ...
#$ YEAR    : num  1982 1982 1982 1982 1982 ...
#$ QUARTER : num  1 1 1 1 1 1 1 1 1 1 ...
#$ MONTH   : num  1 1 1 1 1 1 1 1 1 1 ...
#$ ID      : chr  "1982M01" "1982M01" "1982M01" "1982M01" ...
#$ COUNT   : logi  NA NA NA NA NA NA ...



##### Merge this empty dataframe with the cases dataframe
#memory.size(4000)
rm(list=setdiff(ls(), c("final_dataframe1","units_monthly", "adm_units_brd2")))

tmp1 <- merge(final_dataframe1, units_monthly, by = c("AreaCode", "ID"), all.x = TRUE, 
             all.y = TRUE)
# Error: cannot allocate vector of size 39.4 Mb
tmp1 <- tmp1[,c(1:5,10)]

names(tmp1) # [1] [1] "AreaCode"  "ID"  "YEAR.x"  "QUARTER.x" "MONTH.x" "COUNT.y"  
tmp1$COUNT.y[is.na(tmp1$COUNT.y)]<-0
colnames(tmp1)[3] <- "YEAR"
colnames(tmp1)[4] <- "QUARTER"
colnames(tmp1)[5] <- "MONTH"
colnames(tmp1)[6] <- "COUNT"
head(tmp1)

# Write it
#write.csv(tmp1, file = "full_units_rabrigramm_cases.csv")



###### PLOT MONTHLY MAPS #####

units_data <- adm_units_brd2
names(units_data)
units_data <- units_data[,c(2)]
units_data@data$id <- rownames(units_data@data)
names(units_data) # [1]  "AreaCode" "id"        

vector <- sort(unique(tmp1$ID), decreasing = FALSE)
length(vector) # [1] 300


for(i in vector) 
  {
  cases_month <- subset(tmp1, ID == i)
  cases_month1 <- merge(units_data@data, cases_month, 
                        by.x="AreaCode",by.y="AreaCode") 
  # plus the id
  units_data.df <- fortify(units_data, by="id") # Regions defined for each Polygons
  # i am not sure what this does
  
  map.df <- join(units_data.df,cases_month1, by="id")
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
  cases_month <- subset(tmp1, ID == i)
  cases_month1 <- merge(units_data@data, 
                        cases_month,by.x="AreaCode",
                        by.y="AreaCode")  # plus the id and Name
  
  #  cases_month2 <- rename(cases_month1, Name_ASCII=Name_ASCII) 
  map.df <- adm_units_brd2
  #map.df <- fortify(map.df, by="Name_ASCII") # Regions defined for each Polygons
  #map.df@data <- left_join(map.df@data,cases_month1, by="Name_ASCII")
  map.df@data <- left_join(map.df@data,cases_month1, by="AreaCode")
  map.df@data$COUNT[is.na(map.df@data$COUNT)]<-0
  map.df@data$brks <- cut(map.df@data$COUNT, 
                          breaks=c(-0.1, 0, 5, 10, 15), 
                          labels=c("0", "1 - 5", "6 - 10", "11 - 15"))
  map.df1 <- map.df[,c(2,6:10)]
  #map.df@data$cols <- cut(map.df@data$COUNT, 
  #                        breaks=c(-0.1, 0.9, 10, 20, 30, 35), 
  #                        labels=c("white", "lavanderblush", "red1", 
  #                                 "red3", "red4"))
  
  col <- c("white", "lightblue", "royalblue", "navyblue")
  qtm <- qtm(map.df1, fill = "brks", fill.palette = #"Blues"
               col, 
             fill.title="Number of cases", title=i) + #tm_fill(title = "GDP") +
  tm_borders(lwd=0.5) +
    tm_layout(#i, 
      legend.title.size = 3, legend.text.size = 2,
      title.size=3,
              #legend.position = c("right","bottom"),
              legend.outside = TRUE,
              legend.bg.color = "white")
  print(qtm)
  save_tmap(qtm, paste("rabigramm2_ger_", i, ".png", sep=""), 
            width=4320, height=3105)
}





### AGGREGATE PER QUARTERS ###

tmp1$IDQ <- with(tmp1, paste0(YEAR, "Q", QUARTER))

tmp1_quarters <- tapply(tmp1$COUNT, list(tmp1$AreaCode, tmp1$IDQ), sum)
tmp1_quarters.df <- as.data.frame.table(tmp1_quarters) # dataframe

tmp1_quarters.df$YEAR <- substring(tmp1_quarters.df$Var2, 1,4)
tmp1_quarters.df$QUARTER <- substring(tmp1_quarters.df$Var2, 6,7)

tmp1_quarters.df <- tmp1_quarters.df[,c(1,2,4,5,3)]

colnames(tmp1_quarters.df)[1] <- "ADM_UNIT"
colnames(tmp1_quarters.df)[2] <- "ID"
colnames(tmp1_quarters.df)[5] <- "COUNT"

write.csv(tmp1_quarters.df, file = "full_units_rabigrammnew_cases_QUARTERS.csv")


#########################

#### Add area to germany shapefile ##

# Transfor to utm
adm_units_brd3 <- adm_units_brd2
proj4string(adm_units_brd3) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
adm_units_brd3 <- spTransform(adm_units_brd3, 
                            #CRS("+init=epsg:2040"))
                            CRS("+init=epsg:31253"))

# Calculate area of units
germany.areas <- sapply(slot(adm_units_brd3, "polygons"), slot, "area")
germany.areas.km <- germany.areas/1000000 # transform to km
germany.sum <- sum(germany.areas.km) 
germany.sum # 454577 km2

# Save areas as a new column in the sdpf with original projection
adm_units_brd2@data$area_km2 <- germany.areas.km

# Save it
writeOGR(adm_units_brd2, dsn=".", layer="output/BRD_Rabigramm2_with area", 
         driver="ESRI Shapefile")
adm_units_brd2.df <- as.data.frame(adm_units_brd2)
write.csv(adm_units_brd2.df, file = "output/BRD_Rabigramm2_with area.csv")


