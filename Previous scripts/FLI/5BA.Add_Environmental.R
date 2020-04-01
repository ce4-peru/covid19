
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

# 
# #### ADD  THE NUTS 3 UNITS ####
# full.nuts.cases <- read.csv("output/full_units_nuts_cases.csv")
# head(full.nuts.cases, 3)
# # X  ADM_UNIT      ID YEAR QUARTER MONTH COUNT
# # 1 1 LK Aachen 1982M01 1982       1     1     0
# # 2 2 LK Aachen 1982M02 1982       1     2     0
# # 3 3 LK Aachen 1982M03 1982       1     3     0



### Call shapefile
nuts3_ger <-readOGR("output/nuts3_germany.shp", "nuts3_germany") # done in qgis
plot(nuts3_ger)
str(nuts3_ger@data) # 'data.frame':'data.frame':	429 obs. of  4 variables:
names(nuts3_ger)
# [1] "ID"         "Name"       "Name_ASCII" "CODE"
unit.i <- sort(unique(nuts3_ger$Name_ASCII), decreasing = FALSE) # 429
extent(nuts3_ger)
# class       : Extent 
# xmin        : 5.866256 
# xmax        : 15.04126 
# ymin        : 47.27024 
# ymax        : 55.05682 

projection(nuts3_ger) 
# [1] "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"



## Add elevation variable
elevation <- raster("C:/Users/Micaela/Dropbox/Fox Rabies_MD/Scripts/output/elevationwe_new1.tif")
# class       : RasterLayer 
# dimensions  : 2712, 4032, 10934784  (nrow, ncol, ncell)
# resolution  : 0.008333333, 0.008333333  (x, y)
# extent      : -9.4, 24.2, 35.2, 57.8  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=lcc +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
# data source : C:\Users\Micaela\Dropbox\Fox Rabies_MD\Scripts\output\elevationwe_new1.tif 
# names       : elevationwe_new1 
# values      : -179, 4536  (min, max)


## Check projection
projection(elevation) # [1] "+proj=lcc +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# crs(elevation) # CRS arguments:
# +proj=lcc +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10 +x_0=4000000 +y_0=2800000
# +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
crs(nuts3_ger) #  +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
crs(elevation) <- crs(nuts3_ger)

# Set extent and increase resolution of elevation raster
ext <- extent(nuts3_ger)
# class       : Extent 
# xmin        : 5.866256 
# xmax        : 15.04126 
# ymin        : 47.27024 
# ymax        : 55.05682 
ext1 <- extent(5,16,47,56)
elevation1 <- crop(elevation, ext1)
#class       : RasterLayer 
# dimensions  : 1080, 1320, 1425600  (nrow, ncol, ncell)
# resolution  : 0.008333333, 0.008333333  (x, y)
# extent      : 5, 16, 47, 56  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
# data source : in memory
# names       : elevationwe_new1 
# values      : -179, 3449  (min, max)

plot(elevation1)
plot(nuts3_ger, add=TRUE)



## Add land use variable
fli.raster <-raster("C:/Users/Micaela/Dropbox/Fox Rabies_MD/FLI/Data/Raster/Corine_land/land_raster_fli.tif") # Modified in QGIS
# dimensions  : 33745, 87960, 2968210200  (nrow, ncol, ncell)
# resolution  : 0.001396235, 0.001396229  (x, y)
# extent      : -49.90671, 72.90614, 25.54471, 72.66045  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
# data source : C:\Users\Micaela\Dropbox\Fox Rabies_MD\FLI\Data\Raster\Corine_land\land_raster_fli.tif 
# names       : land_raster_fli 
# values      : 1, 44  (min, max)
plot(fli.raster)

## Check projection
crs(fli.raster)
# CRS arguments:
# +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
# Change projection
crs(fli.raster) <- crs(nuts3_ger)
fli.raster
# values      : 1, 44  (min, max)


# Plot the raster
plot(fli.raster, main="Land use raster_tif")
plot(nuts3_ger, add=TRUE)

# Reclassify
m <- c(0.9, 11.4, 1,   # artificial areas
       11.5, 22.4, 3, # agricultural lands
       22.5, 25.4, 4, # highly forested
       25.5, 29.4, 5, # Moderate forested.
       29.5, 34.4, 6, # barren lands
       34.5, 39.4, 7, # marshlands
       39.5, 44.4, NA) # water bodies

rclmat <- matrix(m, ncol=3, byrow=TRUE)
fli.raster1 <- reclassify(fli.raster, rclmat)

# Copy extent and resolution of NASA land use raster
fli.raster2 <- projectRaster(fli.raster1, elevation1, method="ngb")

## Check the extent
fli.raster2
# class       : RasterLayer 
# dimensions  : 1080, 1320, 1425600  (nrow, ncol, ncell)
# resolution  : 0.008333333, 0.008333333  (x, y)
# extent      : 5, 16, 47, 56  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
# data source : in memory
# names       : layer 
# values      : 1, 7  (min, max)

# Plot
plot(fli.raster2)
plot(nuts3_ger, add=TRUE)

freq(fli.raster2)
#[1,]     1  85243
# [2,]     3 670776
# [3,]     4 356024
# [4,]     5  28788
# [5,]     6  10122
# [6,]     7  15259
# [7,]    NA 259388

# with continuous within artificial
lab.legend <- c("Artificial lands", 
                "Agricultural", "Highly forested", 
                "Moderately forested ", "Barren lands",
                "Marshlands")

## Create the legend colors
col.legend <- c("mediumorchid4", "yellow2", #"chocolate1",
                "forestgreen", "limegreen", "bisque3", "coral1")

breakpoints <- c(0.5, 1.5, 3.5, 4.5, 5.5, 6.5, 7.5) # create breaks to assign colors
colors <- c("mediumorchid4", "yellow2", #"chocolate1",
            "forestgreen", "limegreen", "bisque3", "coral1") # create a vector with colors for land use


## Plot the land use raster
plot(fli.raster2, breaks=breakpoints, col=colors, legend=FALSE)
par(oma = c(1, 1, 1, 4))
legend("right", legend=lab.legend, fill=col.legend, x.intersp = .1, 
       y.intersp = .4, cex = 0.75, xpd = TRUE, inset = c(-0.45,0), bty = "n")



#### CREATE a RASTER OF ELEVATION FOR WE ONLY
elev_raster <- rasterize(nuts3_ger, elevation1, mask=TRUE) 
## Check values
elev_raster
# class       : RasterLayer 
# dimensions  : 1080, 1320, 1425600  (nrow, ncol, ncell)
# resolution  : 0.008333333, 0.008333333  (x, y)
# extent      : 5, 16, 47, 56  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
# data source : in memory
# names       : layer 
# values      : -179, 2701  (min, max)

## Plot the new raster
plot(elev_raster, legend=FALSE)
length(elev_raster) #[1] 1425600 # number of cells
# Check the frequency of each value
freq(elev_raster) 
#765907 NA

#### Mask them to get a better shape
elev_raster2 <- mask(elev_raster, elevation1)
## Check if values are the same that previous raster


## Plot the new raster
plot(elev_raster2)

# We will use this one.
writeRaster(elev_raster2, filename="output/elev_raster.tif", 
            format="GTiff", overwrite=TRUE)


#### CREATE a RASTER OF LAND USE FOR WE ONLY
fli_raster <- rasterize(nuts3_ger, fli.raster2, mask=TRUE) 
## Check values
fli_raster
# # class       : RasterLayer 
# dimensions  : 1080, 1320, 1425600  (nrow, ncol, ncell)
# resolution  : 0.008333333, 0.008333333  (x, y)
# extent      : 5, 16, 47, 56  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
# data source : in memory
# names       : layer 
# values      : 1, 7  (min, max)


## Plot the new raster
plot(fli_raster, legend=FALSE)
length(fli_raster) #[1] 1425600 # number of cells
# Check the frequency of each value
freq(fli_raster) 
#771784 NA

#### Mask them to get a better shape
fli_raster2 <- mask(fli_raster, fli.raster2)
## Check if values are the same that previous raster
## Plot the new raster
plot(fli_raster2)

# We will use this one.
writeRaster(fli_raster2, filename="output/fli_raster.tif", 
            format="GTiff", overwrite=TRUE)



## Call rasters
# Try if it works
land.raster <- raster("output/fli_raster.tif")
elevation.raster <- raster("output/elev_raster.tif")

# CREATE AN STACK OF LAND LAYERS
land_stack_unique <- stack(land.raster, elevation.raster)
dim(land_stack_unique) # [1] 1080 1320    2
plot(land_stack_unique)
land_unique_df <- as.data.frame (land_stack_unique) # turn df
names(land_unique_df) #check
str(land_unique_df) # 1425600 obs. of  2 variables:


## Create the raster of administrative units 
## Rasterize
raster_gadm <- rasterize(nuts3_ger, land.raster, 
                                field=nuts3_ger$Name_ASCII) # country name
raster1_gadm <- mask(raster_gadm, land.raster)
raster1_gadm
#dimensions  : 1080, 1320, 1425600  (nrow, ncol, ncell)
# resolution  : 0.008333333, 0.008333333  (x, y)
# extent      : 5, 16, 47, 56  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
# data source : in memory
# names       : layer 
# values      : 1, 429  (min, max)
# Alphabetic order

plot(raster1_gadm)

# Save GADM raster
writeRaster(raster1_gadm, filename="output/nuts_raster.tif", 
            format="GTiff", overwrite=TRUE)


## Turn into df
nuts_df <- as.data.frame (raster1_gadm) # turn df
names(nuts_df) #check
str(nuts_df) # 1425600 obs. of  2 variables:

nuts3_env <- cbind(nuts_df, land_unique_df)
nuts3_env1 <- na.exclude(nuts3_env)
str(nuts3_env1) # 'data.frame':	652989 obs. of  3 variables:
nuts3_env1$layer <- as.factor(nuts3_env1$layer)
nuts3_env1$fli_raster <- as.factor(nuts3_env1$fli_raster)



#### Check mean of elevation per unit
mean_elev <- aggregate(nuts3_env1$elev_raster, list(nuts3_env1$layer), mean)

#### Check how many cells per unit
no_cells <- as.data.frame(table(nuts3_env1$layer))

#### Check number of cells of each land use per unit
no_cells_lu <- as.data.frame(table(nuts3_env1$fli_raster, 
                                    nuts3_env1$layer))
str(no_cells_lu)

# Level 1
no_cells_lu1 <- no_cells_lu[which(no_cells_lu$Var1=="1"),]
no_cells_lu1 <- no_cells_lu1[,c(2:3)] 
colnames(no_cells_lu1)[1] <- "Unit"
colnames(no_cells_lu1)[2] <- "L1"

# Level 2
no_cells_lu2 <- no_cells_lu[which(no_cells_lu$Var1=="2"),]
no_cells_lu2 <- no_cells_lu2[,c(2:3)] 
colnames(no_cells_lu2)[1] <- "Unit"
colnames(no_cells_lu2)[2] <- "L2"


# Create the columns for the dataframe
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
final_dataframe1$Landuse <- NA
final_dataframe1$Elevation <- NA
str(final_dataframe1) # OK now
# 'data.frame':	128700 obs. of  7 variables:
#   $ ADM_UNIT : Factor w/ 429 levels "LK Aachen","LK Ahrweiler",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ YEAR     : num  1982 1982 1982 1982 1982 ...
# $ QUARTER  : num  1 1 1 1 1 1 1 1 1 1 ...
# $ MONTH    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ ID       : chr  "1982M01" "1982M01" "1982M01" "1982M01" ...
# $ Landuse  : logi  NA NA NA NA NA NA ...
# $ Elevation: logi  NA NA NA NA NA NA ...



head(full.nuts.cases, 3)
