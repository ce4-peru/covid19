##'**FOX RABIES DYNAMIC**
##'**MICAELA DE LA PUENTE L.**        
##'**Check all datasets** 
##########################################################################

library(rgdal)
library(rgeos)
library(lme4)
library(lmerTest)
library(Matrix)
library(lattice)  #Needed for multi-panel graphs
library(MASS)
library(car)
library(plyr)
library(sp)
library(raster)
library(maptools)
library(scales)
library(ggplot2)
library(geosphere)
library(mefa)
##########################################################################

#### CALL ####

countries_zero <-readOGR("data/Boundaries data/welevel0.shp","welevel0")
countries_zero_bond <-readOGR("data/Boundaries data/welevel0_bond.shp",
                              "welevel0_bond")
countries_with_borders <-readOGR("data/Boundaries data/we_with_borders.shp",
                                 "we_with_borders")
# Call exklaves
exklave_77 <-readOGR("data/Incidence/infected_area/exklave_1977.shp",
                     "exklave_1977")
exklave_82 <-readOGR("data/Incidence/infected_area/exklave_1982.shp",
                     "exklave_1982")
exklave_85 <-readOGR("data/Incidence/infected_area/exklave_1985.shp",
                     "exklave_1985")
exklave_87 <-readOGR("data/Incidence/infected_area/exklave_1987.shp",
                     "exklave_1987")

mainarea_77 <-readOGR("data/Incidence/infected_area/main_area_1977.shp",
                      "main_area_1977")
mainarea_78 <-readOGR("data/Incidence/infected_area/main_area_1978.shp",
                      "main_area_1978")
mainarea_79 <-readOGR("data/Incidence/infected_area/main_area_1979.shp",
                      "main_area_1979")
mainarea_80 <-readOGR("data/Incidence/infected_area/main_area_1980.shp",
                      "main_area_1980")
mainarea_81 <-readOGR("data/Incidence/infected_area/main_area_1981.shp",
                      "main_area_1981")
mainarea_82 <-readOGR("data/Incidence/infected_area/main_area_1982.shp",
                      "main_area_1982")
mainarea_83 <-readOGR("data/Incidence/infected_area/main_area_1983.shp",
                      "main_area_1983")
mainarea_84 <-readOGR("data/Incidence/infected_area/main_area_1984.shp",
                      "main_area_1984")
mainarea_85 <-readOGR("data/Incidence/infected_area/main_area_1985.shp",
                      "main_area_1985")
mainarea_86 <-readOGR("data/Incidence/infected_area/main_area_1986.shp",
                      "main_area_1986")
mainarea_87 <-readOGR("data/Incidence/infected_area/main_area_1987.shp",
                      "main_area_1987")
mainarea_88 <-readOGR("data/Incidence/infected_area/main_area_1988.shp",
                      "main_area_1988")
mainarea_89 <-readOGR("data/Incidence/infected_area/main_area_1989.shp",
                      "main_area_1989")


# check countries
plot(countries_zero)
# plot(countries_zero_bond)
# plot(countries_with_borders)
# 
# ### Keep only WE border polygon
# c1 <- gUnaryUnion(countries_zero)
# proj_countries <- projection(countries_zero)
# c1_new <- spTransform(c1,proj_countries)
# attr <- data.frame(attr1 = 1, row.names = 1)
# c1_new_spdf <- SpatialPolygonsDataFrame(c1_new, attr)
# plot(c1_new_spdf)
# 
# # Reduce extent of we with borders
# plot(countries_zero_bond)
# plot(countries_with_borders)
# countries_with_borders # extent: -13.69139, 24.15328, 35.49292, 61.52708  (xmin, xmax, ymin, ymax)
# ext <- extent(-5.5, 17.5, 40.5, 55.5)
# countries_zero_bond1 <- crop(countries_zero_bond, ext)
# plot(countries_zero_bond1)
# countries_with_borders1 <- crop(countries_with_borders, ext)
# plot(countries_with_borders1)

# Check projection
projection(countries_zero)
projection(raster.i)

raster.i <- raster("output/caseskernel.new_2000.tif") 

plot(countries_zero)
plot(raster.i, add=T)


## change projections
# exclaves
plot(exklave_77)
plot(exklave_82)
plot(exklave_85)
plot(exklave_87)
# main areas
plot(mainarea_77)
plot(mainarea_78)
plot(mainarea_79)
plot(mainarea_80)
plot(mainarea_81)
plot(mainarea_82)
plot(mainarea_83)
plot(mainarea_84)
plot(mainarea_85)
plot(mainarea_86)
plot(mainarea_87)
plot(mainarea_88)
plot(mainarea_89)

# change proj
proj_countries <- projection(countries_zero)
latlon_CRS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
#reg_CRS <- CRS("+proj=lcc +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

summary(mainarea_77)
mainarea_77.new <- spTransform(mainarea_77,latlon_CRS)
proj4string(mainarea_77.new) <- proj_countries
summary(mainarea_77.new)

summary(mainarea_78)
mainarea_78.new <- spTransform(mainarea_78,latlon_CRS)
proj4string(mainarea_78.new) <- proj_countries
summary(mainarea_78.new)

plot(mainarea_78.new, border="red")
plot(countries_zero, add =T)
plot(raster.i, add=T)


summary(mainarea_79)
mainarea_79.new <- spTransform(mainarea_79,latlon_CRS)
proj4string(mainarea_79.new) <- proj_countries
summary(mainarea_79.new)

summary(mainarea_80)
mainarea_80.new <- spTransform(mainarea_80,latlon_CRS)
proj4string(mainarea_80.new) <- proj_countries
summary(mainarea_80.new)

summary(mainarea_81)
mainarea_81.new <- spTransform(mainarea_81,latlon_CRS)
proj4string(mainarea_81.new) <- proj_countries
summary(mainarea_81.new)

summary(mainarea_82)
mainarea_82.new <- spTransform(mainarea_82,latlon_CRS)
proj4string(mainarea_82.new) <- proj_countries
summary(mainarea_82.new)

summary(mainarea_83)
mainarea_83.new <- spTransform(mainarea_83,latlon_CRS)
proj4string(mainarea_83.new) <- proj_countries
summary(mainarea_83.new)

summary(mainarea_84)
mainarea_84.new <- spTransform(mainarea_84,latlon_CRS)
proj4string(mainarea_84.new) <- proj_countries
summary(mainarea_84.new)

summary(mainarea_85)
mainarea_85.new <- spTransform(mainarea_85,latlon_CRS)
proj4string(mainarea_85.new) <- proj_countries
summary(mainarea_85.new)

summary(mainarea_86)
mainarea_86.new <- spTransform(mainarea_86,latlon_CRS)
proj4string(mainarea_86.new) <- proj_countries
summary(mainarea_86.new)

summary(mainarea_87)
mainarea_87.new <- spTransform(mainarea_87,latlon_CRS)
proj4string(mainarea_87.new) <- proj_countries
summary(mainarea_87.new)

summary(mainarea_88)
mainarea_88.new <- spTransform(mainarea_88,latlon_CRS)
proj4string(mainarea_88.new) <- proj_countries
summary(mainarea_88.new)

summary(mainarea_89)
mainarea_89.new <- spTransform(mainarea_89,latlon_CRS)
proj4string(mainarea_89.new) <- proj_countries
summary(mainarea_89.new)

summary(exklave_77)
exklave_77.new <- spTransform(exklave_77,latlon_CRS)
proj4string(exklave_77.new) <- proj_countries
summary(exklave_77.new)

summary(exklave_82)
exklave_82.new <- spTransform(exklave_82,latlon_CRS)
proj4string(exklave_82.new) <- proj_countries
summary(exklave_82.new)

summary(exklave_85)
exklave_85.new <- spTransform(exklave_85,latlon_CRS)
proj4string(exklave_85.new) <- proj_countries
summary(exklave_85.new)

summary(exklave_87)
exklave_87.new <- spTransform(exklave_87,latlon_CRS)
proj4string(exklave_87.new) <- proj_countries
summary(exklave_87.new)


# calculate area
# c_aus <- subset(countries_zero, NAME_0=="Austria")
# c_fra <- subset(countries_zero, NAME_0=="France")
# c_bel <- subset(countries_zero, NAME_0=="Belgium")
# c_ger <- subset(countries_zero, NAME_0=="Germany")
# c_swi <- subset(countries_zero, NAME_0=="Switzerland")
# c_net <- subset(countries_zero, NAME_0=="Netherlands")
# c_lux <- subset(countries_zero, NAME_0=="Luxembourg")
# 
# plot(c_aus, col="blue")
# plot(c_fra, col="blue")
# plot(c_bel, col="blue")
# plot(c_ger, col="blue")
# plot(c_swi, col="blue")
# plot(c_net, col="blue")
# plot(c_lux, col="blue")

# Get area
year.m <- unique(c(1977:1989))
countries.i <- unique(c("Austria", "Belgium", "France", "Germany", "Luxembourg",
                        "Netherlands", "Switzerland"))

result_area_polygon <- data.frame(YEAR=character(), INF_AREA=character(), 
                                  COUNTRY=character(), stringsAsFactors=FALSE)

for(i in unique(countries.i))
  {
  print(i)
  country.i <- subset(countries_zero, NAME_0==i)
  #plot(country.i)
  
  for(j in unique(year.m)){
    print(j)
    pol.i_t <- readOGR(paste("data/Incidence/infected_area/main_area_", 
                             j, ".shp",sep=""),
                       paste("main_area_", j, sep=""))
     
    # set long lat coords
    summary(pol.i_t)
    pol.i_t.new <- spTransform(pol.i_t,latlon_CRS)
    proj4string(pol.i_t.new) <- proj_countries
    summary(pol.i_t.new)
    
    # crop by country
    #plot(countries_zero)
    #plot(mainarea_77.new, border="red", add=T)
    pol.i_t.new_country <- crop(pol.i_t.new, country.i)
    #plot(countries_zero)
    #plot(pol.i_t.new_country, border="red", add=T)
    
    # change projection to utm
    proj4string(pol.i_t.new_country) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    pol.i_t.new_country_utm <- spTransform(pol.i_t.new_country, CRS("+init=epsg:31253")) # utm coordinate
  
    # area from polygon
    area_inf_polygon <- sum(sapply(slot(pol.i_t.new_country_utm, "polygons"), slot, "area"))/1000000 # in km
    area_inf_polygon1 <- cbind(j, round(area_inf_polygon,4), i)
    result_area_polygon <- rbind(result_area_polygon, area_inf_polygon1) # bind it with previous loop data  
  }
}
    
write.csv(result_area_polygon, "output/area_per_country_1977_1989.csv", row.names=F)

  
# get area of whole period
# Get area
year.m <- unique(c(1977:2005))
countries.i <- unique(c("Austria", 
  "Belgium", "France", "Germany", "Luxembourg",
                        "Netherlands", "Switzerland"))

result_area_polygon <- data.frame(YEAR=character(), INF_AREA=character(), 
                                  COUNTRY=character(), stringsAsFactors=FALSE)

for(i in unique(countries.i))
{
  print(i)
  country.i <- subset(countries_zero, NAME_0==i)
  plot(country.i)
  
  for(j in unique(year.m)){
    print(j)
    pol.i_t <- readOGR(paste("data/Incidence/infected_area/main_area_", 
                             j, ".shp",sep=""),
                       paste("main_area_", j, sep=""))
    
    # set long lat coords
    summary(pol.i_t)
    pol.i_t.new <- spTransform(pol.i_t,latlon_CRS)
    proj4string(pol.i_t.new) <- proj_countries
    summary(pol.i_t.new)
    #plot(pol.i_t.new)
    
    # crop by country
    #plot(countries_zero)
    #plot(mainarea_77.new, border="red", add=T)
    pol.i_t.new_country <- crop(pol.i_t.new, country.i)
    
    if(is.null(pol.i_t.new_country)){
      
      print("No area this year")
      area_inf_polygon1 <- cbind(j, 0, i)
      result_area_polygon <- rbind(result_area_polygon, area_inf_polygon1) # bind it with previous loop data    
      
    } else {
      
      #plot(countries_zero)
      #plot(pol.i_t.new_country, border="red", add=T)
      
      # change projection to utm
      proj4string(pol.i_t.new_country) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      pol.i_t.new_country_utm <- spTransform(pol.i_t.new_country, CRS("+init=epsg:31253")) # utm coordinate
      
      # area from polygon
      area_inf_polygon <- sum(sapply(slot(pol.i_t.new_country_utm, "polygons"), slot, "area"))/1000000 # in km
      area_inf_polygon1 <- cbind(j, round(area_inf_polygon,4), i)
      result_area_polygon <- rbind(result_area_polygon, area_inf_polygon1) # bind it with previous loop data    
    }
  }
  }

write.csv(result_area_polygon, "output/area_per_country_1977_2006_full.csv", row.names=F)




### before
png("map_87_vs2.png",
    width=6, height=6, units="in", res=600)
plot(c1_new_spdf.utm, lwd=2, main="1987")
plot(mainarea_87.utm, add=T, col="red")
dev.off()

png("map_87_vs3.png",
    width=6, height=6, units="in", res=600)
plot(c1_new_spdf.utm, lwd=2, main="1987")
plot(mainarea_87.utm, add=T, col="red")
plot(exclave_87.utm, add=T, col="green")
dev.off()

png("map_88_vs2.png",
    width=6, height=6, units="in", res=600)
plot(c1_new_spdf.utm, lwd=2, main="1988")
plot(mainarea_88.utm, add=T, col="red")
dev.off()

png("map_89_vs2.png",
    width=6, height=6, units="in", res=600)
plot(c1_new_spdf.utm, lwd=2, main="1989")
plot(mainarea_89.utm, add=T, col="red")
dev.off()