##'**LAURIE'S PROJECT**
##'**FOX RABIES DYNAMIC**
##'**MICAELA DE LA PUENTE L.**
##'**This script shows subset of fox rabies data for Germany, Poland and Czech Rep**

######################################################
# FIRST SET WORKING DIRECTORY IN RSTUDIO
# i.e. Session > Set Working Directory > To Source File Location
rm(list=ls())
####################################################

library(foreign)
library(maptools)
library(maps)
library(mapdata)
library(mapproj)
library(sp)
library(rgdal)
library(rworldmap)
require(rgeos)
library(ggmap)
library(ggplot2)
library(Rcpp)
library(foreign)
library(devtools)
library(animation)
library(reshape2)


#####################################################
#####################################################
## Call the shp
foxrabies <-readOGR("output/Europe_cases.shp", 
                             "Europe_cases") 
str(foxrabies)
######################################################

# Find correct names
unique(foxrabies@data$COUNTRY)
# Czech Republic
# Germany
# Poland
subset <- subset(foxrabies, COUNTRY %in% c("Poland", "Czech Republic", "Germany"))
# R remembers previous values but they are not there.

## Make disappear other values from SPECIES
subset$COUNTRY <- factor(subset$COUNTRY);subset$COUNTRY 
# Levels: Czech Republic Germany Poland

## Explore fox rabies dataset
str(subset)  # 42014 obs. of  7 variables:
sort(unique(subset@data$YEAR), decreasing = FALSE)
#[1] 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 
# 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016
## Only from 1987 ###
## There are 30 years of fox rabies data

# Transform the subset the a normal dataframe
subset_cases.df <- as.data.frame(subset)

## Save the new spatial points data frame and standard dataframe & KMLs!
writeOGR(subset, dsn=".", layer="output/subset_cases", driver="ESRI Shapefile")
write.csv(subset_cases.df, file = "output/subset_cases.df.csv")
writeOGR(subset, dsn=paste("output/subset_cases", ".kml", sep=""), layer="ID", driver="KML")


### PLOT SUBSET OF GERMANY, CZECH REPUBLIC AND POLAND ONLY
#### Map with full rabies cases

## Map of Europe focus on our study area
map <- get_map(location = c(lon = 15, lat = 52),     # select Europe
               zoom = 5,                # 5 to set a higher scale.
               maptype = "terrain",     # setting easy to visualise
               source = "google")       # the source to load the map from
ggmap(map) # ggmap function to load map of Europe - plots map as a layer over a base graphic

png("output/Subset_full_map.png", width=6, height=5, units="in", res=300)
## Map of our study are with rabies cases
ggmap(map) + xlab("Longitude") + ylab("Latitude") +
  geom_point(data = subset_cases.df, # Add points for sites using latitude and longitude.
             aes(x = coords.x1, y = coords.x2), # Set longitudes and latitudes on axes.
             alpha = 0.5, # Define the type of point to be used on the map.
             color = "red", # Set colour of the points
             size = 0.2,
             position = position_jitter(width = 0.1, height = 0.1))
dev.off()



#### Maps with fox rabies cases by years in WE
## Select years
year.i<-sort((unique(subset_cases.df$YEAR)), decreasing = FALSE)

## by year
for(i in seq_along(year.i)){
  
  sub.ggplot <- subset_cases.df[which(subset_cases.df$YEAR==year.i[i]),] 
  # Create the map with fox rabies data
  
  maps <- ggmap(map) + xlab("Longitude") + ylab("Latitude") +
    geom_point(data = sub.ggplot,
               aes(x = coords.x1, y = coords.x2),
               alpha = 0.5, color = "red", size = 0.5,
               position = position_jitter(width = 0.1, height = 0.1)) + 
    ggtitle(paste("Fox rabies in", year.i[i],sep=" ")) # add a title
  # Plot the map
  plot(maps)
  
  # Save plots in png files
  ggsave(paste("output/Subset_cases", year.i[i],  ".png",sep=""), 
         dpi=300, width=6, height=5) # too heavy
}



#### Number of cases per countries and year in WE
########################################################################

table(subset_cases.df$COUNTRY, subset_cases.df$YEAR)

## Summarize in a dataframe
df1 <- table(subset_cases.df$COUNTRY, subset_cases.df$YEAR)
df2 <- as.data.frame(df1)
str(df2)
df2$Var2 <- as.numeric(levels(df2$Var2)[df2$Var2])

## Plot the graph
png("output/subset_countriesandyears_scp.png", 
    width=12, height=6, units="in", res=300)
ggplot(df2, aes(Var2,Freq)) + geom_line(aes(colour = Var1), size=2)+
  labs(x = "Years", y = "Number of cases") +
  theme(legend.text = element_text(size = 21))  +
  theme(axis.title.x = element_text(size = 22)) +
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 22)) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(plot.margin = unit(c(0.5,0.5,0.8,0.5), "cm"))+
  scale_x_continuous(breaks=seq(1987,2016,5))
dev.off()
