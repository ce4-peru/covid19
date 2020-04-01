##'**LAURIE'S PROJECT**
##'**MICAELA DE LA PUENTE L.**
##'**This script shows subset of ORV data**

######################################################

library(ggplot2)
library(ggmap)
library(mapproj)
library(Rcpp)
library(foreign)
library(maptools)
library(maps)
library(mapdata)
library(devtools)
library(animation)
library(rgdal)

# FIRST SET WORKING DIRECTORY IN RSTUDIO
# i.e. Session > Set Working Directory > To Source File Location
rm(list=ls())

## Call the script where we reproject the data.
source("4_Exploration_ORV.r")

######################################################

# Subset
unique(europe_ORV@data$LAND)
europe_ORV_subset <- subset(europe_ORV, LAND %in% c("GERMANY", "CZECH_REPUBLIC", 
                                                    "POLAND"))
levels(europe_ORV_subset$LAND)
europe_ORV_subset$LAND <- factor(europe_ORV_subset$LAND)
levels(europe_ORV_subset$LAND)

## Explore dataset
str(europe_ORV_subset@data)  # 920 obs. of  18 variables:
sort(unique(europe_ORV_subset@data$JAHR), decreasing = FALSE)
#[1] 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998
#[17] 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013
## Only from 1983 to 2013 ###

# Transform the subset the a normal dataframe
europe_ORV_subset.df <- as.data.frame(europe_ORV_subset)

## Save the new spatial points data frame and standard dataframe & KMLs!
#writeOGR(europe_ORV_subset, dsn=".", layer="output/subset_ORV", 
#         driver="ESRI Shapefile")
#write.csv(europe_ORV_subset.df, file = "output/subset_ORV.df.csv")
#writeOGR(europe_ORV_subset, dsn=paste("output/subset_ORV", ".kml", sep=""), 
#         layer="ID", driver="KML")


### Plots

#### Number of ORV campaigns per country in WE
numbers <- table(europe_ORV_subset.df$LAND)
png("output/ORV_percountry.png", width=6, height=4, units="in", res=300)
plot <- plot(europe_ORV_subset.df$LAND, ylim = c(0,600), 
             ylab ="Number of ORV campaings")
text(plot, 100, paste("n=", numbers), cex=0.8)
dev.off()
## Most campaings are in Germany and Poland.



#### Number of ORV campaigns per year in WE
numbers <- table(europe_ORV_subset.df$JAHR)
png("output/ORV_peryear.png", width=6, height=4, units="in", res=300)
plot <- plot(numbers, 
             ylab ="Number of ORV campaings", 
             type="o", col="blue", lwd=2)
dev.off()
# Number of ORV campaigns varies through years.


#### Number of campaings per season
europe_ORV_subset.df$VAC <- factor(europe_ORV_subset.df$VAC)
numbers <- table(europe_ORV_subset.df$VAC)
png("output/ORV_perseason.png", width=6, height=4, units="in", res=300)
plot <- plot(europe_ORV_subset.df$VAC, 
             ylab ="Number of ORV campaings", ylim=c(0,500),
             names=c("Spring", "Summer", "Autumn"))
title(main="Western Europe", cex.lab=1.3)
text(plot, 100, paste("n=", numbers), cex=0.8)
dev.off()
## Only 5 campaigns in Summer. No campaigns in Winter.


#### Calculate the total area covered by all ORV campaings in each country
## Sum the areas
sum.c <- tapply(europe_ORV_subset.df$FLAECHE, list(europe_ORV_subset.df$LAND),
                sum, na.rm=TRUE)
sum.ORV.c <- data.frame(sum.c)
sum.ORV.c$sum.c <- round(sum.ORV.c$sum.c, digits = 1)

## Plot the graph
png("output/ORV_areapercountry.png", width=6, height=4, units="in", res=300)
plot.c <-barplot(sum.ORV.c$sum.c,
                 ylab ="Total area covered (km2)", 
                 ylim = c(0,10000000), col = "grey")
text(plot.c, 1000000, paste("n=", sum.ORV.c$sum.c), cex=0.8)
dev.off()
# Germany had higher total of area covered by ORV.


#### Find total of area covered by ORV in WE
sum <- sum(sum.ORV.c$sum.c)
sum # 15845934 from 1987

#### Calculate the total area covered by all ORV campaings in each year
sum.y <- tapply(europe_ORV_subset.df$FLAECHE, list(europe_ORV_subset.df$JAHR),
                sum, na.rm=TRUE)
sum.ORV.y <- data.frame(sum.y)
sum.ORV.y$sum.y1 <- round(sum.ORV.y$sum.y, digits = 1)

png("output/ORV_areaperyear.png", width=6, height=4, units="in", res=300)
plot.y <-plot(sum.y, 
              type="o", col="blue", lwd=2,
              ann=FALSE, axes=FALSE)
axis(1, at=1:31, lab=c(1983:2013))
axis(2, at = seq(0, 3000000, by = 500000))
box()
title(main="Total area covered per year (km2)", ylab ="area covered (km2)")
dev.off()
## 2005 ORV covered more more area.
