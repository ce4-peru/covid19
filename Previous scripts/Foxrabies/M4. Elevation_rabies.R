##'**MRES PROJECT**
##'
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'
##'**MICAELA DE LA PUENTE L.**        
##'          
##'**This script describes fox rabies according to elevation** 
#############################################################

## Call the packages
library(raster)
library(ggplot2)
library(ggmap)
library(mapproj)
library(Rcpp)
library(foreign)
library(maptools)
library(sp)
library(rgdal)
library(berryFunctions)

###################################################

## Call the shp with rabies cases.
foxrabies <-readOGR("output/foxrabies_final_full.shp","foxrabies_final_full") 

## Call the elevation script
source("M3.Elevation_distribution.r")

#### Rasterize with fox rabies data ####

ele_raster3.lr <- rasterize(foxrabies, elev.strm, mask=TRUE)
# dimensions  : 207, 335, 69345 (nrow, ncol, ncell)
# resolution  : 0.06666667, 0.06666667  (x, y)
# values      : -2.500012, 3366.998  (min, max)
plot(ele_raster3.lr) # you see tiny points.

## Built a dataframe
ele.count3.lr <- freq(ele_raster3.lr)
ele.count3.df.lr <- as.data.frame(ele.count3.lr)
#ele.count3.df.lr <- ele.count3.df.lr[-c(2185), ] # delete NA
ele.count3.df.lr <- ele.count3.df.lr[-c(1640), ] # delete NA # papr

#ele.count3.df.lr <- ele.count3.df.lr[-c(1:4), ] # negative values until -4
ele.count3.df.lr$value <- ele.count3.df.lr$value

## Convert to raw data
ele.count3.df.lr.raw <- data.frame(value = rep(ele.count3.df.lr$value, 
                                               times=ele.count3.df.lr$count),
                                   count = rep(1, 
                                               times=sum(ele.count3.df.lr$count)))

## Check the summary
summary(ele.count3.df.lr.raw$value)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       -3.0   116.0   263.0   385.6   460.0  3367.0 
# The highest point of the WE territory with fox rabies cases is 3367.0.
# At least 75% of the cases is WE area is  under 510 m.a.s.l.


## Create the histogram of rabies distribution by altitude
seq2 <- seq(-100,3600, by=100)
hist(ele.count3.df.lr.raw$value, breaks=seq2, 
     ylim=c(0,3000), xlim=c(0, 4000),
     col="red",
     xlab="Elevation", cex.lab=1.5, cex.axis=1.3)


#### MIX BOTH HISTOGRAMS ####

## SHOW DENSITY AND Y AXIS
seq2 <- seq(-200,4600, by=100)
hist(ele.count2.df.lr.raw$value, breaks=seq2, 
     xlim=c(0, 4600),
     col="lightgreen",
     xlab="Elevation", cex.lab=1.5, cex.axis=1.3, freq=FALSE)

# install package 'berryFunctions'
newcolor <- addAlpha("red", alpha = 0.6)
#newcolor <- addAlpha("red", c(0.4, 0.4, 0.4, 0.4)) # to create an intersection color
hist(ele.count3.df.lr.raw$value, breaks=seq2,
     col="red",
     # #col=newcolor,
     xlab="Elevation", cex.lab=1.5, cex.axis=1.3,
     add=TRUE, freq=FALSE)

## Add just line to histogram
d1 <- density(ele.count2.df.lr.raw$value) # returns the density data 
d2 <- density(ele.count3.df.lr.raw$value) # returns the density data 
lines(d1, lwd=2)
lines(d2, lwd=2, col="blue")



#### SHOW ONLY LINES WITH DENSITY PLOT KERNEL ####
plot(d1, col="green", lwd=2, #cex.xlab=1.2, 
     xlab="Elevation (m2)",
     cex.lab=1.2, 
     cex.axis=1) # plots the results
lines(d2, col="red", lwd=2)
legend("topright", c("Fox rabies cases","Western Europe "), 
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("red", "green"))

# 
# hist(fox_rab1$Rabies_R,xlab="Fox rabies intensity (Rabies_R)", 
#      seq(0,3.2,by=0.01), ylim=c(0,10000),
#      cex.lab=1.5, 
#      cex.axis=1.3
#      ) # plots the results

#### SHOW POLYGONS WITH DENSITY PLOT KERNEL ####
plot(d1) # plots the results
polygon(d1, col="green", border="black", lwd=2)
#newcolor <- addAlpha("red", alpha = 0.7) # to create an intersection color
#polygon(d2, col=newcolor, border="blue", lwd=2, add=TRUE)
polygon(d2, col="red", border="blue", lwd=2, add=TRUE)





##### ANALISIS  ####

#pdf("elevation.pdf", onefile = TRUE)
count <- ele.count3.df.lr$count
value <- ele.count3.df.lr$value

plot(value, count, ylim=c(0,100))
fitline <- lm(count~value)
abline(fitline, col="red")

plot(count,value)
fitline <- lm(value~count)
abline(fitline, col="red")
#dev.off()


# model
ele.mod <- glm(count ~ value, data = ele.count3.df.lr, family = poisson)
summary(ele.mod)
#### ... $density) at the end of hist



#### multipanel 2
png("output/multipanel2_v15_b&w.png", width=11, height=5, units="in", res=600)

par(mfrow=c(1,2), mai = c(0.5, 1, 0.2, 0.1), oma = c(0,0,0,0)#, 
    #plt=c(0.25, 0.20, 0, 0.045)#, mar = c(2,2,0.2,0.2)
    )
m <- rbind(c(1,2))
print(m)
layout(m, widths=c(3,4))

## plot1
#### SHOW ONLY LINES WITH DENSITY PLOT KERNEL ####
plot(d1, col="black", lwd=2, #cex.xlab=1.2, 
     #cex.lab=1.2,
     xlab=NA, ylab=NA,
     cex.axis=1, main=NA, #frame.plot=FALSE,
     bty="n", axes = FALSE)
title(xlab="Elevation (m)", line = 1, cex=1.2)
title(ylab="Density", cex=1.2)
axis(2, at=c(0,0.0005,0.0010,0.0015,0.0020,0.0025,0.0030), cex.axis=1.1, 
     tck=-0.02)# plots the results
axis(1, pos=0, cex.axis=1, mgp=c(1,0.5,0), tck=-0.01)
lines(d2, col="black", lwd=2, lty=2)
legend(1500, 0.0024, c("Altitude", "Fox rabies cases"), 
       lty=c(1,2), # gives the legend appropriate symbols (lines)
       lwd=c(2,2),col=c("black", "black"), bty = "n", seg.len=1.5, x.intersp=0.6,
       y.intersp = 0.8, cex=1)

# Add barplot of land use in L.

dev.off()


##color
par(mfrow=c(1,2), mar=c(0,0,0,0), 
    mai = c(0.5, 1, 0.2, 0.1), oma = c(0,0,0,0)#, 
    #plt=c(0.25, 0.20, 0, 0.045)#, mar = c(2,2,0.2,0.2)
)
m <- rbind(c(1,2))
print(m)
layout(m, widths=c(3,4))

## plot1
#### SHOW ONLY LINES WITH DENSITY PLOT KERNEL ####
plot(d1, col="green4", lwd=2.5, #cex.xlab=1.2, 
     #cex.lab=1.2,
     xlab=NA, ylab=NA,
     cex.axis=1, main=NA, #frame.plot=FALSE,
     bty="n", axes = FALSE)
title(xlab="Elevation (m)", line = 1, cex=1.2)
title(ylab="Density", cex=1.2)
axis(2, at=c(0,0.0005,0.0010,0.0015,0.0020,0.0025,0.0030),
     cex.axis=1.1, 
     tck=-0.02)# plots the results
axis(1, pos=0, cex.axis=1, mgp=c(1,0.5,0), tck=-0.01)
lines(d2, col="red", lwd=2.5, lty=2)
legend(1500, 0.0024, c("Altitude", "Fox rabies cases"), 
       lty=c(1,2), # gives the legend appropriate symbols (lines)
       lwd=c(2,2),col=c("green4", "red"), bty = "n", seg.len=1.5, x.intersp=0.6,
       y.intersp = 0.8, cex=1)