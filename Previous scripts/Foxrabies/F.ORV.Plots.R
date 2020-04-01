##'**MRES PROJECT**
##'
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'
##'**MICAELA DE LA PUENTE L.**        
##'          
##'**This script shows explore the ORV dataset** 
################################################

## Load packages from the R library
library(ggplot2)
library(Rcpp)
library(foreign)
library(rgdal)
###############################################

## Call the ORV shp and dataframe
ORV_we <-readOGR("output/ORV_we_final1.shp","ORV_we_final1")

#remember = (F(eder) = Spring, H(erbst) = Autumn, S(ommer) =  Summer)
# "NAME": Name of the vaccination campaign (F(eder) = Spring, H(erbst) = Autumn, S(ommer) =  Summer)
# "LAND": Country
# "JAHR": Year
# "JA_ZEIT": Time or Season
# "FIKT_DATE": Mid date (fictional date)
# "VON_DATE": Start date
# "BIS_DATE": End date
# "AUSLAGEART": Type of delivery
# "KOED_DICHT": bait density
# "IMPFSTOFF": vaccine type
# "FLAECHE": area covered
# "FLUG_LI_AB": flight line distance
# "KOED_ANZ": ??
# "DAV_FLUG": area covered by flight
# "DAV_HAND": area covered by hand
# "Shape_Leng": length of area covered
# "Shape_Area": Total area or circumference?

#ORV_we.df <-read.csv("output/ORV_we.df_final.csv")
ORV_we.df <-read.csv("output/ORV_we.df_final1.csv")


############## NUMBER OF CASES BY COUNTRIES ##################

#### Number of ORV campaigns per country in WE
#png("output/ORV_countries_bp.png", width=12, height=4, units="in", res=300)
numbers <- table(ORV_we.df$LAND)
plot <- plot(ORV_we.df$LAND, ylim = c(0,500), ylab ="Number of ORV campaings")
text(plot, 90, paste("n=", numbers), cex=0.8)
#dev.off()
## Most campaings are in Germany. Netherlands only have 8.


#### Number of ORV campaigns per year in WE
numbers <- table(ORV_we.df$JAHR)
#ORV_we.df$JAHR <- factor(ORV_we.df$JAHR)
png("output/ORV_years_scp.png", width=10, height=4, 
    units="in", res=300)
plot <- plot(numbers, 
             ylab ="Number of ORV campaings", 
             type="o", col="blue", lwd=2)
dev.off()
# Number of ORV campaigns varies through years.


#### Number of campaings per season
ORV_we.df$VAC <- factor(ORV_we.df$VAC)
numbers <- table(ORV_we.df$VAC)
png("output/ORV_seasonss_bp.png", width=10, height=4, 
    units="in", res=300)
plot <- plot(ORV_we.df$VAC, 
             ylab ="Number of ORV campaings", ylim=c(0,400),
             names=c("Spring", "Summer", "Autumn"))
title(main="Western Europe", cex.lab=1.3)
text(plot, 50, paste("n=", numbers), cex=0.8)
dev.off()
## Only 27 campaigns in Summer. No campaigns in Winter.


#### Calculate the total area covered by all ORV campaings in each country
## Sum the areas
sum.c <- tapply(ORV_we.df$FLAECHE, list(ORV_we.df$LAND),
                sum, na.rm=TRUE)
sum.ORV.c <- data.frame(sum.c)
sum.ORV.c$sum.c <- round(sum.ORV.c$sum.c, digits = 1)

## Plot the graph
png("output/ORV_areabycountry_bp.png", width=12, height=4, 
    units="in", res=300)
plot.c <-barplot(sum.ORV.c$sum.c,
                 ylab ="Total area covered (km2)", 
                 ylim = c(0,4500000), col = "grey")
text(plot.c, 500000, paste("n=", sum.ORV.c$sum.c), cex=0.8)
dev.off()
# Germany had higher total of area covered by ORV.


#### Find total of area covered by ORV in WE
sum <- sum(sum.ORV.c$sum.c)
sum #  6395800 from 1983


#### Calculate the total area covered by all ORV campaings 
# in each year
sum.y <- tapply(ORV_we.df$FLAECHE, list(ORV_we.df$JAHR),
                sum, na.rm=TRUE)
sum.ORV.y <- data.frame(sum.y)
sum.ORV.y$sum.y1 <- round(sum.ORV.y$sum.y, digits = 1)

png("output/ORV_areabyyear_scp2.png", width=15, height=6, 
    units="in", res=300)
par(mar = c(2,8,1,0.5), 
    oma = c(1,1,0,0.5),
    mgp = c(8,1,0))

plot.y <-plot(sum.y, 
              type="o", col="black", lwd=3,
              ann=FALSE, axes=FALSE)
axis(1, at=1:29, lab=c(1978:2006), cex.axis=1.2, cex.lab=1.3)
axis(2, at = seq(0, 800000, by = 100000, cex=.8, cex.axis=.8, 
                 cex.tick=.8), las=2)
box()
title(ylab ="Total area covered (km2)", #cex.sub=2, 
      cex.lab=1.3)
dev.off()
## 1992 ORV covered more more area.



#### Calculate the total area covered by all ORV campaings 
# in each semester
ORV_we.df$SEMESTER1 <- ORV_we.df$SEMESTER + 1
sum.y <- tapply(ORV_we.df$FLAECHE, list(ORV_we.df$SEMESTER1),
                sum, na.rm=TRUE)
sum.ORV.y <- data.frame(sum.y)
sum.ORV.y <- rbind(sum.ORV.y, 0)
sum.ORV.y <- data.frame(sum.ORV.y[c(58,1:57),])
colnames(sum.ORV.y)[1] <- "sum.y"
sum.ORV.y$sum.y1 <- round(sum.ORV.y$sum.y, digits = 1)

png("output/ORV_areabysemester_scp.png", width=15, height=8, 
    units="in", res=300)
par(mar = c(5,8,1,0.5), oma = c(1,1,0,0.5), mgp = c(5,1,0))

plot.y <-plot(sum.ORV.y$sum.y1, 
              type="o", col="black", lwd=3,
              ann=FALSE, axes=FALSE)
axis(1, at=1:58, lab=c(1:58), cex.axis=1.4, cex.lab=1.5)
axis(2, at = seq(0, 450000, by = 50000, cex=.9, cex.axis=.9, 
                 cex.tick=.9), las=2)
box()
title(ylab ="Total area covered (km2)", #cex.sub=2, 
      cex.lab=1.5)
dev.off()


# set for legend
vector.year <- rep(c(1978:2006),each=2)
vector.semester <- rep(c(1:2), 29)

# png("output/ORV_areabysemester_scp.png", width=15, height=6, 
#     units="in", res=300)
# par(mar = c(5,8,1,0.5), oma = c(1,1,0,0.5), mgp = c(5,1,0))
# 
# plot.y <-plot(sum.y, 
#               type="o", col="black", lwd=3,
#               ann=FALSE, axes=FALSE)
# axis(1, at=1:57, lab=c(1:57), cex.axis=1.2, cex.lab=1.3)
# axis(2, at = seq(0, 450000, by = 50000, cex=.8, cex.axis=.8, 
#                  cex.tick=.8), las=2)
# box()
# title(ylab ="Total area covered (km2)", #cex.sub=2, 
#       cex.lab=1.3)
# dev.off()



#### Calculate the total area covered by all ORV campaings in each season
sum.s <- tapply(ORV_we.df$FLAECHE, list(ORV_we.df$VAC),
                sum, na.rm=TRUE)
sum.ORV.s <- data.frame(sum.s)
sum.ORV.s$sum.s1 <- round(sum.ORV.s$sum.s, digits = 1)

png("output/ORV_areabyseason_bp.png", width=8, height=6, 
    units="in", res=300)
plot.s <-barplot(sum.ORV.s$sum.s1,
              xlab ="Total area covered (km2)", xlim = c(0, 4000000),
              col="blue", 
              names.arg =c("Spring", "Summer", "Autumn"),
              horiz = TRUE)
box()
dev.off()
## Spring and autumn between 400 000 and 500 000 km2.
