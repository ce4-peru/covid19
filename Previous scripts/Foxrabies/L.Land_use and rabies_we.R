##'**MRES PROJECT**
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'**MICAELA DE LA PUENTE L.**        
##'**This script shows find rabies coverage in WE using the land use raster** 

############################################################################

## Set working directory FOR RABIES CASES
rm(list=ls())

## Call the fox rabies dataset in WE.
foxrabies.we <-read.csv("output/foxrabies_final_full.csv")
foxrabies <-readOGR("output/foxrabies_final_full.shp","foxrabies_final_full") 
countries_zero <-readOGR("data/Boundaries data/welevel0.shp",
                         "welevel0")

## Call the land use raster
#land.raster <- raster("output/new_raster_nasa2000.tif")
fli.raster <- raster("output/new_raster2_fli_v3.tif")

######################################################################

## Set working directory FOR Land use
#setwd("C:/Users/Micaela/Dropbox/Fox Rabies_MD/Scripts")
source("K1.Land_use_type_we.r")

######################################################################

####  Rasterize again with the rabies cases points
## Use mask in rasterize for grids where cases are to keep their value
hr_raster3 <- rasterize(foxrabies, fli.raster, mask=TRUE) #
#hr_raster3 <- rasterize(foxrabies, hr_raster2, mask=TRUE) #
hr_raster3
# values      : 1, 7  (min, max) # THERE IS NO 3

## Plot it FLI
## Create legend
lab.legend.fli <- c("Artificial lands", 
                    "Agricultural", "Highly forested", 
                    "Moderately forested ", "Treeless and low forested",
                    "Wetlands")
col.legend.fli <- c("mediumorchid4", "yellow2", 
                    "forestgreen", "limegreen", "bisque3", "coral1")

## Create breakpoints
breakpoints.fli <- c(0.5, 1.5, 3.5, 4.5, 5.5, 6.5, 7.5) # create breaks to assign colors
colors.fli <- c("mediumorchid4", "yellow2",
                "forestgreen", "limegreen", "bisque3", "coral1")

plot(hr_raster3, breaks=breakpoints.fli, col=colors.fli, legend=FALSE)
legend <- legend("right", legend=lab.legend.fli, 
                 fill=col.legend.fli, 
                 x.intersp = .3, y.intersp = .6, cex = 0.75, 
                 xpd = TRUE, inset = c(-0.35,0), bty = "n")
# it shows rabies cases only by land use type
plot(hr_raster3, col=c("red"), colNA="blue", legend=FALSE) # shows all NA in blue

## Save the new raster
#writeRaster(hr_raster3, filename="rabies_raster_2000", overwrite=TRUE)
#writeRaster(hr_raster3, filename="rabies_raster_FLI", overwrite=TRUE)


#### Plot Options ####

## PLOT 1. Plot map of the raster with and without rabies cases
#pdf("landuse_rabies_we.pdf", onefile = TRUE)
#par(mfrow=c(2,1))

## Without cases
# plot(land.raster, legend=FALSE, main="Area of WE", 
#      breaks=breakpoints, col=colors)
# plot(countries_zero, add=TRUE)
# legend("right", legend=lab.legend, fill=col.legend, 
#        x.intersp = .5, y.intersp = .9, cex = 0.8, 
#        xpd = TRUE, inset = c(0,0), bty = "n")
# 
# ## With cases
# plot(land.raster, legend=FALSE, main="Area of rabies cases in WE", 
#      breaks=breakpoints, col=colors)
# plot(countries_zero, add=TRUE)
# plot(hr_raster3, add=TRUE, col="red2", legend=FALSE)
# legend("right", legend=lab.legend, fill=col.legend, 
#        x.intersp = .5, y.intersp = .9, cex = 0.8, 
#        xpd = TRUE, inset = c(0,0), bty = "n")

#dev.off()



####### ANALYSIS ####

## Call the raster
#setwd("C:/Users/Mica/Dropbox/Fox Rabies_MD/Land_use_rasters")
rabies_raster <- raster("output/rabies_raster_FLI") # Modified in QGIS
# Also I can continue using hr_raster3
#rabies_raster <- hr_raster3

## Create a pdf of map and raster of just rabies cases
## PLOT 2. MAP with raster and rabies cases
#pdf("rabies.raster.we.pdf", onefile = TRUE)
par(mfrow=c(1,1))
plot(countries_zero)
plot(rabies_raster, breaks=breakpoints.fli, col=colors.fli, legend=FALSE, add=TRUE)
legend("bottom", legend=lab.legend.fli, fill=col.legend.fli,
       x.intersp = .5, y.intersp = .9, cex = 0.8, 
       xpd = TRUE, inset = c(0,0.03), bty = "n", ncol=3)
#dev.off()
# with still could use hr_raster3 instead


###### ANALISIS #####

## Create a dataframe with values of infected grids by land use type
freq.rabies <- freq(rabies_raster)
class(freq.rabies) # matrix
freq.rabies.1 <- as.data.frame(freq.rabies)
print(freq.rabies.1$value)
#[1]   1  3  4  5  6  7 NA

str(freq.rabies.1$value)
#num [1:7] 1 3 4 5 6 7 NA

## Recode values
freq.rabies.1$value <- factor(freq.rabies.1$value)

freq.rabies.1$value <- recode(freq.rabies.1$value, "'1'='Artificial lands'; '3'='Agricultural'; '4'='Highly forested'; '5'='Moderately forested';'6'='Treeless and low forested'; '7'='Wetlands'; 'NA'='NA'")

str(freq.rabies.1$value)
#  Factor w/ 6 levels "Agricultural",..: 2 1 3 4 5 6 NA

## Sum row with the same level of land use

sum.inf <- tapply(freq.rabies.1$count, list(freq.rabies.1$value),
                sum, na.rm=TRUE)
sum.rabies.inf <- data.frame(sum.inf)

# 
# # for 2000
# a <- "Treeless and barren lands"
# a <- factor(a)
# barr <- data.frame(a,0)
# colnames(barr)[1] <- "value" # rename
# colnames(barr)[2] <- "count" # rename
# freq.rabies.1a <- freq.rabies.1
# freq.rabies.1b <- rbind(freq.rabies.1a, barr)
# ####
# sum.inf <- tapply(freq.rabies.1b$count, list(freq.rabies.1b$value),
#                   sum, na.rm=TRUE)
# sum.rabies.inf <- data.frame(sum.inf)
# 
# C <- sum.rabies.inf[c(1:4,6,5),]
# sum.rabies.inf <- as.data.frame(C)



#### Create a barplot ####
## PLOT 3. This shows number of infected grids per land use type
# fli
plot.c <-barplot(sum.rabies.inf$sum.inf,
                 ylab ="Number of grids with cases", 
                 ylim = c(0,12000), cex.lab=1.7, cex.axis=1.5,
                 cex.names=1.2,
                 col=c("yellow", "mediumorchid4", "forestgreen", 
                       "limegreen", "bisque3", "coral1"), 
                 main="Fox rabies cases by land use types", cex.main=2)
text(plot.c, 730, paste("n=", sum.rabies.inf$sum.inf), cex=1.5)

# # nasa 2000
# plot.c <-barplot(sum.rabies.inf$sum.inf,
#                  ylab ="Number of grids with cases", 
#                  ylim = c(0,12000), cex.lab=1.7, cex.axis=1.5,
#                  cex.names=1.2,
#                  col=c("yellow", "mediumorchid4", "chartreuse4", 
#                        "orange2", "bisque4", "turquoise2"), 
#                  main="Fox rabies cases by land use types", cex.main=2)
# text(plot.c, 730, paste("n=", sum.rabies.inf$sum.inf), cex=1.5)
# # This does not lead to any conclussion because does not include how much is the extent of each land use in WE.


## PLOT 4. This shows the percentage of infected grids per each land use type.
## Sum all the values
total.per <- sum(sum.rabies.inf$sum.inf)

## Calculate the percentages (new variable)
sum.rabies.inf$per.rabies <- (sum.rabies.inf$sum.inf/total.per*100)

## Round decimals
sum.rabies.inf$per.rabies1 <- round(sum.rabies.inf$per.rabies, digits=1)

# fli 
## Create the plot
#pdf("per_landuse1.pdf", onefile = TRUE,  width=13, height=6)
par(mfrow=c(1,1))
plot.p <-barplot(sum.rabies.inf$per.rabies1, 
                 width = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2),
                 axes=FALSE, axisname=FALSE,
                 ylim = c(0,55),
                 col=c("yellow", "mediumorchid4", "forestgreen", 
                       "limegreen", "bisque3", "coral1"), 
                 main="Fox rabies presence in Western Europe by land use types"
                 , cex.main=1.8)
axis(2, ylim = c(0,55), cex=1.8)
mtext("Percentage(%)",side=2,line=3, cex=1.6)
text(plot.p, par("usr")[3], labels = c("Artificial lands", 
                                       "Agricultural", "Highly forested", 
                                       "Moderately forested ", "Treeless and low forested",
                                       "Wetlands"), 
     #srt = 45,
     adj = c(0.5,1.2), xpd = TRUE, cex=1.5)
text(plot.p, 32, paste(sum.rabies.inf$per.rabies1), cex=1.6)


# nasa
## Create the plot
#pdf("per_landuse1.pdf", onefile = TRUE,  width=13, height=6)
# par(mfrow=c(1,1))
# plot.p <-barplot(sum.rabies.inf$per.rabies1, 
#                  width = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2),
#                  axes=FALSE, axisname=FALSE,
#                  ylim = c(0,35),
#                  col=c("yellow", "mediumorchid4", "chartreuse4", 
#                        "orange2", "bisque4", 
#                        "turquoise2"), 
#                  main="Fox rabies presence in Western Europe by land use types"
# , cex.main=1.8)
# axis(2, ylim = c(0,35), cex=1.8)
# mtext("Percentage(%)",side=2,line=3, cex=1.6)
# text(plot.p, par("usr")[3], labels = c("Croplands", "Dense\nsettlements", 
#                                    "Forested\nlands", "Rangelands",
#                                    "Tree and\nbarren lands", 
#                                    "Villages"), 
#      #srt = 45,
#      adj = c(0.5,1.2), xpd = TRUE, cex=1.5)
# text(plot.p, 32, paste(sum.rabies.inf$per.rabies1), cex=1.6)
# #dev.off()
### Show the the percentage of infected grids in each land type from the total of infected grids in WE. Villages, rangelands and croplands are the highest. 
# However, we are not considering the extent of each type land in WE. It could be that there is more croplands or Villages area in WE.







### PLOT 5. Maps with grids infected with rabies per land use during each year.

## Create the variable for the loop
year.i <- sort(unique(foxrabies$YEAR), decreasing = FALSE) 
year.i

## Create legend
lab.legend.fli <- c("Artificial lands", 
                    "Agricultural", "Highly forested", 
                    "Moderately forested ", "Treeless and low forested",
                    "Wetlands")
col.legend.fli <- c("mediumorchid4", "yellow2", 
                    "forestgreen", "limegreen", "bisque3", "coral1")

## Create breakpoints
breakpoints.fli <- c(0.5, 1.5, 3.5, 4.5, 5.5, 6.5, 7.5) # create breaks to assign colors
colors.fli <- c("mediumorchid4", "yellow2",
                "forestgreen", "limegreen", "bisque3", "coral1")

## Create a data frame to save results
result <- data.frame(YEAR=character(), VALUE=character(), COUNT=character(), 
                     stringsAsFactors=FALSE)

#pdf("rabies.raster.map.YEARS.pdf", onefile = TRUE)
par(mfrow=c(1,1))
for (i in seq_along(year.i))
  {
  data.rab.i <- foxrabies[foxrabies$YEAR==year.i[i],]
  
  # Rasterize rabies cases points
  hr_raster3.i <- rasterize(data.rab.i, hr_raster3, mask=TRUE) #
  
    # Calculate percentage of infected area
  freq.rabies.i <- freq(hr_raster3.i)
  freq.rabies.i.df <- as.data.frame(freq.rabies.i)
  freq.rabies.i.df$year <- year.i[i]
  result <- rbind(result, freq.rabies.i.df)
    
    # Plot the infected area
    plot(countries_zero, main=paste("Year", year.i[i]))
    plot(hr_raster3.i, breaks=breakpoints.fli, col=colors.fli, legend=FALSE, add=TRUE)
    legend("bottom", legend=lab.legend.fli, fill=col.legend.fli,
           x.intersp = .5, y.intersp = .9, cex = 0.8, 
           xpd = TRUE, inset = c(0,0.03), bty = "n", ncol=3)
    
    # Save the new raster
    #writeRaster(hr_raster3.i, filename=paste("rabies_raster", year.i[i]), 
    #            overwrite=TRUE)
    }
#dev.off()
  


### PLOT 6. Barplot of infected grids per land use during the years.

## Copy the result dataframe
result1 <- result
str(result1)
# 'data.frame':	110 obs. of  3 variables:
#   $ value: num  1 3 4 5 6 7 NA 1 3 4 ...
# $ count: num  92 873 573 54 23 ...
# $ year : num  1987 1987 1987 1987 1987 ...

## Factorize the value variable
result1$value <- factor(result1$value)
str(result1$value)

## Recode to land use levels
result1$value <- recode(result1$value, "'1'='Artificial lands'; #'2'='Villages'; 
                        '3'='Agricultural lands'; '4'='Highly Forested lands';
                        '5'='Moderated Forested lands';'6'='Treeless and barren lands'; 
                        '7'='Wetlands'; 'NA'='NA'")
print(levels(result1$value))

### Set the vector for the loop
year.i <- sort(unique(foxrabies$YEAR), decreasing = FALSE) 
year.i

#pdf("rabies.raster.area.YEARS.pdf", onefile = TRUE, width = 13, height = 7)
par(mfrow=c(1,1))

for (i in seq_along(year.i))
{
  data.rab.i <- result1[result1$year==year.i[i],] # subset only that year data
  
  sum.inf.i <- tapply(data.rab.i$count, list(data.rab.i$value),
                    sum, na.rm=TRUE)              # Sum rows with the same values
  sum.rabies.inf.i <- data.frame(sum.inf.i)
  
  plot.c.i <-barplot(sum.rabies.inf.i$sum.inf.i,
                   ylab ="Number of grids with cases", 
                   ylim = c(0,1700), cex.lab=1.5, cex.axis=1.3,
                   cex.names=1.2,
                   col=c("mediumorchid4", "yellow2",
                         "forestgreen", "limegreen", "bisque3", "coral1"), 
                   main=(paste("Fox rabies cases by land use types",
                               year.i[i])), cex.main=2)
  text(plot.c.i, 50, paste("n=", sum.rabies.inf.i$sum.inf.i), cex=1.5)
}
#dev.off()



###### LOGISTIC REGRESION #########

#source("J1.HR_we_landuse_raster.R")

## Add value of total number of grids per each land type uses in the tapply dataframe

sum.rabies.inf$type.count0 <- sum.rab.t.count1$sum.t.count1 # value from previous script
colnames(sum.rabies.inf)[1] <- "sum.inf" # rename

## Sustract total of grids minus infected grids
sum.rabies.inf$non.inf <- sum.rabies.inf$type.count0-sum.rabies.inf$sum.inf
sum.rabies.inf1 <- as.data.frame(sum.rabies.inf) # save dataframe with new name

## Create the variables for the analysis
free <- c(sum.rabies.inf1$non.inf)
infected <- c(sum.rabies.inf1$sum.inf)
total <- c(sum.rabies.inf1$type.count0)

print(sort(infected))

## Set the names of the levels
land.use <- data.frame(use=rep(c("Agricultural","Artificial lands", 
                                 "Highly forested", "Moderately forested",
                                 "Treeless and low forested",
                                 "Wetlands")), yes=infected, no=free)

land.use1 <- "cbind(yes,no) ~ use"     # bind yes and no levels


## Call the gml
glm.out = glm(land.use1, family=binomial(logit), data=land.use)

## See the anova
library(MASS)
options(show.signif.stars=F)         # turn off significance stars (optional)
anova(glm.out, test="Chisq")
# p value is 2.2e-16, that means that land use can explain variation in the 
# presence of rabies in WE lands (number of cases). Land use variable is significant.

## Look at the summary
summary(glm.out)
# Baseline is agricultural and its estimate is -0.59750 
# All land use levels are significant, but wetlands
# The intercept of moderated forested and low forested and wetlands are negative. 
# Highly forested and artificial are positive

confint(glm.out)
# no zeros
confint.default(glm.out) # with standard errors
# no zeros
# Waiting for profiling to be done...
# 2.5 %       97.5 %
#   (Intercept)                  -0.62868090 -0.566419663
# useArtificial lands           0.01155487  0.198931037
# useHighly forested            0.29479038  0.401339576
# useModerately forested       -0.74189802 -0.484859075
# useTreeless and low forested -0.66661605 -0.297671792
# useWetlands                  -0.80890925 -0.004729734

### PLOT 7. Barplot with percentage of area infected in each land use type.
library(scales)
library(reshape) # for melt function

lu <- land.use[c(-1)]

lu1 <- melt(cbind(lu, ind=rownames(lu)), id.vars = c('ind'))
ggplot(lu1,aes(x = ind, y = value,fill = variable)) + 
  geom_bar(position = "fill",stat = "identity") + 
  scale_y_continuous(labels = scales::percent)+
  #geom_bar(stat='identity') +
  coord_flip()+
  labs(x="Land use", 
    y = "Percentage") +
  theme(legend.text = element_text(size = 18))  +
  theme(axis.title.x = element_text(size = 20)) + 
  theme(axis.text.x = element_text(size = 18)) +
  theme(axis.title.y = element_text(size = 20)) + 
  theme(axis.text.y = element_text(size = 20)) +
  theme(legend.title=element_blank())+
  scale_fill_discrete(breaks=c("yes", "no"),
                      labels=c("With\nfox rabies", 
                               "Fox\nrabies free"))



### OBTAIN ODDS ###

## Villages vs Dense settlements
#exp(0.26319)
# [1] 1.301074 # Presence in dense settlements is 1.3 odd of presence in villages

exp(-0.12608) # 0.8815443????

## villages vs croplands
#exp(-1.05988)
# [1] 0.3464974 # Presence in croplands is only 0.35 odd of presence in villages.
exp(-0.25441) # 0.7753738

## villages vs Forested lands
#exp(-0.60476)
# [1] 0.5462055 # Presence in Forested lands is only 0.55 odd of presence in villages.
exp(-0.6326411)# 0.531187


## villages vs Rangelands
#exp(-0.41440)
# [1] 0.6607366 # Presence in Rangelands is only 0.66 odd of presence in villages.
exp(-0.11108) # 0.8948672


## Dense settlements vs Croplands
#exp(0.26319) / exp(-1.05988)
# [1] 3.754931 # Presence in Dense settlements is 3.75 odd of presence in Croplands.  
exp(-0.12608) / exp(-0.25441) # 1.136928


## Dense settlements vs Forested lands 
#exp(-0.12608) / exp(-0.60476)
# [1] 2.382023 # Presence in Dense settlements is 2.4 odd of presence in Forestes.
exp(-0.12608) / exp(-0.52946) # 1.496876


## Dense settlements vs Rangelands  
#exp(0.26319) / exp(-0.41440)
# [1] 1.969126 # Presence in Dense settlements is 2 odd of presence in Rangelands. 
exp(-0.12608) / exp(-0.11108) # 0.9851119

# http://ww2.coastal.edu/kingw/statistics/R-tutorials/logistic.html


# sum.rabies.inf1
land.use2 <- data.frame(use=rep(c("Agriculture", "Urban", "Dense forest", 
                                 "Moderate forest", "Barren", "Wetlands")), 
                        yes=infected, total=total)
mymat <- t(land.use2[-1])
colnames(mymat) <- land.use2[, 1]

# par(oma=c(0.5,1,0,0) )
# par(mar=c(7,4,3,3))

mp <- barplot(mymat, beside=T, col=c("black", "grey"), 
       names.arg=c("Agriculture","Urban", 
                    "Dense \nforest", "Moderate \nforest",
                   "Barren", "Wetlands"), 
        #space=c(0,0.2), width=1, 
       axes = FALSE, #axisnames = FALSE, 
       #las=2, srt=65, ylim=c(0,18000), 
       cex.names=1)
# labels <- c("Agriculture", "Urban", "Dense forest",
#             "Moderate forest", "Barren", "Wetlands")
# text(mp[1,],  par("usr")[3], labels = labels, #srt = 65,
#      adj = c(2, 2),
#      xpd = TRUE, cex=.9, pos=1
#   )
axis(2, #ylab = "Number of grids",
     at=c(0,3000,6000,9000,12000,15000,18000), cex.axis=1.1# ylim=c(0,18000)
     )
mtext(side=2,text="Gridcells",line=2.2, cex=1.2)
legend(6,15000, c("Infected area", "Total area"),
       pt.bg=c("black", "grey"), bty="n", y.intersp=0.8, x.intersp=0.6, pch=c(22,22), 
       cex = 1)


#### the same than last but colorful
mp1 <- barplot(mymat, beside=T, col=c("red", "yellow2", 
                                      "red", "mediumorchid4",
                                      "red", "forestgreen", 
                                      "red", "limegreen",
                                      "red", "bisque3", 
                                      "red", "coral1"), 
              names.arg=c("Agriculture","Urban", 
                          "Dense \nforest", "Moderate \nforest",
                          "Barren", "Wetlands"), 
              axes = FALSE, #axisnames = FALSE, 
              cex.names=1)
axis(2, #ylab = "Number of grids",
     at=c(0,3000,6000,9000,12000,15000,18000), cex.axis=1.1# ylim=c(0,18000)
)
mtext(side=2,text="Gridcells",line=2.2, cex=1.2)
legend(6,15000, c("Infected area", "Total area"),
       pt.bg=c("red", "yellow2", 
               "red", "mediumorchid4",
               "red", "forestgreen", 
               "red", "limegreen",
               "red", "bisque3", 
               "red", "coral1"), 
       bty="n", y.intersp=0.8, x.intersp=0.6, pch=c(22,22), 
       cex = 1)


###

mp1 <- barplot(mymat, beside=T, col=c("red", "grey", 
                                      "red", "grey",
                                      "red", "grey", 
                                      "red", "grey",
                                      "red", "grey", 
                                      "red", "grey"), 
               names.arg=c("Agriculture","Urban", 
                           "Dense \nforest", "Moderate \nforest",
                           "Barren", "Wetlands"), 
               axes = FALSE, #axisnames = FALSE, 
               cex.names=1)
axis(2, #ylab = "Number of grids",
     at=c(0,3000,6000,9000,12000,15000,18000), cex.axis=1.1# ylim=c(0,18000)
)
mtext(side=2,text="Gridcells",line=2.2, cex=1.2)
legend(6,15000, c("Infected area", "Total area"),
       pt.bg=c("red", "grey", 
               "red", "grey",
               "red", "grey", 
               "red", "grey",
               "red", "grey", 
               "red", "grey"), 
       bty="n", y.intersp=0.8, x.intersp=0.6, pch=c(22,22), 
       cex = 1)


