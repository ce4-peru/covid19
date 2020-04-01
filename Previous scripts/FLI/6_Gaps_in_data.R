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
foxrabies <-readOGR("output/europe_cases.shp", 
                    "europe_cases") 
df <- as.data.frame(foxrabies)
str(foxrabies)
sort(unique(foxrabies$Qs))
#[1]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58
#[23]  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80
#[45]  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102
#[67] 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124
#[89] 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146
#[111] 147 148 149 150 151 152 153 154


table(foxrabies$QUARTER)
#     0     1     2     3     4 
#     9   50352 28473 28961 39845 
# 9 cases in Turkey with Quarter 0!


## Sum number of cases per quarter timestep (Qmarize cases by quarter.
rabies.qs <- hist(foxrabies$Qs, breaks=36:154, plot=FALSE)$counts

plot.qs <- plot(rabies.qs, type="o", col="red", axes=FALSE,
                lwd=1.5, pch=19, ann=FALSE, cex=0.7)
axis(1, lab=c(37:156), at=1:120, cex=1.2)
axis(2, #at = seq(0, 5000, by = 500), 
     cex=1.2, las=2)
box()
title(main="Incidence of fox rabies in Europe", xlab="Quarters",
      ylab="Number infected foxes", cex=1.3)




## Find out gaps per countries

table(foxrabies@data$COUNTRY, foxrabies@data$QUARTER, 
      foxrabies@data$YEAR)

a <- as.data.frame(table(foxrabies@data$COUNTRY, foxrabies@data$QUARTER, 
                         foxrabies@data$YEAR))
# Appear now Quarter 0 for all countries. Delete Turkey 2009 Q0 for this purpose only.

foxrabies1 <-foxrabies[-which(foxrabies$COUNTRY=="Turkey" &
                                         foxrabies$YEAR == 2009 &
                                foxrabies$QUARTER == 0),]


# Built the dataframe
quarter_timestep <- as.data.frame(table(foxrabies1@data$COUNTRY, foxrabies1@data$QUARTER, 
                         foxrabies1@data$YEAR))
colnames(quarter_timestep)[1] <- "COUNTRY" # quarter list
colnames(quarter_timestep)[2] <- "QUARTER" # semester list
colnames(quarter_timestep)[3] <- "YEAR" # semester list
colnames(quarter_timestep)[4] <- "COUNT" # semester list

# Convert Quarter and year to numeric
str(quarter_timestep)
quarter_timestep$YEAR <- as.numeric(levels(quarter_timestep$YEAR)[quarter_timestep$YEAR])
quarter_timestep$QUARTER <- as.numeric(levels(quarter_timestep$QUARTER)[quarter_timestep$QUARTER])

# Create timesteps
quarter_timestep$Qs <- (4*(quarter_timestep$YEAR-1987)) + quarter_timestep$QUARTER + 36

back <- quarter_timestep
quarter_timestep <- back 

# Eliminate quarters before they start having cases.
we.c.i <- as.factor(sort(unique(quarter_timestep$COUNTRY), decreasing = FALSE))
we.c.i # 34 Levels of countries

for(i in levels(we.c.i))
  {
  # subset country data
  print(i)
  country_gap <- quarter_timestep[which(quarter_timestep$COUNTRY == i),]
  country_gap1 <- country_gap[which(country_gap$COUNT > 0),]

  # check timesteps in the country
  timestep <-sort(unique(country_gap1$Qs), decreasing = FALSE)
  #timestep
  timestep_1 <- timestep[1] # 37
  print(timestep_1)
  
  if (timestep_1 != 37){
    # eliminate from the dataset timesteps before the first timestep for that country
    quarter_timestep1 <- quarter_timestep[-which(quarter_timestep$COUNTRY == i & 
                                                   quarter_timestep$Qs < timestep_1),]
    quarter_timestep <- quarter_timestep1 # replace the with new dataset 
    
  } else {
    print("do nothing")    
  }
  }


### Eliminate a year after the last quarter reported

for(i in levels(we.c.i))
{
  # subset country data
  print(i)
  country_gap <- quarter_timestep[which(quarter_timestep$COUNTRY == i),]
  country_gap1 <- country_gap[which(country_gap$COUNT > 0),]
  
  # check timesteps in the country
  timestep <-sort(unique(country_gap1$Qs), decreasing = FALSE)
  #timestep
  timestep_last <- tail(timestep,1)
  print(timestep_last)
  
  if (timestep_last < 156-4){
    # eliminate from the dataset timesteps before the first timestep for that country
    quarter_timestep1 <- quarter_timestep[-which(quarter_timestep$COUNTRY == i & 
                                                   quarter_timestep$Qs > (timestep_last+4)),]
    quarter_timestep <- quarter_timestep1 # replace the with new dataset 
    
  } else {
    print("do nothing")    
  }
}


## Keep only the countries of interest
# Albania, Austria, Belarus, Belgium, Bulgaria, Croatia, Czech Republic, Estonia, 
# Finland, France, Germany, Greece, Hungary, Italy, Kosovo, Latvia, Lithuania, 
# Luxembourg, Macedonia, Monaco, Montenegro, Moldova, Netherlands, Poland, Portugal, 
# Romania, Serbia, Slovakia, Slovenia, Spain, Ukraine, Russia

sort(unique(quarter_timestep$COUNTRY), decreasing = FALSE)
# [1] Albania              Austria              Belarus              Belgium             
# [5] Bosnia - Hercegovina Bulgaria             Croatia              CS                  
# [9] Czech Republic       Estonia              Finland              France              
# [13] Georgia              Germany              Greece               Hungary             
# [17] Italy                Latvia               Lithuania            Luxembourg          
# [21] Macedonia            Moldova              Montenegro           Poland              
# [25] Romania              Russian Federation   Serbia               Slovak Republic     
# [29] Slovenia             Spain                Switzerland          The Netherlands     
# [33] Turkey               Ukraine             

# Delete Bosnia - Hercegovina, CS, Georgia, Turkey 
# There is no cases Kosovo or Monaco or Portugal 
dataframe1 <- quarter_timestep[-which(quarter_timestep$COUNTRY %in% c("Bosnia - Hercegovina",
                                                                  "CS",
                                                                  "Georgia",
                                                                  "Turkey")),]


### SUMMARIES ###
# Summary 1: Countries counts each quarter timestep.
# Keeps only timesteps counts from first to last quarter with counts in each country.
dataframe1
# In Albania and Bulgaria gaps looks more like no cases quarters.
#write.csv(dataframe1, file = "output/gaps_dataframe1.csv")

# Summary 2: Countries timesteps with 0 counts
# Keep only timesteps with 0 counts in each country
dataframe2 <- dataframe1[-which(dataframe1$COUNT != 0),]
#write.csv(dataframe2, file = "output/gaps_dataframe2.csv")

# Summary 3: How many quarters reported in each year per country
dataframe3 <- as.data.frame(table(dataframe1$COUNTRY, 
                                  dataframe1$YEAR))
#write.csv(dataframe3, file = "output/gaps_dataframe3.csv")


## Plots for all countries ##
we.c.i <- as.factor(sort(unique(foxrabies@data$COUNTRY), decreasing = FALSE))
we.c.i

pdf("output/rabies_countriesbyquarters_scp.pdf", width=10, height=4, onefile = TRUE)

## Set the for loop
for(i in levels(we.c.i))
  {
  country_gap_check <- subset(foxrabies, COUNTRY == i)
  print(i)
  print(table(country_gap_check$Qs))

  ## Sum number of cases per quarter timestep (Qmarize cases by quarter.
  rabies.qs <- hist(country_gap_check$Qs, breaks=36:154, plot=FALSE)$counts
  
  plot.qs <- plot(rabies.qs, type="o", col="red", axes=FALSE,
                lwd=1.5, pch=19, ann=FALSE, cex=0.4)
  axis(1, lab=c(37:156), at=1:120, cex=1.2)
  axis(2, #at = seq(0, 5000, by = 500), 
       cex=1.2, las=2)
  box()
  title(main=i, xlab="Quarters",
        ylab="Number infected foxes", cex=1.3)
  }
dev.off()



# Compare dataframe2 to european rabies dataset.
# Check if in quarters with 0 fox rabies cases, 
#     there were or not rabies cases in  other species

# Prepare the full rabies dataset
rabies.data <-readOGR("Cases/Rabies_WGS84_Singlepart.shp", 
                      "Rabies_WGS84_Singlepart") 
names(rabies.data)
# [1] "YEAR"     "QUARTER"  "COUNTRY"  "REGION"   "SPECIES"  
# "IMPORTED"

# Built the dataframe
df <- as.data.frame(rabies.data)
rabies.data.df <- as.data.frame(table(rabies.data@data$COUNTRY, 
                                      rabies.data@data$QUARTER, 
                                        rabies.data@data$YEAR))
colnames(rabies.data.df)[1] <- "COUNTRY" # quarter list
colnames(rabies.data.df)[2] <- "QUARTER" # semester list
colnames(rabies.data.df)[3] <- "YEAR" # semester list
colnames(rabies.data.df)[4] <- "COUNT" # semester list

# Convert Quarter and year to numeric
str(rabies.data.df)
rabies.data.df$YEAR <- as.numeric(levels(rabies.data.df$YEAR)[rabies.data.df$YEAR])
rabies.data.df$QUARTER <- as.numeric(levels(rabies.data.df$QUARTER)[rabies.data.df$QUARTER])

# Keep only data from 1987 onwards
rabies.data.df <- rabies.data.df[which(rabies.data.df$YEAR >1986),]
# Create timesteps
rabies.data.df$Qs <- (4*(rabies.data.df$YEAR-1987)) + rabies.data.df$QUARTER + 36



we.c.i <- as.factor(sort(unique(dataframe2$COUNTRY), decreasing = FALSE))
we.c.i # 34 Levels of countries

# Call the rabies dataset

trial <- dataframe2
for(i in levels(we.c.i))
{
  print(i)
  country_gap_check <- subset(trial, COUNTRY == i)
  rabies.subset <- subset(rabies.data.df, COUNTRY == i)
  
  timestep <- sort(unique(country_gap_check$Qs), 
                       decreasing = FALSE)
  
  for(j in timestep) {
    
   timestep_gap  <- subset(country_gap_check, Qs == j)
   rabies_gap  <- subset(rabies.subset, Qs == j)
   
   if (rabies_gap$COUNT > 0){
     trial1 <- trial[-which(trial$COUNTRY == i & 
                                                   trial$Qs == j),]
     trial <- trial1 # replace the with new dataset 
    
  } else {
    print("do nothing")    
  }
}}


write.csv(trial1, file = "output/gaps_final_dataframe.csv")



#### Check if data are really gaps according to the excel file sent byFLI

# cALL THE FILES
gaps <- read.csv("output/gaps_final_dataframe.csv")
fli.info <- read.csv("Cases/Rabies1.csv")

# keep the columns we need
fli.info <- fli.info[,c(1:8)]

# Create year+quarter ID code
fli.info$ID <- with(fli.info, paste0(Year, "Q", Quarter)) 
names(fli.info)

# create vectors
gaps$ID <- with(gaps, paste0(YEAR, "Q", QUARTER)) 
countries <- sort(unique(gaps$COUNTRY), decreasing = FALSE)
gaps1 <- gaps

# set the loop
for(i in countries)
  {
  fli.i <- subset(fli.info, CountryName1 ==  i)
  gaps.i <- subset(gaps, COUNTRY == i)
  
  ID.gaps <- sort(unique(gaps.i$ID), decreasing = FALSE)
  ID.fli <- sort(unique(fli.i$ID), decreasing = FALSE)
  
  for (j in ID.gaps) 
    {
    if  (j %in% ID.fli) # no gap
      {
      gaps2 <- gaps1[-which(gaps1$COUNTRY == i & gaps1$ID == j),]
      gaps1 <- gaps2 # replace the with new dataset 
      
    } else {   # it is gap
      "do nothing"
      }
  }}

write.csv(gaps1, file = "output/gaps_dataframe_contrasted.fli.csv")

