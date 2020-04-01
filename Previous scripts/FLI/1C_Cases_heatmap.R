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
#library(rworldmap)
require(rgeos)
library(ggmap)
library(ggplot2)
library(Rcpp)
library(foreign)
#library(devtools)
#library(animation)
library(reshape2)
library("RColorBrewer")
library(gplots)

library(lme4)
library(lmerTest)
library(Matrix)
library(lattice)  #Needed for multi-panel graphs
library(MASS)
library(car)
library(plyr)
#####################################################
#####################################################
## Call the shp
foxrabies <-readOGR("output/Europe_cases.shp", 
                             "Europe_cases") 
str(foxrabies@data) # 147640 obs. of  7 variables:

gaps <- read.csv("output/gaps_dataframe_contrasted.fli.csv")
str(gaps) # 'data.frame':	139 obs. of  8 variables:
######################################################

# Find correct names
table1 <- as.data.frame(table(foxrabies$COUNTRY, foxrabies$YEAR))
str(table1)
table1$Var2 <- as.numeric(as.character(table1$Var2))
table1$Freq <- as.numeric(table1$Freq)


# Create a table from the gap csv
gaps.comp <- as.data.frame(table(gaps$COUNTRY, gaps$YEAR))
write.csv(gaps.comp, file = "output/gaps_dataframe_contrasted.fli_byyear.csv")


# find out countries with no cases the whole year
# cHANGE THE YEARLY TABLE WITH COuntries with NAs if needed.

countries <- sort(unique(table1$Var1))
table2 <- table1     

for (i in countries) 
  {
  table.i <- subset(table1, Var1==i)
  gaps.i <- subset(gaps.comp, Var1==i)
  year <- sort(unique(gaps.i$Var2))

  for (j in year) 
    {
    table.ij <- subset(table.i, Var2==j)
    gaps.ij <- subset(gaps.i, Var2==j)
    
    if (table.ij$Freq == 0 & gaps.ij$Freq == 4) # gaps in the 4 quarters!
    {
      table2 <- within(table2, Freq[Var1 == i & Var2 == j] <- NA)
    } else {
      "do nothing"
    }
  }}




# Create a heatmap

# gradient
#png("output/triall.png", width=5, height=5, units="in", res=300)

ggplot(table1, aes(y=Var1, x=Var2, fill=Freq)) + 
  geom_tile(colour="white", #linewidth=2, 
            width=.9, height=.9) + 
  theme_minimal() +  # works
  scale_fill_gradientn(colours=rev(heat.colors(15)),
    #colours=colorRampPalette(c("white", "red"))(5),
                       limits=c(0, 4000),
                       breaks=seq(0, 4000, by=1000), 
                       #na.value=rgb(246, 246, 246, max=255),
                       labels=c("0", "1K", "2K", "3K", "4K"),
                       guide=guide_colourbar(ticks=T, nbin=50,
                                             barheight=.5, label=T, 
                                             barwidth=10)
    ) +
  scale_x_continuous(expand=c(0,0), # works
                     breaks=seq(1987, 2016, by=4)) +
  #geom_segment(x=1963, xend=1963, y=0, yend=51.5, size=.9) +
  labs(x="", y="", fill="") +
  ggtitle("Fox rabies") +
  theme(legend.position=c(.5, -.20),
        legend.direction="horizontal",
        legend.text=element_text(colour="grey20"),
        plot.margin=grid::unit(c(.5,.5,1.5,.5), "cm"),
        axis.text.y=element_text(size=6, family="Helvetica", hjust=1),
        axis.text.x=element_text(size=8),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank(),
        title=element_text(hjust=-.07, face="bold", vjust=1, 
                           family="Helvetica"),
        text=element_text(family="URWHelvetica")) #+
#  annotate(&quot;text&quot;, label=&quot;Vaccine introduced&quot;, x=1963, y=53, 
#           vjust=1, hjust=0, size=I(3), family=&quot;Helvetica&quot;)
ggsave("output/heatmap_Europe.png", dpi=300, width=7, height=5) # too heavy

#dev.off()

#########################################################################################################

# other continous with different pallete
our.palette <- c("white","lightblue", "blue", "green4", "green3",
                 "#FFFF00FF", "#FFE800FF", "#FFD100FF", 
                 "#FFB900FF", 
                 "#FFA200FF",
                  "#FF8B00FF", "#FF7400FF", "#FF5D00FF", "#FF4600FF", 
                 "#FF2E00FF", "#FF1700FF",
                  "#FF0000FF")

ggplot(table1, aes(y=Var1, x=Var2, fill=Freq)) + 
  geom_tile(colour="white", #linewidth=2, 
            width=.9, height=.9) + 
  theme_minimal() +  # works
  scale_fill_gradientn(colours=our.palette,#rev(heat.colors(15)),
                       #colours=colorRampPalette(c("white", "red"))(5),
                       limits=c(0, 4000),
                       breaks=seq(0, 4000, by=1000), 
                       # na.value="grey",  #rgb(246, 246, 246, max=255),
                       labels=c("0", "1K", "2K", "3K", "4K"),
                       guide=guide_colourbar(ticks=T, nbin=50,
                                             barheight=.5, label=T, 
                                             barwidth=10)) +
  scale_x_continuous(expand=c(0,0), # works
                     breaks=seq(1987, 2016, by=4)) +
      #geom_segment(x=1963, xend=1963, y=0, yend=51.5, size=.9) + 
  labs(x="", y="", fill="") + 
  ggtitle("Fox rabies") + 
  theme(legend.position=c(.5, -.20), 
        legend.direction="horizontal",
        legend.text=element_text(colour="grey20"), 
        plot.margin=grid::unit(c(.5,.5,1.5,.5), "cm"),
        axis.text.y=element_text(size=6, family="Helvetica", hjust=1), 
        axis.text.x=element_text(size=8), 
        axis.ticks.y=element_blank(), 
        panel.grid=element_blank(),
            title=element_text(hjust=-.07, face="bold", vjust=1, 
                               family="Helvetica"),
            text=element_text(family="URWHelvetica")) #+
    #  annotate(&quot;text&quot;, label=&quot;Vaccine introduced&quot;, x=1963, y=53, 
    #           vjust=1, hjust=0, size=I(3), family=&quot;Helvetica&quot;)
ggsave("output/heatmap_Europe_v2_cont.png", dpi=300, width=7, height=5) # too heavy
    

    
    
######
    
# other discrete
trial1 <- subset(table1, Freq > 0) # subset each unit
hist(trial1$Freq, seq(1,4001,by=10))

table1 <- table2

table1$brks <- cut(table1$Freq, 
                            breaks=c(-0.1, 0.1, 10, 50, 200, 
                                     500, 1000, 2000, 
                                     3000, 4000), 
                            labels=c("0", "1 - 10", "11 - 50", "51 - 200", 
                                     "201 - 500", "501 - 1000", "1001 - 2000", 
                                     "2001 - 3000", "3001 - 4000"))
    
#our.palette <- c("white","lightblue", "blue", "green4", 
#                 "green3", "yellow1", "yellow3", 
#                 "orange", "red")
#our.palette <- c("white", rev(heat.colors(8)))
our.palette <- c("white", "#FFFFBFFF","#FFFF80FF", "#FFFF40FF",
                 "#FFCC00FF", "#FF9900FF", "#FF6600FF",
                 "#FF3300FF", "#FF0000FF")

ggplot(table1, aes(y=Var1, x=Var2, fill=brks)) + 
      geom_tile(colour="white", #linewidth=2, 
                width=.9, height=.9) + 
      theme_minimal() +  # works
      scale_fill_manual(values = our.palette, name="Incidence", 
                        na.value="grey") +
     scale_x_continuous(expand=c(0,0), # works
                         breaks=seq(1987, 2016, by=4)) +
      #geom_segment(x=1963, xend=1963, y=0, yend=51.5, size=.9) +
      labs(x="", y="", fill="") +
      ggtitle("Fox rabies") +
      theme(#legend.position=c(.5, -.20),
            #legend.direction="horizontal",
            legend.text=element_text(colour="grey20", size = 8),
            plot.margin=grid::unit(c(.5,.5,1.5,.5), "cm"),
            axis.text.y=element_text(size=8, family="Helvetica", hjust=1),
            axis.text.x=element_text(size=8),
            axis.ticks.y=element_blank(),
            panel.grid=element_blank(),
            title=element_text(hjust=-.07, face="bold", vjust=1, 
                               family="Helvetica"),
            text=element_text(family="URWHelvetica"),
            legend.key = element_rect(colour = "grey20"),
            legend.title=element_text(size=10)) #+
    #  annotate(&quot;text&quot;, label=&quot;Vaccine introduced&quot;, x=1963, y=53, 
    #           vjust=1, hjust=0, size=I(3), family=&quot;Helvetica&quot;)
    ggsave("output/heatmap_Europe_v5_dis.png", dpi=300, width=7, height=5) # too heavy
    

    
          
##########
measles <- read.csv("HEPATITIS1.csv")

measles <- melt(measles, id.var=c("YEAR", "WEEK"))
colnames(measles) <- c("year", "week", "state", "cases")

measles1 <- subset(measles, week == 1)
measles1[measles1=="-"]<-NA

measles1$cases <- as.numeric(measles1$cases)
str(measles1)

png("output/triall.png", width=5, height=5, units="in", res=300)

ggplot(measles1, aes(y=state, x=year, fill=cases)) + 
  geom_tile(colour="white", #linewidth=2, 
            width=.9, height=.9) + 
  theme_minimal() +  # works
  scale_fill_gradientn(colours=rev(heat.colors(5)),
                       limits=c(0, 8),
                       breaks=seq(0, 8, by=2), 
                       na.value=rgb(246, 246, 246, max=255),
                       labels=c("0", "2", "4", "6", "8"),
                       guide=guide_colourbar(ticks=T, nbin=50,
                                             barheight=.5, label=T, 
                                             barwidth=10)
    ) +
  scale_x_continuous(expand=c(0,0), # works
                     breaks=seq(1966, 2011, by=5)) +
  #geom_segment(x=1963, xend=1963, y=0, yend=51.5, size=.9) +
  labs(x="", y="", fill="") +
  ggtitle("Hepatitis") +
  theme(legend.position=c(.5, -.13),
        legend.direction="horizontal",
        legend.text=element_text(colour="grey20"),
        plot.margin=grid::unit(c(.5,.5,1.5,.5), "cm"),
        axis.text.y=element_text(size=6, family="Helvetica", hjust=1),
        axis.text.x=element_text(size=8),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank(),
        title=element_text(hjust=-.07, face="bold", vjust=1, 
                           family="Helvetica"),
        text=element_text(family="URWHelvetica")) #+
#  annotate(&quot;text&quot;, label=&quot;Vaccine introduced&quot;, x=1963, y=53, 
#           vjust=1, hjust=0, size=I(3), family=&quot;Helvetica&quot;)

dev.off()
ggsave("output/trial1.png", dpi=300, width=6, height=5) # too heavy
