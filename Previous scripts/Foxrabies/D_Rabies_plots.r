##'**MRES PROJECT**
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'**MICAELA DE LA PUENTE L.**
##'**This script shows graphs of fox rabies incidence by years and countries**
##############################################################################

######################################################
# FIRST SET WORKING DIRECTORY IN RSTUDIO
# i.e. Session > Set Working Directory > To Source File Location
rm(list=ls())

######################################################
library(ggplot2)
library(reshape2)
######################################################

## Call the prepared datafrmae of rabies data.
foxrabies.we <-read.csv("output/foxrabies_final_full.csv")


########################################################################
#### Number of cases per year in WE ####

table(foxrabies.we$YEAR)

## Sum the number of cases in each year
rabies.y <- hist(foxrabies.we$YEAR, breaks=1986:2006, plot=FALSE)$counts

## Plot the graph
png("output/rabies_years_scp.png", width=8, height=4, units="in", res=300)

plot.y <- plot(rabies.y, ylab ="New infected foxes Western Europe",
               type="o", col="red", axes= FALSE, main="Western Europe",
               lwd=2, pch=19, ann=FALSE)
axis(1, at=1:20, lab=c(1987:2006)) ## Set axes and title
axis(2, at = seq(0, 9000, by = 1000), las=2)
box()
title(main="Western Europe", xlab="Years", ylab="Number infected foxes")
dev.off()


########################################################################
#### Number of cases per quarter in WE - BARPLOT ####
table(foxrabies.we$QUARTER)

## Sum the number of cases per quarter
rabies.q <- hist(foxrabies.we$QUARTER, breaks=0:4, plot=FALSE)$counts

## Plot the graph
png("output/rabies_quarters_bp.png", width=6, height=4, units="in", res=300)
bp <- barplot(rabies.q, axes=FALSE, axisnames = FALSE,
              main="Western Europe", ylim = c(0, 15000))
axis(2, ylim=c(0,15000), cex=1.2, las=2)
text(bp, par("usr")[3], labels = c("Quarter 1\n(Jan-Mar)", "Quarter 2\n(Apr-June)", "Quarter 3\n(Jul-Sep)", "Quarter 4\n(Oct-Dec)"), #srt = 45,
     adj = c(0.5,1.2), xpd = TRUE, cex=1.2)
mtext("Number of cases",side=2,line=4, cex=1.3)
# More cases in the first quarter
dev.off()


########################################################################
#### Number of cases per quarter timesteps in WE ####
table(foxrabies.we$Qs)

## Sum number of cases per quarter timestep (Qmarize cases by quarter.
rabies.qs <- hist(foxrabies.we$Qs, breaks=36:115, plot=FALSE)$counts

## Plot the lineplot
png("output/rabies_qseries_scp.png", width=10, height=4, units="in", res=300)

plot.qs <- plot(rabies.qs,
                ylab ="New infected foxes Western Europe",
               type="o", col="red", axes=FALSE, main="Western Europe",
               lwd=1.5, pch=19, ann=FALSE, cex=0.7)
axis(1, lab=c(37:115), at=1:79, cex=1.2)
axis(2, at = seq(0, 9000, by = 1000), cex=1.2, las=2)
box()
title(main="Western Europe", xlab="Quarter timesteps",
      ylab="Number infected foxes",
      cex=1.3)
dev.off()
# We still see the peaks at first quarters.


########################################################################
#### Number of cases per country in WE ####
table(foxrabies.we$COUNTRY)

## Sum the number of cases found in each country
sum.c <- tapply(foxrabies.we$NUMBER,
                list(foxrabies.we$COUNTRY), sum, na.rm=TRUE)
sum.rabies.c <- data.frame(sum.c)

## Plot the graph
png("output/rabies_countries_bp.png", width=11, height=6, units="in", res=300)

plot.c <-barplot(sum.rabies.c$sum.c,
                 ylab ="Number of foxes",
                 ylim = c(0,18000),
                 col = "grey", cex.lab=1.3,
                 cex.axis = 1.2)
text(plot.c, sum.rabies.c$sum.c+500, paste("n=", sum.rabies.c$sum.c), cex=.8)
dev.off()
# Germany, France and Austria are countries with higher incidence.




#### Number of cases per countries and year in WE
########################################################################

table(foxrabies.we$COUNTRY, foxrabies.we$YEAR)

## Summarize in a dataframe
sum.cy <- tapply(foxrabies.we$NUMBER, list(foxrabies.we$COUNTRY, foxrabies.we$YEAR), sum)
sum.rabies.cy <- data.frame(sum.cy) # dataframe
print(sum.cy)

## Change structure of the dataframe.
# Makes easier to plot later
df <- melt(sum.cy,  id.vars = sum.rabies.cy$YEAR,
           variable.name = sum.rabies.cy$COUNTRY)

## Plot the graph
#pdf("output/cases.years.countries1.pdf", onefile = TRUE)
# Plot them together
png("output/rabies_countriesandyears_scp.png", width=11, height=6, units="in", res=300)

ggplot(df, aes(Var2,value)) + geom_line(aes(colour = Var1), size=2)+
  labs(x = "Years", y = "Number of cases") +
  theme(legend.text = element_text(size = 21))  +
  theme(axis.title.x = element_text(size = 22)) +
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 22)) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(plot.margin = unit(c(0.5,0.5,0.8,0.5), "cm"))+
  scale_x_continuous(breaks=seq(1987,2006,3))
dev.off()

# Plot them in different plots
ggplot(df, aes(Var2,value)) + geom_line(aes(colour = "red"), size=2) +
  facet_grid(Var1 ~ .) +
  labs(x = "Years", y = "Number of cases") +
  scale_x_continuous(breaks=seq(1987,2006,3)) +
  theme(axis.title.x = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 18)) +
  theme(axis.text.y = element_text(size = 15))
#dev.off()


#### Number of cases per countries and quarter
table(foxrabies.we$COUNTRY, foxrabies.we$QUARTER)

# Create the dataframe to plot
sum.cq <- tapply(foxrabies.we$NUMBER, list(foxrabies.we$COUNTRY, foxrabies.we$QUARTER), sum)
sum.rabies.cq <- data.frame(sum.cq)
print(sum.cq)
df <- melt(sum.cq,  id.vars = sum.rabies.cq$QUARTER,
           variable.name = sum.rabies.cq$COUNTRY)

# In the same plot
png("output/rabies_countriesandquarters_scp.png", width=8, height=6, units="in", res=300)

ggplot(df, aes(Var2,value)) + geom_line(aes(colour = Var1), size=1.5) +
  labs(x = "Quarter of the year", y = "Number of cases") +
  theme(legend.text = element_text(size = 21))  +
  theme(axis.title.x = element_text(size = 22)) +
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 22)) +
  theme(axis.text.y = element_text(size = 20))
dev.off()



#### Lineplots of number of cases by years per each country

table(foxrabies.we$COUNTRY, foxrabies.we$YEAR)

## Create the vector
we.c.i <- as.factor(foxrabies.we$COUNTRY)
we.c.i

pdf("output/rabies_countriesbyyears_scp.pdf", onefile = TRUE)

## Set the for loop
for(i in levels(we.c.i))
{ # Subset the data
  we.country <- foxrabies.we[which(foxrabies.we$COUNTRY==i),]

  # Sum the number of cases per year
  rabies.iy <- hist(we.country$YEAR, breaks=1986:2006, plot=FALSE)$counts

  # Plot the graphs
  plot.country <-plot(rabies.iy,
                      ylab ="New infected foxes Western Europe",
                      col = "red", pch=19, type="o", ann=FALSE, axes=FALSE)
  axis(1, lab=c(1987:2006), at=1:20)
  axis(2)
  title(main=(paste(i, "by years")), xlab="Years", ylab="Number infected foxes")
  box()
}
dev.off()



#### Barplot of number of cases by years per each country

pdf("output/rabies_countriesbyyears.bp.pdf", width=12, height=4,
   onefile = TRUE)

# Set the for loop function
for(i in levels(we.c.i))
{ we.country <- foxrabies.we[which(foxrabies.we$COUNTRY==i),]

  # Sum the number of cases per year
  rabies.iy <- hist(we.country$YEAR, breaks=1986:2006, plot=FALSE)$counts

  # Plot the barplot
  plot.country <-barplot(rabies.iy,
                         ylab ="New infected foxes Western Europe",
                         col = "grey",
                         main=(paste(i, "by years")))
  text(plot.country, labels=c(1987:2006), par("usr")[3],
       adj = c(0.5,1.2), xpd = TRUE, cex=.5)
}
dev.off()



######## stacked plot #######
names(df) # df comes from line 112
# [1] "Var1"  "Var2"  "value"

# Change NAs with 0s
df[is.na(df)] <- 0

ggplot(df, aes(x = Var2, y = value, fill = Var1)) + 
  geom_area(position = 'stack') +
  labs(x = "Years", y = "Fox rabies incidence") +
  scale_x_continuous(breaks=seq(1987,2006,4)) +
  #theme(axis.title.x = element_text(size = 26)) +
  theme(axis.text.x = element_text(size = 23)) +
  theme(axis.title.y = element_text(size = 26)) +
  theme(axis.text.y = element_text(size = 23)) + 
  theme(legend.title = element_text(size=16, face="bold")) +
  theme(legend.text = element_text(size = 23)) +
  guides(fill=guide_legend(title=NULL))
  #+ scale_linetype_discrete(name = "Country") + facet_grid(. ~ V2)

# IT DOES NOT SEEM THE FINAL ONE
