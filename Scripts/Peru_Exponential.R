#Brinkley Raynor
#Based on Justin Sheen's code
#April 1, 2020

#df0: line list
#df: cumulative cases
#df.lm: Instantaneous exponential rates per region per day based on cases that day and the days before and after
#df.lm1: Overall exponential rate per region

###############################################################################################################
#Load and format data
###############################################################################################################
library(ggplot2)
library(gridExtra)
#library(plyr)
#library(tseries)

#Load case data
# setwd("~/R code/COVID19_BR")                    
# df0 <- read.csv("PeruCovidData_2020Mar30.csv")

setwd("~/R code/covid19/data/modificadas")                    
df0 <- read.csv("covidPE_IND_20200331_MD_clean.csv")

#Format data frame
df0 <- df0[2:17]
colnames(df0) <- c("ID", "Date", "Day", "Month", "Year", "Region", "Provence", "Disctrict",  
                   "Direction", "Age1", "Age", "Sex", "Hospitilized", "AISLADODOMICILS1N0", 
                   "Contact","Imported") #KEY: Sex:M=1,F=0|Hospitilized:Yes=1,No=0|Imported:Yes=1,No=0)
#df0$Date <- as.Date(df0$Date, "%m/%d/%Y") #Fix date format
df0$Date <- as.Date(df0$Date) 

#######Transform data frame from line list to collected cases per region per day##########
#Create reference lists
Regions <- levels(droplevels(unique(df0$Region))) #Regions
Dates <- as.data.frame(seq(as.Date(min(df0$Date)), as.Date(max(df0$Date)), "days")); colnames(Dates) <- c("Date") #Dates
df <- data.frame(Dates) #Create table to build on

#Find total number of cases per day per region (Line list --> incidence table)
for (i in 1:length(Regions)){
  R1 = Regions[i]
  df.sub <- subset(df0, df0$Region == R1) #Subset to region in loop
  df.sub <- as.data.frame(table(df.sub$Date)); df.sub$Var1 <- as.Date(df.sub$Var1) #Generate counts table
  Dates$Match <- df.sub$Freq[match(Dates$Date, df.sub$Var1)]; Dates[is.na(Dates)] <- 0 #Match counts with Date ref
  df <- cbind(df, Dates$Match); colnames(df)[i+1] <- paste0(R1) #bind onto main df
}

df$Peru = rowSums(df[,c(-1)]) #Find totals for Peru

###############################################################################################################
#Estimate exponential growth rates
###############################################################################################################
#SHORT TERM
df <- subset(df, select=-c(TACNA, AYACUCHO)) #Remove CAJAMARCA bc only have one case, messing up loops 

#Set up reference regions and blank dfs
Regions <- colnames(df)
Regions <- Regions[-1]

df.lm <- NULL #INSTANTANEOUS
df.lm2 <- NULL #OVERALL

#Loop through all regions
for (i in 1:length(Regions)){
  R1 = Regions[i]
  df.sub <- data.frame(df[, 1], df[,(i+1)]); colnames(df.sub) <- c("Date", paste0(R1)) #Subset by region
  df.sub <- df.sub[paste0(min(which(df.sub[2] != 0))):length(df.sub$Date),] #Find first non 0 
  
  ######################INSTANTANEOUS RATES################################
  for (j in 2:(length(df.sub$Date) - 1)) { 
    #Create "instantaneous" data frame
    D1 <- df.sub$Date[j]  #Loop through each day (except first and last)
    df.sub2 <- subset(df.sub, df.sub$Date == D1 -1 | df.sub$Date == D1 |  df.sub$Date == D1 + 1) #get "instantaneous" rate based on 3 days
    df.sub2[2] <- log(df.sub2[2]) #take ln of # of cases so can use linear model to get exp rates later
    df.sub2$Day <- seq(1:length(df.sub2$Date)) #Add in day number to use as independent variable in linear model
    colnames(df.sub2) <- c("Date", "LogCases", "Day") 
    df.sub2$LogCases <- ifelse(df.sub2$LogCases < 0, 0, df.sub2$LogCases) ####NOT SURE IF THIS IS BEST APPROACH (ln(0) = -INF --> changed to 0?)
    
    #Fit exponential model to 3 point instantaneous rate
    lm <- lm(LogCases ~ Day, data=df.sub2) #linear model of three days
    row1 <-  c(paste0(R1), paste0(D1), as.numeric(lm$coefficients[1]), as.numeric(lm$coefficients[2])) #Pull out coefficients
    df.lm <- rbind(df.lm, row1)
  }
  
  ######################Overall rates################################
  df.sub$Day <- seq(1:length(df.sub$Date)) #Add in day number to use as independent variable in linear model
  colnames(df.sub) <- c("Date", "Cases", "Day") 
  df.sub$LogCases <- log(df.sub$Cases) #take ln of # of cases so can use linear model to get exp rates later
  df.sub$LogCases <- ifelse(df.sub$LogCases < 0, 0, df.sub$LogCases) ####NOT SURE IF THIS IS BEST APPROACH (ln(0) = -INF --> changed to 0?)
  lm1 <- lm(LogCases ~ Day, data=df.sub) #fit linear model for all days
  row2 <-  c(paste0(R1), as.numeric(lm1$coefficients[1]), as.numeric(lm1$coefficients[2])) #Pull out coefficients
  df.lm2 <- rbind(df.lm2, row2)
}

#Format as df
df.lm <- as.data.frame(df.lm)
colnames(df.lm) <- c("Region", "Date", "Intercept", "InstantaneousRate")
df.lm$Date <- as.Date(df.lm$Date)
df.lm$InstantaneousRate <- as.character(df.lm$InstantaneousRate); df.lm$InstantaneousRate <- as.numeric(df.lm$InstantaneousRate)

#format as df
df.lm2 <- as.data.frame(df.lm2)
colnames(df.lm2) <- c("Region", "Intercept", "OverallRate")
df.lm2$OverallRate <- as.character(df.lm2$OverallRate); df.lm2$OverallRate <- as.numeric(df.lm2$OverallRate)

###############################################################################################################
#Figures
###############################################################################################################
#set color pallettes
palM <- c("maroon", "darkorange3", "goldenrod3", "darkgreen", "navy", "plum4", "black",
          "lightcoral", "peachpuff2", "khaki1", "olivedrab3", "steelblue1", "thistle", "snow2",
          "red3", "orange", "gold", "springgreen4", "dodgerblue3", "mediumpurple", "gray")

pal1 <- palM[1:length(unique(df0$Region))]

#Figure 1: Peru epicurve
fig1 <- ggplot() +
  theme_classic()+
  geom_histogram(data = df0, aes(Date, fill= Region), color="black", binwidth = 1)+
  scale_fill_manual(values = pal1)+
  scale_x_date(breaks=seq(min(df0$Date)-1, max(df0$Date)+1, 1))+
  labs(title= "COVID-19 Peru Cases")+
  xlab("Date of lab confirmation") + ylab("Total new cases")+
  theme(axis.text.x = element_text(angle=90))
fig1

#save as png
path = "~/R code/covid19/outputs_covid19/EpiCurves/"                   
png(file=paste0(path, Sys.Date(), "EpiCurve_Total.png"), width=600, height=350)
fig1
dev.off()

#Figure 2: Cases per region
Regions <- levels(droplevels(unique(df0$Region)))
fig_list <- list()
for (i in 1:length(Regions)){
  R1 <- Regions[i] 
  df.sub <- subset(df0, df0$Region== R1)
  fig2 <- ggplot() +
    theme_classic()+
    geom_histogram(data = df.sub, aes(Date), color="black", fill=pal1[i], binwidth = 1)+
    scale_x_date(date_labels = "%b %d", breaks=seq(min(df0$Date)-1, max(df0$Date)+1, 1), limits= c(min(df0$Date)-1, max(df0$Date)+1))+
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
    labs(title=paste0("COVID-19 ", R1, " Cases"))+
    xlab("Date of lab confirmation") + ylab("Total new cases")+
    theme(axis.text.x = element_text(angle=-90, hjust=5))
  fig2
  fig_list[[i]] <- fig2
}

#Save as png
png(file=paste0(path, Sys.Date(), "EpiCurves_Regions.png"), width=700, height=2100)
do.call("grid.arrange", c(fig_list, ncol=2))
dev.off()

#Figure 3: estimated rates
fig_list1 <- list()
for (i in 1:length(Regions)){
  R1 <- Regions[i] 
  df.sub <- subset(df.lm, df.lm$Region == R1)
  df.sub2 <- subset(df.lm2, df.lm2$Region == R1)
  fig3 <- ggplot() +
    theme_classic()+
    geom_point(data=df.sub, aes(x=Date, y=InstantaneousRate))+
    scale_x_date(date_labels = "%b %d", breaks=seq(min(df0$Date)-1, max(df0$Date)+1, 1), limits= c(min(df0$Date)-1, max(df0$Date)+1))+
    scale_y_continuous(breaks = seq(floor(min(df.sub$InstantaneousRate))-1, floor(max(df.sub$InstantaneousRate))+1, 0.5))+
    geom_hline(yintercept=df.sub2$OverallRate[1])+
    labs(title=paste0("COVID-19 ", R1, " Cases"))+
    xlab("Date of lab confirmation") + ylab("Total new cases")+
    theme(axis.text.x = element_text(angle=-90, hjust=5))
  fig3
  fig_list1[[i]] <- fig3
}

#Save as png
png(file=paste0(path, Sys.Date(), "ExponentialRates.png"), width=700, height=2100)
do.call("grid.arrange", c(fig_list1, ncol=2))
dev.off()


