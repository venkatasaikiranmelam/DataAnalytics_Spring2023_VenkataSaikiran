getwd()
setwd("C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy")
# Cleaning memory
rm(list=ls())

library("readxl")


# Reading CSV file

file = "C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/Data/2010EPI_data.csv"

headers = read.csv(file, skip = 1, header = F, nrows = 1, as.is = T)
df = read.csv(file, skip =2 , header = F)
colnames(df)= headers
head(df$code)
names(df)


# Read the second sheet of latitude.xlsx: latitude_2
df2 <- read_excel("C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/Data/2010EPI_data.xls", sheet="EPI2010_all countries")
df2

# Structure of Dataframe
str(df2)

# FIXING DATAFRAME on a fly
#fix(df2)
help(fix)

# Attching dataframe to memory
attach(df2)
names(df)

# Null value filtering 
df2$EPI

str(df2$EPI)

df2$EPI<- as.numeric(df2$EPI)
df2$EPI

tf <- is.na(df2$EPI) 
tf

E <- df2$EPI[!tf] 
E

summary(df2$EPI)

# fIIVE NUM STATS
fivenum(df2$EPI,na.rm=TRUE)

# sTEM AND LEAF PLOTS
stem(df2$EPI)

# Histogram
hist(df2$EPI)

hist(df2$EPI,  prob=TRUE)
hist(df2$EPI, breaks=20, prob=TRUE)

hist(df2$EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(df2$EPI,na.rm=TRUE,bw="SJ"))
rug(df2$EPI) 


df2


# Saving histogram

attach(df2)
library(ggplot2)

png(filename="C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/LAB1_graphs/histogram$EPI.png")
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw="SJ"))
rug(EPI) 
dev.off()

###########################################
#Exercise1

#Cumulative density for EPI
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)

# Quantile - Quantile for EPI
par(pty="s") 
qqnorm(EPI)

qqline(EPI)

# QQ plot of a sequence

x <-seq(30,95,1)

qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

names(df2)

#Quantile quantile plot  for "DALY" 

str(df2$DALY)
df2$DALY<- as.numeric(df2$DALY)
df2$DALY

qqnorm(df2$DALY)
qqline(df2$DALY)


#Quantile quantile plot  for "DALY" 


str(df2$WATER_H)
df2$WATER_H<- as.numeric(df2$WATER_H)
df2$WATER_H

qqnorm(df2$WATER_H)
qqline(df2$WATER_H)


# ACDF plots

ggplot(df2, aes(WATER_H)) + stat_ecdf(geom = "point")
ggplot(df, aes(WATER_H)) + stat_ecdf(geom = "step")

# q-q plot of EPI and Daly

qqplot(df2$EPI,df2$DALY)


# BOX plot
boxplot(df2$EPI,df2$DALY,df2$WATER_H) 


# EPI, ENVHEALTH, ECOSYSTEM, DALY, AIR_H, WATER_H, AIR_EWATER_E, 

df3  = subset(df2,EPI_regions== "South Asia")
df3

df4 = subset(df2,EPI_regions== "Europe")
df4

# QQ plot of EPI distributions for Europe and south Asia
qqplot(df3$EPI,df4$EPI)


# Box plot for water and Daly
boxplot(df3$EPI,df3$DALY,df3$WATER_H)



#QQ PLOTS OF DALY for Europe and South Asia

qqplot(df3$DALY,df4$DALY)


# QQ plots for EPI_south African regions

qqplot(df3$EPI,df3$DALY)
qqplot(df3$EPI,df3$ENVHEALTH)
qqplot(df3$EPI,df3$ ECOSYSTEM)
qqplot(df3$EPI,df3$AIR_H)


# QQ plots for EPI_south Europe  regions

qqplot(df4$EPI,df4$DALY)
qqplot(df4$EPI,df4$ENVHEALTH)
qqplot(df4$EPI,df4$ ECOSYSTEM)
qqplot(df4$EPI,df4$AIR_H)

# Now comparing 2016 and 2010 data sets


epi_2010 <- read_excel("C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/Data/2010EPI_data.xls", sheet="EPI2010_all countries")
epi_2016 <- read_excel("C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/Data/2016-epi.xlsx", sheet="Indicator Scores")

head(epi_2016)

epi_2016$"2016 EPI Score"

# QQPLOTS

qqplot(epi_2010$EPI,epi_2016$"2016 EPI Score")

qqplot(epi_2010$ECOSYSTEM,epi_2016$"Ecosystem Vitality")

qqplot(epi_2010$AIR_H,epi_2016$"Household Air Quality") # close to normal distrbibution


# Comparing boxplot of 2016 and 2010

str(epi_2010$EPI)

epi_2010$EPI<- as.numeric(epi_2010$EPI)
epi_2010$ECOSYSTEM = as.numeric(epi_2010$ECOSYSTEM)


boxplot(epi_2010$EPI,epi_2016$"2016 EPI Score") # Median is high in 2016

boxplot(epi_2010$ECOSYSTEM,epi_2016$"Ecosystem Vitality") # median is high in 2016



# Exlploring Distributions from alldist.zip

norm_df <- read_excel("Data/normal.xls",sheet ="Hoja2",col_names = c("a","b","c","d","e","f","g","h","i","j","k","l","m"))

head(norm_df)

hist(norm_df$c)

hist(norm_df$i)

hist(norm_df$f)

qqnorm(norm_df$c)
qqline(norm_df$c)


lognorm_df <- read_excel("Data/lognorm.xls",sheet ="Hoja2",col_names = c("a","b","c","d","e"),skip = 1)

head(lognorm_df)

str(lognorm_df$d)

hist(lognorm_df$d)

qqnorm(lognorm_df$d)
qqline(lognorm_df$d)



##########################################################
# Exercise -2


# Filtering EPI Landlock

epi_2010 <- read_excel("C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/Data/2010EPI_data.xls", sheet="EPI2010_all countries")
head(epi_2010)
str(epi_2010)

attach(epi_2010)

epi_2010$EPI<- as.numeric(epi_2010$EPI)
epi_2010$EPI

EPILand  =  epi_2010[!Landlock,]
Eland <- EPILand[!is.na(EPILand$EPI),]


png(filename="C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/LAB1_graphs/histogram$EPILand.png")
hist(Eland$EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(Eland$EPI,na.rm=TRUE,bw="SJ"))
rug(Eland$EPI) 
dev.off()


plot(ecdf(Eland$EPI), do.points=FALSE, verticals=TRUE) 
qqnorm(Eland$EPI); 
qqline(Eland$EPI)

# No surface water


EPI_No_Surfacewater =  epi_2010[!No_surface_water,]
ESurfacewater <- EPI_No_Surfacewater[!is.na(EPI_No_Surfacewater$EPI),]


png(filename="C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/LAB1_graphs/histogram$EPI_Nosurfacewater.png")
hist(ESurfacewater$EPI)
lines(density(ESurfacewater$EPI,na.rm=TRUE,bw=1.))
rug(ESurfacewater$EPI) 
dev.off()

hist(ESurfacewater$EPI)

plot(ecdf(ESurfacewater$EPI), do.points=FALSE, verticals=TRUE) 
qqnorm(ESurfacewater$EPI); 
qqline(ESurfacewater$EPI)

# Water

EPI_Desert=  epi_2010[!Desert,]
EDesert <- EPI_Desert[!is.na(EPI_Desert$EPI),]


hist(EDesert$EPI)

png(filename="C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/LAB1_graphs/histogram$EPI_Desert.png")
hist(EDesert$EPI)
lines(density(EDesert$EPI,na.rm=TRUE,bw=1.))
rug(EDesert$EPI) 
dev.off()


# High_Population_Density


EPI_High_Population_Density =  epi_2010[!High_Population_Density,]
E_High_Population_Density <- EPI_High_Population_Density[!is.na(EPI_High_Population_Density$EPI),]


hist(E_High_Population_Density$EPI)

png(filename="C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/LAB1_graphs/histogram$EPI_HPD.png")
hist(E_High_Population_Density$EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(E_High_Population_Density$EPI,na.rm=TRUE,bw=1.))
rug(E_High_Population_Density$EPI) 
dev.off()

# Filtering by regions
# using subsets

df_region_E  = subset(epi_2010,EPI_regions== "Europe")
df_subregion_WE = subset(epi_2010,GEO_subregion == "Western Europe") 
df_Europe_WE = subset(epi_2010,EPI_regions== "Europe" & GEO_subregion == "Western Europe" )


# Without suing subsets

df_region_E1  = epi_2010[EPI_regions== "Europe",]
df_subregion_WE1 = epi_2010[GEO_subregion == "Western Europe",] 
df_Europe_WE1 = epi_2010[EPI_regions== "Europe" & GEO_subregion == "Western Europe",]







