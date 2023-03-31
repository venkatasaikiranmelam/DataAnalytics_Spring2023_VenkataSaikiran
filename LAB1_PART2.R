###lab1 part2
rm(list=ls())
getwd()
setwd("C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy")


file = "C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/Data/2010EPI_data.csv"

headers = read.csv(file, skip = 1, header = F, nrows = 1, as.is = T)
df = read.csv(file, skip =2 , header = F)
colnames(df)= headers
head(df$code)
names(df)

attach(df)

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s") 
qqnorm(EPI)
qqline(EPI)

# sequence geenral distributions

x<-seq(30,95,1)

qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t 
dsn")

qqline(x)


plot(ecdf(EPI),do.points=FALSE,verticals = TRUE)
plot(ecdf(EPI),do.points=TRUE,verticals = TRUE)
par(pty="s")


qqnorm(EPI)

png(filename="C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/LAB1_graphs/qqplot1.png")
qqplot(EPI,DALY)
dev.off()

png(filename="C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/LAB1_graphs/boxplot1.png")
boxplot(EPI,DALY)
dev.off()


png(filename="C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/LAB1_graphs/qqplot2.png")
qqplot(EPI,ENVHEALTH)
dev.off()

png(filename="C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/LAB1_graphs/qqplot3.png")
qqplot(DALY,AIR_H)
dev.off()

png(filename="C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/LAB1_graphs/qqplot4.png")
qqplot(WATER_H, WATER_E)
dev.off()



### regression
multivariate <- read.csv("C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/Data/multivariate.csv")
attach(multivariate)
multivariate

# Read the second sheet of latitude.xlsx: latitude_2
multivariate_df = multivariate[1:7,1:7]
multivariate_df

multivariate_df_new = na.omit(multivariate)
multivariate_df_new

attach(multivariate_df_new)

mm<-lm(multivariate_df_new$Homeowners~multivariate_df_new$Immigrants)

summary(mm)

plot(multivariate_df_new$Homeowners~multivariate_df_new$Immigrants)
mm<-lm(multivariate_df_new$Homeowners~multivariate_df_new$Immigrants)
abline(mm)
abline(mm,col=2,lwd=3)
attributes(mm)
mm$coefficients

# mtcars

data("mtcars")
mtcars

plot()

# Creating Plots
# Chapter 2
png(filename="C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/LAB1_graphs/mtcars1.png")
plot(mtcars$wt,mtcars$mpg)
dev.off()

library(ggplot2)

png(filename="C:/Users/91983/OneDrive/Desktop/RPI-work/DA/repo/DataAnalyticsSpring2023_SainathReddy/LAB1_graphs/ggplot_mtcars1.png")
qplot(mtcars$wt,mtcars$mpg)
dev.off()

qplot(wt, mpg, data = mtcars)
ggplot (mtcars, aes (x=wt, y=mpg))+ geom_point()


plot (pressure$temperature, pressure$pressure, type="l")

points (pressure$temperature, pressure$pressure)

lines (pressure$temperature, pressure$pressure/2, col="red")
points (pressure$temperature, pressure$pressure/2, col="blue")

qplot(pressure$temperature, pressure$pressure, geom="line")
qplot(temperature, pressure, data = pressure, geom="line")

#####

# Creating Bar graphs
barplot (BOD$demand, names.arg = BOD$Time )
table (mtcars$cyl)

barplot(table(mtcars$cyl)) # generate a table of counts.
qplot(mtcars$cyl) # cyl is continous here
qplot(factor (mtcars$cyl)) # treat cyl as discrete
# Bar graph of counts
qplot(factor (cyl), data = mtcars)
ggplot (mtcars, aes (x=factor(cyl))) + geom_bar()


#### histograms

# Creating Histogram
#View the distribution of one-dimentional data with a histogram.
hist (mtcars$mpg)
hist(mtcars$mpg, breaks =10)
hist(mtcars$mpg, breaks =5)
hist(mtcars$mpg, breaks =12)

qplot(mpg, data = mtcars, binwidth=4)

ggplot(mtcars,aes(x=mpg)) + geom_histogram(binwidth=4)


####blox plots using gg plots


# Creating Box-plot
plot(ToothGrowth$supp, ToothGrowth$len) 
boxplot (len~ supp, data=ToothGrowth) 

boxplot(len~supp+ dose, data = ToothGrowth)
qplot(ToothGrowth$supp, ToothGrowth$len, geom ="boxplot")
qplot(supp, len, data=ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
qplot(interaction (ToothGrowth$supp, ToothGrowth$dose),ToothGrowth$len,geom= "boxplot")  
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot")

ggplot(ToothGrowth, aes(x=interaction (supp, dose), y=len)) + geom_boxplot()


