
------------------------------------# Lab 1------------------------------------------------
help("read.csv")
EPI_ds <- read.csv(file.choose(),header = TRUE)
head(6)
View(EPI_ds)
install.packages("readxl")
library("readxl")
install.packages("lifecycle")
EPI_df <-read_excel("C:/sk/2nd sem/2.Data Analytics/2010EPI_data.xls", sheet="EPI2010_all countries")
View(EPI_df)
attach(EPI_df)#setting default object
getwd()
setwd("C:/sk/2nd sem/2.Data Analytics")
getwd()
data()
View(EPI_df)
attach(EPI_df)
EPI_df$EPI = as.numeric(EPI_df$EPI) # *** to convert strings to numeric
EPI_df$EPI
         # prints all rows of EPI column
tf <- is.na(EPI_df$EPI) # Takes true value if it is NA
tf

E <-EPI_df$EPI[!tf]
E

summary(EPI)
hist(EPI)

stem(EPI_df$EPI)

name(Landlock)
install.packages("name")
names(Landlock)
EPI_df$Landlock
E <-EPI[!tf]    # question1  still showing null
E
EPI
summary(EPI)
dataEPI= EPI_df$EPI #test for stem to store as numeric
fivenum(Landlock,na.rm=TRUE) #question2  not working on EPI
stem(Landlock) # question 3 EPI didnt work here
stem(E)
hist(Landlock)
hist(EPI_df$EPI)
hist(EPI,seq(30,95,1.0),prob=TRUE)  
lines(density(EPI,na.rm=TRUE,bw=1.)) # or try bw=“SJ”   # question 3
rug(EPI)
fdata
lines(density(EPI,na.rm=TRUE,bw=1)) #question 4
rug(Landlock)
help(rug)

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 

par(pty="s") 
qqnorm(EPI);qqline(EPI)
x<-seq(30,95,1)  
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(3)
EPI_df$DALY = as.numeric(EPI_df$DALY)
EPI_df$DALY
dldf<- is.na(EPI_df$DALY)
d <-EPI[!dldf]
boxplot(EPI,EPI_df$DALY)  #--------- na coverted to numeric
EPILand <-EPI[!Landlock]
EPI_df$Landlock = as.numeric(EPI_df$Landlock)
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland,seq(30, 95, 1.0),prob=TRUE)
EPI_South_Asia <- EPI
