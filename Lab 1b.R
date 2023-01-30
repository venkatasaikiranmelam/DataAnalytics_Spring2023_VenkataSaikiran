#-------------------------- Lab 1b--------------------------------
multivariate <-read.csv("C:/sk/2nd sem/2.Data Analytics/multivariate.csv")
head(multivariate)
attach(multivariate)
multivariate
m_df <- as.data.frame(multivariate)

m_df$Immigrants <- as.factor(m_df$Immigrants)
m_df

m1 <-lm(Homeowners~Immigrants, data=m_df)

m1 <- na.omit(m_df)
m1
m1 <-lm(Homeowners~Immigrants, data=m1)
m1
summary(m1)$coef
plot(Homeowners~Immigrants)
help(abline)
abline(m1)
abline(m1,col=2,lwd=3)
newImmigrantdata <- data.frame(Immigrants = c(0, 20))
m1 |> predict(newImmigrantdata)
m1$coefficients
df=data(mtcars)
plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data = mtcars)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt, mpg, data = mtcars)
ggplot (mtcars, aes (x=wt, y=mpg))+ geom_point()
plot (pressure$temperature, pressure$pressure, type = 'l') 
      points (pressure$temperature, pressure$pressure)
      
lines (pressure$temperature, pressure$pressure/2, col="red")
points (pressure$temperature, pressure$pressure/2, col="blue")
library(ggplot2)
qplot(pressure$temperature, pressure$pressure, geom="line")
qplot(temperature, pressure, data = pressure, geom=
      "line")
ggplot (pressure, aes(x-temperature, y=pressure)) + geom_line() + geom_point()
ggplot(pressure, aes(x=temperature, y=pressure))+ geom_line() + geom_point()
hist(mtcars$mpg)
hist(mtcars$mpg,breaks =10)
hist(mtcars$mpg,breaks=5)
hist(mtcars$mpg,breaks=12)
qplot(mpg, data=mtcars,binwidth=4)
ggplot (mtcars,aes(x=mpg)) + geom_histogram(binwidth=4)
ggplot(mtcars, aes(x=mpg))+ geom_histogram(binwidth= 5)

plot(ToothGrowth$supp, ToothGrowth$len) # using plot() function and pass it a factor of x-values and a vecctor of y-values. #Formula Syntax
boxplot(len ~ supp, data = ToothGrowth) # if the tow vectors are in the same dataframe, you can use the formula syntax. With # this syntax you can combine two variables on the x-axis.

boxplot(len ~ supp + dose, data=ToothGrowth)

library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
# if the two vectors are in the same dataframe, you can use the following syntax
qplot(supp, len, data = ToothGrowth, geom = "boxplot")
# in ggplot2, the above is equvalent to:
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
# Using three seperate vectors
qplot(interaction (ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom="boxplot")
qplot(interaction (supp,dose),len,data=ToothGrowth, geom = 'boxplot')                                                    
ggplot(ToothGrowth, aes(x=interaction(supp,dose), y=len))+geom_boxplot()                     
                     
     

 
                   