install.packages("ISLR")
help("Boston")
summary(Auto$weight)
#----------------------------------- Lab 0------------------------------------------------
days <-c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')   #days
temp <-c(23,30.5,32,31,29.3,27.9,26.4)                # temp in Fahrenheit
snowed <-c('T','T','F','F','T','T','F')   
help("data.frame")
RPI_weather_week<-data.frame(days,temp,snowed)
RPI_weather_week
head(RPI_weather_week)
str(RPI_weather_week)
summary(RPI_weather_week)
RPI_weather_week[1,]
RPI_weather_week[,1]
RPI_weather_week[,'snowed']
RPI_weather_week[,'days']
RPI_weather_week[,'temp']
RPI_weather_week[1:5,c("days",'snowed')]
RPI_weather_week$temp
subset(RPI_weather_week,subset = snowed==TRUE) #sub set to get rows and columns
sorted.snowed <- order(RPI_weather_week['snowed'])
sorted.snowed
RPI_weather_week[sorted.snowed,]
dec.snow <- order(-RPI_weather_week$temp)
dec.snow 
empty.DataFrame <- data.frame()
v1 <-1:10
v1
v2 <-letters[1:10]
v2
df <-data.frame(col.name.1=v1,col.name.2=v2)
df