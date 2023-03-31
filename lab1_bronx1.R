library(gdata) 
library(dplyr)
#faster xls reader but requires perl!
bronx1<- read.csv("C:/Users/91983/OneDrive/Desktop/RPI-material/DA/Assignment4/NYC_Citywide_Annualized_Calendar_Sales_Update.csv")
bronx1<-bronx1[which(bronx1$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]


#bronx1 <- na.omit(bronx1)

attach(bronx1)

bronx1 <- bronx1  %>% select("LAND.SQUARE.FEET", "GROSS.SQUARE.FEET", "SALE.PRICE")

str(bronx1)


bronx1[["GROSS.SQUARE.FEET"]] <- as.numeric((gsub("[^0-9]","", bronx1[["GROSS.SQUARE.FEET"]])))
bronx1[["LAND.SQUARE.FEET"]] <- as.numeric((gsub("[^0-9]","", bronx1[["LAND.SQUARE.FEET"]])))


# If you choose to attach, leave out the "data=." in lm regression
SALE.PRICE<-sub("\\$","",SALE.PRICE) 
SALE.PRICE<-as.numeric(gsub(",","", SALE.PRICE)) 
GROSS.SQUARE.FEET<-as.numeric(gsub(",","", GROSS.SQUARE.FEET)) 
LAND.SQUARE.FEET<-as.numeric(gsub(",","", LAND.SQUARE.FEET)) 
plot(log(GROSS.SQUARE.FEET), log(SALE.PRICE)) 

data = bronx1[complete.cases(bronx1),]

m1 <- lm(SALE.PRICE ~ LAND.SQUARE.FEET + GROSS.SQUARE.FEET, data = data)
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

# Model 2

bronx1 <- bronx1  %>% select("LAND.SQUARE.FEET", "GROSS.SQUARE.FEET", "SALE.PRICE","NEIGHBORHOOD")

bronx1 <- na.omit(bronx1)

str(bronx1)

m2<-lm(bronx1$SALE.PRICE~bronx1$GROSS.SQUARE.FEET+bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(bronx1$SALE.PRICE~0+bronx1$GROSS.SQUARE.FEET+bronx1$LAND.SQUARE.FEET+factor(bronx1$NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

bronx1 <- bronx1  %>% select("LAND.SQUARE.FEET", "GROSS.SQUARE.FEET", "SALE.PRICE","NEIGHBORHOOD","BUILDING.CLASS.CATEGORY")

# Model 3
m3<-lm(bronx1$SALE.PRICE~0+bronx1$GROSS.SQUARE.FEET+bronx1$LAND.SQUARE.FEET+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))
#
