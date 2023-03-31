data(swiss)

pairs(~ Fertility + Education + Catholic, data = swiss, subset = Education < 20, main = "Swiss data, Education < 20")

summary(swiss)

plot(density(swiss$Fertility),main="Fertility",xlab="Fertility")
rug(swiss$Fertility)
hist(swiss$Fertility,freq=F,add=T)

# Interesting observation (higher degree of catholic comes with higher fertility )
plot(Fertility ~ Catholic, swiss, xlab="", las=3)


lmod <- lm(Fertility ~ ., swiss);
summary(lmod)


# Interaction term doesn't seem to bring major improvements
lmodi = lm(Fertility ~ (Agriculture + Education + Catholic +  Infant.Mortality)^2, data = swiss)


Wlmodp<-lm(Fertility~Agriculture+Education+poly(Catholic,2)+Infant.Mortality + Education:poly(Catholic,2), swiss) 
summary(Wlmodp)

