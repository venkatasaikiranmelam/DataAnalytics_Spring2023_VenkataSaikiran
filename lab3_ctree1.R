require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_rpart) # try some different plot options
text(swiss_rpart) # try some different text options

require(party)

treeSwiss<-ctree(Species ~ ., data=iris)
plot(treeSwiss)

cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)

cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(mtry=2, mincriterion=0))
# look at help info, vary parameters.

library(tree)
tr <- tree(Species ~ ., data=iris)
tr
tr$frame
plot(tr)
text(tr)
#find "prettier" ways to plot the tree

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



