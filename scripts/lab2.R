library(MASS)
library(ISLR)
library(car)

#Boston
attach(Boston)
names(Boston)

lm.fit = lm(medv~lstat, data = Boston)

names(lm.fit)

confint(lm.fit)

predict(lm.fit, data.frame(lstat = c(5,10,15)),
        interval = "confidence")

predict(lm.fit, data.frame(lstat = c(5,10,15)),
        interval = "prediction")

plot(lstat, medv, pch = 20)
abline(lm.fit, lwd = 2)

plot(1:20,1:20, pch = 1:20)


par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))



lm.fit = lm(medv~ lstat +age)
summary(lm.fit)

lm.fit = lm(medv~., data = Boston)
summary(lm.fit)


vif(lm.fit)
lm.fit1 = lm(medv~. - age, data = Boston)

summary(lm(medv~lstat*age, data = Boston))

lm.fit2 = lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

lm.fit = lm(medv~ lstat)
anova(lm.fit, lm.fit2)

plot(lm.fit2)
plot(lm.fit)

#Carseats
fix(Carseats)
names(Carseats)

lm.fit = lm(Sales ~. + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc)

#Create function
LoadLibraries=function(){
  library(ISLR)
  library(MASS)
  print("Neat.")
}

LoadLibraries()
