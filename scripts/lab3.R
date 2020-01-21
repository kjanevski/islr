library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)

pairs(Smarket)
cor(Smarket[,-9])

attach(Smarket)
plot(Volume)

glm.fits = glm(Direction ~. -Today, data = Smarket, family = binomial)
summary(glm.fits)
