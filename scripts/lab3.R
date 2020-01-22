library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)

pairs(Smarket)
cor(Smarket[,-9])

attach(Smarket)
plot(Volume)

glm.fits = glm(Direction ~. -Today - Year, data = Smarket, family = binomial)
summary(glm.fits)

coef(glm.fits)

glm.probs=predict(glm.fits, type = "response")
contrasts(Direction)
glm.probs[1:10]

glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5] = "Up"

table(glm.pred,Direction)

train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

glm.fits=glm(Direction~. -Year -Today, data = Smarket, family = binomial, subset=train)
glm.probs=predict(glm.fits, Smarket.2005, type="response")

glm.pred=rep("Down", 252)
glm.pred[glm.probs >.5] = "Up"

table(glm.pred,Direction.2005)




glm.fits=glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"

table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

predict(glm.fits,newdata=data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)),type="response")

