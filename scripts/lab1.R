#2.3
x = rnorm(100)
y = rnorm(100)

pdf("figure1.pdf")
plot(x,y)
dev.off()


x = 1:10

y=x
f=outer(x,y,function (x,y)cos(y)/(1+x^2))
plot.new()
contour(x,y,f, nlevels = 45, add = T)

fa = (f-t(f))/2

contour(x,y,fa, nlevels = 15)

image(x,y,fa)
persp(x,y,fa, theta = 30, phi = 40)


A = matrix(1:16, 4, 4)

library(ISLR)

Auto = read.table("data/Auto.data", T, 
                  na.string = "?")
fix(Auto)

Auto = na.omit(Auto)

attach(Auto)
cylinders=as.factor(cylinders)
plot(cylinders,mpg, varwidth = T, 
     xlab = "cylinders", ylab = "MPG")

hist(mpg)

pairs(~mpg + displacement + horsepower)

plot(horsepower, mpg)
identify(horsepower,mpg,name)

summary(Auto)
