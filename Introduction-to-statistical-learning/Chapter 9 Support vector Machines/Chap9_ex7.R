## Chapter 9 Exercise 7

if(!require("MASS")){install.packages("MASS")}
if(!require("e1071")){install.packages("e1071")}
if(!require("ISLR")){install.packages("ISLR")}

attach(Auto)
##a
Auto[Auto$mpg > median(mpg), "gm"] = 1
Auto[is.na(Auto$gm), "gm"] = 0
Auto$gm

##b

tune.out = tune(svm, gm~., data=Auto, kernel = "linear", ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(tune.out)

tune.out = tune(svm, gm ~., data = Auto, kernel = "radial", ranges = list(cost = c(0.001,0.01, 0.1, 1, 5, 10, 100, 1000), gamma = c(0.5,1,2,3,4)))
summary(tune.out)

plot(tune.out$best.model, Auto, weight~horsepower)

tune.out = tune(svm, gm ~., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.001,0.01, 0.1, 1, 5, 10, 100, 1000), gamma = c(0.5,1,2,3,4)))
summary(tune.out)