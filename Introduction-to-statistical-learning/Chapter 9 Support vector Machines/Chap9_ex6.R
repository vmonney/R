## Chapter 9 exercise 6

if(!require("MASS")){install.packages("MASS")}
if(!require("e1071")){install.packages("e1071")}

set.seed(1)

x = matrix( rnorm(20*2), ncol=2)
y = c(rep(-1,10), rep(1,10))
x[y==1,] = x[y==1,]+ 1
x[y==1,] = x[y==1,]+ 0.5

plot(x[,1], x[,2], col = (y+3), pch = 19, cex = 1.25, xlab = "x1", ylab = "x2", main = "initial data")


## b 
dat = data.frame(x1 = x[,1], x2 = x[,2], y = as.factor(y))

tune.out = tune(svm, y~., data=dat, kernel = "linear", ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(tune.out)

bestmod = tune.out$best.model
summary(bestmod)

## generate test data 

x_test = matrix( rnorm(20*2), ncol=2)
y_test = c(rep(-1,10), rep(1,10))
x_test[y==1,] = x_test[y==1,]+ 1
x_test[y==1,] = x_test[y==1,]+ 0.5

dat_test = data.frame(x1 = x_test[,1], x2 = x_test[,2], y = as.factor(y_test))


y_hat = predict(bestmod, newdata = dat_test)
y_hat = as.numeric(as.character(y_hat))
print(sprintf("Linear SVM training error rate = %10.6f", 1 - sum(y_hat == y) / length(y)))