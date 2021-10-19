## Chap9_Ex5

if(!require("MASS")){install.packages("MASS")}
if(!require("e1071")){install.packages("e1071")}

set.seed(0)
n = 5000
p = 2

x1 = runif(n)-0.5
x2 = runif(n)-0.5
y = 1*(x1^2-x2^2 > 0.005)

#b
plot(x1,x2,col=(y+1), pch = 19, cex = 1.05, xlab = "x1", ylab = "x2", main = "initial data")

# c
dat = data.frame(x1 = x1, x2=x2, y=as.factor(y))

m = glm(y~x1 + x2, data =dat, family = binomial)

# d

y_hat = predict(m, newdata = data.frame(x1=x1, x2=x2), type = "response")
predicted_class = 1*(y_hat > 0.5)

print(sprintf("Linear logistic regression training error rate = %10.6f", 1 - sum(predicted_class == y)/ length(y)))

svmfit = svm(y~., data = dat, kernel="linear",  cost=10, scale = FALSE) ## scale function scale each predictor to have mean 0 and sd 1
plot(svmfit, dat)

# e using logistic regression to fit a non-linear 
m = glm(y ~ x1 + x2 + I(x1^2) + I(x2^2), data = dat, family = binomial)
## f
y_hat = predict(m, newdata = data.frame(x1 = x1, x2=x2), type = "response")
predicted_class = 1 * (y_hat > 0.5)
print(sprintf("non-linear logistic regression training error rate = %10.6f", 1 - sum(predicted_class == y)/ length(y)))

## g

## Do CV to select the value of cost using the "tune" function

tune.out = tune(svm, y~., data=dat, kernel = "linear", ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(tune.out)

## selecting the best model
bestmod = tune.out$best.model
summary(bestmod)

y_hat = predict(bestmod, newdata = data.frame(x1=x1, x2=x2))
y_hat = as.numeric(as.character(y_hat))
print(sprintf("Linear SVM training error rate = %10.6f", 1 - sum(y_hat == y) / length(y)))

## h fit a non-linear SVM model

tune.out = tune(svm, y ~., data = dat, kernel = "radial", ranges = list(cost = c(0.001,0.01, 0.1, 1, 5, 10, 100, 1000), gamma = c(0.5,1,2,3,4)))
summary(tune.out)

bestmod = tune.out$best.model
summary(bestmod)

y_hat = predict(bestmod, newdata = data.frame(x1=x1, x2=x2))
y_hat = as.numeric(as.character(y_hat))
print(sprintf("Linear SVM training error rate = %10.6f", 1 - sum(y_hat == y) / length(y)))

plot(x1,x2, col=(y_hat+1), pch = 19, cex = 1.05, xlab = "x1", ylab = "x2")
