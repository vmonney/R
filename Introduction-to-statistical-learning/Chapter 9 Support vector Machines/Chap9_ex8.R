## Chapter 9 Exercise 8

if(!require("e1071")){install.packages("e1071")}
if(!require("ISLR")){install.packages("ISLR")}

attach(OJ)


##a)
n_train = 800
n_test = nrow(OJ) - n_train
train = sample(nrow(OJ), n_train)

##b 

svmfit = svm(Purchase ~., data = OJ[train,], cost = 0.01, kernel = "linear")
summary(svmfit)

##c training and test error set
#train
y_hat = predict(svmfit, newdata = OJ[train,])
table(predicted = y_hat, truth = OJ[train,]$Purchase)
print(sprintf("Linear SVM training error rate (cost = 0.01) = %10.6f", 1 - sum(y_hat == OJ[train,]$Purchase) / n_train))

#test
y_hat = predict(svmfit, newdata = OJ[-train,])
table(predicted = y_hat, truth = OJ[-train,]$Purchase)
print(sprintf("Linear SVM training error rate (cost = 0.01) = %10.6f", 1 - sum(y_hat == OJ[-train,]$Purchase) / n_test))

## d 
tune.out = tune(svm, Purchase~., data=OJ[train,], kernel = "linear", ranges = list(cost=c(0.01, 0.1, 1, 5, 10)))
summary(tune.out)
## selecting the best model
bestmod = tune.out$best.model
summary(bestmod)

## e 
y_hat = predict(bestmod, newdata = OJ[train,])
table(predicted = y_hat, truth = OJ[train,]$Purchase)
print(sprintf("Linear SVM training error rate (cost = 0.01) = %10.6f", 1 - sum(y_hat == OJ[train,]$Purchase) / n_train))

#test
y_hat = predict(bestmod, newdata = OJ[-train,])
table(predicted = y_hat, truth = OJ[-train,]$Purchase)
print(sprintf("Linear SVM training error rate (cost = 0.01) = %10.6f", 1 - sum(y_hat == OJ[-train,]$Purchase) / n_test))

## f 

svmrad = svm(Purchase ~., data = OJ[train,], cost = 0.01, kernel = "polynomial", degree = 2)
summary(svmrad)

y_hat = predict(svmrad, newdata = OJ[train,])
table(predicted = y_hat, truth = OJ[train,]$Purchase)
print(sprintf("Linear SVM training error rate (cost = 0.01) = %10.6f", 1 - sum(y_hat == OJ[train,]$Purchase) / n_train))

#test
y_hat = predict(svmrad, newdata = OJ[-train,])
table(predicted = y_hat, truth = OJ[-train,]$Purchase)
print(sprintf("Linear SVM training error rate (cost = 0.01) = %10.6f", 1 - sum(y_hat == OJ[-train,]$Purchase) / n_test))

tune.out = tune(svm, Purchase~., data=OJ[train,], kernel = "polynomial", ranges = list(cost=c(0.01, 0.1, 1, 5, 10)))
summary(tune.out)
## selecting the best model
bestmod = tune.out$best.model
summary(bestmod)

#train
y_hat = predict(bestmod, newdata = OJ[train,])
table(predicted = y_hat, truth = OJ[train,]$Purchase)
print(sprintf("Linear SVM training error rate (cost = 0.01) = %10.6f", 1 - sum(y_hat == OJ[train,]$Purchase) / n_train))

#test
y_hat = predict(bestmod, newdata = OJ[-train,])
table(predicted = y_hat, truth = OJ[-train,]$Purchase)
print(sprintf("Linear SVM training error rate (cost = 0.01) = %10.6f", 1 - sum(y_hat == OJ[-train,]$Purchase) / n_test))
