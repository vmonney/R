### Chapter 9 Lab

library(e1071)

################## Support Vector Classifier ##################
## generating the observations

set.seed(1)
x = matrix(rnorm(20*2), ncol=2)
y = c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1

## checking whether its linearly separable

plot(x, col=(3-y))

## coding the response as factor for classification
dat = data.frame(x=x, y=as.factor(y))

svmfit = svm(y~., data = dat, kernel="linear",  cost=10, scale = FALSE) ## scale function scale each predictor to have mean 0 and sd 1
plot(svmfit, dat)

## identify support vectors
svmfit$index
summary(svmfit)

## with smaller cost
svmfit = svm(y~., data = dat, kernel="linear",  cost=0.01, scale = FALSE)
plot(svmfit, dat)
svmfit$index

## using cross-validation with a range of values of the cost parameter
set.seed(1)
tune.out = tune(svm, y~., data=dat, kernel = "linear", ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
## selecting the best model
bestmod = tune.out$best.model
summary(bestmod)

## generating the test data set
xtest = matrix(rnorm(20*2), ncol = 2)
ytest = sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] +1
testdat = data.frame(x=xtest, y = as.factor(ytest))
testdat
## predict the class of the test observations
ypred = predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)

### A situation in which the two classes are linearly separable
x[y==1,] = x[y==1,] + 0.5
plot(x, col=(y+5)/2, pch = 19)

dat = data.frame(x=x, y=as.factor(y))
svmfit = svm(y~., data = dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat)

################   Support Vector Machine #########################
# generating the data
set.seed(1)
x = matrix(rnorm(200*2), ncol = 2)
x[1:100,] = x[1:100,] + 2
x[101:150,] = x[101:150,] -2
y = c(rep(1,150), rep(2,50))
dat = data.frame(x = x, y=as.factor(y))

plot(x, col=y)

# fitting a support vector machine
train = sample(200,100)
svmfit = svm(y ~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train,])
summary(svmfit)

## using cross-validation to select the best choice of gamma and cost for a radial kernel:
set.seed(1)

tune.out = tune(svm, y~., data = dat[train,], kernel = "radial", ranges = list(cost=c(0.1,1,10,100,1000), gamma = c(0.5,1,2,3,4)))
summary(tune.out)

table(true = dat[-train, "y"], pred = predict(tune.out$best.model, newx = dat[-train,]))
(19+16) / 100

##### ROC CURVE ###################

library(ROCR)
rocplot = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)
}

svmfit.opt = svm(y~., data=dat[train,], kernel = "radial", gamma = 2, cost = 1, decision.values = T)
fitted = attributes(predict(svmfit.opt, dat[train,], decision.values = T))$decision.values
fitted
par(mfrow = c(1,2))
rocplot(fitted, dat[train,"y"], main = "Training Data")

svmfit.flex = svm(y~., data = dat[train,], kernel = "radial", gamma = 50, cost = 1, decision.values = T)
fitted = attributes(predict(svmfit.flex, dat[train,], decision.values = T))$decision.values
rocplot(fitted, dat[train,"y"], add = T, col = "red")

## testing on test data
fitted = attributes(predict(svmfit.opt, dat[-train,], decision.values=T))$decision.values
rocplot(fitted, dat[-train, "y"], main="Test Data")
fitted = attributes(predict(svmfit.flex, dat[-train,], decision.values=T))$decision.values
rocplot(fitted, dat[-train, "y"], add = T, col = "red")

######## SVM with multiple Classes #####################
set.seed(1)
x = rbind(x, matrix(rnorm(50*22), ncol=2))
y = c(y, rep(0,50))
x[y==0, 2] = x[y==0,2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))

## fit an svm to the data

svmfit = svm(y~., data=dat, kernel = "radial", cost=10, gamma=1)
plot(svmfit, dat)

## application to gene expression Data

library(ISLR)
names(Khan)
dim(Khan$xtrain)
length(Khan$ytrain)
length(Khan$ytest)

table(Khan$ytrain)
table(Khan$ytest)

## with a lot of predictors, we use a linear kernel bcs additional flexibility with polynomial or radial is not necessary
data=data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out = svm(y~., data = dat, kernel = "linear", cost = 10)
summary(out)
table(out$fitted, dat$y)

# results on test dataset

dat.te = data.frame(x=Khan$xtest, y = as.factor(Khan$ytest))
pred.te = predict(out, newdata = dat.te)
table(pred.te, dat.te$y)
