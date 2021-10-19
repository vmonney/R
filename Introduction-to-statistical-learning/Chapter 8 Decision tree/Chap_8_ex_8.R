##Chapter 8 exercise 8
library(ISLR)

##a

train = sample(1:nrow(Carseats), nrow(Carseats)/2)
Sales.test = Sales[-train]
##b
#fit
tree.carseats <- tree(Sales ~ ., data=  Carseats, subset = train)
summary(tree.carseats)
#plot
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.carseats

#test error
yhat = predict(tree.carseats, Carseats[-train,])
MSE_tree = mean((yhat-Sales.test)^2)
MSE_tree

##c
cv.carseats = cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
cv.carseats
which.min(cv.carseats$dev)

prune.carseats = prune.tree(tree.carseats, best = 8)
plot(prune.carseats)
text(prune.carseats, pretty=0)

yhat= predict(prune.carseats, newdata = Carseats[-train,])
plot(yhat, Sales.test)
abline(0,1)
MSE_prune = mean((yhat-Sales.test)^2)
MSE_prune

## d bagging
library(randomForest)
bag.carseats = randomForest(Sales ~., data = Carseats, subset = train, mtry=10, importance = TRUE)
bag.carseats

yhat.bag = predict(bag.carseats, newdata = Carseats[-train,])
plot(yhat.bag, Sales.test)
abline(0,1)
mean((yhat.bag-Sales.test)^2)

importance_table = importance(bag.carseats)
order = order(importance(bag.carseats)[,1])
## e random forest // p/3 for regression and sqrt(p) for classification
rf.carseats = randomForest(Sales~., data=Carseats, subset = train, mtry=3, importance =TRUE)
yhat.rf = predict(rf.carseats, newdata = Carseats[-train,])
mean((yhat.rf-Sales.test)^2)

importance(rf.carseats)[order(importance(rf.carseats)[,1]),]
plot(rf.carseats)
