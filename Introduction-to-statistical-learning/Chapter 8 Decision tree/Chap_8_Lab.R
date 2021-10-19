if( ! require("tree") ){ install.packages("tree") }
if( ! require("randomForest") ){ install.packages("randomForest") }
if( ! require("gbm") ){ install.packages("gbm") }
library(tree)
library(ISLR)
attach(Carseats)

## tree for qualitative response   CLASSIFICATION

High <- as.factor(ifelse(Sales <= 8, "No", "Yes"))
Carseats <- data.frame(Carseats, High)
tree.carseats <- tree(High ~ . - Sales, Carseats)
summary(tree.carseats)

#plot
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.carseats

# predict the test error
set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test = High[-train]
tree.carseats = tree(High ~. -Sales, Carseats, subset = train)
tree.pred = predict(tree.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
## correct prediction
(104+50)/200


### Pruning the tree
set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)## prune.misclass to use the error rate measure instead of the deviance
names(cv.carseats)
cv.carseats

## plot the error rate according to the pruned tree
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

### making the pruned tree
prune.carseats = prune.misclass(tree.carseats, best=3)
plot(prune.carseats)
text(prune.carseats, pretty=0)

tree.pred = predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
(93+58)/200

## tree for quantitative response REGRESSION

library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv ~., Boston, subset= train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty=0)

## PRUNE tree for regression
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")
cv.boston

prune.boston = prune.tree(tree.boston, best = 7)
plot(prune.boston)
text(prune.boston, pretty=0)

## predict
yhat= predict(tree.boston, newdata = Boston[-train,])
boston.test = Boston[-train,"medv"]
plot(yhat, boston.test)
abline(0,1)
MSE = mean((yhat-boston.test)^2)
sqrt(MSE) ## this model leads to test predictions that are within around $5,940 of the true median home value for the suburb.


### Bagging and random forest

library(randomForest)
set.seed(1)
bag.boston = randomForest(medv ~., data = Boston, subset = train, mtry=13, importance = TRUE)
bag.boston

yhat.bag = predict(bag.boston, newdata = Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

bag.boston = randomForest(medv ~., data = Boston, subset = train, mtry=13, importance = TRUE, ntree=25)
yhat.bag = predict(bag.boston, newdata = Boston[-train,])
mean((yhat.bag - boston.test)^2)

set.seed(1)
rf.boston = randomForest(medv~., data=Boston, subset = train, mtry=6, importance =TRUE)
yhat.rf = predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf-boston.test)^2)

## measuring and plotting importance
importance(rf.boston)
varImpPlot(rf.boston)

### Boosting
library(gbm)
set.seed(1)
boost.boston = gbm(medv ~., data=Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost.boston)

## partial dependence plot
par(mfrow = c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

yhat.boost = predict(boost.boston, newdata = Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2)


boost.boston = gbm(medv ~., data = Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage=0.2, verbose = F)
yhat.boost = predict(boost.boston, newdata = Boston[-train,],n.trees = 5000)
mean((yhat.boost-boston.test)^2)
