## chapitre 8 exercice 9
library(ISLR)
attach(OJ)

set.seed(0)
##a

train = sample(1:nrow(OJ), 800)
Purchase.test = Purchase[-train]

##b error rate : 0.1588 / 9 terminal nodes

tree.oj <- tree(Purchase ~ ., data = OJ, subset = train)
summary(tree.oj)

## c 

tree.oj
## analysing the result example 2) : 
## LoyalCH < 0.48285 == the split criterion
## 292 == number of observations
## 330.60 == the deviance
## MM == the overall prediction for the branch
## 0.25342 0.74658 == the fraction of observations that takes the value CH vs MM
## the * indicate a terminal node

## d 
plot(tree.oj)
text(tree.oj, pretty=0)

## e predict the test error // error rate = 0.17
tree.pred = predict(tree.oj, newdata = OJ[-train,], type="class")
table(tree.pred, Purchase.test)
## error rate 
(28+24)/270

## f determine the optimal tree size with cv.tree / optimal = 8

cv.oj = cv.tree(tree.oj, FUN=prune.misclass)## prune.misclass to use the error rate measure instead of the deviance
cv.oj

## g  plot the error rate according to the pruned tree
par(mfrow=c(1,2))
plot(cv.oj$size, cv.oj$dev, type = "b")
plot(cv.oj$k, cv.oj$dev, type = "b")

## h 8 or 9 nodes give equal results

## i produce the pruned tree

prune.oj = prune.misclass(tree.oj, best=2)
plot(prune.oj)
text(prune.oj, pretty=0)
## j  the training error is the same
summary(prune.oj)
summary(tree.oj)

## k the test error is the same as well

prune.pred = predict(prune.oj, newdata = OJ[-train,], type="class")
table(prune.pred, Purchase.test)
(22+26)/270

