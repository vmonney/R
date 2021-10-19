##Chapter 8 exercise 10

library(ISLR)
attach(Hitters)
##a
summary(Hitters)
Hitters = na.omit(Hitters)
Hitters$Salary = log(Hitters$Salary)

##b 
train = sample(1:nrow(Hitters), 200)
Salary.test = Hitters$Salary[-train]

##c + d boosting

library(gbm)
set.seed(0)

lambda_set = seq(1.e-4, 0.04, by=0.001)
training_MSE = rep(NA,length(lambda_set))
test_MSE = rep(NA, length(lambda_set))

for(i in 1:length(lambda_set)){
  boost.hitters = gbm(Salary ~., data=Hitters[train,], distribution = "gaussian", interaction.depth=4, n.trees = 1000, shrinkage = lambda_set[i])
  yhat.boost = predict(boost.hitters, newdata = Hitters[-train,], n.trees=1000)
  test_MSE[i] = mean((yhat.boost-Salary.test)^2)
  
  yhat.boost.train = predict(boost.hitters, newdata = Hitters[train,], n.trees=1000)
  training_MSE[i]= mean((yhat.boost.train -Hitters[train,]$Salary)^2)
}

## plot
par(mfrow = c(1,2))
plot(lambda_set, training_MSE, type = "b", col = "green", pch=19, xlab="Lambda Value", ylab="MSE")
lines(lambda_set, test_MSE, type = "b", col = "red", pch=19, xlab="Lambda Value", ylab="Test MSE")

## e 

# Try linear regression:
# 
m = lm( Salary ~ ., data=Hitters[train,] )
y_hat = predict( m, newdata=Hitters[-train,] )
print( 'linear regression test MSE:' )
print( mean( ( y_hat - Salary.test)^2 ) )

# Try the lasso:
#
library(glmnet)
MM = model.matrix( Salary ~ ., data=Hitters[train,] )
cv.out = cv.glmnet( MM, Hitters[train,]$Salary, alpha=1 )
bestlam = cv.out$lambda.1se
print( "lasso CV best value of lambda (one standard error)" )
print( bestlam )

lasso.mod = glmnet( MM, Hitters[train,]$Salary, alpha=1 )

MM_test = model.matrix( Salary ~ ., data=Hitters[-train,] )
y_hat = predict( lasso.mod, s=bestlam, newx=MM_test )
print( 'lasso regression test MSE:' )
print( mean( ( y_hat - Hitters[-train,]$Salary )^2 ) )


# Try ridge regression:
#
cv.out = cv.glmnet( MM, Hitters[train,]$Salary, alpha=0 )
plot( cv.out ) 
bestlam = cv.out$lambda.1se
print( "ridge CV best value of lambda (one standard error)" )
print( bestlam )

ridge.mod = glmnet( MM, Hitters[train,]$Salary, alpha=0 )
Y_hat = predict( ridge.mod, s=bestlam, newx=MM_test )
print( 'ridge regression test MSE:' )
print( mean( ( y_hat - Hitters[-train,]$Salary )^2 ) )

## f most important variable
boost.hitters_best = gbm(Salary ~., data=Hitters[train,], distribution = "gaussian", interaction.depth=4, n.trees = 1000, shrinkage = lambda_set[3])
summary(boost.hitters_best)

## g bagging
library(randomForest)
bag.hitters = randomForest(Salary ~., data = Hitters, subset = train, importance = TRUE)
bag.hitters

yhat.bag = predict(bag.hitters, newdata = Hitters[-train,])
plot(yhat.bag, Salary.test)
abline(0,1)
mean((yhat.bag-Salary.test)^2)

importance(bag.hitters)[order(importance(bag.hitters)[,1], decreasing = TRUE ),]
