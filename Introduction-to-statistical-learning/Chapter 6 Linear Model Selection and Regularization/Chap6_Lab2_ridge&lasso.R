# Chapter 6 LAB2 Ridge Regression and the Lasso

if( ! require("glmnet") ){ install.packages("glmnet") }
library(glmnet)
Hitters = na.omit(Hitters)

x = model.matrix(Salary ~.,Hitters)[,-1]
y = Hitters$Salary

## Ridge Regression
grid=10^seq(10,-2,length=100)
ridge_mod = glmnet(x,y,alpha = 0, lambda = grid) #alpha = 0 --> ridge / alpha = 1 --> lasso
dim(coef(ridge_mod))

ridge_mod$lambda[50]
coef(ridge_mod)[,50]
sqrt(sum(coef(ridge_mod)[-1,50]^2)) ## calculatin the "l2" norm

ridge_mod$lambda[60]
coef(ridge_mod)[,60]
sqrt(sum(coef(ridge_mod)[-1,60]^2))

## predict the ridge regression coefficient for a particular value
predict(ridge_mod, s=50, type="coefficients")[1:20,]

## split between training and test set
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y_test = y[test]

ridge_mod = glmnet(x[train,],y[train], alpha = 0, lambda = grid, thresh=1e-12)
ridge_pred = predict(ridge_mod, s=4, newx = x[test,])
mean((ridge_pred-y_test)^2)
## with a model with just an intercept
mean((mean(y[train])-y_test)^2)
## equivalent with lambda very large
ridge_pred = predict(ridge_mod, s=1e10, newx=x[test,])
mean((ridge_pred - y_test)^2)
## Comparing with the least square regression
ridge_pred = predict(ridge_mod, s=0, newx = x[test,], exact = T) ## exact = T is to be sure that we do a least square regression, otherwise it extrapolates over the grid values
lm(y~x, subset = train)

## using cross validation to choose the best lambda value
set.seed(1)
cv_out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv_out)
bestlam = cv_out$lambda.min
bestlam

ridge_pred = predict(ridge_mod, s=bestlam, newx=x[test,])
##calculating the MSE
mean((ridge_pred - y_test)^2)

## refit the ridge regression on the full dataset using the best lambda found
out = glmnet(x,y,alpha=0)
predict(out,type="coefficients", s=bestlam)[1:20,]






######################## The Lasso ################################

lasso_mod = glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso_mod)

### doing a cross-validation and compute the associated test error
set.seed(1)
cv_out = cv.glmnet(x[train,],y[train], alpha = 1)
plot(cv_out)
bestlam = cv_out$lambda.min
lasso_pred = predict(lasso_mod, s=bestlam, newx = x[test,])
mean((lasso_pred - y_test)^2)

out = glmnet(x,y,alpha=1, lambda = grid )
lasso.coef = predict(out,type="coefficients", s=bestlam)[1:20,]

lasso.coef[lasso.coef != 0]
