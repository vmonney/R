##Chapitre 6 exercice 8

if( ! require("ISLR") ){ install.packages("ISLR"); library("ISLR") }
if( ! require("leaps") ){ install.packages("leaps"); library("leaps") }
if( ! require("glmnet") ){ install.packages("glmnet"); library("glmnet") }

library(leaps)
library(glmnet)
##a
n = 100
x = rnorm(n)
epsilon = 0.1 * rnorm(n)
b0 = 1
b1 = -0.1
b2 = 0.05
b3 = 0.75

y = b0 + b1*x + b2*x^2 + b3*x^3 + epsilon
data = data.frame( y=y, x=x, x2=x^2, x3=x^3, x4=x^4, x5=x^5, x6=x^6, x7=x^7, x8=x^8, x9=x^9, x10=x^10 )

# Use the validation approach with regsubsets

train = sample(c(TRUE,FALSE), nrow(data), rep=TRUE) # will roughly assign TRUE to one-half of the data (FALSE to the other half).
test = (!train)

# Apply best subset selection: 

regfit_10 = regsubsets(y ~., data = data[train,], nvmax = 10)
reg_summary = summary(regfit_10)
print(reg_summary)

# Test models on the validation set:
test.mat = model.matrix(y ~., data = data[test,])

val_error = rep(NA,10)
for(i in 1:10){
  coefi = coef(regfit_10, id = i)
  pred = test.mat[,names(coefi)]%*%coefi
  val_error[i]= mean((data$y[test] - pred)^2)
}

print( "best subset validation errors" )
print( val_error )
k = which.min( val_error ) 
print( k )
print( coef( regfit_10, id=k ) )

### ploting errors AIC, R2, AdjR2, BIC and Cp
par(mfrow = c(2,2))
plot(reg_summary$rss, xlab = "Number of variables", ylab = "RSS", type = "l")
plot(reg_summary$adjr2, xlab = "Number of variables", ylab = "Adjuster R2", type = "l")
n_adjr2 = which.max(reg_summary$adjr2)
print(n_adjr2)
points(n_adjr2,reg_summary$adjr2[n_adjr2], col = "red", cex = 2, pch = 20 )

plot(reg_summary$cp, xlab = "Number of variables", ylab = "Cp", type = "l")
n_cp = which.min(reg_summary$cp)
print(n_cp)
points(n_cp, reg_summary$cp[n_cp],col = "red", cex = 2, pch = 20)
plot(reg_summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
n_bic = which.min(reg_summary$bic)
print(n_bic)
points(n_bic, reg_summary$bic[n_bic],col = "red", cex = 2, pch = 20)

plot(regfit_10, scale = "r2")
plot(regfit_10, scale = "adjr2")
plot(regfit_10, scale = "Cp")
plot(regfit_10, scale = "bic")

coef(regfit_10, 3)

## use the cross-validation approach for choosing the variables
#function for the validation
predict_regsubsets = function(object, newdata, id,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)
  coefi=coef(object, id = id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}

k = 10
folds = sample(1:k, nrow(data), replace = TRUE)
cv_errors = matrix(NA, k, 10, dimnames = list(NULL, paste(1:10)))

for(j in 1:k){
  best_fit = regsubsets(y ~., data = data[folds !=j,], nvmax = 10)
  for(i in 1:10){
    pred = predict_regsubsets(best_fit, data[folds==j,], id = i)
    cv_errors[j,i] = mean((data$y[folds==j]-pred)^2)
  }
}
mean_cv_errors = apply(cv_errors,2,mean)
par(mfrow=c(1,1))
plot(mean_cv_errors, type = "b")

print( "best subset validation errors" )
print( mean_cv_errors )
k = which.min( mean_cv_errors) 
print( k )
print( coef( best_fit, id=k ) )


## b testing with forward and backward selection

regfit_10_fwd = regsubsets(y ~., data = data[train,], nvmax = 10, method = "forward")
reg_summary = summary(regfit_10_fwd)

val_error = rep(NA,10)
for(i in 1:10){
  coefi = coef(regfit_10_fwd, id = i)
  pred = test.mat[,names(coefi)]%*%coefi
  val_error[i]= mean((data$y[test] - pred)^2)
}

print( "best subset validation errors" )
print( val_error )
k = which.min( val_error ) 
print( k )
print( coef( regfit_10, id=k ) )

## Backward

regfit_10_bwd = regsubsets(y ~., data = data, nvmax = 10, method = "backward")
reg_summary = summary(regfit_10_bwd)

val_error = rep(NA,10)
for(i in 1:10){
  coefi = coef(regfit_10_bwd, id = i)
  pred = test.mat[,names(coefi)]%*%coefi
  val_error[i]= mean((data$y[test] - pred)^2)
}

print( "best subset validation errors" )
print( val_error )
k = which.min( val_error ) 
print( k )
print( coef( regfit_10, id=k ) )


############### e , lasso with cross-validation ##########################
# Now apply the lasso to our training set:
# First fit the lasso model for all of the given lambda values :

grid = 10^seq(10,-2,length=100) # a grid of lambda values
Y = data$y
X = model.matrix(y ~. , data = data)# the predictors as a datamatrix
lasso_mod_10 = glmnet(X, Y, alpha = 1, lambda = grid)
plot(lasso_mod_10) # plots the extracted coefficients as a function of lambda 

# Apply cross validation (to pick the best value of lambda):
# 
cv_out = cv.glmnet(X,Y, alpha = 1)
plot(cv_out)
bestlam = cv_out$lambda.1se
print( "lasso CV best value of lambda (one standard error)" )
print( bestlam )

out =  glmnet(X, Y, alpha = 1, lambda = grid)
lasso_coef = predict(out,type="coefficients", s=bestlam)
print(lasso_coef)

## f Now generate a response vector Y according to the model Y = ??0 + ??7X7 + epsilon,and perform best subset selection and the lasso.
# Part (f) Try a different regression function:
#
X = rnorm(n)
epsilon = 0.1 * rnorm(n)

beta_0 = 1.0 
beta_7 = 2.5
Y = beta_0 + beta_7 * X^7 + epsilon
DF = data.frame( Y=Y, X=X, X2=X^2, X3=X^3, X4=X^4, X5=X^5, X6=X^6, X7=X^7, X8=X^8, X9=X^9, X10=X^10 )

train = sample(c(TRUE,FALSE), n, rep=TRUE) # will roughly assign TRUE to one-half of the data (FALSE to the other half).
test = (!train)

# Best subset selection:
# 
regfit.full = regsubsets( Y ~ ., data=DF[train,], nvmax=10 )
print( summary( regfit.full ) )
# Test best subset models on the validation set:
#
test.mat = model.matrix( Y ~ ., data=DF[test,] )
val.errors = rep(NA,10)
for( ii in 1:10 ){
  coefi = coef( regfit.full, id=ii )
  pred = test.mat[,names(coefi)] %*% coefi
  val.errors[ii] = mean( ( DF$Y[test] - pred )^2 ) 
}
print( "best subsets validation errors" )
print( val.errors )
k = which.min( val.errors )
print( k ) 
print( "best subsets optimal coefficients" )
print( coef( regfit.full, id=k ) ) # print the coefficients of the best model 
print( val.errors[k] ) 

# Using the lasso technique:
#

# First apply cross validation (to find the optimal value of lambda):
# 
MM = model.matrix( Y ~ ., data=DF )

cv.out = cv.glmnet( MM, Y, alpha=1 )
plot( cv.out ) 
bestlam = cv.out$lambda.1se
print( "best lambda (1 se)" )
print( bestlam )

# Now fit the lasso with this value of lambda: 
lasso.mod = glmnet( MM, Y, alpha=1 )

lasso.coef = predict( lasso.mod, type="coefficients", s=bestlam )
print( "lasso optimal coefficients" )
print( lasso.coef )

print( "I do not think the predict method is working correctly..." )
lasso.predict = predict( lasso.mod, s=bestlam, newx=MM )
print( "lasso MSE error" )
print( mean( ( Y - lasso.predict )^2 ) )

