###### Chapter 6 exercise 11

if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("glmnet") ){ install.packages("glmnet") }
if( ! require("pls") ){ install.packages("pls") }
library(MASS)
## exploring the dataset
fix(Boston)
summary(Boston)

## preparing the data for the analysis

n = dim(Boston)[1]
p = dim(Boston)[2]

train = sample(c(TRUE,FALSE), n, replace = TRUE) # will roughly assign TRUE to one-half of the data (FALSE to the other half).
test = (!train)

# the full linear model 

lin_mod = lm(crim ~., data = Boston, subset = train)
summary(lin_mod)
Y_hat = predict(lin_mod, newdata = Boston[test,])
MSE = mean((Boston[test,"crim"] - Y_hat)^2)
print(sprintf("Linear model test MSE= %10.3f",MSE)) 

# ridge regression 

x = model.matrix(crim ~., Boston)
y = Boston$crim
cv_out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv_out)
bestlam = cv_out$lambda.1se
print("the best lambda is :")
print(bestlam)

ridge_mod = glmnet(x[train,], y[train], alpha = 0)

ridge_pred = predict(ridge_mod, s = bestlam, newx = x[test,])
ridge_MSE = mean((ridge_pred - y[test])^2)
print(sprintf("ridge model test MSE= %10.3f",ridge_MSE)) 

# lasso regression

cv_lasso = cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv_lasso)
bestlam = cv_lasso$lambda.1se
print("the best lambda is :")
print(bestlam)

lasso_mod = glmnet(x[train,],y[train],alpha = 1)
plot(lasso_mod)
lasso_pred = predict(lasso_mod, s=bestlam, newx = x[test,])
lasso_MSE = mean((lasso_pred - y[test])^2)
print(sprintf("lasso model test MSE= %10.3f",lasso_MSE)) 
print("lasso coefficients")
lasso_coef = predict(lasso_mod,type="coefficients", s=bestlam)

# Principle Component Regression:

pcr_mod = pcr(crim ~., data = Boston, subset = train,  scale = TRUE, validation = "CV")
validationplot(pcr_mod, val.type = "MSEP")
summary(pcr_mod)
# Use this to select the number of components to include ... looks like CV suggests
# we should use ALL predictors
ncomp = 13
pcr_pred = predict(pcr_mod , Boston[test,], ncomp =ncomp)
MSE = mean((pcr_pred - y[test])^2)
print( sprintf( "PCR (with ncomp= %5d) test MSE= %10.3f", ncomp, MSE ))

# Partial Least Squares:

pls_fit = plsr(crim ~., data = Boston, subset = train, scale = TRUE, validation = "CV")
summary(pls_fit)
validationplot(pls_fit, val.type = "MSEP")
ncomp = 9
pls_pred = predict(pls_fit, Boston[test,], ncomp = ncomp)
MSE = mean((pls_pred - Boston[test,"crim"])^2)
print( sprintf( "PLS (with ncomp= %5d) test MSE= %10.3f", ncomp, MSE ))

pls_fit_9 = plsr(crim ~., data = Boston, scale = TRUE, ncomp = 9)
summary(pls_fit_9)

