############# Chapitre 6 exercice 9 

library(ISLR)
attach(College)

str(College)


## a Split the data set into a training set and a test set.

train = sample(c(TRUE,FALSE), nrow(College), rep=TRUE)
test = (!train)

## (b) Fit a linear model using least squares on the training set, and report the test error obtained.
lin_mod = lm(Apps ~., data = College, subset = train)
summary(lin_mod)
Y_hat = predict(lin_mod, newdata = College[test,])
MSE = mean((College[test,"Apps"] - Y_hat)^2)
print(sprintf("Linear model test MSE= %10.3f",MSE)) 

#(c) Fit a ridge regression model on the training set, with ?? chosen by cross-validation. Report the test error obtained.
library(glmnet)

x = model.matrix(Apps ~., College)[,-1]
y = College$Apps

cv_out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv_out)
bestlam = cv_out$lambda.1se
print("the best lambda is :")
print(bestlam)

ridge_mod = glmnet(x, y, alpha = 0)
ridge_pred = predict(ridge_mod, s = bestlam, newx = x[test,])
ridge_MSE = mean((ridge_pred - y[test])^2)
print(sprintf("ridge model test MSE= %10.3f",ridge_MSE)) 

#(d) Fit a lasso model on the training set, with ?? chosen by crossvalidation.
#Report the test error obtained, along with the number of non-zero coefficient estimates.

cv_lasso = cv_out = cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv_lasso)
bestlam = cv_lasso$lambda.1se
print("the best lambda is :")
print(bestlam)

lasso_mod = glmnet(x,y,alpha = 1)
plot(lasso_mod)
lasso_pred = predict(lasso_mod, s=bestlam, newx = x[test,])
lasso_MSE = mean((lasso_pred - y[test])^2)
print(sprintf("lasso model test MSE= %10.3f",lasso_MSE)) 
print("lasso coefficients")
lasso_coef = predict(lasso_mod,type="coefficients", s=bestlam)

#(e) Fit a PCR model on the training set, with M chosen by crossvalidation.
#Report the test error obtained, along with the value of M selected by cross-

library(pls)

pcr_fit = pcr(Apps ~., data = College, subset = train,  scale = TRUE, validation = "CV")
validationplot(pcr_fit, val.type = "MSEP")
summary(pcr_fit)
# Use this to select the number of components to include ... looks like CV suggests
# we should use ALL predictors
ncomp = 17
pcr_pred = predict( pcr_fit , x[test,], ncomp =ncomp)
MSE = mean((pcr_pred - y[test])^2)
print( sprintf( "PCR (with ncomp= %5d) test MSE= %10.3f", ncomp, MSE ))

#(f) Fit a PLS model on the training set, with M chosen by crossvalidation.
#Report the test error obtained, along with the value of M selected by cross-validation.

pls_fit = plsr(Apps ~., data = College, subset = train, scale = TRUE, validation = "CV")
summary(pls_fit)
validationplot(pls_fit, val.type = "MSEP")
ncomp = 10
pls_pred = predict(pls_fit, x[test,], ncomp = ncomp)
MSE = mean((pls_pred - y[test])^2)
print( sprintf( "PLS (with ncomp= %5d) test MSE= %10.3f", ncomp, MSE ))

#(g) Comment on the results obtained. 
# How accurately can we predict the number of college applications received? Is there much
# difference among the test errors resulting from these five approaches?

#Notice that when using principal component regression (PCR) cross-validation suggested we
#use the full model which gave a MSE equal to that of least squares. For this application the
#smallest MSE on the test data when we use partial least squares.

