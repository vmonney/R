# Chapter 6 LAB SUBSET SELECTION METHOD

#Best Subset Selection
library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters = na.omit(Hitters)

if( ! require("leaps") ){ install.packages("leaps") }
library(leaps)
## fit a model using subset Selection
regfit_full = regsubsets(Salary ~ ., Hitters, nvmax = 19)
reg_summary = summary(regfit_full)
names(reg_summary)
reg_summary$rsq

### ploting errors RSS, R2, AdjR2, BIC and Cp
par(mfrow = c(2,2))
plot(reg_summary$rss, xlab = "Number of variables", ylab = "RSS", type = "l")
plot(reg_summary$adjr2, xlab = "Number of variables", ylab = "Adjuster R2", type = "l")
which.max(reg_summary$adjr2)
points(11,reg_summary$adjr2[11], col = "red", cex = 2, pch = 20 )

plot(reg_summary$cp, xlab = "Number of variables", ylab = "Cp", type = "l")
which.min(reg_summary$cp)
points(10, reg_summary$cp[10],col = "red", cex = 2, pch = 20)
plot(reg_summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
which.min(reg_summary$bic)
points(6, reg_summary$bic[6],col = "red", cex = 2, pch = 20)

## Built in plot for regsubsets

plot(regfit_full, scale = "r2")
plot(regfit_full, scale = "adjr2")
plot(regfit_full, scale = "Cp")
plot(regfit_full, scale = "bic")

coef(regfit_full, 6)
coef(regfit_full, 10)

# Forward and Backward Stepwise Selection
regfit_fwd = regsubsets(Salary ~ ., Hitters, nvmax = 19, method = "forward")
summary(regfit_fwd)
regfit_bwd = regsubsets(Salary ~., Hitters, nvmax = 19, method = "backward")
summary(regfit_bwd)

coef(regfit_full,7)
coef(regfit_fwd, 7)
coef(regfit_bwd, 7)

# the validation set approach for choosing the variables
set.seed(1)
train = sample(c(TRUE,FALSE), nrow(Hitters), rep=TRUE)
test = (!train)

regfit.best = regsubsets(Salary ~., Hitters[train,], nvmax=19)
test.mat = model.matrix(Salary ~., Hitters[test,])

val_error = rep(NA,19)
for(i in 1:19){
  coefi = coef(regfit.best, id = i)
  pred = test.mat[,names(coefi)]%*%coefi
  val_error[i]= mean((Hitters$Salary[test]-pred)^2)
}

val_error
which.min(val_error)
coef(regfit.best, 7)

#function for the validation
predict_regsubsets = function(object, newdata, id,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)
  coefi=coef(object, id = id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}

## select the best variables and unsing it on the full dataset
regfit.best = regsubsets(Salary ~., data = Hitters, nvmax = 19)
coef(regfit.best, 7)

## the cross-validation approach for choosing the variables
k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace = TRUE)
cv_errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for(j in 1:k){
  best_fit = regsubsets(Salary ~., data = Hitters[folds !=j,], nvmax = 19)
  for(i in 1:19){
    pred = predict_regsubsets(best_fit, Hitters[folds==j,], id = i)
    cv_errors[j,i] = mean((Hitters$Salary[folds==j]-pred)^2)
  }
}
mean_cv_errors = apply(cv_errors,2,mean)
mean_cv_errors
par(mfrow=c(1,1))
plot(mean_cv_errors, type = "b")

##performing the best subset selection on the full dataset with the number of variables chosen before
reg_best = regsubsets(Salary ~., data = Hitters, nvmax = 19)
coef(reg_best, 10)
