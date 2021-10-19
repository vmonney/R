### Chap_7_ex_8
library(ISLR)
library(boot)
library(leaps)
library(gam)
attach(Auto)

## discovering the data
?Auto
Auto_a <- subset(Auto, select = -name)  
str(Auto_a)
summary(Auto_a)

# fitting a linear regression with all the variables
lin_mod_all <- lm(mpg ~., data = Auto_a)
summary(lin_mod_all)

## choosing the best subset with cross-validation
regfit.best = regsubsets(mpg ~., data = Auto_a, nvmax = 7)
summary(regfit.best)


#function for the validation
predict_regsubsets = function(object, newdata, id,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)
  coefi=coef(object, id = id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}


## the cross-validation approach for choosing the variables
k = 10
folds = sample(1:k, nrow(Auto_a), replace = TRUE)
cv_errors = matrix(NA, k, 7, dimnames = list(NULL, paste(1:7)))

for(j in 1:k){
  best_fit = regsubsets(mpg ~., data = Auto_a[folds !=j,], nvmax = 7)
  for(i in 1:7){
    pred = predict_regsubsets(best_fit, Auto_a[folds==j,], id = i)
    cv_errors[j,i] = mean((Auto_a$mpg[folds==j]-pred)^2)
  }
}

mean_cv_errors = apply(cv_errors,2,mean)
mean_cv_errors
par(mfrow=c(1,1))
plot(mean_cv_errors, type = "b")


## selecting 1 se value to choose a simpler model
cv_errors_stderr = apply(cv_errors,2,sd)/sqrt(k)
min.cv.index = which.min( mean_cv_errors )
one_se_up_value = ( mean_cv_errors+cv_errors_stderr )[min.cv.index] 

## the best model according to the backward method and 1sd error 
coef(regfit.best, 2)

### fitting a linear regression with the selected variables and check for linearity
lin_mod_2 <- lm(mpg ~ weight + year, data = Auto_a)
summary(lin_mod_2)

## there seems to be nonlinearity in the model
par(mfrow = c(2,2))
plot(lin_mod_2)

## The weight seems to be nonlinear with mpg
plot(Auto_a$weight, Auto_a$mpg)
plot(Auto_a$year, Auto_a$mpg)

# finding the best nonlinear tool for modelling weight 

## 1. linear regression ## the best is with polynomial 2
fit.5=lm(mpg~poly(weight,5),data=Auto_a)
summary(fit.5)

fit.2=lm(mpg~poly(weight,2),data=Auto_a)

gam1 = lm(mpg ~poly(weight,2) + year, data= Auto_a)

### using smoothing splines
fit2=smooth.spline(weight,mpg,cv=TRUE)
fit2$df
gam2 = gam(mpg ~s(weight,10)+year, data = Auto_a)

### testing the different model with anova
anova(gam1,gam2, test = "F")

summary(gam1)

