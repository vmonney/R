# Chapter 6 LAB3 PCR and PLS Regression

## Principal component regression

if( ! require("pls") ){ install.packages("pls") }
library(pls)
Hitters = na.omit(Hitters)

pcr_fit = pcr(Salary ~., data = Hitters, scale = TRUE, validation = "CV") # scale is to standardize the predictors
summary(pcr_fit)

validationplot(pcr_fit, val.type = "MSEP")

x = model.matrix(Salary ~.,Hitters)[,-1]
y = Hsitters$Salary
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y_test = y[test]

pcr_fit = pcr(Salary ~., data = Hitters, subset = train,  scale = TRUE, validation = "CV")
validationplot(pcr_fit, val.type = "MSEP")
summary(pcr_fit)


pcr_pred = predict( pcr_fit , x[test,], ncomp =7)
mean((pcr_pred - y_test)^2)

## fitting on the full data set
pcr_fit =pcr(y ~ x, scale =TRUE , ncomp =7)
summary(pcr_fit )

################# Partial Least Squares #####################

set.seed(1)
pls_fit = plsr(Salary ~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls_fit)
validationplot(pls_fit, val.type = "MSEP")
pls_pred = predict(pls_fit, x[test,], ncomp = 2)
mean((pls_pred - y_test)^2)

pls_fit = plsr(Salary ~., data = Hitters, scale = TRUE, validation = "CV", ncomp = 2)
summary(pls_fit)