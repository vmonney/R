if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("leaps") ){ install.packages("leaps") }
if( ! require("glmnet") ){ install.packages("glmnet") }
if( ! require("pls") ){ install.packages("pls") }

# the sample size and the number of features
p = 20
n = 1000

# Create the true value of beta (and zero out half of the entries):

beta_truth = rnorm(p+1) # add one for the constant beta_0 
zero_locations = c(2,3,4,7,8,11,12,15,17,20)
beta_truth[zero_locations] = 0

# For debugging lets check that we can recover our coefficients: 
#beta_truth = rep(0,p+1); beta_truth[1] = 1.5; beta_truth[10] = 3.5; beta_truth[15] = -3.4
print( "True values for beta (beta_0-beta_20):" )
print( beta_truth )

# Generate some input features and an output response:

X = c(rep(1,n), rnorm(n*p)) # make leading column of ones 
X = matrix(X, nrow=n, ncol=(p+1), byrow=FALSE )
Y = X %*% beta_truth + rnorm(n) ## Y = X?? + epsilon

# Create a dataframe with this data:
DF = data.frame(Y,X[,-1] ) # drop the column of ones 

#(b) Split your data set into a training set containing 100 observations and a test set containing 900 observations.

train = sample(1:n, 100)
test = (1:n)[-train]

## (c) Perform best subset selection on the training set, and plot the training set MSE associated with the best model of each size.
library(ISLR)
regfit_full = regsubsets(Y ~ ., data = DF[train,], nvmax = 20)
reg_summary = summary(regfit_full)

train.mat = model.matrix(Y ~., DF[train,])

MSE = rep(NA,20)
for(i in 1:20){
  coefi = coef(regfit_full, id = i)
  pred = train.mat[,names(coefi)]%*%coefi
  MSE[i]= mean((DF$Y[train]-pred)^2)
}

plot(MSE, xlab = "number of variables", col = "red", type= "o")
points( MSE_test, xlab='number of predictors', ylab='testing MSE', type='o', col='green' )
# (d) Plot the test set MSE associated with the best model of each size.

test.mat = model.matrix(Y ~., DF[test,])
MSE_test = rep(NA,20)
for(i in 1:20){
  coefi = coef(regfit_full, id = i)
  pred = test.mat[,names(coefi)]%*%coefi
  MSE_test[i]= mean((DF$Y[test]-pred)^2)
}
plot(MSE_test, xlab = "number of variables")


grid()
legend( 11, 9.25, c('Training MSE','Testing MSE'), col=c('red','green'), lty=c(1,1) )

## (e) For which model size does the test set MSE take on its minimum value? Comment on your results. 
model_size = which.min(MSE_test)
print( coef( regfit_full, id=model_size ) )


# (f) How does the model at which the test set MSE is minimized compare to the true model used to generate the data? 
#Comment on the coefficient values.
regfit.best = regsubsets(Y ~., data = DF, nvmax = 20)
beta_est = coef(regfit.best, model_size)
beta_truth
## it is very similar.
# Part (g):
#
nms = colnames(DF)
nms[1] = "(Intercept)" 
names(beta_truth) = nms

norm.beta.diff = rep(NA,20)
for( ii in 1:20 ){
  coefi = coef( regfit.best, id=ii )
  norm.beta.diff[ii] = sqrt(sum((beta_truth[names(coefi)] - coefi )^2 ) ) 
}
plot( 1:20, norm.beta.diff, xlab='number of predictors', ylab='||beta_truth - beta^r||', type='o', col='green' )
