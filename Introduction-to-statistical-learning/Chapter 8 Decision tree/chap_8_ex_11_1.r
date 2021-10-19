## Chapter 8 exercise 11
if( ! require("MASS") ){ install.packages("MASS") }
if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("gbm") ){ install.packages("gbm") }
if( ! require("glmnet") ){ install.packages("glmnet") }
if( ! require("randomForest") ){ install.packages("randomForest") }
if( ! require("class") ){ install.packages("class") }

summary(Caravan)
## a

n = nrow(Caravan)
p = ncol(Caravan) -1 # one column is the response we are trying to model i.e. "Purchase"

train = 1:1000
test = 1001:n

Caravan$Purchase = ifelse(Caravan$Purchase == "Yes",1,0) ## Transform the response "Purchase" to be in [0,1] as required by gbm

Caravan$PVRAAUT = NULL # Some variables seem to be very noninformative (have zero variance as reported by gbm)
Caravan$AVRAAUT = NULL

##b the contribution car policies and the purchasing power class seem to be the most important predictors
boost.caravan = gbm(Purchase ~., data=Caravan[train,], distribution = "bernoulli", n.trees = 1000, shrinkage = 0.01)
summary(boost.caravan)

## c Prediction

yhat.boost = predict(boost.caravan, newdata = Caravan[test,], n.trees=1000, type = "response")

yhat.purchase = ifelse(yhat.boost>0.2, 1, 0)

table(yhat.purchase, Caravan$Purchase[test])
35/(118+35)

## logistic regression

# Train a logistic regression:
#
lr_model = glm( Purchase ~ ., data=Caravan[train,], family="binomial" )
y_hat = predict( lr_model, newdata=Caravan[test,] , type = "response")

will.purchase = ifelse(y_hat>0.2, 1, 0)

# Create a confusion matrix:
# 
table( will.purchase, Caravan[test,]$Purchase )
58 / (350 + 58)

## knn
standardized.X=scale(Caravan[, -86])
train.X= standardized.X[-test ,]
test.X= standardized.X[test ,] 
train.Y= Caravan$Purchase[- test]
test.Y= Caravan$Purchase[test]

# choosing the best k 

k = c(1:84)
MSE = rep(NA,84)
for(i in 1:83){
  knn.pred = knn(train.X,test.X, train.Y,k=i)
  MSE[i] = mean(test.Y!= knn.pred)
}

plot(k,MSE)
which.min(MSE)

knn.pred.best = knn(train.X, test.X, train.Y, k=5)
mean(test.Y!= knn.pred.best)
