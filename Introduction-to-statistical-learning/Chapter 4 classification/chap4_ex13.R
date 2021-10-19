if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("MASS") ){ install.packages("MASS") }
if( ! require("class") ){ install.packages("class") }


library(MASS)
attach(Boston)

#a Create a binary variable, mpg01,
Boston$crim01 <- ifelse(crim > median(crim),1,0)
Boston$crim = NULL

#b investigate the association between mpg01 and the other features. Which of the other features seem most likely to be useful in predicting mpg01?

Boston_cor <- cor(Boston)
print(sort(Boston_cor[,"crim01"]))
pairs(Boston)

#c Split the data into a training set and a test set.

n = dim(Boston)[1]
inds.train = sample(1:n, 3*n/4)
boston.train =  Boston[inds.train,]
boston.test =  Boston[-inds.train,]
## finding predictors

log_reg_all <- glm(crim01 ~ ., data = boston.train, family = binomial)
summary(log_reg_all)

## LDA
boston_lda <- lda(crim01 ~ nox + rad + dis + indus + tax +ptratio + black, data = boston.train)
boston_lda_pred <- predict(boston_lda, newdata = boston.test)
table(predicted= boston_lda_pred$class, truth=boston.test$crim01 )
mean(boston_lda_pred$class == boston.test$crim01)

## error test = 0.9183673
# QDA
boston_qda <- qda(crim01 ~ nox + rad + dis + indus + tax +ptratio + black, data = boston.train)
boston_qda_pred <- predict(boston_qda, newdata = boston.test)
table(predicted= boston_qda_pred$class, truth=boston.test$crim01 )
mean(boston_qda_pred$class == boston.test$crim01)
## error test = 0.8875

#f logistic regression

boston_glm <- glm(crim01 ~ nox + rad + dis + indus + tax +ptratio + black, data = boston.train, family = binomial)
summary(boston_glm)
p_hat = predict(boston_glm, newdata = boston.test, type = "response")

y_hat = rep(0, length(p_hat))
y_hat[ p_hat > 0.5] = 1
y_hat = as.factor(y_hat)
table(predicted = y_hat, truth= boston.test$crim01)
mean(y_hat == boston.test$crim01)
## error test = 0.8954

#g KNN
library(class)
train.boston = cbind(nox, rad)[inds.train,]
test.boston = cbind(nox, rad)[-inds.train,]
train.crim01 = Boston$crim01[inds.train]
test.crim01 = Boston$crim01[-inds.train]

set.seed(1)
knn.pred = knn(train.boston, test.boston, train.crim01, k = 1)
table(knn.pred, test.crim01)
mean(knn.pred == test.crim01)