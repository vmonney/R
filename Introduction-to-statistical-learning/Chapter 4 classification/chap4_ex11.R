library(ISLR)
library(MASS)
attach(Auto)
#a Create a binary variable, mpg01,
Auto$mpg01 <- ifelse(mpg > median(mpg),1,0)
Auto$mpg = NULL
Auto$name = NULL
#b investigate the association between mpg01 and the other features. Which of the other features seem most likely to be useful in predicting mpg01?

print(cor(Auto))
pairs(Auto)

Auto$mpg01 = as.factor(mpg01
                       )
#c Split the data into a training set and a test set.

n = dim(Auto)[1]
inds.train = sample(1:n, 3*n/4)
auto.train =  Auto[inds.train,]
auto.test =  Auto[-inds.train,]


#d LDA training test

auto_lda <- lda(mpg01 ~ weight + year, data = auto.train)
auto_lda_pred <- predict(auto_lda, newdata = auto.test)
table(predicted= auto_lda_pred$class, truth=auto.test$mpg01 )
mean(auto_lda_pred$class == auto.test$mpg01)

## error test = 0.9183673
#e QDA
auto_qda <- qda(mpg01 ~ weight + year, data = auto.train)
auto_qda_pred <- predict(auto_qda, newdata = auto.test)
table(predicted= auto_qda_pred$class, truth=auto.test$mpg01 )
mean(auto_qda_pred$class == auto.test$mpg01)
## error test = 0.8875

#f logistic regression

auto_glm <- glm(mpg01 ~ weight + year, data = auto.train, family = binomial)
summary(auto_glm)
p_hat = predict(auto_glm, newdata = auto.test, type = "response")

y_hat = rep(0, length(p_hat))
y_hat[ p_hat > 0.5] = 1
y_hat = as.factor(y_hat)
table(predicted = y_hat, truth= auto.test$mpg01)
mean(y_hat == auto.test$mpg01)
## error test = 0.8954

#g KNN
library(class)
train.auto = cbind(weight, year)[inds.train,]
test.auto = cbind(weight, year)[-inds.train,]
train.mpg01 = Auto$mpg01[inds.train]
test.mpg01 = Auto$mpg01[-inds.train]

set.seed(1)
knn.pred = knn(train.auto, test.auto, train.mpg01, k = 5)
table(knn.pred, test.mpg01)
mean(knn.pred == test.mpg01)
