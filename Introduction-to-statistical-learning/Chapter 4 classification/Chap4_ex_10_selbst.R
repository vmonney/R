
#Chapter 4 : exercise 10 
if(!require("ISLR")){install.packages("ISLR")}
if(!require("MASS")){install.packages("MASS")}
if(!require("class")){install.packages("class")}

set.seed(0)


library(ISLR)
attach(Weekly)
## a : do they appear any pattern?
dim(Weekly)
summary(Weekly)
plot(Weekly$Today)
Weekly$num_direction <- 1
Weekly[Weekly$Direction=="Down","num_direction"] <- -1
cor(Weekly[,-9])
## the only correlation with direction appears to be the Today variable with 0.72

#b logistic regression to predict Direction as a function of 5 lag variables + volume:

log_reg <- glm(Direction ~ Lag1 + Lag2 +Lag3 + Lag4 + Lag5 + Volume, family = binomial, data = Weekly)
summary(log_reg)
contrasts(Weekly$Direction)
## only Lag2 seems to be a litlle significant

#c confusion matrix

log_probs <- predict(log_reg, type = "response")
log_pred <- rep("Down", 1089)
log_pred[log_probs>0.5] = "Up"
table(log_pred, Direction)
mean(log_pred == Direction)

## 0.56 percent of correct prediction. diagonal values = correct predictions, bottom left = type 1 falsly rejected H0, top-right = type 2 error : non-reject H0 but truly rejected.

## d : test on 2009-2010 with only lag2 and logistic regression

train <- (Year<2009)
Weekly_test <- Weekly[!train,]
Direction_test <- Direction[!train]

### logistic regression
log_reg_d <- glm(Direction ~ Lag2, family = binomial, data = Weekly, subset = train)
log_reg_d_probs <- predict(log_reg_d, Weekly_test, type = "response")

log_pred_test <- rep("Down", 104)
log_pred_test[log_reg_d_probs>0.5] = "Up"

table(log_pred_test,Direction_test)
mean(log_pred_test == Direction_test)

##e LDA
library(MASS)

lda.fit <- lda(Direction ~ Lag2, data = Weekly, subset = train)
lda.pred <- predict(lda.fit, Weekly_test)
table(lda.pred$class, Direction_test)
mean(lda.pred$class == Direction_test)


##f QDA
qda.fit <- qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.pred <- predict(qda.fit, Weekly_test)
table(qda.pred$class, Direction_test)
mean(qda.pred$class == Direction_test)

#g KNN with K = 1

library(class)

train.Lag2 <- data.frame(Lag2[train])
test.Lag2 <-  data.frame(Lag2[!train])
train.Direction <- Direction[train]

knn.pred <- knn(train = train.Lag2,test = test.Lag2,cl = train.Direction, k=3)
table(knn.pred, Direction_test)
mean(knn.pred == Direction_test)

#h log reg and LDA with 0.625% 
