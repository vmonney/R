## Chapter 8 exercise 7

library(randomForest)
library(MASS)

train = sample(1:nrow(Boston), nrow(Boston)/2)
boston.test = Boston[-train,"medv"]

n_trees = seq(from=1, to=500, by=10)
p = ncol(Boston)- 1 ## to keep away the response variable
n_predictors = c(p, p/2, sqrt(p))
MSE = data.frame(matrix(NA, nrow = 50, ncol = 3))
colnames(MSE) = c("p","p/2","sqrt(p)")


for(p in 1:length(n_predictors)){
  for(n in 1:length(n_trees)){
    bag.boston = randomForest(medv ~., data = Boston, subset = train, mtry=p, importance = TRUE, ntree=n)
    yhat.bag = predict(bag.boston, newdata = Boston[-train,])
    MSE[n,p] = mean((yhat.bag - boston.test)^2)
  }
}

plot(n_trees, MSE$p, xlab= "Number of Trees", ylab = "Test Classification Error", type = "l", col="orange",ylim = c(0,60))
lines(n_trees, MSE$`p/2`, type ="l", col="blue")
lines(n_trees, MSE$`sqrt(p)`, type ="l", col="green")
legend("topright", legend=c("p","p/2","sqrt(p)"),
       col=c("orange", "blue","green"), lty=1)

