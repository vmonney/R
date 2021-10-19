# chap_10_prob_10.R (PCA and K-means clustering on some simulated data)

set.seed(0)

# Part (a) generate data:
# 
K = 3  # the number of classes
n = 20 # the number of samples per class
p = 50 # the number of variables 

# Create data for class 1: 
X_1 = matrix( rnorm(n*p), nrow=n, ncol=p )
for( row in 1:n ){
  X_1[row,] = X_1[row,] + rep( 1, p ) 
}

# Create data for class 2: 
X_2 = matrix( rnorm(n*p), nrow=n, ncol=p )
for( row in 1:n ){
  X_2[row,] = X_2[row,] + rep( -1, p ) 
}

# Create data for class 3: 
X_3 = matrix( rnorm(n*p), nrow=n, ncol=p )
for( row in 1:n ){
  X_3[row,] = X_3[row,] + c( rep( +1, p/2 ), rep( -1, p/2 ) ) 
}

X = rbind( X_1, X_2, X_3 )
labels = c( rep(1,n), rep(2,n), rep(3,n) ) # the "true" labels of the points 

pr.out = prcomp( X, scale=TRUE ) 

plot( pr.out$x[,1], pr.out$x[,2], col=labels, pch=19 )
grid()


# Part (c):
#
kmean.out = kmeans( X, centers=3, nstart=50 )

table( kmean.out$cluster, labels )

# Part (d):
#
kmean.out = kmeans( X, centers=2, nstart=50 )

# Part (e):
#
kmean.out = kmeans( X, centers=4, nstart=50 )

# Part (f):
#
kmean.out = kmeans( pr.out$x[,c(1,2)], centers=3, nstart=50 )

# Part (g):
#
Xs = scale( X ) 
kmean.out = kmeans( Xs, centers=3, nstart=50 )