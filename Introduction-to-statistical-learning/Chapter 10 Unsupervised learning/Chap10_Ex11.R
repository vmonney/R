# chap_10_prob_11.R (Clustering gene expression data)


set.seed(0)
setwd("~/data science/R/Introduction to statistical learning/Chapter 10 Unsupervised learning")
# Part (a):
# 
DF = read.csv("Ch10Ex11.csv",header=FALSE)
DF = t(DF) # want each row to represent a sample ... should have n=40 samples/rows

# Part (b):
#
D = dist(DF)                  # "n x n matrix of Euclidean distance dissimilarities"
D = as.dist( 1 - cor(t(DF)) ) # cor computes the correlation of *columns* so we need to take the transpose of DF
hclust.cor = hclust( D, method="complete" )
#hclust.cor = hclust( D, method="average" )
#hclust.cor = hclust( D, method="single" )

# How well does our clustering predict health vs. diseased:
# 
print( table( predicted=cutree( hclust.cor, k=2 ), truth=c( rep(0,20), rep(1,20) ) ) )


plot( hclust.cor, xlab="", sub="", cex=0.9 )


# Part (c):
# 
# Compute the unpaired t-test between the means of the gene response in each cluster: 
#
predicted=cutree( hclust.cor, k=2 ) 

n1 = apply( DF[ predicted==1, ], 2, length ) # the number of samples (number of patients in each cluster)
n2 = apply( DF[ predicted==2, ], 2, length )

m1 = apply( DF[ predicted==1, ], 2, mean ) # the means across the 1000 genes in each cluster
m2 = apply( DF[ predicted==2, ], 2, mean )

v1 = apply( DF[ predicted==1, ], 2, var ) # the variances across the 1000 genes in each cluster
v2 = apply( DF[ predicted==2, ], 2, var )

pooled_variance = sqrt( v1 / n1 + v2 / n2 )

t_value = ( m1 - m2 ) / pooled_variance 

plot( t_value, xlab="gene index", ylab="unpaired t-value" )

