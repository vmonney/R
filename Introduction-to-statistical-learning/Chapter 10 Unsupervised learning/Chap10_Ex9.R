# chap_10_prob_9.R (Hierarchical clustering on the USArrests

set.seed(0)

# Part (a-b):
# 
hclust.complete = hclust( dist(USArrests), method="complete" )

plot( hclust.complete, xlab="", sub="", cex=0.9 )


#cutree( hclust.complete, h=150 ) # height we cut at 
ct = cutree( hclust.complete, k=3 ) # number of clusters to cut into

# Print which states go into each cluster:
# 
for( k in 1:3 ){
  print(k)
  print( rownames( USArrests )[ ct == k ] )
}


# Part (c-d):
# 
hclust.complete.scale = hclust( dist(scale(USArrests,center=FALSE)), method="complete" )


plot( hclust.complete.scale, xlab="", sub="", cex=0.9 )


ct = cutree( hclust.complete.scale, k=3 ) # number of clusters to cut into

# Print which states go into each cluster in this case:
# 
for( k in 1:3 ){
  print(k)
  print( rownames( USArrests )[ ct == k ] )
}