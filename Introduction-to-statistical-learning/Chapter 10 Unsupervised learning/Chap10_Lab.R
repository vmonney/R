### Chapter 10 Lab


###############  PRINCIPAL COMPONENT ANALYSIS #####################################################3

## looking at the data

states = row.names(USArrests)
states

names(USArrests)

apply(USArrests, 2, mean)
apply(USArrests, 2, var)

### performing PCA

pr.out = prcomp(USArrests, scale = TRUE) ## scale to have sd equal to 1

names(pr.out) ## center = mean / scale = standard deviation
pr.out$center
pr.out$scale
pr.out$rotation

dim(pr.out$x)

## ploting
biplot(pr.out, scale = 0)

## change the orientation

pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0)

pr.out$sdev
pr.var = pr.out$sdev^2
pr.var
# proportion of variance explained
pve = pr.var/sum(pr.var)
pve

## plotting
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim= c(0,1), type ="b")
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explaines", ylim=c(0,1), type = "b")


########################## K-Means CLUSTERING #########################################################

set.seed(2)
x = matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] = x[1:25, 1] + 3
x[1:25, 2] = x[1:25, 2] - 4

km.out = kmeans(x, 2, nstart=20)
km.out$cluster

plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)

### testing with 3 clusters

set.seed(4)
km.out = kmeans(x, 3, nstart = 20)
km.out

plot(x, col=(km.out$cluster+1), main= "K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)

## diff between nstart = 1 and nstart = 20

set.seed(3)
km.out = kmeans(x, 3, nstart = 1)
km.out$tot.withinss
km.out = kmeans(x, 3, nstart = 20)
km.out$tot.withinss


########### Hierarchical Clustering ####################################################

hc.complete = hclust(dist(x), method="complete")
hc.average = hclust(dist(x), method="average")
hc.single = hclust(dist(x), method="single")

### ploting

par(mfrow=c(1,3))
plot(hc.complete, main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

cutree(hc.single, 4)

## to scale variables

xsc = scale(x, center = FALSE, scale = TRUE)
plot(hclust(dist(xsc), method = "complete"), main="Hierarchical Clustering with Scaled Observations")

## Correlation-based distance

x = matrix(rnorm(30*3), ncol = 3)
dd = as.dist(1-cor(t(x)))
plot(hclust(dd, method = "complete"), main = "Complete Linkage with Correlation-Based Distance", xlab="", sub="")
