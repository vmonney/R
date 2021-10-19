### chapter 10 Exercise 7 a correlation-based distance

set.seed(0)
# Scale each observation (not the features):
# 
USA_scaled = t(scale(t(USArrests)))

# The correlation of each sample with the other samples:
# 
Rij = cor(t(USA_scaled)) # -1 <= Rij <= +1 
OneMinusRij = 1 - Rij # 0 <= 1-Rij <= +2 
X = OneMinusRij[lower.tri(OneMinusRij)]

D = as.matrix( dist( USA_scaled )^2 )
Y = D[lower.tri(D)]

plot( X, Y )

summary( X/Y )