# chap_10_prob_8.R (Using prcomp on the USArrests data set)

set.seed(0)
pr.out = prcomp(USArrests, scale=TRUE)

# Using the output from prcomp:
# 
pr.var = pr.out$sdev^2
pve_1 = pr.var / sum(pr.var)

# Apply Equation 10.8 directly:
#
USArrests_scaled = scale( USArrests )
denom = sum( apply( USArrests_scaled^2, 2, sum ) )

Phi = pr.out$rotation
USArrests_projected = USArrests_scaled %*% Phi # this is the same as pr.out$x

numer = apply( pr.out$x^2, 2, sum )
pve_2 = numer / denom

print(pve_1)
print(pve_2)
print(pve_1 - pve_2)

