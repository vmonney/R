## Part (c) generate data:
## 
n = 1000 # the number of genes
m = 100 # the number of tissue samples

##n = 20
##m = 5

mu_A = 0.1 ## the machines are mostly the same but a bit different
sigma_A = 1.0

mu_B = -0.3
sigma_B = 4.0

mu_C = 0.0 # the control and the treatment means are similar
mu_T = 0.25

X = matrix(0, nrow=n, ncol=m)
prob_of_machine_A = seq(1, 1.e-6, length.out=m)
machine = c()
treatment = c()
for( jj in 1:m ){
  ## What machine did we use.  We slowly change from machine A to machine B.
  ##
  machine_used = sample(c('A', 'B'), size=1, prob=c(prob_of_machine_A[jj], 1-prob_of_machine_A[jj]))
  machine = c(machine, machine_used)
  
  ## Is this a control or a treatment sample: 
  ##
  type = sample(c('C', 'T'), size=1, prob=c(0.5, 0.5))
  treatment = c(treatment, type)
  
  if( machine_used=='A' ){
    if( type=='C' ){
      x = rnorm(n, mean=(mu_A+mu_C), sd=sigma_A)
    }else{
      x = rnorm(n, mean=(mu_A+mu_T), sd=sigma_A)
    }
  }else{
    if( type=='C' ){
      x = rnorm(n, mean=(mu_B+mu_C), sd=sigma_B)
    }else{
      x = rnorm(n, mean=(mu_B+mu_T), sd=sigma_B)
    }
  }
  X[, jj] = x
}

pr.out = prcomp(X, scale=TRUE)

##print(summary(pr.out))

## Lets print the fraction of variance explained for the first 10 PC:
##
print(pr.out$sdev[1:10]^2/sum(pr.out$sdev^2))

## Performe the suggested transformation: 
## 
X_transformed = X - pr.out$x[,1] %*% t(pr.out$rotation[, 1])

## Run the T-test:
##
print(t.test(X_transformed[, treatment=='C'], X_transformed[, treatment=='T']))

## Split into two groups, normalize, and recombine:
## 
machine_A = machine=='A'
X_A = X[, machine_A]

machine_B = machine=='B'
X_B = X[, machine_B]

print(sprintf('mu_A= %f; mean(X_A)= %f; mu_B= %f; mean(X_B)= %f', mu_A, mean(X_A), mu_B, mean(X_B)))

X_2 = cbind(X_A - mean(X_A), X_B - mean(X_B))

pr.out = prcomp(X_2, scale=TRUE)

##print(summary(pr.out))

## Lets print the fraction of variance explained for the first 10 PC:
##
print(pr.out$sdev[1:10]^2/sum(pr.out$sdev^2))

## Run the T-test:
##
print(t.test(X_2[, treatment=='C'], X_2[, treatment=='T']))