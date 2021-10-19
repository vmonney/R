p1 = seq( 0+1e-6, 1-1e-6, length.out=100 )
p2 = 1 - p1

# The missclassification error-rate:
#
E = 1 - apply( rbind( p1, p2 ), 2, max )

# The Gini index:
#
G = p1 * (1-p1) + p2 * (1-p2) 

# The cross-entropy:
#
D = - ( p1 * log( p1 ) + p2 * log( p2 ) )

plot( p1, E, type='l', col='black', xlab='p_1', ylab='value of error metric', ylim=c(min(c(E,G,D)),max(E,G,D)) )
lines( p1, G, col='blue' )
lines( p1, D, col='green' )
legend( 0.2, 0.1, c('Classification error','Gini index','Cross entropy'), col=c('black','blue','green'), lty=c(1,1) )
grid()
