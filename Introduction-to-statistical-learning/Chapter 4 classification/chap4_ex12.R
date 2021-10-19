##a
Power <- function (){
  print(2^3)
}

Power()

##b
Power2 <- function(x,a){
  print(x^a)
}
Power2(3,8)
##c
Power2(10,3)
Power2(8,17)
Power2(131,3)
##d
Power3 <- function(x,a){
  result <- x^a
  return(result)
}
##e
x = c(1:10)
y = Power3(x,2)
plot(x,y, xlab = "x", ylab = "x^2", main = "function f(x) = x^2", log="xy")

##f
PlotPower <- function(x,a){
  plot = plot(x,x^a,xlab = "x", ylab = "x^2", main = "function f(x) = x^2")
  return(plot)
}
PlotPower(1:10,3)