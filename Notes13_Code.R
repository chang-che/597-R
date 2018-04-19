### visualization to get an idea which candidate importance function h is better: plot |g(x)|f(x)/h(x), a better importance function h will uyield |g(x)|f(x)/h(x) to be approximately constant

x <- seq(0,1,length=100)
g <- function(x){
  exp(-x)/(1+x^2)*(x<1)*(x>0)
}
plot(x,g(x)/1,type='l',col='black',ylim=c(0,3.5),lwd=2) ## h0
lines(x,g(x)/exp(-x),col='blue',lwd=2) ## h1
lines(x,g(x)/dcauchy(x),col='red',lwd=2) ## h2
lines(x,g(x)*(1-exp(-1))/exp(-x),col='green',lwd=2) ## h3
lines(x,g(x)*(1+x^2)*pi/4,col='orange',lwd=2) ## h4

### importance
omegahat = se.omega <- rep(NA,5)

g <- function(x){
  exp(-x)/(1+x^2)*(x<1)*(x>0)
}
## note that for this example f(x)=1
### using h0 ###
n <- 10000
x <- runif(n)
gfdvh <- g(x)
omegahat[1] <- mean(gfdvh)
se.omega[1] <- sd(gfdvh)/sqrt(n)

### using h1 ###
x <- rexp(n)
gfdvh <- g(x)/exp(-x)
omegahat[2] <- mean(gfdvh)
se.omega[2] <- sd(gfdvh)/sqrt(n)

### using h2 ###
x <- rcauchy(n)
## to avoid numerical underflow
x[x< -3] <- -3
x[x > 3] <- 3
gfdvh <- g(x)/dcauchy(x)
omegahat[3] <- mean(gfdvh)
se.omega[3] <- sd(gfdvh)/sqrt(n)

### using h3 ###
x <- rexp(2*n)
x[x<0] <- NA
x[x>1] <- NA
x <- na.omit(x)
x <- x[1:n]
gfdvh <- g(x)*(1-exp(-1))/exp(-x)
omegahat[4] <- mean(gfdvh)
se.omega[4] <- sd(gfdvh)/sqrt(n)

### using h4 ###
x <- rcauchy(10*n)
x[x<0] <- NA
x[x>1] <- NA
x <- na.omit(x)
x <- x[1:n]
gfdvh <- g(x)*(1+x^2)*pi/4
omegahat[5] <- mean(gfdvh)
se.omega[5] <- sd(gfdvh)/sqrt(n)

### example on page 4
n <- 10000
x <- matrix(rnorm(2*n),ncol=2)
omegaj <- abs(x[,1]-x[,2]) ## or use apply function
## omegaj  <- apply(x,1,function(y){abs(y[1]-y[2])})
omega <- mean(omegaj)
se.omega <- 1/sqrt(n)sd(omegaj)
