### simulate exponential using ITM
n <- 10000
lambda1 <- 2
u <- runif(n)
my.x <- -log(u)/lambda1

x <- rexp(n,2)
qqplot(my.x,x)
abline(0,1,col='red',lwd=2,lty=2)

### example 2 on ITM
x <- sqrt(2*runif(1000))

### simulate Normal using ITM
Phi_inv <- function(u){
  t2 <- -2*log(u)
  t <- sqrt(t2)
  x <- t-(2.30753+0.27061*t)/(1+0.99229*t+0.04481*t^2)
  return(x)
}

x <- Phi_inv(runif(10000))
qqnorm(x)

### Ex 1 AR method
n <- 10000
counter <- 0
niter <- 0
y <- rep(NA,n)
while(counter < n){
  u <- runif(1)
  x <- runif(1) ### random variable from g
  niter <- niter + 1
  if(u < x){
    counter <- counter+1
    y[counter] <- x
  }
}

### Ex 2 AR on beta
x <- seq(0,1,length=100)
plot(x,dbeta(x,2,2),type='l')

n <- 10000
counter <- 0
niter <- 0
y <- rep(NA,n)
while(counter < n){
  u <- runif(1)
  x <- runif(1) ### random variable from g
  niter <- niter + 1
  if(u < x*(1-x)){
    counter <- counter+1
    y[counter] <- x
  }
}

plot(density(y))
