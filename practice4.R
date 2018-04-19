############
###########AR method
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
qqplot(rbeta(1000,2,2), y)
abline(0,1, col = 'red')
niter ### approx 60000
########################
###### the above sup(f(x)/g(x)) = 3/2 while it didn't use 3/2, it use 6 instead
n <- 10000
counter <- 0
niter <- 0
y <- rep(NA,n)
while(counter < n){
  u <- runif(1)
  x <- runif(1) ### random variable from g
  niter <- niter + 1
  if(u < x*(1-x)*4){
    counter <- counter+1
    y[counter] <- x
  }
}

plot(density(y))
qqplot(rbeta(1000,2,2), y)
abline(0,1, col = 'red')
niter # aprox 15000
## iterations become much less, decreased to its 1/4

exp(0.5)*(1+0.25)^(-2.5)
