set.seed(123)
### Simulate Chi-sq distribution
n <- 1000
k <- 3 ## df of chi-squared
X <- matrix(rnorm(k*n),ncol=3) ## this is a matrix of 1000 x 3
y <- rowSums(X^2)

## or
y <- apply(X^2,1,sum)

## y will be distributed as Chi-Sq(3)

## now compare y to rchisq
y1 <- rchisq(n,k)

qqplot(y,y1)
abline(0,1)

## simulate mixture of normal
set.seed(123)
S <- sample(c(1:3),10000,prob=c(0.2,0.5,0.3),replace=TRUE)
table(S)

x <- rep(NA,10000)
x[S==1] <- rnorm(length(which(S==1)),0,1)
x[S==2] <- rnorm(length(which(S==2)),-1,1)
x[S==3] <- rnorm(length(which(S==3)),2,1)

### x comes from the mixture distribution
hist(x,breaks=50)

### continuous mixture, negative binomial example 
library(MASS)
n <- 1000
p <- 0.4
r <- 10
mu <- p*r/(1-p)
sigma2 <- p*r/(1-p)^2
theta <- mu^2/(sigma2-mu) ## also equals to r, i.e, theta=r
x1 <- rnegbin(n,mu,theta)

## use the poisson-gamma formula
lambda1 <- rgamma(n,r,scale=p/(1-p))
x2 <- rpois(n,lambda1) #***

#*** is also equivalent to the following
x2 <- rep(NA,n)
for(i in 1:n){
  x2[i] <- rpois(1,lambda1[i])
}

qqplot(x1,x2)
abline(0,1)

### generate multivariate normal using spectral decomposition ###

genmultv.eigen <- function(n,mu,Sigma){
  d <- length(mu)
  ev <- eigen(Sigma,symmetric=TRUE)
  eigen.values <- ev$values
  P <- ev$vectors
  Delta <- diag(sqrt(eigen.values))
  Q <- P%*%Delta%*%t(P)
  Z <- matrix(rnorm(n*d),nrow=n)
  X <- Z%*%Q+matrix(rep(1,n),ncol=1)%*%t(mu)
  return(X)
}

my.mu <- c(0,0,0)
my.Sigma <- matrix(c(2,1,1,1,2,1,1,1,2),nrow=3)
my.X <- genmultv.eigen(10000,my.mu,my.Sigma)

colMeans(my.X)
##


### generate multivariate normal using Cholesky decomposition ###

genmultv.chol <- function(n,mu,Sigma){
  d <- length(mu)
  Q <- chol(Sigma)
  Z <- matrix(rnorm(n*d),nrow=n)
  X <- Z%*%Q+matrix(rep(1,n),ncol=1)%*%t(mu)
  return(X)
}

my.mu <- c(0,0,0)
my.Sigma <- matrix(c(2,1,1,1,2,1,1,1,2),nrow=3)
my.Xchol <- genmultv.chol(10000,my.mu,my.Sigma)

colMeans(my.Xchol)
##
apply(my.Xchol,2,mean)
cov(my.Xchol)
