## E-step
Estep <- function(mu0,mu1,sigma0,sigma1,pi,x){
  taui <- pi*dnorm(x,mu1,sigma1)/(pi*dnorm(x,mu1,sigma1)+(1-pi)*dnorm(x,mu0,sigma0))
  loglik <- sum(log((1-pi)*dnorm(x,mu0,sigma0)+pi*dnorm(x,mu1,sigma1)))
  return(list(taui=taui,loglik=loglik))
}

## M-step
Mstep <- function(taui,x){
  mu0 <- sum((1-taui)*x)/sum(1-taui)
  mu1 <- sum(taui*x)/sum(taui)
  sigma0 <- sqrt(sum((1-taui)*(x-mu0)^2)/sum(1-taui))
  sigma1 <- sqrt(sum(taui*(x-mu1)^2)/sum(taui))
  pi <- sum(taui)/length(x)
  return(list(mu0=mu0,mu1=mu1,sigma0=sigma0,sigma1=sigma1,pi=pi))
}

### now simulate from mixture of 2 normal
set.seed(123)
n <- 1000
z <- sample(c(0,1),n,prob=c(0.25,0.75),replace=TRUE)
x1 <- rep(NA,n)
x1[z==0] <- rnorm(length(which(z==0)),1,0.5)
x1[z==1] <- rnorm(length(which(z==1)),1.5,0.2)

### initialization
mu0 <- 0
mu1 <- 1
sigma0 = sigma1 <- 1
pi <- 0.5

cur.Mstep <- list(mu0=mu0,mu1=mu1,sigma0=sigma0,sigma1=sigma1,pi=pi)
log.lik.iter <- c(-1,0)
epsilon <- 1e-6
i <- 1
while(abs(log.lik.iter[i+1]-log.lik.iter[i])>epsilon){
  cur.Estep <- Estep(mu0=cur.Mstep$mu0,mu1=cur.Mstep$mu1,sigma0=cur.Mstep$sigma0,sigma1=cur.Mstep$sigma1,pi=cur.Mstep$pi,x=x1)
  log.lik.iter <- c(log.lik.iter,cur.Estep$loglik)
  cur.Mstep <- Mstep(taui=cur.Estep$taui,x=x1)
  i <- i+1
}

plot(log.lik.iter)
