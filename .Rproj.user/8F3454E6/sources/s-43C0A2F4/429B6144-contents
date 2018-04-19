dat <- read.delim('http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/Qn1Data.txt',header=T,sep='\t')
dat <- as.data.frame(dat)
names(dat)[1] <- 'response'
names(dat)
library(FrF2)
PlanA = FrF2(nruns = 64, nfactor = 6)
x = desnum(PlanA)
class(x)
x.logical <- (x ==1)
B <- rep(NA, 64)

for (i in 1:64){
  a1 <- as.vector(c(T, x.logical[i,]))
  data1 <- dat[,a1]
  data1 <- as.data.frame(data1)
  colnames(data1)[1] <- 'response'
  fit <- glm(response ~ ., data = data1, family = 'binomial')
  B[i] <- BIC(fit)
}

ind <- which(B == min(B))
x.logical[ind, ]# This should give the name of selected variales
#2.
x <- runif(1000)
r.vec <- sqrt(-log(1-x))

#3.
Phi_inv <- function(size){
  u = runif(size)
  t2 <- -2*log(u)
  t <- sqrt(t2)
  x <- t-(2.30753+0.27061*t)/(1+0.99229*t+0.04481*t^2)
  return(x)
}

rand_t <- function(size,df){
  numerator <- Phi_inv(size)
  z <- matrix(Phi_inv(size*df), ncol = df)
  z <- z^2
  w <- apply(z, 1, sum)
  ans <- numerator/sqrt(w/df)
  return(ans)
}

#
S <- sample(c(1:3),100,prob=c(0.3,0.35,0.35),replace=TRUE)

x <- rep(NA,100)
x[S==1] <- rand_t(length(which(S==1)),3)
x[S==2] <- rand_t(length(which(S==2)),5)
x[S==3] <- rand_t(length(which(S==3)),7)
