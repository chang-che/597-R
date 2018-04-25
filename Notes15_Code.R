### t-test ##
set.seed(123)
x <- rnorm(20)
y <- rnorm(15,mean=-0.5)
t.test(x,y,alternative='greater')

t0 <- t.test(x,y,alternative='greater')$stat
B <- 9999
tperm <- rep(NA,B)
z <- c(x,y)
grp <- rep(c(1,2),c(length(x),length(y)))
for(i in 1:B){
  shuffle.id <- sample(grp,size=length(grp),replace=FALSE)
  tperm[i] <- t.test(z[shuffle.id==1],z[shuffle.id==2])$stat
}

emp.pv <- (length(which(tperm>=t0))+1)/(B+1)

### KS test ###
set.seed(123)
x <- rnorm(20)
y <- rnorm(15,mean=-0.5)
ks.test(x,y,exact=FALSE) ## kolmogorov smirnov test

d0 <- ks.test(x,y,exact=FALSE)$stat
B <- 19999
dperm <- rep(NA,B)
z <- c(x,y)
grp <- rep(c(1,2),c(length(x),length(y)))

for(i in 1:B){
  shuffle.id <- sample(grp,size=length(grp),replace=FALSE)
  dperm[i] <- ks.test(z[shuffle.id==1],z[shuffle.id==2],exact=FALSE)$stat
}
emp.pv1 <- (length(which(dperm>=d0))+1)/(B+1)
