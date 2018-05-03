#1.
#(a)
set.seed(123)
f <- function(x){
  return(exp(-2*abs(x-5)))
}
integrate(f,0,10)

n <- 1000
x <- runif(n, min = 0, max = 10)
exp.vec <- exp(-2*abs(x-5))
var(10*exp.vec)
omega.hat <- mean(10*exp.vec)
omega.hat
#(b)
y <- rnorm(n, mean = 5, sd = 1)
gx <- (y>0&y<10)*exp(-2*abs(y-5))/dnorm(y, mean = 5, sd = 1)
omega.star <- mean(gx)
var(gx)
omega.star

#2.
dat <- read.delim('http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/RegData.txt',header=T,sep='\t')
B <-  1000
x <- dat$x
y <- dat$y
stat <- cor.test(x, y, method = 'spearman')$estimate
stat
xy <- c(x,y)
cor.vec <- rep(NA, B)
for (b in 1:B){
  x_index <- sample(1:30, 15, replace = F)
  cor.vec[b] <- cor.test(xy[x_index], xy[-x_index], method = 'spearman')$estimate
}
(sum(abs(cor.vec)>abs(stat)) + 1)/(B+1)
cor.test(x, y, method = 'spearman')$p.value
#p-value > 0.05
# Do not have enough evidence to reject the null hypothesis that variable y doesn't correlate with x.

#3.
n.vec <- c(10, 20, 30, 40, 50)
set.seed(123)

B <- 1000
pw.vec <-pw.vec.true <-  rep(NA, 5)

for (j in 1:length(n.vec)){
  n <- n.vec[j]
  p_value.vec <- rep(NA, B)
  for(i in 1:B){
    x.vec <- rnorm(n)
    y.vec <- rnorm(n, mean = 1, sd = 1)
    p_value.vec[i] <- t.test(x.vec, y.vec, var.equal = T)$p.value
  }
  pw.vec[j] <- mean(p_value.vec<0.1)
}
pw.vec
plot(n.vec, pw.vec, xlab = 'sample size', ylab = 'estimated eperical power', type = 'l')
