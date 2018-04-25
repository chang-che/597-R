library(bootstrap)
data(law)
str(law)
r.obs <- cor(law$LSAT,law$GPA) ### pearson correlation coefficient

B <- 100
r.boot <- rep(NA,B)
for(b in 1:B){
  ind <- sample(1:dim(law)[1],dim(law)[1],replace=TRUE)
  law.boot <- law[ind,]
  r.boot[b] <- cor(law.boot$LSAT,law.boot$GPA)
}

sd(r.boot)
bias <- mean(r.boot)-r.obs
  
library(boot)
r.boot.func <- function(x,i){
  cor(x[i,1],x[i,2])
}

boot1 <- boot(data=law,statistic=r.boot.func,R=100)

## bootstrap t

boot.t.ci <- function(x, B=500, R=100, level=0.95,statistic){
  x <- as.matrix(x); n <- nrow(x)
  stat <- numeric(B); se <- numeric(B)
  boot.se <- function(x, R, f) {
    x <- as.matrix(x); m <- nrow(x)
    th <- replicate(R, expr = {
      i <- sample(1:m, size = m, replace = TRUE)
      f(x[i, ])
    })
    return(sd(th))
  }
  for (b in 1:B) {
    j <- sample(1:n, size = n, replace = TRUE)
    y <- x[j, ]
    stat[b] <- statistic(y)
    se[b] <- boot.se(y, R = R, f = statistic)
  }
  stat0 <- statistic(x)
  t.stats <- (stat - stat0) / se
  se0 <- sd(stat)
  alpha <- 1 - level
  Qt <- quantile(t.stats, c(alpha/2, 1-alpha/2), type = 1)
  names(Qt) <- rev(names(Qt))
  CI <- rev(stat0 - Qt * se0)
}


dat <- cbind(law$LSAT, law$GPA)
stat <- function(dat) {
  cor(dat[,1], dat[,2])
}
ci <- boot.t.ci(dat, statistic = stat, B=2000, R=200)
print(ci)

