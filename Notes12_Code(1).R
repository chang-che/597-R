### example on page 3
set.seed(123)
n <- 10000
x <- runif(n)
omega.hat <- mean(exp(-x))

### theoretical value
true.omega <- 1-exp(-1)

### example on page 5
set.seed(123)
n <- 10000
x <- runif(n,2,4)
omega.hat <- (4-2)*mean(exp(-x))

true.omega <- exp(-2)-exp(-4)

### example on page 6
n <- 1000
x <- rnorm(n)
gx <- x < -1.645
mean(gx)

pnorm(-1.645,lower.tail=TRUE) ## true value

### example on page 12
set.seed(123)
n <- 1000
x <- rcauchy(n)
gx <- (x< -2)*dnorm(x)/dcauchy(x)
omegahat <- mean(gx)
## based on the formula on page 7, an estimate of variance of omegahat
var(gx)/n
#[1] 1.205493e-05
### let's compare this with using simple monte carlo integration
n <- 1000
x <- rnorm(n)
gx <- x < -2
mean(gx)
var(gx)/n
#[1] 1.865766e-05 which is bigger than the one from importance sampling
