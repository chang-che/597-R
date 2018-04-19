par(mfrow = c(1,4))

plot(c(1,1),c(1,5))
plot(c(1,1),c(1,1))

par(mfrow = c(2,4))
plot(c(1,1),c(1,5))

plot(c(1,1),c(1,1))
par(mfrow = c(4,4))
plot(c(1,1),c(1,10))
plot(c(1,1),c(1,10))
par(mfcol = c(2,4))
plot(c(1,1),c(1,10))
plot(c(1,1),c(1,10))

require(graphics)

y <- rt(200, df = 5)
qqnorm(y); qqline(y, col = 2)
qqplot(y, rt(300, df = 5))
qqnorm(precip, ylab = "Precipitation [in/yr] for 70 US cities")
## "QQ-Chisquare" : --------------------------
y <- rchisq(500, df = 3)
## Q-Q plot for Chi^2 data against true theoretical distribution:
qqplot(qchisq(ppoints(500), df = 3), y,
       main = expression("Q-Q plot for" ~~ {chi^2}[nu == 3]))
qqline(y, distribution = function(p) qchisq(p, df = 3),
       prob = c(0.1, 0.6), col = 2)
mtext("qqline(*, dist = qchisq(., df=3), prob = c(0.1, 0.6))")

## Fibonacci sequence
Fibona <- function(n){
  if (n == 1){
    return(c(1))
  }
  if (n == 2){
    return(c(1,1))
  }
  if (n > 2){
    return(c(Fibona(n-1),Fibona(n-1)[n-2]+Fibona(n-1)[n-1]))
  }
}

Fibona(5)
