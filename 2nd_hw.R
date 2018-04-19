#1 ---------------------------------------
#(a)
sample(list(c(1,0,0,0),c(0,1,0,0),c(0,0,1,0),c(0,0,0,1)),size = 1,prob = c(0.1,0.2,0.4,0.3))
### revised after
x = sample(list(c(1,0,0,0),c(0,1,0,0),c(0,0,1,0),c(0,0,0,1)),size = 1000,replace = T, prob = c(0.1,0.2,0.4,0.3))
Reduce('+',x)
# or This doesn't seem to work.
m4 = matrix(x,ncol = 4,byrow = T)
colSums(m4)
#(b)
generator = function(proba = c(1/4,1/4,1/4,1/4)) {
  x = rbinom(1,1,prob = proba[1]+proba[2])
  y = rbinom(1,1,prob = proba[1]/(proba[1]+proba[2]))
  z = rbinom(1,1,prob = proba[3]/(proba[3]+proba[4]))
  if (x==1) { 
    if (y==1) {
      return(c(1,0,0,0))
    } else {
      return(c(0,1,0,0))}
  } else {
    if (z ==1) {
      return(c(0,0,1,0))
    } else {
      return(c(0,0,0,1))}
  }
}
generator(proba=c(0.1,0.2,0.4,0.3))


#2--------------------------------------------

x = rexp(100,2)
n = length(x)
plot(sort(x),(1:n)/n,type = 's',ylim = c(0,1))

#3-----------------------------------------------
#a
log_return = read.table("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/m_logret_10stocks.txt", header=T)
attach(log_return)
t.test(AmerExp)
#b
wilcox.test(AmerExp)
#c
var.test(AmerExp,Pfizer) #p-value =0.04394, reject the null hypothesis and claim the variances are not equal.
# Use welch
t.test(AmerExp,Pfizer) # p-value is 0.35, cannot reject the null hypothesis which is diff between means is 0.
#d
wilcox.test(AmerExp,Pfizer) # fail to reject the Null hypothesis which is location shift is 0.
#e
var.test(AmerExp,Pfizer) # p-value=0.04394 we reject the null hypothesis which is two variances are equal. 

#4---------------------------------------------
my.t.test = function(x, y = NULL,
                     my.alternative = c("two.sided", "less", "greater"),
                     my.mu = 0) {
  if (!is.null(y)) {
    p.value = var.test(x, y)$p.value
    if (p.value >0.05) {
      my.test = t.test(x,y,alternative = my.alternative,mu = my.mu,var.equal = T)
      return(c(my.test$stat, my.test$parameter, my.test$p.value))
    } else {
      my.test = t.test(x, y, alternative = my.alternative, mu = my.mu)
      return(c(my.test$stat, my.test$parameter, my.test$p.value))
      }
  } else {
    my.test = t.test(x, alternative = my.alternative, mu = my.mu)
    return(c(my.test$stat, my.test$parameter, my.test$p.value))
    }
}

#5-----------------------------------------------
#a
beta_hat = sum(x*y)/sum(x^2)
#b
set.seed(123)
x <- rnorm(50)
y <- 2*x+rnorm(50)
beta_hat = sum(x*y)/sum(x^2)
plot(x,y)
lines(x,beta_hat*x)
#c
lm(y~x-1)

