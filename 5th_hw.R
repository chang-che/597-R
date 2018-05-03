######################
# 1.
n <- 10000
sin.vec <- sin(runif(n, min = 0, max = pi/3))
mean(sin.vec)*pi/3
cos(0) - cos(pi/3)

# 2.
#True value
1-exp(-0.5)
#(a)
n <- 10000
exp.vec <- exp(-runif(n, min = 0, max = 1/2))
w_hat <- mean(exp.vec/2)
var(exp.vec/2)*(n-1)/n

#(b)
exp1.vec <- rexp(10000)
exp1.vec <- exp1.vec[exp1.vec<=0.5]
w_star <- rep(1-exp(-0.5), 10000)
var(w_star)

(c)
#omega_star's variance is smaller.

########
#3.
#(a)

rand.norm <- rnorm(20)
t0 <- mean(rand.norm)*sqrt(20)/sd(rand.norm)
w0 <- wilcox.test(rand.norm)$statistic

n <- 1000
rand.norm.matrix <- matrix(rnorm(20*n), ncol = 20)
t.vec <- apply(rand.norm.matrix, MARGIN = 1, function(x) mean(x)*sqrt(20)/sd(x))
w.vec <- apply(rand.norm.matrix, MARGIN = 1, function(x) wilcox.test(x)$statistic)

t.pvalue <- sum(abs(t.vec) >= abs(t0))/n
w.pvalue <- sum(abs(w.vec-20*(20+1)/4) >= abs(w0-20*(20+1)/4))/n # use mean of the statistic to get the deviation and to determine how extreme is this statistic

t.pvalue
w.pvalue
##theoretically p-value based on distribution of the statistic
pt(abs(t0),lower.tail = F, df = 19)*2
wilcox.test(rand.norm)$p.value

##(b)
rand.norm <- rnorm(20, mean = 0.5, sd = 1)
t1 <- mean(rand.norm)*sqrt(20)/sd(rand.norm)
w1 <- wilcox.test(rand.norm)$statistic

n <- 1000
rand.norm.matrix <- matrix(rnorm(20*n), ncol = 20)
t1.vec <- apply(rand.norm.matrix, MARGIN = 1, function(x) mean(x)*sqrt(20)/sd(x))
w1.vec <- apply(rand.norm.matrix, MARGIN = 1, function(x) wilcox.test(x)$statistic)

t.pvalue <- sum(abs(t1.vec) >= abs(t1))/n
w.pvalue <- sum(abs(w1.vec-20*(20+1)/4) >= abs(w1-20*(20+1)/4))/n # use mean of the statistic to get the deviation and to determine how extreme is this statistic
cat('The emprical Type I error for both tests are')
t.pvalue
w.pvalue
##theoretically p-value based on distribution of the statistic
pt(abs(t1),lower.tail = F, df = 19)*2
wilcox.test(rand.norm)$p.value

##power of the test
cL_t <- quantile(t1.vec, 0.025)
cU_t <- quantile(t1.vec, 0.975)
cL_w <- quantile(w1.vec, 0.025)
cU_w <- quantile(w1.vec, 0.975)
pwr_t = rep(NA, n)
pwr_w = rep(NA, n)

for (i in 1:n){
   x = rnorm(20, 0.5, 1)
   pwr_t[i] <- mean(x)*sqrt(20)/sd(x)
   pwr_w[i] <- wilcox.test(x)$statistic
}

cat('The empirical power of one-sample t test is', sum(pwr_t>= cU_t|pwr_t<= cL_t)/n)
cat('The empirical power of exact wilcoxon test is', sum(pwr_w>= cU_w|pwr_w<= cL_w)/n)

# theoretical power of the test

# 4.
set.seed(123)

x <- rnorm(50)

y <- 0.2*x+rnorm(50)

r0 <- cor(x,y,method = 'spearman')
xy <- c(x, y)

B <- 1000
cor.vec <-  rep(NA,B)
for (b in 1:B){
  x_index <- sample(1:100, 50, replace = F)
  cor.vec[b] <- cor(xy[x_index], xy[-x_index], method = 'spearman')
}
cat('The achieved significance level of permutation test is', sum(abs(cor.vec)>= abs(r0))/B)
cat('The p-value of cor.test is', cor.test(x, y, method = 'spearman', exact = T)$p.value)

