
### qqnorm and qqplot
x <- rnorm(1000)
qqnorm(x)
abline(0,1,col='red')
x <- rnorm(1000,mean=1)
x1 <- (x-mean(x))/sd(x)
qqnorm(x1)
abline(0,1,col='red')

x <- rt(1000,df=3)
qqnorm(x)
abline(0,1,col='red')

x <- rexp(1000)
ref_y <- rexp(10000)
qqplot(x,ref_y)
abline(0,1,col='red')

y <- c(rnorm(100,sd=2),rnorm(100,sd=0.5))
x <- rep(c('Pop1','Pop2'),each=100)
boxplot(y~x)

### Tables Ex 1 ###
smoke <- matrix(c(23,34,45,54),nrow=2)
colnames(smoke) <- c("Smoking","Nonsmoking")
rownames(smoke) <- c("Male","Female")
smoke

### Tables Ex 2 ###
set.seed(1)
M <- matrix(sample(c("A","B","C"),50,replace=TRUE),ncol=10)
?apply

sub_f <- function(x){
  y <- table(x)
  return(names(y)[which(y==max(y))])
}

my_f <- function(xmat){
  return(apply(xmat,1,sub_f))
}

my_f(M)

### paired sample t-test is equivalent to one sample t-test

### checking for normal distribution assumption
## either by qqnorm or shapiro wilk's test shapiro.test()
x <- rnorm(100,1,2)
shapiro.test(x)
x <- rexp(100)
shapiro.test(x)

set.seed(10)
x <- rnorm(10)
res <- t.test(x)
res$p.value
res$stat
mean(x)*sqrt(length(x))/sd(x)

### one sided (alternative='greater') p-value for wilcoxon signed rank test is
## pv = length(which(Wnull>=W))/length(Wnull) where Wnull is the collection of all wilcoxon signed rank test statistics enumerated from the null distribution and W is the observed wilcoxon signed rank statistics

?combn

### two sample t-test
ret <- read.table("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/d_logret_6stocks.txt", header=T)
attach(ret)
pR <- t.test(Pfizer,Intel,alternative='greater')$p.value
pL <- t.test(Pfizer,Intel,alternative='less')$p.value
pB <- t.test(Pfizer,Intel)$p.value ### two-sided test
### NOTE that pB = 2*min(pR,pL)
