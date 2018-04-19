## Q1
check.palindrome <- function(x){
  x <- as.character(x)
  y <- strsplit(x,split="")
  x1 <- y[[1]]
  x2 <- rev(y[[1]])
  if(all(x1==x2)==TRUE)print("A palindrome") else print("NOT a palindrome") 
}

#Check the function
check.palindrome("123racecar321") #"A palindrome"
check.palindrome("123radcar321") #"NOT a palindrome"

## Q2
my.F.test <- function(x,y){
  df1 <- length(x)-1
  df2 <- length(y)-1
  F <- var(x)/var(y)
  pv <- 2*min(pf(F,df1,df2,lower.tail=TRUE), pf(F,df1,df2,lower.tail=FALSE))
  #return(c(F,pv))
  return(list(Fstat=F,pvalue=pv))
}

#Check the function
set.seed(12345)
x <- rnorm(30)
y <- rnorm(20)
my.F.test(x,y) #F=0.5125151, pv=0.1017933
my.F.test(y,x) #F=1.9511620, pv=0.1017933

## Q3
#(a)
data(ChickWeight)
ChickWeight <- data.frame(ChickWeight[,-3])
par(mfrow=c(2,3))
for(i in c(0,2,4,6,8)){
  mydat <- ChickWeight[ChickWeight$Time==i,]
  boxplot(mydat$weight~mydat$Diet,xlab='Diet',ylab='Weight',main=paste('Time',i))
}

#(b)
mydat <- ChickWeight[ChickWeight$Time==4,]
x <- mydat$weight[mydat$Diet==1]
y <- mydat$weight[mydat$Diet==2]
shapiro.test(x) #p-value = 0.7103, do not reject H0, can assume the population is normally distributed
shapiro.test(y) #p-value = 0.2388, do not reject H0, can assume the population is normally distributed
var.test(x,y)  #p-value = 0.0774>0.05, do not reject H0, can assume the two populations have equal variances
t.test(x,y,alternative="less", var.equal=T) #p-value = 0.01316 <0.05, reject H0. 
#It can be concluded that the weight for Diet 1 is significantly lower than the weight for Diet 2 for Time 4

#(c)
s <- var(ChickWeight$weight)/mean(ChickWeight$weight) #41.46521
a <- mean(ChickWeight$weight)/s #2.937844
x <- rgamma(1000,shape=a,scale=s)
qqplot(x,ChickWeight$weight, xlab="Generated Gamma",ylab="Weight",main="QQPLOT")
abline(0,1) #The weight fits the gamma distribution
