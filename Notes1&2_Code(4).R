library("ISwR")
rnorm(20)
weight <- c(60,72,57,90,95,72)
height=c(1.75,1.8,1.65,1.9,1.74,1.91)
bmi <- weight/height^2
var(bmi) ## this is to compute variance
sqrt(var(bmi))
sd(bmi)
median(bmi) ## compute median
cov(weight,height) ## compute covariance
cor(weight,height) ## compute pearson correlation

### to get help in R
help.search('covariance')
height2 <- height
height2[1]=NA
cor(weight,height2)
cor(weight,height2,use='complete.obs')
t.test(bmi,mu=22.5)

## ? Do not understand par()
par(mfrow=c(1,2)) #Divide the plot area into two?
plot(height,weight)
plot(height,weight,pch=2)#pch (plotting character)is to get the symbol to use
hh <- c(1.65,1.7,1.8,1.85,1.9)
lines(hh,22.5*hh^2)
points(1.7,70,pch=19,cex=1.5,col='red') #ces: character expansion
## to change x-axis label, use xlab
## to add main title, use main
plot(height,weight,xlab='HEIGHT',ylab='WT',main='MyPLOT')


### 7,7,7,9,9,9,13,13,13,
oops <- c(7,9,13)
rep(oops,each=3)

## plotting mathematical expression
## please see https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html

par(mfrow=c(1,2))
plot(1:10,1:10,xlab=expression(hat(beta)),ylab=expression(x+x^2))
plot(1:10, 1:10,type='n')
text(4, 9, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
text(4, 7, expression(bar(x) == sum(frac(x[i], n), i==1, n)))
text(8, 5, expression(paste(frac(1, sigma*sqrt(2*pi)), " ",plain(e)^{frac(-(x-mu)^2, 2*sigma^2)})),cex = 1.2)

### to save a file ##
pdf('myfirstplot.pdf',width=10,height=6)
plot(1,2)
dev.off()

### reading data
logret <- read.table("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/m_logret_10stocks.txt", header=T)
logret 
dim(logret)
names(logret)
logret[1:10,-c(2,4)]
levels(logret$Date)
subset.1<-logret[logret[,2]>0.01,seq(1,5)]
plot(logret$AAPL, logret$ADBE)


## advanced graphics
## documentation is at http://ggplot2.tidyverse.org/reference/

library(ggplot2)
x <- sort(rnorm(100))
mydat <- data.frame(myx=x,myy=x+rnorm(100),mygroup=factor(rep(1:5,each=20)))
ggplot(mydat, aes(myx,myy,colour=mygroup)) + geom_point() 
ggplot(mydat, aes(myx,myy,colour=mygroup)) + geom_line()  
ggplot(mydat, aes(myx,myy,colour=mygroup)) + geom_point() + geom_line()

library(gridExtra)
p1 <- ggplot(mydat, aes(myx,myy,colour=mygroup)) + geom_point() 
p2 <- ggplot(mydat, aes(myx,myy,colour=mygroup)) + geom_line()  
p3 <- ggplot(mydat, aes(myx,myy,colour=mygroup)) + geom_point() + geom_line()
grid.arrange(p1,p2,p3,ncol=2)

     
df2 <- data.frame(group=rep(c('Discovery','Replication'),c(100,50)),y=c(rnorm(100,1),rnorm(50,2)))
ggplot(df2, aes(group, y))+ geom_boxplot(aes(fill=factor(group)))+ggtitle("My Plot")+ theme(text = element_text(size=20),legend.text=element_text(size=20),legend.title=element_blank(),plot.title = element_text(hjust = 0.5))

### alternative way of manipulating data.frame
library("ISwR")
data(thuesen)
str(thuesen)
thue3 <- thuesen[thuesen$blood.glucose<7,]

### changing symbol, label sizes
plot(x,y,xlab='myx',main='MYTITLE',cex=2,cex.lab=2,cex.main=1.5,ylim=c(-2,2))

### Fibonacci sequence
fibo <- function(N){
  if(N<=0|N%%1!=0) stop("N must be integer > 0!!")
  if(N==1) fibovec <- 1
  if(N==2) fibovec <- c(1,1)
  if(N>2){
    fibovec <- rep(NA,N)
    fibovec[1:2] <- 1
    for(i in 3:N){
      fibovec[i] <- fibovec[i-1]+fibovec[i-2]
    }
  }
  return(fibovec)
}

### reading external files
read.table()
read.csv()
read.delim()

x <- rnorm(50)
summary(x)

##without attach() statement
logret$Intel[1:10]
