library(ISwR)
attach(cystfibr)
?cystfibr
plot(height,pemax)

fit1 <- lm(pemax~height+height^2)


fit_linear <- lm(pemax~height)
fit_quad <- lm(pemax~height+I(height^2))
fit_cubic <- lm(pemax~height+I(height^2)+I(height^3))


pred.frame <- data.frame(height=seq(110,180,2))
pp <- predict(fit_quad,newdata=pred.frame,interval='p')
plot(height,pemax,ylim=c(0,200))
matlines(pred.frame$height,pp,lty=c(1,2,2),col='green')

### cubic polynomial with 99% PI and CI
pp <- predict(fit_cubic,newdata=pred.frame,interval='p',level=0.99)
pc <- predict(fit_cubic,newdata=pred.frame,interval='c',level=0.99)
plot(height,pemax,ylim=c(0,200))
matlines(pred.frame$height,pp,lty=c(1,2,2),col='green')
matlines(pred.frame$height,pc,lty=c(1,2,2),col='red')


x <- rep(c("A","B","C"),each=10)
y <- rnorm(30)
mydat <- data.frame(y=y,x=x)
model.matrix(y~factor(x))
fit <- lm(y~factor(x))
summary(fit)
mean(y[x=='A']) ## this is equal to intercept which is the sample mean of group A
fit$coef[2] ## this is the mean difference of group B and group A
mean(y[x=='B'])-mean(y[x=='A'])
fit$coef[3] ## this is the mean difference of group C and group A
mean(y[x=='C'])-mean(y[x=='A'])
### note that A is also known as the reference level


### performing two sample t-test using lm
x <- rnorm(20)
y <- rnorm(15,0.5)
t.test(x,y,var.equal=TRUE)

new.y <- c(x,y)
new.grp <- rep(c(0,1),c(20,15)) ### when you have two groups, you can name the group 0, 1, to be safe, you can always let new.grrp=factor(new.grp)

fit <- lm(new.y~new.grp) ### this is equivalent to two sample t-test with equal variance assumption

x <- rep(c("A","B","C","D","E"),10)
y <- rnorm(50)
model.matrix(y~factor(x))


### model comparison
## either by adjusted R^2,
## if comparing two models, you may use anova()
## if comparing more than 2 models, you may use AIC() or BIC(), this is comparing the likelihood function of your models, adjsut
ing for number of parameters

### factor*factor interaction example

set.seed(123)
x1 <- rep(c('M','F'),each=30)
x2 <- rep(c('placebo','drug'),30)
y <- rep(NA,60)
y[x1=='M'&x2=='placebo'] <- rnorm(15)
y[x1=='M'&x2=='drug'] <- rnorm(15,1)
y[x1=='F'&x2=='placebo'] <- rnorm(15,3)
y[x1=='F'&x2=='drug'] <- rnorm(15,1)

fit <- lm(y~x1*x2)
par(mfrow=c(1,2))
boxplot(y~paste(x1,x2),main='Sig interaction')

### no interaction
set.seed(123)
x1 <- rep(c('M','F'),each=30)
x2 <- rep(c('placebo','drug'),30)
y <- 1+1*(x1=='F')+2*(x2=='drug')+rnorm(60)

fit.big <- lm(y~x1*x2)
fit.small <- lm(y~x1+x2)
boxplot(y~paste(x1,x2),main='No Int')

### more than two levels ###
set.seed(123)
x1 <- rep(c('AM','AF','C'),each=30)
x2 <- rep(c('placebo','drug'),45)
y <- rep(NA,90)
y[x1=='AM'&x2=='placebo'] <- rnorm(15)
y[x1=='AM'&x2=='drug'] <- rnorm(15,1)
y[x1=='AF'&x2=='placebo'] <- rnorm(15,3)
y[x1=='AF'&x2=='drug'] <- rnorm(15,1)
y[x1=='C'&x2=='placebo'] <- rnorm(15,3)
y[x1=='C'&x2=='drug'] <- rnorm(15,1)
fit.big <- lm(y~x1*x2)
fit.small <- lm(y~x1+x2)
anova(fit.small,fit.big)

### continuous*factor interaction
set.seed(123)
x1 <- rep(c('M','F'),each=30)
x2 <- rnorm(60)
y <- rep(NA,60)
y[x1=='M'] <- x2[x1=='M']+rnorm(30)
y[x1=='F'] <- 1+2*x2[x1=='F']+rnorm(30)
fit <- lm(y~x1*x2)
mydat <- data.frame(y=y,x1=x1,x2=x2)
plot(x2,y,type='n')
points(x2[x1=='M'],y[x1=='M'],col='blue',pch=19)
points(x2[x1=='F'],y[x1=='F'],col='red',pch=19)
abline(a=fit$coef[1]+fit$coef[2],b=fit$coef[3]+fit$coef[4],col='blue')
abline(a=fit$coef[1],b=fit$coef[3],col='red')

## HW3 QN4(d), example of generating rxc table
mydat <- data.frame(y=sample(1:5,100,replace=T),x=sample(LETTERS[1:3],100,replace=T))

xtabs(~mydat$y+mydat$x)
