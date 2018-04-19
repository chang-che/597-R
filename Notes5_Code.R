mygrp <- factor(rep(1:9,4))
mytime <- factor(rep(c(0,30,60,120),each=9))

set.seed(123)
y <- c(rnorm(100),rnorm(100,1),rnorm(100,0.1))
grp <- factor(rep(1:3,each=100))
fit <- lm(y~grp)
anova(fit)
boxplot(y~grp)
fit1 <- aov(y~grp)
TukeyHSD(fit1)

y <- c(rnorm(100),rnorm(100,1),rnorm(100,0.1,2))
grp <- factor(rep(1:3,each=100))
boxplot(y~grp)

### once you reject your null hypothesis that at least one equality is not true, you can go on to perform post-hoc all possible two sample comparisons using e.g, Tukey's method implemented in R as TukeyHSD

### ANOVA in regression
logret <- read.table("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/d_logret_6stocks.txt", header=T)
fit1 <- lm(logret$Pfizer~logret$Intel)
#fit1 <- lm(Pfizer~Intel,data=logret)


### now fit a model without intercept
fit2 <- lm(Pfizer~Intel-1,data=logret)
SST <- sum((logret$Pfizer-mean(logret$Pfizer))^2)
SSM <- sum((fit2$fitted-mean(logret$Pfizer))^2)
SSR <- sum((logret$Pfizer-fit2$fitted)^2)
SSM+SSR
SST
          
### fisher's exact test
mydat <-
matrix(c(10, 15, 4, 20),
       nrow = 2,
       dimnames = list(Group = c("Grp1", "Grp2"),
                       Outcome = c("S", "F")))
fisher.test(mydat) ## exact test

success <- c(10,15)
n <- c(14,35)
prop.test(success,n) ### normal approx

#### Chisq-test ###
caff.marital <- matrix(c(652,1537,598,242,36,46,38,21,218,327,
106,67), nrow=3,byrow=T)
colnames(caff.marital) <- c("0","1-150","151-300",">300")
rownames(caff.marital) <- c("Married","Prev.married","Single")

test1 <- chisq.test(caff.marital)
E <- test1$expected
O <- test1$observed
sum((O-E)^2/E)
pchisq(sum((O-E)^2/E),df=6,lower.tail=F)

## expected counts for (1,1) cell
sum(caff.marital[1,])*sum(caff.marital[,1])/sum(caff.marital)
