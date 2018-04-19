#1.
check.palindrome <- function(x){
  x = as.character(x)
  y1 = strsplit(x,split = '')
  y2 = y1[length(y1):1]
  if (identical(y1,y2)) {
    return('A palindrome')
  }else { return('Not a palindrome')}
}
check.palindrome(12321)
check.palindrome('noon')

#2.
my.F.test <-  function(x,y){
  s1 = var(x)
  s2 = var(y)
  stat = s1/s2
  if (stat<1) {
    pv = pf(stat,length(x)-1,length(y)-1,lower.tail = T)*2
    return(list(stat=stat,pvalue=pv))
  } else {pv = pf(stat,length(x)-1,length(y)-1,lower.tail = F)*2
  return(list(stat=stat,pvalue=pv))
 }
}

#3
#(a)
data("ChickWeight")
ChickWeight <- data.frame(ChickWeight)
par(mfrow = c(2,3))
boxplot(weight~Diet,data = ChickWeight[ChickWeight$Time==0,],xlab = 'Diet',ylab = 'Weight', main = 'Time0')
boxplot(weight~Diet,data = ChickWeight[ChickWeight$Time==2,],xlab = 'Diet',ylab = 'Weight', main = 'Time2')
boxplot(weight~Diet,data = ChickWeight[ChickWeight$Time==4,],xlab = 'Diet',ylab = 'Weight', main = 'Time4')
boxplot(weight~Diet,data = ChickWeight[ChickWeight$Time==6,],xlab = 'Diet',ylab = 'Weight', main = 'Time6')
boxplot(weight~Diet,data = ChickWeight[ChickWeight$Time==8,],xlab = 'Diet',ylab = 'Weight', main = 'Time8')

#(b)
DATA1 = ChickWeight[ChickWeight$Time==4,]
var.test(DATA1[DATA1$Diet==1,]$weight,DATA1[DATA1$Diet==2,]$weight)
#p-value = 0.07 cannot reject the null when alpha = 0.05,so use euqal variance t test under normal assumption
t.test(DATA1[DATA1$Diet==1,]$weight,DATA1[DATA1$Diet==2,]$weight,var.equal = T)
#reject the null that their mean are equal because the p-value is significant at alpha level 0.05
wilcox.test(DATA1[DATA1$Diet==1,]$weight,DATA1[DATA1$Diet==2,]$weight)
#without the normal assumption, reject the null that their mean are equal because the p-value is significant at alpha level 0.05

#(c)
chick.weight= ChickWeight$weight
m = mean(chick.weight)
v = var(chick.weight)
beta = var(chick.weight)/mean(chick.weight)
alpha = mean(chick.weight)/beta
w <- qgamma((1:length(chick.weight) - 0.5)/length(chick.weight), shape = alpha, scale = beta)
qqplot(w, chick.weight, xlab="Theoretical Quantile", ylab="Sample Quantile")
abline(0,1)
?rgamma
