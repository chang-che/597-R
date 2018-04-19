######################################################
#####1.
#(a)
log_return <- read.table('http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/d_logret_6stocks.txt', header = T)
attach(log_return)
lm.a = lm(Pfizer ~ Citigroup + Exxon, data = log_return)
lm.a$coefficients

#(b)
df.citi.ex = data.frame(Citigroup, Exxon)

str(lm.a$fitted.values)
pc = predict(lm.a, interval = 'conf')

plot(Citigroup, Pfizer)
matlines(sort(Citigroup), pc[order(Citigroup),], lty = c(1,2,2), col = 'red')

plot(Exxon, Pfizer)
matlines(sort(Exxon), pc[order(Exxon),], lty = c(1,2,2), col = 'blue')

#(c)

anova(lm.a) # the p-values are less than 0.05, so we reject the null hypothesis and conclude the regression effects are significant

#(d)
lm.d = lm(Pfizer~ Citigroup + Exxon +0, data = log_return)
lm.d$coefficients

#(e)
cor(Pfizer, Exxon)
cor.test(Pfizer, Exxon)# p-value = 0.004238 < 0.05, so we reject the null hypothesis and conclude they are correlated.

##########################################
# 2.
#(a)
gr1 = c(Citigroup, AmerExp)
class(gr1)
gr2 = c(Exxon, GenMotor)
gr3 = Intel

Group = rep(1:3, c(128,128,64))
Group = as.factor(Group)
Y = c(gr1, gr2, gr3)
Data.gr = data.frame(Group, Y)
anova(lm(Y~Group, data = Data.gr[1:256,]))#p-value = 0.7681 > 0.05, so cannot reject the null which is means of Group1 and Group2 are different

#(b)
anova(lm(Y~Group, data = Data.gr)) # p-value = 0.5632, so cannot reject the null hypothesis that means of Gr1,Gr2,Gr3 are different

#############################################
#3.
data("ChickWeight")
ChickWeight = as.data.frame(ChickWeight)
str(ChickWeight)
attach(ChickWeight)
anova(lm(as.numeric(weight) ~ Time + Diet, data = ChickWeight))
# Both the p-values of Time and Diet are less than 0.05, so we can conclude at least one mean weight of Time i is different from others
# Same conclusion is for Diet.

#(b)
anova(lm(as.numeric(weight) ~ Diet, data = ChickWeight[Time == 2,]))
#p-value = 0.004 < 0.05, so we can conclude at least one mean weight of Diet is different from others
aov.Diet = aov(lm(as.numeric(weight) ~ Diet, data = ChickWeight[Time == 2,]))
TukeyHSD(aov.Diet) # only the the p-value of pair Diet4 and Diet1 is smaller than 0.05, so we can conclude Diet1 and Diet4 's means are different

##############################################
#4.
Pfizer.normal = scale(Pfizer)
qqnorm(Pfizer.normal)
shapiro.test(Pfizer) #p-value is 0.4194, so cannot reject the null hypothesis that the distribution is normal.

Pfizer.count = as.integer(Pfizer > 0)
Intel.count = as.integer(Intel > 0)

#(a)
binom.test(sum(Pfizer.count), 64, 0.55) #p-value is 0.04424 < 0.05 so reject the null hypothesis and conclude the prob of success is not equal to 0.55
#(b)
binom.test(sum(Intel.count), 64, 0.55, alternative = 'greater') # p-value=0.92 so do not have enough evidence to conclude proportion of positive > 0.55
#(c)
prop.test(c(sum(Pfizer.count), sum(Intel.count)), c(64, 64)) # p-value = 0.7221 so refuse to reject the null hypothesis that proportions of them are same.
#(d)
cutpoints = c(-Inf, -0.1, 0, 0.1, Inf)
Group1 = as.matrix(table(cut(gr1, cutpoints)))
Group2 = as.matrix(table(cut(gr2, cutpoints)))
Group3 = as.matrix(table(cut(gr3, cutpoints)))
Sum_Gr123 = cbind(Group1, Group2, Group3)
colnames(Sum_Gr123) <- c('Group1', 'Group2', 'Group3')
chisq.test(Sum_Gr123) # p-value= 0.000246 < 0.05, so there is enough evidence to conclude group and range effects are not independent


#######################################################
#5.
library(MASS)
library(lmtest)
data("mcycle")
attach(mcycle)

# Assume the noise of the model has constant variance and follows the normal distribution
# construct the design matrix of times to the power of 30.
matrix.model = as.data.frame(accel)
for (i in 1:30) {
  matrix.model = cbind(matrix.model, (times)^i)
  colnames(matrix.model)[i+1] = paste0('tiems^', i )
}
fit.30 = lm(accel ~., data = matrix.model)
# stepwise regression
step.fit = step(fit.30, direction = 'both')
summary(step.fit)
plot(step.fit$fitted.values, step.fit$residuals)# the points do not follow a consistent pattern along the x-axis instead follow a high order polynomial curve.
#perform Breusch-Pagan Test
bptest(step.fit)# p-value = 0.008925 < 0.05, so we have enough evidence to conclude the noise do not have a constant variance

qqnorm(step.fit$residuals)
qqline(step.fit$residuals) 
shapiro.test(step.fit$residuals) # p-value = 0.03542 reject the null that residuals follow normal distribution

