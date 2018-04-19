cancerdat = read.table('http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/cancerdat.txt', header = T)

#(a)
str(cancerdat)
cancerdat$smoking = as.factor(cancerdat$smoking)

summary(aov(y~smoking, data = cancerdat)) #p-value << 0.05, conclude that at least one mean of y differ by smoking
TukeyHSD(aov(y~smoking, data = cancerdat)) # smoking group2 and group1, group3 and group1's means are different as their p-value are significant

#(b)
cancerdat$race = as.factor(cancerdat$race)
str(cancerdat)
summary(lm(y~race, data = cancerdat))
#Both the Caucasian and other race's means are not different from that of African American under the alpha level = 0.05 

#(c)
num.cau = nrow(cancerdat[cancerdat$race == '2',])
num.afr = nrow(cancerdat[cancerdat$race == '1',])
num.cau.smoking = nrow(cancerdat[cancerdat$race == '2'&cancerdat$smoking == '3',])
num.afr.smoking = nrow(cancerdat[cancerdat$race == '1'&cancerdat$smoking == '3',])
prop.test(c(num.cau.smoking, num.afr.smoking), c(num.cau, num.afr), alternative = 'greater')
# p-value = 0.5 >0.05, so do not have enough evidence against the null that proportion of current smoker among caucasian is greater than that among African American

#(d)
summary(cancerdat$y)
cutpoints = c(-Inf, -17.857, -10.463, -4.541, Inf)
smoking1 = as.matrix(table(cut(cancerdat[cancerdat$smoking == '1',]$y, cutpoints)))
smoking2 = as.matrix(table(cut(cancerdat[cancerdat$smoking == '2',]$y, cutpoints)))
smoking3 = as.matrix(table(cut(cancerdat[cancerdat$smoking == '3',]$y, cutpoints)))

table.ycat = cbind(smoking1, smoking2, smoking3)
chisq.test(table.ycat)
#p-value < 0.05, so we have enough evidence to conclude that the smoking status and biomarker measurements are not independent

#(e)
fit.1 = lm(BMI~age, data = cancerdat)
summary(fit.1)
plot(fit.1$residuals, fit.1$fitted.values) # the residuals are approximately flat, so the assmuption of homogeneous variance is not violated.
qqnorm(fit.1$residuals)
abline(0,1, col = 'red')
shapiro.test(fit.1$residuals)
# both the qqnorm plot and shapiro wilk test's result indicate that the residuals are not normally distributed.
#(f)
?predict.lm
predict(fit.1, interval = 'pred', newdata = data.frame('age' = 85), level = 0.90)
#      fit      lwr      upr
# 24.9661 12.76887 37.16332

#2.

dat_sub1 <- cancerdat[1:150,]
dat_sub2 <- cancerdat[-c(1:150),]

#(a)
plot(y~smoking, data = dat_sub1)
plot(y~BMI, data = dat_sub1)
plot(y~age, data = dat_sub1)
plot(y~race, data = dat_sub1)

#(b)
fit.2 = lm(y~.^4, dat_sub1)# get a full model that up to 4 order interactions in the model
fit.step = step(fit.2, direction = 'both', k = log(150)) # use stepwise regression, when BIC is improved most at that step, that term stays or eliminated.
summary(fit.step)
#(c)
y_hat = predict(fit.step, newdata = dat_sub2)
cor(dat_sub2$y, y_hat, method = 'spearman')
#0.9809844
