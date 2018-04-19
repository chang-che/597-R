


require(splines)
require(ISLR)
attach(Wage) 
agelims<-range(age)
age.grid<-seq(from=agelims[1], to = agelims[2])

# Cubic Spline with 3 Knots (cutpoints) 
#3 cutpoints at ages 25 ,50 ,60
fit<-lm(wage ~ bs(age,knots = c(25,40,60)),data = Wage )
summary(fit)

#Plotting the Regression Line to the scatterplot   
plot(age,wage,col="grey",xlab="Age",ylab="Wages")
points(age.grid,predict(fit,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(25,40,60),lty=2,col="darkgreen")

#fitting smoothing splines using smooth.spline(X,Y,df=...)
fit1<-smooth.spline(age,wage,df=16) #16 degrees of freedom
#Plotting both cubic and Smoothing Splines 
plot(age,wage,col="grey",xlab="Age",ylab="Wages")
points(age.grid,predict(fit,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(25,40,60),lty=2,col="darkgreen")
lines(fit1,col="red",lwd=2)
legend("topright",c("Smoothing Spline with 16 df","Cubic Spline"),col=c("red","darkgreen"),lwd=2)

# Implementing regular Cross Validation (cv=TRUE) or GCV (cv=FALSE) to select value of smoothing parameters and Implement Smoothing Splines:
fit2<-smooth.spline(age,wage)
fit2

#It selects $\lambda=0.0348627$ and df = 6.468299 
plot(age,wage,col="grey")
#Plotting Regression Line
lines(fit2,lwd=2,col="purple")
legend("topright",("Smoothing Splines with 6.47 df selected by GCV"),col="purple",lwd=2)
