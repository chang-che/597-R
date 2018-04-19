miles <- c(rnorm(30,20),rnorm(20,25),rnorm(100,30))
cars <- rep(c('A','B','C'),c(30,20,100))
my.dat = cbind(miles,cars)
row.names(my.dat) <- 1:150
str(my.dat)
my.dat <- as.data.frame(my.dat)
my.dat$miles <- as.numeric(my.dat$miles)
my.dat$cars <- as.factor(my.dat$cars)
str(my.dat)
boxplot(miles~cars, data = my.dat, xlab = 'Car model', ylab = 'Miles', main = 'My boxplot')
v1=sample(c(1,2,3,4),1000,replace=T,prob=c(0.1,0.2,0.4,0.3))
v1

x = rnorm(10000,1,20)
y = x + x^2
plot(x,y)