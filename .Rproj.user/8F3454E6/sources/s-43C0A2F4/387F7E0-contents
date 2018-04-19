####################################
#1.
#1)
dat <- read.delim('http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/leukemiaDataSet.txt' ,header=T,sep='\t')

library(glmnet)
library(caret)
require(doMC)
registerDoMC(cores=4)# cores should equal the number of cores in the computer

set.seed(123) 
trainID <- sample(1:72,round(0.7*72))

trainData <- dat[trainID,] 
testData <- dat[-trainID,]
trainData$Group <- factor(trainData$Group, levels = c('ALL', 'AML'))
#trainData$Group <- as.numeric(trainData$Group)
str(trainData)


trainX_data = as.matrix(trainData[,-1])
trainY_data = trainData[,1]
alpha_cand = seq(0, 1, 0.01) ## candidate alpha values
cvms = rep(0, length(alpha_cand)) ## cvms stores the minimum cross validation rate for each alpha
cvElaNet = lapply(alpha_cand, function(a){ cv.glmnet(x = trainX_data, y = trainY_data, family = "binomial",
                                                     nfolds = 4, alpha = a, type.measure = "auc", parallel = T ) } )
for(k in 1:length(alpha_cand)){
  
  cvms[k] = min(cvElaNet[[k]]$cvm)
  
}
opt_alpha = alpha_cand[which.min(cvms)]
opt_lambda = cvElaNet[[which.min(cvms)]]$lambda.1se
opt_model = cvElaNet[[which.min(cvms)]]

pred = predict(opt_model, as.matrix(testData[,-1]), type = 'class')
cbind(pred, testData[,1])

confusionMatrix(pred, factor(testData[,1], labels = c('ALL','AML')))
# the true positive rate(sensitivity) and the accuracy is in the output.
# Because the grid search method that Elastic Net applies, the output results may not be identical everytime. 



##########################
################
#2
vec.unif = runif(1000)
vec.exp = 1-1/2*log(1-vec.unif)
plot(density(vec.exp))

############################
##########################
#3
# the desity function of beta(3,2) is 1/Beta(2,3)*x*(1-x)^2 :f(x)
# use uniform random variable as proposal g(x)
# sup(f(x)/g(x)) = 160/9
num.beta = 1000
vec.beta = rep(NA, 1000)
niter = 0
i = 1
while(i < 1001){
  x <- runif(1)
  f <- runif(1)
  if (f <= dbeta(x, 2, 3)*9/160){
    vec.beta[i] = x
    i = i + 1
  }
  niter = niter + 1
}
plot(density(vec.beta))
qqplot(rbeta(1000, 2, 3), vec.beta)
abline(0, 1, col = 'red')

########################################
########################
#4
ga_generator <- function(size, alpha){
  ga.vec <- rep(NA, size)
  if (alpha != 1){
    i = 1 
    m = alpha*(alpha-1)/(alpha+1)
    M = dgamma(m, alpha, 1)/dexp(m, rate = 1/alpha)
    niter = 0
    while (i <= size) {
          x = runif(1)
          exp.point <- -alpha*log(runif(1))
          if (x*M < dgamma(exp.point, alpha, 1)/dexp(exp.point, rate = 1/alpha)){
            ga.vec[i] <- exp.point
            i = i+1
          }
          niter = niter + 1
        } 
  } 
  if (alpha == 1) {
    ga.vec = -alpha*log(runif(size))
    }
  return(ga.vec)
}
ga_generator(size = 1000, alpha = 3)
qqplot(qgamma((1:1000-0.5)/1000, shape = 3, scale = 1), ga_generator(1000, alpha = 3))
abline(0,1,col = 'red')


################################################
#5
size = 10000
niter = 0
i = 1
F.vec <- rep(NA, size)
while (i <= size) {
    x = runif(1)
    ga.p <- ga_generator(1, 5/2)
    m = 1/2
    M = df(m, 5, 10)/dgamma(m, shape = 5/2, scale = 1)
  if (x*M < df(ga.p, 5, 10)/dgamma(ga.p, shape = 5/2, scale = 1)){
    F.vec[i] <- ga.p
  i = i+1
  }
  niter = niter + 1
}
niter
qqplot(qf((1:10000 - 0.5)/10000, 5, 10), F.vec)
abline(0,1,col = 'red')




