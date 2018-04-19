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

################################################
#Use Elastic net
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
# Because the grid search that Elastic Net applies, the output results may not be identical everytime. 
