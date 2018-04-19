mu <- seq(0,1,length=100)
logitf <- log(mu/(1-mu))
plot(mu,logitf,type='l')


### Construct a dataset
no.yes <- c("No","Yes")
smoking <- gl(2,1,8,no.yes)
### The function ‘gl’ is to generate levels. The first three
### arguments to gl are, respectively, the number of levels,
### the repeat count of each level, and the total length of the ### vector.
obesity <- gl(2,2,8,no.yes)
snoring <- gl(2,4,8,no.yes)
n.tot <- c(60,17,8,2,187,85,51,23)
n.hyp <- c(5,2,1,0,35,13,15,8)
data.frame(smoking,obesity,snoring,n.tot,n.hyp)

hyp.tbl <- cbind(n.hyp,n.tot-n.hyp)
glm.hyp <- glm(hyp.tbl~smoking+obesity+snoring, family=binomial("logit"))
summary(glm.hyp)
anova(glm.hyp, test="Chisq")

### alternative method ###
hyp.v = smoking.v = obesity.v = snoring.v <- NULL

for(i in 1:length(n.tot)){
  hyp.v <- c(hyp.v,rep(1,n.hyp[i]),rep(0,n.tot[i]-n.hyp[i]))
  smoking.v <- c(smoking.v,rep(as.character(smoking)[i],n.tot[i]))
  obesity.v <- c(obesity.v,rep(as.character(obesity)[i],n.tot[i]))
  snoring.v <- c(snoring.v,rep(as.character(snoring)[i],n.tot[i]))
}

new.dat <- data.frame(smoking.v,obesity.v,snoring.v,hyp.v)

glm.hyp.alt <- glm(hyp.v~smoking.v+obesity.v+snoring.v,data=new.dat,family=binomial("logit"))
