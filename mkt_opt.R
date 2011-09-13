# MARKETING OPTIMIZATION R-SCRIPT
# Bob Agnew, raagnew@comcast.net, http://home.comcast.net/~raagnew
# The algorithm imbedded in this script optimizes the assignment of offers to
# prospects, subject to stipulated offer quantity constraints and the limit of
# at most one offer per prospect.  Remarkably, this demo problem, with a million
# prospects and three offers, solves in about a minute on my home PC.  Previously,
# I solved it using SAS PROC NLP on a big server and the time required was much
# greater. Ref:
# http://home.comcast.net/~raagnew/Downloads/Dual_Solution_Marketing_Optimization.pdf
# This script can be adapted for a different number of offers and alternative
# equality or inequality constraints, as long as they can be linearized, although
# the dual must be formulated for solution.  Inequality constraints would require
# a different R solver (nlminb) which incorporates lower bounds. It is also possible
# to allow more than one offer per prospect.  R for Windows or Mac can be downloaded
# free from www.r-project.org. This script can be opened, highlighted, and run from
# the R console.  It can also be run essentially in batch using the source function,
# e.g., source("c:/Optimization/R/mktg_opt.R").  
# Number of Prospects
n <- 1000000
# Initialize prospect offer and profit at zero, i.e., no offer.
Profit <- Offer <- array(0,dim = n)
# Simulate Prospect Profits for Three Offers
# Different profit vectors will be generated for each execution, which has the
# advantage of showing that solution is robust across various instances.
p1 <- 100*runif(n,0,1) - 10
p2 <- .6*p1 + .4*(60*runif(n,0,1) - 6)
p3 <- .4*p1 + .6*(40*runif(n,0,1) - 4)
# Ordinarily, offer profit "scores" would be input from an external text file using the
# scan function, i.e., inp <- scan("infile", list(0,0,0)); p1 <- inp[[1]];
# p2 <- inp[[2]]; p3 <- inp[[3]].  You can check this out by saving an instance of the
# simulated vectors via write(rbind(p1,p2,p3), file = "infile", ncolumns = 3) and then
# use a different version of this script with scan rather than simulation. 
# Mean Offer Profits
c(mean(p1),mean(p2),mean(p3))
# Stipulated Offer Quantities
b <- c(300000,200000,100000)
# Dual Function
dual <- function(u) {
d1 <- p1 - u[1]; d2 <- p2 - u[2]; d3 <- p3 - u[3]
v <- (d1>=0&d1>=d2&d1>=d3)*d1 + (d2>=0&d2>d1&d2>=d3)*d2 + (d3>=0&d3>d1&d3>d2)*d3
y <- b%*%u + sum(v)
y
}
# Dual Minimization using Nonlinear Minimization Function
out <- nlm(dual,c(0,0,0))
# Dual Minimization Output
out
# Keep dual minimum and estimates.
mindual <- out$minimum
Profit <- Offer <- array(0,dim = n)
u <- out$estimate
d1 <- p1 - u[1]; d2 <- p2 - u[2]; d3 <- p3 - u[3]
v <- (d1>=d2&d1>=d3)*d1 + (d2>d1&d2>=d3)*d2 + (d3>d1&d3>d2)*d3
ord <- order(v, decreasing = TRUE)
s <- c(0,0,0)
h <- n
for (j in 1:sum(b)) {
k <- s < b
m <- ord[j]
e1<-k[1]*d1[m]-(1-k[1])*h;e2<-k[2]*d2[m]-(1-k[2])*h;e3<-k[3]*d3[m]-(1-k[3])*h
e <- c(e1,e2,e3)
i <- order(e)[3]
Offer[m] <- i
p <- c(p1[m],p2[m],p3[m])
Profit[m] <- p[i]
s[i] <- s[i] + 1
}
# Dual Minimum (Upper Bound)
mindual
# Primal Optimum (Maximum Profit)
sum(Profit)
# Note that we are extremely close, in some instances right on.
# Offer Quantities
s
# Offer Profits
c(sum(Profit[Offer == 1]),sum(Profit[Offer == 2]),sum(Profit[Offer == 3]))
# In actual application, you would export a text file containing the optimal
# prospect offers, i.e., write(Offer, file = "outfile", ncolumns = 1).



###################################################################
# test CCBA problem
dat <- read.csv('d:/temp/sep11_martin_ab.csv')
dat <- read.csv('d:/temp/sep11_martin_cd.csv')
# dim(dat)
names(dat)
sort(names(dat))
order(names(dat))

price <- c(0.62, 0.66, 0.75, 0.82, 0.90) # A/b
price <- c(0.28, 0.29, 0.35, 0.39, 0.43, 0.49, 0.55, 0.59) # c/d


# row.idx <- dat$profseg == 'A/B' # & dat$PRICE_GRP == 'EXPERIAN'
row.idx <- rep(T, dim(dat)[1])
# sum(row.idx)

# L: loan amount
# A/B Kenny
# L <- as.matrix(dat[row.idx, c(28,46,31:33)])
# A/B Experian
# col.idx <- c(27, 46, 30, 33)
# C/D Kenny
# L <- as.matrix(dat[row.idx, c(36,51,40, 41, 52, 42)])
# C/D Experian
col.idx <- c(19,27,21,22,23) # A/B 0.66 ~ 27
col.idx <- c(36,37,22,34,35,32,23,33) # C/D

L <- as.matrix(dat[row.idx, col.idx])
head(L)
sum(is.na(L))

# R: response probability
col.idx <- c(7,26,9,10,13) # a/b 0.66 ~ 26
col.idx <- c(30,31,10,28,29,26,11,27) # c/d

# **********0.7 is bump rate applied to C/D******************
R <- as.matrix(dat[row.idx, col.idx]) # a/B
R <- as.matrix(dat[row.idx, col.idx]) * 0.73 # C/D
head(R)
sum(is.na(R))

# I: int income 
I <- sapply(1:length(price), function(x) {L[,x] * price[x]/100*20.8})

# Fee Income
FI <- L * 0.01733 # for A/B
FI <- L * 0.000933 # for C/D

######################################################################################

# COF: cost of fund
COF <- L * 0.01718 # for A/B
COF <- L * 0.01575 # for C/D


# Marketing cost
mc <- 3.31
# OB cost
obc <- 11.1 # for A/B
obc <- 7.48 # for C/D
# OB incentive
obi <- 0.0022 # for A/B
obi <- 0.003 # for C/D

# C: bad debt
C <- sapply(1:length(price), function(x) {L[,x] * dat[['exp_co']][row.idx]/2*1.75})

head(C)
sum(is.na(C))

# Cannibilisation
ca <- 0.012 # A/B
ca <- 0 # C/D

#######################################################################################

minL <- 166000000 # A/B
maxC <- 6400000 # A/B

minL <- 134000000 # c/d
maxC <- 2500000 # c/d
# minLWIR <- minL * 0.497



profit <- R*(I+FI-COF-C-obi*L-ca*L)-obc-mc
ExpL <- R*L
ExpC <- R*C
# ExpLR <- R*t(t(L)*price)

head(profit)
sum(is.na(profit))

# for additional column wise (resource) constraints
res.const.row <- c(1,0,0,0,0)
res.const <- matrix(rep(res.const.row, sum(row.idx)), nrow = sum(row.idx), byrow=T)
res.const.1 <- matrix(rep(c(1,0,0,0,0), sum(row.idx)), nrow = sum(row.idx), byrow=T)
res.const.2 <- matrix(rep(c(0,1,0,0,0), sum(row.idx)), nrow = sum(row.idx), byrow=T)


dual.obj <- function(u) {
  # loan amount constraint is not binding. so remove it from dual
  # v <- apply(profit - u*ExpC, 1, max)
  # sum(v) + u*maxC
  
  v <- apply(profit + u[1]*ExpL - u[2]*ExpC, 1, max)
  sum(v) - u[1]*minL + u[2]*maxC 

  #v <- apply(profit + u[1]*ExpL - u[2]*ExpC - u[3]*res.const, 1, max)
  # sum(v) - u[1]*minL + u[2]*maxC + u[3]*10000
  
  #v <- apply(profit - u[1]*ExpC - u[2]*res.const.1 - u[3]*res.const.2, 1, max)
  #sum(v) + u[1]*maxC + u[2]*10000 + u[3]*15000

}
# dual.obj(c(0,0))

#dual.opt <- optim(par = 0, fn = dual.obj, lower = 0, method = 'L-BFGS-B')
 dual.opt <- optim(par = c(0,0.5), fn = dual.obj, lower = c(0,0), method = 'L-BFGS-B')
 # dual.opt <- optim(par = c(0,0,0), fn = dual.obj, lower = c(0,0, 0), method = 'L-BFGS-B')
print(dual.opt)

# optimal solution
dual.opt$par
# optimal objective
dual.opt$value

# caluclate customer value without column wise constraints
# vv <- profit - dual.opt$par[1]*ExpC - dual.opt$par[2]*res.const.1 - dual.opt$par[3]*res.const.2
vv <- profit + dual.opt$par[1]*ExpL - dual.opt$par[2]*ExpC # + dual.opt$par[3]*ExpLR
# loan amount constraint is not binding. so remove it from dual
# vv <- profit - dual.opt$par*ExpC # + dual.opt$par[3]*ExpLR
v <- apply(vv, 1, max)
# row.order <- t(apply(vv, 1, order, decreasing=T))
hist(v)
summary(v)
# assign offer 
offer <- sapply(1:sum(row.idx), function(x) {which(vv[x,] == v[x])})
table(offer)

# special case, move some accounts to price 0.82 (A/B) in order to meet min 6000 acounts
candidate.idx <- which(row.order[,2] == 3)


# if there are column wise constraints
vv <- profit + dual.opt$par[1]*ExpL - dual.opt$par[2]*ExpC - dual.opt$par[3]*res.const
v <- apply(vv, 1, max)
hist(v)
summary(v)
# assign offer 
offer <- sapply(1:sum(row.idx), function(x) {which(vv[x,] == v[x])})
table(offer)
# const.customer <- which(offer==1 | offer ==2 | offer == 3)
const.customer <- which(offer %in% which(res.const.row==1))
length(const.customer)
if (length(const.customer) > 12000) {
  ord <- order(v[const.customer], decreasing = T)
  change.customer <- const.customer[order(v[const.customer], decreasing = T)[(12000+1):length(const.customer)]]
  v[change.customer] <- apply(vv[change.customer, -which(res.const.row==1)], 1, max)
  offer <- sapply(1:sum(row.idx), function(x) {which(vv[x,] == v[x])})
}


table(offer)

############### bau ################################
offer.bau <- dat$bau_pricing
table(offer.bau)
offer.bau[offer.bau==0.0028] <- 1
offer.bau[offer.bau==0.0029] <- 2
offer.bau[offer.bau==0.0035] <- 3
offer.bau[offer.bau==0.0039] <- 4
offer.bau[offer.bau==0.0043] <- 5
offer.bau[offer.bau==0.0049] <- 6
offer.bau[offer.bau==0.0055] <- 7
offer.bau[offer.bau==0.0059] <- 8

offer <- offer.bau

#################### bau #############################





# test offer vs dual.opt
x <- sum(profit[as.matrix(cbind(1:sum(row.idx), offer))])
cat('profit of offer:', format(x, big.mark=','), '\n')
cat('profit of dual.opt:', format(dual.opt$value, big.mark=','), '\n')


# test constraints
# minL
x <- sum(ExpL[as.matrix(cbind(1:sum(row.idx), offer))])
cat('loan amt of final offer:', format(x, big.mark=','), '\n')
cat('min required loan amt:', format(minL, big.mark = ',', scientific = F), '\n')
# maxC
x <- sum(ExpC[as.matrix(cbind(1:sum(row.idx), offer))])
cat('bad debt of final offer:',  format(x, big.mark=','), '\n')
cat('max required bad debt:', format(maxC, big.mark=',', scientific=F), '\n')

# test WIR
WIR <- sum((R*L)[as.matrix(cbind(1:sum(row.idx), offer))] * price[offer]) /sum((R*L)[as.matrix(cbind(1:sum(row.idx), offer))])
cat('WIR of offer:', WIR, '\n')

offer.summary <- function(x) {
  Count <- sum(offer == x)
  RR <- mean(R[which(offer == x), x])
  Profit <- sum(profit[which(offer == x), x])
  PIL.Int <- sum((R*I)[which(offer == x), x])
  Fee <- sum((R*FI)[which(offer == x), x])
  COF <- sum((R*COF)[which(offer == x), x])
  Mkt.Cost <- Count * mc
  OB.Cost <- Count * obc
  Loan.Amt <- sum((R*L)[which(offer == x), x])
  OB.Incentive <- Loan.Amt * obi
  Cannib <- Loan.Amt * ca
  Bad.Debt <- sum((R*C)[which(offer == x), x])
  Yield <- price[x]
  return(c(Count = Count, RR = RR, Loan.Amt = Loan.Amt, Yield = Yield, PIL.Int = PIL.Int, Fee = Fee, COF = COF, Mkt.Cost = Mkt.Cost, OB.Incentive = OB.Incentive, OB.Cost = OB.Cost,  Bad.Debt = Bad.Debt, Cannib = Cannib, Profit = Profit))
}

offer.table <- t(sapply(1:length(price), offer.summary))
offer.table <- rbind(offer.table, apply(offer.table, 2, sum))
offer.table[length(price)+1, 2] <- sum(offer.table[1:length(price),2]*offer.table[1:length(price),1] , na.rm = T)/sum(offer.table[1:length(price),1])
offer.table[length(price)+1, 4] <- sum(offer.table[1:length(price), 4] * offer.table[1:length(price), 3]) / offer.table[1+length(price), 3]
rownames(offer.table) <- c(as.character(price), 'Overall')
offer.table


# saved scenario

dual.opt.cd1 <- dual.opt
offer.cd1 <- offer


# dump to csv file
optim.price <- price[offer] / 100
write.csv(data.frame(CARDNBR = as.character(formatC(dat$CARDNBR,format='f',digits=0)), optim.price = optim.price), 'd:/temp/optim.price.AB.csv', row.names=F)


############################################################################
# bulid one model for all 3 prices (A/B)
###########################################################################

dat <- read.csv('d:/temp/pil_ab_dev.csv')
names(dat)
dat$response = 2 - dat$response
dat$resp = as.factor(dat$response)

dat$mth_flat_rate = dat$mth_flat_rate * 10000
dat$UPL_OS_avg6 = dat$UPL_OS_avg6 / 10000
dat$OTB_avg6 = dat$OTB_avg6 / 10000
dat$PIL_accept_rate_12M[is.na(dat$PIL_accept_rate_12M)] <- 0
dat$TOT_INT_FEE_avg3[is.na(dat$TOT_INT_FEE_avg3)] <- 0
dat$CSHBAL_avg6[is.na(dat$CSHBAL_avg6)] <- 0
dat$ARU_avg6[is.na(dat$ARU_avg6)] <- 0
dat$RTLBAL_chg13[is.na(dat$RTLBAL_chg13)] <- 0
dat$max_mature_to_date[is.na(dat$max_mature_to_date)] <- min(dat$max_mature_to_date, na.rm = T)
dat$min_mature_to_date[is.na(dat$min_mature_to_date)] <- max(dat$min_mature_to_date, na.rm = T)
dat$MTH_SINCE_LSTCSH[is.na(dat$MTH_SINCE_LSTCSH)] <- max(dat$MTH_SINCE_LSTCSH, na.rm=T)
dat$OTB_avg6[is.na(dat$OTB_avg6)] <- max(dat$OTB_avg6, na.rm=T)

dat$ageprice <- dat$age * sqrt(dat$mth_flat_rate)


set.seed(211)
row.idx <- runif(dim(dat)[1])
length(which(row.idx<=0.5))

dev <- dat[which(row.idx<=0.5), -(1:6)]
val <- dat[-which(row.idx<=0.5), -(1:6)]
names(dev)
summary(dev)
table(dev$PILPIL_SIZE_mth6)


# can I use tree to bin continuous variables?
require(rpart)
tree1 <- rpart(resp ~ age, data=dev, na.action = na.rpart, control=rpart.control(cp=-1, maxdepth=3, minbucket = 0.05*dim(dev)[1]))
print(tree1)
summary(tree1)
plot(tree1)


# can I use tree to bin continuous variables?


# test gradient boosting mathine (gbm) - begin
require(gbm)
gbm1 <- gbm(response ~ .,         # formula
    data=dev,                   # dataset
    #var.monotone=c(0,0,0,0,0,0), # -1: monotone decrease,
                                 # +1: monotone increase,
                                 #  0: no monotone restrictions
    distribution="adaboost",     # bernoulli, adaboost, gaussian,
                                 # poisson, coxph, and quantile available
    n.trees=3000,                # number of trees
    shrinkage=0.003,             # shrinkage or learning rate,
                                 # 0.001 to 0.1 usually work
    interaction.depth=3,         # 1: additive model, 2: two-way interactions, etc.
    bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
    #train.fraction = 0.5,        # fraction of data for training,
                                 # first train.fraction*N used for training
    n.minobsinnode = 100,         # minimum total weight needed in each node
    #cv.folds = 5,                # do 5-fold cross-validation
    # keep.data=TRUE,              # keep a copy of the dataset with the object
    verbose=TRUE)                # print out progress

gbm.perf(gbm1, method='OOB')

# convert f(xi) to probability when distribution = 'bernoulli'
prob <- 1/(1+exp(-gbm1$fit))
# convert f(xi) to probability when distribution = 'adaboost'
prob <- 1/(1+exp(-2*gbm1$fit))

ks.test(prob[dev$response==1], prob[dev$response==0])

f.predict <- predict.gbm(gbm1,val,2000)
val.prob <- 1/(1+exp(-f.predict))
ks.test(val.prob[val$response==1], val.prob[val$response==0])

# test gradient boosting mathine (gbm) - end


# m <- glm(response ~ .^2, data = dev, family = binomial)
summary(m)
require(MASS)
stepAIC(m)
summary(step)

require(mgcv)

m1 <- glm(response ~ PIL_accept_rate_12M + PILPIL_SIZE_mth6 +   UPL_OS_avg6 + age+ max_mature_to_date + min_mature_to_date + ARU_avg6 + MTH_SINCE_LSTCSH+OTB_avg6 + LATE_FEE_mth6 + (  age):sqrt(mth_flat_rate)  ,data = dev, family = binomial)

m2 <- glm(response ~ PIL_accept_rate_12M + PILPIL_SIZE_mth6 +   UPL_OS_avg6 + age+ max_mature_to_date + min_mature_to_date + ARU_avg6 + MTH_SINCE_LSTCSH+OTB_avg6 + LATE_FEE_mth6 + (  age):sqrt(mth_flat_rate) + I(min_mature_to_date^3) ,data = dev, family = binomial)

anova(m1,m2, test='Chisq')

mgam <- gam(response ~ s(PIL_accept_rate_12M) + PILPIL_SIZE_mth6 +   s(UPL_OS_avg6) + s(age)+ s(max_mature_to_date) + s(min_mature_to_date) + s(ARU_avg6) + s(MTH_SINCE_LSTCSH)+s(OTB_avg6) + LATE_FEE_mth6 + s(I(age*sqrt(mth_flat_rate)))  ,data = dev, family = binomial)


summary(mgam)
plot(mgam)
plot(mgam, pages=1,residuals=TRUE)


m.full <- m

length(m$fitted.values[dev$mth_flat_rate==56 & m.full$fitted.values >0.15])

m <- glm(response ~ PIL_accept_rate_12M + PILPIL_SIZE_mth6 +   UPL_OS_avg6 + age+ max_mature_to_date + min_mature_to_date + ARU_avg6 + MTH_SINCE_LSTCSH+OTB_avg6 + LATE_FEE_mth6 + (  age):sqrt(mth_flat_rate)            ,data = dev[-which(dev$mth_flat_rate==56 & m.full$fitted.values >0.15),], family = binomial)

m.part <- m


summary(m)
m <- m.full
m <- m.part
m <- m1
ks.test(m$fitted.values[m$data$response==1], m$fitted.values[m$data$response==0])


ks.test(m$fitted.values[m$data$response==1 & m$data$mth_flat_rate==56], m$fitted.values[m$data$response==0 & m$data$mth_flat_rate==56])
ks.test(m$fitted.values[m$data$response==1 & m$data$mth_flat_rate==73], m$fitted.values[m$data$response==0 & m$data$mth_flat_rate==73])
ks.test(m$fitted.values[m$data$response==1 & m$data$mth_flat_rate==90], m$fitted.values[m$data$response==0 & m$data$mth_flat_rate==90])


val.pred <- predict(m, val)
sum(is.na(val.pred))
ks.test(val.pred[val$response==1], val.pred[val$response==0])
ks.test(val.pred[val$response==1&val$mth_flat_rate==56], val.pred[val$response==0&val$mth_flat_rate==56])
ks.test(val.pred[val$response==1&val$mth_flat_rate==73], val.pred[val$response==0&val$mth_flat_rate==73])
ks.test(val.pred[val$response==1&val$mth_flat_rate==90], val.pred[val$response==0&val$mth_flat_rate==90])



require(ggplot2)
ggplot(dev) + geom_density(aes(x=age, color = factor(mth_flat_rate))) + facet_wrap(~response)



# learn caret package
require(caret)
names(dat)

inTrain <- createDataPartition(dat$response, p=0.6, list=F)
yy <- dat$resp
levels(yy) <- c('NR', 'R')


nzv <- nearZeroVar(dat[inTrain,])
nzv
cor(dat[inTrain, c(7, 9:22)])

bootctrl <- trainControl(number=25)


#svmFit <- train(dat[inTrain, c(7,9:22)], yy, method='svmRadial', tuneLength=3, trControl = bootctrl, scaled=T, verbose = T)
#svmFit

gbmGrid <- expand.grid(.interaction.depth = 4, .n.trees = 500, .shrinkage = .1)
gbmFit <- train(dat[inTrain, c(7,9:22)], yy[inTrain], method='gbm', trControl=bootctrl, bag.fraction=0.5, tuneGrid=gbmGrid)

# Pred <- predict(gbmFit, newdata=dat[-inTrain, c(7, 9:22)], type=c('prob'))
# Pred <- extractPrediction(list(gbmFit), testX = dat[-inTrain, c(7, 9:22)], testY = yy[-inTrain])
Pred <- extractProb(list(gbmFit), testX = dat[-inTrain, c(7, 9:22)], testY = yy[-inTrain])

plotClassProbs(Pred)

with(Pred, confusionMatrix(pred[dataType=='Training'], obs[dataType=='Training']))
with(Pred, ks.test(R[dataType=='Training' & obs == 'R'], R[dataType=='Training' & obs == 'NR']))
with(Pred, confusionMatrix(pred[dataType=='Test'], obs[dataType=='Test']))
with(Pred, ks.test(R[dataType=='Test' & obs == 'R'], R[dataType=='Test' & obs == 'NR']))


plot(roc(testPred$NR[testPred$dataType=='Test'],testPred$obs[testPred$dataType=='Test'] ))
aucRoc(roc(testPred$NR[testPred$dataType=='Test'],testPred$obs[testPred$dataType=='Test'] ))


gbmImp <- varImp(gbmFit, scale=F)
plot(gbmImp)


# test rfe

rfe.result <- rfe(dat[inTrain, c(7,9:22)], yy[inTrain],sizes=c(1:15), rfeControl = rfeControl(number = 40, functions = treebagFuncs))


# test sbf

sbf.result <- sbf(dat[inTrain, c(7,9:22)], yy[inTrain], sbfControl = sbfControl(number = 5, functions = treebagSBF))

# filterVarImp

filterVarImp(dat[inTrain, c(7,9:22)], yy[inTrain])


# ### learn rms and Hmisc
require(rms)
page(describe(dat),multi=T, method='print')

# plsmo takes y as numeric. factor doesn't work
with(dat, plsmo(c_mc_avgmth, response, group=mth_flat_rate, fun=qlogis))
