
##############################################
### Apply Various Models to CDNOW data set ###
##############################################
setwd('C:\\Documents and Settings\\cahra6w\\Desktop\\clv-master-thesis\\clv-master-thesis')

source("helper.R")
source("model-nbd.R")
source("model-pareto-nbd.R")
source("model-bg-nbd.R")
source("model-cbg-cnbd-k.R")

### Read Data ###

cdData <- read.table("data/cdnow.csv", head=T)
names(cdData)[2] <- "x"
cdData$xcen <- cut(cdData$x,breaks=c(-1,0,1,2,3,4,5,6,Inf),labels=c(0,1,2,3,4,5,6,"7+"))

# load actual cumulative repeat sales data
cdActual <- read.table("data/cdnow-x.csv", head=T)[,2]


########################
### Pareto-NBD Model ###
########################

paMleFit <- paretoEstimateParametersMle(cdData, list(r=0.55, alpha=10, s=0.6, beta=11))
summary(paMleFit)
#        Estimate Std. Error z value     Pr(z)    
# r      0.553093   0.047596 11.6205 < 2.2e-16 ***
# alpha 10.573640   0.842213 12.5546 < 2.2e-16 ***
# s      0.606227   0.187266  3.2373  0.001207 ** 
# beta  11.670774   6.211526  1.8789  0.060259 .  
#     -2 log L: 19189.95
cdParetoParams <- as.list(coef(paMleFit))


### Plot estimated Churn Rate ###

cdPaPActive <- paretoComputePActive(cdData, cdParetoParams)

# compute average P(active|x,tx,T) and determine the proportion of
# customers buying in the second 39 weeks for each level of x
cdPaActual <- rep(0, max(cdData$x)+1)
cdPaEst <- rep(0, max(cdData$x)+1)
nx <- rep(0, max(cdData$x)+1)
for (y in unique(cdData$x)) {
  isx <- which(cdData$x==y)
  nx[y+1] <- length(isx)
  cdPaActual[y+1] <- sum(cdData$p2x[isx]>0)/nx[y+1]
  cdPaEst[y+1] <- sum(cdPaPActive[isx])/nx[y+1]
}
rm(y, isx)

# create right-censored version for plot
nrOfCensor <- 7
denom <- sum(nx[(nrOfCensor+1):length(nx)])

cdPaActCen <- cdPaActual[1:nrOfCensor]
cdPaActCen[nrOfCensor+1] <- (nx[(nrOfCensor+1):length(nx)] %*% cdPaActual[(nrOfCensor+1):length(nx)]) / denom

cdPaEstCen <- cdPaEst[1:nrOfCensor]
cdPaEstCen[nrOfCensor+1] <- (nx[(nrOfCensor+1):length(nx)] %*% cdPaEst[(nrOfCensor+1):length(nx)]) / denom

plot(c(0:nrOfCensor), cdPaActCen, typ="l", col="red", lwd=2, xlab="# Transactions in Weeks 1-39", ylab="P(active)", ylim=c(0,1))
lines(c(0:nrOfCensor), cdPaEstCen, col="blue", lwd=2)
legend(5, 0.4, legend=c("Empirical", "Pareto/NBD"), lwd=2, col=c("red", "blue"))
savePlot("plots/cdnow01.png", typ="png")


### Plot Conditional Expressions E[Y(t)|p1x,tx,T] ###

# period for which conditional expectations are to be computed
t <- 39

cdPaCe <- paretoConditionalForecast(cdData, cdParetoParams, t)

# compute average E[Y(t)|p1x,tx,T] and average actual number of 
# transactions in the second 39 weeks for each level of x
cdPaCeAct <- rep(0, max(cdData$x)+1)
cdPaCeEst <- rep(0, max(cdData$x)+1)
nx <- rep(0, max(cdData$x)+1)
for (y in unique(cdData$x)) {
  isx <- which(cdData$x==y)
  nx[y+1] <- length(isx)
  cdPaCeAct[y+1] <- sum(cdData$p2x[isx])/nx[y+1]
  cdPaCeEst[y+1] <- sum(cdPaCe[isx])/nx[y+1]
}
rm(y, isx)

# create right-censored version for plot
nrOfCensor <- 7
denom <- sum(nx[(nrOfCensor+1):length(nx)])

cdPaCeActCen <- cdPaCeAct[1:nrOfCensor]
cdPaCeActCen[nrOfCensor+1] <- (nx[(nrOfCensor+1):length(nx)] %*% cdPaCeAct[(nrOfCensor+1):length(nx)]) / denom
cdPaCeEstCen <- cdPaCeEst[1:nrOfCensor]
cdPaCeEstCen[nrOfCensor+1] <- (nx[(nrOfCensor+1):length(nx)] %*% cdPaCeEst[(nrOfCensor+1):length(nx)]) / denom

plot(c(0:nrOfCensor), cdPaCeActCen, typ="l", col="red", lwd=2, xlab="# Transactions in Weeks 1-39", ylab="Average # Transactions in Weeks 40-78")
lines(c(0:nrOfCensor), cdPaCeEstCen, col="blue", lwd=2)
legend(5, 2, legend=c("Actual", "Pareto/NBD"), lwd=2, col=c("red", "blue"))
savePlot("plots/cdnow02.png", typ="png")


####################
### BG-NBD Model ###
####################

bgMleFit <- bgEstimateParameters(cdData, list(r=1, alpha=2, a=1, b=2))
summary(bgMleFit)
#       Estimate Std. Error z value     Pr(z)    
# r     0.242598   0.012557 19.3194 < 2.2e-16 ***
# alpha 4.413702   0.378230 11.6694 < 2.2e-16 ***
# a     0.792900   0.185721  4.2693 1.961e-05 ***
# b     2.425772   0.705332  3.4392 0.0005835 ***

cdBgParams <- as.list(coef(bgMleFit))
#        r     alpha         a         b 
# 0.2425982 4.4137019 0.7929001 2.4257717 

t <- 39
cdBgCe <- bgConditionalForecast(cdData, cdBgParams, t)

(cdBgSumEstimate <- sum(cdBgCe))
# 1653.421

(cdBgMsle <- mean((log(cdData$p2x+1)-log(cdBgCe+1))^2))
# 0.234272

# Result: The aggregated estimate is a bit worse than Pareto-NBD (i.e. they both
# under-estimate the actual value), but mean squared error is lower, and therefor
# provides better forecast on an individual base.

(corr <- cor(cdData$p2x, cdBgCe))
# 0.625633


#####################
### CBG-NBD Model ###
#####################

cbgMleFit <- cbgkEstimateParameters(cdData, k=1, list(r=1, alpha=2, a=1, b=2))
summary(cbgMleFit)
# r     0.523965   0.082885  6.3216 2.589e-10 ***
# alpha 6.175587   0.693238  8.9083 < 2.2e-16 ***
# a     0.892346   0.155126  5.7524 8.799e-09 ***
# b     1.618559   0.538979  3.0030  0.002673 ** 
# -2 log L: 19164.27 

cdCbgParams <- as.list(coef(cbgMleFit))

t <- 39
cdCbgCe <- cbgkConditionalForecast(cdData, k=1, cdCbgParams, t)

(cdCbgSumEstimate <- sum(cdCbgCe))
# 1576.475

(cdCbgMsle <- mean((log(cdData$p2x+1)-log(cdCbgCe+1))^2))
#  0.2314298

# Result: The individual-level forecast is a little better than BG-NBD. But 
# the aggregated estimate is worse.


### CBG-CNBD-2
cbg2MleFit <- cbgkEstimateParameters(cdData, k=2, list(r=1, alpha=2, a=1, b=2))
logLik(cbg2MleFit)
cdCbg2Params <- as.list(coef(cbg2MleFit))
cdCbg2Ce <- cbgkConditionalForecast(cdData, k=2, cdCbg2Params, 39)


### CBG-CNBD-3
cbg3MleFit <- cbgkEstimateParameters(cdData, k=3, list(r=1, alpha=2, a=1, b=2))
logLik(cbg3MleFit)
cdCbg3Params <- as.list(coef(cbg3MleFit))
cdCbg3Ce <- cbgkConditionalForecast(cdData, k=3, cdCbg3Params, 39)

### CBG-CNBD-4
cbg4MleFit <- cbgkEstimateParameters(cdData, k=4, list(r=1, alpha=2, a=1, b=2))
logLik(cbg4MleFit)
cdCbg4Params <- as.list(coef(cbg4MleFit))
cdCbg4Ce <- cbgkConditionalForecast(cdData, k=4, cdCbg4Params, 39)

### CBG-CNBD-5
cbg5MleFit <- cbgkEstimateParameters(cdData, k=5, list(r=1, alpha=2, a=1, b=2))
logLik(cbg5MleFit)
cdCbg5Params <- as.list(coef(cbg5MleFit))
cdCbg5Ce <- cbgkConditionalForecast(cdData, k=5, cdCbg5Params, 39)

### NBD Model
nbdMleFit <- nbdEstimateParameters(cdData, list(r=1, alpha=2))
logLik(nbdMleFit)
cdNbdParams <- as.list(coef(nbdMleFit))
cdNbdCe <- nbdConditionalForecast(cdData, cdNbdParams, 39)


######################
### Compare Models ###
######################

# Overview

data <- list(
    nbd=list(param=unlist(cdNbdParams), est=cdNbdCe),
    pa=list(param=c(coef(paMleFit)[1:2],NA,NA), est=cdPaCe),
    bg=list(param=coef(bgMleFit), est=cdBgCe), 
    cbg=list(param=coef(cbgMleFit), est=cdCbgCe),
    cbg2=list(param=coef(cbg2MleFit), est=cdCbg2Ce),
    cbg3=list(param=coef(cbg3MleFit), est=cdCbg3Ce),
    cbg4=list(param=coef(cbg4MleFit), est=cdCbg4Ce),
    cbg5=list(param=coef(cbg5MleFit), est=cdCbg5Ce)
  )
result <- buildResultTable(data, cdData$p2x, cdNbdCe)
round(result,4)
#           r   alpha      a      b     El     Ep   msle   rmse   corr  cumerr  gmrae  mdrae
# nbd  0.3847 12.0715     NA     NA 0.0319     NA 0.2917 1.7483 0.5772  0.0338 1.0000 1.0000
# pa   0.5531 10.5736     NA     NA 0.0523     NA 0.2377 1.6029 0.6302 -0.1150 1.9056 3.7154
# bg   0.2426  4.4137 0.7929 2.4258 0.0550 0.2463 0.2343 1.6080 0.6256 -0.1215 2.4623 6.6973
# cbg  0.5240  6.1756 0.8923 1.6186 0.0848 0.3554 0.2314 1.6072 0.6272 -0.1623 2.2077 5.3131
# cbg2 0.5774  2.2801 1.1397 1.5985 0.2532 0.4162 0.2388 1.6928 0.6141 -0.4747 1.1508 2.3578
# cbg3 0.5329  1.2580 1.2888 1.8456 0.4236 0.4112 0.2542 1.7759 0.6070 -0.6071 0.7395 1.6004
# cbg4 0.4958  0.8307 1.4067 2.0891 0.5968 0.4024 0.2688 1.8443 0.6025 -0.6867 0.5210 1.2295
# cbg5 0.4696  0.6071 1.4995 2.2943 0.7736 0.3953 0.2821 1.8981 0.5986 -0.7401 0.3757 1.0009

# Plot disaggregated forecast accuracy

actual <- tapply(cdData$p2x, cdData$xcen, mean)
nbd <- tapply(cdNbdCe, cdData$xcen, mean)
cbg2 <- tapply(cdCbg2Ce, cdData$xcen, mean)
bg <- tapply(cdBgCe, cdData$xcen, mean)
pa <- tapply(cdPaCe, cdData$xcen, mean)
plotConditionalForecast(data=list("Actual Mean"=actual,"Pareto/NBD"=pa,"BG/NBD"=bg,"CBG/CNBD-2"=cbg2), box=cdData$p2x~cdData$xcen)
savePlot("plots/cdnow03.png", typ="png")

# Goodness-of-fit Test for Training Period

propsActual <- prop.table(table(cdData$xcen))
Tmean <- round(mean(cdData$T))
propsBg <- sapply(0:6,bgPx,t=Tmean,params=cdBgParams)
propsBg[8] <- 1-sum(propsBg[1:7])
propsCbg <- sapply(0:6,cbgkPx,k=1,t=Tmean,params=cdCbgParams)
propsCbg[8] <- 1-sum(propsCbg[1:7])
propsCbg2 <- sapply(0:6,cbgkPx,k=2,t=Tmean,params=cdCbg2Params)
propsCbg2[8] <- 1-sum(propsCbg2[1:7])
propsNbd <- sapply(0:6,nbdPx,t=Tmean,params=cdNbdParams)
propsNbd[8] <- 1-sum(propsNbd[1:7])
bp <- barplot(rbind("Observed"=propsActual,"NBD"=propsNbd,"BG/NBD"=propsBg,"CBG/NBD"=propsCbg,"CBG2/NBD"=propsCbg2),main="Distribution of Purchase Frequencies\nin Training Period",beside=T,legend=T,space=c(0,0.5))
savePlot("plots/cdnow04.png", typ="png")

round(rbind(propsNbd, propsCbg, propsCbg2), 5)
#              [,1]    [,2]    [,3]    [,4]    [,5]    [,6]    [,7]    [,8]
# propsCbg  0.60024 0.18221 0.08454 0.04619 0.02769 0.01763 0.01171 0.02980
# propsCbg2 0.60112 0.18157 0.08376 0.04554 0.02727 0.01742 0.01165 0.03166
