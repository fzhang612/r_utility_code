setwd('d://temp')
dir()
abv2 <- read.csv('abv2.csv')
head(abv2)
library(ggplot2)
ggplot(abv2, aes(Score, colour = flg2, linetype = factor(response))) + geom_density(size = 1.2)
by(abv2, abv2$flg2, function(x) {ks.test(x$Score[x$response == 1], x$Score[x$response == 2])})
ks.test(abv2$Score[abv2$response == 1], abv2$Score[abv2$response == 2])
ddply(abv2, .(flg2, response), function(x) {summary(x$Score)})


ab2 <- read.csv('ab2.csv')
head(ab2)
ab2[ab2$flg2 == "",]
ab2 <- ab2[-5278,]
ab2$flg2 <- ab2$flg2[, drop = T]
levels(ab2$flg2)


by(ab2, ab2$flg2, function(x) {ks.test(x$Score[x$response == 1], x$Score[x$response == 2])})
ks.test(ab2$Score[ab2$response == 1], ab2$Score[ab2$response == 2])
ggplot(ab2, aes(Score, colour = flg2, linetype = factor(response))) + geom_density(size = 1.2)
ddply(ab2, .(flg2, response), function(x) {summary(x$Score)})


setInternet2(T)
require(RCurl)
txt <- getURL('http://www.omegahat.org/RCurl/')


setwd('d:\\temp')
df <- read.csv('cif201102ce.csv')

summary(df$FINCHG)
dim(df)
# df <- data.frame(loan = runif(100000), ibar = runif(100000), trx = runif(100000), unused = runif(100000), ibflag = rbinom(10000, 1, 0.1))
# df$ibar = df$ibar * df$ibflag
# df$trx = df$trx * (1-df$ibflag)
# df <- df[,-5]
# df1 <- as.data.frame(prop.table(as.matrix(df), 1)*100)

df$loan <- df$UNBILL_AR / df$CRLIMIT *100
df$ibar <- ifelse(df$FINCHG > 0, df$BILL_AR / df$CRLIMIT*100, 0)
df$trx <- ifelse(df$FINCHG <= 0, df$BILL_AR / df$CRLIMIT*100, 0)

df$loan[df$loan<0] <- 0
df$loan[df$loan>100] <- 100
df$ibar[df$ibar<0] <- 0
df$ibar[df$ibar>100] <- 100
df$trx[df$trx<0] <- 0
df$trx[df$trx>100] <- 100

plot(density(df$ibar))
summary(df$ibar)


library(ggplot2)
ggplot(df, aes(y=loan)) + geom_point(aes(x=ibar, colour = 'blue', alpha = 0.05))
last_plot() + geom_point(aes(x=-1*trx, colour = 'green', alpha = 0.05))
last_plot() + geom_abline(slope = 1, intercept = 100, linetype = 2) + geom_abline(slope = -1, intercept = 100, linetype = 2)
last_plot() + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + coord_equal()
last_plot() + scale_y_continuous(limits = c(0,100))
last_plot() + scale_x_continuous('Card', limits = c(-100, 100), breaks = c(-100, -50, 0, 50, 100), labels = c('T100', 'T50', '0', 'R50', 'R100'))
last_plot() + opts(legend.position = 'none')


setwd('d:\\temp')
hkh <- read.csv('hkhousing.csv')
head(hkh)

require(corrplot)
corr <- cor(hkh[,-c(1:2)])
corrplot(corr, type = 'upper', diag = F, addtextlabel = 'td')

pca <- princomp(hkh[,3:10], cor = T)
# pca <- prcomp(hkh[,3:8], scale. = T, retx = T)
# summary(pca)
print(pca)
plot(pca)
screeplot(pca)
biplot(pca)
loadings(pca)
rotated <- varimax(loadings(pca)[,1:2])
rotated$loadings
plot(rotated$loadings)


loading.df <- as.data.frame(matrix(pca$loadings, nrow = 8))
rownames(loading.df) <- dimnames(pca$loadings)[[1]]
colnames(loading.df) <- dimnames(pca$loadings)[[2]]

ggplot(loading.df, aes(Comp.2, Comp.1)) + 
geom_point(colour = 'blue', size = 5) + 
geom_text(aes(label=rownames(loading.df)), vjust = 1, hjust = 0.2, angle = 30) +    
scale_x_continuous(limits = c(-0.7, 0.7)) + opts(title = 'Comp 1 vs Comp 2')

ggplot(loading.df, aes(Comp.3, Comp.1)) + 
geom_point(colour = 'blue', size = 5) + 
geom_text(aes(label=rownames(loading.df)), vjust = 1, hjust = 0.2) +  
scale_x_continuous(limits = c(-0.3, 0.9)) + opts(title = 'Comp 1 vs Comp 3')


############ collaborative filtering product xsell
setwd('d:/temp')
d <- read.csv('next_best_var.csv', na.strings = '.')
d1 <- d[,1:25]
d2 <- d[,-(1:25)]

d1[is.na(d1)] <- 0
d1 <- as.matrix(d1)

# ########### test package recommenderlab ########## 
require(recommenderlab)
d1.brm <- as(d1, 'binaryRatingMatrix')
d1.brm
dim(d1.brm); dimnames(d1.brm); colCounts(d1.brm); colSums(d1.brm)

d1.brm2 <- d1.brm[rowCounts(d1.brm)>=2,]
d1.brm3 <- d1.brm[rowCounts(d1.brm)>=3,]
#d1.brm3 <- d1.brm3[,colCounts(d1.brm3)>=1200]
d1.brm3
hist(rowCounts(d1.brm3))
hist(colCounts(d1.brm3), breaks=20)
LIST(d1.brm2[1:3])
image(sample(d1.brm3, 200))

rec1 <- Recommender(d1.brm2[1:20000], method='POPULAR')
getModel(rec1)

# names(d)[order(colCounts(d1.brm2[1:20000]), decreasing=T)]
LIST(predict(rec1, d1.brm2[20001:20002], n= 5))
LIST(bestN(predict(rec1, d1.brm2[20001:20002], n= 5),n=3))

scheme <- evaluationScheme(d1.brm3, method = "cross", k = 10, given = 2)
results <- evaluate(scheme, method = "POPULAR", n = c(3,5,7), keepModel=T)
getConfusionMatrix(results)[[1]]
avg(results)



x1 <- c(1,0,0,1,0,0)
x2 <- c(1,1,0,0,1,1)
x3 <- c(0,1,1,1,0,0)

dat <- as.matrix(rbind(x1,x2))

dat.rm <- as(dat, 'binaryRatingMatrix')
scheme <- evaluationScheme(dat.rm, method='cross',k=2, given = 1)
results <- evaluate(scheme, method='POPULAR', n=1:2)
getConfusionMatrix(results)


 # LIST(d1.brm2[20001:20002])


# ########### test package recommenderlab ##########





# remove records with missing age
missing.age <- which(is.na(d2[,4]))
d1 <- d1[-missing.age, ]
d2 <- d2[-missing.age, ]

d <- as.matrix(d1)
dimnames(d)[[2]] <- substr(dimnames(d)[[2]], 1, 2)



set.seed(321)


# num of product distribution
table(apply(d, 1, sum))/dim(d)[1]*100

# product penetration distribution
single_prod_cust <- which(apply(d,1,sum) == 1)
apply(d[single_prod_cust, 2, sum)

# data split
multiple_prod <- which(apply(d, 1, sum) > 1)
val.set <- sample(multiple_prod, 10000)

# prod-based
prod.rate <- apply(d[-val.set,], 2, mean)
r <- matrix(NA, 25, 25)
dimnames(r)[[1]] <- dimnames(d)[[2]]
dimnames(r)[[2]] <- dimnames(d)[[2]]

for(i in 1:24) {
  for (j in (i+1):25) {
    r[i, j] <- sum(d[-val.set,i]*d[-val.set,j])/sqrt(sum(d[-val.set,i])*sum(d[-val.set,j]))
    r[j, i] <- r[i, j]
  }
}

require(ggplot2)
longr <- melt(r)
ggplot(longr, aes(X1, X2, fill = value)) + geom_tile()


rec_prod <- matrix(NA, nrow=25, ncol=3)
dimnames(rec_prod)[[1]] <- dimnames(r)[[1]]
dimnames(rec_prod)[[2]] <- c('First', 'Second', 'Third')

for (i in 1:25)   rec_prod[i,] <- dimnames(r)[[1]][order(r[i,], decreasing=T)[1:3]]


# mutlivariate probit model
require(bayesm)
dev.set <- sample((1:dim(d)[1])[-val.set], 10000)

# intercept only
X <- matrix(rep(diag(1, 25), dim(d[dev.set,])[1]), ncol = dim(d)[2], byrow = T)
y <- as.vector(t(d[dev.set,]))
p <- dim(d)[2]
R <- 500
out <- rmvpGibbs(Data = list(p=p, y=y, X=X), Mcmc = list(R=R))

rdraw <- t(apply(out$sigmadraw, 1, nmat))
attributes(rdraw)$class <- 'bayesm.var'
summary(rdraw)
sigma <- matrix(apply(rdraw[101:1000,],2,mean), ncol=25)

ind <- seq(from=0, by=p, length =p) + (1:p)
betatilde <- out$betadraw / sqrt(out$sigmadraw[,ind])
attributes(betatilde)$class <- 'bayesm.mat'
summary(betatilde)
beta <- apply(betatilde[101:1000,], 2, mean)


# intercept + age

X <- matrix(rep(diag(1, 3), dim(d[dev.set,])[1]), ncol = 3, byrow = T)
X <- cbind(X, X * rep(d2[dev.set, 4], each = 3))
y <- as.vector(t(d[dev.set, c(2,4,5)]))
p <- 3
R <- 500
out <- rmvpGibbs(Data = list(p=p, y=y, X=X), Mcmc = list(R=R))

rdraw <- t(apply(out$sigmadraw, 1, nmat))
attributes(rdraw)$class <- 'bayesm.var'
summary(rdraw)
sigma <- matrix(apply(rdraw[101:1000,],2,mean), ncol=25)

ind <- seq(from=0, by=p, length =p) + (1:p)
betatilde <- out$betadraw / sqrt(rep(out$sigmadraw[,ind],times=2))
attributes(betatilde)$class <- 'bayesm.mat'
summary(betatilde)
beta <- apply(betatilde[101:1000,], 2, mean)



# validation



# randomly pick one product to hide
random.hide <- function (x) sample(which(x==1), 1)
hide.set <- apply(d[val.set, ], 1, random.hide)
hide.prod <- dimnames(d)[[2]][hide.set]

val.data <- d[val.set,]
val.data[cbind(1:length(val.set), hide.set)] <- 0


# multivariate probit model
require(mnormt)

recom.mpm <- function(x, beta, sigma) {
#  x <- val.data[1,]
#  beta <- b
#  sigma <- s
  had <- which(x==1)
  had.prob <- pmnorm(beta[had], varcov=sigma[had,had])
  next.ind <- which(x==0)
  next.prob <- rep(0, length(next.ind))
  for (j in 1:length(next.ind)) next.prob[j] <- pmnorm(beta[c(had, next.ind[j])], varcov = sigma[c(had, next.ind[j]),c(had, next.ind[j])]) / had.prob
#  names(x)[order(next.prob, decreasing=T)][1:3]
  names(next.ind[order(next.prob, decreasing=T)])[1:3]

}

test <- apply(val.data, 1, recom.mpm, beta = beta, sigma = sigma)
sum(hide.prod == test[1,])
sum(hide.prod == test[2,])
sum(hide.prod == test[3,])




recom <- function(x, r) {
  rx <- as.vector(r[, which(x==1)])
  prod_names <- rep(dimnames(r)[[1]], sum(x, na.rm=T))[order(rx, decreasing=T)]
  unique(prod_names)[1:3]
}

neighbors <- d[-val.set, ]
prod.bonus <- 1/apply(neighbors, 2, sum)

recom.user <- function(x, K, prod.bonus) {
  #x <- val.data[1, ]
#  user.matrix <- d[-val.set,]
  #K <- 100
  
    
  # remove neighbors without any recommendations
  neighbors.set <- which(apply(t(t(.GlobalEnv$neighbors) - x), 1, function(x) sum(x==1) >= 1))
  
  # score calculation
  score <- .GlobalEnv$neighbors[neighbors.set,] %*% (x * (1+prod.bonus))
  top.score.set <- order(score, decreasing = T)[1:K]
  
  # remove existing products
  # neighbors[top.score.set, which(x==1)] <- 0
  # rec <- apply(neighbors * (1/apply(neighbors, 1, sum) * (score[neighbors.set]/max(score[neighbors.set]))), 2, sum)
  
  rec <- apply(.GlobalEnv$neighbors[neighbors.set,][top.score.set,],2,sum)
  rec[which(x==1)] <- 0
  names(rec)[order(rec, decreasing = T)[1:3]]
}

recom.prod.user <- apply(val.data[1:100,], 1, recom.user, K=100, prod.bonus = prod.bonus)


recom.prod <- apply(val.data, 1, recom, r=r)

# matching between the first recommendation and hiden product
sum(hide.prod == recom.prod[1,])
sum(hide.prod == recom.prod[2,])
sum(hide.prod == recom.prod[3,])

# matching between the top 3 recommendations and hiden product
sum(hide.prod == recom.prod[1,]) + sum(hide.prod == recom.prod[2,]) + sum(hide.prod == recom.prod[3,])

# If recommend based on popularity

recom.naive <- function(x, prod.rate) {
  names(sort(prod.rate[which(x==0)], decreasing=T))[1:3]
}

recom.prod.naive <- apply(val.data, 1, recom.naive, prod.rate = prod.rate)

sum(hide.prod == recom.prod.naive[1,])
sum(hide.prod == recom.prod.naive[2,])
sum(hide.prod == recom.prod.naive[3,])

sum(hide.prod == recom.prod.naive[1,]) + sum(hide.prod == recom.prod.naive[2,]) + sum(hide.prod == recom.prod.naive[3,])




# customer-based recommendation
i <- 13
score <- d[-i, ] %*% d[i,]
num_prod <- sum(d[i,])
rec <- apply(d[-i,][which(score == num_prod),], 2, sum)
rec[which(d[i,]==1)] <- 0
sort(rec, decreasing = T)



# scatter plot with hist

scatterhist = function(x, y, xlab="", ylab="") {  
  zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)  
  layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))  
  xhist = hist(x, plot=FALSE)  
  yhist = hist(y, plot=FALSE)  
  top = max(c(xhist$counts, yhist$counts))  
  par(mar=c(3,3,1,1))  
  plot(x,y)  
  par(mar=c(0,3,1,1))  
  barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)  
  par(mar=c(3,0,1,1))  
  barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)  
  par(oma=c(3,3,0,0))  
  mtext(xlab, side=1, line=1, outer=TRUE, adj=0, at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
  mtext(ylab, side=2, line=1, outer=TRUE, adj=0, at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
}


ds = read.csv("http://www.math.smith.edu/r/data/help.csv")

with(ds, scatterhist(mcs, pcs, xlab="MCS", ylab="PCS"))
