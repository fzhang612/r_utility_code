setwd('d:\\temp')

df <- read.csv('ab_jan_p73_reach2.csv')
require(mice)
mice(df)

head(df)

require(plyr)
nmissing <- function(x) sum(is.na(x))
nunique <- function(x) length(unique(x))


a <- colwise(nmissing)(df)
b <- colwise(nunique)(df)
df.summary <- cbind(t(a), t(a)/nrow(df)*100, t(b))
df.summary <- cbind(data.frame(sapply(df, class)), df.summary)
colnames(df.summary) <- c('type', 'nmissing', 'missing.pct', 'nunique')

df.summary[df.summary$nmissing > 10,]




# read web page

setInternet2(T)

getlinks <- function(url){
  require(stringr)
  
  webpage <- readLines(url)
  pattern <- '.*(http:.*?)[\'\"].*'
  linklines <- webpage[grep(pattern, webpage, ignore.case = T)]
  links <- unique(gsub(pattern, '\\1', linklines))
  return(links[str_length(links) > 7])
}

webpage <- readLines('http://www.fenxi001.com/index.html')

links<-getlinks('http://www.1128.org/')
getlinks('http://www.fenxi001.com')
getlinks(links[113])




################ PIL Pricing Optimization ##############################
setwd('d:\\temp\\optim')
intinc <- read.csv('may_intinc.csv')
intinc <- as.matrix(intinc[intinc$profseg == 'A/B', 3:13])
co <- read.csv('may_co.csv')
co <- as.matrix(co[co$profseg == 'A/B', 3:13])
intexp <- read.csv('may_int_exp.csv')
intexp <- as.matrix(intexp[intexp$profseg == 'A/B', 3:13])

N <- 1500
p <- 11

coN <- co[1:N, ]
intexpN <- intexp[1:N, ]
intincN <- intinc[1:N, ]


price <- c(0.56, 0.59, 0.62, 0.66, 0.69, 0.72, 0.73, 0.75, 0.79, 0.82, 0.90)
require(lpSolve)
direction <- 'max'
objective.in <- as.vector(t(intincN - coN - intexpN))

bin.const <- matrix(0, N, N*p)
bin.const[cbind(rep(1:N, each=p), 1:(N*p))] <- 1

#const.mat <- matrix(rep(price, N), nrow = 1)
dc <- as.matrix(cbind(rep(1:N, each = p), 1:(N*p), rep(1, N*p)))
c.dir <- rep('==', N)
c.rhs <- rep(1, N)
all.bin <- T

optim <- lp ("max", objective.in, bin.const, c.dir, c.rhs, all.bin = T)
optim <- lp ("max", objective.in, , c.dir, c.rhs, all.bin = T, dense.const = dc)
optim
a <- matrix(optim$solution, nrow = N, byrow = T)
apply(a, 2, sum)

optim <- lp('max', c(3,1,3),matrix(c(-1, 0, 1, 2, 4, -3, 1, -3, 2), nrow = 3), c("<=", "<=", "<="), c(4,2,3), int.vec=c(1,3))
optim$solution

head(dense.const, 100)




##################### collection channel optimization Apr. 18, 2011 #############################
setwd('d:/temp')
opt_raw <- as.matrix(read.csv('opt_raw.csv')[,4:7])
require(lpSolve)

# channels are sc, sms and call, or 1,2, 3
N <- 6267
# randomly pick 60% accounts
idx <- runif(N)
portion <- 0.05
objective.in <- as.vector(t(opt_raw[idx<=portion, 2:4])) * (rep(opt_raw[idx<=portion, 1], each = 3))

NN <- sum(idx <= portion)

bin.const <- matrix(0, NN, NN*3)
bin.const[cbind(rep(1:NN, each=3), 1:(NN*3))] <- 1

sms.const <- (1:NN) * 3 - 1
call.const <- (1:NN) * 3

trt.const <- matrix(0, 2, NN*3)
trt.const[1, ((1:NN) * 3 - 1)] <- 1
trt.const[2, ((1:NN)*3)] <- 1

c.dir <- c(rep('=', NN), '<=', '<=')
c.rhs <- c(rep(1,NN), NN*0.5, NN*0.2)

optim <- lp ("max", objective.in, rbind(bin.const, trt.const), c.dir, c.rhs, all.bin = T)

# in dense format
bin.const.dense <- matrix(c(rep(1:NN, each = 3), rep(NN+1, NN), rep(NN+2, NN), 1:(3*NN), seq.int(2, 3*NN, by = 3), seq.int(3, 3*NN, by = 3), rep(1, NN*5)), ncol = 3)
c.dir.dense <- c(rep('=', NN), rep('<=', 2))
c.rhs.dense <- c(rep(1, NN), NN*0.5, NN*0.2)
optim.dense <- lp('max', objective.in, ,c.dir.dense, c.rhs.dense, dense.const = bin.const.dense, all.bin=T)
