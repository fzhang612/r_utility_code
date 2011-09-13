# multivariate logistic regression 

set.seed(12345)

x1 <- rnorm(100)
x4 <- rnorm(100)
x7 <- rnorm(100)
x2 <- runif(100,-1,1)
x5 <- runif(100,-1,1)
x8 <- runif(100,-1,1)
x3 <- rnorm(100,0,2)
x6 <- rnorm(100,0,2)
x9 <- rnorm(100,0,2)
x10 <- rnorm(100,0,2)

y1 <- factor(rbinom(100,1,1/(1+exp(-.1*x1-.4*x4-.7*x7))))
y2 <- factor(rbinom(100,1,1/(1+exp(-.2*x2-.5*x5-.8*x8))))
y3 <- factor(rbinom(100,1,1/(1+exp(-.3*x3-.6*x6-.9*x9))))

mydata <- data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,y1,y2,y3)
xdata <- data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
ydata <- data.frame(y3,y2,y1)

var.selection <- data.frame(matrix(0, 10, 3))
dimnames(var.selection)[[1]] <- names(xdata)
dimnames(var.selection)[[2]] <- names(ydata)

var.selection[,1] <- 1
var.selection[2:8,2] <- 1
var.selection[1:5,3] <- 1



mylog <- function(response, ydata, xdata, var.selection) {
  training_data <- data.frame(ydata[response],xdata[,var.selection[response]==1])
  glm <- glm(formula(training_data), data = training_data, family = binomial)
  print(response)
  
  coeff <- rep(NA, dim(xdata)[2])
  names(coeff) <- names(xdata)
  coeff[var.selection[response]==1] <- glm$coefficients[-1]
  coeff <- c(glm$coefficients[1], coeff)
  
  list(response = response, fitted.values = glm$fitted.values, coeff = coeff, coeff.summary = summary(glm)$coefficients)
  
  #list(response = response, fitted.values = glm$fitted.values, coeff = glm$coefficients, coeff.summary = summary(glm)$coefficients)
}

y.names <- names(ydata)
names(y.names) <- y.names

glm_list <- lapply(y.names, mylog, xdata = xdata, ydata = ydata, var.selection)


myks.test <- function(x, ydata) {
  # ks <- ks.test(x$fitted.values[x$data$response==1], x$fitted.values[x$data$response==0])
  #ks <- ks.test(x$fitted.values[x$response==1], x$fitted.values[x$response==0])
  ks <- with(x, ks.test(fitted.values[ydata[response] == 1], fitted.values[ydata[response] == 0]))
  c(ks = ks$statistic, p = ks$p.value)
}


sapply(glm_list, myks.test, ydata = ydata)


# show all coefficients for all/selected models
sapply(glm_list, function(x) x$coeff)
sapply(glm_list[c('y1','y3')], function(x) x$coeff)

# show selected coeffients for all/selected models
sapply(glm_list, function(x, var) x$coeff[var], var = c('x1','x3') )
sapply(glm_list[c('y1','y3')], function(x, var) x$coeff[var], var = c('x1','x3') )


# show all coefficients and tests for all/selected models
sapply(glm_list, function(x) x$coeff.summary)
sapply(glm_list[c('y1','y3')], function(x) x$coeff.summary)

# show selected coefficients and tests for all/selected models
# make sure the selected variable exists in all models !!!!!!!!!!!!!!!!!!
sapply(glm_list, function(x, var) x$coeff.summary[var,], var = c('x3'))




#########################################################################################

# test difference between partial and full model

n <- 1000000
xx1 <- sample(c(0,1), n, T, c(0.99,0.01))
xx2 <- runif(n)
xx3 <- rnorm(n)

y <- rbinom(n, 1, 1/(1+exp(-(1.8*xx1 - 0.4*xx2 + 0.5*xx3 ))))
table(y)
table(y, xx1)

dat <- as.data.frame(cbind(xx1,xx2,xx3,y))

partial.model <- glm(y~xx1+xx2+xx3, dat[xx1==1,], family=binomial)
full.model <- glm(y~xx1+xx2+xx3, dat, family = binomial)

plot(partial.model$fitted.values, full.model$fitted.values[xx1==1], xlab='partial model', ylab = 'full model')
lines(partial.model$fitted.values,partial.model$fitted.values, col='red')






