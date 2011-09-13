
##############################################
### Implementation of the Pareto/NBD model ###
##############################################


### Log-Likelihood for Pareto/NBD

paretoLlh <- function(data, r, alpha, s, beta) {
  library(hypergeo) # by me
  
  if (s<=0 | beta<=0 | r<=0 | alpha<=0) return (NaN)
  maxab <- max(alpha, beta)
  absab <- abs(alpha-beta)
  param2 <- s+1
  if (alpha < beta) param2 <- r+data$x
  part1 <- (alpha^r * beta^s / gamma(r)) * gamma(r+data$x)
  part2 <- 1 / ((alpha+data$T)^(r+data$x)*(beta+data$T)^s)
  if (absab == 0) {
    F1 <- 1 / ((maxab+data$tx)^(r+s+data$x))
    F2 <- 1 / ((maxab+data$T)^(r+s+data$x))
  } else {
    # F1 <- h2f1(r+s+data$x, param2, r+s+data$x+1, absab / (maxab+data$tx)) / ((maxab+data$tx)^(r+s+data$x))
    # F2 <- h2f1(r+s+data$x, param2, r+s+data$x+1, absab / (maxab+data$T)) / ((maxab+data$T)^(r+s+data$x))
    
    F1 <- as.double(hypergeo(r+s+data$x, param2, r+s+data$x+1, absab / (maxab+data$tx)) / ((maxab+data$tx)^(r+s+data$x)))
    F2 <- as.double(hypergeo(r+s+data$x, param2, r+s+data$x+1, absab / (maxab+data$T)) / ((maxab+data$T)^(r+s+data$x)))
  }
  f <- -sum(log(part1*(part2+(s/(r+s+data$x))*(F1-F2))))
  return (f)
}


### Estimate Parameters via MLE for Pareto/NBD

paretoEstimateParametersMle <- function(data, initValues, safeMode=FALSE) {
  llhd <- function(r, alpha, s, beta) {
    return (paretoLlh(data, r, alpha, s, beta))
  }
  library(bbmle)
  if (safeMode) {
    fit <- mle2(llhd, initValues, skip.hessian=TRUE, method="Nelder-Mead")
  } else {
    fit <- mle2(llhd, initValues)
  }
  return (fit)
}


### Estimate Parameters via Two-Step Estimation of Moments for Pareto/NBD

paretoEstimateParameters2Step <- function(actualMeanX, actualVarX, ts) {

  # step 1
  estEX <- function(ralpha,s,beta,T) {
    return ((ralpha*beta/(s-1))*(1-(beta/(beta+T))^(s-1)))
  }
  fn1 <- function(par) {
    ralpha <- par[1]
    s <- par[2]
    beta <- par[3]
    if (min(ralpha,s,beta)<=0) return (NaN)
    return (sum((estEX(ralpha,s,beta,ts) - actualMeanX)^2))
  }
  optPar <- optim(par=c(0.5,0.5,0.5), fn=fn1)$par
  ralpha <- optPar[1]
  s <- optPar[2]
  beta <- optPar[3]

  # step 2 (enhanced according to Krafft 2002, resp. wuebben/wangenheim)
  estVarX <- function(r,alpha,s,beta,T) {
    term1 <- estEX(r/alpha,s,beta,T)
    term2 <- term1^2
    term3 <- ((2*r*(r+1)*beta)/((alpha^2)*(s-1))) * ((beta/(s-2)) - (beta/(s-2))*(beta/(beta+T))^(s-2) - T*(beta/(beta+T))^(s-1))
    return (term1-term2+term3)
  }
  fn2 <- function(par) {
    alpha <- par[1]
    return (sum((estVarX(ralpha*alpha,alpha,s,beta,ts) - actualVarX)^2))    
  }
  alpha <- optimize(interval=c(0,10^4), f=fn2)$minimum
  r <- ralpha*alpha

  return (list(r=r,alpha=alpha,s=s,beta=beta))
}


### Compute P(active at T|x,tx,T), i.e. the conditional individual probability of still being active at time T, for Pareto/NBD

paretoComputePActive <- function(data, params) {

  maxab <- max(params$alpha,params$beta)
  absab <- abs(params$alpha-params$beta)
  param2 <- params$s+1
  if (params$alpha < params$beta) param2 <- params$r+data$x

  F0 <- (params$alpha+data$T)^(params$r+data$x)*(params$beta+data$T)^params$s

  #F1 <- h2f1(params$r+params$s+data$x, param2, params$r+params$s+data$x+1, absab / (maxab+data$tx)) / ((maxab+data$tx)^(params$r+params$s+data$x))
  #F2 <- h2f1(params$r+params$s+data$x, param2, params$r+params$s+data$x+1, absab / (maxab+data$T)) / ((maxab+data$T)^(params$r+params$s+data$x))
  
  F1 <- as.double(hypergeo(params$r+params$s+data$x, param2, params$r+params$s+data$x+1, absab / (maxab+data$tx)) / ((maxab+data$tx)^(params$r+params$s+data$x)))
  F2 <- as.double(hypergeo(params$r+params$s+data$x, param2, params$r+params$s+data$x+1, absab / (maxab+data$T)) / ((maxab+data$T)^(params$r+params$s+data$x)))
  pactive <- 1 / (1 + (params$s / (params$r+params$s+data$x)) * F0 * (F1-F2))

  return (pactive)
}


### Compute E[Y(T,T+t)|x,tx,T], i.e. the conditional individual forecast for each customer, for Pareto/NBD

paretoConditionalForecast <- function(data, params, t) {
  if (class(params)[1] == "mle2") params <- as.list(coef(params))
  tmp1 <- (params$r+data$x) * (params$beta+data$T) / ((params$alpha+data$T) * (params$s-1))
  tmp2 <- ((params$beta+data$T) / (params$beta+data$T+t))^(params$s-1)
  ce <- tmp1 * (1-tmp2) * paretoComputePActive(data, params)
  return (ce)
}


### Compute P(X(t)=x), i.e. the unconditional probability distribution of purchase frequencies, for Pareto/NBD

paretoPx <- function(t,x,params) {
  with(params, {
    abmax <- max(alpha, beta)
    xterm <- ifelse(beta>alpha, r+x, s+1)
    term1 <- choose(x+r-1, x) * (alpha / (alpha+t))^r * (t / (alpha+t))^x * (beta / (beta+t))^s
    sterm <- 0
    for (j in 0:x) {
      term2 <- choose(x, j) * (-abmax)^j * (s/(s+r+j)) * alpha^r * beta^s
      term3 <- abmax^(-(r+j+s)) * h2f1(xterm, r+j+s, r+j+s+1, ifelse(beta>alpha, -1, 1) * (alpha-beta) / abmax)
      term4 <- (abmax + t)^(-(r+j+s)) * h2f1(xterm, r+j+s, r+j+s+1, ifelse(beta>alpha, -1, 1) * (alpha-beta) / (abmax+t))
      sterm <- sterm + term2 * (term3 - term4)
    }
    out <- term1 + choose(x+r-1, x) * sterm
    return (out)
  })
}
