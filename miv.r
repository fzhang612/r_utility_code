# miv and mivbatch assume
# 1) DV = 0 or 1, while 1 means response
# 2) score is actually the predicted probability
# 3) arguments n and brks only work for continuous variables. n is # of groups,
#    user can specify how to group by brks
# 4) can be used to calculate regular IV. just set parameter score = population response rate vector


mivbatch <- function(x, y, score, n = 10) {
	lapply(x, miv, y = y, score = score, n = n)
}

print.top.miv <- function (mivlist, n = 20) {
  mivlist.null <- sapply(mivlist, is.null)
  if (length(which(mivlist.null)) > 0) tmplist <- mivlist[-which(mivlist.null)]
  else tmplist <- mivlist
  
	mivsummary <- t(sapply(tmplist, function(t) t[dim(t)[1], c(3, 8:10)]))
	mivsummary <- mivsummary[order(mivsummary[,2], decreasing = T), ]
	head(mivsummary, n = min(n, dim(mivsummary)[1]))
}


miv <- function(x, y, score, n = 10, brks = NULL, show.plot = F){

	require(classInt)
	WoEadj <- 0.5
  
  if(length(unique(x)) == 1) {
    t <- NULL
    return(t)
  }

		
	if(is.numeric(x) && (any(is.na(x)) && length(unique(x[-which(is.na(x))])) > 1 || !any(is.na(x)))) {
		if (is.null(brks)) cutx <- cut(x, breaks = classIntervals(x, n, style = 'pretty')$brks, include.lowest = T)
		else cutx <- cut(x, breaks = brks, include.lowest = T)
		}
	else cutx <- x
	 
	t <- table(cutx, y, useNA = 'ifany')
	colnames(t) <- paste('actual', colnames(t))

  poplnodds <- log((sum(y == 1) + WoEadj) / (sum(y == 0) + WoEadj))
	
  t <- cbind(t,log((t[,2] + WoEadj) /(t[,1] + WoEadj)) - poplnodds)
	colnames(t)[3] <- 'actual WoE'

	# tmp <- aggregate(score, list(cutx), mean)[2]
	if(sum(is.na(cutx)) > 0) cutx <- addNA(cutx, ifany = T)
#   browser()
  tmp <- tapply(score, cutx, mean)

	t <- cbind(t,  (1- tmp) * rowSums(t[,1:2]), tmp * rowSums(t[,1:2]))
	t <- cbind(t,log((t[,5] + WoEadj) /(t[,4] + WoEadj)) - poplnodds)
	colnames(t)[4:6] <- c('exp 0', 'exp 1', 'exp WoE')

	t <- cbind(t, t[,3] - t[,6])
	colnames(t)[7] <- 'delta score'

	t <- cbind(t, (t[,2]/sum(t[,2]) - t[,1]/sum(t[,1])) * t[,7], (t[,1] - t[,4])^2 / t[,4] + (t[,2] - t[,5])^2 / t[,5])
  t <- cbind(t, pchisq(t[,9], 1, lower.tail = F))
	colnames(t)[8:10] <- c('miv', 'Chitest', 'pvalue')

	chitest <- sum(t[,9], na.rm = T)
	pvalue <- pchisq(chitest, dim(t)[1] - 1, lower.tail = F)
  
  iv <- sum(t[,2] * t[,3])/sum(t[,2]) - sum(t[,1] * t[,3])/sum(t[,1])
  eiv <- sum(t[,5] * t[,6], na.rm = T)/sum(t[,5], na.rm = T) - sum(t[,4] * t[,6], na.rm = T)/sum(t[,4], na.rm = T)

	t <- rbind(t, c(colSums(t[,1:2]), iv, colSums(t[,4:5], na.rm = T), eiv, NA, sum(t[,8], na.rm = T), chitest, pvalue))
	rownames(t)[dim(t)[1]] <- 'overall'
  
  if(show.plot) {
    y.lower = min(min(t[-dim(t)[1], 3], na.rm = T), min(t[-dim(t)[1], 6], na.rm = T))
    y.upper = max(max(t[-dim(t)[1], 3], na.rm = T), max(t[-dim(t)[1], 6], na.rm = T))
    plot(t[-dim(t)[1], 3], type = 'l', col = 'red', ylim = c(y.lower, y.upper))
    points(t[-dim(t)[1], 6], type = 'l', col = 'blue')
    points(rep(0, dim(t)[1]-1), type = 'l', lty = 2)
    title('red - Actual WoE, blue - Expected WoE')
  }
	
	return(t)

}

# to get optimal split for a continuous variable
# x - variable to be analyzed
# y - dependent variable (non-binary case not tested yet)
# brks - original break vector including -Inf and Inf
# p - p-value threshold

optimal.brks <- function (x, y, brks, p = 0.05) {
  
  cutx <- cut(x, breaks = brks, include.lowest = T)
  t <- table(cutx, y, useNA = 'ifany')
  
  chitest <- rep(NA, length(brks)-2)
  p.values <- rep(NA, length(brks)-2)
  
  for(i in c(1:length(brks)-2)) {
    cht <- chisq.test(t[i:(i+1), ], correct = F)
    chitest[i] <- cht$statistic
    p.values[i] <- cht$p.value
  }
  
  if (max(p.values, na.rm = T) > p) {
    optimal.brks(x, y, brks[-(which(chitest == min(chitest, na.rm = T)) + 1)], p)
  }
  else return(list(chitest = chitest, p.values = p.values, brks = brks))
#   return(list(chitest = chitest, p.value = p, brks = brks))


optimal.brks(ablow$age, ablow$response, c(-Inf, 30, 40, 50, 60, Inf))

vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y) 
arrange <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) { 
  dots <- list(...)  
  n <- length(dots)  
  if(is.null(nrow) & is.null(ncol)) { 
    nrow = floor(n/2)
    ncol = ceiling(n/nrow)
  }  
  if(is.null(nrow)) nrow = ceiling(n/ncol)  
  if(is.null(ncol)) ncol = ceiling(n/nrow)         
  ## NOTE see n2mfrow in grDevices for possible alternative 
  grid.newpage() 
  pushViewport(viewport(layout=grid.layout(nrow,ncol)) )  
  ii.p <- 1  
  for(ii.row in seq(1, nrow)){  
    ii.table.row <- ii.row   
    if(as.table) ii.table.row <- nrow - ii.table.row + 1   
    for(ii.col in seq(1, ncol)){    
      ii.table <- ii.p    
      if(ii.p > n) break    
      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))    
      ii.p <- ii.p + 1   
    }  
  } 
} 


data(diamonds)   
# Create a histogram, assign to "plot1" 
plot1 <- qplot(price,data=diamonds,binwidth=1000)   
# Create a scatterplot 
plot2 <- qplot(carat,price,data=diamonds)   
# Arrange and display the plots into a 2x1 grid 
arrange(plot1,plot2,ncol=1) 




##############################################################
# cut continuous variable into groups and labels by WoE
#############################################################

cut.cont <- function(x, y, g = 10, WoE = T) {
  require(Hmisc)
  tmp.factor <- cut2(x, g = g)
  tmp.table <- table(tmp.factor, y)
  if (Woe) {
    cut.x <- tmp.factor
    levels(cut.x) = log(tmp.table[,2]/tmp.table[,1]) - log(mean(y)/(1-mean(y)))
  }
  
  cut.x
}


table(cut.cont(ablow$age, ablow$response, WoE = T))

