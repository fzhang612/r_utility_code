
setwd('d:/temp')
se <- read.csv('sedor201005Qual.csv')
contents(se)


# packages
setInternet2(T)
install.packages('SJava', repos = "http://www.omegahat.org/R", type = "source")

install.packages('JavaGD',dependencies = 'Depends')
update.packages(ask = T)
old.packages()

library(tableplot)
require(Hmisc)
require(nlme)
require(plyr)

# help
library(help = 'tableplot')
?sas.get
??sas.get


# data
data(Oxboys, package = 'nlme')

str(Oxboys)
head(Oxboys)




tax <- function(x) {
	taxable <- x - 2000 - (855.9 + 214 + 107 + 807)
	if(x < 500) tax <- taxable * 0.05
	if(500 <= x && x < 2000) tax <- taxable * 0.1 - 25
	if(2000 <= x && x < 5000) tax <- taxable * 0.15 - 125
	if(5000 <= x && x < 20000) tax <- taxable * 0.2 - 375
	if(20000 <= x && x < 40000) tax <- taxable * 0.25 - 1375
	if(40000 <= x && x < 60000) tax <- taxable * 0.3 - 3375
	if(60000 <= x && x < 80000) tax <- taxable * 0.35 - 6375
	if(80000 <= x && x < 100000) tax <- taxable * 0.4 - 10375
	if(100000 <= x) tax <- taxable * 0.45 - 15375

	return(tax)
}
tax(48592)

(tax(47570+159876/4) - tax(47570))*4

# SALES with error

IV <- function(x, y, g = 10) { 
	if (is.numeric(x)) { 
		breaks <- quantile(x, probs = seq(0, 1, 1/g), na.rm=T)
		breaks <- c(breaks[1], breaks[-1][!diff(breaks)==0])
		if (length(breaks) > 2 ) cutx <- cut(x, breaks = breaks, include.lowest=T)
		else cutx <- x
	}
	else cutx <- x
	
	t <- table(y, cutx, useNA='ifany')
	t1 <- t[1,]/sum(t[1,])
	t1[t1==0] <- 0.00001	
	t2 <- t[2,]/sum(t[2,])
	t2[t2==0] <- 0.00001
	IV = sum((t1-t2)*log(t1/t2))
}


BatchIV <- function(df, y, g=10){
	IVList <- data.frame()
	for (i in 1:ncol(df)) 
		IVList <- rbind(IVList, data.frame(Var = colnames(df)[i], IV = IV(df[,i], y, g)))
	return(IVList)
}
l <- BatchIV(longco, longco$resp, g=20)
l[order(-l$IV),]



IVChart <- function(mydf, xx, yy) {
	ggplot(data = mydf) + geom_histogram(aes_string(x = xx, fill = factor(yy)), position='fill', alpha=0.5)
}

IVChart(longco, 'co2mob', resp)

drawVar <- function(x, y, type = 'CAT', include.missing = T, g = 10) {
	require(ggplot2)
	firstarg <- strsplit(gettext(match.call())[2], '$', fixed = T)
	VarName <- firstarg[[1]][length(firstarg[[1]])]
	
	if( type == 'CON') {
		breaks <- quantile(x, probs = seq(0, 1, 1/g), na.rm=T)
		breaks <- c(breaks[1], breaks[-1][!diff(breaks)==0])
		if (length(breaks) > 2 ) cutx <- cut(x, breaks = breaks, include.lowest=T)
		else cutx <- x
	}
	if (type == 'CAT') cutx <- x

	df1 <- as.data.frame(table(cutx, useNA='ifany'))
	colnames(df1)[1:2] <- c('VarValue', 'ToPlot')
	df1$panel <- 'Count'
	if(!include.missing) df1 <- na.omit(df1)
	#print(df1)
	df2 <- ddply(data.frame(cutx, y), .(cutx), summarize, ToPlot = mean(y))
	df2[,1]<-as.factor(df2[,1])
	colnames(df2)[1:2] <- c('VarValue', 'ToPlot')
	df2$panel <- 'Rate'
	if(!include.missing) df2 <- na.omit(df2)
	#print(df2)
	df.big <- rbind(df1, df2)
	#print(df.big)

	# to understand grouping and layer
	p <- ggplot(data = df.big, mapping = aes(x = VarValue, y = ToPlot))
	p <- p + facet_grid(panel ~ ., scale = "free")
	p <- p + layer(data = df1, geom = "bar", stat = "identity")
	p <- p + layer(data = df2, geom = 'line', mapping = aes(group = 2))
	print(p + xlab('') + ylab('') + opts(title = VarName))
}
drawVar(longco$currbal, longco$resp, include.missing = F, type = 'CON', g=5)


df1 <- data.frame(x=1:5, y=c(100, 200, 300, 200, 100), panel=rep('bar'))
df2 <- data.frame(x=1:5, y=c(100, 200, 300, 200, 100)/900, panel=rep('rate'))
df.big <- rbind(df1,df2)
p <- ggplot(df.big, aes(x, y)) + facet_wrap(panel ~., scale='free', drop=F)
p1 <- p + geom_line(data=df2)
p2 <- p1 + geom_bar(data=df1, stat='identity')


KS.group <- function(y, grp){
	require(ggplot2)
	# y<-df$y; grp<-df$grp
	KStable <- apply(prop.table(table(y, grp), 1) * 100, 1, cumsum)
	KS <- max(abs(KStable[,1] - KStable[,2]))
	p <- ggplot(melt(KStable), aes(x=Var.1, y=value, color=factor(y)))+geom_line() 
	p <- p + ylab('Cum. Percentage') + xlab('Group') + labs(colour='Y') + opts(title = 'KS Chart')
	print(p)
	return(list(KStable = KStable, KS = KS))
}



dir()

# require(foreign)
# sashome <- "c:/Program Files/SAS/SAS 9.1"
# longco <- read.ssd('J:/vicky/projects/longco_list', 'longco', sascmd = file.path(sashome, "sas.exe"))
step4 <- read.csv('d:/temp/step4_base.csv',na.strings = c('NA', ''))
#step3 <- sas.get(lib = 'd:/temp', mem = 'step3_base')
require(Hmisc)
contents(step4)
describe(step4)

BatchIV(longco, longco$resp, 10)


# To understand how R functions work
# Do parameters pass value or reference? - by value!!!
mydf <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
test <- function(x, y) {
	x <- x * (-1)
	y <- y * (-1)
}
attach(mydf)
test(x, y)
detach(mydf)
mydf
# End


# Painless way to install new version of R

#--run in the old version of R 
setwd("C:/Temp/") 
packages <- installed.packages()[,"Package"] 
save(packages, file="Rpackages") 

# Followed by this in the new version:

#--run in the new version 
setwd("C:/Temp/") 
load("Rpackages") 
for (p in setdiff(packages, installed.packages()[,"Package"])) 
install.packages(p) 

# End


# ggplots samples
library(ggplot2)
# sample 1 -- polar plot
DF <- data.frame(variable = LETTERS[1:10], value = sample(10, replace = TRUE))
ggplot(DF, aes(factor(variable), value, fill=factor(variable))) + geom_bar(width = 1)
last_plot() + scale_y_continuous(breaks = 0:10) 
last_plot() + coord_polar() 
last_plot() + labs(x = "", y = "") 
last_plot() + opts(legend.position = "none", axis.text.x = theme_blank(), axis.text.y = theme_blank(), axis.ticks = theme_blank())

# adding gridline
DF <- ddply(DF, .(variable), transform, border = rep(1, value))
ggplot(DF, aes(factor(variable))) + geom_bar(width = 1, aes(y = value, fill = factor(variable)))
last_plot() + geom_bar(aes(y = border, width = 1), position = "stack", stat = "identity", fill = NA, colour = "white") 
last_plot() + scale_y_continuous(breaks = 0:10) + coord_polar() 
last_plot() + labs(x = "", y = "") 
last_plot() + opts(legend.position = "none", axis.text.x = theme_blank(), axis.text.y = theme_blank(), axis.ticks = theme_blank())

# sample 2 - waterfall
balance <- data.frame(desc = c("Starting Cash", "Sales", "Refunds", "Payouts", "Court Losses", "Court Wins", "Contracts", "End Cash"), 
	amount = c(2000, 3400, -1100, -100, -6600, 3800, 1400, 2800))

balance$desc <- factor(balance$desc, levels = balance$desc)
balance$id <- seq_along(balance$amount)
balance$type <- ifelse(balance$amount > 0, "in", "out")
balance[balance$desc %in% c("Starting Cash", "End Cash"), "type"] <- "net"	
balance$end <- cumsum(balance$amount)
balance$end <- c(head(balance$end, -1), 0)
balance$start <- c(0, head(balance$end, -1))
balance <- balance[, c(3, 1, 4, 6, 5, 2)]

ggplot(balance, aes(desc, fill = type)) + geom_rect(aes(x = desc, xmin = id - 0.45, xmax = id + 0.45, ymin = end, ymax = start))

balance$type <- factor(balance$type, levels = c("out","in", "net"))
strwr <- function(str) gsub(" ", "\n", str)

p1 <- ggplot(balance, aes(fill = type)) + geom_rect(aes(x = desc, xmin = id - 0.45, xmax = id + 0.45, ymin = end, 
	ymax = start)) + scale_y_continuous("", formatter = "comma") + scale_x_discrete("", breaks = levels(balance$desc),
    labels = strwr(levels(balance$desc))) + opts(legend.position = "none")

p1 + geom_text(subset = .(type == "in"), aes(id, end, label = comma(amount)), vjust = 1, size = 3) 
last_plot() + geom_text(subset = .(type == "out"), aes(id, end, label = comma(amount)), vjust = -0.3, size = 3) 
last_plot() + geom_text(data = subset(balance, type == "net" & id == min(id)), aes(id, end, colour = type, label = comma(end), vjust = ifelse(end <
	start, 1, -0.3)), size = 3.5) 
last_plot() + geom_text(data = subset(balance, type == "net" & id == max(id)), aes(id, start, colour = type, label = comma(start), vjust = ifelse(end <
	start, -0.3, 1)), size = 3.5)
	
# sample 3 - Changing the Ordering of Legend Labels

ggplot(diamonds, aes(clarity, fill = cut)) + geom_bar()
diamonds$cut <- factor(diamonds$cut, levels = rev(levels(diamonds$cut)))
ggplot(diamonds, aes(clarity, fill = cut)) + geom_bar()
ggplot(diamonds, aes(clarity, fill = cut, order = -as.numeric(cut))) + geom_bar()

# sample 4 - Plotting Dates, Hours and Minutes
library(maptools)
x_seq <- seq(from = as.POSIXct("2010-01-01", tz = "GMT"), length.out = 365, by = "days")
coord <- matrix(c(-0.13, 51.5), nrow = 1) 
sunrise <- sunriset(coord, x_seq, direction = "sunrise", POSIXct.out = TRUE)
sunrise$hms <- format(sunrise$time, format = "%H:%M:%S")
sunrise$hms <- as.POSIXct(sunrise$hms, format = "%H:%M:%S")

ggplot(sunrise, aes(time, hms)) + geom_line() + opts(axis.text.x=theme_text(angle = 60, hjust = 1.2 ))
last_plot() + scale_x_datetime("", format = "%b") + ylab("")

# sample 5 - directlabels: Adding direct labels to ggplot2 and lattice plots
http://learnr.wordpress.com/2010/01/03/directlabels-adding-direct-labels-to-ggplot2-and-lattice-plots/

# sample 6 - Data Profiling in R
http://learnr.wordpress.com/2009/12/17/data-profiling-in-r/

# change axis font
# https://github.com/hadley/ggplot2/wiki/Axis-Attributes
opts(axis.text.x = theme_text(colour = 'red', angle = 45, size = 10, hjust = -3, vjust = 7, face = 'italic'))



library(ggplot2)
p <- ggplot(data = step4[step4$group2=='ALL',], aes(x = B_age2)) + stat_bin(aes(y = ..density..), geom = 'bar') + xlab('Customer Cluster') 

p <- ggplot(data = step3, aes(x = group2, y = district18)) + stat_bin2d(aes(fill = ..count..), geom = 'tile') + scale_fill_gradient(
	low="white", high="steelblue", breaks = seq(0, 20000, by=4000)) + xlab('Customer Cluster') + opts(title = 'District Distribution by Cluster')

step3c <- melt(step3[,c(8:11, 14:15)], 'group2')


p <- ggplot(data = step3, aes(x = M_HH_M_RAT)) + geom_density(aes(color = group2), size = 1.2) + ylab('') + opts(title = 'long name')
p <- ggplot(data = step3, aes(x = MDN_HH_RENT_RATIO)) + geom_density(aes(color = group2), size = 1.2) + ylab('') + opts(title = 'long name')
p <- ggplot(data = step3, aes(x = MTH_HH_INC_2n)) + geom_density(aes(color = group2), size = 1.2) + ylab('') + opts(title = 'long name')
p <- ggplot(data = step3[step3$CPV>0,], aes(x = CPV)) + geom_density(aes(color = group2), size = 1.2) + ylab('') + opts(title = 'long name')
p <- ggplot(data = step3, aes(x = group2, fill = factor(AVG_HH_SIZE))) + geom_bar() + xlab('Customer Cluster') + opts(title = 'Household Size
 Distribution by Cluster')

ggplot(data=step3, aes(x=group2, y=DISTRICT)) + stat_sum(aes(group=group2), geom='point')
ggplot(data=step3, aes(x=DISTRICT, color=group2)) + stat_bin(aes(y = ..count..), geom='point')

p <- ggplot(data = step4, aes(x = group2, y = district18)) + geom_tile(aes(size = ..prop.., fill = ..prop.., group = group2), stat = 'sum') 
p
p + scale_fill_gradient(low = 'white', high = 'steelblue', formatter = 'percent') + scale_size(legend = F)
 

p1 <- ggplot(data = step4, aes(x = group2, y = district18))

#p <- ggplot(data = step3c, aes(x = value, color = group2)) + geom_density() + facet_wrap(~variable , ncol = 1, scales = 'free')
p

df = expand.grid(x=letters[1:10], y=LETTERS[1:10])
