setwd('d:\\temp')
hkh <- read.csv('hkhousing.csv')
summary(hkh)

library(arm)
corrplot(hkh[,3:8])
pairs(hkh[,3:10])

pca <- princomp(hkh[,3:10], cor = T)
# pca <- prcomp(hkh[,3:8], scale. = T, retx = T)
# summary(pca)
print(pca)
plot(pca)
screeplot(pca)
biplot(pca)
loadings(pca)
pca$scores[,1] <- pca$scores[,1] * (-1)

cl <- kmeans(hkh[,3:8], centers = 9)
print(cl)
plot(hkh[,3:8], col = cl$cluster)

color <- hcl(seq(10, by = 35, length = 14) %% 360)
year <- 1999
library(ggplot2)
ggplot(data = as.data.frame(pca$scores[hkh$year <= year, 1:2]), aes(x = Comp.1, y=Comp.2, colour = factor(hkh[hkh$year <= year, 1]))) + geom_point(size = 5) + scale_colour_manual(values = color)
ggplot(data = as.data.frame(pca$scores[hkh$year <= year, c(1,3)]), aes(x = Comp.1, y=Comp.3, colour = factor(hkh[hkh$year <= year, 1]))) + geom_point(size = 5) + scale_colour_manual(values = color)
ggplot(data = as.data.frame(pca$scores[hkh$year <= year, c(2,3)]), aes(x = Comp.2, y=Comp.3, colour = factor(hkh[hkh$year <= year, 1]))) + geom_point(size = 5) + scale_colour_manual(values = color)

library(scatterplot3d)
s3dcolor <- rep(hcl(runif(14, 0, 360)), each = 12)
s3dpch <- rep(1:14, each = 12)
scatterplot3d(pca$scores[,1:3], col.axis = "blue", col.grid = "lightblue", pch = 20, color = s3dcolor[-168], type = 'h', cex.symbols = 3)


library(animation)
setwd('d:\\temp\\anitest')
oopt = ani.options(ani.dev = "pdf", ani.type = "pdf", ani.height = 600, ani.width = 600)
saveLatex({
	for (year in 1997:2010) {
		p <- ggplot(data = as.data.frame(pca$scores[hkh$year <= year, 1:2]), aes(x = Comp.1, y=Comp.2, colour = factor(hkh[hkh$year <= year, 1]))) 
		p <- p + geom_point(size = 5) + scale_colour_manual(name = 'YEAR', values = color) 
		p <- p + opts(title = 'HK Property Market Movement 1997-2010 (Comp 1 VS 2)', axis.text.x = theme_blank(), axis.text.y = theme_blank()) 
		p <- p + scale_x_continuous(name = 'Comp 1 - Overall', limits = c(-3, 8)) + scale_y_continuous(name = 'Comp 2', limits = c(-4, 3))
		print(p)
		}
    }, ani.basename = "comp12", latex.filename = "comp12ani.tex", interval = 3)
	
saveLatex({
	for (year in 1997:2010) {
		p <- ggplot(data = as.data.frame(pca$scores[hkh$year <= year, c(1:3)]), aes(x = Comp.1, y=Comp.3, colour = factor(hkh[hkh$year <= year, 1]))) 
		p <- p + geom_point(size = 5) + scale_colour_manual(name = 'YEAR', values = color) 
		p <- p + opts(title = 'HK Property Market Movement 1997-2010 (Comp 1 VS 3)', axis.text.x = theme_blank(), axis.text.y = theme_blank()) 
		p <- p + scale_x_continuous(name = 'Comp 1 - Overall', limits = c(-3, 8)) + scale_y_continuous(name = 'Comp 3', limits = c(-3, 4))
		print(p)
		}
    }, ani.basename = "comp13", latex.filename = "comp13ani.tex", interval = 3)
	
saveLatex({
	for (year in 1997:2010) {
		p <- ggplot(data = as.data.frame(pca$scores[hkh$year <= year, c(2:3)]), aes(x = Comp.2, y=Comp.3, colour = factor(hkh[hkh$year <= year, 1]))) 
		p <- p + geom_point(size = 5) + scale_colour_manual(name = 'YEAR', values = color) 
		p <- p + opts(title = 'HK Property Market Movement 1997-2010 (Comp 2 VS 3)', axis.text.x = theme_blank(), axis.text.y = theme_blank()) 
		p <- p + scale_x_continuous(name = 'Comp 2', limits = c(-4, 3)) + scale_y_continuous(name = 'Comp 3', limits = c(-3, 4))
		print(p)
		}
    }, ani.basename = "comp23", latex.filename = "comp23ani.tex", interval = 3)

ani.options(oopt)


df <- melt(pca$loadings[,1:3]) 
df[df$X2 == 'Comp.1',3] <- df[df$X2 == 'Comp.1',3] * (-1)
dev.new()
ggplot(data = df, aes(x = X1, y = value, fill = X2)) + geom_bar(position = 'dodge', width = 0.7)  + coord_polar(start = pi/4) + labs(x = '', y = '', fill = 'Components')
ggplot(data = df, aes(x = X1, y = value, colour = X2, group = X2)) + geom_line()  + coord_polar()

first4vars <- 1:672
second4vars <- 673:1344
hkhlong <- melt(hkh, id.var = c('year', 'yearmth'))
head(hkhlong)
ggplot(data = hkhlong[hkhlong$variable %in% levels(hkhlong$variable)[-c(1,3,6,8)],], aes(x = as.Date(as.character(paste(yearmth, '01', '')), '%Y%m%d'))) +  
	geom_path(aes(y = value), group = 1) + 
#	geom_rect(aes(xmin = as.Date('1997-01-01'), xmax = as.Date('1997-12-01'), ymin = min(value), ymax = max(value)), colour = 'red', fill = 'red', alpha = 0.2) + 
#	geom_vline(xintercept = as.Date('1997-12-01')) + 
	facet_grid (variable ~ ., scales = 'free_y') + scale_x_date('YEAR', major = 'years', minor = 'months') + ylab('') + 
	opts(title = 'HK Property Market Movement 1997 - 2010')
