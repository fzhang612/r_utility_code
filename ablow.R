setwd('d:/temp')
ablow <- read.csv('ab_JanP56_reach.csv')
ablow$response[is.na(ablow$response)] <- 0
summary(ablow$response)

# head(ablow$RESPD_PROB_middle)
# head(ablow$response, 20)
# head(log(ablow$RESPD_PROB_middle/(1-ablow$RESPD_PROB_middle)))
# dim(ablow)
# sum(ablow$response, na.rm = T)
colnames(ablow)
with(ablow, ks.test(score_middle[response == 1], score_middle[response == 0]))


with(ablow, miv(PIL_accept_rate_12M, response, PROB_middle, 5, show.plot = T))
with(ablow, miv(c_mc_avgmth, response, PROB_middle, 5, show.plot = T, brks = c(-Inf, 50, 300, 600, 900, 1000, Inf)))

mivlist <- mivbatch(ablow[,13:657], ablow$response, ablow$PROB_middle, 5)
mivlist <- mivbatch(ablow[,13:657], ablow$response, ablow$PROB_middle, 5)
print.top.miv(mivlist, 10)


miv(ablow$ARU_ratio13, ablow$response, ablow$PROB_middle, 5, show.plot = T, brks = c(-Inf, 0, 0.4, 0.8, 1.2, 1.6, 2, 2.4,2.8,Inf))

x <- ablow[,'OTB_avg6']
brks = c(-Inf, 5000, 10000, 15000, Inf)
n <- 5
cutx <- cut(x, breaks = classIntervals(x, n, style = 'pretty')$brks, include.lowest = T)
cutx <- cut(x, breaks = brks, include.lowest = T)

cutx <- ifelse(ablow$ARU_ratio13 > 0.8 & ablow$ARU_ratio13 <= 1, 1, 0)
cutx[which(is.na(cutx))] <- 0

if (any(is.na(cutx))) cutx <- addNA(cutx, ifany = T)
levels(cutx)


modx <- glm(ablow$response ~ cutx , offset = mod$linear.predictors, family = 'binomial', na.action = na.pass)
summary(modx)

ks.test(modx$fitted.values[ablow$response == 1], modx$fitted.values[ablow$response == 0])


# STEP 1 
# PIL_accept_rate_12M :: c(-Inf, Inf)
# TOT_INT_FEE_mth6 ::  c(-Inf,  2:5 ,  Inf)

cut_PIL_accept_rate_12M <- with(ablow, cut(PIL_accept_rate_12M, breaks = c(-Inf, Inf), include.lowest = T))
if (any(is.na(cut_PIL_accept_rate_12M))) cut_PIL_accept_rate_12M <- addNA(cut_PIL_accept_rate_12M, ifany = T)
levels(cut_PIL_accept_rate_12M)

cut_TOT_INT_FEE_mth6 <- with(ablow, cut(TOT_INT_FEE_mth6, breaks = c(-Inf, 2:5, Inf), include.lowest = T))
if (any(is.na(cut_TOT_INT_FEE_mth6))) cut_TOT_INT_FEE_mth6 <- addNA(cut_TOT_INT_FEE_mth6, ifany = T)
levels(cut_TOT_INT_FEE_mth6)

mod <- glm(ablow$response ~ cut_PIL_accept_rate_12M + cut_TOT_INT_FEE_mth6 + cutx, 
offset = ablow$lnodds_middle, family = 'binomial',na.action = na.pass)

summary(mod)
anova(mod)
ks.test(mod$fitted.values[ablow$response == 1], mod$fitted.values[ablow$response == 0])

plot(ablow$PROB_middle, ablow$PROB_middle, type = 'l', lty = 2, col = 'red')
points(ablow$PROB_middle, mod$fitted.values)

mivlist <- mivbatch(ablow[,13:657], ablow$response, mod$fitted.values, 5)
print.top.miv(mivlist, 40)


# STEP 2 
# PIL_INT_FEE_avg6 :: c(-Inf, 1000, Inf)
# c_mc_avgmth ::  c(-Inf, 50, 300, Inf)
# don't add these 2 variables because they can not boost KS

# cut_PIL_INT_FEE_avg6 <- with(ablow, cut(PIL_INT_FEE_avg6, breaks = c(-Inf, 1000, Inf), include.lowest = T))
# if (any(is.na(cut_PIL_INT_FEE_avg6))) cut_PIL_INT_FEE_avg6 <- addNA(cut_PIL_INT_FEE_avg6, ifany = T)
# levels(cut_PIL_INT_FEE_avg6)
# 
# # cut_c_mc_avgmth <- with(ablow, cut(c_mc_avgmth, breaks = c(-Inf, 50, 300, Inf), include.lowest = T))
# # if (any(is.na(cut_c_mc_avgmth))) cut_c_mc_avgmth <- addNA(cut_c_mc_avgmth, ifany = T)
# # levels(cut_c_mc_avgmth)
# 
# mod <- glm(ablow$response ~ cut_PIL_accept_rate_12M + cut_TOT_INT_FEE_mth6 + cut_PIL_INT_FEE_avg6,
#  offset = ablow$lnodds_middle, family = 'binomial',na.action = na.pass)
# summary(mod)
# ks.test(mod$fitted.values[ablow$response == 1], mod$fitted.values[ablow$response == 0])
# 
# mivlist <- mivbatch(ablow[,13:657], ablow$response, mod$fitted.values, 5)
# print.top.miv(mivlist, 40)

# STEP 3 
ARU_ratio13 :: cutx <- ifelse(ablow$ARU_ratio13 > 0.8 & ablow$ARU_ratio13 <= 1, 1, 0)
                cutx[which(is.na(cutx))] <- 0
