############################################################
######## B B ###############################################
##### BBBBBBBB ###### TTTTTTTTTTTTTTTT ##### CCCCCCCCC #####
##### BBBBBBBBBBBB ## TTTTTTTTTTTTTTTT ### CCCCCCCCCCCC ####
##### BBB      BBB ######## TTTT ######## CCCCC ############
##### BBB     BBB########## TTTT ######## CCCC #############
##### BBBBBBBBBB ########## TTTT ######## CCCC #############
##### BBBBBBBBBBB ######### TTTT ######## CCCC #############
##### BBB     BBBBB ####### TTTT ######## CCCC #############
##### BBB      BBBB ####### TTTT ######## CCCCC ############
##### BBBBBBBBBBBBB ####### TTTT ######### CCCCCCCCCCCC ####
##### BBBBBBBBBBB ######### TTTT ########## CCCCCCCCCC #####
######## B B ###############################################
############################################################
#write.csv(forecasts, file="forecasts_test.csv", row.names=FALSE)

# Loading libraries

library(devtools)
library(astsa)
#library(xtable)
library(ggplot2)
library(DescTools) 
library(forecast)
library(xts)
#library(crypto)

#install.packages("quantmod",
#                 repos = c("http://rstudio.org/_packages",
#                           "http://cran.rstudio.com"))
library(quantmod) #se da problemi usa sopra


##########################################################
######################## %%%%%%%%%% ####### %%%%%%%%%% ###
##### TOTAL DATA ########## %%%% #### %%% #### %%%% ######
########################### %%%% ### %   % ### %%%% ######
########################### %%%% #### %%% #### %%%% ######
##########################################################


## Here I had some troubles in retrievieng data. This is because the source (Yahoo Finance) has changed
## the dataset, which before started from 2010 and subsequently started from the end of 2014. 
## Fortunately, I have saved the workspace which is added on GitHub.

load("C:/______/_______/_______/YahooFinHistorBitcoin.RData") # Here goes your path

btc_xtsUNO <- getSymbols("BTC-USD", auto.assign = F)
btc_xtsUNO$`BTC-USD.Adjusted`<- NULL
colnames(btc_xtsUNO) <- c("open", "high", "low", "close", "volume")

sum(is.na(btc_xtsUNO)) # To check if there are Nas (missing values)
#btc_xtsUNO <- na.locf(btc_xtsUNO) # If there are missing values

# Unifying the old saved dataset with more recent data 
# 2014-09-17: the last day of Yahoo Series

btc_xtshAdapt <- btc_xtsh["/20140916"]
btc_xtsL <- rbind(btc_xtshAdapt, btc_xtsUNO)

btc_xts <- btc_xtsL["/"] # If you want to set the start or end date, change the content
#btc_xts <- btc_xtsL["/20191215"] # here we stop data at exactly one year after the second cycle
plot(btc_xts$close)
str(btc_xts)

#########################
### TRANSFORMING DATA ###
#########################

### logarithmic price
logprice <- log(btc_xts$close)   
autoplot(logprice) 

# Box-Cox transformation using lambda  
(lambda <- BoxCox.lambda(btc_xts$close))
boxcoxtr <- BoxCox(btc_xts$close,lambda)
autoplot(boxcoxtr)
# Choosing shorter and shorter intervals shows that lambda decreases
# from 0.033,0.064,0.011,-0.045. This is because we leave out the effect
# of the first bubble. Log should be appropriate for estimating the trend


# To see how data changes with different lambdas
autoplot(BoxCox(btc_xts$close,0.01))
autoplot(BoxCox(btc_xts$close,0.15))

#### return
diffprice <- diff(btc_xts$close)
plot(diffprice) #### daily returns

##### calculating the log daily log returns which are an approximation of precentage change (with log in base10?)
dlprice <- diff(logprice)[-1,]
plot(dlprice)

# store ts objects
btc_ts <- ts(as.numeric(btc_xts$close), start = c(2013), frequency = 365) #### transforming in a ts object
lbtc_ts <- ts(as.numeric(log(btc_xts$close)), start = c(2013), frequency = 365)
dlbtc_ts <- ts(as.numeric(dlprice$close), start = c(2013), frequency = 365) #### ts return


####### Fractional differencing
# library(fracdiff)
# # To understand the fractional difference with some graphs
# fdGPH(logprice, bandw.exp = 0.1)
# 
# fdiff1 <- diffseries(lbtc_ts, 0.25)
# fdiff1 <- ts(as.data.frame(fdiff1)[-1,], start = 2010, frequency = 365)
# fdiff2 <- diffseries(lbtc_ts, 0.5)
# fdiff2 <- ts(as.data.frame(fdiff2)[-1,], start = 2010, frequency = 365)
# fdiff3 <- diffseries(lbtc_ts, 0.75)
# fdiff3 <- ts(as.data.frame(fdiff3)[-1,], start = 2010, frequency = 365)
# fdiff4 <- diffseries(lbtc_ts, 1)
# fdiff4 <- ts(as.data.frame(fdiff4)[-1,], start = 2010, frequency = 365)
# fdiff5 <- diffseries(lbtc_ts, 1.5)
# fdiff5 <- ts(as.data.frame(fdiff5)[-1,], start = 2010, frequency = 365)
# fdiff6 <- diffseries(lbtc_ts, 2)
# fdiff6 <- ts(as.data.frame(fdiff6)[-1,], start = 2010, frequency = 365)
# 
# par(mfrow= c(3,1))
# plot(fdiff1)
# plot(fdiff2)
# plot(fdiff3)
# 
# plot(fdiff4)
# plot(fdiff5)
# plot(fdiff6)
# par(mfrow = c(1,1))

####transform OHLC from daily to week and month # close price

btc_xtsw <- to.weekly(btc_xts)
lbtc_xtsw <- log(btc_xtsw)
btc_xtsm <- to.monthly(btc_xts)
lbtc_xtsm <- log(btc_xtsm)

#####transform OHLC from daily to week and month # mean value
#### here you can extract also the boxcox and log values
#btc_xtsmeanw <- apply.weekly(btc_xts, mean, na.rm = T)
#btc_xtsmeanm <- apply.monthly(btc_xts, mean, na.rm = T)
#
## the mean of ohlc format
#f1 <- as.numeric(btc_xts$open)
#f2 <- as.numeric(btc_xts$close)
#f3 <- as.numeric(btc_xts$high)
#f4 <- as.numeric(btc_xts$low)
#ff <- cbind(f1,f2)
#ff <- cbind(ff,f3)
#ff <- cbind(ff,f4)
#meanvec <- apply(ff, 1, mean)


nsdiffs(lbtc_ts)
ndiffs(lbtc_ts)
seasonplot(lbtc_ts, 730) ### what the luck
# you could experiment with different values to search for reccurrent patterns 


#################################################
###### CYCLICAL ANALYSIS ########################
#################################################


##########################################################
################################################### % ####
###### FIRST CYCLE (A) ## 1435 days # 16400% ##### %%% ###
################################################# %   % ##
###### 09/02/2011 (1$) - 15/01/2015 (165$) ##### %%%%%%% #
################################################ %     % #
##########################################################

as.Date("14/01/2015", format="%d/%m/%Y") - as.Date("09/02/2011", format="%d/%m/%Y") 
(165/1 - 1)*100 # CF/CI - 1 = i #calcolo interesse

btc.xtsA <- btc_xtsh["20110209/20150115"] 
btc.tsAl <- ts(as.numeric(log(btc.xtsA$close)), start = c(2011,02), frequency = 365)
# Box-Cox transformation using lambda  
(lambdah <- BoxCox.lambda(btc.xtsA$close))
boxcoxtrhA <- BoxCox(btc.xtsA$close,lambdah)
autoplot(boxcoxtrhA)
dlpriceA <- diff(log(btc.xtsA$close))[-1,]
btc.xts.weeklyA <- apply.weekly(btc.xtsA, mean, na.rm = T)
btc.tsAbox <- ts(as.numeric(boxcoxtrhA), start = c(2010,12), frequency = 365)

# Because Outliers are too strong we should adjust them
### Create a robust ts from outliers
# library(robustbase)
# # Clean the return series
# robdlpriceA <- Return.clean(dlpriceA, method = "boudt", alpha = 0.005)
# # Plot them on top of each other
# plotretA <- plot(dlpriceA, col = "red")
# plotretA <- addSeries(robdlpriceA, col = "blue", on = 1)
# plotretA
# 
# btc.tsAdl <- ts(as.numeric(robdlpriceA), start = c(2010,12), frequency = 365)
# 
# # To see the effects on the original series
# plot(ts(exp(cumsum(robdlpriceA))))
# plot(btc.xtsA$close)

##############################################
#### UPTREND (1) ## 1032 + 1 days ############ 
##############################################
#### 09/02/2011 (1$) - 06/12/2013 (1205$) #### 
##############################################
as.Date("06/12/2013", format="%d/%m/%Y") - as.Date("09/02/2011", format="%d/%m/%Y")
(1205/1 -1)*100 # 120400% from low to first high
btc.xts1 <- btc_xtsh["20110209/20131206"]
btc.ts1l <- ts(as.numeric(log(btc.xts1$close)), start = c(2010,12), frequency = 365)

# robdlprice1 <- robdlpriceA["20110209/20131206"]

##############################################
#### DOWNTREND (2) ## 404 days ###############
##############################################
#### 07/12/2013 (1205$) - 15/01/2015 (165$) ##
##############################################
as.Date("15/01/2015", format="%d/%m/%Y") - as.Date("07/12/2013", format="%d/%m/%Y")
(165/1205 - 1)*100 # -86.307%
btc.xts2 <- btc_xtsh["20131130/20150115"]
btc.ts2l <- ts(as.numeric(log(btc.xts2$close)), start = c(2013,12), frequency = 365)
#robdlprice2 <- robdlpriceA["20131130/20150115"]

##########################################################
################################################# %%%% ###
##### SECOND CYCLE (B) ## 1430 days # 1858.78% ## %   % ##
################################################# %%%% ###
##### 15/01/2015 (165$) - 15/12/2018 (3232$) #### %   %% #
################################################# %%%%% ##
##########################################################

as.Date("15/12/2018", format="%d/%m/%Y") - as.Date("15/01/2015", format="%d/%m/%Y") 
(3232/165 -1)*100 # 1981% from low to low# CF/CI - 1 = i #calcolo interesse

#xts
btc.xtsB <- btc_xts["20150115/20181215"] 
logpriceB <- log(btc.xtsB$close)
autoplot(logpriceB)
(lambda <- BoxCox.lambda(btc.xtsB$close))
boxcoxB <- BoxCox(btc.xtsB$close,lambda)
autoplot(boxcoxB)
dlpriceB <- diff(log(btc.xtsB$close))[-1,]
btc.xts.weeklyB <- apply.weekly(btc.xtsB, mean, na.rm = T)

btc.tsBl <- ts(as.numeric(logpriceB), start = c(2015,01), frequency = 365)
btc.tsBbox <-ts(as.numeric(boxcoxB), start = c(2015,01), frequency = 365)
btc.tsBdl <- ts(as.numeric(dlpriceB), start = c(2015,01), frequency = 365)

##############################################
#### UPTREND (3) ## 1067 days # 11624.24% ####
##############################################
#### 15/01/2015 (165$) - 17/12/2017 (19345$) #
##############################################
as.Date("17/12/2017", format="%d/%m/%Y") - as.Date("15/01/2015", format="%d/%m/%Y")
(19345/165 -1)*100 

btc.xts3 <- btc_xts["20150116/20171217"]
logprice3 <- log(btc.xts3$close)
(lambda <- BoxCox.lambda(btc.xts3$close))
boxcox3 <- BoxCox(btc.xts3$close,lambda)
autoplot(boxcox3)
dlprice3 <- diff(log(btc.xts3$close))[-1,]

btc.ts3l <- ts(as.numeric(logprice3), start = c(2015,01), frequency = 365)
btc.ts3box <-ts(as.numeric(boxcox3), start = c(2015,01), frequency = 365)
btc.ts3dl <- ts(as.numeric(dlprice3), start = c(2015,01), frequency = 365)


################################################
#### DOWNTREND (4) ## 363 days ## -83.29% ######
################################################
#### 17/12/2017 (19345$) - 15/12/2018 (3232$) ##
################################################
as.Date("15/12/2018", format="%d/%m/%Y") - as.Date("17/12/2017", format="%d/%m/%Y")
(3232/19345 -1)*100 

btc.xts4 <- btc_xts["20171218/20181217"]
logprice4 <- log(btc.xts4$close)
(lambda <- BoxCox.lambda(btc.xts4$close))
boxcox4 <- BoxCox(btc.xts4$close,lambda)
autoplot(boxcox4)
dlprice4 <- diff(log(btc.xts4$close))[-1,]

btc.ts4l <- ts(as.numeric(logprice4), start = c(2017,12), frequency = 365)
btc.ts4box <-ts(as.numeric(boxcox4), start = c(2017,12), frequency = 365)
btc.ts4dl <- ts(as.numeric(dlprice4), start = c(2017,12), frequency = 365)


##########################################################
################################################# %%%%% ##
##### THIRD CYCLE (C) ## ____ days # ______ % ## %%     ##
################################################ %%     ##
##### 15/12/2018 (3232$) - when Lambo? 2023  ### %%     ##
################################################# %%%%% ##
##########################################################

btc.xtsC <- btc_xts["20181215/"]
logpriceC <- log(btc.xtsC$close)
(lambda <- BoxCox.lambda(btc.xtsC$close))
boxcoxC <- BoxCox(btc.xtsC$close,lambda)
autoplot(boxcoxC)
dlpriceC <- diff(log(btc.xtsC$close))[-1,]
btc.xts.weeklyC <- apply.weekly(btc.xtsC, mean, na.rm = T)

btc.tsCl <- ts(as.numeric(logpriceC), start = c(2018,12), frequency = 365)
btc.tsCbox <-ts(as.numeric(boxcoxC), start = c(2018,12), frequency = 365)
btc.tsCdl <- ts(as.numeric(dlpriceC), start = c(2018,12), frequency = 365)


## ADD VARIABLES TO DATASET
btc_xts$logprice <- logprice
btc_xts$dlprice <- dlprice
btc_xts$boxcox <- boxcoxtr
#btc_xts$ohlcmean <- meanvec


# Adding the second and the beginning of the third cycle to assess riskness.
# The idea is that using a dynamic system for return behaviour is more complete
# with the last two cycles jointed. 

btc.xtsR <- btc_xts["20150115/"]
dlpriceR <- diff(log(btc.xtsR$close))[-1,]
logpriceR <- log(btc.xtsR$close)
(lambda <- BoxCox.lambda(btc.xtsR$close))
boxcoxR <- BoxCox(btc.xtsR$close,lambda)
btc.tsRl <- ts(as.numeric(logpriceR), start = c(2018,12), frequency = 365)
btc.tsRbox <-ts(as.numeric(boxcoxR), start = c(2018,12), frequency = 365)
btc.tsRdl <- ts(as.numeric(dlpriceR), start = c(2018,12), frequency = 365)

# fdiffR <- diffseries(btc.tsRl, 0.8)
# fdiffR <- ts(as.data.frame(fdiffR)[-c(1:5),], start = 2010, frequency = 365)
# plot(fdiffR)


# ### Create a robust ts from outliers
# library(robustbase)
# # Clean the return series
# robdlpriceR <- Return.clean(dlpriceR, method = "boudt", alpha = 0.15)
# # Plot them on top of each other
# plotret <- plot(dlpriceR, col = "red")
# plotret <- addSeries(robdlpriceR, col = "blue", on = 1)
# plotret


# # to see how the adjustment affects the real series.
# ts.robdlpriceR <- ts(as.numeric(robdlpriceR), start = c(2015), frequency = 365)
# csumfitR <- ts(cumsum(as.numeric(robdlpriceR)))
# plot(csumfitR)



#################################
#### TECHNICAL ANALYSIS 1M ######
#################################

## graph for tecnical analysis 
# TA indicators should be adjusted to maximize for months or weeks or months


#monthly chart
chartSeries(lbtc_xtsm["2010/08/01/"], theme = 'white', 
            TA = 'addVo();addRSI();addSMA(n = 10);addEMA(n = 2)', 
            name = 'Log Price of Bitcoin vs. Dollars')
#
###comparing two years cycles 
#chartSeries(lbtc_xtsw["201007/201501"], theme = 'white', 
#            TA = 'addVo();addRSI();addSMA(n = 10);addEMA(n = 2)', name = 'Bitcoin USD')
chartSeries(lbtc_xtsw["2015/01/01/"], theme = 'white', 
            TA = 'addVo();addRSI();addSMA(n = 10);addEMA(n = 2)', name = 'Bitcoin USD')

chartSeries(btc_xts["2020/"], theme = 'white', 
            TA = 'addVo();addRSI();addSMA(n = 10);addEMA(n = 2)', name = 'Bitcoin USD')


# the best indicators to add
#addRSI(n = 14)
#addADX()
#addCCI()
#addCMF()
#addCMO()



###SOME INDICATORS FOR CHARTSERIES
#addADX() OK
#addATR()
#addBBands()
#addCCI() OK
#addCMF() OK
#addCMO() OK
#addDEMA() ADJUST
#addEnvelope() nein
#addMACD()
#addMomentum() nein
#addROC() NEIN
#addRSI()
#addSAR() OK
#addSMA() OK
#addSMI() OK 
#addTRIX() NEIN
#addVO()
#adobject
#dWMA() MMM
#addWPR() OK WILLIAMS %R
#addZLEMA() MMM

#####################################################
# positive and negative intervals and periodicity ###
#####################################################

library(scales)

#plot cycle A
provaA <- merge(log(btc.xtsA$close), dailyReturn(log(btc.xtsA$close)))
provaA <- merge(lag(log(btc.xtsA$close),1), provaA)
names(provaA) <- c("ymin", "ymax", "return")
df <- with(provaA,
           data.frame(xmin=c(lag(index(provaA),1)),
                      xmax=index(provaA), 
                      ymin, ymax, return))
df$status <- with(df,ifelse(return>0.01,"up",ifelse(return< -0.01,"down","neutral")))
dfpoints <- df[seq(58, nrow(df), 75), ] 
Aplot <- ggplot(df) + 
  geom_segment(aes(x=xmin, xend=xmax, y=ymin, yend=ymax, color=status), size =2) +
  geom_point(data = dfpoints,
             aes(x=xmax, y=(ymax - 0.2)), size = 3, shape = 17) +
  scale_color_manual(values=c(up="green", down="red", neutral="grey50"), 
                     breaks=c("up","down"),
                     labels=c("Daily Gain > 1%", "Daily Loss > 1%")) +
  scale_x_date(breaks=date_breaks("years"), labels=date_format("%Y"))+
  labs(x=NULL, y="Closing Price", title="First cycle") +
  theme_bw()
Aplot

##plot cycle B 
provaB <- merge(logpriceB, dailyReturn(logpriceB))
provaB <- merge(lag(logpriceB,1), provaB)
names(provaB) <- c("ymin", "ymax", "return")
dfB <- with(provaB,
            data.frame(xmin=c(lag(index(provaB),1)),
                       xmax=index(provaB), 
                       ymin, ymax, return))
dfB$status <- with(dfB,ifelse(return>0.005,"up",ifelse(return< -0.005,"down","neutral")))
dfpointsBull <- dfB[seq(14, nrow(dfB), 60), ]
dfpointsBull <- dfpointsBull[-c(20:25),]
dfpointsBear <- dfB[seq(20, nrow(dfB), 61), ] #54, 61
dfpointsBear <- dfpointsBear[-c(1:18),]
dfpointsB <- rbind(dfpointsBull, dfpointsBear)

Bplot <- ggplot(dfB) + 
  geom_segment(aes(x=xmin, xend=xmax, y=ymin, yend=ymax, color=status), size =2) +
  geom_point(data = dfpointsB,
             aes(x=xmax, y=(ymax - 0.2)), size = 3, shape = 17) +
  scale_color_manual(values=c(up="green", down="red", neutral="grey50"), 
                     breaks=c("up","down"),
                     labels=c("Daily Gain > 0.5%", "Daily Loss > 0.5%")) +
  scale_x_date(breaks=date_breaks("years"), labels=date_format("%Y"))+
  labs(x=NULL, y="Closing Price", title="Second cycle") +
  theme_bw()
Bplot


#plot cycle C

provaC <- merge(logpriceC, dailyReturn(logpriceC))
provaC <- merge(lag(logpriceC,1), provaC)
names(provaC) <- c("ymin", "ymax", "return")
dfC <- with(provaC,
            data.frame(xmin=c(lag(index(provaC),1)),
                       xmax=index(provaC), 
                       ymin, ymax, return))
dfC$status <- with(dfC,ifelse(return>0.005,"up",ifelse(return< -0.005,"down","neutral")))
dfpointsC <- dfC[seq(19, nrow(dfC), 42), ] #19 42
Cplot <- ggplot(dfC) + 
  geom_segment(aes(x=xmin, xend=xmax, y=ymin, yend=ymax, color=status), size =2) +
  geom_point(data = dfpointsC,
             aes(x=xmax, y=(ymax - 0.1)), size = 3, shape = 17) +
  scale_color_manual(values=c(up="green", down="red", neutral="grey50"), 
                     breaks=c("up","down"),
                     labels=c("Daily Gain > 0.5%", "Daily Loss > 0.5%")) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%b"))+
  labs(x=NULL, y="Closing Price", title="Third cycle") +
  theme_bw()
Cplot

library(pdp)
#grid.arrange(Aplot, Bplot, Cplot, ncol = 1, nrow = 3, top = "Returns and cycles")
grid.arrange(Aplot, Bplot, ncol = 1, nrow = 2, top = "Returns and cycles")








############################### $$$$$$$$$$$$$$$$$$$$
######## RETURNS ############## $$$$$$   R    $$$$$$
############################### $$$$$$$$$$$$$$$$$$$$

library(PerformanceAnalytics) #In general, this package requires return (rather than price) data. 
library(moments)
library(MASS)
library(aTSA)

########################
##### FIRST CYCLE ######
########################

plot(diff(log(btc.xtsA$close))) #["20110109/20150215"])
chart.Histogram(diff(log(btc.xtsA$close))) #["20110109/20150215"])
#UPTREND
plot(diff(log(btc.xts1$close))) #["20101201/20131129"])
chart.Histogram(diff(log(btc.xts1$close)))
#DOWNTREND
plot(diff(log(btc.xts2$close))) #["20131130/20150115"]) 
chart.Histogram(diff(log(btc.xts2$close))) #["20131130/20150115"])

# Looking at the absolute value of the distribution and others exp
autoplot(abs(robdlpriceA))
acf2(abs(robdlpriceA), max.lag = 30)
autoplot(robdlpriceA^2)
acf2(dlpriceB^2, max.lag = 60)
autoplot(robdlpriceA^3)
acf2(robdlpriceA^3, max.lag = 60)
autoplot(robdlpriceA^4)
acf2(robdlpriceA^4, max.lag = 60)

meanA28 <- apply.rolling(R = robdlpriceA, width = 18, trim = TRUE, gap = 12, by = 1,
                         FUN = "mean")
plotmeanA <- plot(robdlpriceA, col = "black", lwd = 0.5, main ="Log Returns")
plotmeanA <- addSeries(meanA28, col = "red", on = 1, lwd = 2)
plotmeanA 


sdA28 <- apply.rolling(R = robdlpriceA, width = 18, trim = TRUE, gap = 12, by = 1,
                       FUN = "sd")
plotsdA <- plot(abs(robdlpriceA), col = "black", lwd = 0.5, main = "Absolute Log Returns")
plotsdA <- addSeries(sdA28, col = "orange", on = 1, lwd = 2)
plotsdA

#note: sd is different from the mean of abs(x), but they are similar

adf.test(robdlpriceA) #Augmented Dickey Fuller Test
pp.test(robdlpriceA) #Perron-Philips test


bpA <- boxplot(coredata(robdlpriceA),
               main = "Distribution of Log Returns Second cycle",
               border = "blue",
               horizontal = TRUE,
               notch = TRUE)
abline(v=0)

# bpA2 <- chart.Boxplot(robdlpriceA)

histA <- chart.Histogram(robdlpriceA, main = "Density", breaks=100,
                         methods = c("add.density", "add.normal","add.risk"))

QQA <- chart.QQPlot(robdlpriceA)

skewness(robdlpriceA) #-0.2805342
kurtosis(robdlpriceA) # 8.004536 

### in another way.. ok!
make.index.unique(robdlpriceA, drop=T)
tbA <- round(table.Stats(robdlpriceA),4) 
tbA


#lagplot
library(stats)
ReturnA <- robdlpriceA
lag1.plot(ReturnA,9) #do.lines = T) to see lines connetting lags


### lag plot and studying outliers
alti <- btc.tsAdl[order(abs(btc.tsAdl))][(length(btc.tsAdl)-4):length(btc.tsAdl)]
dat <- cbind(ts(btc.tsAdl), lag(ts(btc.tsAdl)))
lag.plot(btc.tsAdl, main = "Oulier Behaviour")
for(i in 1:length(alti)){
  val<-alti[i]
  for(j in 1:length(btc.tsAdl)){  
    if(val== btc.tsAdl[j]){
      points(dat[j,2], dat[j,1], col=i, cex=2)
      lines(dat[(j-5):(j+5),c(2,1)], col=i)
    }}}


alti <- btc.tsAdl[order(btc.tsAdl)][1:5]
lag.plot(btc.tsAdl, main = "Oulier Behaviour")
for(i in 1:length(alti)){
  val<-alti[i]
  for(j in 1:length(btc.tsAdl)){
    if(val== btc.tsAdl[j]) {
      points(dat[j,2], dat[j,1], col=i, cex=2)
      lines(dat[j:(j+10),c(2,1)], col=i)
      # readline()
    }}}


layout(c(1,2,3),)
#layout(rbind(c(1,2),c(3,4)))
#plot(dlpriceB, main = "Log Returns")
chart.Histogram(robdlpriceA, main = "Density", breaks=100,
                methods = c("add.density", "add.normal","add.risk"))
boxplot(coredata(robdlpriceA),
        main = "Boxplot",
        border = "blue",
        horizontal = TRUE,
        notch = TRUE)
abline(v=0)
chart.QQPlot(robdlpriceA, main = "Quantiles")

layout(1,1)
par(mfrow=c(1,1))

##############################
###### UPTREND (1) ###########
##############################

autoplot(robdlprice1)
acf2(robdlprice1, max.lag = 60)

lag1.plot(robdlprice1,9)
lag.plot(robdlprice1, 4, do.lines = T)

adf.test(robdlprice1) #Augmented Dickey Fuller Test
pp.test(robdlprice1) #Perron-Philips test


boxplot(coredata(robdlprice1),
        main = "Distribution of Log Returns Uptrend",
        border = "blue",
        horizontal = TRUE,
        notch = TRUE)
abline(v=0)

chart.Histogram(robdlprice1, main = "Density", breaks=100,
                methods = c("add.density", "add.normal","add.risk"))
chart.QQPlot(robdlprice1)

skewness(robdlprice1) #-0.222703 
kurtosis(robdlprice1) # 11.26025   

### in another way.. ok!
make.index.unique(robdlprice1, drop=T)
tb1 <- round(table.Stats(robdlprice1),4) 
tb1


##############################
###### DOWNTREND (2) #########
##############################

autoplot(robdlprice2)
acf2(robdlprice2, max.lag = 60)

lag1.plot(robdlprice2,9)
lag.plot(robdlprice2, 4, do.lines = T)

adf.test(robdlprice2) #Augmented Dickey Fuller Test
pp.test(robdlprice2) #Perron-Philips test


boxplot(coredata(robdlprice2),
        main = "Distribution of Log Returns Downtrend",
        border = "blue",
        horizontal = TRUE,
        notch = TRUE)
abline(v=0)

chart.Boxplot(robdlprice2)

chart.Histogram(robdlprice2, main = "Density", breaks=100,
                methods = c("add.density", "add.normal","add.risk"))

chart.QQPlot(robdlprice2)

skewness(robdlprice2) #-0.3128192 
kurtosis(robdlprice2) #13.41783

### in another way.. ok!
make.index.unique(robdlprice2, drop=T)
tb2 <- round(table.Stats(robdlprice2),4) 
tb2



### COMPARING THESE 3 INTERVALS (A,1,2)


layout(c(1,2),)
boxplot(coredata(robdlprice1),
        main = "Distribution of Log Returns Uptrend",
        border = "blue",
        horizontal = TRUE,
        notch = TRUE, 
        ylim = c(-0.4, 0.4))

abline(v=0)
boxplot(coredata(robdlprice2),
        main = "Distribution of Log Returns Downtrend",
        border = "blue",
        horizontal = TRUE,
        notch = TRUE, 
        ylim = c(-0.4, 0.4))
abline(v=0)
boxplot(coredata(robdlpriceA),
        main = "Distribution of Log Returns of Cycle A",
        border = "blue",
        horizontal = TRUE,
        notch = TRUE, 
        ylim = c(-0.4, 0.4))
abline(v=0)


chart.Histogram(robdlprice1, main = "Density of Uptrend", breaks=100,
                methods = c("add.density", "add.normal","add.risk"),
                xlim = c(-0.4, 0.4))
chart.Histogram(robdlprice2, main = "Density of Downtrend", breaks=100,
                methods = c("add.density", "add.normal","add.risk"),
                xlim = c(-0.4, 0.4))
chart.Histogram(robdlpriceA, main = "Density of Cycle A", breaks=100,
                methods = c("add.density", "add.normal","add.risk"),
                xlim = c(-0.4, 0.4))
par(mfrow=c(1,1))










#####################################################################


##############################
##### SECOND CYCLE  (B) ######
##############################

plot(btc_xts$dlprice["20141215/20190115"])
autoplot(dlpriceB)
plot(dlpriceB)
# abline(h = 0, col = "red") no
acf2(dlpriceB, max.lag = 30)

# Looking at the absolute value of the distribution and others exp
autoplot(abs(dlpriceB))
acf2(abs(dlpriceB), max.lag = 30)
autoplot(dlpriceB^2)
acf2(dlpriceB^2, max.lag = 60)

meanB28 <- apply.rolling(R = dlpriceB, width = 18, trim = TRUE, gap = 12, by = 1,
                         FUN = "mean")
plotmean <- plot(dlpriceB, col = "black", lwd = 0.5, main = "Log Returns")
plotmean <- addSeries(meanB28, col = "red", on = 1, lwd = 2)
plotmean 


sdB28 <- apply.rolling(R = dlpriceB, width = 18, trim = TRUE, gap = 12, by = 1,
                       FUN = "sd")
plotsd <- plot(abs(dlpriceB), col = "black", lwd = 0.5, main = "Absolute Log Returns")
plotsd <- addSeries(sdB28, col = "orange", on = 1, lwd = 2)
plotsd

adf.test(dlpriceB) #Augmented Dickey Fuller Test
pp.test(dlpriceB) #Perron-Philips test


bpB <- boxplot(coredata(dlpriceB),
               main = "Distribution of Log Returns Second cycle",
               border = "blue",
               horizontal = TRUE,
               notch = TRUE)
abline(v=0)

#bpB2 <- chart.Boxplot(dlpriceB)

histB <- chart.Histogram(dlpriceB, main = "Density", breaks=100,
                         methods = c("add.density", "add.normal","add.risk"))

QQB <- chart.QQPlot(dlpriceB)

skewness(dlpriceB) #-0.2805342
kurtosis(dlpriceB) # 8.004536 

### in another way.. ok!
make.index.unique(dlpriceB, drop=T)
tbB <- round(table.Stats(dlpriceB),4) 
tbB


#lagplot
library(stats)
Return <- dlpriceB
lag1.plot(Return,9) #do.lines = T) to see lines connetting lags



# With the TsDyn package
library(sm)
library(tsDyn)

par(mfrow = c(1,1))
# autopairs(dlpriceB, lag = 1, type = "persp")
# autopairs(dlpriceB, lag = 2, type = "persp")

# par(mfrow = c(2,3))
# autopairs(dlpriceB, lag = 1, type = "levels")
# autopairs(dlpriceB, lag = 2, type = "levels")
# 
# autopairs(dlpriceB, lag = 1, type = "image")
# autopairs(dlpriceB, lag = 2, type = "image")
# 
# autopairs(dlpriceB, lag = 1, type = "lines")
# autopairs(dlpriceB, lag = 2, type = "lines")
# 
# #why this one is different from lag.plot?
# 
# autopairs(dlpriceB, lag = 1, type = "regression")
# autopairs(dlpriceB, lag = 2, type = "regression")
# 
# par(mfrow = c(1,1))

# library(scatterplot3d)
# autotriples(dlpriceB, type = "persp")
# autotriples(dlpriceB, type = "levels")
# autotriples(dlpriceB, type = "image")
# autotriples(dlpriceB, type = "lines")
# autotriples(dlpriceB, type = "points")



### lag plot and studying outliers
alti <- btc.tsBdl[order(abs(btc.tsBdl))][(length(btc.tsBdl)-4):length(btc.tsBdl)]
dat <- cbind(ts(btc.tsBdl), lag(ts(btc.tsBdl)))
lag.plot(btc.tsBdl, main = "Oulier Behaviour")
for(i in 1:length(alti)){
  val<-alti[i]
  for(j in 1:length(btc.tsBdl)){  
    if(val== btc.tsBdl[j]){
      points(dat[j,2], dat[j,1], col=i, cex=2)
      lines(dat[(j-5):(j+5),c(2,1)], col=i)
    }}}


alti <- btc.tsBdl[order(btc.tsBdl)][1:5]
lag.plot(btc.tsBdl, main = "Oulier Behaviour")
for(i in 1:length(alti)){
  val<-alti[i]
  for(j in 1:length(btc.tsBdl)){
    if(val== btc.tsBdl[j]) {
      points(dat[j,2], dat[j,1], col=i, cex=2)
      lines(dat[j:(j+10),c(2,1)], col=i)
      # readline()
    }}}


layout(c(1,2,3),)
#layout(rbind(c(1,2),c(3,4)))
#plot(dlpriceB, main = "Log Returns")
chart.Histogram(dlpriceB, main = "Density", breaks=100,
                methods = c("add.density", "add.normal","add.risk"))
boxplot(coredata(dlpriceB),
        main = "Boxplot",
        border = "blue",
        horizontal = TRUE,
        notch = F)
abline(v=0)
chart.QQPlot(dlpriceB, main = "Quantiles")

layout(1,1)
par(mfrow=c(1,1))



##############################
###### UPTREND (3) ###########
##############################

autoplot(dlprice3)
acf2(dlprice3, max.lag = 60)

lag1.plot(dlprice3,9)
lag.plot(dlprice3, 4, do.lines = T)

adf.test(dlprice3) #Augmented Dickey Fuller Test
pp.test(dlprice3) #Perron-Philips test


boxplot(coredata(dlprice3),
        main = "Distribution of Log Returns Uptrend",
        border = "blue",
        horizontal = TRUE,
        notch = TRUE)
abline(v=0)

chart.Histogram(dlprice3, main = "Density", breaks=100,
                methods = c("add.density", "add.normal","add.risk"))


chart.QQPlot(dlprice3)

skewness(dlprice3) #-0.09416991
kurtosis(dlprice3) # 9.846092  

### in another way.. ok!
make.index.unique(dlprice3, drop=T)
tb3 <- round(table.Stats(dlprice3),4) 
tb3

##############################
###### DOWNTREND (4) #########
##############################

autoplot(dlprice4)
acf2(dlprice4, max.lag = 60)

lag1.plot(dlprice4,9)
lag.plot(dlprice4, 4, do.lines = T)

adf.test(dlprice4) #Augmented Dickey Fuller Test
pp.test(dlprice4) #Perron-Philips test


bp4 <- boxplot(coredata(dlprice4),
               main = "Distribution of Log Returns Downtrend",
               border = "blue",
               horizontal = TRUE,
               notch = TRUE)
abline(v=0)


chart.Histogram(dlprice4, main = "Density", breaks=100,
                methods = c("add.density", "add.normal","add.risk"))

chart.QQPlot(dlprice4)

skewness(dlprice4) #-0.4062501 
kurtosis(dlprice4) # 4.881439

### in another way.. ok!
make.index.unique(dlprice4, drop=T)
tb4 <- round(table.Stats(dlprice4),4) 
tb4


##########################
#### THIRD CYCLE (C) #####
##########################

autoplot(dlpriceC)
acf2(dlpriceC, max.lag = 30)
autoplot(abs(dlpriceC))
acf2(abs(dlpriceC), max.lag = 30)

lag1.plot(dlpriceC,9)
lag.plot(dlpriceC, 4, do.lines = T)

meanC28 <- apply.rolling(R = dlpriceC, width = 18, trim = TRUE, gap = 12, by = 1,
                         FUN = "mean")
plotmeanC <- plot(dlpriceC, col = "black", lwd = 0.5, main = "Log Returns")
plotmeanC <- addSeries(meanC28, col = "red", on = 1, lwd = 2)
plotmeanC 


sdC28 <- apply.rolling(R = dlpriceC, width = 18, trim = TRUE, gap = 12, by = 1,
                       FUN = "sd")
plotsdC <- plot(abs(dlpriceC), col = "black", lwd = 0.5, main = "Absolute Log Returns")
plotsdC <- addSeries(sdC28, col = "orange", on = 1, lwd = 2)
plotsdC


adf.test(dlpriceC) #Augmented Dickey Fuller Test
pp.test(dlpriceC) #Perron-Philips test


bpC <-boxplot(coredata(dlpriceC),
              main = "Distribution of Log Returns Third cycle",
              border = "blue",
              horizontal = TRUE,
              notch = TRUE)
abline(v=0)

chart.Boxplot(dlpriceC)

chart.Histogram(dlpriceC, main = "Density", breaks=100,
                methods = c("add.density", "add.normal","add.risk"))

skewness(dlpriceC) # 0.04406
kurtosis(dlpriceC) # 6.37538 

### in another way.. ok!
make.index.unique(dlpriceC, drop=T)
tbC <- round(table.Stats(dlpriceC),4) 
tbC

### lag plot and studying outliers
alti <- btc.tsCdl[order(abs(btc.tsCdl))][(length(btc.tsCdl)-4):length(btc.tsCdl)]
dat <- cbind(ts(btc.tsCdl), lag(ts(btc.tsCdl)))
lag.plot(btc.tsCdl)
for(i in 1:length(alti)){
  val<-alti[i]
  for(j in 1:length(btc.tsCdl)){  
    if(val== btc.tsCdl[j]){
      points(dat[j,2], dat[j,1], col=i, cex=2)
      lines(dat[(j-5):(j+5),c(2,1)], col=i)
    }}}


alti <- btc.tsCdl[order(btc.tsCdl)][1:5]
lag.plot(btc.tsCdl)
for(i in 1:length(alti)){
  val<-alti[i]
  for(j in 1:length(btc.tsCdl)){
    if(val== btc.tsCdl[j]) {
      points(dat[j,2], dat[j,1], col=i, cex=2)
      lines(dat[j:(j+10),c(2,1)], col=i)
      # readline()
    }}}


### COMPARING THESE 3 INTERVALS (3,4,C)


layout(c(1,2,3),)
boxplot(coredata(dlprice3),
        main = "Distribution of Log Returns Uptrend",
        border = "blue",
        horizontal = TRUE,
        notch = TRUE, 
        ylim = c(-0.22, 0.22))

abline(v=0)
boxplot(coredata(dlprice4),
        main = "Distribution of Log Returns Downtrend",
        border = "blue",
        horizontal = TRUE,
        notch = TRUE, 
        ylim = c(-0.22, 0.22))
abline(v=0)
boxplot(coredata(dlpriceC),
        main = "Distribution of Log Returns Third cycle",
        border = "blue",
        horizontal = TRUE,
        notch = TRUE, 
        ylim = c(-0.22, 0.22))
abline(v=0)


chart.Histogram(dlprice3, main = "Density of Uptrend", breaks=100,
                methods = c("add.density", "add.normal","add.risk"),
                xlim = c(-0.22, 0.22))
chart.Histogram(dlprice4, main = "Density of Downtrend", breaks=100,
                methods = c("add.density", "add.normal","add.risk"),
                xlim = c(-0.22, 0.22))
chart.Histogram(dlpriceC, main = "Density of Third Cycle", breaks=100,
                methods = c("add.density", "add.normal","add.risk"),
                xlim = c(-0.22, 0.22))


chart.QQPlot(dlprice3, main = "Uptrend Second Cycle")
chart.QQPlot(dlprice4, main = "Downtrend Second Cycle")
chart.QQPlot(dlpriceC, main = "Beginning of Third Cycle")
par(mfrow = c(1,1))

########################
### GARCH MODELS #######
########################


# EXploiting volatility

# heteroskedasticity
wnsquaredB <- dlpriceB$close^2
acf2(wnsquaredB) # to find errors we should subtract the mean?

# Following data camp

CalculateReturns(btc.xtsB$close) # not equal to dlprice
sd(dlpriceB) # 0.03853594 ok equal to the table

# Annualized SD
sqrt(365)*sd(dlpriceB["2015"]) # 0.6055235
sqrt(365)*sd(dlpriceB["2016"]) # 0.4823489
sqrt(365)*sd(dlpriceB["2017"]) # 0.9419083
sqrt(365)*sd(dlpriceB["2018"]) # 0.8129125

sqrt(365)*sd(dlpriceC) # 0.7222156 #maybe not use 365 but n.of days


# Rolling volatility in several intervals

par(mfrow=c(3,1))
chart.RollingPerformance(R = dlpriceB["2015::2019"], width = 7,
                         FUN = "sd.annualized", scale = 365, main = "15 days rolling volatility")
chart.RollingPerformance(R = dlpriceB["2015::2019"], width = 14,
                         FUN = "sd.annualized", scale = 365, main = "One month rolling volatility")
chart.RollingPerformance(R = dlpriceB["2015::2019"], width = 28,
                         FUN = "sd.annualized", scale = 365, main = "Two months rolling volatility")


# Calculating mean, absolute errors, squared errors and acf

(meanB <- mean(dlpriceB))
errorsB <- dlpriceB - meanB
errors2B <- errorsB^2
par(mfrow = c(2,1)) #,mar = c(3, 2, 2, 2))
plot(abs(errorsB))
acf(abs(errorsB))
plot(errors2B)
acf(errors2B)
par(mfrow=c(1,1))



# Predicted variability, reproducing a garch model
alpha <- 0.1
beta <- 0.8
omega <- var(dlpriceB)*(1-alpha-beta)
nobsB <- length(dlpriceB)
predvarB <- rep(NA, nobsB)
predvarB[1] <- var(dlpriceB)
# Loop starting at 2 because of the lagged predictor
for (t in 2:nobsB){
  # GARCH(1,1) equation
  predvarB[t] <- omega + 0.1 * errors2B[t - 1] + 0.8 * predvarB[t-1]
}

predvolB <- sqrt(predvarB)
predvolB <- xts(predvolB, order.by = time(dlpriceB))
uncvolB <- sqrt(omega / (1 - alpha-beta))
uncvolB <- xts(rep(uncvolB, nobsB), order.by = time(dlpriceB))
plot(predvolB)
lines(uncvolB, col = "red", lwd = 2)

plot(garchvolB["2015"])
plot(garchvolB["2016"])
plot(garchvolB["2017"])
plot(garchvolB["2018"])
plot(garchvolB)

#this is the same as above a part of the scale
ann_predvolB <- xts(sqrt(365) * sqrt(predvarB), order.by = time(dlpriceB))
plot(ann_predvolB["2015::2018"], main = "Ann. Bitcoin vol. in 2015-2018")


library(rugarch)

######################
# the simplest garch #
######################

# Specify a standard GARCH(1,1) model with constant mean
garchspecRsimp <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "sGARCH"), 
                             distribution.model = "snorm")



# the best for criterion is arima 3,2 but with high p values
# the best for p value are 2,3; 3,3

# Estimate the model
garchfitRsimp <- ugarchfit(data = dlpriceR, spec = garchspecRsimp)

show(garchfitRsimp)
coef(garchfitRsimp)
(sqrt(uncvariance(garchfitRsimp))) # sd (unconditional volatility) in the long term
(head(fitted(garchfitRsimp)))
(head(sigma(garchfitRsimp))) # predicted/estimated volatilities
likelihood(garchfitRsimp) # 2821.07
infocriteria(garchfitRsimp)
# Akaike       -3.939958
# Bayes        -3.925230
# Shibata      -3.939974
# Hannan-Quinn -3.934458
round(garchfitR@fit$matcoef,6)


stdretR1simp <- residuals(garchfitRsimp, standardize = T)
chart.Histogram(stdretR1simp, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"))



### SEARCHING FOR THE BEST NON LINEAR GARCH


# normal distribution doesnt work weel, but other distributions give alfa + beta > 1

variance.models <- c("eGARCH", "csGARCH","gjrGARCH") #,"apARCH","realGARCH", "fiGARCH"
distribution.models <- c("sstd", "sged", "nig")
c <- 1
for (variance.model in variance.models) {
  for (distribution.model in distribution.models) {
    garchspec <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                            variance.model = list(model = variance.model,
                                                  variance.targeting = F),
                            distribution.model = distribution.model)
    garchfit <- ugarchfit(data = dlpriceR, spec = garchspec)
    if (c==1) {
      listGARCH <- list(garchfit)
    } else {
      listGARCH <- c(listGARCH, list(garchfit))
    }
    c <- c + 1
  }
}


sapply(listGARCH, likelihood)
sapply(listGARCH, infocriteria)
urca <- sapply(listGARCH, coef)
urca <- as.data.frame(urca)

espgarch <-cbind(urca[[1]],urca[[2]],urca[[3]]) 
round(as.numeric(espgarch), 4)
csgarch <- cbind(urca[[4]],urca[[5]],urca[[6]])
round(as.numeric(csgarch), 4)
gjrGarch <- cbind(urca[[7]],urca[[8]],urca[[9]])
round(as.numeric(gjrGarch), 4)



#the criteria in latex are egarch/nig
#values for criteria for different classes are unknown, now the best is aparch
#maybe the others are done with var.targeting? 


ars <- c(0,1,2,3)
mas <- c(0,1,2,3)
cc <- 1
for (ar in ars) {
  for (ma in mas) {
    garchspecARMA <- ugarchspec(mean.model = list(armaOrder = c(ar, ma)),
                                variance.model = list(garchOrder = c(1,1), model = "eGARCH",
                                                      variance.targeting = F),
                                distribution.model = "nig")
    garchfitARMA <- ugarchfit(data = dlpriceR, spec = garchspecARMA)
    if (cc==1) {
      ordGARCH <- list(garchfitARMA)
    } else {
      ordGARCH <- c(ordGARCH, list(garchfitARMA))
    }
    cc <- cc + 1
  }
}

sapply(ordGARCH, likelihood)
sapply(ordGARCH, infocriteria)
sapply(ordGARCH, coef)


garchspecR <- ugarchspec(mean.model = list(armaOrder = c(3,2)),
                         variance.model = list(garchOrder = c(1,1), model = "eGARCH",
                                               variance.targeting = F),
                         # submodel = "AVGARCH"),
                         distribution.model = "nig")
# Original model 
## mu is predicted mean, sigma is predicted volatility
garchfitR <- ugarchfit(data = dlpriceR, spec = garchspecR)
garchfitR
plot(garchfitR)
persistence(garchfitR) # in the egarch P = beta1
stdresR <- residuals(garchfitR, standardize = T) 
resR <- residuals(garchfitR, standardize = F)
fitR <- fitted(garchfitR) # this gives the predicted mean
lag1.plot(stdresR,12)

round(coef(garchfitR), 6)
(sqrt(uncvariance(garchfitR))) # sd (unconditional volatility) in the long term
(head(fitted(garchfitR)))
(head(sigma(garchfitR))) # predicted/estimated volatilities
likelihood(garchfitR) # 3018.875
infocriteria(garchfitR)
# Akaike       -4.213811
# Bayes        -4.191718
# Shibata      -4.213846
# Hannan-Quinn -4.205560
round(garchfitR@fit$matcoef,6)

Box.test(abs(stdresR), 22, type = "Ljung-Box")
#Rule of thumb: p-value less than 5% indicates that the model used is not valid.



######## the distribution of something in parameters
hisPDF <- ugarchdistribution(garchfitR, n.sim = 200, n.start = 1, m.sim = 20,  
                             recursive = F,
                             solver = "hybrid")
show(hisPDF)
plot(hisPDF, which = 1)
plot(hisPDF, which = 2)
plot(hisPDF, which = 3)
plot(hisPDF, which = 4)

cluster = makePSOCKcluster(15)
gdR = ugarchdistribution(garchfitR, n.sim = 300, recursive = T, recursive.length = 1800, 
                         recursive.window = 300, 
                         m.sim = 20, solver = 'hybrid') #, cluster = cluster)
stopCluster(cluster)

show(gdR)
plot(gdR, which = 1)
plot(gdR, which = 2)
plot(gdR, which = 3)
plot(gdR, which = 4)

garchforecast <- ugarchforecast(fitORspec = garchfitR,
                                n.ahead = 10)
sigma(garchforecast) # with this we obtain predicted volatilities
fitted(garchforecast) # with this we obtain predicted mean
# if pred. vol. of t+1 is less than the unc.var. of the model, then
# the volatility is expected to increase in the following days because
# short term var. follows long term var.

show(garchforecast)
plot(garchforecast, which = 1)
plot(garchforecast, which = 2)
plot(garchforecast, which = 3)
plot(garchforecast, which = 4)
#datacamp mette dopo questo garchroll


# ugarch bootstrap
bootpR = ugarchboot(garchfitR, method = "partial",  #c("Partial", "Full")[1],
                    n.ahead = 365, n.bootpred = 365)
show(bootpR)
plot(bootpR, which = 2)
plot(bootpR, which = 3)

