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
