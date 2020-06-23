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

btc_xts <- btc_xtsL["/"] # If you want to set the start or end date change the content
#btc_xts <- btc_xtsL["/20191215"] # here we stop data at exactly one year after the second cycle
plot(btc_xts$close)
str(btc_xts)


