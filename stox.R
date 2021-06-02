### Mock portfolio using Shiny web server
### May 16th, 2021 Evret Korb, evretakorb@gmail.com
library(TSstudio)
library(dplyr)
library(data.table)
library(quantmod)
library(fpp2)
library(ggplot2)
library(plotly)

#Load our files / variables we want 
df1 <- read.csv(file = "/Users/evretkorb/downloads/SPY.csv")
colnames(df1) [6] <- "spy_close"
mydata1 <- df1[c(1,6)]

df2 <- read.csv(file = "/Users/evretkorb/downloads/TSLAA.csv")
colnames(df2) [6] <- "tsla_close"
mydata2 <- df2[6]

df3 <- read.csv(file = "/Users/evretkorb/downloads/DIS.csv")
colnames(df3) [6] <- "dis_close"
mydata3 <- df3[6]

#Combine all variables into one data frame
stocks <- cbind(mydata1,mydata2,mydata3)
stocker <- ts(stocks[2:4], frequency = 253)
#forecast these stock values
autoplot(stocker)
stockx <- diff(stocker)
# find the difference between these values


#Find daily percent change bc this returns % in whole number must divide answer by 100 
spy_pct <- exp(diff(log(stocks$spy_close))) - 1
tsla_pct <- exp(diff(log(stocks$tsla_close))) - 1
dis_pct <- exp(diff(log(stocks$dis_close))) - 1
#since that evaluates to exp(log(v[2:6]) - log(v[1:5])) - 1 which equals (v[2:6] / v[1:5]) - 1 
#which in turn equals (v[2:6] - v[1:5]) / v[1:5].
# Using adjusted closing price to account for stock splits / dividends
# this method assumes the investor reinvests their dividends earnings to assume the correct return rate
print(spy_pct)
percent_change <- data.frame(spy_pct, tsla_pct, dis_pct)

spy_value <- ((100 * percent_change$spy_pct) + 100) / 100
tsla_value <- ((100 * percent_change$tsla_pct) + 100) / 100
dis_value <- ((100 * percent_change$dis_pct) + 100) / 100
interest <- data.frame(spy_value, tsla_value, dis_value)
###

percent <- ts(interest, frequency = 253)
ts_info(percent)

ts_plot(percent)

