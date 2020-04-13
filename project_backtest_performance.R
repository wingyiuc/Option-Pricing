#######################################
### Code for Backtesting Performance

#######################################
### Load packages
library(data.table)
library(readxl)
library(stringr)
library(ggplot2)
library(tidyverse)
library(doParallel)
library(doRNG)
registerDoParallel(cores=detectCores())

#######################################
### Directory setup
# ***User edit area***
mainDir = "E:/Yoyo Chan/Documents/FINA4354 Financial engineering/Group project/"
setwd(mainDir)
dataDir = paste(mainDir , "Price_data/", sep="")

#######################################
### Functions
preprocess.df = function(df){
  
  # Input: dataframe from rds files with Time and Closed_Price
  # Output: datatable with date, Closed_Price and ret (return)
  
  setDT(df)
  names(df) = str_replace_all(names(df)," ","_")
  df = df[,list(date=Time,Closed_Price=as.numeric(Closed_Price)),]
  df = df[,ret:= log(Closed_Price)-log(shift(Closed_Price,type="lag",n=1)),]
  df = na.omit(df)
  setkey(df,"date")
  return(df)
}

join_price_table = function(df){
  
  # Input: list of dataframes (3D) of stock data (date, Closed_Price, ret)
  # Output: Join price columns into one 2D dataframe
  
  df.ret = data.table(cbind(df[[1]]$date,df[[1]]$Closed_Price))
  colnames(df.ret) = c("date","Closed_Price.1")
  for (i in 2:num.files) {
    df.temp = data.table(cbind(df[[i]]$date,df[[i]]$Closed_Price))
    colnames(df.temp) = c("date",paste("Closed_Price.",i,sep=""))
    df.ret = merge(df.ret,df.temp,by="date")
  }
  setDT(df.ret)
  setkey(df.ret,date)
  df.ret = df.ret[, lapply(.SD, as.numeric), by=date]
  return(df.ret)
}
#######################################
# Get all files from folder
files = list.files(dataDir,pattern = "_backtest.rds")

# Get number of underlyings
num.files =length(files)

# Read rds files
df = lapply(paste(dataDir,files,sep=""),readRDS)
df = lapply(df,setDT)
df = lapply(df,preprocess.df)

# Graphing basket one-year historical return
df.price = join_price_table(df)
df.price[, mean.price := rowMeans(.SD), by=date]
df.price$date = as.Date(df.price$date)
df.price = df.price[date>"2018/03/31"&date<"2020/04/02"]
df.price[, color := ifelse(date<"2019/04/02",'b','r')]
ggplot(df.price, aes(x=date, y=mean.price, group = color, color=color))+ geom_line() +
  geom_hline(yintercept=26.37833,color='red') + geom_hline(yintercept=54.64083)

###################################################
### Calculating participation rate
com.fee = 0.04
original.I = 1000000
I = original.I*(1-com.fee)
bond.yield = 3/100
B = exp(-bond.yield*T)*I
print(paste("remaining amount to invest:",I-B))
UIC = 0.2516
DIP = 0.1829
total.option.price = UIC + DIP
print(paste("total option price is", total.option.price))
P.rate = (I-B)/(I*total.option.price)
print(paste("participation rate is: ", P.rate*100, "%"))
P = P.rate * I
print(paste("participation is: ", P))

###################################################
### Backtested payoff
begin.price = df.price[date == "2019/04/01",mean.price]
end.price = df.price[date == "2020/04/01",mean.price]
payoff = abs(begin.price - end.price)
contract.payoff = P*payoff + I
contract.ret = contract.payoff/original.I-1
print(paste("Contract payoff:", contract.payoff))
print(paste("Contract return:", contract.ret*100,"%"))
print(paste("Portfolio return:", (end.price/begin.price-1)*100,"%"))

