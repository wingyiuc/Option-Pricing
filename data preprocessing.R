#######################################
### Code for data preprocessing
# How to use: drop the historical price excel/csv files into "data" folder
# Run the code and get the processed rds files
# Expected ouput: pricing/ backtest rds files with columns "date", "Closd_price" and "ret"
#######################################
### Load packages
library(data.table)
library(readxl)
library(stringr)
library(dplyr)
#######################################
### Directory setup
# ***User edit area***
mainDir = "C:/Users/kenneth.DESKTOP-DIPDF8F/Option-Pricing/"
setwd(mainDir)
dataDir = paste(mainDir , "data/", sep="")

#######################################
# Functions
preprocess.df = function(df){
  
  # Input: dataframe from rds files
  # Output: datatable with date, Closed_Price and ret (return)
  
  setDT(df)
  names(df) = str_replace_all(names(df)," ","_")
  df = df[,list(date=Time,Closed_Price= Closed_Price %>% as.character %>% as.numeric),]
  df = df[,ret:= log(Closed_Price)-log(shift(Closed_Price,type="lag",n=1)),]
  df = na.omit(df)
  return(df)
}

#######################################
## Save excel from HKEX as rds for pricing
files = list.files(dataDir,pattern = ".xlsx")
for (file in files) {
  df = read_excel(paste(dataDir,file,sep=""))
  filename = strsplit(file,".xlsx")[1]
  setDT(df)
  df = df[Time > "2018/03/31" & Time<"2019/04/02",]
  df = preprocess.df(df)
  saveRDS(df, paste(dataDir,filename,"_pricing.rds",sep="") )
}

#######################################
## Save excel from HKEX as rds for backtesting
files = list.files(dataDir,pattern = ".xlsx")
for (file in files) {
  df = read_excel(paste(dataDir,file,sep=""))
  filename = strsplit(file,".xlsx")[1]
  setDT(df)
  df = df[Time > "2018/03/31" & Time<"2020/04/02",]
  df = preprocess.df(df)
  saveRDS(df, paste(dataDir,filename,"_backtest.rds",sep="") )
}

#######################################
## Save csv from Yahoo Finance as rds for pricing
files = list.files(dataDir,pattern = ".csv")
for (file in files) {
  df = read.csv(paste(dataDir,file,sep=""))
  filename = strsplit(file,".csv")[1]
  setDT(df)
  df = df[,.(Time=format(as.Date(Date),"%Y/%m/%d"), Closed_Price=Adj.Close)]
  df = df[Time > "2017/04/01" & Time<"2018/04/01",]
  df = preprocess.df(df)
  saveRDS(df, paste(dataDir,filename,"_pricing.rds",sep=""))
}

#######################################
## Save csv from Yahoo Finance as rds for backtesting
files = list.files(dataDir,pattern = ".csv")
for (file in files) {
  df = read.csv(paste(dataDir,file,sep=""))
  filename = strsplit(file,".csv")[1]
  setDT(df)
  df = df[,.(Time=format(as.Date(Date),"%Y/%m/%d"), Closed_Price=Adj.Close)]
  df = df[Time > "2017/04/01" & Time<"2019/04/01",]
  df = preprocess.df(df)
  saveRDS(df, paste(dataDir,filename,"_backtest.rds",sep=""))
}
