#######################################
### Code for data preprocessing
# How to use: drop the historical price rds files into "data" folder
# Run the code and get the processed rds files
#######################################
### Load packages
library(data.table)
library(readxl)
library(stringr)
#######################################
### Directory setup
# ***User edit area***
mainDir = "C:/Users/kenneth.DESKTOP-DIPDF8F/Option-Pricing/"
setwd(mainDir)
dataDir = paste(mainDir , "data/", sep="")

#######################################
## Save excel as rds for pricing
files = list.files(dataDir,pattern = ".xlsx")
for (file in files) {
  df = read_excel(paste(dataDir,file,sep=""))
  filename = strsplit(file,".xlsx")[1]
  setDT(df)
  df = df[Time > "2018/03/31" & Time<"2019/04/02",]
  saveRDS(df, paste(dataDir,filename,"_pricing.rds",sep="") )
}

#######################################
## Save excel as rds for backtesting
files = list.files(dataDir,pattern = ".xlsx")
for (file in files) {
  df = read_excel(paste(dataDir,file,sep=""))
  filename = strsplit(file,".xlsx")[1]
  setDT(df)
  df = df[Time > "2018/03/31" & Time<"2020/04/02",]
  saveRDS(df, paste(dataDir,filename,"_backtest.rds",sep="") )
}

#######################################
