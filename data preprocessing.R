#######################################
### Code for pricing multiple stocks
# How to use: drop the historical price rds files into "Price_data" folder
# Run the code and get the basket option price
# For new data download from HKEX website:
# drop the downloaded excel files into "Price_data" folder
# uncomment and run the "Save excel as rds" section
#######################################

library(data.table)
library(readxl)
library(stringr)
library(doParallel)
library(doRNG)
registerDoParallel(cores=detectCores())
#####
# Directory setup
mainDir = "E:/Yoyo Chan/Documents/FINA4354 Financial engineering/Group project/"
setwd(mainDir)
dataDir = paste(mainDir , "Price_data/", sep="")
#####

#######################################
## Save excel as rds 
files = list.files(dataDir,pattern = ".xlsx")
for (file in files) {
  df = read_excel(paste(dataDir,file,sep=""))
  filename = strsplit(file,".xlsx")[1]
  setDT(df)
  df = df[Time > "2018/12/31" & Time<"2020/01/01",]
  saveRDS(df, paste(dataDir,filename,".rds",sep="") )
}

#######################################

