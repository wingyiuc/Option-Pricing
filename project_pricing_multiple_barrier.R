#######################################
### Code for Basket Barrier In-Options

# How to use: drop the historical price rds files into "Price_data" folder
# Adjust parameters in ***User edit area***
# Run the code and get the basket option price
# For new data download from HKEX website:
# Drop the downloaded excel files into "Price_data" folder
# Run "data preprocessing.R" to get rds files with suitable timeframe 
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
### Global Variables
# ***User edit area***
annual = 252              # Number of trading days in a year
d = 100000                # Number of simulated trials
T <- 1                    # time until expiration (in years)
m <- T * 252              # number of subintervals
delta.t <- T / m          # time per subinterval (in years)
r = 1.950/100             # Risk free rate
call.barrier.factor = 1.45 # Multiplier on strike price to calculate call barrier
put.barrier.factor = 0.7  # Multiplier on strike price to calculate put barrier
kappa <- 2                # Speed of mean reversion.
xi <- 0.9                 # Vol of vol.
set.seed(1)

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

join_ret_table = function(df){
  
  # Input: list of dataframes (3D) of stock data (date, Closed_Price, ret)
  # Output: Join return columns into one 2D dataframe
  
  df.ret = data.table(cbind(df[[1]]$date,df[[1]]$ret))
  colnames(df.ret) = c("date","ret.1")
  for (i in 2:num.files) {
    df.temp = data.table(cbind(df[[i]]$date,df[[i]]$ret))
    colnames(df.temp) = c("date",paste("ret.",i,sep=""))
    df.ret = merge(df.ret,df.temp,by="date")
  }
  setkey(df.ret,date)
  df.ret = df.ret[,!"date"]
  df.ret = sapply(df.ret,as.numeric)
  return(df.ret)
}

get_price = function(out, call=TRUE, barrier=TRUE){
  
  # input: dorng output
  # output: option pirce (Up-and-In Call as default)
  
  if (call==TRUE) {
    if (barrier == TRUE) {
      # Up-and-In Basket Call
      option.price = round(mean(unlist(out[,1])),4)
    }
    else{ 
      # Basket call
      option.price = round(mean(unlist(out[,3])),4)
    }
  }
  else{
    if (barrier == TRUE) {
      # Down-and-in Basket Put
      option.price = round(mean(unlist(out[,2])),4)
    }
    else{
      # Basket Put
      option.price = round(mean(unlist(out[,4])),4)
    }
  }
  return(option.price)
}
#######################################
# Get all files from folder
files = list.files(dataDir,pattern = "_pricing.rds")

# Get number of underlyings
num.files =length(files)

# Read rds files
df = lapply(paste(dataDir,files,sep=""),readRDS)

# Preprocess the raw table
# Compute the return from closed price
df = lapply(df,preprocess.df)

# Get annualized volatility, last price and strike price
theta = unlist(lapply(df,function(df)sd(df$ret)/sqrt(1/annual)))  # long term vol
last.price = unlist(lapply(df,function(df)last(df$Closed_Price))) # last prices
K <- mean(last.price)                                             # strike price

# Join tables so that return columns have same length
df.ret = join_ret_table(df)

# Covariance matrix
cov.matrix = cov(df.ret)*annual # Annualized volatility 
# Cholesky decomposition
chol.decomp = chol(cov.matrix)

# Initialize vol and stock price.
nu <- s <- matrix(0, nrow=m+1,ncol=num.files) 
nu[1,] <- theta          # First vol is current vol
s[1, ] <- last.price     # First price is current price

# Get barriers 
L.call = call.barrier.factor*K
L.put = put.barrier.factor*K

# Initialize state of options
# The options are inactive until barriers hit
if (L.put < mean(last.price)){
  down_in.put.active = FALSE
}else{
  down_in.put.active = TRUE
}
if (L.call < mean(last.price)){
  up_in.call.active = TRUE
}else{
  up_in.call.active = FALSE
}

### Small Time Step Monete Carlo Simulation
# Volatility: Heston Model
start_time <- Sys.time()
system.time({
  out <- foreach(j=1:d, .combine = "rbind") %dorng% {
    stdNormal <- matrix(sqrt(delta.t) * rnorm(num.files*m),nrow = num.files)
    corrNormal <- t(chol.decomp) %*% stdNormal # C'Z
    dW1 <- t(corrNormal) # simulated correlated returns 
    dW2 <- matrix(sqrt(delta.t) * rnorm(num.files*m),nrow=m,ncol = num.files)
    for (i in 1:m) { # cycle through time
      for (k in 1:num.files) { # cycle through underlyings
        ds <- r*s[i,k]*delta.t + sqrt(nu[i,k])*s[i,k]*dW1[i,k]
        dnu <- kappa*(theta[k]-nu[i,k])*delta.t + xi*sqrt(nu[i,k])*dW2[i,k]
        s[i + 1,k] <- s[i,k] + ds
        nu[i + 1,k] <- max(nu[i,k] + dnu, 0) # Ensure non-negative 'nu'.
      }
      if(mean(s[i+1,]) < L.put){
        if(down_in.put.active == FALSE){
          # Activate if barrier crossed
          down_in.put.active = TRUE
        }
      }
      if(mean(s[i+1,]) > L.call){
        if(up_in.call.active == FALSE){
          # Activate if barrier crossed
          up_in.call.active = TRUE
        }
      }
    }
    price1 <- price2 <- 0
    if(up_in.call.active == TRUE){
      price1 = exp(-r * T) * max(mean(s[m+1,]) - K, 0)
    }
    if(down_in.put.active == TRUE){
      price2 = exp(-r * T) * max(K-mean(s[m+1,]), 0)
    }
    price3 = exp(-r * T) * max(mean(s[m+1,]) - K, 0)
    price4 = exp(-r * T) * max(K - mean(s[m+1,]), 0)
    list(price1,price2,price3,price4)
  }
})
end_time <- Sys.time()
print(paste("Used time for simulation:",end_time - start_time,"mins"))
print(paste("Basket Call Price Estimate:",get_price(out, call=TRUE, barrier = FALSE )))
print(paste("Basket Put Price Estimate:",get_price(out, call=FALSE, barrier = FALSE )))
print(paste("Basket Up-and-In Call Price Estimate:",get_price(out, call=TRUE, barrier = TRUE )))
print(paste("Basket Down-and-In Put Price Estimate:",get_price(out, call=FALSE, barrier = TRUE )))

###################################################
# Graphing a one-year simulation
stdNormal <- matrix(sqrt(delta.t) * rnorm(num.files*m),nrow = num.files)
corrNormal <- t(chol.decomp) %*% stdNormal # C'Z
dW1 <- t(corrNormal)
dW2 <- matrix(sqrt(delta.t) * rnorm(num.files*m),nrow=m,ncol = num.files)
for (i in 1:m) { # cycle through time
  for (k in 1:num.files) {
    ds <- r*s[i,k]*delta.t + sqrt(nu[i,k])*s[i,k]*dW1[i,k]
    dnu <- kappa*(theta[k]-nu[i,k])*delta.t + xi*sqrt(nu[i,k])*dW2[i,k]
    s[i + 1,k] <- s[i,k] + ds
    nu[i + 1,k] <- max(nu[i,k] + dnu, 0) # Ensure non-negative 'nu'.
  }
}
# Basket simulation 
s.m = rowMeans(s)
plot(s.m, type = 'l')

# Individual stocks simulation
s.plt = data.frame(s)
s.plt$id = seq.int(nrow(s.plt))
df <- s.plt %>%
  select(colnames(s.plt)) %>%
  gather(key = "variable", value = "value", -id)
ggplot(df, aes(x = id, y = value)) + 
  geom_line(aes(color = variable, linetype = "l")) + 
  scale_color_manual(values = rep(1:num.files))

