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
library(ggplot2)
library(tidyverse)
library(doParallel)
library(doRNG)
registerDoParallel(cores=detectCores())
#######################################
# Directory setup
mainDir = "E:/Yoyo Chan/Documents/FINA4354 Financial engineering/Group project/"
setwd(mainDir)
dataDir = paste(mainDir , "Price_data/", sep="")

#######################################
### Global Variables

annual = 252
d = 100000
T <- 1                   # time until expiration (in years)
m <- T * 252             # number of subintervals
delta.t <- T / m         # time per subinterval (in years)
r = 1.950/100
set.seed(2)
#######################################
### Functions
preprocess.df = function(df){
  
  # Input: dataframe from rds files
  # Output: datatable with date, Closed_Price and ret (return)
  
  setDT(df)
  names(df) = str_replace_all(names(df)," ","_")
  df = df[,list(date=Time,Closed_Price=as.numeric(Closed_Price)),]
  df = df[,ret:= log(Closed_Price)-log(shift(Closed_Price,type="lag",n=1)),]
  df = na.omit(df)
  setkey(df,"date")
  return(df)
}

get_price = function(df, call=TRUE){
  
  # input: df with payoff table
  # output: option pirce (Call as default, put otherwise)
  if (call==TRUE) {
    option.price = round(mean(df$discounted.payoff.call), 4)
  }
  else{
    option.price = round(mean(df$discounted.payoff.put), 4)
  }
  return(option.price)
}
#######################################
# read rds files

files = list.files(dataDir,pattern = ".rds")
num.files =length(files)
fnames = gsub("Equities_*","",files)
fnames = gsub("*.rds","",fnames)

df = lapply(paste(dataDir,files,sep=""),readRDS)

# Preprocess the raw table
# Compute the return from closed price
df = lapply(df,preprocess.df)

# Get annualized volatility and last price
sd = unlist(lapply(df,function(df)sd(df$ret)/sqrt(1/annual)))
last.price = unlist(lapply(df,function(df)last(df$Closed_Price)))
K <- mean(last.price)               # strike price


# Join tables so that they have same length
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

##################################################
theta <- sd      # Long-run mean.
kappa <- 3               # Speed of mean reversion.
xi <- 0.5                # Vol of vol.

nu <- s <- matrix(0, nrow=m+1,ncol=num.files) # Initialize vol and stock price.

nu[1,] <- theta
s[1, ] <- last.price

# Covariance matrix
cov.matrix = cov(df.ret)*m 
# Cholesky decomposition
chol.decomp = chol(cov.matrix)


### tester
L = 1.5*K
if (L < mean(last.price)){
  up_out = FALSE
  down_out = TRUE
  up_in = TRUE
  down_in = FALSE
}else{
  up_out = TRUE
  down_out = FALSE
  up_in = FALSE
  down_in = TRUE
}
system.time({
  out <- foreach(j=1:d, .combine = "rbind") %dorng% {
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
      if(mean(s[i+1,]) < L){
        if(down_out == TRUE){
          down_out = FALSE
        }
        if(down_in == FALSE){
          down_in = TRUE
        }
      }else if(mean(s[i+1,]) > L){
        if(up_out == TRUE){
          up_out = FALSE
        }
        if(up_in == FALSE){
          up_in = TRUE
        }
      }
    }
    price1 <- price2 <- price3 <- price4 <- 0
    if(down_out == TRUE){
      price1 = exp(-r * T) * max(mean(s[m+1,]) - K, 0)
    }
    if(up_out == TRUE){
      price2 = exp(-r * T) * max(mean(s[m+1,]) - K, 0)
    }
    if(down_in == TRUE){
      price3 = exp(-r * T) * max(mean(s[m+1,]) - K, 0)
    }
    if(up_in == TRUE){
      price4 = exp(-r * T) * max(mean(s[m+1,]) - K, 0)
    }
    list(price1,price2,price3,price4,exp(-r * T) * max(mean(s[m+1,]) - K, 0))
  }
})
print(paste("Basket Option Price Estimate:",round(mean(unlist(out[,5])),8)))
print(paste("Basket Option Price Estimate (Down-Out):",round(mean(unlist(out[,1])),8)))
print(paste("Basket Option Price Estimate (Up-Out):",round(mean(unlist(out[,2])),8)))
print(paste("Basket Option Price Estimate (Down-In):",round(mean(unlist(out[,3])),8)))
print(paste("Basket Option Price Estimate (Up-In):",round(mean(unlist(out[,4])),8)))

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
# plot
s.plt = data.frame(s)
s.plt$id = seq.int(nrow(s.plt))
df <- s.plt %>%
  select(colnames(s.plt)) %>%
  gather(key = "variable", value = "value", -id)
head(df)
# Visualization
ggplot(df, aes(x = id, y = value)) + 
  geom_line(aes(color = variable, linetype = "l")) + 
  scale_color_manual(values = rep(1:num.files))
