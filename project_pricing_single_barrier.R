#######################################
### Code for single Barrier In-Options

# How to use: drop the historical price rds files into "Price_data" folder
# Adjust parameters in ***User edit area***
# Run the code and get the single option price
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
mainDir = "/Users/varunnagar/Desktop/Option-Pricing-master/"
setwd(mainDir)
dataDir = paste(mainDir , "Price_data/", sep="")

#######################################
# Global Variables

annual = 252
d = 1*10^6
T <- 1                   # time until expiration (in years)
r = 1.950/100
call.barrier.factor = 1.45 # Multiplier on strike price for call barrier
put.barrier.factor = 0.7  # Multiplier on strike price for put barrier
delta.h = 0.01 # Value of h used for Delta Calculation as a perentage of stock price
#######################################
# Functions
preprocess.df = function(df){
  
  # Input: dataframe from rds files
  # Output: datatable with date, Closed_Price and ret (return)
  
  setDT(df)
  names(df) = str_replace_all(names(df)," ","_")
  df = df[,list(date=Time,Closed_Price=as.numeric(Closed_Price)),]
  df = df[,ret:= log(Closed_Price)-log(shift(Closed_Price,type="lag",n=1)),]
  df = na.omit(df)
  return(df)
}

### Addition in Function
get_payoff_table = function(df,s0, K, L, sigma, r, T, type_basic,type_adjust){
  
  # input: df with one column of Wiener processes
  # Output: df with Wiener processes, simulated returns, simulated stock prices
  # mean returns and discounted payoff
  
  df[, r.t := (r-(1/2)*sigma^2)*T+sigma*dW]
  df[, s.t := s0*exp(r.t)]
  df[, adj.s.t := L^2/s.t]
  df[, s.t.barrier := phi(s.t, L, type=type_basic)] 
  df[, adj.s.t.barrier := phi(adj.s.t, L, type=type_adjust)]
  df[, payoff.call := exp(-r*T)*pmax(s.t-K,0)]
  df[, payoff.put := exp(-r*T)*pmax(K-s.t,0)]
  df[, payoff.call.barrier := exp(-r*T)*pmax(s.t.barrier-K,0)]
  df[, payoff.call.barrier.adj := exp(-r*T)*pmax(adj.s.t.barrier-K,0)]
  df[, payoff.put.barrier := ifelse(s.t.barrier==0,0,exp(-r*T)*pmax(K-s.t.barrier,0))]
  df[, payoff.put.barrier.adj := ifelse(adj.s.t.barrier==0,0,exp(-r*T)*pmax(K-adj.s.t.barrier,0))]
  return(df)
}

phi = function(S,L,type){
  # Input: S, L, type
  # Output: L chopped off claim
  if(type == 1){
    return(ifelse(S<L, S, 0)) 
  }
  else{
    return(ifelse(S>L, S, 0)) 
  }
}

get_price = function(df, call=TRUE, vanilla=TRUE ,basic=TRUE){
  if (call==TRUE) {
    if (vanilla == TRUE) {
      option.price = round(mean(df$payoff.call), 4)
      return(option.price)
    }
    else{
      if (basic==TRUE) {
        std.err = var(df$payoff.call.barrier)
        option.price = round(mean(df$payoff.call.barrier), 4)
      }
      else{
        std.err = var(df$payoff.call.barrier.adj)
        option.price = round(mean(df$payoff.call.barrier.adj), 4)
      }
    }
  }
  else{ # Put
    if (vanilla == TRUE) {
      option.price = round(mean(df$payoff.put), 4)
      return(option.price)
    }
    else{
      if (basic==TRUE) {
        std.err = var(df$payoff.put.barrier)
        option.price = round(mean(df$payoff.put.barrier), 4)
      }
      else{
        std.err = var(df$payoff.put.barrier.adj)
        option.price = round(mean(df$payoff.put.barrier.adj), 4)
      }
    }
  }
  return(list(option.price,std.err))
}
price.DOC <- function(s0, K, L, sigma, r) {
  # Pricing Down-and-Out Call
  if ( s0 <= L) return(0)
  df = data.table(sqrt(T)*rnorm(d))
  df = df[, list(dW=V1)]
  df = get_payoff_table(df,s0, K, L, sigma, r, T, type_basic=2,type_adjust=2)
  adjustment.factor <- (L / s0)^(2 * (r - (1/2) * sigma^2) / sigma^2)
  price.basic <- get_price(df, call=TRUE, vanilla=FALSE, basic=TRUE)
  price.basic <- unlist(price.basic)
  price.adjusted <- get_price(df, call=TRUE, vanilla=FALSE, basic=FALSE)
  price.adjusted <- unlist(price.adjusted)
  temp.mean <- price.basic[1] - adjustment.factor * price.adjusted[1]
  temp.sd <- (price.basic[2] + adjustment.factor^2 * price.adjusted[2])^0.5
  print(paste("Confidence Interval: ",temp.mean-1.96*temp.sd,", ",temp.mean+1.96*temp.sd))
  return(price.basic[1] - adjustment.factor * price.adjusted[1])
}

price.UIC <- function(s0, K, L, sigma, r) {
  # Input: df, s0, K, L, sigma, r, T
  # Output: price of Up-and-In call
  df = data.table(sqrt(T)*rnorm(d))
  df = df[, list(dW=V1)]
  df = get_payoff_table(df,s0, K, L, sigma, r, T, type_basic=2,type_adjust=1)
  if ( s0 >= L) return(get_price(df, call=TRUE, vanilla=TRUE)) # Get rid of easy case.
  adjustment.factor <- (L / s0)^(2 * (r - (1/2) * sigma^2) / sigma^2)
  price.basic <- get_price(df, call=TRUE, vanilla=FALSE, basic=TRUE)
  price.basic = unlist(price.basic)
  price.adjusted <- get_price(df, call=TRUE, vanilla=FALSE, basic=FALSE)
  price.adjusted <- unlist(price.adjusted)
  temp.mean <- price.basic[1] + adjustment.factor * price.adjusted[1]
  temp.sd <- (price.basic[2] + adjustment.factor^2 * price.adjusted[2])^0.5
  print(paste("Confidence Interval: ",temp.mean-1.96*temp.sd,", ",temp.mean+1.96*temp.sd))
  return(price.basic[1] + adjustment.factor * price.adjusted[1])
}

price.UOP <- function(s0, K, L, sigma, r) {
  # Input: df, s0, K, L, sigma, r, T
  # Output: price of Up-and-Out Put
  if ( s0 >= L) return(0)
  df = data.table(sqrt(T)*rnorm(d))
  df = df[, list(dW=V1)]
  df = get_payoff_table(df,s0, K, L, sigma, r, T, type_basic=1,type_adjust=1)
  adjustment.factor <- (L / s0)^(2 * (r - (1/2) * sigma^2) / sigma^2)
  price.basic <- get_price(df, call=FALSE, vanilla=FALSE, basic=TRUE)
  price.basic <- unlist(price.basic)
  price.adjusted <- get_price(df, call=FALSE, vanilla=FALSE, basic=FALSE)
  price.adjusted <- unlist(price.adjusted)
  temp.mean <- price.basic[1] - adjustment.factor * price.adjusted[1]
  temp.sd <- (price.basic[2] + adjustment.factor^2 * price.adjusted[2])^0.5
  print(paste("Confidence Interval: ",temp.mean-1.96*temp.sd,", ",temp.mean+1.96*temp.sd))
  return(price.basic[1] - adjustment.factor * price.adjusted[1])
}

price.DIP <- function(s0, K, L, sigma, r) {
  # Input: df, s0, K, L, sigma, r, T
  # Output: price of Down-and-In Put
  df = data.table(sqrt(T)*rnorm(d))
  df = df[, list(dW=V1)]
  df = get_payoff_table(df,s0, K, L, sigma, r, T, type_basic=1,type_adjust=2)
  if ( s0 <= L) return(get_price(df, call=FALSE, vanilla=TRUE))
  adjustment.factor <- (L / s0)^(2 * (r - (1/2) * sigma^2) / sigma^2)
  price.basic <- get_price(df, call=FALSE, vanilla=FALSE, basic=TRUE)
  price.basic <- unlist(price.basic)
  price.adjusted <- get_price(df, call=FALSE, vanilla=FALSE, basic=FALSE)
  price.adjusted <- unlist(price.adjusted)
  temp.mean <- price.basic[1] + adjustment.factor * price.adjusted[1]
  temp.sd <- (price.basic[2] + adjustment.factor^2 * price.adjusted[2])^0.5
  print(paste("Confidence Interval: ",temp.mean-1.96*temp.sd,", ",temp.mean+1.96*temp.sd))
  return(price.basic[1] + adjustment.factor * price.adjusted[1])
}
### End

#######################################
# read rds files
files = list.files(dataDir,pattern = "_pricing.rds")
file = files[1]
data.df = readRDS(paste(dataDir,file,sep=""))
# data.df = readRDS(paste(dataDir,"Equities_8083.rds",sep=""))

# Preprocess the raw table
# Compute the return from closed price
data.df = preprocess.df(data.df)

# Get annualized volatility, last price and strike price
sigma <- sd(data.df$ret) / sqrt(1/annual)   # Annualized vola.(standard deviation)
s0 = last(data.df$Closed_Price)
K = s0 # at-the-money option
L = K*call.barrier.factor
vanilla.call = price.UIC(s0, K, 0, sigma, r)
vanilla.call.h_up = price.UIC(s0+s0 *(delta.h/2), K, 0, sigma, r)
vanilla.call.h_down = price.UIC(s0-s0 *(delta.h/2), K, 0, sigma, r)
delta.vanilla.call = (vanilla.call.h_up-vanilla.call.h_down)/(s0*delta.h)
UIC = price.UIC(s0, K, L, sigma, r)
UIC.h_up = price.UIC(s0+s0 *(delta.h/2), K, L, sigma, r)
UIC.h_down = price.UIC(s0-s0 *(delta.h/2), K, L, sigma, r)
delta.UIC.call = (UIC.h_up-UIC.h_down)/(s0*delta.h)
UOP = price.UOP(s0, K, L, sigma, r)
print(paste("Vanilla Call Price Estimate:",vanilla.call))
print(paste("Up-and-In Call Price Estimate:",UIC))
print(paste("Up-and-Out Put Price Estimate:",UOP))

L = K*put.barrier.factor
vanilla.put = price.DIP(s0, K, 1000, sigma, r)
vanilla.put.h_up = price.DIP(s0+s0 *(delta.h/2), K, 1000, sigma, r)
vanilla.put.h_down = price.DIP(s0-s0 *(delta.h/2), K, 1000, sigma, r)
delta.vanilla.put = (vanilla.put.h_up-vanilla.put.h_down)/(s0*delta.h)
DOC = price.DOC(s0, K, L, sigma, r)
DIP = price.DIP(s0, K, L, sigma, r)
DIP.h_up = price.DIP(s0+s0 *(delta.h/2), K, L, sigma, r)
DIP.h_down = price.DIP(s0-s0 *(delta.h/2), K, L, sigma, r)
delta.DIP.put = (DIP.h_up-DIP.h_down)/(s0*delta.h)
print(paste("Vanilla Put Price Estimate:",vanilla.put))
print(paste("Down-and-Out Call Price Estimate:",DOC))
print(paste("Down-and-In Put Price Estimate:",DIP))

print(paste("Delta of Vanilla Straddle: ",delta.UIC.call+delta.DIP.put))
print(paste("Delta of Barrier Option Straddle: ",delta.vanilla.call+delta.vanilla.put))
