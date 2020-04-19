#######################################
### Code for single underlying
# 
# How to use: drop the historical price and backtest rds files into "data/single/" folder
# Note: Please drop rds files for ONE ticker ONLY
# Adjust parameters in ***User edit area***
# Set your directory in ***User edit area***
# Run the code and get the single underlying barrier option price
# Note: No need to specify file name in this code
# 
# Required data input:
# For pricing: file name ticker + "_pricing.rds"
# With columns: date, Closed_Price, ret ONLY
# For backtesting: file name ticker + "_backtest.rds"
# With columns: date, Closed_Price, ret ONLY
# 
# Expected output:
## For pricing: 
# at-the-money up-and-in call price, at-the-money down-and-in put price
# With vanilla call and put prices for reference 
# with confidence intervals for reference
# with delta for reference
## For backtesting: 
# graph plot of stock price and barriers
# With participation calculation, and backtest return on the PGN
# 
# For new data:
# Drop the downloaded excel/csv files into "data" folder
# Preprocess the data using "data preprocessing.R"
# Expected outputs: ticker + "_pricing.rds" & ticker + "_backtest.rds"
# With columns: date, Closed_Price, ret ONLY
# 
#######################################
### Load packages
library(data.table)
library(readxl)
library(stringr)
library(ggplot2)
library(gridExtra)

#######################################
### Directory setup
# ***User edit area***
mainDir = "C:/Users/kenneth.DESKTOP-DIPDF8F/Option-Pricing/"
setwd(mainDir)
dataDir = paste(mainDir , "data/single/", sep="")

#######################################
# Global Variables
# ***User edit area***
annual = 252                 # Number of trading days in a year
d = 1*10^7                   # Number of simulated trials
T <- 1                       # time until expiration (in years)
r = 1.950/100                # Risk free rate
bond.yield = 3.65/100        # Corporate bond yield for principal guaranteed feature
call.barrier.factor = 1.30   # Multiplier on strike price for call barrier
put.barrier.factor = 0.75    # Multiplier on strike price for put barrier
delta.h = 0.01               # Value of h used for Delta Calculation as a perentage of stock price
com.fee = 0.01               # commision fee
original.I = 1000000         # Principal for the PGN
set.seed(1)               
#######################################
# Functions

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
      sd = sd(df$payoff.call)/sqrt(d)
      option.price = round(mean(df$payoff.call), 4)
      print(paste("Confidence Interval: [ ",mean(df$payoff.call)-1.96*sd,", ",mean(df$payoff.call)+1.96*sd," ]"))
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
      sd = sd(df$payoff.call)/sqrt(d)
      option.price = round(mean(df$payoff.put), 4)
      print(paste("Confidence Interval: [ ",mean(df$payoff.call)-1.96*sd,", ",mean(df$payoff.call)+1.96*sd," ]"))
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
  # temp.mean <- price.basic[1] - adjustment.factor * price.adjusted[1]
  # temp.sd <- (price.basic[2] + adjustment.factor^2 * price.adjusted[2])^0.5
  # print(paste("Confidence Interval: ",temp.mean-1.96*temp.sd,", ",temp.mean+1.96*temp.sd))
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
  # temp.mean <- price.basic[1] + adjustment.factor * price.adjusted[1]
  # temp.sd <- (price.basic[2] + adjustment.factor^2 * price.adjusted[2])^0.5
  # print(paste("Confidence Interval: ",temp.mean-1.96*temp.sd,", ",temp.mean+1.96*temp.sd))
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
  # temp.mean <- price.basic[1] - adjustment.factor * price.adjusted[1]
  # temp.sd <- (price.basic[2] + adjustment.factor^2 * price.adjusted[2])^0.5
  # print(paste("Confidence Interval: ",temp.mean-1.96*temp.sd,", ",temp.mean+1.96*temp.sd))
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
  # temp.mean <- price.basic[1] + adjustment.factor * price.adjusted[1]
  # temp.sd <- (price.basic[2] + adjustment.factor^2 * price.adjusted[2])^0.5
  # print(paste("Confidence Interval: ",temp.mean-1.96*temp.sd,", ",temp.mean+1.96*temp.sd))
  return(price.basic[1] + adjustment.factor * price.adjusted[1])
}
### End

#######################################
# read rds files
files = list.files(dataDir,pattern = "_pricing.rds")
file = files[1]
data.df = readRDS(paste(dataDir,file,sep=""))
# data.df = readRDS(paste(dataDir,"Equities_8083.rds",sep=""))

# Get annualized volatility, last price and strike price
sigma <- sd(data.df$ret) / sqrt(1/annual)   # Annualized vola.(standard deviation)
s0 = last(data.df$Closed_Price)
K = s0 # at-the-money option
L.call = K*call.barrier.factor
vanilla.call = price.UIC(s0, K, 0, sigma, r)
vanilla.call.h_up = price.UIC(s0+s0 *(delta.h/2), K, 0, sigma, r)
vanilla.call.h_down = price.UIC(s0-s0 *(delta.h/2), K, 0, sigma, r)
delta.vanilla.call = (vanilla.call.h_up-vanilla.call.h_down)/(s0*delta.h)
UIC = price.UIC(s0, K, L.call, sigma, r)
UIC.h_up = price.UIC(s0+s0 *(delta.h/2), K, L.call, sigma, r)
UIC.h_down = price.UIC(s0-s0 *(delta.h/2), K, L.call, sigma, r)
delta.UIC.call = (UIC.h_up-UIC.h_down)/(s0*delta.h)
print(paste("Vanilla Call Price Estimate:",vanilla.call))
print(paste("Up-and-In Call Price Estimate:",UIC))


L.put = K*put.barrier.factor
vanilla.put = price.DIP(s0, K, 100000, sigma, r)
vanilla.put.h_up = price.DIP(s0+s0 *(delta.h/2), K, 100000, sigma, r)
vanilla.put.h_down = price.DIP(s0-s0 *(delta.h/2), K, 100000, sigma, r)
delta.vanilla.put = (vanilla.put.h_up-vanilla.put.h_down)/(s0*delta.h)
DIP = price.DIP(s0, K, L.put, sigma, r)
DIP.h_up = price.DIP(s0+s0 *(delta.h/2), K, L.put, sigma, r)
DIP.h_down = price.DIP(s0-s0 *(delta.h/2), K, L.put, sigma, r)
delta.DIP.put = (DIP.h_up-DIP.h_down)/(s0*delta.h)
print(paste("Vanilla Put Price Estimate:",vanilla.put))
print(paste("Down-and-In Put Price Estimate:",DIP))

print(paste("Delta of Vanilla Straddle: ",delta.UIC.call+delta.DIP.put))
print(paste("Delta of Barrier Option Straddle: ",delta.vanilla.call+delta.vanilla.put))

#######################################
### Backtesting Performance

#######################################
### Functions

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
files = list.files(dataDir,pattern = "_backtest.rds")[1]

# Get number of underlyings
num.files = 1

# Read rds files
df = lapply(paste(dataDir,files,sep=""),readRDS)
df = lapply(df,setDT)


# Graphing basket one-year historical return
if (num.files > 1) {
  df.price = join_price_table(df)
}else{
  df.price = df[[1]][,list(date,Closed_Price)]
}

df.price[, mean.price := rowMeans(.SD), by=date]
df.price$date = as.Date(df.price$date)
contract.begin.date = last(data.df$date)
call.active = any(df.price[date>contract.begin.date,mean.price] >L.call)
put.active = any(df.price[date>contract.begin.date,mean.price] < L.put)
df.price[, color := ifelse(date<contract.begin.date,'b','r')] # Optional
ggplot(df.price, aes(x=date, y=mean.price, group = color, color=color))+ geom_line() +
  geom_hline(yintercept=L.put,color='red') + geom_hline(yintercept=L.call)

###################################################
### Calculating participation rate

I = original.I*(1-com.fee)

B = exp(-bond.yield*T)*original.I
print(paste("remaining amount to invest:",I-B))

P.rate.call = (I-B)*0.5/(I*UIC)
P.rate.put = (I-B)*0.5/(I*DIP)
print(paste("participation rate for call is: ", P.rate.call*100, "%"))
print(paste("participation rate for put is: ", P.rate.put*100, "%"))
P.call = P.rate.call * I
P.put = P.rate.put * I
# print(paste("participation is: ", P))

###################################################
### Backtested payoff
begin.price = K
end.price = last(df.price$mean.price)
# Assuming options active
call.payoff = ifelse(call.active,max(end.price - begin.price,0),0)
put.payoff = ifelse(put.active,max(begin.price - end.price,0),0)
contract.payoff = P.call*call.payoff + P.put*put.payoff + original.I
contract.ret = contract.payoff/original.I-1
print(paste("Options payoff:",P.call*call.payoff + P.put*put.payoff))
print(paste("Options return:",(P.call*call.payoff + P.put*put.payoff)/(I-B)*100,"%"))
print(paste("Contract payoff:", contract.payoff))
print(paste("Contract return:", contract.ret*100,"%"))
print(paste("Portfolio return:", (end.price/begin.price-1)*100,"%"))

###################################################
### Producing better visualization

# Give a name to the ggplot object that has the return plot
plot_option <- ggplot(df.price, aes(x=date, y=mean.price, group = color, color=color))+ geom_line() +
  geom_hline(yintercept=L.put,color='red') + geom_hline(yintercept=L.call) + theme_minimal() + ggtitle("Mean Price over time")+labs(x = "Date", y = "Mean Price")

# Create a dataframe for the returns (Contract, Option and Portfolio)
item <- c("Contract","Portfolio")
item_return <- c(contract.ret*100,(end.price/begin.price-1)*100)
df_plot <- data.frame(item,item_return)

# verify that the dataframe is correct
df_plot

# Create a barplot of the returns
plot_bar <- ggplot(data = df_plot, aes(x= item, y = item_return, fill = item)) + geom_bar(stat = "identity") + theme_minimal()+
  ggtitle("Bar Plot of Returns")+labs(x = "Item", y = "Return")

# Arrange the plot_option and plot_bar into a grid
grid_combined <- grid.arrange(plot_option,plot_bar, nrow = 1,ncol = 2)
grid_combined
