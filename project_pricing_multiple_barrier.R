#######################################
### Code for multiple underlyings (basket)
# 
# How to use: drop the historical price and backtest rds files into "data/multiple/" folder
# Note: Please drop rds files for the basket of tickers you wish to price ONLY
# Adjust parameters in ***User edit area***
# Set your directory in ***User edit area***
# Run the code and get the basket barrier option price
# Note: No need to specify file names in this code
# 
# Required data input:
# For pricing: file name ticker + "_pricing.rds"
# With columns: date, Closed_Price, ret ONLY
# For backtesting: file name ticker + "_backtest.rds"
# With columns: date, Closed_Price, ret ONLY
# 
# Expected output:
# For pricing: at-the-money up-and-in call price, at-the-money down-and-in put price
# With vanilla call and put prices for reference 
# with confidence intervals for reference
# For backtesting: graph plot of stock price and barriers
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
library(doParallel)
library(doRNG)
registerDoParallel(cores=detectCores())

#######################################
### Directory setup
# ***User edit area***
mainDir = "C:/Users/kenneth.DESKTOP-DIPDF8F/Option-Pricing/"

setwd(mainDir)
dataDir = paste(mainDir , "data/multiple/", sep="")

#######################################
### Global Variables
# ***User edit area***
annual = 252               # Number of trading days in a year
d = 100000                 # Number of simulated trials
T <- 1                     # time until expiration (in years)
m <- T * 252               # number of subintervals
delta.t <- T / m           # time per subinterval (in years)
r = 1.950/100              # Risk free rate
bond.yield = 3.6/100       # Corporate bond yield for principal guaranteed feature
call.barrier.factor = 1.3  # Multiplier on strike price to calculate call barrier
put.barrier.factor = 0.75  # Multiplier on strike price to calculate put barrier
kappa <- 2                 # Speed of mean reversion.
xi <- 0.9                  # Vol of vol.
original.I = 1000000       # Principal for the PGN
com.fee = 0.01             # commision fee
set.seed(1)

#######################################
### Pricing options

#######################################
### Functions

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
      stdev = sd(unlist(out[,1]))/sqrt(d)
      print(paste('lower bound:',option.price - 1.96*stdev))
      print(paste('upper bound:',option.price + 1.96*stdev))
    }
    else{ 
      # Basket call
      option.price = round(mean(unlist(out[,3])),4)
      stdev = sd(unlist(out[,3]))/sqrt(d)
      print(paste('lower bound:',option.price - 1.96*stdev))
      print(paste('upper bound:',option.price + 1.96*stdev))
    }
  }
  else{
    if (barrier == TRUE) {
      # Down-and-in Basket Put
      option.price = round(mean(unlist(out[,2])),4)
      stdev = sd(unlist(out[,2]))/sqrt(d)
      print(paste('lower bound:',option.price - 1.96*stdev))
      print(paste('upper bound:',option.price + 1.96*stdev))
    }
    else{
      # Basket Put
      option.price = round(mean(unlist(out[,4])),4)
      stdev = sd(unlist(out[,4]))/sqrt(d)
      print(paste('lower bound:',option.price - 1.96*stdev))
      print(paste('upper bound:',option.price + 1.96*stdev))
    }
  }
  return(option.price)
}
#######################################
### Main code for pricing

# Get all files from folder
files = list.files(dataDir,pattern = "_pricing.rds")

# Get number of underlyings
num.files =length(files)

# Read rds files
df = lapply(paste(dataDir,files,sep=""),readRDS)


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

UIC = get_price(out, call=TRUE, barrier = TRUE ) # Price of Up In Call
DIP = get_price(out, call=FALSE, barrier = TRUE ) # Price of DOwn In Put
print(paste("Used time for simulation:",end_time - start_time,"mins"))
print(paste("Basket Call Price Estimate:",get_price(out, call=TRUE, barrier = FALSE )))
print(paste("Basket Put Price Estimate:",get_price(out, call=FALSE, barrier = FALSE )))
print(paste("Basket Up-and-In Call Price Estimate:",get_price(out, call=TRUE, barrier = TRUE )))
print(paste("Basket Down-and-In Put Price Estimate:",get_price(out, call=FALSE, barrier = TRUE )))

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
files = list.files(dataDir,pattern = "_backtest.rds")

# Get number of underlyings
num.files =length(files)

# Read rds files
df.pricing = lapply(paste(dataDir,files,sep=""),readRDS)
df.pricing = lapply(df.pricing,setDT)

### Graphing basket one-year historical return

if (num.files > 1) {
  df.price = join_price_table(df.pricing)
}else{
  df.price = df[[1]][,list(date,Closed_Price)]
}

df.price[, mean.price := rowMeans(.SD), by=date]
df.price$date = as.Date(df.price$date)
contract.begin.date = last(df[[1]]$date)
call.active = any(df.price[date>contract.begin.date,mean.price] >L.call)
put.active = any(df.price[date>contract.begin.date,mean.price] < L.put)
df.price[, color := ifelse(date<contract.begin.date,'b','r')] # Optional
ggplot(df.price, aes(x=date, y=mean.price, group = color, color=color))+ geom_line() +
  geom_hline(yintercept=L.put,color='red') + geom_hline(yintercept=L.call)


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


### Backtested payoff

begin.price = K
end.price = last(df.price$mean.price)
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

