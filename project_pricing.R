library(data.table)
library(readxl)
library(stringr)
#####
mainDir = "/Users/varunnagar/Desktop/Option-Pricing-master/"
setwd(mainDir)
dataDir = paste(mainDir , "Price_data/", sep="")
#####

#######################################
# Save excel as rds
# 
# files = list.files(dataDir,pattern = ".xlsx")
# for (file in files) {
#   df = read_excel(paste(dataDir,file,sep=""))
#   filename = strsplit(file,".xlsx")[1]
#   saveRDS(df, paste(dataDir,filename,".rds",sep="") )
# }

#######################################
# Global Variables
       
annual = 252
d = 100000
T <- 1                   # time until expiration (in years)
m <- T * 252             # number of subintervals
delta.t <- T / m         # time per subinterval (in years)
r = 1.950/100
### Addition
L <- 200
### End
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
get_payoff_table = function(df,s1.last.price, s2.last.price,type_basic,type_adjust){
  
  # input: df with two columns of correlated Wiener processes
  # Output: df with correlated Wiener processes, returns, simulated stock prices
  # mean returns and discounted payoff
  
  df[, ret1 := (r-(1/2)*s1.sd^2)*T+sqrt(T)*df3.ret.x]
  df[, ret2 := (r-(1/2)*s2.sd^2)*T+sqrt(T)*df3.ret.y]
  df[, s1.t := s1.last.price*exp(ret1)]
  df[, s2.t := s2.last.price*exp(ret2)]
  df[, mean.ret := mean(c(s1.t,s2.t)), by=1:nrow(df)]
  df[, mean.adj.ret := L^2/mean(c(s1.t,s2.t)), by=1:nrow(df)]
  df[, mean.ret.barrier := option_type(mean.ret,L,type_basic), by=1:nrow(df)] 
  df[, mean.adj.ret.barrier := option_type(mean.adj.ret,L,type_adjust), by=1:nrow(df)]
  df[, discounted.payoff.call := exp(-r*T)*pmax(mean.ret-K,0)]
  df[, discounted.payoff.put := exp(-r*T)*pmax(K-mean.ret,0)]
  df[, discounted.payoff.call.barrier := exp(-r*T)*pmax(mean.ret.barrier-K,0)]
  df[, discounted.payoff.call.barrier.adj := exp(-r*T)*pmax(mean.adj.ret.barrier-K,0)]
  return(df)
}
### End
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
### Addition

option_type = function(num,L,type){
  if(type == 1){
    if(num <= L ){return(0)} else{return(num)}
  }
  else{
    if(num > L ){return(0)} else{return(num)}
  }
}
get_price_new = function(df, call=TRUE){
  if (call==TRUE) {
    option.price = round(mean(df$discounted.payoff.call.barrier), 4)
  }
  else{
    option.price = round(mean(df$discounted.payoff.call.barrier.adj), 4)
  }
  return(option.price)
}
price.DO <- function() {
  last_price = mean(c(s1.last.price,s2.last.price))
  #if(K >= L){ phi = function(s) s }
  #else {phi = function(s) max(s - K, 0)}
  if ( last_price <= L) return(0)                
  stdNormal <- matrix(rnorm(d * 2), nrow = 2)
  corrNormal <- t(chol.decomp) %*% stdNormal # C'Z
  df = data.table(t(corrNormal))
  df = get_payoff_table(df, s1.last.price, s2.last.price,1,1)
  adjustment.factor <- (L / last_price)^(2 * (r - (1/2) * abs(joint.cov)) / abs(joint.cov))
  price.basic <- get_price_new(df,TRUE)
  price.adjusted <- get_price_new(df,FALSE)
  return(price.basic - adjustment.factor * price.adjusted)
}

price.UIC <- function() {
  last_price = mean(c(s1.last.price,s2.last.price))
  #if(K >= L){ phi = function(s) s }
  #else {phi = function(s) max(s - K, 0)}
  stdNormal <- matrix(rnorm(d * 2), nrow = 2)
  corrNormal <- t(chol.decomp) %*% stdNormal # C'Z
  df = data.table(t(corrNormal))
  df = get_payoff_table(df, s1.last.price, s2.last.price,1,2)
  if ( last_price >= L) return(get_price(df,call=TRUE))    
  adjustment.factor <- (L / last_price)^(2 * (r - (1/2) * abs(joint.cov)) / abs(joint.cov))
  price.basic <- get_price_new(df,TRUE)
  price.adjusted <- get_price_new(df,FALSE)
  print(get_price(df,TRUE))
  return(price.basic + adjustment.factor * price.adjusted)
}
### End

#######################################
# read rds files
# These are two stocks from the GEM board
# Note that these two stocks did not pay dividend for the past year
#df1 = readRDS(paste(dataDir,"Equities_8083.rds",sep=""))
#df2 = readRDS(paste(dataDir,"Equities_8631.rds",sep=""))
df1 = readRDS(paste(dataDir,"Equities_700.rds",sep=""))
df2 = readRDS(paste(dataDir,"Equities_2388.rds",sep=""))


# Preprocess the raw table
# Compute the return from closed price
df1 = preprocess.df(df1)
df2 = preprocess.df(df2)
# Inner join the two tables so that they have the same length
df3 = merge(df1,df2,by='date')


# Get annualized volatility and last price
s1.sd <- sd(df3$ret.x) / sqrt(1/annual)   # Annualized vola.(standard deviation)
s2.sd <- sd(df3$ret.y) / sqrt(1/annual)
s1.last.price = last(df3$Closed_Price.x)
s2.last.price = last(df3$Closed_Price.y)
w = matrix(0.5,nrow = 2, ncol = 1)
K <- mean(c(s1.last.price,s2.last.price))                # strike price
print(K)
# Create covariance matrix
cov.matrix = cov(data.frame(df3$ret.x,df3$ret.y))*annual
joint.cov = t(w)%*%cov.matrix%*%w
# Cholesky decomposition
chol.decomp = chol(cov.matrix)


## create realizations of two correlated normal random variables 
stdNormal <- matrix(rnorm(d * 2), nrow = 2)
corrNormal <- t(chol.decomp) %*% stdNormal # C'Z
df = data.table(t(corrNormal))

# Calculate payoff from correlated random variables
df = get_payoff_table(df, s1.last.price, s2.last.price,1,1)
# Get option price from simulated discounted payoff
call.price = get_price(df,call=TRUE)
put.price = get_price(df,call=FALSE)
price.UIC()
price.DO()
#cat("Call Price Estimate:",call.price, "\n")
#cat("Put Price Estimate:",put.price, "\n")
#cat("Standard Error:", round(sd(df$discounted.payoff.call) / sqrt(d), 4), "\n")
#cat("Standard Error:", round(sd(df$discounted.payoff.put) / sqrt(d), 4), "\n")

