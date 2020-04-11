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
mainDir = "/Users/varunnagar/Desktop/Option-Pricing-master/"
setwd(mainDir)
dataDir = paste(mainDir , "Price_data/", sep="")
#####

#######################################
### Save excel as rds
# 
# files = list.files(dataDir,pattern = ".xlsx")
# for (file in files) {
#   df = read_excel(paste(dataDir,file,sep=""))
#   filename = strsplit(file,".xlsx")[1]
#   saveRDS(df, paste(dataDir,filename,".rds",sep="") )
# }

#######################################
### Global Variables

annual = 252
d = 1000
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

# Inner join the two tables so that they have the same length
# dfm = Reduce(function(...)merge(...,by="date"),df)

# Get annualized volatility and last price
sd = unlist(lapply(df,function(df)sd(df$ret)/sqrt(1/annual)))
last.price = unlist(lapply(df,function(df)last(df$Closed_Price)))
K <- mean(last.price)               # strike price


# Create covariance matrix
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
cov.matrix = cov(df.ret)*annual
# Cholesky decomposition
chol.decomp = chol(cov.matrix)


## create realizations of two correlated normal random variables 
stdNormal <- matrix(rnorm(d * num.files), nrow = num.files)
corrNormal <- t(chol.decomp) %*% stdNormal # C'Z
df.wt = data.table(t(corrNormal))

mean.col.names = list()
for (i in 1:num.files) {
  ds = paste("ds.",i,sep="")
  ret = paste("ret.",i,sep="")
  df.wt[, (ds):= (r-(1/2)*sd[i]^2)*T+sqrt(T)*get(ret)]
  st = paste("st.",i,sep="")
  df.wt[, (st) := last.price[i]*exp(get(ds))]
  mean.col.names[i] = st
}
mean.col.names = unlist(mean.col.names)
df.wt$mean.ret = rowMeans(df.wt[,..mean.col.names])
df.wt[, discounted.payoff.call := exp(-r*T)*pmax(mean.ret-K,0)]
df.wt[, discounted.payoff.put := exp(-r*T)*pmax(K-mean.ret,0)]

# Get option price from simulated discounted payoff
call.price = get_price(df.wt,call=TRUE)
put.price = get_price(df.wt,call=FALSE)

cat("Call Price Estimate:",call.price, "\n")
cat("Put Price Estimate:",put.price, "\n")
cat("Standard Error:", round(sd(df.wt$discounted.payoff.call) / sqrt(d), 4), "\n")
cat("Standard Error:", round(sd(df.wt$discounted.payoff.put) / sqrt(d), 4), "\n")

##################################################
theta <- sd      # Long-run mean.
kappa <- 0.5               # Speed of mean reversion.
xi <- 0.5                # Vol of vol.

nu <- s <- matrix(0, nrow=m+1,ncol=num.files) # Initialize vol and stock price.

nu[1,] <- theta
s[1, ] <- last.price
f <- rep(0, d)           # Initialize discounted payoff.

# system.time({
#   out <- foreach(j=1:d, .combine = c) %dopar% {
#     dW1 <- matrix(sqrt(delta.t) * rnorm(num.files*m),nrow=m,ncol = num.files)
#     dW2 <- matrix(sqrt(delta.t) * rnorm(num.files*m),nrow=m,ncol = num.files)
#     for (i in 1:m) { # cycle through time
#       for (k in 1:num.files) {
#         ds <- r*s[i,k]*delta.t + sqrt(nu[i,k])*s[i,k]*dW1[i,k]
#         dnu <- kappa*(theta[k]-nu[i,k])*delta.t + xi*sqrt(nu[i,k])*dW2[i,k]
#         s[i + 1,k] <- s[i,k] + ds
#         nu[i + 1,k] <- max(nu[i,k] + dnu, 0) 
#       }
#     }
#     exp(-r * T) * max(mean(s[m+1,]) - K, 0)
#   }
# })
# print(paste("Basket Option Price Estimate:",round(mean(out),4)))

### tester
print(K)
L.call = 2.0*K
L.put = 0.50*K
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
system.time({
  out <- foreach(j=1:d, .combine = "rbind") %dorng% {
    dW1 <- matrix(sqrt(delta.t) * rnorm(num.files*m),nrow=m,ncol = num.files)
    dW2 <- matrix(sqrt(delta.t) * rnorm(num.files*m),nrow=m,ncol = num.files)
    for (i in 1:m) { # cycle through time
      for (k in 1:num.files) {
        ds <- r*s[i,k]*delta.t + sqrt(nu[i,k])*s[i,k]*dW1[i,k]
        dnu <- kappa*(theta[k]-nu[i,k])*delta.t + xi*sqrt(nu[i,k])*dW2[i,k]
        s[i + 1,k] <- s[i,k] + ds
        nu[i + 1,k] <- max(nu[i,k] + dnu, 0) # Ensure non-negative 'nu'.
      }
      if(mean(s[i+1,]) < L.put){
        if(down_in.put.active == FALSE){
          down_in.put.active = TRUE
        }
      }
      if(mean(s[i+1,]) > L.call){
        if(up_in.call.active == FALSE){
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
    list(price1,price2,exp(-r * T) * max(mean(s[m+1,]) - K, 0))
  }
})
print(paste("Basket Option Price Estimate:",round(mean(unlist(out[,3])),4)))
print(paste("Basket Option Price Estimate (Up-In Call):",round(mean(unlist(out[,1])),4)))
print(paste("Basket Option Price Estimate (Down-In Put):",round(mean(unlist(out[,2])),4)))
