#######################################
### Load packages
library(data.table)
library(readxl)
library(stringr)
library(dplyr)
library(ggplot2)
library(future.apply)
library(doParallel)
library(gridExtra)

#######################################
### Directory setup
# ***User edit area***
mainDir = "C:/Users/kenneth.DESKTOP-DIPDF8F/Option-Pricing/"
setwd(mainDir)
dataDir = paste(mainDir , "data/single/", sep="")

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
call.barrier.factor = 1.2  # Multiplier on strike price to calculate call barrier
put.barrier.factor = 0.8   # Multiplier on strike price to calculate put barrier
kappa <- 2                 # Speed of mean reversion.
xi <- 0.9                  # Vol of vol.
original.I = 1000000       # Principal for the PGN
com.fee = 0.01             # commision fee

#######################################
### Price path simulation

#######################################
### Functions

join_ret_table = function(df){
  
  # Input: list of dataframes (3D) of stock data (date, Closed_Price, ret)
  # Output: Join return columns into one 2D dataframe
  
  if (num.files == 1) {
    df.ret = data.table(cbind(df[[1]]$date,df[[1]]$ret))
    colnames(df.ret) = c("date","ret.1")
    setkey(df.ret,date)
    df.ret = df.ret[,!"date"]
    df.ret = sapply(df.ret,as.numeric)
    return(df.ret)
  }
  
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

price_simulation = function(a){
  
  # Input: None (uses global variables)
  # Required global variables: "num.files","m","delta.t",
  # "chol.decomp","r","kappa","xi",
  # "theta", "L.put","L.call","K", "last.price"
  # Does: Simulate one price path for basket option
  # Output: vanilla & barrier call & put price from 1 trial
  
  # Initialize vol and stock price.
  nu <- s <- matrix(0, nrow=m+1,ncol=num.files) 
  nu[1,] <- theta          # First vol is current vol
  s[1, ] <- last.price     # First price is current price
  
  
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
  if (num.files > 1) {
    stdNormal = matrix(sqrt(delta.t) * rnorm(num.files*m),nrow = num.files)
    corrNormal <- t(chol.decomp) %*% stdNormal # C'Z
    dW1 <- t(corrNormal) # simulated correlated returns
  }else{
    dW1 = matrix(sqrt(delta.t) * rnorm(num.files*m),nrow=m,ncol = num.files)
  }
  
  dW2 = matrix(sqrt(delta.t) * rnorm(num.files*m),nrow=m,ncol = num.files)
  for (i in 1:m) { # cycle through time
    for (k in 1:num.files) { # cycle through underlyings
      ds <- r*s[i,k]*delta.t + sqrt(nu[i,k])*s[i,k]*dW1[i,k]
      dnu <- kappa*(theta[k]-nu[i,k])*delta.t + xi*sqrt(nu[i,k])*dW2[i,k]
      s[i + 1,k] <- s[i,k] + ds
      nu[i + 1,k] <- max(nu[i,k] + dnu, 0) # Ensure non-negative 'nu'.
    }
  }
  return(s)
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


# Get barriers 
L.call = call.barrier.factor*K
L.put = put.barrier.factor*K

s1 = price_simulation()
s2 = price_simulation()
s3 = price_simulation()
s4 = price_simulation()
s5 = price_simulation()
s6 = price_simulation()
s7 = price_simulation()
s8 = price_simulation()
s9 = price_simulation()
s10 = price_simulation()

df.graph = data.frame(cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10))
setDT(df.graph)
colnames(df.graph) = c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10")
df.graph[, s1.max:=max(s1)]
df.graph[, s1.min:=min(s1)]
df.graph[, s2.max:=max(s2)]
df.graph[, s2.min:=min(s2)]
df.graph[, s3.max:=max(s3)]
df.graph[, s3.min:=min(s3)]
df.graph[, s4.max:=max(s4)]
df.graph[, s4.min:=min(s4)]
df.graph[, s5.max:=max(s5)]
df.graph[, s5.min:=min(s5)]
df.graph[, s6.max:=max(s6)]
df.graph[, s6.min:=min(s6)]
df.graph[, s7.max:=max(s7)]
df.graph[, s7.min:=min(s7)]
df.graph[, s8.max:=max(s8)]
df.graph[, s8.min:=min(s8)]
df.graph[, s9.max:=max(s9)]
df.graph[, s9.min:=min(s9)]
df.graph[, s10.max:=max(s10)]
df.graph[, s10.min:=min(s10)]

df.graph[,c1:=ifelse(s1.min<L.put,"Put barrier hit",ifelse(s1.max>L.call,"Call barrier hit","No barrier hit"))]
df.graph[,c2:=ifelse(s2.min<L.put,"Put barrier hit",ifelse(s2.max>L.call,"Call barrier hit","No barrier hit"))]
df.graph[,c3:=ifelse(s3.min<L.put,"Put barrier hit",ifelse(s3.max>L.call,"Call barrier hit","No barrier hit"))]
df.graph[,c4:=ifelse(s4.min<L.put,"Put barrier hit",ifelse(s4.max>L.call,"Call barrier hit","No barrier hit"))]
df.graph[,c5:=ifelse(s5.min<L.put,"Put barrier hit",ifelse(s5.max>L.call,"Call barrier hit","No barrier hit"))]
df.graph[,c6:=ifelse(s6.min<L.put,"Put barrier hit",ifelse(s6.max>L.call,"Call barrier hit","No barrier hit"))]
df.graph[,c7:=ifelse(s7.min<L.put,"Put barrier hit",ifelse(s7.max>L.call,"Call barrier hit","No barrier hit"))]
df.graph[,c8:=ifelse(s7.min<L.put,"Put barrier hit",ifelse(s8.max>L.call,"Call barrier hit","No barrier hit"))]
df.graph[,c9:=ifelse(s9.min<L.put,"Put barrier hit",ifelse(s9.max>L.call,"Call barrier hit","No barrier hit"))]
df.graph[,c10:=ifelse(s10.min<L.put,"Put barrier hit",ifelse(s10.max>L.call,"Call barrier hit","No barrier hit"))]
df.graph %>% head
### plotting
ggplot(df.graph, aes(x=rep(1:nrow(df.graph))))+
  geom_line(aes(y=s1,color = df.graph[1,c1]))+
  geom_line(aes(y=s2,color = df.graph[1,c2]))+
  geom_line(aes(y=s3,color = df.graph[1,c3]))+
  geom_line(aes(y=s4,color = df.graph[1,c4]))+
  geom_line(aes(y=s5,color = df.graph[1,c5]))+
  geom_line(aes(y=s6,color = df.graph[1,c6]))+
  geom_line(aes(y=s7,color = df.graph[1,c7]))+
  geom_hline(yintercept=L.put,color='red') + 
  geom_hline(yintercept=L.call)+
  ggtitle("Simulated Price Paths")+
  labs(x = "Time", y = "Price")

#################################################
# 
# payoff = function(s,K){
#   # straddle
#   return(abs(s-K))
# }
# payoff2 = function(s,k1,k2){
#   # strangle
#   call = max(k1-s,0)
#   put = max(s-k2,0)
#   return(max(call,put))
# }
# 
# payoff3 = function(s,K,L.put,L.call){
#   price = ifelse(s<L.put,K-s,ifelse(s>L.call,s-K,0))
#   return(price)
# }
# 
# K = 50 
# K1 = 0.9*K
# K2 = 1.2*K
# L.put = 0.9*K
# L.call = 1.2*K
# s = rep(1:100)
# f.straddle = lapply(s,payoff,K)
# f.straddle = unlist(f.straddle)
# f.strangle = lapply(s,payoff2,K1,K2)
# f.strangle = unlist(f.strangle)
# f.straddle2 = lapply(s,payoff3,K,L.put,L.call)
# f.straddle2 = unlist(f.straddle2)
# payoff.diagram = data.frame(cbind(s,f.straddle,f.strangle,f.straddle2))
# setDT(payoff.diagram)
# colnames(payoff.diagram) = c("s","straddle","strangle","straddle2")
# 
# 
# ggplot(payoff.diagram, aes(x=s))+
#   geom_line(aes(y=straddle2,color="barrier straddle (barrier not crossed)"))+
#   geom_line(aes(y=straddle,color="barrier straddle (barrier crossed)"))+
#   geom_line(aes(y=strangle,color="strangle payoff"))+
#   scale_color_manual(name="Groups",values=c("red", "grey","blue"))+
#   ggtitle("Payoff diagram")+
#   labs(x = "Price", y = "Payoff")+
#   theme_minimal()

##################################################


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
  
  # Input: S (price), L (barrier), type (1 for S<L, S>L otherwise)
  # Output: L chopped off claim
  
  if(type == 1){
    return(ifelse(S<L, S, 0)) 
  }
  else{
    return(ifelse(S>L, S, 0)) 
  }
}

get_price = function(df, call=TRUE, vanilla=TRUE ,basic=TRUE, print.CI = FALSE){
  
  # Input: dataframe with payoff table, indicators for call/put, vanilla/barrier,
  # basic/adjustment, print confidence interval
  # Output price of vanilla/barrier, basic/chopped off call/put
  
  if (call==TRUE) {
    if (vanilla == TRUE) {
      sd = sd(df$payoff.call)/sqrt(d)
      option.price = round(mean(df$payoff.call), 4)
      if (print.CI==TRUE) {
        print(paste("Confidence Interval: [ ",mean(df$payoff.call)-1.96*sd,", ",mean(df$payoff.call)+1.96*sd," ]"))
      }
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
      if (print.CI==TRUE) {
        print(paste("Confidence Interval: [ ",mean(df$payoff.call)-1.96*sd,", ",mean(df$payoff.call)+1.96*sd," ]"))
      }
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


price.UIC <- function(s0, K, L, sigma, r, T, print.CI = FALSE) {
  
  # Input: df (date, ret), s0, K, L, sigma, r, T
  # Output: price of Up-and-In call
  
  df = data.table(sqrt(T)*rnorm(d))
  df = df[, list(dW=V1)]
  df = get_payoff_table(df,s0, K, L, sigma, r, T, type_basic=2,type_adjust=1)
  if ( s0 >= L) return(get_price(df, call=TRUE, vanilla=TRUE, print.CI=print.CI)) # Get rid of easy case.
  adjustment.factor <- (L / s0)^(2 * (r - (1/2) * sigma^2) / sigma^2)
  price.basic <- get_price(df, call=TRUE, vanilla=FALSE, basic=TRUE)
  price.basic = unlist(price.basic)
  price.adjusted <- get_price(df, call=TRUE, vanilla=FALSE, basic=FALSE)
  price.adjusted <- unlist(price.adjusted)
  return(price.basic[1] + adjustment.factor * price.adjusted[1])
}

price.DIP <- function(s0, K, L, sigma, r, T, print.CI = FALSE) {
  
  # Input: df(date, ret), s0, K, L, sigma, r, T
  # Output: price of Down-and-In Put
  
  df = data.table(sqrt(T)*rnorm(d))
  df = df[, list(dW=V1)]
  df = get_payoff_table(df,s0, K, L, sigma, r, T, type_basic=1,type_adjust=2)
  if ( s0 <= L) return(get_price(df, call=FALSE, vanilla=TRUE, print.CI=print.CI)) # Get rid of easy case.
  adjustment.factor <- (L / s0)^(2 * (r - (1/2) * sigma^2) / sigma^2)
  price.basic <- get_price(df, call=FALSE, vanilla=FALSE, basic=TRUE)
  price.basic <- unlist(price.basic)
  price.adjusted <- get_price(df, call=FALSE, vanilla=FALSE, basic=FALSE)
  price.adjusted <- unlist(price.adjusted)
  return(price.basic[1] + adjustment.factor * price.adjusted[1])
}

payoff.vanilla = function(s,K,vanilla.call.premium,vanilla.put.premium){
  # vanilla straddle 

  return(abs(s-K)-vanilla.call.premium - vanilla.put.premium)
}

payoff.barrier = function(s,K, call.premium, put.premium){
  # straddle (active)
  return(abs(s-K)-call.premium-put.premium)
}

payoff2 = function(s,K1,K2, call.premium, put.premium){
  # strangle
  put = max(K1-s,0)
  call = max(s-K2,0)

  return(max(call,put)-call.premium-put.premium)
}

payoff3 = function(s,K,call.premium, put.premium){
  # straddle (inactive)
  payoff = ifelse(s<L.put,K-s,ifelse(s>L.call,s-K,0))
  return(payoff - call.premium - put.premium)
}


sigma = 0.1
r = 1.95/100
T = 1
s <- seq(40, 65, by = 1)
s0 = 50
K = s0
K1 = 0.9*K
K2 = 1.2*K
L.put = 0.9*K
L.call = 1.2*K
I = 1000

vanilla.call.premium = price.UIC(s0, K, L=0, sigma, r, T)
vanilla.call.premium
vanilla.put.premium = price.DIP(s0, K, L = .Machine$integer.max, sigma, r, T)
vanilla.call.premium + vanilla.put.premium
P.vanilla = I / (vanilla.call.premium + vanilla.put.premium)
P.vanilla
f.straddle.vanilla = lapply(s,payoff.vanilla,K,vanilla.call.premium,vanilla.put.premium) %>% unlist

barrier.call.premium = price.UIC(s0, K, L.call, sigma, r, T)
barrier.call.premium
barrier.put.premium = price.DIP(s0, K, L.put, sigma, r, T)
barrier.call.premium + barrier.put.premium
P.barrier = I / (barrier.call.premium + barrier.put.premium)
P.barrier
f.straddle.barrier.active = lapply(s,payoff.barrier,K, barrier.call.premium, barrier.put.premium) %>% unlist
f.straddle.barrier.inactive = lapply(s,payoff3,K, barrier.call.premium, barrier.put.premium)%>% unlist

strangle.call.premium = price.UIC(s0, K2, L=0, sigma, r, T)
strangle.call.premium
strangle.put.premium = price.DIP(s0, K1, L = .Machine$integer.max, sigma, r, T)
f.strangle = lapply(s,payoff2,K1,K2, strangle.call.premium, strangle.put.premium) %>% unlist
P.strangle = I / (strangle.call.premium + strangle.put.premium)
strangle.call.premium + strangle.put.premium
P.strangle

payoff.diagram = data.frame(cbind(s,f.straddle.vanilla,f.straddle.barrier.active,f.straddle.barrier.inactive,f.strangle))
setDT(payoff.diagram)
colnames(payoff.diagram) = c("s","straddle_vanilla","straddle_barrier_active","straddle_barrier_inactive","strangle")
cols = colnames(payoff.diagram)
# payoff.diagram[ , (cols) := lapply(.SD, "*", 10), .SDcols = cols]
payoff.diagram[, straddle_vanilla_payoff := straddle_vanilla * P.vanilla]
payoff.diagram[, straddle_barrier_payoff := straddle_barrier_active * P.barrier]
payoff.diagram[, strangle_payoff := strangle * P.strangle]

ggplot(payoff.diagram, aes(x=s))+
  geom_line(aes(y=straddle_vanilla_payoff,color="Vanilla straddle"))+
  geom_line(aes(y=straddle_barrier_payoff,color="barrier straddle (active)"))+
  geom_line(aes(y=strangle_payoff,color="strangle"))+
  geom_hline(yintercept=0) + 
  scale_color_manual(name="Groups",values=c("blue", "red","green","black"))+
  ggtitle("Payoff * Participation")+
  labs(x = "Price", y = "Payoff")+
  theme_minimal()

