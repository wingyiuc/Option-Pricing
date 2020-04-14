# Volatility Trading PGN Pricing Project
This project contains the code for Volatility Trading PGN Pricing Project. It produces option prices for single/basket barrier options, and present the backtesting performance.

## Getting Started
The following instructions will get you a replication of the project results

### Prerequisites
Required R packages are: `data.table`, `readxl`, `stringr`, `ggplot2`, `tidyverse`, `doParallel`, `doRNG`. 

### Getting single underlying barrier options prices and backtesting
Please go to `roject_pricing_single_barrier.R` and run the code to get the option prices. Please change `mainDir` to your current directory. 
The programme uses Monte Carlo Simulation and a closed form pricing formula with contract functions. It gives the option prices, delta of the options, as well as backtesting results. If you wish to price more than one underlying, please use `project_pricing_multiple_barrier.R`. 

### Getting basket barrier options prices and backtesting
Please go to `project_pricing_multiple_barrier.R` and run the code to get the option prices. Please change `mainDir` to your current directory. 
The programme uses small-time-step Monte Carlo Simulation to generate simulated price paths, and Heston model to simulate changing volatility. To reduce the computation time, it uses parallel computing for the large simulations.

## Customizing results
### Changing underlyings
You may download historical daily data from the [HKEX website](https://www.hkex.com.hk/?sc_lang=en) and put into the `Price_data` directory. Then, use `data preprocessing.R` to generate rds files with for the pricing. The pricing programme can automatically read all files from the `Price_data` directory and give option prices in the basket. Please change `mainDir` to your current directory. 
If you use data not downloaded from [HKEX website](https://www.hkex.com.hk/?sc_lang=en), you may add code on `data preprocessing.R` to generate rds files with `Time` and `Close price` columns, for the pricing programme to read automatically. 

### Changing model parameters 
If you wish to change parameters for the Monte Carlo simulation, you may edit the section under `### Global variables` `***User edit area***`. You can change _number of trading days in a year_, _number of simulated trials_, _time until expiration_, _number of subinterval_, _time per subinterval_, _risk free rate_, _multiplier on strike for call barrier_, _multiplier on strike for put barrier_, _speed of mean reversion_, _volatility of volatility_.
