# Volatility Protected PGN (VOLPROTEGN) Pricing & Backtesting
This project contains the code for Volatility Protected PGN (VOLPROTEGN) product pricing and backtesting. It produces option prices for single/basket barrier options, and present the backtesting performance.

## Getting Started
The following instructions will get you a replication of the project results.

### Prerequisites
Required R packages are: `data.table`, `readxl`, `stringr`, `ggplot2`, `doParallel`, `future.apply`,`dplyr`,`extraGrid`. 

### Single underlying barrier options prices and backtesting
#### Required data input
- In rds format, with file name "*ticker*+**_pricing.rds**" for the pricing; "*ticker*+**_backtest.rds**" for backtesting
- Must contain ONLY the following columns: `date`, `Closed_Price` and `ret` (return)
- To price the barrier options for the specific ticker, please move ONLY the two rds files into `/data/single` directory. Remove the existing files if necessary.
#### Running the codes
1. Open `project_pricing_single_barrier.R`
2. Load the R packages under **Load packages**
3. Change the working directory to your current one in the **Directory setup** section
4. Change the user defined global variables under **Global variables** if neccessary
5. Run through the whole script. No need to specify the input file names even if you have changed the input files.
#### Expected output
1. Option prices for the barrier call and put, with vanilla call and put prices with confidence interval for reference
2. Delta for the barrier call and put, in case delta hedging is needed
3. Backtesting result with a graph showing the prices and barrier, participation rates, options payoff and return in the PGN, contract total payoff and return, and underlying return for reference
#### Pricing methods & Computation
- Monte Carlo Simulation with contract function formula
- Vectorized code (in `data.table`) to reduce computation time

### Multiple underlying (basket) barrier options prices and backtesting
#### Required data input
- In rds format, with file name "*ticker*+**_pricing.rds**" for the pricing; "*ticker*+**_backtest.rds**" for backtesting
- Must contain ONLY the following columns: `date`, `Closed_Price` and `ret` (return)
- To price the barrier options for the specific *basket* of tickers, please move the corresponding rds files into `/data/multiple` directory. Remove the existing files if necessary.
#### Running the codes
1. Open `project_pricing_single_barrier.R`
2. Load the R packages under **Load packages**
3. Change the working directory to your current one in the **Directory setup** section
4. Change the user defined global variables under **Global variables** if neccessary
5. Run through the whole script. No need to specify the input file names even if changed the input files.
#### Expected output
1. Option prices for the barrier call and put, with vanilla call and put prices with confidence interval for reference
2. Delta for the barrier call and put, in case delta hedging is needed
3. Backtesting result with a graph showing the prices and barrier, participation rates, options payoff and return in the PGN, contract total payoff and return, and underlying return for reference
#### Pricing methods & Computation
- Small-time-steps Monte Carlo Simulation with Heston Model for volatility estimation
- Parallel processing (in `parSapply`) to reduce computation time

## Customizing results
### Changing underlyings
You may download historical daily data from the [HKEX website](https://www.hkex.com.hk/?sc_lang=en) or [Yahoo Finance](https://finance.yahoo.com/) and put into the `data` directory. Then, use `data preprocessing.R` to generate rds files with for the pricing. The pricing programme can automatically read all files from the `Price_data` directory and give option prices in the basket, so you do not need to specify the file names. Please change `mainDir` to your current directory. 
If you use data not downloaded from [HKEX website](https://www.hkex.com.hk/?sc_lang=en) or [Yahoo Finance](https://finance.yahoo.com/), you may add code on `data preprocessing.R` to generate rds files with `Time`, `Closed_Price` and `ret` columns, for the pricing programme to read automatically. 

### Changing model parameters 
If you wish to change parameters for the Monte Carlo simulation, you may edit the section under `### Global variables` `***User edit area***`. You can change _number of trading days in a year_, _number of simulated trials_, _time until expiration_, _number of subinterval_, _time per subinterval_, _risk free rate_, _multiplier on strike for call barrier_, _multiplier on strike for put barrier_, _vol factor_ (single), _speed of mean reversion_ (multiple), _volatility of volatility_(multiple).
