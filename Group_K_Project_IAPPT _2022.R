install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("tseries")
install.packages("forecast")
install.packages("fGarch")
install.packages("rugarch")


rm(list=ls(all=TRUE))
graphics.off()
close.screen(all = TRUE)
erase.screen()
windows.options(record=TRUE)

p_load(xts)                   # to manipulate time series of stock data
p_load(quantmod)              # to download stock data
p_load(PerformanceAnalytics)  # to compute performance measures
p_load(CVXR)                  # Convex Optimization in R
p_load(riskParityPortfolio)   # RPP

# -------------------------------------------------------------------------------------------
# Loading Data
# -------------------------------------------------------------------------------------------

#1-Select a diversified portfolio of 11 S&P500 listed stocks, each belonging to one of the 11 different sectors that comprise index (e.g., Health Care, Financials, Consumer Discretionary, Industrials), and download market data from 01-01-2015 to 30-12-2021.

# helth care sector stocks :ABBV (AbbVie Inc.), ABT (Abbott Laboratories), AMGN (Amgen Inc.), BIIB (Biogen Inc.), CELG (Celgene Corp.), GILD (Gilead Sciences Inc.), JNJ (Johnson & Johnson), MRK (Merck & Co. Inc.), PFE (Pfizer Inc.), REGN (Regeneron Pharmaceuticals Inc.), and TMO (Thermo Fisher Scientific Inc.) # nolint
# financial sector stocks : AIG (American International Group), AXP (American Express), BK (Bank of New York Mellon), C (Citigroup), GS (Goldman Sachs), JPM (JPMorgan Chase), MS (Morgan Stanley), PNC (PNC Financial Services), SPGI (S&P Global), USB (U.S. Bancorp), and WFC (Wells Fargo)
# consumer discretionary sector stocks : AAPL (Apple), AMZN (Amazon), CMCSA (Comcast), DIS (Walt Disney), FB (Facebook), GOOGL (Alphabet), HD (Home Depot), MCD (McDonald’s), SBUX (Starbucks), TGT (Target), and WMT (Walmart)
# industrials sector stocks : BA (Boeing), CAT (Caterpillar), CVX (Chevron), DD (DuPont), GE (General Electric), HON (Honeywell), IBM (IBM), MMM (3M), NKE (Nike), PG (Procter & Gamble), and UTX (United Technologies)
# information technology sector stocks : ADP (Automatic Data Processing), CRM (Salesforce), CSCO (Cisco), INTC (Intel), MSFT (Microsoft), ORCL (Oracle), QCOM (Qualcomm), SAP (SAP), SYMC (Symantec), TXN (Texas Instruments), and V (Visa)
# consumer staples sector stocks : CL (Colgate-Palmolive), KO (Coca-Cola), KMB (Kimberly-Clark), MDLZ (Mondelez), MKC (McCormick), MO (Altria), PG (Procter & Gamble), SJM (JM Smucker), STZ (Constellation Brands), UL (Unilever), and WBA (Walgreens Boots Alliance)
# energy sector stocks : APA (Apache), COP (ConocoPhillips), CVX (Chevron), DVN (Devon Energy), EOG (EOG Resources), HAL (Halliburton), HPQ (HP), HES (Hess), KMI (Kinder Morgan), MPC (Marathon Petroleum), and SLB (Schlumberger)
# materials sector stocks : APD (Air Products & Chemicals), BDX (Becton Dickinson), CF (CF Industries), DOW (Dow), EMR (Emerson Electric), FMC (FMC), HON (Honeywell), LYB (LyondellBasell), MON (Monsanto), NEM (Newmont), and SHW (Sherwin-Williams)
# real estate sector stocks : AMT (American Tower), AVB (AvalonBay Communities), CBRE (CBRE Group), DRE (Duke Realty), EQIX (Equinix), EXR (Extra Space Storage), HCP (HCP), IRM (Iron Mountain), KIM (Kimco Realty), O (Realty Income), and VTR (Ventas)
# utilities sector stocks : AEE (Ameren), AEP (American Electric Power), AES (AES), AWK (American Water Works), DUK (Duke Energy), ED (Consolidated Edison), EIX (Edison International), NEE (NextEra Energy), NI (NiSource), PPL (PPL), and XEL (Xcel Energy)

#compute the daily and weekly linear return and log-return using the adjusted closing price information (APPL, MSFT, GOOGL, NKE, KO, TSLA, JNJ, V, GS, NFLX, AMT) and download market data from 01-01-2015 to 30-12-2021.

getSymbols("AAPL", from = "2015-01-01", to = "2021-12-30") #Apple stock in the consumer discretionary sector
SP500dailyPrices <- AAPL$AAPL.Adjusted
head(SP500dailyPrices)
dim(SP500dailyReturns)

getSymbols("MSFT", from = "2015-01-01", to = "2021-12-30") #Microsoft stock in the information technology sector
SP500dailyPrices <- cbind(SP500dailyPrices, MSFT$MSFT.Adjusted) #ajusted closing price is used to compute the daily return of the stock without the effect of stock splits and dividends

getSymbols("GOOGL", from = "2015-01-01", to = "2021-12-30") #Alphabet stock in the information technology sector is a holding company that owns Google and several other companies
SP500dailyPrices <- cbind(SP500dailyPrices, GOOGL$GOOGL.Adjusted) #cbind is used to combine the data frames by columns

getSymbols("NKE", from = "2015-01-01", to = "2021-12-30") #Nike stock in the consumer discretionary sector
SP500dailyPrices <- cbind(SP500dailyPrices, NKE$NKE.Adjusted)

getSymbols("KO", from = "2015-01-01", to = "2021-12-30") #Coca-Cola stock in the consumer staples sector
SP500dailyPrices <- cbind(SP500dailyPrices, KO$KO.Adjusted)

getSymbols("TSLA", from = "2015-01-01", to = "2021-12-30") #Tesla stock in the consumer discretionary sector
SP500dailyPrices <- cbind(SP500dailyPrices, TSLA$TSLA.Adjusted)

getSymbols("JNJ", from = "2015-01-01", to = "2021-12-30") #Johnson & Johnson stock in the health care sector
SP500dailyPrices <- cbind(SP500dailyPrices, JNJ$JNJ.Adjusted)

getSymbols("V", from = "2015-01-01", to = "2021-12-30") #Visa stock in the information technology sector
SP500dailyPrices <- cbind(SP500dailyPrices, V$V.Adjusted)

getSymbols("GS", from = "2015-01-01", to = "2021-12-30") #Goldman Sachs stock
SP500dailyPrices <- cbind(SP500dailyPrices, GS$GS.Adjusted)

getSymbols("NFLX", from = "2015-01-01", to = "2021-12-30") #Netflix stock in the consumer discretionary sector
SP500dailyPrices <- cbind(SP500dailyPrices, NFLX$NFLX.Adjusted)

getSymbols("AMT", from = "2015-01-01", to = "2021-12-30") #American Tower stock in the real estate sector
SP500dailyPrices <- cbind(SP500dailyPrices, AMT$AMT.Adjusted)


# dataframe assign column names as stock
colnames(SP500dailyPrices) <- c("AAPL", "MSFT", "GOOGL", "NKE", "KO", "TSLA", "JNJ", "V", "GS", "NFLX", "AMT")

head(SP500dailyPrices)
tail (SP500dailyPrices)
summary(SP500dailyPrices)
dim(SP500dailyPrices)

#weekly prices 
SP500weeklyPrices <- SP500dailyPrices[seq(1, nrow(SP500dailyPrices), 5),] #select every 5th row
head(SP500weeklyPrices)
tail(SP500weeklyPrices)
dim(SP500weeklyPrices)


#plot the adjusted closing price of each stocks color rainbow
plot(SP500dailyPrices, col = rainbow(11), main = "Adjusted Closing Price of 11 Stocks", xlab = "Date", ylab = "Adjusted Closing Price")
legend("topright", legend = colnames(SP500dailyPrices), col = rainbow(11), lty = 1, cex = 0.8)


#2-For each stock, compute the daily and weekly linear  and log using the adjusted closing price information

#2.1.1-Daily linear return
SP500dailyPrices_linearReturn <- (SP500dailyPrices / lag(SP500dailyPrices) - 1) [-1]#[-1] is to drop the first row

head(SP500dailyPrices_linearReturn)
summary(SP500dailyPrices_linearReturn)
dim(SP500dailyPrices_linearReturn)

#2.1.2Daily log-return diff
SP500dailyPrices_logReturn <- diff(log(SP500dailyPrices))[-1] #diff function is used to compute the difference between the current and previous value
head(SP500dailyPrices_logReturn)
summary(SP500dailyPrices_logReturn)
dim(SP500dailyPrices_logReturn)



#2.2.1-Weekly linear return
SP500weeklyPrices_linearReturn <- (SP500weeklyPrices / lag(SP500weeklyPrices) - 1) [-1]#lag function is used to shift the data by one period

head(SP500weeklyPrices_linearReturn)
summary(SP500weeklyPrices_linearReturn)
dim(SP500weeklyPrices_linearReturn)

#2.2.2Weekly log-return diff
SP500weeklyPrices_logReturn <- diff(log(SP500weeklyPrices))[-1] #diff function is used to compute the difference between the current and previous value
head(SP500weeklyPrices_logReturn)
summary(SP500weeklyPrices_logReturn)


#save the data frame as a csv file on the folder "C:\Users\João Carlos Fidalgo\OneDrive - Banco de Cabo Verde\Pós-Graduação_ Data_science\2-IAPT_Investments, Asset Pricing_Portfolio Theory"
write.csv(SP500dailyPrices, file = "C:/Users/João Carlos Fidalgo/OneDrive - Banco de Cabo Verde/Pós-Graduação_ Data_science/2-IAPT_Investments, Asset Pricing_Portfolio Theory/SP500dailyPrices.csv", row.names = TRUE)
write.csv(SP500dailyPrices_linearReturn, file = "C:/Users/João Carlos Fidalgo/OneDrive - Banco de Cabo Verde/Pós-Graduação_ Data_science/2-IAPT_Investments, Asset Pricing_Portfolio Theory/SP500dailyPrices_linearReturn.csv", row.names = TRUE)
write.csv(SP500dailyPrices_logReturn, file = "C:/Users/João Carlos Fidalgo/OneDrive - Banco de Cabo Verde/Pós-Graduação_ Data_science/2-IAPT_Investments, Asset Pricing_Portfolio Theory/SP500dailyPrices_logReturn.csv", row.names = TRUE)
write.csv(SP500weeklyPrices, file = "C:/Users/João Carlos Fidalgo/OneDrive - Banco de Cabo Verde/Pós-Graduação_ Data_science/2-IAPT_Investments, Asset Pricing_Portfolio Theory/SP500weeklyPrices.csv", row.names = TRUE)
write.csv(SP500weeklyPrices_linearReturn, file = "C:/Users/João Carlos Fidalgo/OneDrive - Banco de Cabo Verde/Pós-Graduação_ Data_science/2-IAPT_Investments, Asset Pricing_Portfolio Theory/SP500weeklyPrices_linearReturn.csv", row.names = TRUE)
write.csv(SP500weeklyPrices_logReturn, file = "C:/Users/João Carlos Fidalgo/OneDrive - Banco de Cabo Verde/Pós-Graduação_ Data_science/2-IAPT_Investments, Asset Pricing_Portfolio Theory/SP500weeklyPrices_logReturn.csv", row.names = TRUE)

## number of stocks

# number of days


#3-Compute Empirically investigate the stylized facts of financial market returns different data frequencies: 
#3.1-The statistical distribution of financial market returns is not normal

#3.1.1-Daily linearof market

hist(SP500dailyPrices_linearReturn$AAPL, main = "Daily Linear Return", xlab = "Daily Linear Return", col = "blue", breaks = 100) #histogram of daily linear return

qqnorm(SP500dailyPrices_linearReturn$AAPL, main = "Daily Linear Return", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "blue") #qqplot of daily linear return
qqline(SP500dailyPrices_linearReturn$AAPL, col = "blue")

#3.1.2-Daily log
hist(SP500dailyPrices_logReturn$AAPL, main = "Daily Log Return", xlab = "Daily Log Return", col = "blue", breaks = 100) #histogram of daily log return

qqnorm(SP500dailyPrices_logReturn$AAPL, main = "Daily Log Return", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "blue") #qqplot of daily log return
qqline(SP500dailyPrices_logReturn$AAPL, col = "blue")

#3.1.3-Weekly linear
hist(SP500weeklyPrices_linearReturn$AAPL, main = "Weekly Linear Return", xlab = "Weekly Linear Return", col = "blue", breaks = 100) #histogram of weekly linear return

qqnorm(SP500weeklyPrices_linearReturn$AAPL, main = "Weekly Linear Return", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "blue") #qqplot of weekly linear return
qqline(SP500weeklyPrices_linearReturn$AAPL, col = "blue")

#3.1.4-Weekly log
hist(SP500weeklyPrices_logReturn$AAPL, main = "Weekly Log Return", xlab = "Weekly Log Return", col = "blue", breaks = 100) #histogram of weekly log return

qqnorm(SP500weeklyPrices_logReturn$AAPL, main = "Weekly Log Return", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "blue") #qqplot of weekly log return
qqline(SP500weeklyPrices_logReturn$AAPL, col = "blue")

#3.2-The volatility of return processes is not constant with respect to time, 

#3.2.1-Daily linear
plot(SP500dailyPrices_linearReturn$AAPL, main = "Daily Linear Return", xlab = "Time", ylab = "Daily Linear Return", col = "blue") #plot of daily linear return

#3.3-The absolute or squared returns are highly autocorrelated

#3.3.1-Daily linear
acf(SP500dailyPrices_linearReturn$AAPL, main = "Daily Linear Return", xlab = "Lag", ylab = "Autocorrelation", col = "blue") #acf of daily linear return

#3.4-Extreme returns are observed closely in time (volatility clustering),

#3.4.1-Daily linear
plot(SP500dailyPrices_linearReturn$AAPL, main = "Daily Linear Return", xlab = "Time", ylab = "Daily Linear Return", col = "blue") #plot of daily linear return

#3.5-The empirical distribution of returns is skewed to the left."

#3.5.1-Daily linear
hist(SP500dailyPrices_linearReturn$AAPL, main = "Daily Linear Return", xlab = "Daily Linear Return", col = "blue", breaks = 100) #histogram of daily linear return

#4- Split the dataset of daily returns into a training set (2/3 of data) and a test set (1/3 of data) to:
#A. Empirically investigate the performance of the following Heuristic Portfolios
#    • Buy & Hold
#    • Equally weighted portfolio
#    • Quintile portfolio
#    • Global maximum return portfolio


## Split the dataset of daily returns into a training set (2/3 of data) and a test set (1/3 of data)
#split SP500dailyPrices_linearReturn  
SP500dailyPrices_linearReturn_train <- SP500dailyPrices_linearReturn[1:round(0.66*nrow(SP500dailyPrices_linearReturn)),]
SP500dailyPrices_linearReturn_test <- SP500dailyPrices_linearReturn[(round(0.66*nrow(SP500dailyPrices_linearReturn))+1):nrow(SP500dailyPrices_linearReturn),]
dim(SP500dailyPrices_linearReturn_train) # 1162 rows and 11 columns
dim(SP500dailyPrices_linearReturn_test) # 598 rows and 11 columns

#split SP500dailyPrices_logReturn
SP500dailyPrices_logReturn_train <- SP500dailyPrices_logReturn[1:round(0.66*nrow(SP500dailyPrices_logReturn)),]
SP500dailyPrices_logReturn_test <- SP500dailyPrices_logReturn[(round(0.66*nrow(SP500dailyPrices_logReturn))+1):nrow(SP500dailyPrices_logReturn),]
dim(SP500dailyPrices_logReturn_train) # 1162 rows and 11 columns
dim(SP500dailyPrices_logReturn_test) # 598 rows and 11 columns

###################################################################

#4.1-A. Empirically investigate the performance of the following Heuristic Portfolios

#4.1-A.1-Buy & Hold
# Estimation of the expected return and covariance matrix
mu <- colMeans(SP500dailyPrices_logReturn_train) #vector of expected returns 
Sigma <- cov(SP500dailyPrices_logReturn_train) # covariance matrix

w_BnH <- diag(1,11) 
rownames(w_BnH) <- colnames(SP500dailyPrices_linearReturn_train)
colnames(w_BnH) <- paste0("w_BnH_", colnames(SP500dailyPrices_linearReturn_train))
w_BnH

# compute returns of all B&H portfolios
#BnH_returns_linearReturn <- xts(SP500dailyPrices_linearReturn %*% w_BnH, index(SP500dailyPrices_linearReturn))
BnH_returns_linearReturn_train <- xts(SP500dailyPrices_linearReturn_train %*% w_BnH, index(SP500dailyPrices_linearReturn_train))
head(BnH_returns_linearReturn_train)

BnH_returns_linearReturn_test <- xts(SP500dailyPrices_linearReturn_test %*% w_BnH, index(SP500dailyPrices_linearReturn_test))
head(BnH_returns_linearReturn_test)

dim(BnH_returns_linearReturn_train) # 1162 rows and 11 columns
dim(BnH_returns_linearReturn_test) # 598 rows and 11 columns

#Performance measures
library(PerformanceAnalytics)
# Table of Annualized Return, Annualized Std Dev, and Annualized Sharpe
t(table.AnnualizedReturns(BnH_returns_linearReturn_train, scale = 252, Rf = 0.0))
t(table.AnnualizedReturns(BnH_returns_linearReturn_test, scale = 252, Rf = 0.0))

#Summary statistics of the returns of all B&H portfolios
table.DownsideRisk(BnH_returns_linearReturn_train)
table.DownsideRisk(BnH_returns_linearReturn_test)

chart.CumReturns(BnH_returns_linearReturn_train, main = "Cumulative Returns of Buy & Hold Portfolios", legend.loc = "topleft")
chart.Boxplot(BnH_returns_linearReturn_train, main = "Boxplot of Buy & Hold Portfolios", legend.loc = "topleft")
chart.Correlation(BnH_returns_linearReturn_train, main = "Correlation of Buy & Hold Portfolios", legend.loc = "topleft")


#4.1-A.2-Equally weighted portfolio
# Estimation of the expected return and covariance matrix
mu <- colMeans(SP500dailyPrices_logReturn_train) #vector of expected returns
Sigma <- cov(SP500dailyPrices_logReturn_train) # covariance matrix

####validar o codigo####
w_EW <- rep(1/11,11)
names(w_EW) <- colnames(SP500dailyPrices_linearReturn_train)
w_EW

# compute returns of all EW portfolios
#EW_returns_linearReturn <- xts(SP500dailyPrices_linearReturn %*% w_EW, index(SP500dailyPrices_linearReturn))
EW_returns_linearReturn_train <- xts(SP500dailyPrices_linearReturn_train %*% w_EW, index(SP500dailyPrices_linearReturn_train))
head(EW_returns_linearReturn_train)

#4.1-A.3-Quintile portfolio
#  Quintile divide them 3 part portfolios (Q1, Q2, Q3)
# Estimation of the expected return and covariance matrix
mu <- colMeans(SP500dailyPrices_logReturn_train) #vector of expected returns
Sigma <- cov(SP500dailyPrices_logReturn_train) # covariance matrix

# 1- 1st quintile portfolio
w_1stQ <- rep(1/11,11)
names(w_1stQ) <- colnames(SP500dailyPrices_linearReturn_train)
w_1stQ[1:5] <- 0
w_1stQ

# 2- 2nd quintile portfolio
w_2ndQ <- rep(1/11,11)
names(w_2ndQ) <- colnames(SP500dailyPrices_linearReturn_train)
w_2ndQ[1:5] <- 0
w_2ndQ[6:10] <- 0
w_2ndQ

# 3- 3rd quintile portfolio
w_3rdQ <- rep(1/11,11)
names(w_3rdQ) <- colnames(SP500dailyPrices_linearReturn_train)
w_3rdQ[6:10] <- 0
w_3rdQ

# combine all 3 portfolios
w_Quintile <- cbind(w_1stQ, w_2ndQ, w_3rdQ)
rownames(w_Quintile) <- colnames(SP500dailyPrices_linearReturn_train)
colnames(w_Quintile) <- paste0("w_Quintile_", colnames(SP500dailyPrices_linearReturn_train))
w_Quintile


#4.1-A.4-Global maximum return portfolio (GMRP)
# compute weights of the GMRP
w_GMRP <- mu/sum(mu)
w_GMRP

i_max <- which(mu == max(mu))
w_GMRP <- matrix(0, nrow = 11, ncol = 1)
w_GMRP[i_max,1] <- 1
names(w_GMRP) <- colnames(SP500dailyPrices_linearReturn_train)
w_GMRP

#put together all portfolios w_EW, w_Quintile, w_GMRP
w_heuristic <- cbind(w_EW, w_Quintile, w_GMRP)
round(w_heuristic, digits = 2)
rownames(w_heuristic) <- colnames(SP500dailyPrices_linearReturn_train)
colnames(w_heuristic) <- paste0("w_heuristic_", colnames(SP500dailyPrices_linearReturn_train))
w_heuristic

barplot(w_heuristic, beside = TRUE, main = "Weights of Heuristic Portfolios", legend = colnames(w_heuristic), col = rainbow(11), legend.loc = "topleft")

# compute returns of all heuristic portfolios
heuristic_returns_linearReturn <- xts(SP500dailyPrices_linearReturn %*% w_heuristic, index(SP500dailyPrices_linearReturn))
head(heuristic_returns_linearReturn)

#Performance measures
library(PerformanceAnalytics)
# Table of Annualized Return, Annualized Std Dev, and Annualized Sharpe
t(table.AnnualizedReturns(heuristic_returns_linearReturn, scale = 252, Rf = 0.0))

#train and test
heuristic_returns_linearReturn_train <- xts(SP500dailyPrices_linearReturn_train %*% w_heuristic, index(SP500dailyPrices_linearReturn_train))
head(heuristic_returns_linearReturn_train)

heuristic_returns_linearReturn_test <- xts(SP500dailyPrices_linearReturn_test %*% w_heuristic, index(SP500dailyPrices_linearReturn_test))
head(heuristic_returns_linearReturn_test)

#Summary statistics of the returns of all heuristic portfolios
table.DownsideRisk(heuristic_returns_linearReturn_train)
table.DownsideRisk(heuristic_returns_linearReturn_test)
####validar o codigo####

"
B. Estimate the Markowitz’s mean-variance portfolio (MVP) with no short-selling
C. Estimate the Global Minimum Variance Portfolio (GMVP) with no short-selling
D. Estimate the Maximum Sharpe ratio portfolio (MSRP)
E. Empirically investigate the performance of the following Risk-Based Portfolios:
    • Global minimum variance portfolio
    • Inverse volatility portfolio
    • Risk parity portfolio
    • Most diversified portfolio
    • Maximum decorrelation portfolio
F. Compare the performance of the alternative Heuristic, MVP, GMVP and MSRP and Risk-Based Portfolios in the training data set.
"

#4.1-B.1-Markowitz’s mean-variance portfolio (MVP) with no short-selling
