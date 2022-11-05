install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("tseries")
install.packages("forecast")
install.packages("fGarch")
install.packages("rugarch")

library(quantmod) #load quantmod library is used to download stock data from Yahoo Finance
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


getSymbols("AAPL", from = "2015-01-01", to = "2021-12-30") #Apple stock in the consumer discretionary sector
SP500dailyReturns <- data.frame(AAPL = dailyReturn(AAPL$AAPL.Adjusted))
dim(SP500dailyReturns)
getSymbols("MSFT", from = "2015-01-01", to = "2021-12-30") #Microsoft stock in the information technology sector
SP500dailyReturns <- cbind(SP500dailyReturns, MSFT = dailyReturn(MSFT$MSFT.Adjusted))
dim(SP500dailyReturns)
getSymbols("GOOGL", from = "2015-01-01", to = "2021-12-30")# Google stock in the information technology sector
SP500dailyReturns <- cbind(SP500dailyReturns, GOOGL = dailyReturn(GOOGL$GOOGL.Adjusted))
dim(SP500dailyReturns)
getSymbols("NKE", from = "2015-01-01", to = "2021-12-30") # Nike stock in the industrials sector
SP500dailyReturns <- cbind(SP500dailyReturns, NKE = dailyReturn(NKE$NKE.Adjusted))
dim(SP500dailyReturns)
getSymbols("KO", from = "2015-01-01", to = "2021-12-30") # Coca-Cola stock in the consumer staples sector
SP500dailyReturns <- cbind(SP500dailyReturns, KO = dailyReturn(KO$KO.Adjusted))
dim(SP500dailyReturns)
getSymbols("TSLA", from = "2015-01-01", to = "2021-12-30") #Tesla stock in the consumer discretionary sector
SP500dailyReturns <- cbind(SP500dailyReturns, TSLA = dailyReturn(TSLA$TSLA.Adjusted))
dim(SP500dailyReturns)
getSymbols("JNJ", from = "2015-01-01", to = "2021-12-30") # Johnson & Johnson stock in the health care sector
SP500dailyReturns <- cbind(SP500dailyReturns, JNJ = dailyReturn(JNJ$JNJ.Adjusted))
dim(SP500dailyReturns)
getSymbols("V", from = "2015-01-01", to = "2021-12-30") #PayPal stock in the financial sector
SP500dailyReturns <- cbind(SP500dailyReturns, PYPL = dailyReturn(PYPL$PYPL.Adjusted))
dim(SP500dailyReturns)
getSymbols("GS", from = "2015-01-01", to = "2021-12-30") # Goldman Sachs stock in the financial sector
SP500dailyReturns <- cbind(SP500dailyReturns, GS = dailyReturn(GS$GS.Adjusted))
dim(SP500dailyReturns)
getSymbols("NFLX", from = "2015-01-01", to = "2021-12-30") #Netflix stock in the consumer discretionary sector
SP500dailyReturns <- cbind(SP500dailyReturns, NFLX = dailyReturn(NFLX$NFLX.Adjusted))
dim(SP500dailyReturns)
getSymbols("AMT", from = "2015-01-01", to = "2021-12-30") # American Tower stock in the real estate sector
SP500dailyReturns <- cbind(SP500dailyReturns, AMZN = dailyReturn(AMZN$AMZN.Adjusted))
dim(SP500dailyReturns)

head(SP500dailyReturns)
tail(SP500dailyReturns)
#2-For each stock, compute the daily and weekly linear return and log-return using the adjusted closing price information.

#Daily linear return
SP500dailyReturns <- data.frame(AAPL = dailyReturn(AAPL$AAPL.Adjusted),
                                MSFT = dailyReturn(MSFT$MSFT.Adjusted),
                                GOOGL = dailyReturn(GOOGL$GOOGL.Adjusted),
                                NKE = dailyReturn(NKE$NKE.Adjusted),
                                KO = dailyReturn(KO$KO.Adjusted),
                                TSLA = dailyReturn(TSLA$TSLA.Adjusted),
                                JNJ = dailyReturn(JNJ$JNJ.Adjusted),
                                V = dailyReturn(V$V.Adjusted),
                                GS = dailyReturn(GS$GS.Adjusted),
                                NFLX = dailyReturn(NFLX$NFLX.Adjusted),
                                AMT = dailyReturn(AMT$AMT.Adjusted))
head(SP500dailyReturns)
tail(SP500dailyReturns)
dim(SP500dailyReturns)

#Weekly linear return
SP500weeklyReturns <- data.frame(AAPL = weeklyReturn(AAPL$AAPL.Adjusted),
                            MSFT = weeklyReturn(MSFT$MSFT.Adjusted),
                            GOOGL = weeklyReturn(GOOGL$GOOGL.Adjusted),
                            NKE = weeklyReturn(NKE$NKE.Adjusted),
                            KO = weeklyReturn(KO$KO.Adjusted),
                            TSLA = weeklyReturn(TSLA$TSLA.Adjusted),
                            JNJ = weeklyReturn(JNJ$JNJ.Adjusted),
                            V = weeklyReturn(V$V.Adjusted),
                            GS = weeklyReturn(GS$GS.Adjusted),
                            NFLX = weeklyReturn(NFLX$NFLX.Adjusted),
                            AMT = weeklyReturn(AMT$AMT.Adjusted))
head(SP500weeklyReturns)
tail(SP500weeklyReturns)
dim(SP500weeklyReturns)


#Daily log return
SP500dailyReturnsLog <- data.frame(AAPL = dailyReturn(AAPL$AAPL.Adjusted, type = "log"),
                                MSFT = dailyReturn(MSFT$MSFT.Adjusted, type = "log"),
                                GOOGL = dailyReturn(GOOGL$GOOGL.Adjusted, type = "log"),
                                NKE = dailyReturn(NKE$NKE.Adjusted, type = "log"),
                                KO = dailyReturn(KO$KO.Adjusted, type = "log"),
                                TSLA = dailyReturn(TSLA$TSLA.Adjusted, type = "log"),
                                JNJ = dailyReturn(JNJ$JNJ.Adjusted, type = "log"),
                                V = dailyReturn(V$V.Adjusted, type = "log"),
                                GS = dailyReturn(GS$GS.Adjusted, type = "log"),
                                NFLX = dailyReturn(NFLX$NFLX.Adjusted, type = "log"),
                                AMT = dailyReturn(AMT$AMT.Adjusted, type = "log"))
head(SP500dailyReturnsLog)
tail(SP500dailyReturnsLog)
dim(SP500dailyReturnsLog)

#Weekly log return
SP500weeklyReturnsLog <- data.frame(AAPL = weeklyReturn(AAPL$AAPL.Adjusted, type = "log"),
                            MSFT = weeklyReturn(MSFT$MSFT.Adjusted, type = "log"),
                            GOOGL = weeklyReturn(GOOGL$GOOGL.Adjusted, type = "log"),
                            NKE = weeklyReturn(NKE$NKE.Adjusted, type = "log"),
                            KO = weeklyReturn(KO$KO.Adjusted, type = "log"),
                            TSLA = weeklyReturn(TSLA$TSLA.Adjusted, type = "log"),
                            JNJ = weeklyReturn(JNJ$JNJ.Adjusted, type = "log"),
                            V = weeklyReturn(V$V.Adjusted, type = "log"),
                            GS = weeklyReturn(GS$GS.Adjusted, type = "log"),
                            NFLX = weeklyReturn(NFLX$NFLX.Adjusted, type = "log"),
                            AMT = weeklyReturn(AMT$AMT.Adjusted, type = "log"))
head(SP500weeklyReturnsLog)
tail(SP500weeklyReturnsLog)
dim(SP500weeklyReturnsLog)


"3-compute Empirically investigate the stylized facts of financial market returns different data frequencies: The statistical distribution of financial market returns is not normal,
The volatility of return processes is not constant with respect to time, The absolute or squared returns are highly autocorrelated, Extreme returns are observed closely in time (volatility clustering),
The empirical distribution of returns is skewed to the left."

#3.1-Compute the mean, variance, skewness, and kurtosis of the daily and weekly returns for each stock.
#Daily
SP500dailyReturnsMean <- apply(SP500dailyReturns, 2, mean)
SP500dailyReturnsVar <- apply(SP500dailyReturns, 2, var)
SP500dailyReturnsSkew <- apply(SP500dailyReturns, 2, skewness)
SP500dailyReturnsKurt <- apply(SP500dailyReturns, 2, kurtosis)

#Weekly
SP500weeklyReturnsMean <- apply(SP500weeklyReturns, 2, mean)
SP500weeklyReturnsVar <- apply(SP500weeklyReturns, 2, var)
SP500weeklyReturnsSkew <- apply(SP500weeklyReturns, 2, skewness)
SP500weeklyReturnsKurt <- apply(SP500weeklyReturns, 2, kurtosis)

#3.2-Compute the mean, variance, skewness, and kurtosis of the daily and weekly log-returns for each stock.
#Daily
SP500dailyReturnsLogMean <- apply(SP500dailyReturnsLog, 2, mean)
SP500dailyReturnsLogVar <- apply(SP500dailyReturnsLog, 2, var)
SP500dailyReturnsLogSkew <- apply(SP500dailyReturnsLog, 2, skewness)

#Weekly
SP500weeklyReturnsLogMean <- apply(SP500weeklyReturnsLog, 2, mean)
SP500weeklyReturnsLogVar <- apply(SP500weeklyReturnsLog, 2, var)
SP500weeklyReturnsLogSkew <- apply(SP500weeklyReturnsLog, 2, skewness)

#3.3-Compute the autocorrelation function (ACF) of the daily and weekly returns for each stock.
#Daily
SP500dailyReturnsACF <- apply(SP500dailyReturns, 2, acf)
SP500dailyReturnsACF

#Weekly
SP500weeklyReturnsACF <- apply(SP500weeklyReturns, 2, acf)
SP500weeklyReturnsACF

#3.4-Compute the autocorrelation function (ACF) of the daily and weekly log-returns for each stock.
#Daily
SP500dailyReturnsLogACF <- apply(SP500dailyReturnsLog, 2, acf)
SP500dailyReturnsLogACF

#Weekly
SP500weeklyReturnsLogACF <- apply(SP500weeklyReturnsLog, 2, acf)
SP500weeklyReturnsLogACF



"4- Split the dataset of daily returns into a training set (2/3 of data) and a test set (1/3 of data) to:
A. Empirically investigate the performance of the following Heuristic Portfolios
    • Buy & Hold
    • Equally weighted portfolio
    • Quintile portfolio
    • Global maximum return portfolio
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

## Split the dataset of daily returns into a training set (2/3 of data) and a test set (1/3 of data)

#Daily
SP500dailyReturnsTrain <- SP500dailyReturns[1:round(0.66*nrow(SP500dailyReturns)),] #2/3 of data
SP500dailyReturnsTest <- SP500dailyReturns[(round(0.66*nrow(SP500dailyReturns))+1):nrow(SP500dailyReturns),] #1/3 of data

#Weekly
SP500weeklyReturnsTrain <- SP500weeklyReturns[1:round(0.66*nrow(SP500weeklyReturns)),] #2/3 of data
SP500weeklyReturnsTest <- SP500weeklyReturns[(round(0.66*nrow(SP500weeklyReturns))+1):nrow(SP500weeklyReturns),] #1/3 of data

## A. Empirically investigate the performance of the following Heuristic Portfolios
## • Buy & Hold

#Daily
SP500dailyReturnsTrainBuyHold <- SP500dailyReturnsTrain[1,] #first row of the training set
SP500dailyReturnsTestBuyHold <- SP500dailyReturnsTest[1,]

#Weekly
SP500weeklyReturnsTrainBuyHold <- SP500weeklyReturnsTrain[1,] 
SP500weeklyReturnsTestBuyHold <- SP500weeklyReturnsTest[1,]

## • Equally weighted portfolio

#Daily
SP500dailyReturnsTrainEquallyWeighted <- apply(SP500dailyReturnsTrain, 1, mean) # apply is a function that applies a function to each column of a matrix
SP500dailyReturnsTestEquallyWeighted <- apply(SP500dailyReturnsTest, 1, mean)

#Weekly
SP500weeklyReturnsTrainEquallyWeighted <- apply(SP500weeklyReturnsTrain, 1, mean)
SP500weeklyReturnsTestEquallyWeighted <- apply(SP500weeklyReturnsTest, 1, mean)


## • Quintile portfolio

#Daily
SP500dailyReturnsTrainQuintile <- apply(SP500dailyReturnsTrain, 1, function(x) quantile(x, probs = c(0.2, 0.4, 0.6, 0.8))) # function(x) quantile(x, probs = c(0.2, 0.4, 0.6, 0.8)) is a function that computes the 20th, 40th, 60th, and 80th percentiles of a vector x
SP500dailyReturnsTestQuintile <- apply(SP500dailyReturnsTest, 1, function(x) quantile(x, probs = c(0.2, 0.4, 0.6, 0.8)))

## • Global maximum return (GMR) portfolio

#Daily
SP500dailyReturnsTrainGMR <- apply(SP500dailyReturnsTrain, 1, max) 
SP500dailyReturnsTestGMR <- apply(SP500dailyReturnsTest, 1, max)

####################################################################################################################
#####################ERROR IN THE CODE BELOW, I DON'T KNOW HOW TO FIX IT############################################

## B. Estimate the Markowitz’s mean-variance portfolio (MVP) with no short-selling´

library(PortfolioAnalytics)
library(PerformanceAnalytics)

#Daily  
#SP500dailyReturnsTrainMVP <- MVP(SP500dailyReturnsTrain, short = FALSE)
#SP500dailyReturnsTestMVP <- MVP(SP500dailyReturnsTest, short = FALSE)

#Daily
SP500dailyReturnsTrainMVP <- apply(SP500dailyReturnsTrain, 1, mean)
SP500dailyReturnsTestMVP <- apply(SP500dailyReturnsTest, 1, mean)

## C. Estimate the Global Minimum Variance Portfolio (GMVP) with no short-selling 

#Daily
#SP500dailyReturnsTrainGMVP <- gmvp(SP500dailyReturnsTrain, short = FALSE)
#SP500dailyReturnsTestGMVP <- gmvp(SP500dailyReturnsTest, short = FALSE)

SP500dailyReturnsTrainGMVP <- apply(SP500dailyReturnsTrain, 1, min)
SP500dailyReturnsTestGMVP <- apply(SP500dailyReturnsTest, 1, min)



## D. Estimate the Maximum Sharpe ratio portfolio (MSRP)

#Daily
SP500dailyReturnsTrainMSRP <- apply(SP500dailyReturnsTrain, 1, max)
SP500dailyReturnsTestMSRP <- apply(SP500dailyReturnsTest, 1, max)

#SP500dailyReturnsTrainMSRP <- msrp(SP500dailyReturnsTrain)
#SP500dailyReturnsTestMSRP <- msrp(SP500dailyReturnsTest)

## E. Empirically investigate the performance of the following Risk-Based Portfolios:
## • Global minimum variance portfolio

#Daily
SP500dailyReturnsTrainGMVP <- apply(SP500dailyReturnsTrain, 1, min)
SP500dailyReturnsTestGMVP <- apply(SP500dailyReturnsTest, 1, min)

#SP500dailyReturnsTrainGMVP <- gmvp(SP500dailyReturnsTrain, short = FALSE)
#SP500dailyReturnsTestGMVP <- gmvp(SP500dailyReturnsTest, short = FALSE)

## • Inverse volatility portfolio

#Daily
SP500dailyReturnsTrainInverseVolatility <- apply(SP500dailyReturnsTrain, 1, function(x) 1/sd(x)) # function(x) 1/sd(x) is a function that computes the inverse of the standard deviation of a vector x
SP500dailyReturnsTestInverseVolatility <- apply(SP500dailyReturnsTest, 1, function(x) 1/sd(x))

#SP500dailyReturnsTrainInverseVolatility <- inverse.volatility(SP500dailyReturnsTrain)
#SP500dailyReturnsTestInverseVolatility <- inverse.volatility(SP500dailyReturnsTest)

## • Risk parity portfolio 


#Daily
SP500dailyReturnsTrainRiskParity <- risk.parity(SP500dailyReturnsTrain)
SP500dailyReturnsTestRiskParity <- risk.parity(SP500dailyReturnsTest)


## • Most diversified portfolio

#Daily
SP500dailyReturnsTrainMostDiversified <- most.diversified(SP500dailyReturnsTrain)
SP500dailyReturnsTestMostDiversified <- most.diversified(SP500dailyReturnsTest)

## • Maximum decorrelation portfolio

#Daily
SP500dailyReturnsTrainMaximumDecorrelation <- maximum.decorrelation(SP500dailyReturnsTrain)
SP500dailyReturnsTestMaximumDecorrelation <- maximum.decorrelation(SP500dailyReturnsTest)

## F. Compare the performance of the alternative Heuristic, MVP, GMVP and MSRP and Risk-Based Portfolios in the training data set.

#Daily
SP500dailyReturnsTrainPortfolioPerformance <- data.frame(BuyHold = SP500dailyReturnsTrainBuyHold, 
                                                        EquallyWeighted = SP500dailyReturnsTrainEquallyWeighted, 
                                                        Quintile = SP500dailyReturnsTrainQuintile, 
                                                        GMR = SP500dailyReturnsTrainGMR, 
                                                        MVP = SP500dailyReturnsTrainMVP, 
                                                        GMVP = SP500dailyReturnsTrainGMVP, 
                                                        MSRP = SP500dailyReturnsTrainMSRP, 
                                                        GMVP = SP500dailyReturnsTrainGMVP, 
                                                        InverseVolatility = SP500dailyReturnsTrainInverseVolatility, 
                                                        RiskParity = SP500dailyReturnsTrainRiskParity, 
                                                        MostDiversified = SP500dailyReturnsTrainMostDiversified, 
                                                        MaximumDecorrelation = SP500dailyReturnsTrainMaximumDecorrelation)
SP500dailyReturnsTrainPortfolioPerformance
# create visualization 
SP500dailyReturnsTrainPortfolioPerformance %>% 
  gather(key = "Portfolio", value = "Return", -Date) %>% 
  ggplot(aes(x = Date, y = Return, color = Portfolio)) + 
  geom_line() + 
  labs(title = "Portfolio Performance", x = "Date", y = "Return") + 
  theme_minimal()

#plot lines of the different portfolios each line with a different color
plot(SP500dailyReturnsTrainPortfolioPerformance, type = "l", col = c("red", "blue", "green", "black", "yellow", "pink", "orange", "purple", "brown", "grey", "cyan", "magenta"))



plot(SP500dailyReturnsTrainPortfolioPerformance, type = "h", main = "Performance of the Heuristic Portfolios",
          ylab = "", xlab = "", col = "blue")
lines(SP500dailyReturnsTrainMVP, type = "h", col = "red")
lines(SP500dailyReturnsTrainGMVP, type = "h", col = "green")
lines(SP500dailyReturnsTrainMSRP, type = "h", col = "orange")
lines(SP500dailyReturnsTrainGMVP, type = "h", col = "purple")
lines(SP500dailyReturnsTrainInverseVolatility, type = "h", col = "brown")
lines(SP500dailyReturnsTrainRiskParity, type = "h", col = "pink")
lines(SP500dailyReturnsTrainMostDiversified, type = "h", col = "grey")
lines(SP500dailyReturnsTrainMaximumDecorrelation, type = "h", col = "black")
legend("topright", legend = c("Buy & Hold", "Equally Weighted", "Quintile", "GMR", "MVP", "GMVP", "MSRP", "Inverse Volatility", "Risk Parity", "Most Diversified", "Maximum Decorrelation"), 
       col = c("blue", "blue", "blue", "blue", "red", "green", "orange", "purple", "brown", "pink", "grey", "black"), lty = 1, cex = 0.8)

## G. Compare the performance of the alternative Heuristic, MVP, GMVP and MSRP and Risk-Based Portfolios in the test data set.

#Daily
SP500dailyReturnsTestPortfolioPerformance <- data.frame(BuyHold = SP500dailyReturnsTestBuyHold, 
                                                        EquallyWeighted = SP500dailyReturnsTestEquallyWeighted, 
                                                        Quintile = SP500dailyReturnsTestQuintile, 
                                                        GMR = SP500dailyReturnsTestGMR, 
                                                        MVP = SP500dailyReturnsTestMVP, 
                                                        GMVP = SP500dailyReturnsTestGMVP, 
                                                        MSRP = SP500dailyReturnsTestMSRP, 
                                                        GMVP = SP500dailyReturnsTestGMVP, 
                                                        InverseVolatility = SP500dailyReturnsTestInverseVolatility, 
                                                        RiskParity = SP500dailyReturnsTestRiskParity, 
                                                        MostDiversified = SP500dailyReturnsTestMostDiversified, 
                                                        MaximumDecorrelation = SP500dailyReturnsTestMaximumDecorrelation)
SP500dailyReturnsTestPortfolioPerformance   

