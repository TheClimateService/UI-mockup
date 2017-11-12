
# References:
#	https://www.r-bloggers.com/quantitative-stock-analysis-tutorial-screening-the-returns-for-every-sampp500-stock-in-less-than-5-minutes/

library(quantmod)

# Get daily data on specified stock.
# Output format:
#            AAPL.Open AAPL.High AAPL.Low AAPL.Close AAPL.Volume AAPL.Adjusted
# 2007-01-03  12.32714  12.36857 11.70000   11.97143   309579900      10.77017
# 2007-01-04  12.00714  12.27857 11.97429   12.23714   211815100      11.00922
# 2007-01-05  12.25286  12.31428 12.05714   12.15000   208685400      10.93082
# x = getSymbols("AAPL", env=NULL)

# Get list of stocks and associate parameters.
# Output format:
#  Symbol                                                       Security.Name Market.Category Test.Issue Financial.Status Round.Lot.Size ETF NextShares
# 1   AAAP Advanced Accelerator Applications S.A. - American Depositary Shares               Q          N                N            100   N          N
# 2   AABA                                          Altaba Inc. - Common Stock               Q          N                N            100   N          N
# 3    AAL                        American Airlines Group, Inc. - Common Stock               Q          N                N            100   N          N
# Read from live source or from local file.
# y <- read.csv("ftp://ftp.nasdaqtrader.com/SymbolDirectory/nasdaqlisted.txt", sep="|")
# write.table(y, "stocks_nasdaq.csv")
stocks_nasdaq = read.table("./data/financial/stocks_nasdaq.csv", header=TRUE)
symbols = stocks_nasdaq[,1]
names = stocks_nasdaq[,2]
symbols_names = paste(symbols, names)
stocks_nasdaq_parameters = read.table("./data/financial/stocks_nasdaq.csv.parameters", header=TRUE)
#parameters_fin1 = stocks_nasdaq_parameters[,3]
#parameters_fin2 = stocks_nasdaq_parameters[,4]
#parameters_fin3 = stocks_nasdaq_parameters[,5]
stocks_nasdaq_factors = read.table("./data/financial/stocks_nasdaq.csv.factors", header=TRUE)


