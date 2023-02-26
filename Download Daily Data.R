# Used to generate ETFs_adj_close_data.csv file
# Do not need to run again unless you want to edit the csv in the repo

library(quantmod)

tickers <- c('XLE', 'XLRE', 'XLY', 'XLC', 'XLK', 'XLF', 'XLU', 'XLB', 'XLP', 'XLI', 'XLV', 'SPY')

getSymbols(tickers, from = '2010-01-01', to = '2011-01-01')

adjusted_close_data <- cbind(SPY$SPY.Adjusted, XLE$XLE.Adjusted, XLY$XLY.Adjusted, XLK$XLK.Adjusted, XLF$XLF.Adjusted, 
                             XLU$XLU.Adjusted, XLB$XLB.Adjusted, XLP$XLP.Adjusted, XLI$XLI.Adjusted, XLV$XLV.Adjusted)

write.csv(adjusted_close_data, 'ETFs_adj_close_data.csv')