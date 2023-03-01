library(quantmod)

tickers <- c('XLE', 'XLRE', 'XLY', 'XLC', 'XLK', 'XLF', 'XLU', 'XLB', 'XLP', 'XLI', 'XLV', 'SPY')

getSymbols(tickers, from = '2006-01-01', to = '2023-01-01')

adjusted_close_data <- cbind(SPY$SPY.Adjusted, XLE$XLE.Adjusted, XLY$XLY.Adjusted, XLK$XLK.Adjusted, XLF$XLF.Adjusted, 
                             XLU$XLU.Adjusted, XLB$XLB.Adjusted, XLP$XLP.Adjusted, XLI$XLI.Adjusted, XLV$XLV.Adjusted)

colnames(adjusted_close_data) <- c('SPY.Price', 'XLE.Price', 'XLY.Price', 'XLK.Price', 'XLF.Price', 
                                   'XLU.Price', 'XLB.Price', 'XLP.Price', 'XLI.Price', 'XLV.Price')

adjusted_close_data$SPY.logrtn <- diff(log(adjusted_close_data$SPY.Price))
adjusted_close_data$XLE.logrtn <- diff(log(adjusted_close_data$XLE.Price))
adjusted_close_data$XLY.logrtn <- diff(log(adjusted_close_data$XLY.Price))
adjusted_close_data$XLK.logrtn <- diff(log(adjusted_close_data$XLK.Price))
adjusted_close_data$XLF.logrtn <- diff(log(adjusted_close_data$XLF.Price))
adjusted_close_data$XLU.logrtn <- diff(log(adjusted_close_data$XLU.Price))
adjusted_close_data$XLB.logrtn <- diff(log(adjusted_close_data$XLB.Price))
adjusted_close_data$XLP.logrtn <- diff(log(adjusted_close_data$XLP.Price))
adjusted_close_data$XLI.logrtn <- diff(log(adjusted_close_data$XLI.Price))
adjusted_close_data$XLV.logrtn <- diff(log(adjusted_close_data$XLV.Price))

write.csv(adjusted_close_data, 'ETF_Daily_Data.csv')
