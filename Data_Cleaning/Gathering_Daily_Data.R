library(quantmod)

tickers <- c('XLE', 'XLRE', 'XLY', 'XLC', 'XLK', 'XLF', 'XLU', 'XLB', 'XLP', 'XLI', 'XLV', 'SPY')

getSymbols(tickers, from = '2006-01-01', to = '2023-01-01')

adjusted_close_data <- as.data.frame(cbind(SPY$SPY.Adjusted, XLE$XLE.Adjusted, XLY$XLY.Adjusted, XLK$XLK.Adjusted, XLF$XLF.Adjusted, 
                                      XLU$XLU.Adjusted, XLB$XLB.Adjusted, XLP$XLP.Adjusted, XLI$XLI.Adjusted, XLV$XLV.Adjusted))

colnames(adjusted_close_data) <- c('SPY.Price', 'XLE.Price', 'XLY.Price', 'XLK.Price', 'XLF.Price', 
                                   'XLU.Price', 'XLB.Price', 'XLP.Price', 'XLI.Price', 'XLV.Price')

write.csv(adjusted_close_data, file = 'ETF_Daily_Data/ETF_Daily_Prices.csv')

daily_rtns <- as.data.frame(apply(adjusted_close_data, MARGIN = 2, FUN = function(x) {diff(log(x))}))
colnames(daily_rtns) <- c('SPY.logrtn', 'XLE.logrtn', 'XLY.logrtn', 'XLK.logrtn', 'XLF.logrtn', 'XLU.logrtn', 
                          'XLB.logrtn', 'XLP.logrtn', 'XLI.logrtn', 'XLV.logrtn')

write.csv(daily_rtns, file = 'ETF_Daily_Data/ETF_Daily_Returns.csv')

# Function to calculate weekly returns from daily
calc_weekly_rtn <- function(x) {
  weekly_rtns <- c()
  for (i in 1:(length(x)-4)) {
    weekly_rtns <- c(weekly_rtns, sum(x[i:(i+4)]))
  }
  return(weekly_rtns)
}

# Function to calculate monthly returns from daily
calc_monthly_rtn <- function(x) {
  monthly_rtns <- c()
  for (i in 1:(length(x)-20)) {
    monthly_rtns <- c(monthly_rtns, sum(x[i:(i+20)]))
  }
  return(monthly_rtns)
}

# Apply above functions to make weekly and monthly csvs
weekly_rtn <- apply(daily_rtns, MARGIN = 2, FUN = calc_weekly_rtn)
monthly_rtn <- apply(daily_rtns, MARGIN = 2, FUN = calc_monthly_rtn)

write.csv(weekly_rtn, file = 'ETF_Daily_Data/ETF_Weekly_Returns.csv')
write.csv(monthly_rtn, file = 'ETF_Daily_Data/ETF_Monthly_Returns.csv')

################################################################################
# Making Lagged SPY Data

daily_rtn <- read.csv('ETF_Daily_Data/ETF_Daily_Returns.csv')
SPY <- daily_rtn[c(-1), 2]
ETFs <- daily_rtn[c(1:nrow(daily_rtn)-1),c(1, 3:11)]
lagged_spy_data <- cbind(ETFs, SPY)

write.csv(lagged_spy_data, 'ETF_Daily_Data/ETF_Daily_Returns_Lagged_SPY.csv')