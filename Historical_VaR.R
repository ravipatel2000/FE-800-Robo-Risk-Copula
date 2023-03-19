# Read daily csv
daily_rtns <- read.csv('ETF_Daily_Data/ETF_Daily_Returns.csv')[, c(-1)]
# Read weekly csv
weekly_rtns <- read.csv('ETF_Daily_Data/ETF_Weekly_Returns.csv')[, c(-1)]
# Read monthly csv
monthly_rtns <- read.csv('ETF_Daily_Data/ETF_Monthly_Returns.csv')[, c(-1)]

# 95% VaR Values
var <- 0.95
var_95_daily <- apply(daily_rtns, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
var_95_weekly <- apply(weekly_rtns, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
var_95_monthly <- apply(monthly_rtns, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
# Combine into one matrix
var_95 <- rbind(var_95_daily, var_95_weekly, var_95_monthly)
colnames(var_95) <- c('SPY', 'XLE', 'XLY', 'XLK', 'XLF', 'XLU', 'XLB', 'XLP', 'XLI', 'XLV')

# 99% VaR Values
var <- 0.99
var_99_daily <- apply(daily_rtns, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
var_99_weekly <- apply(weekly_rtns, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
var_99_monthly <- apply(monthly_rtns, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
# Combine into one matrix
var_99 <- rbind(var_99_daily, var_99_weekly, var_99_monthly)
colnames(var_99) <- colnames(var_95)

# Median Values
var <- 0.50
median_daily <- apply(daily_rtns, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
median_weekly <- apply(weekly_rtns, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
median_monthly <- apply(monthly_rtns, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
# Combine into one matrix
medians <- rbind(median_daily, median_weekly, median_monthly)
colnames(medians) <- colnames(var_95)

# Combine all historical VaR values into one matrix
all_hist_var <- rbind(var_95, var_99, medians)
all_hist_var

write.csv(all_hist_var, file = 'Hist_VaR.csv')
