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

write.csv(all_hist_var, file = 'VaR/Hist_VaR.csv')


### Intraday Data ##############################################################
# Read daily csv
intra_15min_rtn <- read.csv('ETF_Cleaned_Minute_Data/All_15min.csv')[, c(-1)]
# Read weekly csv
intra_30min_rtn <- read.csv('ETF_Cleaned_Minute_Data/All_30min.csv')[, c(-1)]
# Read monthly csv
intra_1hr_rtn <- read.csv('ETF_Cleaned_Minute_Data/All_1hr.csv')[, c(-1)]

# 95% VaR Values
var <- 0.95
var_95_15min <- apply(intra_15min_rtn, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
var_95_30min <- apply(intra_30min_rtn, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
var_95_1hr <- apply(intra_1hr_rtn, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
# Combine into one matrix
var_95 <- rbind(var_95_15min, var_95_30min, var_95_1hr)


# 99% VaR Values
var <- 0.99
var_99_15min <- apply(intra_15min_rtn, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
var_99_30min <- apply(intra_30min_rtn, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
var_99_1hr <- apply(intra_1hr_rtn, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
# Combine into one matrix
var_99 <- rbind(var_99_15min, var_99_30min, var_99_1hr)


# Median Values
var <- 0.50
median_15min <- apply(intra_15min_rtn, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
median_30min <- apply(intra_30min_rtn, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
median_1hr <- apply(intra_1hr_rtn, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]})
# Combine into one matrix
medians <- rbind(median_15min, median_30min, median_1hr)


# Combine all historical VaR values into one matrix
all_intraday_hist_var <- rbind(var_95, var_99, medians)
all_intraday_hist_var <- all_intraday_hist_var[, c(9, 1, 7, 8, 2, 5, 4, 3, 10, 6)]
colnames(all_intraday_hist_var) <- c('SPY', 'XLE', 'XLY', 'XLK', 'XLF', 'XLU', 'XLB', 'XLP', 'XLI', 'XLV')
all_intraday_hist_var

write.csv(all_intraday_hist_var, file = 'VaR/Hist_Intraday_VaR.csv')

