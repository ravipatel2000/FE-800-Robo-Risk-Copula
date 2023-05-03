rtn_15min <- read.csv('ETF_Cleaned_Minute_Data/All_15min.csv')[,-1][, c(9, 1, 7, 8, 2, 5, 4, 3, 10, 6)]
rtn_30min <- read.csv('ETF_Cleaned_Minute_Data/All_30min.csv')[,-1][, c(9, 1, 7, 8, 2, 5, 4, 3, 10, 6)]
rtn_1hr <- read.csv('ETF_Cleaned_Minute_Data/All_1hr.csv')[,-1][, c(9, 1, 7, 8, 2, 5, 4, 3, 10, 6)]

ETF <- c("XLE", "XLY", "XLK", "XLF", "XLU", "XLB", "XLP", "XLI", "XLV")

# VaR Calculation
var <- 0.95
intraday_95_VaR <- rbind(apply(rtn_15min, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}),
                       apply(rtn_30min, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}),
                       apply(rtn_1hr, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}))

rownames(intraday_95_VaR) <- c('15_min', '30_min', '1_hr')

var <- 0.5
intraday_medians <- rbind(apply(rtn_15min, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}),
                       apply(rtn_30min, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}),
                       apply(rtn_1hr, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}))

rownames(intraday_medians) <- rownames(intraday_95_VaR)

calculate_covar <- function(df, row_num) {
  covar_95 <- c()
  delta_covar_95 <- c()
  
  for (i in 2:10) {
    regr <- lm(df[,1] ~ df[,i])
    covar_95 <- c(covar_95, regr$coefficients[1] + regr$coefficients[2] * intraday_95_VaR[row_num, i])
    delta_covar_95 <- c(delta_covar_95, regr$coefficients[2] * (intraday_95_VaR[row_num, i] - intraday_medians[row_num, i]))
  }
  daily_covar <- rbind(covar_95, delta_covar_95)
  rownames(daily_covar) <- c('Daily 95% CoVaR', 'Daily 95% Delta CoVaR')
  colnames(daily_covar) <- colnames(df[2:ncol(df)])
  
  return(daily_covar)
}

ETF <- c("XLE", "XLY", "XLK", "XLF", "XLU", "XLB", "XLP", "XLI", "XLV")

min_15_vals <- as.data.frame(t(rbind(intraday_95_VaR[1,2:10], calculate_covar(rtn_15min, 1))))
colnames(min_15_vals) <- c('VaR_95', 'CoVaR_95', 'Delta_CoVaR_95')
min_15_vals <- cbind(ETF, as.data.frame(lapply(min_15_vals, as.numeric)))

min_30_vals <- as.data.frame(t(rbind(intraday_95_VaR[1,2:10], calculate_covar(rtn_30min, 2))))
colnames(min_30_vals) <- c('VaR_95', 'CoVaR_95', 'Delta_CoVaR_95')
min_30_vals <- cbind(ETF, as.data.frame(lapply(min_30_vals, as.numeric)))

hr_1_vals <- as.data.frame(t(rbind(intraday_95_VaR[1,2:10], calculate_covar(rtn_1hr, 3))))
colnames(hr_1_vals) <- c('VaR_95', 'CoVaR_95', 'Delta_CoVaR_95')
hr_1_vals <- cbind(ETF, as.data.frame(lapply(hr_1_vals, as.numeric)))

# Graphing #####################################################################
library(ggplot2)

# 15 min
ggplot(min_15_vals, aes(x = VaR_95, y = CoVaR_95, label = ETF)) + 
  xlab('95% VaR') + ylab('95% Delta CoVaR') +
  geom_point() + geom_text(hjust=0.7, vjust=-0.7) + 
  labs(title = '95% VaR for each ETF vs Delta CoVaR 15 Minute Intraday Data')

# 30 min
ggplot(min_30_vals, aes(x = VaR_95, y = CoVaR_95, label = ETF)) + 
  xlab('95% VaR') + ylab('95% Delta CoVaR') +
  geom_point() + geom_text(hjust=0.7, vjust=-0.7) + 
  labs(title = '95% VaR for each ETF vs Delta CoVaR 30 Minute Intraday Data')

# 1 hr
ggplot(hr_1_vals, aes(x = VaR_95, y = CoVaR_95, label = ETF)) + 
  xlab('95% VaR') + ylab('95% Delta CoVaR') +
  geom_point() + geom_text(hjust=0.7, vjust=-0.7) + 
  labs(title = '95% VaR for each ETF vs Delta CoVaR 1 hour Intraday Data')

# Flash Crash ##################################################################
intraday_data <- read.csv('ETF_Cleaned_Minute_Data/All.csv')
flash_crash <- intraday_data[substr(intraday_data$Date.Time, 6, 10) == '05-06', -1][, c(9, 1, 7, 8, 2, 5, 4, 3, 10, 6)]

flash_crash_VaR <- apply(flash_crash, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-0.95)]})
flash_crash_median <- apply(flash_crash, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-0.5)]})

covar_95 <- c()
delta_covar_95 <- c()

for (i in 2:10) {
  regr <- lm(flash_crash[,1] ~ flash_crash[,i])
  covar_95 <- c(covar_95, regr$coefficients[1] + regr$coefficients[2] * flash_crash_VaR[i])
  delta_covar_95 <- c(delta_covar_95, regr$coefficients[2] * (flash_crash_VaR[i] - flash_crash_median[i]))
}

flash_crash_covar <- rbind(covar_95, delta_covar_95)
rownames(flash_crash_covar) <- c('Flash Crash 95% CoVaR', 'Flash Crash 95% Delta CoVaR')
colnames(flash_crash_covar) <- colnames(flash_crash[2:ncol(flash_crash)])

flash_crash_covar

ETF <- c("XLE", "XLY", "XLK", "XLF", "XLU", "XLB", "XLP", "XLI", "XLV")

flash_crash_vals <- as.data.frame(t(rbind(flash_crash_VaR[2:10], flash_crash_covar)))
colnames(flash_crash_vals) <- c('VaR_95', 'CoVaR_95', 'Delta_CoVaR_95')
flash_crash_vals <- cbind(ETF, as.data.frame(lapply(flash_crash_vals, as.numeric)))

flash_crash_vals

ggplot(flash_crash_vals, aes(x = VaR_95, y = CoVaR_95, label = ETF)) + 
  xlab('95% VaR') + ylab('95% Delta CoVaR') +
  geom_point() + geom_text(hjust=0.7, vjust=-0.7) + 
  labs(title = '95% VaR for each ETF vs Delta CoVaR 15 Minute Intraday Data')
