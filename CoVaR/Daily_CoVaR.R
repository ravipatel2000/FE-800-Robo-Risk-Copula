library(dplyr)

# Read daily csv
daily_rtns <- read.csv('ETF_Daily_Data/ETF_Daily_Returns.csv')[, c(-1)]
# Read weekly csv
weekly_rtns <- read.csv('ETF_Daily_Data/ETF_Weekly_Returns.csv')[, c(-1)]
# Read monthly csv
monthly_rtns <- read.csv('ETF_Daily_Data/ETF_Monthly_Returns.csv')[, c(-1)]

VaR <- read.csv('VaR/Hist_VaR.csv')
daily_VaR <- VaR[c(1, 4, 7),]
weekly_VaR <- VaR[c(2, 5, 8),]
monthly_VaR <- VaR[c(3, 6, 9),]

covar_95 <- c()
covar_99 <- c()
delta_covar_95 <- c()
delta_covar_99 <- c()

for (i in 2:10) {
  regr <- lm(daily_rtns[,1] ~ daily_rtns[,2])
  covar_95 <- c(covar_95, regr$coefficients[1] + regr$coefficients[2] * daily_VaR$XLE[1])
  covar_99 <- c(covar_99, regr$coefficients[1] + regr$coefficients[2] * daily_VaR$XLE[2])
  delta_covar_95 <- c(delta_covar_95, regr$coefficients[2] * (daily_VaR$XLE[1] - daily_VaR$XLE[3]))
  delta_covar_99 <- c(delta_covar_99, regr$coefficients[2] * (daily_VaR$XLE[2] - daily_VaR$XLE[3]))
}
daily_covar <- rbind(covar_95, covar_99, delta_covar_95, delta_covar_99)
rownames(daily_covar) <- c('Daily 95% CoVaR', 'Daily 99% CoVaR', 'Daily 95% Delta CoVaR', 'Daily 99% Delta CoVaR')
colnames(daily_covar) <- colnames(daily_VaR[3:ncol(daily_VaR)])

covar_95 <- c()
covar_99 <- c()
delta_covar_95 <- c()
delta_covar_99 <- c()

for (i in 2:10) {
  regr <- lm(weekly_rtns[,1] ~ weekly_rtns[,2])
  covar_95 <- c(covar_95, regr$coefficients[1] + regr$coefficients[2] * weekly_VaR$XLE[1])
  covar_99 <- c(covar_99, regr$coefficients[1] + regr$coefficients[2] * weekly_VaR$XLE[2])
  delta_covar_95 <- c(delta_covar_95, regr$coefficients[2] * (weekly_VaR$XLE[1] - weekly_VaR$XLE[3]))
  delta_covar_99 <- c(delta_covar_99, regr$coefficients[2] * (weekly_VaR$XLE[2] - weekly_VaR$XLE[3]))
}
weekly_covar <- rbind(covar_95, covar_99, delta_covar_95, delta_covar_99)
rownames(weekly_covar) <- c('Weekly 95% CoVaR', 'Weekly 99% CoVaR', 'Weekly 95% Delta CoVaR', 'Weekly 99% Delta CoVaR')
colnames(weekly_covar) <- colnames(daily_covar)

covar_95 <- c()
covar_99 <- c()
delta_covar_95 <- c()
delta_covar_99 <- c()

for (i in 2:10) {
  regr <- lm(monthly_rtns[,1] ~ monthly_rtns[,2])
  covar_95 <- c(covar_95, regr$coefficients[1] + regr$coefficients[2] * monthly_VaR$XLE[1])
  covar_99 <- c(covar_99, regr$coefficients[1] + regr$coefficients[2] * monthly_VaR$XLE[2])
  delta_covar_95 <- c(delta_covar_95, regr$coefficients[2] * (monthly_VaR$XLE[1] - monthly_VaR$XLE[3]))
  delta_covar_99 <- c(delta_covar_99, regr$coefficients[2] * (monthly_VaR$XLE[2] - monthly_VaR$XLE[3]))
}
monthly_covar <- rbind(covar_95, covar_99, delta_covar_95, delta_covar_99)
rownames(monthly_covar) <- c('Monthly 95% CoVaR', 'Monthly 99% CoVaR', 'Monthly 95% Delta CoVaR', 'Monthly 99% Delta CoVaR')
colnames(monthly_covar) <- colnames(daily_covar)

CoVaR <- rbind(daily_covar, weekly_covar, monthly_covar)

write.csv(CoVaR, 'CoVaR/Daily_CoVaR.csv')
