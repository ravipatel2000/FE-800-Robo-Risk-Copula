library(ggplot2)

daily_VaR <- read.csv('VaR/Hist_VaR.csv')

ETFs <- c()
for (i in colnames(daily_VaR)[2:ncol(daily_VaR)]) {
  ETFs <- c(ETFs, rep(i, 3))
}

type <- rep(c("1_daily", "2_weekly", "3_monthly"), ncol(daily_VaR)-1)

values <- c()
for (i in 2:ncol(daily_VaR)) {
  for (j in 1:3) {
    values <- c(values, daily_VaR[j, i])
  }
}

daily_VaR_data <- data.frame(ETFs, type, values)

df <- data.frame(t(daily_VaR[1,3:ncol(daily_VaR)]))
df$ETFs <- colnames(daily_VaR[3:ncol(daily_VaR)])
df <- df[order(df$X1),]

ggplot(daily_VaR_data, aes(fill = type, y = values, x = ETFs)) +
  geom_bar(position = "dodge", stat = "identity") + 
  scale_x_discrete(limits = c('SPY', rev(rownames(df)))) +
  ggtitle("95% VaR for SPY and Sector ETFs Using Daily Data") +
  xlab("ETF") + ylab("VaR") + labs(fill = "Time Period") + 
  scale_fill_discrete(labels = c("Daily", "Weekly", "Monthly"))


################################################################################

intraday_VaR <- read.csv('VaR/Hist_Intraday_VaR.csv')

ETFs <- c()
for (i in colnames(intraday_VaR)[2:ncol(intraday_VaR)]) {
  ETFs <- c(ETFs, rep(i, 3))
}

type <- rep(c("1_15_min", "2_30_min", "3_1_hr"), ncol(intraday_VaR)-1)

values <- c()
for (i in 2:ncol(intraday_VaR)) {
  for (j in 1:3) {
    values <- c(values, intraday_VaR[j, i])
  }
}

intraday_VaR_data <- data.frame(ETFs, type, values)

df <- data.frame(t(intraday_VaR[1,3:ncol(intraday_VaR)]))
df$ETFs <- colnames(intraday_VaR[3:ncol(intraday_VaR)])
df <- df[order(df$X1),]

ggplot(intraday_VaR_data, aes(fill = type, y = values, x = ETFs)) +
  geom_bar(position = "dodge", stat = "identity") + 
  scale_x_discrete(limits = c('SPY', rev(rownames(df)))) +
  ggtitle("95% VaR for SPY and Sector ETFs Using Intraday Data") +
  xlab("ETF") + ylab("VaR") + labs(fill = "Time Period") + 
  scale_fill_discrete(labels = c("15 Minute", "30 Minute", "1 Hour"))

















