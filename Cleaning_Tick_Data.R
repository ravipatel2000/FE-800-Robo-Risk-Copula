library(dplyr)
library(zoo)

tick_data <- read.csv('All_ETF_Minute_Tick_Data.csv')

df <- tick_data[,c(1,4, 5,10)]
colnames(df) <- c('ETF', 'Date.Time', 'GMT.Offset', 'Close')

SPY <- df[df$ETF == 'SPY',]
XLB <- df[df$ETF == 'XLB',]
XLE <- df[df$ETF == 'XLE',]
XLF <- df[df$ETF == 'XLF',]
XLI <- df[df$ETF == 'XLI',]
XLK <- df[df$ETF == 'XLK',]
XLP <- df[df$ETF == 'XLP',]
XLU <- df[df$ETF == 'XLU',]
XLV <- df[df$ETF == 'XLV',]
XLY <- df[df$ETF == 'XLY',]

###################################

clean_etf_df <- function(x) {
  # Create UTC.Hour, EST.Hour and Minute columns
  x[,5] <- as.numeric(lapply(x[,2], FUN = function(x){substr(x, 12, 13)}))
  x[,6] <- x[,5] - 5
  x[,6] <- as.numeric(lapply(x[,6], FUN = function(x){if(x<0) {x = 24+x}else{x}}))
  x[,7] <- as.numeric(lapply(x[,2], FUN = function(x){substr(x, 15, 16)}))
  
  # Filter for values bewteen 9:30AM and 4PM (Market hours)
  sum(((x[,6] == 9)  & (x[,7] >= 30)) | ((x[,6] > 9) & (x[,6] < 16)))
  x <- (x[((x[,6] == 9)  & (x[,7] >= 30)) | ((x[,6] > 9) & (x[,6] < 16)),])
  # Fill na values with last non na value
  x[,4] <- na.locf(x[,4])
  
  # Calculate log and simple returns
  x[,8] <- c(0, diff(log(x[,4])))
  x[,9] <- c(0, (x[,4][2:nrow(x)] - x[,4][1:(nrow(x) - 1)])/(x[,4][1:(nrow(x) - 1)]))
  
  # Set column names
  colnames(x) <- c('ETF', 'Date.Time', 'GMT.Offset', 'Close', 'UTC.Hour', 'EST.Hour', 'Minute', 'Log.Rtn', 'Simple.Rtn')
  return(x[, c(1, 2, 3, 6, 7, 4, 8, 9)])
}

# Running clean_etf_df for each etf
SPY_clean <- clean_etf_df(SPY)
XLB_clean <- clean_etf_df(XLB)
XLE_clean <- clean_etf_df(XLE)
XLF_clean <- clean_etf_df(XLF)
XLI_clean <- clean_etf_df(XLI)
XLK_clean <- clean_etf_df(XLK)
XLP_clean <- clean_etf_df(XLP)
XLU_clean <- clean_etf_df(XLU)
XLV_clean <- clean_etf_df(XLV)
XLY_clean <- clean_etf_df(XLY)

# Full year log and simple returns calculated from minute returns
sum(XLY_clean$Log.Rtn)
prod(XLY_clean$Simple.Rtn + 1)-1

# First and last close price
head(XLY_clean$Close, 1)
tail(XLY_clean$Close, 1)

# Actual log and simple returns for full year
log(37.43/30.05)
(37.43-30.05)/(30.05)

write.csv(SPY_clean, 'ETF_Cleaned_Minute_Data/SPY.csv')
write.csv(XLB_clean, 'ETF_Cleaned_Minute_Data/XLB.csv')
write.csv(XLE_clean, 'ETF_Cleaned_Minute_Data/XLE.csv')
write.csv(XLF_clean, 'ETF_Cleaned_Minute_Data/XLF.csv')
write.csv(XLI_clean, 'ETF_Cleaned_Minute_Data/XLI.csv')
write.csv(XLK_clean, 'ETF_Cleaned_Minute_Data/XLK.csv')
write.csv(XLP_clean, 'ETF_Cleaned_Minute_Data/XLP.csv')
write.csv(XLU_clean, 'ETF_Cleaned_Minute_Data/XLU.csv')
write.csv(XLV_clean, 'ETF_Cleaned_Minute_Data/XLV.csv')
write.csv(XLY_clean, 'ETF_Cleaned_Minute_Data/XLY.csv')

