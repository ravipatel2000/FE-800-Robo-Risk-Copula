daily_rtns <- read.csv('ETF_Daily_Data/ETF_Daily_Returns.csv')

pre_housing_crash <- daily_rtns[1:391, -c(1)]
housing_crash <- daily_rtns[392:809, -c(1)]
between_housing_covid_crashes <- daily_rtns[810:3554, -c(1)]
covid_crash <- daily_rtns[3555:3589, -c(1)]
after_covid_crash <- daily_rtns[3590:nrow(daily_rtns), -c(1)]

non_recession <- daily_rtns[c(1:391, 810:3554), -c(1)]
recession <- daily_rtns[c(392:809, 3555:3589), -c(1)]

# VaR Calculation
var <- 0.95
Regime_95_VaR <- rbind(apply(pre_housing_crash, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}),
                    apply(housing_crash, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}),
                    apply(between_housing_covid_crashes, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}),
                    apply(covid_crash, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}),
                    apply(after_covid_crash, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}),
                    apply(non_recession, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}),
                    apply(recession, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}))

rownames(Regime_95_VaR) <- c('pre_housing_crash', 'housing_crash', 'bewtween_housing_covid_crashes', 
                                'covid_crash', 'after_covid_crash', 'non_recession', 'recession')

var <- 0.5
Regime_Medians <- rbind(apply(pre_housing_crash, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}),
                       apply(housing_crash, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}),
                       apply(between_housing_covid_crashes, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}),
                       apply(covid_crash, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}),
                       apply(after_covid_crash, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}),
                       apply(non_recession, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}),
                       apply(recession, MARGIN = 2, FUN = function(x) {sort(x)[length(x)*(1-var)]}))

rownames(Regime_Medians) <- rownames(Regime_95_VaR)

calculate_covar <- function(df, row_num) {
  covar_95 <- c()
  delta_covar_95 <- c()
  
  for (i in 2:10) {
    regr <- lm(df[,1] ~ df[,i])
    covar_95 <- c(covar_95, regr$coefficients[1] + regr$coefficients[2] * Regime_95_VaR[row_num, i])
    delta_covar_95 <- c(delta_covar_95, regr$coefficients[2] * (Regime_95_VaR[row_num, i] - Regime_Medians[row_num, i]))
  }
  daily_covar <- rbind(covar_95, delta_covar_95)
  rownames(daily_covar) <- c('Daily 95% CoVaR', 'Daily 95% Delta CoVaR')
  colnames(daily_covar) <- colnames(df[2:ncol(df)])
  
  return(daily_covar)
}

ETF <- c("XLE", "XLY", "XLK", "XLF", "XLU", "XLB", "XLP", "XLI", "XLV")
pre_housing_vals <- as.data.frame(t(rbind(Regime_95_VaR[1,2:10], calculate_covar(pre_housing_crash, 1))))
colnames(pre_housing_vals) <- c('Daily_95_VaR', 'Daily_95_CoVaR', 'Daily_95_Delta_CoVaR')
pre_housing_vals <- cbind(ETF, as.data.frame(lapply(pre_housing_vals, as.numeric)))

housing_vals <- as.data.frame(t(rbind(Regime_95_VaR[2,2:10], calculate_covar(housing_crash, 2))))
colnames(housing_vals) <- colnames(pre_housing_vals)[2:4]
housing_vals <- cbind(ETF, as.data.frame(lapply(housing_vals, as.numeric)))

between_housing_covid_vals <- as.data.frame(t(rbind(Regime_95_VaR[3,2:10], calculate_covar(between_housing_covid_crashes, 3))))
colnames(between_housing_covid_vals) <- colnames(pre_housing_vals)[2:4]
between_housing_covid_vals <- cbind(ETF, as.data.frame(lapply(between_housing_covid_vals, as.numeric)))

covid_crash_vals <- as.data.frame(t(rbind(Regime_95_VaR[4,2:10], calculate_covar(covid_crash, 4))))
colnames(covid_crash_vals) <- colnames(pre_housing_vals)[2:4]
recession_vals <- cbind(ETF, as.data.frame(lapply(covid_crash_vals, as.numeric)))

after_covid_crash_vals <- as.data.frame(t(rbind(Regime_95_VaR[5,2:10], calculate_covar(after_covid_crash, 5))))
colnames(after_covid_crash_vals) <- colnames(pre_housing_vals)[2:4]
after_covid_crash_vals <- cbind(ETF, as.data.frame(lapply(after_covid_crash_vals, as.numeric)))

non_recession_vals <- as.data.frame(t(rbind(Regime_95_VaR[6,2:10], calculate_covar(non_recession, 6))))
colnames(non_recession_vals) <- colnames(pre_housing_vals)[2:4]
non_recession_vals <- cbind(ETF, as.data.frame(lapply(non_recession_vals, as.numeric)))

recession_vals <- as.data.frame(t(rbind(Regime_95_VaR[7,2:10], calculate_covar(recession, 7))))
colnames(recession_vals) <- colnames(pre_housing_vals)[2:4]
recession_vals <- cbind(ETF, as.data.frame(lapply(recession_vals, as.numeric)))

VaR <- read.csv('VaR/Hist_VaR.csv')
rownames(VaR) <- VaR[,1]
VaR <- VaR[,c(-1)]
CoVaR <- read.csv('CoVaR/Daily_CoVaR.csv')
rownames(CoVaR) <- CoVaR[,1]
CoVaR <- CoVaR[,c(-1)]

df <- cbind(t(VaR[1,-1]), t(CoVaR[3,]))
colnames(df) <- c('Entire_Dataset_VaR_95', 'Entire_Dataset_Delta_CoVaR_95')
df <- as.data.frame(df)
df$ETF <- rownames(df)


# Graphing #####################################################################
library(ggplot2)

# Entire Dataset
ggplot(df, aes(x = Entire_Dataset_VaR_95, y = Entire_Dataset_Delta_CoVaR_95, label = ETF)) + 
  xlab('95% VaR') + ylab('95% Delta CoVaR') +
  geom_point() + geom_text(hjust=0.7, vjust=-0.7) + 
  labs(title = '95% VaR for each ETF vs Delta CoVaR Relative to SPY Over Entire Dataset')

# Non-Recession
ggplot(non_recession_vals, aes(x = Daily_95_VaR, y = Daily_95_Delta_CoVaR, label = ETF)) + 
  xlab('95% VaR') + ylab('95% Delta CoVaR') +
  geom_point() + geom_text(hjust=0.7, vjust=-0.7) + 
  labs(title = '95% VaR for each ETF vs Delta CoVaR Relative to SPY Not During Recessions')

# Recession
ggplot(recession_vals, aes(x = Daily_95_VaR, y = Daily_95_Delta_CoVaR, label = ETF)) + 
  xlab('95% VaR') + ylab('95% Delta CoVaR') +
  geom_point() + geom_text(hjust=0.7, vjust=-0.7) + 
  labs(title = '95% VaR for each ETF vs Delta CoVaR Relative to SPY During Recessions')

# Pre Housing Crisis
ggplot(pre_housing_vals, aes(x = Daily_95_VaR, y = Daily_95_Delta_CoVaR, label = ETF)) + 
  xlab('95% VaR') + ylab('95% Delta CoVaR') +
  geom_point() + geom_text(hjust=0.7, vjust=-0.7) + 
  labs(title = '95% VaR for each ETF vs Delta CoVaR Relative to SPY Before Great Recession')

# Housing Crisis
ggplot(housing_vals, aes(x = Daily_95_VaR, y = Daily_95_Delta_CoVaR, label = ETF)) + 
  xlab('95% VaR') + ylab('95% Delta CoVaR') +
  geom_point() + geom_text(hjust=0.7, vjust=-0.7) + 
  labs(title = '95% VaR for each ETF vs Delta CoVaR Relative to SPY During Great Recession')

# Between Housing and Covid Crashes
ggplot(between_housing_covid_vals, aes(x = Daily_95_VaR, y = Daily_95_Delta_CoVaR, label = ETF)) + 
  xlab('95% VaR') + ylab('95% Delta CoVaR') +
  geom_point() + geom_text(hjust=0.7, vjust=-0.7) + 
  labs(title = '95% VaR for each ETF vs Delta CoVaR Relative to SPY Between Great Recession and Covid')

# During Covid Crash
ggplot(covid_crash_vals, aes(x = Daily_95_VaR, y = Daily_95_Delta_CoVaR, label = ETF)) + 
  xlab('95% VaR') + ylab('95% Delta CoVaR') +
  geom_point() + geom_text(hjust=0.7, vjust=-0.7) + 
  labs(title = '95% VaR for each ETF vs Delta CoVaR Relative to SPY During Covid-19 Recession')


# After Covid Crash
ggplot(after_covid_crash_vals, aes(x = Daily_95_VaR, y = Daily_95_Delta_CoVaR, label = ETF)) + 
  xlab('95% VaR') + ylab('95% Delta CoVaR') +
  geom_point() + geom_text(hjust=0.7, vjust=-0.7) + 
  labs(title = '95% VaR for each ETF vs Delta CoVaR Relative to SPY After Covid-19 Recession')





