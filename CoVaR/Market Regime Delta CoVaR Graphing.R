library(ggplot2)
library(ggrepel)

# Entire Dataset
ggplot(df, aes(x = Entire_Dataset_VaR_95, y = Entire_Dataset_Delta_CoVaR_95, label = ETF)) + 
  ylab("95% Delta CoVaR") + xlab("95% Value at Risk") +
  geom_point(color = "#514AD6") + geom_text(hjust=0.7, vjust=-0.7) + 
  labs(title = 'VaR vs Delta CoVaR Relative to SPY Over Entire Dataset')
  
  
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

df1 <- cbind(rep("1 All Data", nrow(non_recession_vals)), df[, c(3, 1, 2)])
df2 <- cbind(rep("2 Bull Market", nrow(non_recession_vals)), non_recession_vals[,c(1, 2, 4)])
df3 <- cbind(rep("3 Bear Market", nrow(recession_vals)), recession_vals[,c(1, 2, 4)])

colnames(df1) <- c("Market", "ETF", "VaR", "Delta_CoVaR")
colnames(df2) <- c("Market", "ETF", "VaR", "Delta_CoVaR")
colnames(df3) <- c("Market", "ETF", "VaR", "Delta_CoVaR")

dataset1 <- rbind(df1, df2, df3)
dataset1$Market <- as.factor(dataset1$Market)
dataset1$ETF <- as.factor(dataset1$ETF)

ggplot(dataset1, aes(x = VaR, y = Delta_CoVaR, colour = Market)) +
  geom_point(key_glyph = draw_key_point) +
  scale_color_manual(labels = c("All Data", "Bull Market", "Recession"), values = c("#2271DD", "#52C61D", "#D10533")) +
  geom_label_repel(data = dataset1, 
                            mapping = aes(x = VaR, y = Delta_CoVaR, label = ETF)) +
  ggtitle("VaR vs Delta CoVaR Relative to SPY in Bear vs Bull Markets From 2006 Through 2022") +
  ylab("95% Delta CoVaR") + xlab("95% Value at Risk")

####################################################################################

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

min_15_vals


df1 <- cbind(rep("15 Minute", nrow(min_15_vals)), min_15_vals[,c(1, 2, 4)])
df2 <- cbind(rep("30 Minute", nrow(min_30_vals)), min_30_vals[,c(1, 2, 4)])
df3 <- cbind(rep("1 Hour", nrow(hr_1_vals)), hr_1_vals[,c(1, 2, 4)])

colnames(df1) <- c("Time", "ETF", "VaR", "Delta_CoVaR")
colnames(df2) <- c("Time", "ETF", "VaR", "Delta_CoVaR")
colnames(df3) <- c("Time", "ETF", "VaR", "Delta_CoVaR")

dataset2 <- rbind(df1, df2, df3)
dataset2$Time <- as.factor(dataset2$Time)
dataset2$ETF <- as.factor(dataset2$ETF)


ggplot(dataset2, aes(x = VaR, y = Delta_CoVaR, colour = Time)) +
  geom_point(key_glyph = draw_key_point) +
  scale_color_manual(values = c("#514AD6", "#A8294A", "#29A888")) +
  geom_label_repel(data = dataset2, 
                   mapping = aes(x = VaR, y = Delta_CoVaR, label = ETF)) +
  ggtitle("VaR vs Delta CoVaR Relative to SPY in Bear vs Bull Markets From 2006 Through 2022") +
  ylab("95% Delta CoVaR") + xlab("95% Value at Risk")

####################################################################################

dataset1

df1 <- cbind(rep("1 All Data", nrow(non_recession_vals)), df[, c(3, 1, 2)])
df2 <- cbind(rep("2 Bull Market", nrow(non_recession_vals)), non_recession_vals[,c(1, 2, 4)])
df3 <- cbind(rep("3 Bear Market", nrow(recession_vals)), recession_vals[,c(1, 2, 3)])

colnames(df1) <- c("Market", "ETF", "VaR", "Delta_CoVaR")
colnames(df2) <- c("Market", "ETF", "VaR", "Delta_CoVaR")
colnames(df3) <- c("Market", "ETF", "VaR", "Delta_CoVaR")

dataset1 <- rbind(df1, df2, df3)
dataset1$Market <- as.factor(dataset1$Market)
dataset1$ETF <- as.factor(dataset1$ETF)

dataset1$ratio <- dataset1$Delta_CoVaR/dataset1$VaR

ggplot(dataset1, aes(fill = Market, y = ratio, x = ETF)) +
  geom_bar(position = "dodge", stat = "identity") + 
  scale_x_discrete(limits = c("XLE", "XLF", "XLB", "XLU", "XLY", "XLK", "XLI", "XLV", "XLP")) +
  ggtitle("Ratio of Delta CoVaR to VaR for each ETF in Different Market Regimes") +
  xlab("ETF") + ylab("Ratio of Delta CoVaR to VaR") + labs(fill = "Data")  + 
  scale_fill_manual(labels = c("All Data", "Bull Market", "Recession"), values = c("#2271DD", "#52C61D", "#D10533")) +
  scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2))


