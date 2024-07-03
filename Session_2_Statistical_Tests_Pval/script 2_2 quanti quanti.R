# go get english on my computer
Sys.setenv(LANG='en')

# Load required libraries --------------------------------------------------
library(Rcmdr)

# Correlation --------------------------------------------------------------
# https://github.com/ngay/kustats/blob/master/data/ds16_Nobel_Laureates_and_Chocolate.rda
# https://www.scribbr.com/statistics/pearson-correlation-coefficient/

# data = ds16_Nobel_Laureates_and_Chocolate
# write.csv2(data, "chocolat_nobel.csv")

data <- read.csv2("chocolate_nobel.csv")

# Plot ---------------------------------------------------------------------
plot(data$CHOCOLATE, data$NOBEL)
plot(data$NOBEL~data$CHOCOLATE)
# Correlation --------------------------------------------------------------
cor.test(data$CHOCOLATE, data$NOBEL, method = "pearson")

shapiro.test(data$NOBEL)
shapiro.test(data$CHOCOLATE)

# Plot the histogram
hist(data$NOBEL, breaks = 10, prob = TRUE, main = "Histogram of Data")

# Plot the normal distribution density plot
# x <- seq(min(data$NOBEL), max(data$NOBEL), length = 100)
# lines(x, dnorm(x, mean(data$NOBEL), sd(data$NOBEL)), col = "blue")
qqnorm(data$NOBEL)
qqline(data$NOBEL)

cor.test(data$CHOCOLATE, data$NOBEL, method = "pearson")
cor.test(data$CHOCOLATE, data$NOBEL, method = "spearman")
