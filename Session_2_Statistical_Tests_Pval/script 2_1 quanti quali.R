# go get english on my computer
Sys.setenv(LANG='en')

# packages ----------------------------------------------------------------
library(Rcmdr)

# Load data from CSV file ------------------------------------------------
HP <- read.csv2("Wand_HP.csv")
head(HP)

# Base R for boxplot and adding points (dot plot alternative) ------------
boxplot(Wand_size ~ Gender, data = HP,col = c("purple", "gray"))

# Hypothesis application: Test normality in each group -------------------
HP_F <- HP[HP$Gender == 'Female', ]
HP_M <- HP[HP$Gender == 'Male', ]

# Shapiro-Wilk normality test
shapiro.test(HP_F$Wand_size)
shapiro.test(HP_M$Wand_size)

# Plot histogram and density plot for 'Female'
hist(HP_F$Wand_size, breaks = 10)
x <- seq(min(HP_F$Wand_size), max(HP_F$Wand_size), length = 100)

qqnorm(HP_F$Wand_size)
qqline(HP_F$Wand_size)

# Plot histogram and density plot for 'Male'
hist(HP_M$Wand_size, breaks = 10, prob = TRUE, main = "Histogram of Data - Male", xlab = "Length of Baguette (cm)")
x <- seq(min(HP_M$Wand_size), max(HP_M$Wand_size), length = 100)
lines(x, dnorm(x, mean(HP_M$Wand_size), sd(HP_M$Wand_size)), col = "blue")
qqnorm(HP_M$Wand_size)
qqline(HP_M$Wand_size)

# Test for homoscedasticity (equal variances)
var.test(Wand_size ~ Gender, data = HP)

# Summarize variance by group using base R
aggregate(Wand_size ~ Gender, data = HP, FUN = var)

# t-test ------------------------------------------------------------------
t.test(Wand_size ~ Gender, HP)
wilcox.test(Wand_size ~ Gender,HP)


