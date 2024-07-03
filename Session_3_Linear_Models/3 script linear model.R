# go get english on my computer
Sys.setenv(LANG='en')

# Load necessary packages ------------------------------------------------
library(emmeans)     # For post hoc tests
library(car)         # For model diagnostics
library(Rcmdr)         # Fo Button click

# Importing the dataset --------------------------------------------------
data <- read.csv2("pokemon_sub.csv")  # Using read_csv for comma-separated values

# exploration  -----------------------------------------------------------
#sampling effort
table(data$type1)

#outliers
boxplot(data$base_happiness, main = "Base Happiness", ylab = "Base Happiness")
boxplot(data$height_m, main = "Height (m)", ylab = "Height in meters")
boxplot(data$attack, main = "Attack", ylab = "Attack")
boxplot(data$percentage_male, main = "Percentage Male", ylab = "Percentage Male")
boxplot(data$speed, main = "Speed", ylab = "Speed")

#relationships
boxplot(data$speed ~ data$type1)

plot(data$speed ~ data$attack)
plot(data$speed ~ data$height_m)

# Fitting linear model --------------------------------------------------
mod <- lm(speed ~ height_m + attack + type1 + percentage_male + base_happiness, data = data)
data2 <- data[-85, ]  # Excluding the 85th row for robustness check
mod_updated <- lm(speed ~ height_m + attack + type1 + percentage_male + base_happiness, data = data2)

# Model diagnostics ----------------------------------------------------
Anova(mod_updated)
summary(mod_updated)

# Checking for collinearity ---------------------------------------------
vif(mod_updated)  # Variance inflation factors

# Post hoc analysis -----------------------------------------------------
tcm <- emmeans(mod_updated, pairwise ~ type1)
print(tcm)

# Base R functions for scatter plots with points and lines --------------
boxplot(data$speed ~ data$type1, xlab = "Type", ylab = "Speed", main = "Speed by Type")

plot(data$speed ~ data$attack, xlab = "Attack", ylab = "Speed", main = "Speed vs. Attack")

plot(data$speed ~ data$height_m, xlab = "Height (m)", ylab = "Speed", main = "Speed vs. Height")
