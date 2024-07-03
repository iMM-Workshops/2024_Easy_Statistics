
# Load data ---------------------------------------------------------------
titanic <- read.csv('titanic.csv')
print(titanic)

# Contingency table -------------------------------------------------------
contingency_table <- table(titanic$Pclass, titanic$Survived)

# Chi-square test ---------------------------------------------------------
chi_square_test <- chisq.test(contingency_table)
chi_square_test
chi_square_test$expected

# Fisher's Exact Test for Count Data --------------------------------------
fisher.test(contingency_table)

# Graphs ------------------------------------------------------------------
# Create frequency data frame from contingency table
data_freq <- as.data.frame(contingency_table)
names(data_freq) <- c("Pclass", "Survived", "Freq")

# Plot for Pclass vs. Frequency by Survival status
barplot(Freq ~ Pclass + Survived, data = data_freq, beside = TRUE,
        col = c("cornsilk2", "black"), legend.text = c("Survived", "Not Survived"),
        args.legend = list(title = "Survival", x = "topright"),
        main = "Count of Passengers by Pclass and Survival",
        ylab = "Count", xlab = "Pclass", ylim = c(0, max(data_freq$Freq) + 10))
