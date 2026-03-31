# Load cleaned data
df <- read.csv("C:/Users/ASUS/Downloads/PISA_Cleaned_Data.csv")

# Summary statistics
summary(df$Communication_Score)
summary(df$Collaboration_Score)

sd(df$Communication_Score, na.rm = TRUE)
sd(df$Collaboration_Score, na.rm = TRUE)

median(df$Communication_Score, na.rm = TRUE)
median(df$Collaboration_Score, na.rm = TRUE)

# Histograms
hist(df$Communication_Score,
     main="Communication Score Distribution",
     xlab="Communication Score")

hist(df$Collaboration_Score,
     main="Collaboration Score Distribution",
     xlab="Collaboration Score")

# Boxplots
boxplot(df$Communication_Score,
        main="Communication Score Boxplot")

boxplot(df$Collaboration_Score,
        main="Collaboration Score Boxplot")

# Correlation matrix
cor(df[, c("Communication_Score",
           "Collaboration_Score")],
    use="complete.obs")

# Scatter plot
plot(df$Collaboration_Score,
     df$Communication_Score,
     main="Collaboration vs Communication",
     xlab="Collaboration Score",
     ylab="Communication Score")

