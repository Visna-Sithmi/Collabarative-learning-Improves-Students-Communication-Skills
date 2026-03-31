# ------------------------------------------------------------
# PISA 2018 Student Questionnaire - Data Cleaning + Validation
# Topic: Collaborative Learning improves Students' Communication Skills
# ------------------------------------------------------------

# 1) Load the dataset
data <- read.csv("C:\\Users\\ASUS\\Downloads\\CY07_MSU_STU_QQQ.csv", stringsAsFactors = FALSE)

# Basic checks (raw)
cat("RAW DATASET DIMENSIONS (rows, cols):\n")
print(dim(data))
cat("Unique CNTSTUID (raw): ", length(unique(data$CNTSTUID)), "\n")
cat("Duplicate CNTSTUID count (raw): ", sum(duplicated(data$CNTSTUID)), "\n\n")

# 2) Select only required variables
df_selected <- data[, c(
  "CNTSTUID",
  "ST097Q01TA",
  "ST218Q03HA",
  "ST218Q02HA",
  "ST218Q06HA",
  "ST218Q04HA",
  "ST206Q04HA",
  "ST206Q02HA",
  "ST153Q03HA",
  "ST176Q06IA",
  "ST206Q01HA"
)]

# 3) Ensure selected columns (except CNTSTUID) are numeric
vars <- setdiff(names(df_selected), "CNTSTUID")
df_selected[vars] <- lapply(df_selected[vars], function(x) as.numeric(x))

# 4) Replace invalid / special codes with NA
# (7/8/9 and 97/98/99 represent non-valid responses in PISA codebooks)
df_selected[df_selected %in% c(7, 8, 9, 97, 98, 99)] <- NA

# 5) Missing values check (before scoring)
cat("Missing values per selected variable (before scoring):\n")
print(colSums(is.na(df_selected)))
cat("\n")

# 6) Duplicate check on selected data
cat("Duplicate CNTSTUID count (selected): ", sum(duplicated(df_selected$CNTSTUID)), "\n\n")

# ------------------------------------------------------------
# 7) Bring collaboration items to a common 1–4 scale (recommended)
# ------------------------------------------------------------

# 7a) Rescale ST176Q06IA from 1–5 to 1–4:
# new = 1 + (old - 1) * (3/4)
df_selected$ST176Q06IA_scaled <- ifelse(
  is.na(df_selected$ST176Q06IA),
  NA,
  1 + (df_selected$ST176Q06IA - 1) * (3/4)
)

# 7b) Convert ST153Q03HA: 1=Yes, 2=No -> Yes=4, No=1
df_selected$ST153Q03HA_scaled <- ifelse(
  is.na(df_selected$ST153Q03HA),
  NA,
  ifelse(df_selected$ST153Q03HA == 1, 4,
         ifelse(df_selected$ST153Q03HA == 2, 1, NA))
)

# ------------------------------------------------------------
# 8) Create composite scores WITH a "minimum answered items" rule
#    Rule: At least 3 out of 5 items must be answered for each score.
# ------------------------------------------------------------

# Communication items (all 1–4)
comm_items <- c("ST097Q01TA", "ST218Q03HA", "ST218Q02HA", "ST218Q06HA", "ST218Q04HA")
df_selected$Comm_valid_count <- rowSums(!is.na(df_selected[, comm_items]))

df_selected$Communication_Score <- ifelse(
  df_selected$Comm_valid_count >= 3,
  rowMeans(df_selected[, comm_items], na.rm = TRUE),
  NA
)

# Collaboration items (use scaled versions for ST153Q03HA + ST176Q06IA)
collab_items <- c("ST206Q04HA", "ST206Q02HA", "ST153Q03HA_scaled", "ST176Q06IA_scaled", "ST206Q01HA")
df_selected$Collab_valid_count <- rowSums(!is.na(df_selected[, collab_items]))


df_selected$Collaboration_Score <- ifelse(
  df_selected$Collab_valid_count >= 3,
  rowMeans(df_selected[, collab_items], na.rm = TRUE),
  NA
)

# 9) Keep only rows where BOTH composite scores exist
df_clean <- df_selected[
  !is.na(df_selected$Communication_Score) &
    !is.na(df_selected$Collaboration_Score),
]

cat("Rows after cleaning (usable for analysis): ", nrow(df_clean), "\n\n")

# ------------------------------------------------------------
# 10) Validation checks (Is cleaning OK?)
# ------------------------------------------------------------

cat("STRUCTURE OF CLEAN DATA:\n")
print(str(df_clean))

cat("\nMissing values in df_clean (note: item-level NA can remain, scores should be 0):\n")
print(colSums(is.na(df_clean)))

cat("\nDuplicate CNTSTUID count (clean): ", sum(duplicated(df_clean$CNTSTUID)), "\n\n")

cat("Score ranges:\n")
print(range(df_clean$Communication_Score))
print(range(df_clean$Collaboration_Score))
cat("\n")

cat("Score summaries:\n")
cat("Communication_Score:\n"); print(summary(df_clean$Communication_Score))
cat("\nCollaboration_Score:\n"); print(summary(df_clean$Collaboration_Score))
cat("\n")

cat("Correlation (Communication vs Collaboration):\n")
print(cor(df_clean[, c("Communication_Score", "Collaboration_Score")] , use = "complete.obs"))
cat("\n")

# Variance check for original selected variables (NA-safe)
cat("Variance of selected original variables (NA-safe):\n")
print(apply(df_clean[, vars], 2, var, na.rm = TRUE))
cat("\n")

# ------------------------------------------------------------
# 11) Outlier detection (IQR rule) using boxplot
# ------------------------------------------------------------

boxplot(df_clean$Communication_Score, main = "Communication Score")
boxplot(df_clean$Collaboration_Score, main = "Collaboration Score")

comm_out_vals <- boxplot.stats(df_clean$Communication_Score)$out
collab_out_vals <- boxplot.stats(df_clean$Collaboration_Score)$out

cat("Number of Communication outlier VALUES (IQR rule): ", length(comm_out_vals), "\n")
cat("Number of Collaboration outlier VALUES (IQR rule): ", length(collab_out_vals), "\n\n")

# ------------------------------------------------------------
# 12) Histograms (distribution check)
# ------------------------------------------------------------

hist(df_clean$Communication_Score, main = "Histogram: Communication Score", xlab = "Communication Score")
hist(df_clean$Collaboration_Score, main = "Histogram: Collaboration Score", xlab = "Collaboration Score")

# ------------------------------------------------------------
# 13) Simple regression (Inferential + Predictive)
# ------------------------------------------------------------

model <- lm(Communication_Score ~ Collaboration_Score, data = df_clean)
cat("\nRegression Summary:\n")
print(summary(model))

# ------------------------------------------------------------
# 14) Export cleaned dataset to CSV
# ------------------------------------------------------------

write.csv(df_clean,
          "C:\\Users\\ASUS\\Downloads\\PISA_Cleaned_Data.csv",
          row.names = FALSE)



