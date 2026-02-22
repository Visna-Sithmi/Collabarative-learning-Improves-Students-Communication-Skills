data <- read.csv("C:\\Users\\ASUS\\Downloads\\CY07_MSU_STU_QQQ.csv")

# Convert to data frame (safety step)
# We do it to make sure the dataset is a data frame format, so all data manipulation functions work properly.
data <- as.data.frame(data)

# Check structure and summary of dataset
str(data)
summary(data)

# Independent Variable (Collaborative Learning items)
collaborative_vars <- c("ST100Q03TA",
                        "ST100Q04TA",
                        "ST102Q02TA",
                        "ST100Q02TA",
                        "ST104Q04NA",
                        "ST104Q02NA",
                        "ST206Q02HA",
                        "ST206Q04HA")

# Dependent Variable (Communication Skill items)
communication_vars <- c("ST097Q01TA",
                        "ST097Q03TA",
                        "ST211Q03HA",
                        "ST211Q02HA",
                        "ST213Q01HA",
                        "ST213Q02HA",
                        "ST218Q03HA",
                        "ST218Q02HA",
                        "ST218Q06HA",
                        "ST218Q04HA")

# Extract only selected variables
data_selected <- data[, c(collaborative_vars, communication_vars)]

names(data_selected)
names(data)
summary(data_selected)

# Save into working dataframe
df <- data_selected
dim(df)

# Detect missing values

# How many NA per column
na_count <- colSums(is.na(data_selected))
na_count

# NA percentage per column
na_pct <- round(na_count / nrow(data_selected) * 100, 2)
na_pct

# see how many duplicates
sum(duplicated(df))

# Keep rows that are NOT duplicates
df <- df[!duplicated(df), ]
dim(df)

# Make Sure All Variables Are Numeric
df[] <- lapply(df, function(x) as.numeric(x))

# Split dataset into 2 parts (IV group and DV group)
collab_df <- df[, collaborative_vars]
comm_df   <- df[, communication_vars]

# Filter Students Based on Minimum Responses
min_collab <- 4
min_comm   <- 5

# Count how many answers each student gave
collab_answered <- rowSums(!is.na(collab_df))
comm_answered   <- rowSums(!is.na(comm_df))

# Decide which students to keep
keep_rows <- (collab_answered >= min_collab) &
  (comm_answered >= min_comm)

df_clean <- df[keep_rows, ]

table(collab_answered)
table(comm_answered)

# ------------------------------------------------------------
# REVERSE CODING 
# These items are negatively worded; reverse so higher = better.
# Scale is 1-4, so reverse using: new = 5 - old
df_clean$ST097Q01TA <- 5 - df_clean$ST097Q01TA
df_clean$ST097Q03TA <- 5 - df_clean$ST097Q03TA
# ------------------------------------------------------------

# Calculate average collaborative learning score
# na.rm = TRUE ignores missing responses
df_clean$collaborative_score <- rowMeans(df_clean[, collaborative_vars], na.rm = TRUE)
df_clean$communication_score <- rowMeans(df_clean[, communication_vars], na.rm = TRUE)

table(collab_answered)
table(comm_answered)

# check distribution of final scores
summary(df_clean$collaborative_score)
summary(df_clean$communication_score)

# IMPORTANT: keep your original code, but put scores AFTER this final filter
keep_rows <- (collab_answered >= 5) &
  (comm_answered >= 6)

df_clean <- df[keep_rows, ]

dim(df_clean)

# ------------------------------------------------------------
# REVERSE CODING AGAIN (because df_clean was recreated above)
df_clean$ST097Q01TA <- 5 - df_clean$ST097Q01TA
df_clean$ST097Q03TA <- 5 - df_clean$ST097Q03TA
# Recalculate scores again (because df_clean was recreated)
df_clean$collaborative_score <- rowMeans(df_clean[, collaborative_vars], na.rm = TRUE)
df_clean$communication_score <- rowMeans(df_clean[, communication_vars], na.rm = TRUE)
# ------------------------------------------------------------

getwd()
setwd("C:/Users/ASUS/OneDrive/Documents/GitHub/Collabarative-learning-Improves-Students-Communication-Skills")

getwd()

# Save cleaned dataset
write.csv(df_clean,
          "data/cleaned/cleaned_collab_comm.csv",
          row.names = FALSE)

# counts how many missing values (NA) are in that column.
sum(is.na(df_clean$collaborative_score))
sum(is.na(df_clean$communication_score))

