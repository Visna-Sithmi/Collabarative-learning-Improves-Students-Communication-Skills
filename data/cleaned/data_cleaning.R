# Read the dataset
data <- read.csv("C:\\Users\\ASUS\\Downloads\\CY07_MSU_STU_QQQ.csv", stringsAsFactors = FALSE)

# Convert to data frame (safety step)
data <- as.data.frame(data)

# Relevant Variables
collaborative_vars <- c("ST206Q04HA", "ST206Q02HA", "ST153Q03HA", "ST176Q06IA", "ST206Q01HA")
communication_vars <- c("ST097Q01TA", "ST218Q03HA", "ST218Q02HA", "ST218Q06HA", "ST218Q04HA")

# Combine the variables into one vector
selected_vars <- c("STRATUM", collaborative_vars, communication_vars)


# Extract only the selected variables
data_selected <- data[, selected_vars]

# Remove duplicate rows based on student ID
data_selected <- data_selected[!duplicated(data_selected$STRATUM), ]


# Check for missing values
na_count <- colSums(is.na(data_selected))
na_pct <- round(na_count / nrow(data_selected) * 100, 2)
print(na_pct)

# Option 1: Remove rows with too many NAs
data_selected <- na.omit(data_selected)

# Option 2: Impute missing values (e.g., fill NAs with column mean or median)
# Impute missing values with column mean (for numeric columns)
data_selected[] <- lapply(data_selected, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))




# Step 1: Check the number of rows before removing duplicates
cat("Rows before removing duplicates:", nrow(data_selected), "\n")

# Step 2: Remove duplicates based on student ID (STRATUM)
# If you want to keep only the first instance of each student (STRATUM)
data_selected_no_duplicates <- data_selected[!duplicated(data_selected$STRATUM), ]

# Step 3: Check the number of rows after removing duplicates
cat("Rows after removing duplicates:", nrow(data_selected_no_duplicates), "\n")

