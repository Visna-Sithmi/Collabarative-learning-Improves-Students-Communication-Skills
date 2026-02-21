data <- read.csv("C:\\Users\\ASUS\\Downloads\\CY07_MSU_STU_QQQ.csv")
data <- as.data.frame(data)
str(data)
summary(data)


collaborative_vars <- c("ST100Q03TA",
                        "ST100Q04TA",
                        "ST102Q02TA",
                        "ST100Q02TA",
                        "ST104Q04NA",
                        "ST104Q02NA",
                        "ST206Q02HA",
                        "ST206Q04HA",
                        "ST176Q06IA")

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


data_selected <- data[, c(collaborative_vars, communication_vars)]
names(data_selected)
names(data)
summary(data_selected)

#save data
df <- data_selected
dim(df)


#Detect missing values

# How many NA per column
na_count <- colSums(is.na(data_selected))
na_count

# NA percentage per column
na_pct <- round(na_count / nrow(data_selected) * 100, 2)
na_pct

# see how many duplicates
sum(duplicated(df)) 
df <- df[!duplicated(df), ]
dim(df)

#Make Sure All Variables Are Numeric
df[] <- lapply(df, function(x) as.numeric(x))

#Split dataset into 2 parts (IV group and DV group)
collab_df <- df[, collaborative_vars]
comm_df   <- df[, communication_vars]


min_collab <- 5
min_comm   <- 6

#Count how many answers each student gave
collab_answered <- rowSums(!is.na(collab_df))
comm_answered   <- rowSums(!is.na(comm_df))

#Decide which students to keep
keep_rows <- (collab_answered >= min_collab) &
  (comm_answered >= min_comm)

df_clean <- df[keep_rows, ]

table(collab_answered)
table(comm_answered)

df_clean$collaborative_score <- rowMeans(df_clean[, collaborative_vars], na.rm = TRUE)
df_clean$communication_score <- rowMeans(df_clean[, communication_vars], na.rm = TRUE)

table(collab_answered)
table(comm_answered)


keep_rows <- (collab_answered >= 5) &
  (comm_answered >= 6)

df_clean <- df[keep_rows, ]

dim(df_clean)


