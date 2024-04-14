library(tidyverse)
library(dplyr)


#PREPROCESSING TRANSACTION

dir_mit <- "C:\\Users\\glauc\\Desktop\\data\\transactions_mit"
setwd(dir_mit)
list.files()

# List the files in the directory
files <- list.files(pattern = "\\.csv$")

# Filter only the CSV files
csv_files <- files[str_detect(files, "\\.csv$")]

# Read and combine the CSV files into a single dataframe
transaction <- lapply(csv_files, read.csv) %>% 
  bind_rows()

# View the combined dataframe
View(transaction)


#PREPROCESSING TRANSACTION KVN

dir_kvn <- "C:/Users/glauc/Desktop/data/transactions_kvn"
setwd(dir_kvn)
transaction_kvn <- NULL
folders <- list.files()
for (folder in folders) {
  setwd(file.path(dir_kvn, folder))
  files <- list.files()
  csv_file <- files[str_detect(files, "\\.csv$")]
  temp_kvn <- read.csv(csv_file)
  if (is.null(transaction_kvn)) {
    transaction_kvn <- temp_kvn
  } else {
    transaction_kvn <- rbind(transaction_kvn, temp_kvn)
  }
}



#PREPROCESSING TRANSACTION SD

dir_sd <- "C:/Users/glauc/Desktop/data/transactions_sd"
setwd(dir_sd)
transaction_sd <- NULL
folders <- list.files()
for (folder in folders) {
  setwd(file.path(dir_sd, folder))
  files <- list.files()
  csv_file <- files[str_detect(files, "\\.csv$")]
  temp_sd <- read.csv(csv_file)
  if (is.null(transaction_sd)) {
    transaction_sd <- temp_sd
  } else {
    transaction_sd <- rbind(transaction_sd, temp_sd)
  }
}







#PREPROCESSING TRANSACTION

dir <- "C:\\Users\\glauc\\Desktop\\data\\dimensions"
setwd(dir)

# List the files in the directory
files <- list.files(pattern = "\\.csv$")

customer <- read.csv("customer_mit.csv")
product <- read.csv("product_mit.csv")
store <- read.csv("store_mit.csv")

View(customer)
View(product)
View(store)


# Group by customer_key and calculate the total amount for each customer
total_amount_per_customer <- transaction %>%
  group_by(customer_key) %>%
  summarise(total_amount = sum(amount))

# Sort the total amounts in decreasing order
total_amount_per_customer <- total_amount_per_customer %>%
  arrange(desc(total_amount))

# View the total amounts in decreasing orde
View(total_amount_per_customer)
quantile(total_amount_per_customer$total_amount,0.97)

#gender, age, cat_g1_code

vip <- data.frame(vip = ifelse(total_amount_per_customer$total_amount > 393, 1, 0),
                   customer_key = total_amount_per_customer$customer_key)
head(vip)


vip <- vip[complete.cases(vip), ]

head(customer)

# Merge the datasets
vip_all <- merge(vip, customer, by = "customer_key")

# Display the merged data
View(vip_all)

head(vip_all$birth_dt)
vip_all <- vip_all[vip_all$birth_dt != "", ]
vip_all$genz <- ifelse(year(vip_all$birth_dt) >= 1997 & year(vip_all$birth_dt) <= 2012, 1, 0)
head(vip_all)

# Check for missing values
sum(is.na(vip_all$birth_dt))

logistic_model <- glm(vip ~ gender+genz, data = vip_all, family = binomial)

summary(logistic_model)


#How many genZ? 11%
customer <- customer[customer$birth_dt != "", ]
customer$genz <- ifelse(year(customer$birth_dt) >= 1997 & year(customer$birth_dt) <= 2012, 1, 0)
mean(customer$genz)

#Loyalty program genZ
View(customer)

regression_model <- lm(points_earned ~ genz, data = customer)


















































































































































# Initialize an empty list to store dataframes
dataframes_list <- list()

# Iterate over each file
for (file in files) {
  # Read the CSV file
  data <- read.csv(file)
  
  # Add the dataframe to the list
  dataframes_list[[file]] <- data
}

# Concatenate all dataframes vertically
final_dataframe <- do.call(rbind, dataframes_list)

# Print the final dataframe
print(final_dataframe)

# Loop through each file
for (file in files) {
  # Read the CSV file
  setwd(file)
  df <- read.csv(file_path)
  
  # Combine dataframes
  combined_df <- rbind(combined_df, df)
}

# Print the combined dataframe
print(combined_df)

# List all files in the directory
for (file in files) {
  print(file)
  cur_files <- list.files(file, full.names = TRUE)
  data_frames <- list()
  # Loop through each file
  for (cfile in cur_files) {
    # Check if the file is a CSV
    if (grepl("\\.csv$", cfile)) {
      # Extract the folder name
      folder_name <- basename(dirname(cfile))
      
      # Read the CSV file into a data frame
      df <- read.csv(cfile)
      
      # Store the data frame in the list with the folder name as the key
      data_frames[[folder_name]] <- df
    }
  }
  # Create DataFrames from each CSV file
  dfs <- list()
  for (file in csv_files) {
    df_name <- tools::file_path_sans_ext(file)
    dfs[[df_name]] <- read.csv(file)
  }
  
  # Access each DataFrame individually
  for (df_name in names(dfs)) {
    assign(paste(df_name, "df", sep = "_"), dfs[[df_name]])
  }
}


View(customer_mit_df) #birth_dt, join_dt, gender
View(transaction_mit_30_df) #customer_key, #product_key
View(product_mit_df) #cat_g1_description
View(store_mit_df) #city, start_date


#MERGE
library(lubridate)

merged_df <- merge(merge(customer_mit_df, transaction_mit_30_df, by = "customer_key"), product_mit_df, by = "product_key")
merged_df <- merged_df[, c("cat_g1_description", "gender", "birth_dt")]
merged_df$birth_dt <- as.Date(merged_df$birth_dt)
# Trasformazione della variabile birth_dt
merged_df$birth_dt_transformed <- ifelse(year(merged_df$birth_dt) >= 1997 & year(merged_df$birth_dt) <= 2012, 1, 0)

# Rimuoviamo la colonna originale birth_dt
merged_df <- subset(merged_df, select = -c(birth_dt))
merged_df <- na.omit(merged_df)

View(merged_df)

# Convert cat_g1_description to factor
merged_df$cat_g1_description <- as.factor(merged_df$cat_g1_description)

# Specify "OTHERS" as the reference category
merged_df$cat_g1_description <- relevel(merged_df$cat_g1_description, ref = "OTHERS")

logistic_model <- glm(birth_dt_transformed ~ cat_g1_description + gender, data = merged_df, family = binomial)

summary(logistic_model)

