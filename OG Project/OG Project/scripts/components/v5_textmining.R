
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(tidyverse)
library(text2vec)
# used for linear models regularized or not 
library(glmnet)
# used for variable importance plots
library(vip)
#install.packages("xgboost")
#install.packages("randomForest")
#install.packages("ROCR")
library(tidyverse)
library(caret)
library(lubridate)
library(glmnet)
library(xgboost)
library(randomForest)
library(dplyr)
library(ROCR)
#install.packages("janitor")
library(janitor)

# text mining package: needed in case removing stop words in cleaning_tokenizer
library(tm)
# used for word stemming 
library(SnowballC)

# Load CSV and create Train DF --------------------------------------------

# Set working directory
setwd('C:/Users/Bhoomi/Desktop/Data Mining and Predictive Analytics/Project/')


# Read training and test data
train_x <- read_csv("airbnb_train_x_2024.csv")
train_y <- read_csv("airbnb_train_y_2024.csv")
test_x <- read_csv("airbnb_test_x_2024.csv")

# Combine features and target variable in training data
train <- cbind(train_x, train_y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score))

train <- train %>%
  select(-high_booking_rate)


# Data Cleaning Train DF-----------------------------------------------------------

train <- train %>%
  mutate(
    cleaning_fee = as.numeric(gsub("[^0-9.]", "", cleaning_fee)),
    price = as.numeric(gsub("[^0-9.]", "", price)),
    cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
    price = ifelse(is.na(price), 0, price),
    across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)),
    price_per_person = price / accommodates,
    has_cleaning_fee = as.factor(ifelse(cleaning_fee > 0, "YES", "NO")),
    bed_category = as.factor(ifelse(bed_type == "Real Bed", "bed", "other")),
    room_type = as.factor(room_type),
    state = as.factor(state),
    Free_parking_flag = as.factor(ifelse(grepl("Free parking", amenities), "YES", "NO")),
    Host_Is_Superhost_flag = as.factor(ifelse(grepl("Host Is Superhost", features), "YES", "NO"))
  ) %>%
  mutate(
    host_since = as.Date(host_since, format = "%Y-%m-%d"),
    months_since_host_listed = as.numeric(interval(host_since, as.Date("2024-04-30")) / months(1)),
    average_months_since_host_listed = mean(months_since_host_listed, na.rm = TRUE),
    months_since_host_listed = ifelse(is.na(months_since_host_listed), 
                                      average_months_since_host_listed, 
                                      months_since_host_listed),
    years_since_host_listed = as.numeric(interval(host_since, as.Date("2024-04-30")) / years(1)),
    average_listings_per_month = ifelse(is.na(months_since_host_listed) | months_since_host_listed == 0, 0, host_total_listings_count / months_since_host_listed)
  ) %>%
  mutate(
    property_category = case_when(
      property_type %in% c("Apartment", "Serviced apartment", "Loft") ~ "apartment",
      property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel") ~ "hotel",
      property_type %in% c("Townhouse", "Condominium") ~ "condo",
      property_type %in% c("Bungalow", "House") ~ "house",
      TRUE ~ "other"
    ),
    property_category = as.factor(property_category),
    cancellation_policy = case_when(
      cancellation_policy %in% c("strict", "super_strict_30", "super_strict_60") ~ "strict",
      TRUE ~ cancellation_policy
    ),
    cancellation_policy = as.factor(cancellation_policy),
    bathrooms = ifelse(is.na(bathrooms), mode(bathrooms, na.rm = TRUE), bathrooms),
    host_acceptance_rate = as.numeric(gsub("%", "", host_acceptance_rate)),
    host_response_rate = as.numeric(gsub("%", "", host_response_rate)),
    charges_for_extra = factor(ifelse(extra_people > 0, "YES", "NO"), levels = c("NO", "YES")),
    host_acceptance = factor(
      case_when(
        host_acceptance_rate >= 100 ~ "ALL",
        host_acceptance_rate < 100 & !is.na(host_acceptance_rate) ~ "SOME",
        is.na(host_acceptance_rate) ~ "MISSING"
      ),
      levels = c("ALL", "SOME", "MISSING")
    ),
    host_response = factor(
      case_when(
        host_response_rate >= 100 ~ "ALL",
        host_response_rate < 100 & !is.na(host_response_rate) ~ "SOME",
        is.na(host_response_rate) ~ "MISSING"
      ),
      levels = c("ALL", "SOME", "MISSING")
    ),
    has_min_nights = factor(ifelse(minimum_nights > 1, "YES", "NO"), levels = c("NO", "YES"))
  )
english_pattern <- "[a-zA-Z]+"
filtered_data <- train %>%
  filter(grepl(english_pattern, city)) %>%  # Filter for English city names
  mutate(city = toupper(city)) %>% # Convert city names to title case
  mutate(state = toupper(state))

train <- filtered_data


# Data Cleaning Test DF ---------------------------------------------------

test_x <- test_x %>%
  mutate(
    cleaning_fee = as.numeric(gsub("[^0-9.]", "", cleaning_fee)),
    price = as.numeric(gsub("[^0-9.]", "", price)),
    cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
    price = ifelse(is.na(price), 0, price),
    across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)),
    price_per_person = price / accommodates,
    has_cleaning_fee = as.factor(ifelse(cleaning_fee > 0, "YES", "NO")),
    bed_category = as.factor(ifelse(bed_type == "Real Bed", "bed", "other")),
    room_type = as.factor(room_type),
    state = as.factor(state),
    Free_parking_flag = as.factor(ifelse(grepl("Free parking", amenities), "YES", "NO")),
    Host_Is_Superhost_flag = as.factor(ifelse(grepl("Host Is Superhost", features), "YES", "NO"))
  ) %>%
  mutate(
    host_since = as.Date(host_since, format = "%Y-%m-%d"),
    months_since_host_listed = as.numeric(interval(host_since, as.Date("2024-04-30")) / months(1)),
    average_months_since_host_listed = mean(months_since_host_listed, na.rm = TRUE),
    months_since_host_listed = ifelse(is.na(months_since_host_listed), 
                                      average_months_since_host_listed, 
                                      months_since_host_listed),
    years_since_host_listed = as.numeric(interval(host_since, as.Date("2024-04-30")) / years(1)),
    average_listings_per_month = ifelse(is.na(months_since_host_listed) | months_since_host_listed == 0, 0, host_total_listings_count / months_since_host_listed)
  ) %>%
  mutate(
    property_category = case_when(
      property_type %in% c("Apartment", "Serviced apartment", "Loft") ~ "apartment",
      property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel") ~ "hotel",
      property_type %in% c("Townhouse", "Condominium") ~ "condo",
      property_type %in% c("Bungalow", "House") ~ "house",
      TRUE ~ "other"
    ),
    property_category = as.factor(property_category),
    cancellation_policy = case_when(
      cancellation_policy %in% c("strict", "super_strict_30", "super_strict_60") ~ "strict",
      TRUE ~ cancellation_policy
    ),
    cancellation_policy = as.factor(cancellation_policy),
    bathrooms = ifelse(is.na(bathrooms), mode(bathrooms, na.rm = TRUE), bathrooms),
    host_acceptance_rate = as.numeric(gsub("%", "", host_acceptance_rate)),
    host_response_rate = as.numeric(gsub("%", "", host_response_rate)),
    charges_for_extra = factor(ifelse(extra_people > 0, "YES", "NO"), levels = c("NO", "YES")),
    host_acceptance = factor(
      case_when(
        host_acceptance_rate >= 100 ~ "ALL",
        host_acceptance_rate < 100 & !is.na(host_acceptance_rate) ~ "SOME",
        is.na(host_acceptance_rate) ~ "MISSING"
      ),
      levels = c("ALL", "SOME", "MISSING")
    ),
    host_response = factor(
      case_when(
        host_response_rate >= 100 ~ "ALL",
        host_response_rate < 100 & !is.na(host_response_rate) ~ "SOME",
        is.na(host_response_rate) ~ "MISSING"
      ),
      levels = c("ALL", "SOME", "MISSING")
    ),
    has_min_nights = factor(ifelse(minimum_nights > 1, "YES", "NO"), levels = c("NO", "YES")),
    city = toupper(city),
    state = toupper(state)
  )



# Text Mining for Train -------------------------------------------------------------
text_df <- select(train, amenities, host_verifications) %>%
  mutate(id = row_number()) %>%
  mutate(text = paste(amenities, host_verifications, sep = ","))  # Combine text

cleaning_tokenizer <- function(v) {
  v %>%
    space_tokenizer(sep = ',') }

it_train <- itoken(text_df$text,
                   preprocessor = tolower,
                   tokenizer = cleaning_tokenizer,
                   ids = text_df$id,
                   progressbar = FALSE)

# Learn vocabulary from the combined tokenized data
vocab <- create_vocabulary(it_train)

#vectorize
vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)
dim(dtm_train)

#head(dtm_train)
#names(dtm_train)
#print(dtm_train[4, ])
#temp <- data.frame(dtm_train[4, ])
#print(temp)

# Assuming dtm_train is a sparse matrix
dtm_df <- data.frame(rownames(dtm_train))  # Create a data frame with row names

# Iterate through columns and add them as columns in the data frame
for (i in 1:ncol(dtm_train)) {
  dtm_df[, colnames(dtm_train)[i]] <- dtm_train[, i]
}

# Optionally, add document IDs if available
if (is.data.frame(it_train) & "id" %in% names(it_train)) {
  dtm_df$id <- it_train$id
}

#clean column names
dtm_df <- dtm_df %>% clean_names()

# convert all positive frequencies to 1
dtm_df[dtm_df > 0] <- 1


#for (col in colnames(dtm_df)) {
  # Print column name
#  print(paste(col, ":"))
  # Print unique values
#  print(unique(dtm_df[, col]))  } # Access column using selected_features

# Text Mining tor Test  ---------------------------------------------------

#Text Mining Code for Test
text_df <- select(test_x, amenities, host_verifications) %>%
  mutate(id = row_number()) %>%
  mutate(text = paste(amenities, host_verifications, sep = ","))  # Combine text

cleaning_tokenizer <- function(v) {
  v %>%
    space_tokenizer(sep = ',') 
}

it_test <- itoken(text_df$text,
                  preprocessor = tolower,
                  tokenizer = cleaning_tokenizer,
                  ids = text_df$id,
                  progressbar = FALSE)

# Learn vocabulary from the combined tokenized data
vocab <- create_vocabulary(it_test)

#vectorize
vectorizer <- vocab_vectorizer(vocab)
dtm_test <- create_dtm(it_test, vectorizer)
dim(dtm_test)

#head(dtm_train)
#names(dtm_train)
#print(dtm_train[4, ])
temp <- data.frame(dtm_test[4, ])
print(temp)


# Assuming dtm_train is a sparse matrix
dtm_df_test <- data.frame(rownames(dtm_test))  # Create a data frame with row names

# Iterate through columns and add them as columns in the data frame
for (i in 1:ncol(dtm_test)) {
  dtm_df_test[, colnames(dtm_test)[i]] <- dtm_test[, i]
}

# Optionally, add document IDs if available
if (is.data.frame(it_test) & "id" %in% names(it_test)) {
  dtm_df_test$id <- it_test$id
}

#clean column names
dtm_df_test <- dtm_df_test %>% clean_names()

# convert all positive frequencies to 1
dtm_df_test[dtm_df_test > 0] <- 1

# Get names of common text mining columns from train and test_x ------------------
tm_cols_train <- names(dtm_df)
tm_cols_test <- names(dtm_df_test)
common_tm_cols <- intersect(tm_cols_train, tm_cols_test)

# code to join dtm_df to train --------------------------------------------

#train <- cbind(train, dtm_df[, !(names(dtm_df) == "rownames_dtm_train")])
train <- cbind(train, dtm_df[, common_tm_cols])

# code to join dtm_df_test to text_x --------------------------------------

#test_x <- cbind(test_x, dtm_df_test[, !(names(dtm_df_test) == "rownames_dtm_test")])
test_x <- cbind(test_x, dtm_df_test[, common_tm_cols])


# Select relevant features for modeling -----------------------------------

selected_features <- c("accommodates", "bedrooms", "bathrooms", "price_per_person",
                       "has_cleaning_fee", "bed_category", "room_type",
                       "Free_parking_flag", "Host_Is_Superhost_flag", "months_since_host_listed",
                       "average_listings_per_month", "has_min_nights")
selected_features <- c(selected_features, common_tm_cols)

#selected_features <- cols_no_missing_train
#temp_vector <- c("perfect_rating_score", "host_verifications", "city")

#for (col in temp_vector) { selected_features <- setdiff(selected_features, col)}

#for (col in selected_features) {
  # Print column name
#  print(paste(col, ":"))
  # Print unique values
#  print(unique(train[, col]))  # Access column using selected_features }

# Split data into train and test sets -------------------------------------

set.seed(45)
trainIndex <- createDataPartition(train$perfect_rating_score, p = 0.7, list = FALSE)
train_data <- train[trainIndex, ]
test_data <- train[-trainIndex, ]


# RF Model and Predictions, Confusion Matrix ------------------------------
# Train the Random Forest model
rf_model <- randomForest(perfect_rating_score ~ ., data = train[, c(selected_features, "perfect_rating_score")])
#rf_model <- randomForest(perfect_rating_score ~ ., data = train)
# Make predictions
predicted_prob_yes <- predict(rf_model, newdata = test_x, type = "prob")[, "YES"]
binary_predictions <- ifelse(predicted_prob_yes >= 0.43, "YES", "NO")

#Evaluate
rf_model1 <- randomForest(perfect_rating_score ~ ., data = train_data[, c(selected_features, "perfect_rating_score")])
# Make predictions
predicted_prob_yes1 <- predict(rf_model1, newdata = test_data, type = "prob")[, "YES"]
binary_predictions1 <- ifelse(predicted_prob_yes1 >= 0.43, "YES", "NO")
confusion_matrix_rf <- table(binary_predictions1, test_data$perfect_rating_score)
#print(confusion_matrix_rf)
TPR <- confusion_matrix_rf["YES", "YES"] / sum(confusion_matrix_rf[ , "YES"])
FPR <- confusion_matrix_rf["YES", "NO"] / sum(confusion_matrix_rf[ , "NO"])
print(paste("True Positive Rate (TPR):", TPR))
print(paste("False Positive Rate (FPR):", FPR))


#CSV
write.table(binary_predictions, "perfect_rating_score_group19.csv", row.names = FALSE)



