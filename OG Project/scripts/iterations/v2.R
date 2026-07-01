# Load necessary libraries
library(tidyverse)
library(caret)
library(lubridate)
library(glmnet)

setwd('C:/Users/Bhoomi/Desktop/Data Mining and Predictive Analytics/Project/')
train_x <- read_csv("airbnb_train_x_2024.csv")
train_y <- read_csv("airbnb_train_y_2024.csv")


train <- cbind(train_x, train_y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score))


# Data Cleaning and Feature Engineering
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
    #years_since_host_listed = as.numeric(interval(host_since, as.Date("2024-04-30")) / years(1)),
    average_listings_per_month = ifelse(is.na(months_since_host_listed) | months_since_host_listed == 0, 0, host_total_listings_count / months_since_host_listed)
  )
train <- train %>%
  mutate(property_category = case_when(
           property_type %in% c("Apartment", "Serviced apartment", "Loft") ~ "apartment",
           property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel") ~ "hotel",
           property_type %in% c("Townhouse", "Condominium") ~ "condo",
           property_type %in% c("Bungalow", "House") ~ "house",
           TRUE ~ "other"),
         property_category = as.factor(property_category))

train <- select(train, -experiences_offered)

names(train)
str(train)
# Get data types of all variables
data_types <- sapply(train, class)
# Print data types and identify categories
data_types
str(train)

# Select relevant features for modeling
selected_features <- c("accommodates", "bedrooms", "bathrooms", "price_per_person",
                       "has_cleaning_fee", "bed_category", "room_type", "state",
                       "Free_parking_flag", "Host_Is_Superhost_flag", "months_since_host_listed",
                       "average_listings_per_month")


# 70/30 Split
set.seed(45)
trainIndex <- createDataPartition(train$perfect_rating_score, p = 0.89, list = FALSE)
train_data <- train[trainIndex, ]
test_data <- train[-trainIndex, ]

# Subset test_data to contain only 10,000 rows
test_data <- test_data[1:10000, ]


# Model Training
model <- glm(perfect_rating_score ~ ., data = train_data[, c(selected_features, "perfect_rating_score")], family = "binomial")

# Model Evaluation
predictions <- predict(model, newdata = test_data[, c(selected_features, "perfect_rating_score")], type = "response")
predicted_class <- ifelse(is.na(predictions), "NO", ifelse(predictions > 0.40, "YES", "NO"))
confusion_matrix <- table(Predicted = predicted_class, Actual = test_data$perfect_rating_score)
print(confusion_matrix)

sum(confusion_matrix[ , "YES"])
confusion_matrix["NO", "YES"]
confusion_matrix
# Calculate TPR and FPR
TPR <- confusion_matrix["YES", "YES"] / sum(confusion_matrix[ , "YES"])
FPR <- confusion_matrix["YES", "NO"] / sum(confusion_matrix[ , "NO"])
print(paste("True Positive Rate (TPR):", TPR))
print(paste("False Positive Rate (FPR):", FPR))

#write.table(predicted_class, "perfect_rating_score_group19.csv", row.names = FALSE)




# Random Forest Code:
install.packages("xgboost")
install.packages("randomForest")
install.packages("ROCR")
library(tidyverse)
library(caret)
library(lubridate)
library(glmnet)
library(xgboost)
library(randomForest)
library(dplyr)
library(ROCR)

setwd('C:/Users/Bhoomi/Desktop/Data Mining and Predictive Analytics/Project/')
train_x <- read_csv("airbnb_train_x_2024.csv")
train_y <- read_csv("airbnb_train_y_2024.csv")


train <- cbind(train_x, train_y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score))

# Data Cleaning and Feature Engineering
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
    months_since_host_listed = as.numeric(interval(host_since, Sys.Date()) / months(1)),
    months_since_host_listed = ifelse(is.na(months_since_host_listed), mean(months_since_host_listed, na.rm = TRUE), months_since_host_listed),
    average_listings_per_month = ifelse(is.na(months_since_host_listed) | months_since_host_listed == 0, 0, host_total_listings_count / months_since_host_listed)
  )

# Select relevant features for modeling
selected_features <- c("accommodates", "bedrooms", "bathrooms", "price_per_person",
                       "has_cleaning_fee", "bed_category", "room_type", "state",
                       "Free_parking_flag", "Host_Is_Superhost_flag", "months_since_host_listed",
                       "average_listings_per_month")



# 70/30 Split
set.seed(45)
trainIndex <- createDataPartition(train$perfect_rating_score, p = 0.7, list = FALSE)
train_data <- train[trainIndex, ]
test_data <- train[-trainIndex, ]

colnames(train_data)

# Subset test_data to contain only 10,000 rows
test_data <- test_data[1:10000, ]

#Random Forest

# Train the Random Forest model
rf_model <- randomForest(perfect_rating_score ~ ., data = train_data[, c(selected_features, "perfect_rating_score")])

# Predict on test data
predicted_prob_yes <- predict(rf_model, newdata = test_data, type = "prob")[, "YES"]
binary_predictions <- ifelse(predicted_prob_yes >= 0.45, "YES", "NO")

# Evaluate the model (for example, using confusion matrix for classification)
confusion_matrix_rf <- table(binary_predictions, test_data$perfect_rating_score)
print(confusion_matrix_rf)

TPR <- confusion_matrix_rf["YES", "YES"] / sum(confusion_matrix_rf[ , "YES"])
FPR <- confusion_matrix_rf["YES", "NO"] / sum(confusion_matrix_rf[ , "NO"])
print(paste("True Positive Rate (TPR):", TPR))
print(paste("False Positive Rate (FPR):", FPR))

write.table(binary_predictions, "perfect_rating_score_group19.csv", row.names = FALSE)
