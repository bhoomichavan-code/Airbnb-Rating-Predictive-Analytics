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
    state = toupper(state),
    state = as.factor(state)
  ) %>%
  mutate(
    host_since = as.Date(host_since, format = "%Y-%m-%d"),
    months_since_host_listed = as.numeric(interval(host_since, as.Date("2024-04-30")) / months(1)),
    average_months_since_host_listed = mean(months_since_host_listed, na.rm = TRUE),
    months_since_host_listed = ifelse(is.na(months_since_host_listed), 
                                      average_months_since_host_listed, 
                                      months_since_host_listed),
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
      cancellation_policy %in% c("strict", "super_strict_30", "super_strict_60","no_refunds") ~ "strict",
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
round_to_zero <- c("host_response_rate", "host_listings_count", 
                   "host_total_listings_count", "accommodates","bedrooms","beds",
                   "cleaning_fee","guests_included","extra_people",
                   "minimum_nights","maximum_nights","availability_30",
                   "availability_60","availability_90","availability_365",
                   "months_since_host_listed","average_months_since_host_listed")
round_to_one <- c("bathrooms")
round_to_two <- c("price","square_feet","weekly_price",
                  "monthly_price","security_deposit","price_per_person")

# Round columns to 0 decimals using mutate (corrected)
train <- train %>%
  mutate(across(all_of(round_to_zero), ~ round(.x, digits = 0)),
         across(all_of(round_to_one), ~ round(.x, digits = 1)),
         across(all_of(round_to_two), ~ round(.x, digits = 2)))


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
    state = toupper(state),
    state = as.factor(state)
  ) %>%
  mutate(
    host_since = as.Date(host_since, format = "%Y-%m-%d"),
    months_since_host_listed = as.numeric(interval(host_since, as.Date("2024-04-30")) / months(1)),
    average_months_since_host_listed = mean(months_since_host_listed, na.rm = TRUE),
    months_since_host_listed = ifelse(is.na(months_since_host_listed), 
                                      average_months_since_host_listed, 
                                      months_since_host_listed),
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
      cancellation_policy %in% c("strict", "super_strict_30", "super_strict_60","no_refunds") ~ "strict",
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
  )

test_x <- test_x %>%
  mutate(across(all_of(round_to_zero), ~ round(.x, digits = 0)),
         across(all_of(round_to_one), ~ round(.x, digits = 1)),
         across(all_of(round_to_two), ~ round(.x, digits = 2)))

# Text Mining for Train -------------------------------------------------------------
text_df <- select(train, amenities, host_verifications, features) %>%
  mutate(id = row_number()) %>%
  mutate(text = paste(amenities, host_verifications, features, sep = ","))  # Combine text

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

# Text Mining tor Test  ---------------------------------------------------

#Text Mining Code for Test
text_df <- select(test_x, amenities, host_verifications, features) %>%
  mutate(id = row_number()) %>%
  mutate(text = paste(amenities, host_verifications, features, sep = ","))  # Combine text

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
#dim(dtm_test)

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

train <- cbind(train, dtm_df[, common_tm_cols])

# code to join dtm_df_test to text_x --------------------------------------

test_x <- cbind(test_x, dtm_df_test[, common_tm_cols])


# Select relevant features for modeling -----------------------------------

selected_features <- c("host_response_rate","host_listings_count",
                       "host_total_listings_count","state","room_type",
                       "accommodates","bathrooms","bedrooms","beds",
                       "square_feet","price","weekly_price","monthly_price",
                       "security_deposit","cleaning_fee","guests_included",
                       "extra_people","minimum_nights","maximum_nights",
                       "availability_30","availability_60","availability_90",
                       "availability_365","cancellation_policy","price_per_person",
                       "has_cleaning_fee","bed_category","months_since_host_listed",
                       "average_months_since_host_listed","property_category",
                       "charges_for_extra","host_response","has_min_nights")


selected_features <- c(selected_features, common_tm_cols)


# Lasso Model on Split Data -----------------------------------------------
train_lasso_selected <- train[, selected_features]
split_train_dummy <- dummyVars( ~ . , data=train_lasso_selected, fullRank = TRUE)
one_hot <- predict(split_train_dummy, newdata =train_lasso_selected)

train_lasso_perfect <- train$perfect_rating_score

# set seed, load data, and do train/validation split
set.seed(50)
train_99 <- sample(nrow(train_lasso_selected),.7*nrow(train_lasso_selected))
train_x_99 <- one_hot[train_99,]
train_y_99 <- train_lasso_perfect[train_99]

test_x_99 <- one_hot[-train_99,]
test_y_99 <- train_lasso_perfect[-train_99]

lambda_seq <- 10^seq(-7, 7, length.out = 100)

lasso_cv_valid <- cv.glmnet(train_x_99, train_y_99, family = "binomial",
                            alpha = 1, lambda = lambda_seq, nfolds = 10)

plot(lasso_cv_valid)


optimal_lambda_lasso <- lasso_cv_valid$lambda.min

lasso_features <- coef(lasso_cv_valid, s=optimal_lambda_lasso)[-1,]
lasso_features <- which(lasso_features!=0)

final_lasso_model <- glmnet(train_x_99, train_y_99, family = "binomial", 
                            alpha = 1, lambda = optimal_lambda_lasso)

#coef(glmnet(train_x_99, train_y_99, family = "binomial", alpha = 1, lambda = optimal_lambda_lasso))

predicted_probs <- predict(final_lasso_model, newx = test_x_99, type = "response")

binary_predictions <- ifelse(predicted_probs >= 0.45, "YES", "NO")

confusion_matrix_lasso <- table(test_y_99, binary_predictions)

TPR <- confusion_matrix_lasso["YES", "YES"] / sum(confusion_matrix_lasso["YES",])
FPR <- confusion_matrix_lasso["NO","YES"] / sum(confusion_matrix_lasso["NO",])
print(paste("True Positive Rate (TPR):", TPR))
print(paste("False Positive Rate (FPR):", FPR))

# Lasso Model -------------------------------------------------------------

lasso_train_x <- train[, selected_features]
lasso_train_y <- train$perfect_rating_score
lasso_test <- test_x[, selected_features]

# glmnet requires a matrix of dummy variables rather than factors
dummy_train <- dummyVars( ~ . , data=lasso_train_x, fullRank = TRUE)
dummy_test <- dummyVars( ~ . , data=lasso_test, fullRank = TRUE)
one_hot_train <- predict(dummy_train, newdata =lasso_train_x)
one_hot_test <- predict(dummy_test, newdata =lasso_test)

#names_one_hot_train <- names(data.frame(one_hot_train))
#names_one_hot_test <- names(data.frame(one_hot_test))

#com <- intersect(names_one_hot_train, names_one_hot_test)


lambda_seq <- 10^seq(-7, 7, length.out = 100)

lasso_cv <- cv.glmnet(one_hot_train, lasso_train_y, family = "binomial", alpha = 1, lambda = lambda_seq, nfolds = 5)

plot(lasso_cv)

optimal_lambda_lasso <- lasso_cv$lambda.min
#cat("Optimal lambda for Lasso:", optimal_lambda_lasso, "\n")

lasso_model <- glmnet(one_hot_train, lasso_train_y, family = "binomial", alpha = 1, lambda = optimal_lambda_lasso)

preds_lasso <- predict(lasso_model, newx = one_hot_test, type = "response")
binary_preds_lasso <- ifelse(preds_lasso >= 0.44, "YES", "NO")










# extra code (DONT EXECUTE) --------------------------------------------------------------




# Loop through columns and print data types
for (col in selected_features) {
  # Get data type using typeof
  data_type <- str(lasso_train_x[, col])
  
  # Print column name and data type
  cat(paste(col, ":", data_type), sep = " - ")
  cat("\n")  # Add newline for each column
}






library(writexl)
#write_xlsx(train, "train.xlsx")
write_xlsx(test_x, "test.xlsx")
