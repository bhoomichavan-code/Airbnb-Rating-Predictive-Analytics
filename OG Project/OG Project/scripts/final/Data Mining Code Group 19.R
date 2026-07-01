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

# Logistic Regression Model -----------------------------------------------
set.seed(45)
trainIndex <- createDataPartition(train$perfect_rating_score, p = 0.7, list = FALSE)
train_data_log <- train[trainIndex, ]
test_data_log <- train[-trainIndex, ]
selected_features_log <- c("host_response_rate","host_listings_count",
                           "host_total_listings_count","state","room_type",
                           "accommodates","bathrooms","bedrooms","beds",
                           "square_feet","price","weekly_price","monthly_price",
                           "security_deposit","cleaning_fee","guests_included",
                           "extra_people","minimum_nights","maximum_nights",
                           "availability_365","cancellation_policy","price_per_person",
                           "has_cleaning_fee","bed_category","months_since_host_listed",
                           "average_months_since_host_listed","property_category",
                           "charges_for_extra","host_response","has_min_nights")

model_log <- glm(perfect_rating_score ~ ., data = train_data_log[, c(selected_features_log, "perfect_rating_score")], family = "binomial")

# Model Evaluation
predictions_log <- predict(model_log, newdata = test_data_log[, c(selected_features_log, "perfect_rating_score")], type = "response")
predicted_class_log <- ifelse(is.na(predictions_log), "NO", ifelse(predictions_log > 0.435, "YES", "NO"))
confusion_matrix_log <- table(Predicted = predicted_class_log, Actual = test_data_log$perfect_rating_score)

# Calculate TPR and FPR
TPR_log <- confusion_matrix_log["YES", "YES"] / sum(confusion_matrix_log[ , "YES"])
FPR_log <- confusion_matrix_log["YES", "NO"] / sum(confusion_matrix_log[ , "NO"])
ACC_log <- sum(confusion_matrix_log["NO","NO"],confusion_matrix_log["YES","YES"]) / 
  sum(confusion_matrix_log[ , ])
print(paste("True Positive Rate (TPR) for logistic regression:", TPR_log))
print(paste("False Positive Rate (FPR) for logistic regression:", FPR_log))
print(paste("Accuracy for logistic regression:", ACC_log))

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


# Select relevant features for Lasso and Ridge modeling -----------------------------------
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

# Split data for Lasso and Ridge Models -----------------------------------
train_LR_Selected <- train[, selected_features]
train_LR_dummy <- dummyVars( ~ . , data=train_LR_Selected, fullRank = TRUE)
one_hot_LR <- predict(train_LR_dummy, newdata =train_LR_Selected)
one_hot_LR <- one_hot_LR %>% clean_names()

train_LR_perfect <- train$perfect_rating_score

set.seed(40)
train_99 <- sample(nrow(train_LR_Selected),.7*nrow(train_LR_Selected))
train_x_99 <- one_hot_LR[train_99,]
train_y_99 <- train_LR_perfect[train_99]

test_x_99 <- one_hot_LR[-train_99,]
test_y_99 <- train_LR_perfect[-train_99]

# Lasso Model -----------------------------------------------
lambda_seq <- 10^seq(-7, 7, length.out = 100)

lasso_cv_valid <- cv.glmnet(train_x_99, train_y_99, family = "binomial",
                            alpha = 1, lambda = lambda_seq, nfolds = 5)

optimal_lambda_lasso <- lasso_cv_valid$lambda.min

final_lasso_model <- glmnet(train_x_99, train_y_99, family = "binomial", 
                            alpha = 1, lambda = optimal_lambda_lasso)

predicted_probs_lasso <- predict(final_lasso_model, newx = test_x_99, type = "response")

binary_predictions_lasso <- ifelse(predicted_probs_lasso >= 0.45, "YES", "NO")

confusion_matrix_lasso <- table(test_y_99, binary_predictions_lasso)

TPR_las <- confusion_matrix_lasso["YES", "YES"] / sum(confusion_matrix_lasso["YES",])
FPR_las <- confusion_matrix_lasso["NO","YES"] / sum(confusion_matrix_lasso["NO",])
ACC_las <- sum(confusion_matrix_lasso["YES", "YES"],confusion_matrix_lasso["NO", "NO"]) /
  sum(confusion_matrix_lasso[ , ])
print(paste("True Positive Rate (TPR) for Lasso model:", TPR_las))
print(paste("False Positive Rate (FPR) for Lasso Model:", FPR_las))
print(paste("Accuracy for Lasso Model:", ACC_las))

# Select best features from Lasso -----------------------------------------
lasso_features <- coef(lasso_cv_valid, s=optimal_lambda_lasso)[-1,]
lasso_features <- which(lasso_features!=0)
selected_lasso_features <- row.names(data.frame(lasso_features))

# Ridge Model -------------------------------------------------------------
lambda_seq <- 10^seq(-7, 7, length.out = 100)

ridge_cv_valid <- cv.glmnet(train_x_99, train_y_99, family = "binomial",
                            alpha = 0, lambda = lambda_seq, nfolds = 5)

optimal_lambda_ridge <- ridge_cv_valid$lambda.min

final_ridge_model <- glmnet(train_x_99, train_y_99, family = "binomial", 
                            alpha = 0, lambda = optimal_lambda_ridge)

predicted_probs_ridge <- predict(final_ridge_model, newx = test_x_99, type = "response")

binary_predictions_ridge <- ifelse(predicted_probs_ridge >= 0.45, "YES", "NO")

confusion_matrix_ridge <- table(test_y_99, binary_predictions_ridge)

TPR_rid <- confusion_matrix_ridge["YES", "YES"] / sum(confusion_matrix_ridge["YES",])
FPR_rid <- confusion_matrix_ridge["NO","YES"] / sum(confusion_matrix_ridge["NO",])
ACC_rid <- sum(confusion_matrix_ridge["YES", "YES"],confusion_matrix_ridge["NO", "NO"]) /
  sum(confusion_matrix_ridge[ , ])
print(paste("True Positive Rate (TPR) for Ridge model:", TPR_rid))
print(paste("False Positive Rate (FPR) for Ridge Model:", FPR_rid))
print(paste("Accuracy for Ridge Model:", ACC_rid))

# Split data into train and test sets for RF-------------------------------------
train_RF_selected <- train[,c(selected_features)]
train_RF_dummy <- dummyVars( ~ . , data=train_RF_selected, fullRank = TRUE)
one_hot_RF <- predict(train_RF_dummy, newdata =train_RF_selected)
one_hot_RF <- one_hot_RF %>% clean_names()
one_hot_RF <- data.frame(one_hot_RF)

train_RF_perfect <- train$perfect_rating_score

set.seed(45)
train_RF <- sample(nrow(train_RF_selected),.7*nrow(train_RF_selected))
train_x_RF <- one_hot_RF[train_RF,]
train_y_RF <- train_RF_perfect[train_RF]
train_data_RF <- cbind(train_x_RF, train_y_RF)
names(train_data_RF)[ncol(train_data_RF)] <- "perfect_rating_score"

test_x_RF <- one_hot_RF[-train_RF,]
test_y_RF <- train_RF_perfect[-train_RF]
test_data_RF <- cbind(test_x_RF, test_y_RF)
names(test_data_RF)[ncol(test_data_RF)] <- "perfect_rating_score"

# RF Model to check TPR FPR -----------------------------------------------

rf_model <- randomForest(perfect_rating_score ~ ., data = train_data_RF[, c(selected_lasso_features, "perfect_rating_score")])

# Make predictions
predicted_prob_RF <- predict(rf_model, newdata = test_data_RF, type = "prob")[, "YES"]
binary_predictions_RF <- ifelse(predicted_prob_RF >= 0.452, "YES", "NO")
confusion_matrix_RF <- table(Predicted = binary_predictions_RF, Actual = test_data_RF$perfect_rating_score)

#print(confusion_matrix_rf)
TPR_RF <- confusion_matrix_RF["YES", "YES"] / sum(confusion_matrix_RF[ , "YES"])
FPR_RF <- confusion_matrix_RF["YES", "NO"] / sum(confusion_matrix_RF[ , "NO"])
Acc_RF <- sum(confusion_matrix_RF["YES", "YES"],confusion_matrix_RF["NO", "NO"]) /
  sum(confusion_matrix_RF[ , ])
print(paste("True Positive Rate (TPR) for Random Forest:", TPR_RF))
print(paste("False Positive Rate (FPR) for Random Forest:", FPR_RF))
print(paste("Accuracy of Random Forest:", Acc_RF))

# XgBoost -----------------------------------------------------------------
train_XG_selected <- train
train_XG_selected <- train_XG_selected[, c(selected_features)]
train_XG_dummy <- dummyVars( ~ . , data=train_XG_selected, fullRank = TRUE)
one_hot_XG <- predict(train_XG_dummy, newdata =train_XG_selected)
one_hot_XG <- one_hot_XG %>% clean_names() 

train_XG_perfect <- train$perfect_rating_score

set.seed(45)
train_XG <- sample(nrow(train_XG_selected),.7*nrow(train_XG_selected))
train_x_XG <- one_hot_XG[train_XG,]
train_y_XG <- train_XG_perfect[train_XG]
train_y_XG <- ifelse(train_y_XG == "YES", 1, 0)
train_x_XG <- train_x_XG[, selected_lasso_features]

test_x_XG <- one_hot_XG[-train_XG,]
test_y_XG <- train_XG_perfect[-train_XG]
test_y_XG <- ifelse(test_y_XG == "YES", 1, 0)
test_x_XG <- test_x_XG[, selected_lasso_features]

bst <- xgboost(data = train_x_XG,
               label = train_y_XG,
               max.depth = 10,
               eta = 0.01,
               nrounds = 1500,
               alpha = 1,
               gamma=3,
               objective = "binary:logistic")
pred_bst <- predict(bst, newdata = test_x_XG)
preds_bst <- ifelse(pred_bst > 0.49, 1, 0)
confusion_matrix_XG <- table(Predicted = preds_bst, Actual = test_y_XG)
TPR_XG <- confusion_matrix_XG[2, 2] / sum(confusion_matrix_XG[ , 2])
FPR_XG <- confusion_matrix_XG[2, 1] / sum(confusion_matrix_XG[ , 1])
ACC_XG <- sum(confusion_matrix_XG[2,2], confusion_matrix_XG[1,1])/sum(confusion_matrix_XG[,])
print(paste("True Positive Rate (TPR) for XG Boost:", TPR_XG))
print(paste("False Positive Rate (FPR) for XG Boost:", FPR_XG))
print(paste("Accuracy for XG Boost:", ACC_XG))

# GBM ----------------------------------------------------------------------------------

gbm_model <- gbm(
  formula = train_y_XG ~ .,
  distribution = "bernoulli",
  data = as.data.frame(train_x_XG),
  n.trees = 1000, # Number of trees
  interaction.depth = 5, # Maximum depth of variable interactions
  shrinkage = 0.01, # Learning rate
  bag.fraction = 0.5, # Proportion of observations used for each tree
  train.fraction = 1, # Fraction of data used for training each tree
  n.minobsinnode = 10, # Minimum number of observations in terminal nodes
  verbose = FALSE
)

# Predictions
pred_gbm <- predict(gbm_model, newdata = as.data.frame(test_x_XG), n.trees = 1000, type = "response")
preds_gbm <- ifelse(pred_gbm > 0.45, 1, 0)

# Confusion Matrix, Accuracy
confusion_matrix_gbm <- table(Predicted = preds_gbm, Actual = test_y_XG)
TPR_gbm <- confusion_matrix_gbm[2, 2] / sum(confusion_matrix_gbm[, 2])
FPR_gbm <- confusion_matrix_gbm[2, 1] / sum(confusion_matrix_gbm[, 1])
ACC_gbm <- sum(diag(confusion_matrix_gbm)) / sum(confusion_matrix_gbm)
print(paste("True Positive Rate (TPR) for GBM:", TPR_gbm))
print(paste("False Positive Rate (FPR) for GBM:", FPR_gbm))
print(paste("Accuracy for GBM:", ACC_gbm))

# Train the Random Forest model and export CSV ----------------------------
rf_model1 <- randomForest(perfect_rating_score ~ ., data = train[, c(selected_features, "perfect_rating_score")])
# Make predictions
predicted_prob_yes1 <- predict(rf_model1, newdata = test_x, type = "prob")[, "YES"]
binary_predictions1 <- ifelse(predicted_prob_yes1 >= 0.45, "YES", "NO")

#CSV
write.table(binary_predictions, "perfect_rating_score_group19.csv", row.names = FALSE)
