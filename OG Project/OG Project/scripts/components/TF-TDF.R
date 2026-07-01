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




# TD-IDF ------------------------------------------------------------------


