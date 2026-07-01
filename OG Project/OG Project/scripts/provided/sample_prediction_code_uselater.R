#load libraries
library(tidyverse)
library(dbplyr)

#load data files
setwd('C:/Users/Bhoomi/Desktop/Data Mining and Predictive Analytics/Project/')
train_x <- read_csv("airbnb_train_x_2024.csv")
train_y <- read_csv("airbnb_train_y_2024.csv")
test_x <- read_csv("airbnb_test_x_2024.csv")
predictions_test <- read.csv("perfect_rating_score_group19.csv")


#join the training y to the training x file
#also turn the target variables into factors
train <- cbind(train_x, train_y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score),
         high_booking_rate = as.factor(high_booking_rate))

clean_data <- train %>%
  mutate(cancellation_policy = ifelse(cancellation_policy %in% c("strict",
                                                                 "super_strict_30"),"strict",cancellation_policy),
         cleaning_fee = as.numeric(gsub("[^0-9.]", "", cleaning_fee)),
         price = as.numeric(gsub("[^0-9.]", "", price)),
         cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
         price = ifelse(is.na(price), 0, price),
         across(where(is.numeric) & !matches(c("cleaning_fee", "price")),
                ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

clean_data <- clean_data %>%
  mutate(bed_type= as.factor(bed_type),
         cancellation_policy = as.factor(cancellation_policy),
         room_type = as.factor(room_type))
clean_data <- clean_data %>%
  mutate(bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm = TRUE),
                            bathrooms),
         extra_people = as.numeric(gsub("[$,]", "", extra_people)),
         host_acceptance_rate = as.numeric(gsub("%", "", host_acceptance_rate)),
         host_response_rate = as.numeric(gsub("%", "", host_response_rate))
         )
clean_data <- clean_data %>%
  group_by(market) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(market = if_else(count < 300, "OTHER", market)) %>%
  select(-count) %>%
  mutate(market = as.factor(market))


# Select only the desired columns
clean_data <- clean_data %>%
  select(square_feet, availability_365, price, 
         availability_90, cleaning_fee, bathrooms, availability_60,
         security_deposit, guests_included, room_type, bed_type, perfect_rating_score,)


#clean_data <- clean_data[!is.na(clean_data$name), ]


#not_common_attributes <- setdiff(names(clean_data), names(train_x))
#not_common_attributes

#summary(train)

#transform text_x
clean_test_data <- test_x %>%
  mutate(cancellation_policy = ifelse(cancellation_policy %in% c("strict",
                                                                 "super_strict_30"),"strict",cancellation_policy),
         cleaning_fee = as.numeric(gsub("[^0-9.]", "", cleaning_fee)),
         price = as.numeric(gsub("[^0-9.]", "", price)),
         cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
         price = ifelse(is.na(price), 0, price),
         across(where(is.numeric) & !matches(c("cleaning_fee", "price")),
                ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

clean_test_data <- clean_test_data %>%
  mutate(bed_type= as.factor(bed_type),
         cancellation_policy = as.factor(cancellation_policy),
         room_type = as.factor(room_type))
clean_test_data <- clean_test_data %>%
  mutate(bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm = TRUE),
                            bathrooms),
         extra_people = as.numeric(gsub("[$,]", "", extra_people)),
         host_acceptance_rate = as.numeric(gsub("%", "", host_acceptance_rate)),
         host_response_rate = as.numeric(gsub("%", "", host_response_rate))
         )
clean_test_data <- clean_test_data %>%
  group_by(market) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(market = if_else(count < 300, "OTHER", market)) %>%
  select(-count) %>%
  mutate(market = as.factor(market))


# Select only the desired columns
clean_test_data <- clean_test_data %>%
  select(square_feet, availability_365, price, 
         availability_90, cleaning_fee, bathrooms, availability_60,
         security_deposit, guests_included, room_type, bed_type)



# EXAMPLE PREDICTIONS FOR CONTEST 1

#create a simple model to predict perfect_rating_score and generate predictions in the test data
#train_perfect <- clean_data %>%
#  select(-high_booking_rate)

logistic_perfect <- glm(perfect_rating_score~square_feet+availability_365+
                          price+availability_90+cleaning_fee+bathrooms+availability_60+
                          security_deposit+guests_included+room_type+bed_type, 
                        data = clean_data, family = "binomial")

summary(logistic_perfect)
probs_perfect <- predict(logistic_perfect, newdata = clean_test_data, type = "response")

#make binary classifications (make sure to check for NAs!)
classifications_perfect <- ifelse(probs_perfect > .29, "YES", "NO")
#classifications_perfect <- factor(classifications_perfect, levels = c("No","Yes"))

#summary(classifications_perfect)

confusion_matrix <- table(predictions_test$x, classifications_perfect)

assertthat::assert_that(sum(is.na(classifications_perfect))==0)
table(classifications_perfect)


#output your predictions
#they must be in EXACTLY this format
#a .csv file with the naming convention targetvariable_groupAAA.csv, where you replace targetvariable with your chosen target, and AAA with your group name
#in exactly the same order as they are in the test_x file

# For perfect_rating_score, each row should be a binary YES (is perfect) or NO (not perfect)
# For high_booking_rate, each row should be a number representing the likelihood of high_booking_rate = YES

#this code creates sample outputs in the correct format
write.table(classifications_perfect, "perfect_rating_score_group19.csv", row.names = FALSE)
write.table(probs_rate, "high_booking_rate_group0.csv", row.names = FALSE)

# I have evaluated these predictions against the test set
# the above perfect_rating_score predictions have TPR = 0.5244 and FPR = 0.4894 (so they would be disqualified!)
# the above high_booking_rate predictions have AUC = 0.525738