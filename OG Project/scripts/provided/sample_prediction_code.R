#load libraries
library(tidyverse)
library(caret)

#load data files
setwd('C:/Users/Bhoomi/Desktop/Data Mining and Predictive Analytics/Project/')
 
train_x <- read_csv("airbnb_train_x_2024.csv")

train_y <- read_csv("airbnb_train_y_2024.csv")
#test_x <- read_csv("airbnb_test_x_2024.csv")

#join the training y to the training x file
#also turn the target variables into factors
train <- cbind(train_x, train_y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score),
         high_booking_rate = as.factor(high_booking_rate))

#airbnb data
airbnb <- read_csv("airbnb_hw2.csv")

clean_data <- airbnb %>%
  mutate(cancellation_policy = ifelse(cancellation_policy %in% c("strict",
                                                                 "super_strict_30"),"strict",cancellation_policy),
         cleaning_fee = as.numeric(gsub("[^0-9.]", "", cleaning_fee)),
         price = as.numeric(gsub("[^0-9.]", "", price)),
         cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
         price = ifelse(is.na(price), 0, price),
         across(where(is.numeric) & !matches(c("cleaning_fee", "price")),
                ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

clean_data <- clean_data %>%
  mutate(price_per_person = price/accommodates,
         has_cleaning_fee = ifelse(cleaning_fee ==0 | is.na(cleaning_fee) , "NO", "YES"),
         bed_category = ifelse(bed_type == "Real Bed", "bed", "other"),
         property_category = case_when(
           property_type %in% c("Apartment", "Serviced apartment", "Loft") ~ "apartment",
           property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel") ~ "hotel",
           property_type %in% c("Townhouse", "Condominium") ~ "condo",
           property_type %in% c("Bungalow", "House") ~ "house",
           TRUE ~ "other"),
         property_category = as.factor(property_category)) %>%
  group_by(property_category) %>%
  mutate(median_ppp = median(price_per_person, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ppp_ind = ifelse(price_per_person > median_ppp, 1, 0),
         ppp_ind = as.factor(ppp_ind))


clean_data <- clean_data %>%
  mutate(bed_type= as.factor(bed_type),
         cancellation_policy = as.factor(cancellation_policy),
         room_type = as.factor(room_type),
         property_catgory= as.factor(property_category),
         bed_category= as.factor(bed_category),
         ppp_ind= as.factor(ppp_ind))
clean_data <- clean_data %>%
  mutate(bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm = TRUE),
                            bathrooms),
         host_is_superhost = ifelse(is.na(host_is_superhost), FALSE,
                                    host_is_superhost),
         extra_people = as.numeric(gsub("[$,]", "", extra_people)),
         host_acceptance_rate = as.numeric(gsub("%", "", host_acceptance_rate)),
         host_response_rate = as.numeric(gsub("%", "", host_response_rate)),
         charges_for_extra = factor(ifelse(extra_people > 0, "YES", "NO"),
                                    levels = c("NO", "YES")),
         host_acceptance = factor(case_when(
           host_acceptance_rate >= 100 ~ "ALL",
           host_acceptance_rate < 100 &
             !is.na(host_acceptance_rate) ~ "SOME",
           is.na(host_acceptance_rate) ~ "MISSING"),
           levels = c("ALL", "SOME", "MISSING")),
         host_response = factor(case_when(
           host_response_rate >= 100 ~ "ALL",
           host_response_rate < 100 &
             !is.na(host_response_rate) ~ "SOME",
           is.na(host_response_rate) ~ "MISSING"),
           levels = c("ALL", "SOME", "MISSING")),
         has_min_nights = factor(if_else(minimum_nights > 1, "YES", "NO"),
                                 levels = c("NO", "YES")),
         high_booking_rate= as.factor(high_booking_rate))
clean_data <- clean_data %>%
  group_by(market) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(market = if_else(count < 300, "OTHER", market)) %>%
  select(-count) %>%
  mutate(market = as.factor(market))
summary(clean_data)

common_attributes <- intersect(names(clean_data), names(train))
common_attributes

clean_data_from_train <- train
clean_data_from_train <- clean_data_from_train %>%
  select(all_of(common_attributes), perfect_rating_score)



clean_data <- clean_data %>%
  left_join(clean_data_from_train %>% 
              select(name,accommodates,bed_type,bedrooms,
                     beds,cancellation_policy,cleaning_fee,
                     host_total_listings_count,price,property_type,
                     room_type,high_booking_rate,bathrooms,extra_people,
                     host_acceptance_rate,host_response_rate,
                     minimum_nights,market,perfect_rating_score) %>% 
              distinct(), by = "name") %>%
  filter(!is.na(perfect_rating_score))


# Set the seed for reproducibility
set.seed(123)

# Determine the number of rows for training data
train_size <- floor(0.7 * nrow(clean_data))

# Create indices for training data
train_indices <- createDataPartition(clean_data$perfect_rating_score, times = 1, p = 0.7, list = FALSE)

# Create training data
data_train <- clean_data[train_indices, ]

# Create testing data
data_test <- clean_data[-train_indices, ]



# EXAMPLE PREDICTIONS FOR CONTEST 1

#create a simple model to predict perfect_rating_score and generate predictions in the test data
train_perfect <- train %>%
  select(-high_booking_rate)

logistic_perfect <- glm(perfect_rating_score~accommodates, data = train_perfect, family = "binomial")
probs_perfect <- predict(logistic_perfect, newdata = test_x, type = "response")

#make binary classifications (make sure to check for NAs!)
classifications_perfect <- ifelse(probs_perfect > .29, "YES", "NO")
#classifications_perfect <- ifelse(is.na(classifications_perfect), "NO", classifications_perfect)
assertthat::assert_that(sum(is.na(classifications_perfect))==0)
table(classifications_perfect)


# EXAMPLE PREDICTIONS FOR CONTEST 2

#create a simple model to predict high_booking_rate and generate predictions in the test data
# make sure there are no NAs in your predictions
train_rate <- train %>%
  select(-c(perfect_rating_score))

logistic_rate <- glm(high_booking_rate~accommodates, data = train_rate, family = "binomial")
probs_rate <- predict(logistic_rate, newdata = test_x, type = "response")
#probs_rate <- ifelse(is.na(probs_rate), 0, probs_rate)
assertthat::assert_that(sum(is.na(probs_rate))==0)


#output your predictions
#they must be in EXACTLY this format
#a .csv file with the naming convention targetvariable_groupAAA.csv, where you replace targetvariable with your chosen target, and AAA with your group name
#in exactly the same order as they are in the test_x file

# For perfect_rating_score, each row should be a binary YES (is perfect) or NO (not perfect)
# For high_booking_rate, each row should be a number representing the likelihood of high_booking_rate = YES

#this code creates sample outputs in the correct format
write.table(classifications_perfect, "perfect_rating_score_group0.csv", row.names = FALSE)
write.table(probs_rate, "high_booking_rate_group0.csv", row.names = FALSE)

# I have evaluated these predictions against the test set
# the above perfect_rating_score predictions have TPR = 0.5244 and FPR = 0.4894 (so they would be disqualified!)
# the above high_booking_rate predictions have AUC = 0.525738