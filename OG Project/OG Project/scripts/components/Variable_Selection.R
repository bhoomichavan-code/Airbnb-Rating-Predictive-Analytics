#load libraries
library(tidyverse)
library(dbplyr)

#load data files
setwd('C:/Users/Bhoomi/Desktop/Data Mining and Predictive Analytics/Project/')
train_x <- read_csv("airbnb_train_x_2024.csv")
train_y <- read_csv("airbnb_train_y_2024.csv")

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

#print names of all variables
names(clean_data)
str(clean_data)



library(dplyr)

# Get data types of all variables
data_types <- sapply(clean_data, class)

# Print data types and identify categories
str(data_types)

# Alternatively, for a more readable output:
clean_data_summary <- lapply(clean_data, function(x) {
  # Check for numeric data (including integers)
  if (is.numeric(x)) {
    "numeric"
    # Check for character data (potential text or categorical)
  } else if (is.character(x)) {
    # Further inspection might be needed to distinguish text from factors (categorical)
    "character (text/categorical?)"
    # Less common data types (logical, dates, etc.)
  } else {
    class(x)  # Print the actual class (e.g., "logical", "Date")
  }
})

names(clean_data_summary) <- names(clean_data)
print(clean_data_summary)


train_perfect <- clean_data %>%
  select(-high_booking_rate)

library(dplyr)

# Function to build and summarize logistic regression model
build_and_summarize_model <- function(formula, data) {

  # Build the model
  model <- glm(formula, data = train_perfect, family = binomial)
  
  # Print model summary
  summary(model)
}

# List of numeric variables
numeric_variables <- c(
  "host_response_rate",
  "host_listings_count",
  "host_total_listings_count",
  "latitude",
  "longitude",
  "accommodates",
  "bathrooms",
  "bedrooms",
  "beds",
  "square_feet",
  "price",
  "weekly_price",
  "monthly_price",
  "security_deposit",
  "cleaning_fee",
  "guests_included",
  "extra_people",
  "minimum_nights",
  "maximum_nights",
  "availability_30",
  "availability_60",
  "availability_90",
  "availability_365",
  "price_per_person",
  "median_ppp"
)

# Open a connection to a text file (replace "model_summaries.txt" with your desired filename)
sink("model_summaries.txt")

for (var in numeric_variables) {
  # Define formula within the loop for each variable
  formula <- paste("perfect_rating_score ~", var, sep = "")
  
  # Build and summarize the model for the current variable
  build_and_summarize_model(formula, train_perfect)
  
  # Add newline for readability between models
  cat("\n")
}

# Close the connection
sink()





# To view the saved output (optional)
# cat(read.table("model_summaries.txt", sep = "\t"))  # Assuming tab-delimited output

#model1 <- glm(perfect_rating_score ~ host_response_rate, data = train_perfect, family = binomial)
#summary(model1)

