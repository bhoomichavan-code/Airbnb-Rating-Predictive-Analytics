import pandas as pd

# Create a list of data
data = [
    "name - Type: character",
    "summary - Type: character",
    "space - Type: character",
    "description - Type: character",
    "experiences_offered - Type: character",
    "neighborhood_overview - Type: character",
    "notes - Type: character",
    "transit - Type: character",
    "access - Type: character",
    "interaction - Type: character",
    "house_rules - Type: character",
    "host_name - Type: character",
    "host_since - Type: Date",
    "host_location - Type: character",
    "host_about - Type: character",
    "host_response_time - Type: character",
    "host_response_rate - Type: numeric",
    "host_acceptance_rate - Type: character",
    "host_neighbourhood - Type: character",
    "host_listings_count - Type: numeric",
    "host_total_listings_count - Type: numeric",
    "host_verifications - Type: character",
    "street - Type: character",
    "neighborhood - Type: character",
    "neighborhood_group - Type: character",
    "city - Type: character",
    "state - Type: character",
    "zipcode - Type: character",
    "market - Type: factor",
    "smart_location - Type: character",
    "country_code - Type: character",
    "country - Type: character",
    "latitude - Type: numeric",
    "longitude - Type: numeric",
    "property_type - Type: character",
    "room_type - Type: factor",
    "accommodates - Type: numeric",
    "bathrooms - Type: numeric",
    "bedrooms - Type: numeric",
    "beds - Type: numeric",
    "bed_type - Type: factor",
    "amenities - Type: character",
    "square_feet - Type: numeric",
    "price - Type: numeric",
    "weekly_price - Type: numeric",
    "monthly_price - Type: numeric",
    "security_deposit - Type: numeric",
    "cleaning_fee - Type: numeric",
    "guests_included - Type: numeric",
    "extra_people - Type: numeric",
    "minimum_nights - Type: numeric",
    "maximum_nights - Type: numeric",
    "availability_30 - Type: numeric",
    "availability_60 - Type: numeric",
    "availability_90 - Type: numeric",
    "availability_365 - Type. numeric",
    "first_review - Type: Date",
    "license - Type: character",
    "jurisdiction_names - Type: character",
    "cancellation_policy - Type: factor",
    "features - Type: character",
    "high_booking_rate - Type: factor",
    "perfect_rating_score - Type: factor",
    "price_per_person - Type: numeric",
    "has_cleaning_fee - Type: character",
    "bed_category - Type: factor",
    "property_category - Type: factor",
    "median_ppp - Type: numeric",
    "ppp_ind - Type: factor",
    "property_catgory - Type: factor",
]

# Split each line into a list using space as delimiter
data_list = []
for line in data:
    key, value = line.split(" ", 1)
    data_list.append([key.strip(), value.strip()])  # Strip leading/trailing whitespaces

# Create a DataFrame from the list of key-value pairs
df = pd.DataFrame(data_list, columns=["Attribute", "Type"])

# Print the DataFrame
print(df.to_string(index=False))
