# Processed data

`airbnb_modeling.csv` (~12 MB) is the structured-column extract the notebook reads:
the 23 raw structured fields plus the `perfect_rating_score` target, for 92,067 listings.
It was created from the raw files with:

```python
import pandas as pd
cols = ['host_response_rate','host_response_time','host_listings_count',
        'host_total_listings_count','host_since','room_type','property_type','bed_type',
        'state','accommodates','bathrooms','bedrooms','beds','price','security_deposit',
        'cleaning_fee','extra_people','guests_included','minimum_nights','maximum_nights',
        'availability_30','availability_365','cancellation_policy']
x = pd.read_csv('../raw/airbnb_train_x_2024.csv', usecols=lambda c: c in cols, low_memory=False)
y = pd.read_csv('../raw/airbnb_train_y_2024.csv')['perfect_rating_score']
x['perfect_rating_score'] = y.values
x.to_csv('airbnb_modeling.csv', index=False)
```
