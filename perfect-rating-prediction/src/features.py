"""Reusable feature engineering for the Airbnb perfect-rating model.

Every transform here is row-wise (depends only on a single listing's own values),
so it is safe to run before the train/validation split. Learned statistics
(imputation medians, scaling, category encoding) are handled separately inside the
sklearn pipeline, fit on the training split only.
"""
import numpy as np
import pandas as pd

PROPERTY_MAP = {
    "Apartment": "apartment", "Serviced apartment": "apartment", "Loft": "apartment",
    "Bed & Breakfast": "hotel", "Boutique hotel": "hotel", "Hostel": "hotel",
    "Townhouse": "condo", "Condominium": "condo",
    "Bungalow": "house", "House": "house",
}

NUM_FEATS = ["host_response_rate", "host_listings_count", "host_total_listings_count",
             "host_tenure_months", "accommodates", "bathrooms", "bedrooms", "beds",
             "price", "price_per_person", "security_deposit", "cleaning_fee",
             "extra_people", "guests_included", "minimum_nights", "maximum_nights",
             "availability_30", "availability_365"]
FLAG_FEATS = ["has_cleaning_fee", "has_security_deposit", "charges_for_extra", "has_min_nights"]
CAT_FEATS = ["room_type", "property_category", "bed_category", "cancellation_policy",
             "host_response_time", "state"]


def engineer(d: pd.DataFrame, today: str = "2024-04-30") -> pd.DataFrame:
    """Return a copy of ``d`` with engineered features added."""
    d = d.copy()
    hs = pd.to_datetime(d["host_since"], errors="coerce")
    d["host_tenure_months"] = (pd.Timestamp(today) - hs).dt.days / 30.44
    d["property_category"] = d["property_type"].map(PROPERTY_MAP).fillna("other")
    d["bed_category"] = np.where(d["bed_type"] == "Real Bed", "bed", "other")
    d["cancellation_policy"] = d["cancellation_policy"].replace(
        {"super_strict_30": "strict", "super_strict_60": "strict", "no_refunds": "strict"})
    d["host_response_time"] = d["host_response_time"].fillna("unknown")
    d["cleaning_fee"] = d["cleaning_fee"].fillna(0)
    d["security_deposit"] = d["security_deposit"].fillna(0)
    d["has_cleaning_fee"] = (d["cleaning_fee"] > 0).astype(int)
    d["has_security_deposit"] = (d["security_deposit"] > 0).astype(int)
    d["charges_for_extra"] = (d["extra_people"] > 0).astype(int)
    d["has_min_nights"] = (d["minimum_nights"] > 1).astype(int)
    price_filled = d["price"].fillna(d["price"].median())
    d["price_per_person"] = price_filled / d["accommodates"].replace({0: np.nan})
    return d
