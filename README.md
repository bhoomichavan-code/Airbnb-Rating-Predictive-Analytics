# Airbnb "Perfect Rating Score" Prediction

A supervised machine-learning project that predicts whether an Airbnb listing
will earn a **perfect guest rating score** (binary: `YES` / `NO`). Originally
built as a Group 19 competition entry for a Data Mining & Predictive Analytics
course.

> **The original coursework lives in the [`OG Project/`](OG%20Project/) folder.**
> All the old files — scripts, outputs, docs, the data dictionary, and the
> known-issues review — are archived there unchanged. This top-level README
> describes the repository as a whole; new/improved work will live at the root.

## Problem

Given ~92,000 Airbnb listings with 60+ raw features (listing text, host
attributes, location, property type, price, availability, amenities), predict
the `perfect_rating_score` label for a held-out test set. The deliverable is a
CSV of `YES`/`NO` predictions.

## Approach (original submission)

- **Data cleaning & feature engineering** — parse price/fee strings to numbers,
  impute missing values, and build derived features such as `price_per_person`,
  `property_category`, `bed_category`, `has_cleaning_fee`, and
  `months_since_host_listed`.
- **Text mining** — tokenize `amenities`, `host_verifications`, and `features`
  into a document-term matrix (binary presence flags); a separate TF-IDF
  exploration also exists.
- **Models** — Logistic Regression, Lasso, Ridge, Random Forest, and XGBoost.
  Lasso is also used for feature selection feeding the tree models.
- **Evaluation** — confusion matrices with TPR / FPR at tuned thresholds.

Best validation result observed: **TPR ≈ 0.90 at FPR ≈ 0.04** (tree/boosting
models). See `OG Project/output/model_summaries.txt`.

## Repository structure

```
.
├── README.md                       # This file (repo overview)
└── OG Project/                     # Original coursework (archived, unchanged)
    ├── ISSUES.txt                  # Post-hoc review: known bugs & improvements
    ├── .gitignore
    ├── data/                       # Raw CSVs (gitignored — see "Data" below)
    ├── scripts/
    │   ├── final/                  # Final pipeline + group submission code
    │   │   ├── V9.R
    │   │   └── Data Mining Code Group 19.R
    │   ├── iterations/             # Earlier working versions (v1, v2, v6, v7, V8)
    │   ├── components/             # Lasso, TF-IDF, text mining, variable selection, helper
    │   └── provided/               # Instructor-provided template code
    ├── output/
    │   ├── perfect_rating_score_group19.csv   # Final predictions
    │   └── model_summaries.txt                # Run logs / confusion matrices
    └── docs/
        └── list of variables.xlsx             # Data dictionary
```

## Data

The raw data files are **not tracked in git** because
`airbnb_train_x_2024.csv` is ~337 MB, above GitHub's 100 MB per-file limit.
Place the following files in `OG Project/data/` to run the pipeline:

- `airbnb_train_x_2024.csv` — training features
- `airbnb_train_y_2024.csv` — training labels (`high_booking_rate`, `perfect_rating_score`)
- `airbnb_test_x_2024.csv` — test features
- `airbnb_hw2.csv` — earlier homework dataset

## Running

The main pipeline is `OG Project/scripts/final/V9.R` (R). It reads from `data/`,
cleans and engineers features, trains the models, and writes predictions to
`output/`.

> Note: the original scripts use a hardcoded `setwd()` path. Update it to point
> at this repo (or switch to relative paths) before running.

## Status & known issues

This was a time-boxed competition project. A review of the pipeline found
several bugs and methodology issues (e.g., the final export wrote the wrong
variable, imputation leakage across the train/validation split, and a mismatch
between the validated and submitted model). See
**[`OG Project/ISSUES.txt`](OG%20Project/ISSUES.txt)** for the full list,
severity ratings, and suggested fixes.
