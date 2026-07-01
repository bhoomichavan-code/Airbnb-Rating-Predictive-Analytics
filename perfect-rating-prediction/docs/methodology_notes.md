# Methodology notes (judgment calls & why)

**Target.** `perfect_rating_score` (`YES`/`NO`), used as given. 28.7% positive → imbalanced,
so AUC + TPR/FPR lead, not accuracy. `high_booking_rate` label was dropped (out of scope).

**Validation.** Single **stratified 70/30 split**, fixed seed (42). Chosen for simplicity and
explainability; 5-fold CV is the rigorous next step but adds narration cost. The split is the
*same* for every model so comparisons are apples-to-apples (the original used different
seeds/splits per model).

**Leakage prevention.** All learned preprocessing (imputation medians, scaling, one-hot
categories) lives in a `ColumnTransformer` inside each model's `Pipeline`, so it is fit on the
training fold only. Row-wise feature engineering (ratios, flags, category grouping, host tenure)
is leakage-free by construction and done up front.

**Missing-data decisions.**
- `cleaning_fee`, `security_deposit` missing → **0** plus a `has_*` flag (blank most plausibly
  means *none*).
- `price` missing → **median impute** (a price is never truly 0; the original set it to 0, which
  also corrupted `price_per_person`).
- Other numerics → median; categoricals → most-frequent, with `host_response_time` blanks made an
  explicit `"unknown"` level.
- Dropped for excessive missingness/redundancy: `square_feet` (98% missing), `weekly_price`,
  `monthly_price`, `host_acceptance_rate` (85% missing), `availability_60/90` (collinear).

**Feature engineering.** `host_tenure_months` (from `host_since`); `property_category` (35 sparse
`property_type` levels → 5 buckets); `bed_category`; collapsed `cancellation_policy`;
`price_per_person`; presence flags. High-cardinality geo/text fields excluded to keep the model
simple and explainable; `state` kept as the one geographic signal.

**Imbalance handling.** Logistic/Lasso/RF use `class_weight="balanced"`; XGBoost uses
`scale_pos_weight = neg/pos`. No resampling (keeps the pipeline simple and the probabilities
interpretable).

**Threshold.** Reported at the default 0.50 and at **Youden's J** (max `TPR − FPR`). The
contest's TPR/FPR depend on this choice; in practice it's a business decision (precision vs.
recall), not a fixed constant — unlike the original's hand-picked per-model thresholds.

**Models & why these.** Logistic = interpretable baseline (odds ratios); Lasso = transparent
feature selection; Random Forest and XGBoost = the strong tabular workhorses. Ridge and GBM from
the original were dropped as redundant.
