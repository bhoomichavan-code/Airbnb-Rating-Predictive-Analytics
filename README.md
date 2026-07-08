# Airbnb "Perfect Rating Score" — What Makes a Listing Great?

## Overview

This is a supervised machine-learning project that predicts whether an Airbnb
listing will earn a **perfect guest rating score** (binary: `YES` / `NO`) from the attributes
known when the listing is posted. It moves from raw listing data through deliberate feature
engineering, a leakage-safe modeling pipeline, and an honest, threshold-aware evaluation —
comparing an interpretable baseline against two strong tree-based models. The emphasis is on
**sound methodology and clear communication**: turning messy listing data into a defensible,
reproducible answer rather than an inflated accuracy score.

The repository holds two things: the **original course-competition submission** (archived, in R)
and a **clean, from-scratch rework** (Python) that fixes its methodology. See
[Repository layout](#repository-layout).

## Business Problem

A perfect rating is a strong trust signal — it helps a listing rank, convert, and hold its
price. The practical question: **given only what's known at posting time, which listing
attributes are associated with earning a perfect rating, and how well can we predict it?** The
output is the kind of evidence a host could use to prioritise improvements, or a platform could
use to flag promising or at-risk listings.

A realistic wrinkle: this was a competition, and the **held-out test set has no labels**, so it
cannot be scored offline. Every metric here is therefore computed on a **validation split of the
labelled data** — which keeps the numbers honest and fully reproducible.

## Dataset

- **`airbnb_train_x_2024.csv`** (~337 MB) — 92,067 listings × 60+ raw features: listing text,
  host attributes, location, property type, price, availability, amenities.
- **`airbnb_train_y_2024.csv`** — labels per listing; this project uses
  **`perfect_rating_score`** (`YES` / `NO`).
- Data dictionary:
  [`perfect-rating-prediction/data/raw/data_dictionary.md`](perfect-rating-prediction/data/raw/data_dictionary.md).

**Target balance:** **28.7% `YES`** — imbalanced, which shapes both the modeling (class
weighting) and the choice of metrics (ROC-AUC and TPR/FPR over accuracy). Raw files are too large
for GitHub and are git-ignored; the notebook runs from a ~12 MB structured extract in
`perfect-rating-prediction/data/processed/`.

## Project Structure

```
.
├── README.md
├── OG Project/                        <- original coursework (archived, R)
└── perfect-rating-prediction/         <- the reworked project (Python)
    ├── requirements.txt
    ├── data/
    │   ├── raw/                        <- data dictionary (raw CSVs git-ignored)
    │   └── processed/                 <- structured extract the notebook reads
    ├── notebooks/
    │   └── airbnb_perfect_rating.ipynb    <- the whole analysis, top to bottom
    ├── src/                           <- reusable feature-engineering helper
    ├── visualizations/                <- exported charts (ROC, importance, confusion matrix)
    ├── models/                        <- saved model artifact (git-ignored, regenerated)
    └── docs/                          <- findings summary, methodology notes, metrics
```

## How to Run

1. **Clone and enter the project**

   ```bash
   git clone <your-repo-url>
   cd <repo>/perfect-rating-prediction
   ```

2. **Set up a virtual environment and install dependencies**

   ```bash
   python -m venv .venv
   source .venv/bin/activate          # Windows: .venv\Scripts\activate
   pip install -r requirements.txt
   ```

3. **Check the data** — the notebook reads `data/processed/airbnb_modeling.csv`, which is
   included. To rebuild it from raw, place the original CSVs in `data/raw/` (see the data
   dictionary).

4. **Run the analysis** — open `notebooks/airbnb_perfect_rating.ipynb` and Run All, or:

   ```bash
   jupyter nbconvert --to notebook --execute --inplace notebooks/airbnb_perfect_rating.ipynb
   ```

   It regenerates every figure, the metrics files, and the saved model.

## Deliverables

- One documented notebook (profiling → feature engineering → baseline → feature selection →
  models → evaluation → cross-validation)
- An interpretable baseline plus two tree models, compared on a single shared split
- Exported visualizations and machine-readable metrics
- A one-page [findings summary](perfect-rating-prediction/docs/findings_summary.md) and
  [methodology notes](perfect-rating-prediction/docs/methodology_notes.md)

## Results

On a held-out validation split of **27,621 listings** (default 0.50 threshold; the "predict all
NO" accuracy baseline is **0.713**):

| Model         | ROC-AUC   | TPR   | FPR   | Precision | Accuracy |
|---------------|:---------:|:-----:|:-----:|:---------:|:--------:|
| **XGBoost**   | **0.730** | 0.663 | 0.324 | 0.452     | 0.672    |
| Random Forest | 0.724     | 0.489 | 0.195 | 0.503     | 0.714    |
| Lasso         | 0.692     | 0.622 | 0.341 | 0.424     | 0.649    |
| Logistic      | 0.692     | 0.623 | 0.341 | 0.424     | 0.648    |

Five-fold cross-validation agrees (XGBoost **0.716 ± 0.007**), confirming the split wasn't lucky.
Because TPR/FPR move with the threshold, tuning to **Youden's J** (≈ 0.485) puts XGBoost at
**TPR ≈ 0.69 / FPR ≈ 0.35** — a trade-off that can be set to taste.

A deliberate, stated limitation: structured listing attributes carry only **moderate** predictive
signal (AUC ~0.69 → 0.73), and accuracy sits right on the imbalanced baseline. The project's
value is in **leakage-safe methodology and honest evaluation**, not a headline accuracy number —
the original version's implausible "0.90 TPR" was a leakage artifact this rework removes.

## Key Findings

1. **Perfect ratings are moderately predictable, not easily so** — AUC climbs from **0.69**
   (logistic) to **0.73** (XGBoost): a real but modest lift that honestly bounds what these
   fields can do.
2. **Non-linear models help** — XGBoost edges Random Forest and both beat the linear baseline, so
   interactions among price, host behaviour and availability carry signal.
3. **Accuracy is the wrong headline** — at ~29% positives, a "predict all NO" model already
   scores 71%, so the analysis leads with ROC-AUC and the TPR/FPR trade-off.
4. **The strongest attributes are host tenure, availability, price / price-per-person, and
   cleaning fee** (Random Forest importance) — established, active hosts with sensible pricing
   look most likely to earn perfect ratings.
5. **Leakage-safe evaluation changes the story** — fitting all preprocessing on the training fold
   only lowers the numbers versus the original, but makes them trustworthy and reproducible.

See [`docs/findings_summary.md`](perfect-rating-prediction/docs/findings_summary.md) for the
one-page write-up and [`docs/methodology_notes.md`](perfect-rating-prediction/docs/methodology_notes.md)
for the judgment calls.

## Roadmap

- [x] Structured feature set defined and documented
- [x] Leakage-safe preprocessing pipeline (fit on train only)
- [x] Logistic baseline + Lasso feature selection
- [x] Random Forest + XGBoost with class-imbalance handling
- [x] ROC-AUC + TPR/FPR/precision/accuracy on one shared split
- [x] 5-fold cross-validation
- [x] Notebook runs end to end with zero errors; figures + metrics exported
- [ ] Light hyper-parameter tuning (small grid)
- [ ] Add listing-text features (TF-IDF) and compare lift

## Repository layout

- **`perfect-rating-prediction/`** — the clean Python rework described above.
- **`OG Project/`** — the original Group 19 course submission (R), archived unchanged, including
  a post-hoc review of its bugs and methodology issues.

---

*Project. Each step is documented so the analysis and decisions can be followed end to
end.*
