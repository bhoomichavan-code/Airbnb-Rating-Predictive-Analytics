# One-page findings summary

**Question.** Can we predict whether an Airbnb listing earns a *perfect rating score* from its
structured attributes, and which attributes matter?

**Data.** 92,067 labelled listings; 28.7% are `YES` (perfect rating). Evaluated on a held-out
30% validation split (27,621 listings). The competition test file has no labels, so the
leaderboard score is unrecoverable — these are honest, reproducible validation numbers.

**What we did.** Structured features only; one stratified split; all preprocessing fit on the
training data only (no leakage); four models in increasing complexity — Logistic (baseline),
Lasso (feature selection), Random Forest, XGBoost.

**Headline result.** XGBoost is the best model at **ROC–AUC 0.730**, ahead of Random Forest
(0.724) and the linear models (0.692). At the default threshold it gives **TPR 0.66 / FPR 0.32**;
tuned with Youden's J it reaches **TPR ≈ 0.69 / FPR ≈ 0.35**.

**Interpretation.**
- The signal in structured fields is **moderate, not strong** — a ~0.04 AUC gain from linear to
  boosted trees, and accuracy that barely clears the 71% "predict all NO" baseline. That honesty
  is the point: the original version reported an implausible ~0.90 TPR that was a leakage artifact.
- Non-linear models help, so interactions among price, host behaviour, availability and property
  type carry information a straight-line model can't.
- The most influential attributes are host tenure, availability (365- and 30-day), price and
  price-per-person, and cleaning fee.

**Why we don't headline accuracy.** With 71% of listings labelled `NO`, a model that always
predicts `NO` scores 71% accuracy while being useless. AUC and the TPR/FPR trade-off describe
real discrimination; accuracy here does not.

**Limitations / next steps.** Structured fields only (listing text and review history excluded
by design); a single split rather than full cross-validation; light hyper-parameter tuning. Each
is a deliberate simplicity trade-off and a natural "what I'd do next" — add TF-IDF text features,
move to 5-fold CV, and tune via a small grid.
