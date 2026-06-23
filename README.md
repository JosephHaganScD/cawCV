# cawCV
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20789601.svg)](https://doi.org/10.5281/zenodo.20789601)
Cluster-aware cross-validation partitioning for clinical prediction models
with repeated-measures predictors and subject-level binary outcomes.
## Overview
When a prediction model is built from data containing several observations per
subject, ordinary k-fold cross-validation that splits at the observation level
allows records from the same subject to appear in both the training and the
test set. The resulting performance estimates are optimistically biased.
`cawCV` supplies cross-validation partitioning that respects subject
clustering, together with diagnostic functions that characterise within-subject
dependence before a partitioning strategy is chosen.
Three partitioning functions are provided:
- `partition_naive_kfold()` — observation-level k-fold (the leakage-prone
  baseline)
- `partition_subject_kfold()` — subject-level k-fold; whole subjects are
  assigned to folds
- `partition_loco()` — leave-one-cluster-out
and two pre-validation diagnostics:
- `predictor_icc()` — per-predictor intraclass correlation
- `predictor_lag1()` — per-predictor within-subject lag-1 autocorrelation
The partitioning functions are agnostic to the modelling method and the
performance metric: they return fold assignments only, so any model and any
metric can be used downstream. The package depends only on base R and the
recommended `stats` package, both of which ship with every R installation.
## Installation
```r
# install.packages("devtools")
devtools::install_github("JosephHaganScD/cawCV")
```
## Example
```r
library(cawCV)
# 20 subjects, 6 records each; x1 carries subject-level structure, x2 does not
set.seed(1)
n_subj <- 20
n_obs <- 6
subject <- rep(seq_len(n_subj), each = n_obs)
b <- rnorm(n_subj)[subject]
dat <- data.frame(
  subject = subject,
  x1 = b + rnorm(n_subj * n_obs),
  x2 = rnorm(n_subj * n_obs)
)
# Diagnose within-subject dependence before choosing a strategy
predictor_icc(dat, c("x1", "x2"), dat$subject)
predictor_lag1(dat, c("x1", "x2"), dat$subject)
# Subject-level 5-fold partition
p <- partition_subject_kfold(dat$subject, k = 5, seed = 1)
p
# Extract the row indices for fold 1
fold1 <- get_fold(p, 1)
str(fold1)
```
## Development status and roadmap
`cawCV` v0.1.0 includes the partitioning and diagnostic functions documented
above, a unit-test suite, and a package vignette. Later releases will add
further pre-validation diagnostics as the associated methodological work is
published.
## Citation
The methodology implemented in this package is described in:
Hagan JL. Quantifying the Optimism of Naive Cross-Validation for Binary
Outcome Prediction with Repeated-Measures Predictors: A Simulation Study and
Clinical Illustration. medRxiv. 2026.
https://doi.org/10.64898/2026.05.27.26354222
The software itself is permanently archived on Zenodo:
Hagan JL. cawCV: Cluster-Aware Cross-Validation for Repeated-Measures
Predictors with Binary Outcomes [Software]. Zenodo. 2026.
https://doi.org/10.5281/zenodo.20789601
The manuscript is currently under review. Please cite both the preprint and
the Zenodo archive until a peer-reviewed journal reference is available.
## License
MIT. Copyright Joseph Hagan.
