# Beyond the Rank Order

Code, data and results for *"Beyond the Rank Order: Quantifying Uncertainty in Physical-Test Rankings
for Football Player Selection"* (MIT Sloan Sports Analytics Conference 2027, Soccer track).

The pipeline ranks footballers from repeated force-plate jump tests, benchmarks 15 ranking methods,
adjusts for age and reports every rank with its uncertainty (95% rank range, probability of selection
and a select / borderline / do-not-select decision).

## Run

1. Open `R_pipeline.Rproj` in RStudio.
2. Install packages once:
   `install.packages(c("tidyverse", "here", "data.table", "gamlss", "lme4", "csranks", "writexl", "gt", "ragg", "MASS"))`
3. Run `ranking.R` top to bottom (about 40 minutes; seed 2026). For a 2-minute test run set
   `RANKING_MODE=quick` in `.Renviron`.

The analysis starts from the de-identified `results/public/trials.csv`, so every result can be
reproduced from this repository.

## Data

`results/public/trials.csv` holds 152,027 force-plate trial values (countermovement, drop and
single-leg jumps) from 190 male footballers tested in Mumbai, India; 183 remain after quality
control. It is released with the written approval of the data owner.

De-identification: athletes carry anonymous codes (`AID001`…) instead of names or device IDs;
calendar dates are replaced by days since each athlete's first visit; age is given to 0.1 year; only
size-independent metrics are included. Columns are described in `results/public/data_dictionary.csv`.

## Pipeline

| Section | Step | Output folder |
|---|---|---|
| 1 | Data → de-identified trial file (needs the private export; skipped otherwise) | `results/public/` |
| 2 | Clean: impossible attempts (any jump height > 80 cm or 0), attempts and values out of line with the athlete's own attempts (factor 2); ranking uses complete attempts only | `results/tables/1_quality_control/` |
| 3 | Select KPIs: relevant, available, reliable (ICC ≥ 0.75), not redundant (\|r\| < 0.7) | `results/tables/2_kpi_selection/` |
| 4 | Score: current and age-relative (LMS) z-scores | `results/tables/3_scores/` |
| 5 | True level: longitudinal mixed model per KPI | `results/tables/4_longitudinal/` |
| 6 | Benchmark 15 ranking methods: simulation (A) and held-out visit (B) | `results/tables/5_benchmark/` |
| 7 | Uncertainty: bootstrap rank ranges, P(top k) for k = 10/20/30%, decision groups, expected wrong selections (lower bound), exploratory retention check | `results/tables/6_uncertainty/` |
| 8 | Headline numbers, figures, Table 1, Excel workbook | `results/` |

## Main outputs

| File | Content |
|---|---|
| `results/headline.csv` | Key numbers |
| `results/figures/Figure_1_rank_ranges.png` | Figure 1: every player's age-adjusted rank with its 95% range, coloured by decision |
| `results/figures/Table_1_summary.png` | Table 1: selection decisions at 10%, 20% and 30% cut-offs (also `.html`, `.csv`) |
| `results/figures/Figure_S1–S5` | Selection probability, current vs age rank, benchmark, longitudinal vs single visit, retention check |
| `results/tables/6_uncertainty/athlete_rankings.csv` | Per player: ranks, 95% ranges, P(top k), decision |
| `results/athlete_ranking_results.xlsx` | All key tables, one sheet each |

## Files

| File | Purpose |
|---|---|
| `ranking.R` | The analysis, in numbered sections |
| `R/functions.R` | Quality control, LMS age models, mixed model, 15 ranking methods, simulation |
| `config/kpi_rules.csv` | Variable dictionary: excluded metrics and better direction; edit to reuse the framework with another test battery |
