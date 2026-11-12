# Beyond the Rank Order

Code, data and results for *"Beyond the Rank Order: Quantifying Uncertainty in Physical-Test Rankings
for Football Player Selection"* (MIT Sloan Sports Analytics Conference 2027, Soccer track).

The pipeline ranks footballers from repeated force-plate jump tests, benchmarks 15 ranking methods,
adjusts for age and reports every rank with its uncertainty (95% rank range, probability of selection
and a select / borderline / do-not-select decision).

## Run

1. Open `R_pipeline.Rproj` in RStudio (or run from the repository root).
2. Install packages once: `Rscript install_packages.R` (installs the versions listed in
   `packages.csv`; the full environment of the paper run is in `results/session_info.txt`).
3. Run the single analysis script `Rscript ranking.R` from the repository root (about 11–40 minutes depending on the machine; seed 2026).
   For a 2-minute test run set `RANKING_MODE=quick` in `.Renviron`.

Section 1 (data extraction) runs only if the private VALD ForceDecks export is available
(`VALD_EXPORT_DIR`, default `data/private_export/`). It rebuilds the de-identified trial file and,
if the result matches the released `results/public/trials.csv`, keeps the released file. Without the
export, Section 1 is skipped and the analysis starts from `results/public/trials.csv`, so every
result can be reproduced from this repository.

### Paper

The manuscript (SSAC 2027, Soccer track, Paper ID 268) is generated from the script's outputs:

```
Rscript ranking.R                     # writes results/paper/ (numbers, tables, figures)
python3 paper/build_docx.py           # fills paper/SSAC27_paper_template.docx; needs python-docx, lxml, pandoc
soffice --headless --convert-to pdf --outdir paper paper/SSAC27_268_Beyond_the_Rank_Order.docx
```

`paper/manuscript.txt` is the text; every number in it is a `{{key}}` filled from
`results/paper/numbers.csv`, and every table and figure is read from `results/paper/`.
The official SSAC 2027 Word template (sent by the conference to invited authors) is not redistributed here
(`paper/SSAC27_paper_template.docx` is gitignored): save your copy under that name or pass its path as the first
argument, e.g. `python3 paper/build_docx.py /path/to/SSAC27_paper_template.docx`. Do not commit the template.
The generated `.docx` and `.pdf` are committed, so the paper can be read without rebuilding it.
`paper/references.json` holds Crossref metadata for the references (DOIs in `paper/dois.txt`).
`results/reproducibility_runs.csv` records the reruns on different platforms and package versions.

**Official submission:** the Word file `paper/SSAC27_268_Beyond_the_Rank_Order.docx` (PDF alongside). A LaTeX copy of the
same paper (same text, numbers, figures and references) is kept in a separate private repository,
`praveenmaths89/ssac27-268-beyond-the-rank-order-latex`; it is not the submission. The paper has three figures
(the earlier pipeline figure was removed) and the mapping below is the current one. See `STATUS.md` for open items.

| Paper | File in `results/paper/` |
|---|---|
| Figure 1: agreement of the 15 ranking methods (simulation and held-out visit) | `fig1_benchmark.png` |
| Figure 2: current versus age-adjusted rank | `fig2_current_vs_age.png` |
| Figure 3: age-adjusted rank of every player with its 95% rank set and decision | `fig3_rank_ranges.png` |
| Table 1: selection decisions at 10%, 20% and 30% cut-offs | `table_selection_age.csv` |
| Table 2: case-study leaderboard around the 20% cut-off | `table_leaderboard.csv` |
| Table A1: the 11 KPIs | `table_kpis.csv` |
| Numbers in the text | `numbers.csv` |

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
| 9 | Paper assets: every number, table and figure used in the manuscript | `results/paper/` |

## Main outputs

| File | Content |
|---|---|
| `results/headline.csv` | Key numbers |
| `results/figures/Figure_1_rank_ranges.png` | Abstract Figure 1 (paper Figure 3): every player's age-adjusted rank with its 95% range, coloured by decision |
| `results/figures/Table_1_summary.png` | Abstract and paper Table 1: selection decisions at 10%, 20% and 30% cut-offs (also `.html`, `.csv`) |
| `results/figures/Figure_S1–S5` | Supplementary figures (not in the paper's numbering): selection probability, current vs age rank (cf. paper Figure 2), benchmark (cf. paper Figure 1), longitudinal vs single visit, retention check |
| `results/tables/6_uncertainty/athlete_rankings.csv` | Per player: ranks, 95% ranges, P(top k), decision |
| `results/athlete_ranking_results.xlsx` | All key tables, one sheet each |

## Files

| File | Purpose |
|---|---|
| `ranking.R` | The whole analysis in one script: setup, functions (quality control, LMS age models, mixed model, 15 ranking methods, simulation), then numbered sections 1–9 from raw data to paper assets |
| `results/public/` | De-identified data (`trials.csv`) and its data dictionary |
| `install_packages.R`, `packages.csv` | Package installation and versions |
| `paper/` | Manuscript source (`manuscript.txt`), Word builder (`build_docx.py`), references, generated `.docx` and `.pdf` |
| `config/kpi_rules.csv` | Variable dictionary: excluded metrics and better direction; edit to reuse the framework with another test battery |

## License

Code is released under the MIT License (see `LICENSE`).
