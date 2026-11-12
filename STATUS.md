# STATUS — pickup note (12 November 2026)

**Paper:** *Beyond the Rank Order: Quantifying Uncertainty in Physical-Test Rankings for Football Player Selection*
**Venue:** MIT Sloan Sports Analytics Conference 2027, Soccer track, Paper ID 268
**Repository:** https://github.com/praveenmaths89/athlete-ranking-uncertainty — **public** (keep public)
**Snapshot commit:** `bbe1ac4` (code, data, results and manuscript as of 12 Nov 2026; later commits only touch this note)

## What is in the repository
- `ranking.R` — the whole analysis in one procedural script (sections 1–9, seed 2026).
- `results/public/trials.csv` + `data_dictionary.csv` — de-identified trial data (athletes as `AID…` codes, days since first visit, age to 0.1 y).
- `results/` — all tables, figures, `headline.csv`, Excel workbook, `session_info.txt`, `reproducibility_runs.csv`.
- `results/paper/` — every number (`numbers.csv`), table and figure (Figures 1–3) used in the manuscript.
- `paper/` — manuscript text (`manuscript.txt`), Word builder (`build_docx.py`), references (`references.json`, `dois.txt`),
  and the **official submission file** `SSAC27_268_Beyond_the_Rank_Order.docx` with its PDF (10 pages; six main sections, no numbered subsections).
- `install_packages.R`, `packages.csv`, `config/kpi_rules.csv`, `LICENSE` (MIT).

## How to rebuild (from README)
```
Rscript install_packages.R
Rscript ranking.R                       # 11–40 min; RANKING_MODE=quick for a short test run
python3 paper/build_docx.py <path to SSAC27_paper_template.docx>   # needs python-docx, lxml and pandoc on PATH
soffice --headless --convert-to pdf --outdir paper paper/SSAC27_268_Beyond_the_Rank_Order.docx
```
The official SSAC 2027 Word template is **not** in the repository (gitignored, and its file properties name a
conference contact). A copy is kept outside the repo on the box at `/workspace/schools/ssac27-invitation/SSAC27_paper_template.docx`.
Section 1 of `ranking.R` needs the private VALD export and is skipped without it.

## Still open (to be filled by Praveen; no wording has been invented)
- [ ] Ethics approval and consent / assent procedure for players under 18 — highlighted placeholder
      `[[Ethics approval and the consent or assent procedure for players under 18 to be confirmed by the authors.]]`
      in `paper/manuscript.txt` (Data paragraph) and therefore in the Word/PDF. Fill it in `manuscript.txt` and rebuild,
      and make the same change in the private LaTeX copy.
- [ ] SSAC Google submission form **not submitted**. Deadline 11:59 PM EST, Tue 1 Dec 2026 (10:29 AM IST, Wed 2 Dec 2026).
- [ ] The manuscript is anonymous. Official format is the Word file; the LaTeX copy is a private convenience copy.
      Adding authors to the LaTeX copy needs Praveen's go-ahead.

## Never commit
- The private VALD ForceDecks export (`data/`, `VALD_EXPORT_DIR`), `.Renviron`, any name/ID ↔ code key.
- The official empty SSAC Word template (`paper/SSAC27_paper_template.docx`).

## Related repository
- LaTeX copy (private): https://github.com/praveenmaths89/ssac27-268-beyond-the-rank-order-latex

## Cited papers
- `papers/` holds legally available copies (open access, preprints, accepted manuscripts, official manuals) of the works cited in the paper. `papers/README.md` lists every cited work with its DOI, what version is stored, and why any item is link-only (paywalled or a book).
