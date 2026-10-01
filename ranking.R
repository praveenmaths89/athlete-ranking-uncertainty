# =============================================================================
# Longitudinal multi-test athlete ranking
# -----------------------------------------------------------------------------
# Paper: "Beyond the Rank Order: Quantifying Uncertainty in Physical-Test
#         Rankings for Youth Football Selection"
#
# Run top to bottom in RStudio (open R_pipeline.Rproj first).
# For a fast test run set RANKING_MODE=quick (e.g. in .Renviron).
#
#   0. Setup ............ packages, settings, output folders, plot style
#   1. Data ............. private VALD tables -> de-identified trial file
#   2. Clean ............ remove impossible / inconsistent attempts and values
#   3. Select KPIs ...... relevant, available, reliable, not redundant
#   4. Score ............ current and age-relative (LMS) z-scores
#   5. True level ....... longitudinal mixed model per KPI
#   6. Benchmark ........ 15 rankers: simulation (A) and held-out visit (B)
#   7. Uncertainty ...... bootstrap: rank ranges, P(top k), decisions, retention check
#   8. Outputs .......... headline, Figure 1, Table 1, supplementary figures, Excel
#
# Sections 2-8 only read results/public/trials.csv, so anyone can reproduce
# them from the public repository without the private VALD data.
# =============================================================================


# ---- 0. Setup ----------------------------------------------------------------

library(gamlss)      # LMS age models (load before tidyverse so dplyr wins)
library(tidyverse)
library(here)
library(data.table)  # fast reading of the large VALD trial file
library(lme4)        # longitudinal mixed models
library(csranks)     # confidence sets for ranks (Mogstad et al. 2024)
library(writexl)     # Excel output
library(gt)          # Table 1 as a formatted table (PNG + HTML)

source(here("R", "functions.R"))

quick <- Sys.getenv("RANKING_MODE") == "quick"

settings <- list(
  sport        = "Football",
  gender       = "Male",
  battery      = c(CMJ = "CMJ", DJ = "Drop Jump", SLJ = "Single-Leg Jump"),
  max_jump_cm  = 80,                     # QC rule 1: physically impossible jump height
  max_value_removed = 0.05,              # QC rule 3: a metric losing > 5% of values is unstable
  min_coverage = 0.95,                   # KPI measured in >= 95% of athletes
  min_icc      = 0.75,                   # reliability of the trial mean (Koo & Li 2016)
  max_r        = 0.70,                   # redundancy (Dormann et al. 2013)
  top_shares   = c(0.10, 0.20, 0.30),     # k = selection places as a share of the cohort
  main_share   = 0.20,                   # k used in the figures
  p_select     = 0.90,                   # decision groups from P(top k): select if >= 0.9,
  p_reject     = 0.10,                   #   don't select if < 0.1, otherwise retest
  min_followup_days = 365,               # retention check: time needed to be retested
  example_pair = c(7, 12),               # example: P(rank 7 truly above rank 12), full paper
  n_sim        = if (quick) 3 else 200,
  n_boot       = if (quick) 5 else 200,
  seed         = 2026
)

# Size-independent units only (Jaric 2002)
relative_units <- c("Second", "Millisecond", "Meter Per Second", "Meter Per Second Per Second",
                    "Newton Per Kilo", "Watt Per Kilo", "Newton Per Second Per Kilo",
                    "Watt Per Second Per Kilo", "Newton Second Per Kilo", "Centimeter",
                    "Meter", "Percent", "No Unit", "RSIModified")

# Private VALD export (not public); set VALD_DATA_DIR in .Renviron. Without it, section 1 is
# skipped and the analysis starts from the de-identified results/public/trials.csv
data_dir <- Sys.getenv("VALD_DATA_DIR", here("data"))

# Output folders
out <- list(public = "public", qc = "tables/1_quality_control", kpi = "tables/2_kpi_selection",
            score = "tables/3_scores", long = "tables/4_longitudinal", bench = "tables/5_benchmark",
            unc = "tables/6_uncertainty", fig = "figures") |>
  map(\(p) here("results", p))
walk(out, dir.create, recursive = TRUE, showWarnings = FALSE)

save_table <- function(df, folder, name) {
  write_csv(df, file.path(out[[folder]], paste0(name, ".csv")), na = "")
  invisible(df)
}
# PNG at 600 dpi (ragg) for Word documents; vector PDF (cairo, keeps symbols such as rho) for print
save_figure <- function(plot, name, width, height) {
  ggsave(file.path(out$fig, paste0(name, ".png")), plot, width = width, height = height, dpi = 600,
         bg = "white", device = ragg::agg_png)
  ggsave(file.path(out$fig, paste0(name, ".pdf")), plot, width = width, height = height, bg = "white",
         device = cairo_pdf)
}

# Supplementary figures: light grid, legend below
theme_paper <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 11), plot.title.position = "plot")
# Submitted figure: plain axes (no grid) so the rank ranges carry the picture
theme_figure <- theme_classic(base_size = 10, base_family = "Helvetica") +
  theme(plot.title = element_text(face = "bold", size = 11, margin = margin(b = 8)),
        plot.title.position = "plot",
        axis.title = element_text(size = 10), axis.text = element_text(size = 9, colour = "grey25"),
        axis.line = element_line(colour = "grey40", linewidth = 0.3),
        axis.ticks = element_line(colour = "grey40", linewidth = 0.3),
        legend.title = element_text(size = 9, face = "bold"), legend.text = element_text(size = 9),
        legend.key.height = unit(10, "pt"), plot.margin = margin(8, 12, 6, 6))
# Decision colours (Okabe-Ito blue and vermilion, checked for colour-blind separation and contrast on
# white; "don't select" is a deliberately neutral grey so the two decisions that matter stand out)
decision_colours <- c("select" = "#0072B2", "retest (grey zone)" = "#D55E00", "don't select" = "#9E9E9E")
decision_labels  <- c("select" = "Select", "retest (grey zone)" = "Borderline (retest)",
                      "don't select" = "Do not select")

set.seed(settings$seed)


# ---- 1. Data: private tables -> de-identified trial file ----------------------

public_trials_file <- file.path(out$public, "trials.csv")

if (dir.exists(data_dir)) {

  tests_clean <- read_csv(file.path(data_dir, "processed", "tests_clean.csv"),
                          col_select = c(profile_id, sport, gender, test_type, test_date, age_at_test),
                          show_col_types = FALSE) |>
    filter(sport == settings$sport, gender == settings$gender,
           test_type %in% settings$battery, !is.na(age_at_test))

  # Visits = days on which the athlete completed the whole battery
  sessions <- tests_clean |>
    summarise(age = mean(age_at_test), n_tests = n_distinct(test_type),
              .by = c(profile_id, test_date)) |>
    filter(n_tests == length(settings$battery))

  # Anonymous athlete codes; dates replaced by days since the first visit and by the
  # follow-up window (days from the first visit to the end of data collection)
  data_end <- max(sessions$test_date)
  athletes <- sessions |>
    distinct(profile_id) |>
    arrange(profile_id) |>
    mutate(athlete = sprintf("AID%03d", row_number()))
  sessions <- sessions |>
    left_join(athletes, by = "profile_id") |>
    arrange(athlete, test_date) |>
    mutate(session = row_number(),
           days_from_first = as.numeric(test_date - first(test_date)),
           followup_window_days = as.numeric(data_end - first(test_date)), .by = athlete)

  tests <- read_csv(file.path(data_dir, "raw", "tests.csv"), show_col_types = FALSE) |>
    filter(testType %in% names(settings$battery)) |>
    mutate(test_type = settings$battery[testType],
           test_date = as_date(ymd_hms(recordedDateUtc) +                       # local test date;
                               minutes(coalesce(recordedDateOffset, 330)))) |>   # 330 min = IST
    inner_join(sessions, by = c(profileId = "profile_id", "test_date")) |>
    mutate(test_no = dense_rank(testId), .by = c(athlete, session, test_type))

  trials_raw <- fread(file.path(data_dir, "raw", "trials.csv"),
                      select = c("testId", "trialId", "trialLimb", "resultName", "value", "limb", "unit")) |>
    as_tibble() |>
    filter(testId %in% tests$testId, limb == "Trial", unit %in% relative_units)

  trials_raw |>
    inner_join(select(tests, testId, athlete, session, days_from_first, followup_window_days, age, test_type, test_no),
               by = "testId") |>
    mutate(metric = paste(test_type, resultName, sep = ": "),
           limb   = trialLimb,
           trial  = dense_rank(trialId), .by = c(testId, trialLimb)) |>   # trials numbered in ID order
    select(athlete, session, days_from_first, followup_window_days, age, test_type, test_no, limb, trial,
           metric, unit, value) |>
    arrange(athlete, session, metric, test_no, limb, trial) |>
    write_csv(public_trials_file)

  tribble(~column, ~description,
          "athlete", "Anonymous athlete code",
          "session", "Visit number (1 = first visit with the full battery)",
          "days_from_first", "Days since the athlete's first visit",
          "followup_window_days", "Days from the athlete's first visit to the end of data collection",
          "age", "Age at the visit (years, 0.1)",
          "test_type", "CMJ, Drop Jump or Single-Leg Jump",
          "test_no", "Test number when a test was repeated on the same day",
          "limb", "Both, Left or Right",
          "trial", "Trial (attempt) number within the test and limb",
          "metric", "Test: VALD metric name",
          "unit", "VALD unit",
          "value", "Trial value") |>
    save_table("public", "data_dictionary")
}

trials_all <- read_csv(public_trials_file, show_col_types = FALSE)
attempt    <- c("athlete", "session", "test_type", "test_no", "limb", "trial")   # one jump


# ---- 2. Clean: trial quality control -------------------------------------------------
# Each attempt is judged only against the same athlete's other attempts in the same
# test, leg and day - never against other athletes.

jump_height <- trials_all |>
  filter(str_detect(metric, fixed(": Jump Height (Flight Time)")), !str_detect(metric, "Inches")) |>
  select(all_of(attempt), jump_height = value)

# Rule 1: jump height is computed three ways from the same force recording; if any
# calculation is physically impossible (> 80 cm or 0 cm), the recording failed
impossible_attempts <- trials_all |>
  filter(unit == "Centimeter", str_detect(metric, "Jump Height")) |>
  summarise(impossible = any(value > settings$max_jump_cm | value <= 0), .by = all_of(attempt))
rule1 <- jump_height |>
  left_join(impossible_attempts, by = attempt) |>
  mutate(impossible = coalesce(impossible, FALSE))

# Rule 2: inconsistent with the athlete's own attempts (applied after rule 1)
rule2 <- rule1 |>
  filter(!impossible) |>
  mutate(inconsistent = inconsistent_attempt(jump_height),
         .by = c(athlete, session, test_type, test_no, limb))

removed <- bind_rows(
  filter(rule1, impossible) |> mutate(reason = sprintf("impossible jump height (> %d cm or 0)", settings$max_jump_cm)),
  filter(rule2, inconsistent) |> mutate(reason = "inconsistent with own attempts (factor 2)")) |>
  select(all_of(attempt), jump_height, reason) |>
  save_table("qc", "removed_attempts")

jump_height |>
  left_join(select(removed, all_of(attempt), reason), by = attempt) |>
  summarise(attempts = n(),
            removed_impossible   = sum(str_detect(reason, "impossible"), na.rm = TRUE),
            removed_inconsistent = sum(str_detect(reason, "inconsistent"), na.rm = TRUE),
            percent_removed      = round(100 * mean(!is.na(reason)), 1),
            .by = test_type) |>
  save_table("qc", "qc_summary")

trials <- anti_join(trials_all, removed, by = attempt)

# Metrics must keep one sign (e.g. velocities stored as negative -> magnitude)
one_sign <- trials |> summarise(ok = all(value > 0) | all(value < 0), .by = metric)
trials   <- trials |> semi_join(filter(one_sign, ok), by = "metric") |> mutate(value = abs(value))

# Rule 3: the same check on each metric value (a metric can be mis-calculated inside
# an otherwise valid attempt); only that value is removed
trials <- trials |>
  mutate(inconsistent = inconsistent_attempt(value),
         .by = c(athlete, session, test_type, test_no, limb, metric))

value_qc <- trials |>
  mutate(single_value = n() == 1, .by = c(athlete, session, test_type, test_no, limb, metric)) |>
  summarise(values = n(), removed = sum(inconsistent), share_removed = mean(inconsistent),
            share_not_cross_checkable = mean(single_value), .by = metric) |>
  save_table("qc", "removed_values_by_metric")

trials <- trials |> filter(!inconsistent) |> select(-inconsistent)


# ---- 3. Select KPIs -------------------------------------------------------------------

first_trials <- filter(trials, session == 1)
n_athletes   <- n_distinct(first_trials$athlete)

# (a) Relevant: rules and directions from the variable dictionary
rules <- read_csv(here("config", "kpi_rules.csv"), show_col_types = FALSE)
candidates <- tibble(metric = unique(trials$metric)) |>
  mutate(pattern = map_chr(str_remove(metric, "^.*?: "), \(x) first_match(x, rules$pattern))) |>
  left_join(rules, by = "pattern")

# (b) Available and (c) reliable: ICC of the athlete's trial mean (Weir 2005)
coverage <- first_trials |>
  summarise(coverage = n_distinct(athlete) / n_athletes, .by = metric)
reliability <- first_trials |>
  summarise(var_within = var(value), n = n(), .by = c(athlete, test_no, limb, metric)) |>
  filter(n >= 2) |>
  summarise(var_within = mean(var_within), .by = metric) |>
  left_join(first_trials |>
              summarise(mean = mean(value), k = n(), .by = c(athlete, metric)) |>
              summarise(var_between = var(mean), k = mean(k), .by = metric),
            by = "metric") |>
  mutate(icc = 1 - (var_within / k) / var_between) |>
  select(metric, icc)

candidates <- candidates |>
  left_join(coverage, by = "metric") |>
  left_join(reliability, by = "metric") |>
  left_join(select(value_qc, metric, share_removed), by = "metric") |>
  mutate(status = case_when(rule == "exclude"                   ~ "excluded: not a performance KPI",
                            coverage < settings$min_coverage    ~ "excluded: coverage",
                            share_removed > settings$max_value_removed ~ "excluded: unstable (> 5% values inconsistent)",
                            is.na(icc) | icc < settings$min_icc ~ "excluded: reliability",
                            .default = "pool"))
pool <- filter(candidates, status == "pool") |> arrange(desc(icc))

# Age models (LMS) for every pool KPI, fitted on first-visit attempts (used in step 4 too)
lms_models <- map(pool$metric, \(k) {
  d <- filter(first_trials, metric == k)
  fit_lms(d$value, d$age)
}) |> set_names(pool$metric)

# (d) Not redundant: |r| of age-relative first-visit means, most reliable first
first_age_z <- first_trials |>
  filter(metric %in% pool$metric) |>
  mutate(z = lms_z(lms_models[[metric[1]]], value, age), .by = metric) |>
  summarise(z = mean(z), .by = c(athlete, metric)) |>
  pivot_wider(names_from = metric, values_from = z)
selected <- greedy_select(cor(as.matrix(first_age_z[pool$metric]), use = "pairwise.complete.obs"),
                          pool$metric, settings$max_r)

candidates <- candidates |>
  mutate(status = case_when(status == "pool" & metric %in% selected ~ "selected",
                            status == "pool" ~ "excluded: redundant with a more reliable KPI",
                            .default = status)) |>
  arrange(status, desc(icc)) |>
  select(metric, rule, rationale, coverage, share_removed, icc, status) |>
  save_table("kpi", "candidates")

kpis <- filter(candidates, status == "selected") |>
  arrange(desc(icc)) |>
  select(kpi = metric, direction = rule, icc) |>
  save_table("kpi", "selected_kpis")
message("Selected KPIs (", nrow(kpis), "):\n  ", paste(kpis$kpi, collapse = "\n  "))


# ---- 4. Score ------------------------------------------------------------------------

is_cost <- set_names(kpis$direction == "cost", kpis$kpi)
first_stats <- first_trials |>
  filter(metric %in% selected) |>
  summarise(ref_mean = mean(value), ref_sd = sd(value), .by = metric)

trials_z <- trials |>
  filter(metric %in% selected) |>
  left_join(first_stats, by = "metric") |>
  mutate(z_current = (value - ref_mean) / ref_sd,
         z_age     = lms_z(lms_models[[metric[1]]], value, age), .by = metric) |>
  mutate(sign      = if_else(is_cost[metric], -1, 1),
         z_current = sign * z_current,
         z_age     = sign * z_age) |>
  mutate(time = (days_from_first - max(days_from_first)) / 365.25, .by = athlete) |>
  mutate(visit = paste(athlete, session), kpi = metric) |>
  select(all_of(attempt), visit, days_from_first, time, age, kpi, value, z_current, z_age)

# Ranking uses complete attempts only: every selected KPI of the test passed QC
# (an attempt with one mis-calculated metric is a partly corrupt recording)
kpis_per_test <- count(tibble(test_type = str_remove(selected, ":.*")), test_type, name = "n_selected")
complete_attempts <- trials_z |>
  summarise(n_kpi = n_distinct(kpi), .by = all_of(attempt)) |>
  left_join(kpis_per_test, by = "test_type") |>
  mutate(complete = n_kpi == n_selected)
complete_attempts |>
  summarise(attempts = n(), incomplete_excluded = sum(!complete), .by = test_type) |>
  save_table("qc", "incomplete_attempts_excluded_from_ranking")
trials_z <- semi_join(trials_z, filter(complete_attempts, complete), by = attempt)

# No cap on z: stop if any score is not finite, list |z| > 4 for inspection
if (any(!is.finite(c(trials_z$z_current, trials_z$z_age)))) {
  stop("Non-finite z-scores found: inspect these attempts before continuing.")
}
trials_z |>
  filter(abs(z_current) > 4 | abs(z_age) > 4) |>
  select(all_of(attempt), age, kpi, value, z_current, z_age) |>
  save_table("score", "extreme_scores_for_inspection")


# ---- 5. True level: longitudinal model -------------------------------------------------

long_version <- function(version, data = trials_z) {
  data |>
    mutate(z = if (version == "age") z_age else z_current) |>
    select(athlete, session, visit, time, kpi, z)
}

fits <- list(current = fit_levels(long_version("current"), selected),
             age     = fit_levels(long_version("age"), selected))

# Athletes with a level for every KPI in both versions
athletes_main <- intersect(rownames(fits$age$level)[complete.cases(fits$age$level)],
                           rownames(fits$current$level)[complete.cases(fits$current$level)])
X <- map(fits, \(f) f$level[athletes_main, selected])

map_dfr(fits, \(f) f$varcomp, .id = "version") |> save_table("long", "variance_components")
imap_dfr(X, \(x, v) as_tibble(x, rownames = "athlete") |> mutate(version = v, .before = 1)) |>
  save_table("long", "athlete_levels")

athlete_info <- trials_z |>
  summarise(visits = n_distinct(session), attempts = n_distinct(paste(session, test_type, test_no, limb, trial)),
            age_latest = max(age), .by = athlete) |>
  filter(athlete %in% athletes_main) |>
  arrange(match(athlete, athletes_main))


# ---- 6. Benchmark ----------------------------------------------------------------------

# Test A: simulation with known truth (real visit pattern, fitted noise)
design <- trials_z |>
  filter(athlete %in% athletes_main) |>
  summarise(n_trials = n(), .by = c(athlete, session, time, kpi)) |>
  summarise(n_trials = round(median(n_trials)), .by = c(athlete, session, time))

test_a <- map(seq_len(settings$n_sim), \(i) simulate_once(design, fits$age$varcomp, cor(X$age))) |>
  bind_rows(.id = "rep")
test_a_summary <- test_a |>
  summarise(across(c(spearman, ndcg_at_10), mean), .by = c(input, truth, ranker)) |>
  save_table("bench", "test_a_simulation")

# Test B: held-out last visit (athletes with >= 2 visits)
multi_visit <- filter(athlete_info, visits >= 2)$athlete
last_visit  <- trials_z |>
  filter(athlete %in% multi_visit) |>
  summarise(session = max(session), .by = athlete) |>
  mutate(visit = paste(athlete, session))
train <- filter(trials_z, !visit %in% last_visit$visit)

visit_means <- function(d, version) {
  d |>
    mutate(z = if (version == "age") z_age else z_current) |>
    summarise(z = mean(z), .by = c(athlete, kpi)) |>
    pivot_wider(names_from = kpi, values_from = z) |>
    arrange(match(athlete, multi_visit)) |>
    select(all_of(selected)) |>
    as.matrix()
}

test_b <- map_dfr(c("current", "age"), \(v) {
  predicted <- fit_levels(long_version(v, data = train), selected)$level[multi_visit, selected]
  observed  <- visit_means(filter(trials_z, visit %in% last_visit$visit), v)
  latest    <- train |>
    filter(athlete %in% multi_visit) |>
    filter(session == max(session), .by = athlete) |>
    visit_means(v)
  # Compare only athletes with every KPI measured at the held-out visit (and before it)
  ok    <- complete.cases(predicted) & complete.cases(observed) & complete.cases(latest)
  s_obs <- score_all(observed[ok, ])
  tibble(version = v, ranker = rankers$ranker, athletes_compared = sum(ok),
         longitudinal      = map2_dbl(score_all(predicted[ok, ]), s_obs, \(a, b) cor(a, b, method = "spearman")),
         latest_visit_only = map2_dbl(score_all(latest[ok, ]), s_obs, \(a, b) cor(a, b, method = "spearman")))
}) |>
  save_table("bench", "test_b_held_out_visit")

# Winner: rank the rankers within each check, average within each test, then
# average the two tests so that A and B count equally (one method for the study)
checks <- bind_rows(
  test_a_summary |>
    filter(input == "longitudinal") |>
    pivot_longer(c(spearman, ndcg_at_10), names_to = "metric") |>
    mutate(test = "A: simulation", check = paste(truth, metric)),
  test_b |>
    transmute(test = "B: held-out visit", check = version, ranker, value = longitudinal)
) |>
  mutate(check_rank = rank(-value, ties.method = "average"), .by = c(test, check))

benchmark <- checks |>
  summarise(mean_rank = mean(check_rank), .by = c(test, ranker)) |>
  pivot_wider(names_from = test, values_from = mean_rank) |>
  mutate(overall_mean_rank = (`A: simulation` + `B: held-out visit`) / 2) |>
  left_join(test_a_summary |> filter(input == "longitudinal") |>
              summarise(sim_spearman = mean(spearman), sim_ndcg10 = mean(ndcg_at_10), .by = ranker),
            by = "ranker") |>
  left_join(test_b |> summarise(heldout_spearman = mean(longitudinal), .by = ranker), by = "ranker") |>
  arrange(overall_mean_rank) |>
  save_table("bench", "benchmark_summary")

winner <- benchmark$ranker[1]
message("Winning ranking method: ", winner)

# Does the longitudinal model beat ranking from the latest single visit?
longitudinal_gain <- bind_rows(
  test_b |>
    summarise(longitudinal = mean(longitudinal), latest_visit_only = mean(latest_visit_only), .by = version) |>
    mutate(test = "B: held-out visit (main evidence)"),
  test_a_summary |>
    summarise(spearman = mean(spearman), .by = input) |>
    pivot_wider(names_from = input, values_from = spearman) |>
    mutate(version = "age", test = "A: simulation (supportive)")
) |>
  select(test, version, longitudinal, latest_visit_only) |>
  save_table("bench", "longitudinal_vs_single_visit")


# ---- 7. Uncertainty ------------------------------------------------------------------------

n_main   <- length(athletes_main)
k_values <- set_names(ceiling(settings$top_shares * n_main), paste0(100 * settings$top_shares, "%"))
k_top    <- ceiling(settings$main_share * n_main)                 # k used in the figures

# 7a. Bootstrap: resample whole attempts (all KPIs together) within athlete, visit, test and leg
attempt_keys <- trials_z |> filter(athlete %in% athletes_main) |> distinct(across(all_of(attempt)))

boot_scores <- map(seq_len(settings$n_boot), \(b) {
  picked <- slice_sample(attempt_keys, prop = 1, replace = TRUE,
                         by = c(athlete, session, test_type, test_no, limb))
  d <- inner_join(filter(trials_z, athlete %in% athletes_main), picked, by = attempt,
                  relationship = "many-to-many")
  map(c(current = "current", age = "age"), \(v)
    score_one(fit_levels(long_version(v, data = d), selected)$level[athletes_main, selected], winner))
})

# Point rank, formal 95% rank range (csranks) and the bootstrap ranks, per version
rank_uncertainty <- function(version) {
  S     <- sapply(boot_scores, \(b) b[[version]])                  # athletes x bootstraps
  score <- score_one(X[[version]], winner)
  cs    <- csranks(score, diag(apply(S, 1, var)), coverage = 0.95, simul = FALSE, seed = settings$seed)
  list(table = tibble(athlete = athletes_main, score = score, rank = rank_best1(score),
                      rank_lower = cs$L, rank_upper = cs$U),
       ranks = apply(S, 2, rank_best1))
}
unc <- map(c(current = "current", age = "age"), rank_uncertainty)

# For k selection places: P(truly in top k), formal tier and decision group
decisions <- function(u, k) {
  n <- nrow(u$table)
  u$table |>
    mutate(p_topk   = rowMeans(u$ranks <= k),
           tier     = case_when(rank_upper <= k     ~ "clearly top",
                                rank_lower >  n - k ~ "clearly bottom",
                                .default = "not separable"),
           decision = case_when(p_topk >= settings$p_select ~ "select",
                                p_topk <  settings$p_reject ~ "don't select",
                                .default = "retest (grey zone)"))
}

# 7b. Selection summary for k = 10%, 20%, 30% of the cohort
selection <- expand_grid(version = c("current", "age"), share = names(k_values)) |>
  pmap_dfr(\(version, share) {
    k <- k_values[[share]]
    d <- decisions(unc[[version]], k)
    tibble(version = version, share = share, k = k,
           select = sum(d$decision == "select"),
           retest_grey_zone = sum(d$decision == "retest (grey zone)"),
           dont_select = sum(d$decision == "don't select"),
           clearly_top_formal = sum(d$tier == "clearly top"),
           median_rank_range_width = median(d$rank_upper - d$rank_lower),
           expected_wrong_selections_lower_bound = sum(1 - d$p_topk[d$rank <= k]))
  }) |>
  save_table("unc", "selection_by_k")

# 7c. Pairwise P(row athlete ranks above column athlete), age-relative
age_ranks <- unc$age$ranks
pairwise  <- Reduce(`+`, map(seq_len(ncol(age_ranks)), \(b) outer(age_ranks[, b], age_ranks[, b], "<"))) /
  ncol(age_ranks)
dimnames(pairwise) <- list(athletes_main, athletes_main)
as_tibble(pairwise, rownames = "athlete") |> save_table("unc", "pairwise_probability_age")

by_rank <- arrange(unc$age$table, rank)
pair    <- by_rank$athlete[settings$example_pair]
example <- tibble(athlete_a = pair[1], rank_a = settings$example_pair[1],
                  athlete_b = pair[2], rank_b = settings$example_pair[2],
                  p_a_ranks_above_b = pairwise[pair[1], pair[2]]) |>
  save_table("unc", "example_pair")

# 7d. Per-athlete table for the main k
athlete_report <- athlete_info |>
  left_join(rename_with(decisions(unc$current, k_top), \(x) paste0("current_", x), -athlete), by = "athlete") |>
  left_join(rename_with(decisions(unc$age, k_top), \(x) paste0("age_", x), -athlete), by = "athlete") |>
  mutate(hidden_talent = age_p_topk >= 0.5 & current_rank > k_top) |>
  arrange(age_rank) |>
  save_table("unc", "athlete_rankings")

# 7e. Exploratory retention check: do athletes who were retested score higher at their
# first visit? Only athletes with >= 1 year in which a retest was possible; first-visit
# data only (no longitudinal model, so no shrinkage artefact)
athlete_visits <- trials_all |>
  summarise(retested = max(session) >= 2, followup_window_days = first(followup_window_days), .by = athlete)

retention <- map_dfr(c(current = "current", age = "age"), \(v) {
  first_visit <- trials_z |>
    filter(session == 1) |>
    mutate(z = if (v == "age") z_age else z_current) |>
    summarise(z = mean(z), .by = c(athlete, kpi)) |>
    pivot_wider(names_from = kpi, values_from = z) |>
    drop_na()
  tibble(athlete = first_visit$athlete, version = v,
         first_visit_score = score_one(as.matrix(first_visit[selected]), winner))
}) |>
  left_join(athlete_visits, by = "athlete") |>
  filter(followup_window_days >= settings$min_followup_days) |>
  save_table("unc", "retention_first_visit_scores")

retention_summary <- retention |>
  summarise(athletes = n(), n_retested = sum(retested),
            auc = auc(first_visit_score, retested),
            wilcoxon_p = wilcox.test(first_visit_score[retested], first_visit_score[!retested])$p.value,
            .by = version) |>
  save_table("unc", "retention_summary")

# Where do retested athletes stand at their first visit vs in the final ranking
# (standing at their latest visit)? Descriptive only: a change can reflect real
# development or the model using more data for them.
retention |>
  filter(version == "age") |>
  mutate(first_visit_rank = rank_best1(first_visit_score)) |>
  inner_join(select(athlete_report, athlete, age_rank), by = "athlete") |>
  summarise(athletes = n(),
            median_first_visit_rank = median(first_visit_rank),
            median_final_rank = median(age_rank),
            .by = retested) |>
  save_table("unc", "retention_rank_first_vs_final")


# ---- 8. Outputs -----------------------------------------------------------------------------

main_sel <- filter(selection, version == "age", k == k_top)
gain_b   <- filter(longitudinal_gain, str_detect(test, "^B"), version == "age")
ret_age  <- filter(retention_summary, version == "age")
headline <- tibble(
  athletes = n_main, athletes_multi_visit = length(multi_visit), attempts_removed_qc = nrow(removed),
  kpis = length(selected), winner = winner, k_main = k_top,
  select_age = main_sel$select, retest_age = main_sel$retest_grey_zone, dont_select_age = main_sel$dont_select,
  clearly_top_formal_age = main_sel$clearly_top_formal,
  expected_wrong_selections_lower_bound_age = main_sel$expected_wrong_selections_lower_bound,
  p_rank7_above_rank12 = example$p_a_ranks_above_b,
  heldout_spearman_longitudinal = gain_b$longitudinal,
  heldout_spearman_latest_visit = gain_b$latest_visit_only,
  median_age_topk_current = median(athlete_report$age_latest[athlete_report$current_rank <= k_top]),
  median_age_topk_age     = median(athlete_report$age_latest[athlete_report$age_rank <= k_top]),
  topk_overlap_current_vs_age = sum(athlete_report$current_rank <= k_top & athlete_report$age_rank <= k_top),
  hidden_talent = sum(athlete_report$hidden_talent),
  retention_auc_age = ret_age$auc, retention_p_age = ret_age$wilcoxon_p
)
write_csv(headline, here("results", "headline.csv"))
print(headline, width = Inf)

# Figure 1 (abstract): age-adjusted rank of every player with its 95% range, coloured by decision group.
# y = rank position (1 at the top); legend sits in the empty top-right corner; every text element counts
# towards the abstract's word limit, so labels are short.
rank_breaks <- c(1, k_top, 100, 150, n_main)
fig1 <- ggplot(athlete_report, aes(x = age_rank, y = age_rank, xmin = age_rank_lower, xmax = age_rank_upper,
                                   colour = factor(age_decision, levels = names(decision_labels)))) +
  geom_vline(xintercept = k_top + 0.5, linetype = "dashed", linewidth = 0.4, colour = "grey30") +
  annotate("text", x = k_top + 3, y = n_main - 2, hjust = 0, size = 3.1, colour = "grey20",
           label = sprintf("Cut-off (top %d%%)", round(100 * settings$main_share))) +
  geom_linerange(linewidth = 0.35) +
  geom_point(size = 0.6) +
  scale_x_continuous(breaks = rank_breaks, limits = c(1, n_main), expand = expansion(mult = 0.01)) +
  scale_y_reverse(breaks = rank_breaks, expand = expansion(mult = 0.01)) +
  scale_colour_manual(values = decision_colours, labels = decision_labels, name = "Decision") +
  guides(colour = guide_legend(override.aes = list(linewidth = 1.2, size = 1.6))) +
  labs(title = "Rankings are certain at the extremes, uncertain near the cut-off",
       x = "Plausible rank (95% range)", y = "Age-adjusted rank (1 = best)") +
  theme_figure +
  theme(legend.position = "inside", legend.position.inside = c(0.97, 0.97), legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", colour = "grey80", linewidth = 0.3),
        legend.margin = margin(4, 6, 4, 6))
save_figure(fig1, "Figure_1_rank_ranges", 6.5, 4.8)

# Table 1 (abstract): selection decisions at each cut-off (age-adjusted ranking); supports the
# Results' robustness and wrong-selection statements
table1 <- selection |>
  filter(version == "age") |>
  transmute(`Cut-off` = paste("Top", share), Places = k,
            Select = select, Borderline = retest_grey_zone, `Do not select` = dont_select,
            wrong = sprintf("%.1f", expected_wrong_selections_lower_bound))   # lower bound
write_csv(rename(table1, "Wrong selections (\u2265)" = wrong), file.path(out$fig, "Table_1_summary.csv"))
table1_gt <- gt(table1) |>
  tab_header(title = md("**Table 1.** Selection decisions by cut-off (age-adjusted ranking).")) |>
  cols_label(wrong = "Wrong selections (\u2265)") |>
  cols_align("left", `Cut-off`) |> cols_align("center", -`Cut-off`) |>
  cols_width(`Cut-off` ~ px(90), wrong ~ px(170), everything() ~ px(110)) |>
  tab_options(table.font.names = "Helvetica", table.font.size = px(13), heading.align = "left",
              heading.title.font.size = px(13), column_labels.font.weight = "bold",
              table.border.top.color = "black", table.border.bottom.color = "black",
              table_body.border.bottom.color = "black", column_labels.border.bottom.color = "black",
              heading.border.bottom.color = "white", table_body.hlines.color = "grey85",
              data_row.padding = px(5))
gtsave(table1_gt, file.path(out$fig, "Table_1_summary.png"), zoom = 6, expand = 8)   # ~600 dpi at 6 in
gtsave(table1_gt, file.path(out$fig, "Table_1_summary.html"))

# Figure S1: probability of truly being in the top k, by point rank
fig_s1 <- ggplot(athlete_report, aes(age_rank, age_p_topk, colour = age_decision)) +
  geom_vline(xintercept = k_top + 0.5, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = c(settings$p_reject, settings$p_select), linetype = "dotted", colour = "grey60") +
  geom_point(alpha = 0.8) +
  scale_colour_manual(values = decision_colours, labels = decision_labels, breaks = names(decision_labels)) +
  labs(title = "Selection is uncertain only around the cut-off",
       x = "Age-relative rank (1 = best)", y = sprintf("P(truly in top %d)", k_top), colour = "Decision") +
  theme_paper
save_figure(fig_s1, "Figure_S1_selection_probability", 6.5, 4)

# Figure S2: current vs age-relative rank
fig_s2 <- ggplot(athlete_report, aes(current_rank, age_rank, colour = age_latest)) +
  geom_abline(linetype = "dashed", colour = "grey60") +
  geom_point() +
  scale_colour_viridis_c() +
  labs(title = "Age adjustment re-orders the ranking",
       x = "Current rank", y = "Age-relative rank", colour = "Age (years)") +
  theme_paper + theme(legend.position = "right")
save_figure(fig_s2, "Figure_S2_current_vs_age_rank", 6.5, 5)

# Figure S3: benchmark of the 15 ranking methods
fig_s3 <- benchmark |>
  pivot_longer(c(`A: simulation`, `B: held-out visit`), names_to = "test", values_to = "mean_rank") |>
  ggplot(aes(mean_rank, reorder(ranker_label(ranker), -overall_mean_rank), colour = test)) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("#0072B2", "#D55E00")) +
  labs(title = "Benchmark of 15 ranking methods",
       x = "Mean rank across checks (lower = better)", y = NULL, colour = NULL) +
  theme_paper
save_figure(fig_s3, "Figure_S3_benchmark", 6.5, 5)

# Figure S4: longitudinal model vs latest single visit
fig_s4 <- test_b |>
  pivot_longer(c(longitudinal, latest_visit_only), names_to = "input", values_to = "spearman") |>
  mutate(input = recode(input, longitudinal = "All visits (longitudinal model)",
                        latest_visit_only = "Latest visit only")) |>
  ggplot(aes(spearman, input, fill = input)) +
  geom_boxplot(show.legend = FALSE, width = 0.5) +
  facet_wrap(~ version) +
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  labs(title = "Predicting the held-out visit: all visits vs latest visit",
       x = "Spearman correlation with the held-out ranking (15 methods)", y = NULL) +
  theme_paper
save_figure(fig_s4, "Figure_S4_longitudinal_vs_single_visit", 6.5, 3.5)

# Figure S5: first-visit score of retested vs not retested athletes (exploratory)
fig_s5 <- retention |>
  mutate(retested = if_else(retested, "Retested later", "Not retested")) |>
  ggplot(aes(first_visit_score, retested, fill = retested)) +
  geom_boxplot(show.legend = FALSE, width = 0.5) +
  facet_wrap(~ version, scales = "free_x") +
  scale_fill_manual(values = c("#9E9E9E", "#0072B2")) +
  labs(title = "First-visit score: retested vs not retested athletes",
       subtitle = sprintf("Athletes with >= %d days of possible follow-up; age-relative AUC = %.2f",
                          settings$min_followup_days, ret_age$auc),
       x = "First-visit score (winning method)", y = NULL) +
  theme_paper
save_figure(fig_s5, "Figure_S5_retention_check", 6.5, 3.5)

# Excel workbook: key tables, one sheet each
sheets <- list(
  headline = headline, table_1 = rename(table1, "Wrong selections (\u2265)" = wrong),
  qc_summary = read_csv(file.path(out$qc, "qc_summary.csv"), show_col_types = FALSE),
  kpi_candidates = candidates, selected_kpis = kpis,
  benchmark = benchmark, test_b_held_out = test_b, longitudinal_gain = longitudinal_gain,
  selection_by_k = selection, athlete_rankings = athlete_report, example_pair = example,
  retention = retention_summary)
readme <- tibble(sheet = names(sheets),
                 content = c("Numbers for the abstract", "Table 1 of the abstract: decisions at 10/20/30% cut-offs",
                             "Attempts removed by quality control, per test",
                             "Every candidate metric and why it was kept or excluded",
                             "Final KPIs with direction and reliability (ICC)",
                             "Mean rank of each of the 15 ranking methods (Tests A and B)",
                             "Held-out visit results per method", "Longitudinal model vs latest visit only",
                             "Select / retest / don't select and wrong selections for k = 10%, 20%, 30%",
                             "Per-athlete ranks, 95% ranges, P(top k), decision group, hidden talent",
                             "P(player #7 ranks above #12)",
                             "Exploratory: first-visit score of retested vs not retested athletes"))
write_xlsx(c(list(README = readme), sheets), here("results", "athlete_ranking_results.xlsx"))

writeLines(capture.output(sessionInfo()), here("results", "session_info.txt"))
message("Done.")
