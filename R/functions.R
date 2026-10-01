# =============================================================================
# Functions used by ranking.R; section numbers follow ranking.R
# -----------------------------------------------------------------------------
# Convention: X is a matrix (athletes x KPIs) of z-scores in which higher is
# always better (lower-is-better KPIs are sign-flipped first). Every ranking
# method returns a score where higher = better.
# =============================================================================


# ---- Helpers -----------------------------------------------------------------

rescale01   <- function(x) (x - min(x)) / (max(x) - min(x))
to_positive <- function(X) apply(X, 2, \(x) 0.01 + 0.99 * rescale01(x))   # values in [0.01, 1]
rank_best1  <- function(score) rank(-score, ties.method = "average")      # 1 = best
first_match <- function(x, patterns) patterns[which(str_detect(x, patterns))[1]]


# ---- Section 2: trial quality control ----------------------------------------------

# Rules 2 and 3: a value is inconsistent with the athlete's own attempts (same
# test, leg and day) if it is below half or above double their median; with only
# two attempts that differ by more than 2x, both are inconsistent.
inconsistent_attempt <- function(x) {
  n <- length(x)
  if (n == 1) return(FALSE)
  if (n == 2) return(rep(max(x) / min(x) > 2, 2))
  x < 0.5 * median(x) | x > 2 * median(x)
}


# ---- Section 3: redundancy filter ----------------------------------------------------

# Walk through KPIs from most to least reliable; keep a KPI only if |r| with
# every KPI already kept is below the threshold (Dormann et al. 2013)
greedy_select <- function(R, ordered_kpis, threshold) {
  Reduce(\(kept, k) if (all(abs(R[k, kept]) < threshold)) c(kept, k) else kept,
         ordered_kpis[-1], ordered_kpis[1])
}


# ---- Sections 3-4: age-relative scores (LMS method, Cole & Green 1992) -----------------

# Median (M), spread (S) and skewness (L) of a KPI as smooth functions of age
fit_lms <- function(y, age) {
  lms_data <- data.frame(y = y, age = age)
  fit <- gamlss(y ~ pb(age), sigma.formula = ~ pb(age), nu.formula = ~ 1,
                family = BCCGo, data = lms_data, trace = FALSE,
                control = gamlss.control(n.cyc = 200))
  list(fit = fit, data = lms_data)
}

# z-score of a value relative to athletes of the same age (not capped)
lms_z <- function(lms, y, age) {
  p <- predictAll(lms$fit, newdata = data.frame(age = age), data = lms$data)
  qnorm(pBCCGo(y, p$mu, p$sigma, p$nu))
}


# ---- Section 5: longitudinal model ------------------------------------------------------

# One KPI: z ~ time + (1 + time | athlete) + (1 | visit); time = years before the
# athlete's latest visit, so the athlete intercept is the level at the latest visit.
# The random slope is dropped when it cannot be estimated (singular fit).
fit_one_kpi <- function(d) {
  quiet_lmer <- \(f) suppressMessages(suppressWarnings(
    lmer(f, data = d, control = lmerControl(calc.derivs = FALSE))))
  fit <- quiet_lmer(z ~ time + (1 + time | athlete) + (1 | visit))
  slope_kept <- !isSingular(fit)
  if (!slope_kept) fit <- quiet_lmer(z ~ time + (1 | athlete) + (1 | visit))
  athlete_coef <- coef(fit)$athlete
  vc <- as.data.frame(VarCorr(fit))
  sd_of <- \(grp, var) sum(vc$sdcor[vc$grp == grp & vc$var1 %in% var & is.na(vc$var2)])
  list(level      = setNames(athlete_coef[, "(Intercept)"], rownames(athlete_coef)),
       slope_kept = slope_kept,
       varcomp    = tibble(sd_athlete = sd_of("athlete", "(Intercept)"),
                           sd_slope   = sd_of("athlete", "time"),
                           sd_visit   = sd_of("visit", "(Intercept)"),
                           sd_trial   = sd_of("Residual", NA)))
}

# All KPIs: athlete x KPI matrix of levels plus variance components.
# long: one row per trial with athlete, visit, time, kpi, z
fit_levels <- function(long, kpis) {
  fits <- map(kpis, \(k) fit_one_kpi(filter(long, kpi == k))) |> set_names(kpis)
  athletes <- sort(unique(long$athlete))
  list(level   = sapply(fits, \(f) f$level[athletes]) |> `rownames<-`(athletes),
       varcomp = map_dfr(fits, \(f) mutate(f$varcomp, slope_kept = f$slope_kept), .id = "kpi"))
}


# ---- Section 6: weights and ranking methods -------------------------------------

weights_equal <- function(X) rep(1 / ncol(X), ncol(X))

# Entropy weights: information from dispersion
weights_entropy <- function(X) {
  P <- sweep(to_positive(X), 2, colSums(to_positive(X)), "/")
  e <- -colSums(P * log(P)) / log(nrow(X))
  (1 - e) / sum(1 - e)
}

# MEREC weights (Keshavarz-Ghorabaee et al. 2021): a KPI's weight is how much the
# athletes' overall performance measure changes when that KPI is removed
weights_merec <- function(X) {
  P <- to_positive(X)
  L <- abs(log(sweep(1 / P, 2, apply(P, 2, min), "*")))        # |ln(min / x)|
  m <- ncol(X)
  S <- log(1 + rowSums(L) / m)
  E <- sapply(seq_len(m), \(j) sum(abs(log(1 + rowSums(L[, -j, drop = FALSE]) / m) - S)))
  E / sum(E)
}

# Weighted average of z-scores
rank_wavg <- function(X, w) as.vector(X %*% w)

# TOPSIS (Hwang & Yoon 1981): closeness to the ideal athlete
rank_topsis <- function(X, w) {
  P       <- to_positive(X)
  V       <- sweep(sweep(P, 2, sqrt(colSums(P^2)), "/"), 2, w, "*")
  d_best  <- sqrt(rowSums(sweep(V, 2, apply(V, 2, max))^2))
  d_worst <- sqrt(rowSums(sweep(V, 2, apply(V, 2, min))^2))
  d_worst / (d_best + d_worst)
}

# SPOTIS (Dezert et al. 2020): distance to a fixed ideal (+3 z) within fixed
# bounds (+/- 3 z), so adding or removing athletes never reorders the others
rank_spotis <- function(X, w, bound = 3) {
  distance <- abs(pmin(pmax(X, -bound), bound) - bound) / (2 * bound)
  -as.vector(distance %*% w)
}

# MARCOS (Stević et al. 2020): utility relative to the ideal and anti-ideal athlete
rank_marcos <- function(X, w) {
  P       <- to_positive(X)
  ideal   <- apply(P, 2, max)
  anti    <- apply(P, 2, min)
  S       <- as.vector(sweep(P, 2, ideal, "/") %*% w)
  k_minus <- S / sum(anti / ideal * w)
  k_plus  <- S / sum(w)
  f_minus <- k_plus  / (k_plus + k_minus)
  f_plus  <- k_minus / (k_plus + k_minus)
  (k_plus + k_minus) / (1 + (1 - f_plus) / f_plus + (1 - f_minus) / f_minus)
}

# PROMETHEE II (Brans & Vincke 1985), usual preference function: net flow
rank_promethee <- function(X, w) {
  P <- Reduce(`+`, lapply(seq_len(ncol(X)), \(j) w[j] * outer(X[, j], X[, j], ">")))
  (rowSums(P) - colSums(P)) / (nrow(X) - 1)
}

methods <- list(wavg = rank_wavg, topsis = rank_topsis, spotis = rank_spotis,
                marcos = rank_marcos, promethee = rank_promethee)
weights <- list(equal = weights_equal, entropy = weights_entropy, merec = weights_merec)
rankers <- expand_grid(method = names(methods), weight = names(weights)) |>
  mutate(ranker = paste(method, weight, sep = "_"))

# Readable name for tables and figures, e.g. "spotis_equal" -> "SPOTIS, equal weights"
ranker_label <- function(ranker) {
  method <- c(wavg = "Weighted average", topsis = "TOPSIS", spotis = "SPOTIS",
              marcos = "MARCOS", promethee = "PROMETHEE II")
  weight <- c(equal = "equal", entropy = "entropy", merec = "MEREC")
  parts  <- str_split_fixed(ranker, "_", 2)
  paste0(method[parts[, 1]], ", ", weight[parts[, 2]], " weights")
}

# One ranker by name, e.g. "topsis_entropy"
score_one <- function(X, ranker) {
  r <- rankers[rankers$ranker == ranker, ]
  methods[[r$method]](X, weights[[r$weight]](X))
}

# Scores of all 15 rankers: one column per ranker
score_all <- function(X) {
  W <- map(weights, \(f) f(X))
  map2(rankers$method, rankers$weight, \(m, w) methods[[m]](X, W[[w]])) |>
    set_names(rankers$ranker) |>
    as_tibble()
}


# ---- Section 6: benchmark -------------------------------------------------------

# Agreement of an estimated ranking with the true one
rank_metrics <- function(true_score, est_score, k = 10) {
  relevance <- pmax(0, k + 1 - rank_best1(true_score))            # true #1 = k, ..., #k = 1
  dcg  <- sum(relevance[order(-est_score)[1:k]] / log2(2:(k + 1)))
  idcg <- sum(sort(relevance, decreasing = TRUE)[1:k] / log2(2:(k + 1)))
  c(spearman = cor(true_score, est_score, method = "spearman"), ndcg_at_10 = dcg / idcg)
}

# Test A: one simulated cohort with the real visit pattern and known true levels
# (ADEMP; Morris et al. 2019). design: athlete, session, time, n_trials.
# par: variance components per KPI. R: correlation of athlete levels.
simulate_once <- function(design, par, R) {
  athletes <- sort(unique(design$athlete))
  n <- length(athletes); p <- nrow(par)
  true_level <- MASS::mvrnorm(n, rep(0, p), R) %*% diag(par$sd_athlete, p)
  true_slope <- matrix(rnorm(n * p), n) %*% diag(par$sd_slope, p)
  visits <- mutate(design, v = row_number())
  visit_effect <- matrix(rnorm(nrow(visits) * p), ncol = p) %*% diag(par$sd_visit, p)
  trials <- visits |> uncount(n_trials) |> mutate(a = match(athlete, athletes))
  noise  <- matrix(rnorm(nrow(trials) * p), ncol = p) %*% diag(par$sd_trial, p)
  Z <- true_level[trials$a, ] + true_slope[trials$a, ] * trials$time + visit_effect[trials$v, ] + noise
  colnames(Z) <- par$kpi
  long <- bind_cols(select(trials, athlete, session, time), as_tibble(Z)) |>
    mutate(visit = paste(athlete, session)) |>
    pivot_longer(all_of(par$kpi), names_to = "kpi", values_to = "z")

  X_long   <- fit_levels(long, par$kpi)$level[athletes, ]
  X_latest <- long |>
    filter(session == max(session), .by = athlete) |>
    summarise(z = mean(z), .by = c(athlete, kpi)) |>
    pivot_wider(names_from = kpi, values_from = z) |>
    arrange(athlete) |> select(all_of(par$kpi)) |> as.matrix()

  truths <- list(additive       = rowMeans(true_level),
                 weakest_link   = apply(true_level, 1, min),
                 random_weights = as.vector(true_level %*% prop.table(rexp(p))))
  inputs <- list(longitudinal = score_all(X_long), latest_visit_only = score_all(X_latest))
  expand_grid(input = names(inputs), truth = names(truths), ranker = rankers$ranker) |>
    mutate(metrics = pmap(list(input, truth, ranker),
                          \(i, t, r) as_tibble_row(rank_metrics(truths[[t]], inputs[[i]][[r]])))) |>
    unnest(metrics)
}


# ---- Section 7: retention check ----------------------------------------------------

# AUC: probability that a randomly chosen member of the group scores higher than a
# randomly chosen non-member (ties count half)
auc <- function(score, in_group) {
  higher <- outer(score[in_group], score[!in_group], ">")
  ties   <- outer(score[in_group], score[!in_group], "==")
  mean(higher + 0.5 * ties)
}
