# =============================================================================
# phonActivR: Validation & Benchmarking Functions
# =============================================================================

#' Systematic Parameter Sensitivity Analysis
#'
#' Runs the simulation across a grid of activation parameters (alpha, decay,
#' gamma) to verify that the qualitative effect of the delta parameter is
#' robust and not an artifact of specific parameter settings.
#'
#' @param stimuli A \code{"phonActivR_stimuli"} object from
#'   \code{\link{create_stimuli}}.
#' @param alpha_values Numeric vector. Activation growth rates to test
#'   (default: \code{c(0.04, 0.08, 0.12)}).
#' @param decay_values Numeric vector. Passive decay rates to test
#'   (default: \code{c(0.01, 0.02, 0.04)}).
#' @param gamma_values Numeric vector. Lateral inhibition strengths to test
#'   (default: \code{c(0.02, 0.04, 0.06)}).
#' @param delta_values Integer vector. Delta values to test at each parameter
#'   combination (default: \code{c(0, 20, 40)}).
#' @param verbose Logical. Print progress (default: TRUE).
#'
#' @return A list of class \code{"phonActivR_sensitivity"} containing:
#'   \describe{
#'     \item{grid}{Data frame of parameter combinations tested}
#'     \item{results}{List of simulation objects, one per grid row}
#'     \item{summary}{Data frame with peak asymmetry and curve ordering
#'       for each parameter combination and delta value}
#'     \item{robust}{Logical. TRUE if the delta ordering (asymmetry gradient)
#'       is never reversed (non-decreasing) across all parameter
#'       combinations; exact ties under saturating regimes still count as
#'       robust and are reported separately in the console output}
#'   }
#' @export
#' @examples
#' stim <- example_stimuli_jp()
#' sens <- sensitivity_analysis(stim,
#'   alpha_values = c(0.04, 0.08),
#'   decay_values = c(0.01, 0.02),
#'   gamma_values = c(0.04),
#'   delta_values = c(0, 20))
#' sens$robust
sensitivity_analysis <- function(stimuli,
                                  alpha_values = c(0.04, 0.08, 0.12),
                                  decay_values = c(0.01, 0.02, 0.04),
                                  gamma_values = c(0.02, 0.04, 0.06),
                                  delta_values = c(0L, 20L, 40L),
                                  verbose      = TRUE) {

  stopifnot(inherits(stimuli, "phonActivR_stimuli"))

  grid <- expand.grid(
    alpha = alpha_values,
    decay = decay_values,
    gamma = gamma_values,
    stringsAsFactors = FALSE
  )

  if (verbose) {
    cli::cli_h1("Sensitivity Analysis")
    cli::cli_alert_info(
      "Testing {nrow(grid)} parameter combinations x {length(delta_values)} delta values"
    )
  }

  results <- list()
  summary_rows <- list()

  for (i in seq_len(nrow(grid))) {
    if (verbose) {
      cli::cli_alert_info(
        "  [{i}/{nrow(grid)}] alpha={grid$alpha[i]}, decay={grid$decay[i]}, gamma={grid$gamma[i]}"
      )
    }

    sim <- run_simulation(
      stimuli,
      delta_values = delta_values,
      alpha        = grid$alpha[i],
      gamma        = grid$gamma[i],
      decay        = grid$decay[i],
      verbose      = FALSE,
      seed         = 1234L
    )
    results[[i]] <- sim

    # Extract peak asymmetry at each delta
    for (d in delta_values) {
      peak_asym <- max(sim$results[[as.character(d)]]$asymmetry)
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        grid_row   = i,
        alpha      = grid$alpha[i],
        decay      = grid$decay[i],
        gamma      = grid$gamma[i],
        delta      = d,
        peak_asym  = round(peak_asym, 5),
        stringsAsFactors = FALSE
      )
    }
  }

  summary_df <- dplyr::bind_rows(summary_rows)

  # Check robustness: the delta ordering must never reverse (non-decreasing
  # peak asymmetry with increasing delta) in any parameter combination.
  # Exact ties are allowed and counted separately: under saturating regimes
  # (e.g., low alpha and/or high gamma) the peak asymmetry hits a
  # parameter-determined bound and delta leaves it unchanged.
  robust <- TRUE
  n_tied <- 0L
  for (i in seq_len(nrow(grid))) {
    sub <- summary_df[summary_df$grid_row == i, ]
    sub <- sub[order(sub$delta), ]
    if (!all(diff(sub$peak_asym) >= -1e-10)) {
      robust <- FALSE
      break
    }
    if (max(sub$peak_asym) - min(sub$peak_asym) < 1e-9) {
      n_tied <- n_tied + 1L
    }
  }

  if (verbose) {
    cli::cli_h1("Sensitivity Analysis Complete")
    if (robust) {
      cli::cli_alert_success(
        "Asymmetry gradient ordering is never reversed (non-decreasing) across all {nrow(grid)} parameter combinations."
      )
      if (n_tied > 0) {
        cli::cli_alert_info(
          "{n_tied} combination{?s} show{?s/} exact ties (saturated peak asymmetry): adjacent delta models are not discriminable by peak asymmetry there."
        )
      }
    } else {
      cli::cli_alert_warning(
        "Asymmetry gradient ordering REVERSES in at least one combination."
      )
    }
  }

  structure(
    list(
      grid    = grid,
      results = results,
      summary = summary_df,
      robust  = robust
    ),
    class = "phonActivR_sensitivity"
  )
}


#' @export
print.phonActivR_sensitivity <- function(x, ...) {
  cat("phonActivR sensitivity analysis: ",
      nrow(x$grid), " parameter combinations\n", sep = "")
  cat("Robust ordering: ", x$robust, "\n")
  invisible(x)
}


#' Parameter Recovery Test
#'
#' Tests whether the phonActivR estimation workflow can correctly recover a
#' known delta value from synthetic data. Generates synthetic empirical data
#' at a specified \code{true_delta}, then identifies which simulated delta
#' curve minimises the residual sum of squares (RSS).
#'
#' @param sim A \code{"phonActivR_sim"} object from \code{\link{run_simulation}}.
#' @param true_delta Numeric. The "true" delta value to embed in the
#'   synthetic data (default: 15).
#' @param noise_sd Numeric. Noise level for synthetic data (default: 0.01).
#' @param n_replications Integer. Number of synthetic datasets to generate
#'   for testing recovery robustness (default: 100).
#' @param seed Integer. Random seed (default: 1234).
#'
#' @return A list of class \code{"phonActivR_recovery"} containing:
#'   \describe{
#'     \item{true_delta}{The embedded true delta value}
#'     \item{recovery_rate}{Proportion of replications that correctly identified
#'       the nearest delta value}
#'     \item{best_deltas}{Integer vector of best-fitting delta values across
#'       replications}
#'     \item{rss_summary}{Data frame of mean RSS per delta value across
#'       replications}
#'     \item{r_squared}{Mean R-squared between the true generating curve and
#'       the best-fitting simulated curve}
#'   }
#' @export
#' @examples
#' stim <- example_stimuli_jp()
#' sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30), verbose = FALSE)
#' rec  <- parameter_recovery(sim, true_delta = 15, n_replications = 50)
#' rec$recovery_rate
parameter_recovery <- function(sim,
                                true_delta     = 15,
                                noise_sd       = 0.01,
                                n_replications = 100L,
                                seed           = 1234L) {

  stopifnot(inherits(sim, "phonActivR_sim"))
  set.seed(seed)

  delta_vals <- sim$params$delta_values
  ts <- sim$params$time_steps

  # Identify the two nearest deltas for identifying correct recovery
  d_sorted <- sort(delta_vals)
  d_below  <- max(d_sorted[d_sorted <= true_delta])
  d_above  <- min(d_sorted[d_sorted >= true_delta])
  expected <- if (abs(true_delta - d_below) <= abs(true_delta - d_above)) {
    d_below
  } else {
    d_above
  }

  best_deltas <- integer(n_replications)
  rss_all     <- matrix(NA, nrow = n_replications, ncol = length(delta_vals))
  colnames(rss_all) <- as.character(delta_vals)

  for (rep in seq_len(n_replications)) {
    # Generate synthetic data from true_delta
    emp <- example_empirical(sim, true_delta = true_delta,
                              noise_sd = noise_sd,
                              seed = seed + rep)
    # Use only the first group (experimental)
    groups <- unique(emp$group)
    exp_data <- emp[emp$group == groups[1], ]

    # Compute RSS against each delta curve
    for (j in seq_along(delta_vals)) {
      d <- delta_vals[j]
      sim_asym <- sim$results[[as.character(d)]]$asymmetry
      # Normalize empirical to simulation scale
      sim_max <- max(sim_asym)
      emp_max <- max(abs(exp_data$asymmetry))
      scale_f <- if (emp_max > 0) sim_max / emp_max else 1
      emp_scaled <- exp_data$asymmetry * scale_f
      rss_all[rep, j] <- sum((emp_scaled - sim_asym)^2)
    }
    best_deltas[rep] <- delta_vals[which.min(rss_all[rep, ])]
  }

  recovery_rate <- mean(best_deltas == expected)

  # Mean RSS summary
  rss_summary <- data.frame(
    delta    = delta_vals,
    mean_rss = round(colMeans(rss_all), 6),
    sd_rss   = round(apply(rss_all, 2, sd), 6)
  )

  # R-squared for the best-fitting curve vs true generating curve
  # (using the mean across replications)
  true_curve <- sim$results[[as.character(expected)]]$asymmetry
  r_sq_vals  <- numeric(n_replications)
  for (rep in seq_len(n_replications)) {
    emp <- example_empirical(sim, true_delta = true_delta,
                              noise_sd = noise_sd, seed = seed + rep)
    exp_data <- emp[emp$group == unique(emp$group)[1], ]
    sim_max <- max(true_curve)
    emp_max <- max(abs(exp_data$asymmetry))
    scale_f <- if (emp_max > 0) sim_max / emp_max else 1
    emp_scaled <- exp_data$asymmetry * scale_f
    ss_res <- sum((emp_scaled - true_curve)^2)
    ss_tot <- sum((emp_scaled - mean(emp_scaled))^2)
    r_sq_vals[rep] <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  }

  result <- list(
    true_delta    = true_delta,
    expected_best = expected,
    recovery_rate = recovery_rate,
    best_deltas   = best_deltas,
    rss_summary   = rss_summary,
    r_squared     = round(mean(r_sq_vals, na.rm = TRUE), 4)
  )

  cat("\n=== phonActivR Parameter Recovery ===\n")
  cat("True delta:", true_delta, "\n")
  cat("Expected nearest simulated delta:", expected, "\n")
  cat("Recovery rate:", round(recovery_rate * 100, 1), "%",
      "(", sum(best_deltas == expected), "/", n_replications, ")\n")
  cat("Mean R-squared:", result$r_squared, "\n\n")
  cat("RSS by delta value:\n")
  print(rss_summary, row.names = FALSE)

  invisible(structure(result, class = "phonActivR_recovery"))
}


#' Cohort and Rhyme Benchmark (Qualitative Interactive-Activation Checks)
#'
#' Verifies that phonActivR reproduces the core qualitative competition
#' phenomena that define the interactive-activation family and that were
#' established for TRACE (McClelland & Elman, 1986) and confirmed
#' empirically in the visual world paradigm (Allopenna et al., 1998):
#' (1) the \emph{cohort effect} -- words sharing onset segments produce
#' stronger competition than unrelated words; (2) \emph{onset priority} --
#' onset (cohort) competitors produce stronger competition than rhyme
#' competitors, as expected from left-to-right incremental processing;
#' (3) the \emph{rhyme time-course} -- rhyme competition emerges and peaks
#' later than cohort competition, mirroring the late rise of rhyme-competitor
#' fixations in Allopenna et al. (1998); and (4) \emph{target resolution} --
#' the target ends the trial with higher activation than any competitor.
#' Also verifies the delta gating logic: at very high delta, sub-prosodic
#' competitor activation should be exactly zero until the gating period ends.
#'
#' @param delta_logic_value Integer. A high delta value to test gating
#'   behaviour (default: 50).
#' @param verbose Logical. Print results (default: TRUE).
#'
#' @return A list of class \code{"phonActivR_benchmark"} containing:
#'   \describe{
#'     \item{cohort_gt_unrelated}{Logical. Cohort competition > unrelated?}
#'     \item{cohort_gt_rhyme}{Logical. Cohort competition > rhyme competition?}
#'     \item{rhyme_peaks_later}{Logical. Rhyme competition peaks later than
#'       cohort competition?}
#'     \item{target_wins}{Logical. Target activation ends above all
#'       competitors?}
#'     \item{delta_gating_correct}{Logical. At high delta, sub-prosodic
#'       activation = 0 until the gating period ends?}
#'     \item{all_pass}{Logical. All checks passed?}
#'     \item{details}{Data frame with numerical results}
#'   }
#' @references
#' Allopenna, P. D., Magnuson, J. S., & Tanenhaus, M. K. (1998). Tracking the
#' time course of spoken word recognition using eye movements. \emph{Journal
#' of Memory and Language}, 38(4), 419--439.
#' @export
#' @examples
#' bench <- cohort_rhyme_benchmark()
#' bench$all_pass
cohort_rhyme_benchmark <- function(delta_logic_value = 50L,
                                    verbose = TRUE) {

  # --- Test stimuli: beaker (target), beetle (cohort), speaker (rhyme) ---
  # beaker: /b/ /iy/ /k/ ...

  # beetle: /b/ /iy/ /t/ ...   (shares onset CV = /bi/)
  # speaker: /s/ /p/ /iy/ /k/ ... (shares rhyme /iker/ but not onset)
  # unrelated: /d/ /ao/ /g/      (shares nothing)

  # For this benchmark we use run_activation directly with known overlaps
  ts <- 101L

  # Cohort competitor (shares onset CV): high overlap
  cohort_out <- run_activation(
    overlap_comp  = 0.9,
    overlap_ctrl  = 0.1,
    overlap_onset = 1L,
    time_steps    = ts
  )

  # Rhyme competitor (shares later segments only): moderate overlap, late onset
  rhyme_out <- run_activation(
    overlap_comp  = 0.5,
    overlap_ctrl  = 0.1,
    overlap_onset = 30L,  # rhyme info available later
    time_steps    = ts
  )

  # Unrelated competitor: minimal overlap
  unrelated_out <- run_activation(
    overlap_comp  = 0.15,
    overlap_ctrl  = 0.1,
    overlap_onset = 1L,
    time_steps    = ts
  )

  peak_cohort     <- max(cohort_out$competition_effect)
  peak_rhyme      <- max(rhyme_out$competition_effect)
  peak_unrelated  <- max(unrelated_out$competition_effect)

  # Check 1-2: competition magnitude ordering (cohort > rhyme > unrelated)
  cohort_gt_unrelated <- peak_cohort > peak_unrelated
  cohort_gt_rhyme     <- peak_cohort > peak_rhyme

  # Check 3: rhyme competition should PEAK LATER than cohort competition,
  # mirroring the late rise of rhyme-competitor fixations in Allopenna et
  # al. (1998, Figure 4) and TRACE's left-to-right activation dynamics.
  t_peak_cohort <- which.max(cohort_out$competition_effect)
  t_peak_rhyme  <- which.max(rhyme_out$competition_effect)
  rhyme_peaks_later <- t_peak_rhyme > t_peak_cohort

  # Check 4: lexical selection -- by the end of the trial the target should
  # have out-competed both competitor types (interactive activation resolves
  # toward the best-matching candidate).
  end_t <- nrow(cohort_out)
  target_wins <- (cohort_out$act_target[end_t] > cohort_out$act_comp[end_t]) &&
    (rhyme_out$act_target[end_t] > rhyme_out$act_comp[end_t])

  # --- Delta gating logic test ---
  # At very high delta, sub-moraic competitor should have zero activation
  # until cycle = 1 + delta
  gated_out <- run_activation(
    overlap_comp  = 0.8,
    overlap_ctrl  = 0.1,
    overlap_onset = 1L + delta_logic_value,
    time_steps    = max(ts, delta_logic_value + 20L)
  )
  # Check that competitor activation is exactly 0 before gating period ends
  pre_gate <- gated_out$act_comp[seq_len(delta_logic_value)]
  delta_gating_correct <- all(pre_gate == 0)

  all_pass <- cohort_gt_unrelated && cohort_gt_rhyme && rhyme_peaks_later &&
    target_wins && delta_gating_correct

  details <- data.frame(
    check = c("Cohort > Unrelated", "Cohort > Rhyme", "Rhyme peaks later",
              "Target wins", "Delta gating correct"),
    result = c(cohort_gt_unrelated, cohort_gt_rhyme, rhyme_peaks_later,
               target_wins, delta_gating_correct),
    value_a = c(round(peak_cohort, 4), round(peak_cohort, 4),
                t_peak_rhyme,
                round(cohort_out$act_target[end_t], 4),
                round(max(pre_gate), 6)),
    value_b = c(round(peak_unrelated, 4), round(peak_rhyme, 4),
                t_peak_cohort,
                round(cohort_out$act_comp[end_t], 4),
                delta_logic_value),
    stringsAsFactors = FALSE
  )

  if (verbose) {
    cat("\n=== phonActivR Qualitative Benchmark ===\n\n")
    cat("1. Cohort effect (onset CV competitor > unrelated):\n")
    cat("   Cohort peak =", round(peak_cohort, 4),
        "| Unrelated peak =", round(peak_unrelated, 4),
        "->", ifelse(cohort_gt_unrelated, "PASS", "FAIL"), "\n\n")
    cat("2. Cohort > Rhyme (onset priority):\n")
    cat("   Cohort peak =", round(peak_cohort, 4),
        "| Rhyme peak =", round(peak_rhyme, 4),
        "->", ifelse(cohort_gt_rhyme, "PASS", "FAIL"), "\n\n")
    cat("3. Rhyme peaks later than cohort (Allopenna et al., 1998 pattern):\n")
    cat("   Rhyme peak step =", t_peak_rhyme,
        "| Cohort peak step =", t_peak_cohort,
        "->", ifelse(rhyme_peaks_later, "PASS", "FAIL"), "\n\n")
    cat("4. Target resolution (target ends above competitors):\n")
    cat("   Target end =", round(cohort_out$act_target[end_t], 4),
        "| Cohort competitor end =", round(cohort_out$act_comp[end_t], 4),
        "->", ifelse(target_wins, "PASS", "FAIL"), "\n\n")
    cat("5. Delta gating logic (delta =", delta_logic_value, "):\n")
    cat("   Max activation before gate:", round(max(pre_gate), 6),
        "->", ifelse(delta_gating_correct, "PASS", "FAIL"), "\n\n")
    cat("Overall:", ifelse(all_pass, "ALL CHECKS PASSED", "SOME CHECKS FAILED"), "\n")
  }

  invisible(structure(
    list(
      cohort_gt_unrelated  = cohort_gt_unrelated,
      cohort_gt_rhyme      = cohort_gt_rhyme,
      rhyme_peaks_later    = rhyme_peaks_later,
      target_wins          = target_wins,
      delta_gating_correct = delta_gating_correct,
      all_pass             = all_pass,
      details              = details
    ),
    class = "phonActivR_benchmark"
  ))
}


#' Empirical Goodness-of-Fit: RSS and AIC Model Comparison
#'
#' Compares the fit of a universalist model (delta = 0) against each non-zero
#' delta model using residual sum of squares (RSS) and the Akaike Information
#' Criterion (AIC). This provides a formal statistical framework for selecting
#' the best-fitting prosodic constraint hypothesis, replacing visual
#' "eyeballing" of the overlay figure with a quantitative model-selection
#' procedure. It is Step 6b of the recommended workflow: run it on the same
#' empirical data passed to \code{\link{overlay_empirical}} (or simply set
#' \code{fit = TRUE} in \code{overlay_empirical()}, which calls this function
#' internally).
#'
#' @section Analysis window:
#' The delta estimate can depend on the analysis window the researcher chose
#' when extracting the empirical curve (e.g., eye-tracking analyses often
#' start 200 ms after word onset, whereas mouse-tracking includes motor
#' planning time). Use \code{time_window} to restrict the fit to a sub-window
#' and check that the selected delta is robust to reasonable window choices.
#' Because window conventions differ across paradigms, best-fitting delta
#' values are most safely compared \emph{within} a paradigm; see the tutorial
#' section "Analysis Windows and Cross-Paradigm Comparability".
#'
#' @param sim A \code{"phonActivR_sim"} object.
#' @param empirical_data A data.frame with columns \code{time} and
#'   \code{asymmetry}. If a \code{group} column is present, the model
#'   comparison is run separately for each group.
#' @param time_window Optional numeric vector of length 2,
#'   \code{c(first, last)}, giving the range of normalized time steps over
#'   which to compute the fit (default: the full curve).
#'
#' @return A data.frame with columns: (\code{group},) \code{delta},
#'   \code{rss}, \code{n_params}, \code{aic}, \code{delta_aic} (difference
#'   from the best model within that group). The best-fitting delta per group
#'   is attached as the \code{"best_delta"} attribute (a named numeric vector).
#' @export
#' @examples
#' stim <- example_stimuli_jp()
#' sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30), verbose = FALSE)
#' emp  <- example_empirical(sim, true_delta = 15)
#' gof  <- goodness_of_fit(sim, emp)          # both groups at once
#' attr(gof, "best_delta")
#' gof_win <- goodness_of_fit(sim, emp, time_window = c(21, 101))  # robustness
goodness_of_fit <- function(sim, empirical_data, time_window = NULL) {

  stopifnot(inherits(sim, "phonActivR_sim"))
  stopifnot(is.data.frame(empirical_data))
  stopifnot(all(c("time", "asymmetry") %in% names(empirical_data)))

  delta_vals <- sim$params$delta_values

  # If no group column is present, treat the input as a single group so the
  # same code path handles both cases.
  if (!"group" %in% names(empirical_data)) {
    empirical_data$group <- "Empirical"
  }
  groups <- unique(empirical_data$group)

  # Validate the optional analysis window
  if (!is.null(time_window)) {
    stopifnot(length(time_window) == 2, time_window[1] < time_window[2])
  }

  all_rows  <- list()
  best_by_g <- stats::setNames(numeric(0), character(0))

  for (g in groups) {
    gd <- empirical_data[empirical_data$group == g, ]
    gd <- gd[order(gd$time), ]

    # Restrict both the empirical curve and the simulated curves to the
    # requested analysis window (if any).
    keep_t <- if (is.null(time_window)) gd$time else
      gd$time[gd$time >= time_window[1] & gd$time <= time_window[2]]
    gd <- gd[gd$time %in% keep_t, ]
    n  <- nrow(gd)

    rows <- list()
    for (d in delta_vals) {
      sim_asym <- sim$results[[as.character(d)]]$asymmetry[gd$time]

      # Normalize the empirical curve to the simulation's activation scale
      # (peak-to-peak), preserving its temporal shape. The same scaling is
      # used by overlay_empirical(), so the visual overlay and the formal
      # fit are directly comparable.
      sim_max <- max(sim_asym, na.rm = TRUE)
      emp_max <- max(abs(gd$asymmetry), na.rm = TRUE)
      scale_f <- if (emp_max > 0) sim_max / emp_max else 1
      emp_scaled <- gd$asymmetry * scale_f

      # Residual sum of squares between the scaled empirical curve and the
      # simulated curve for this delta.
      rss <- sum((emp_scaled - sim_asym)^2)
      # Parameter count for AIC: the universalist model (delta = 0) has
      # k = 1 (scale only); each delta > 0 model adds the delta parameter.
      k <- if (d == 0) 1L else 2L
      aic <- n * log(rss / n) + 2 * k

      rows[[length(rows) + 1]] <- data.frame(
        group    = g,
        delta    = d,
        rss      = round(rss, 6),
        n_params = k,
        aic      = round(aic, 2),
        stringsAsFactors = FALSE
      )
    }

    gres <- dplyr::bind_rows(rows)
    gres$delta_aic <- round(gres$aic - min(gres$aic), 2)
    best_by_g[g] <- gres$delta[which.min(gres$aic)]
    all_rows[[g]] <- gres
  }

  result <- dplyr::bind_rows(all_rows)

  cat("\n=== phonActivR Goodness-of-Fit ===\n")
  if (!is.null(time_window)) {
    cat("Analysis window: time steps", time_window[1], "to", time_window[2], "\n")
  }
  for (g in groups) {
    gres <- result[result$group == g, ]
    cat("\nGroup:", g, "(n =", sum(result$group == g), "models)\n")
    print(gres[, setdiff(names(gres), "group")], row.names = FALSE)
    cat("Best model: delta =", best_by_g[g], "(lowest AIC)\n")
  }
  cat("\nModels with delta_AIC > 10 have essentially no support (Burnham & Anderson).\n")
  cat("Report the best-fitting delta together with the delta_AIC table, and\n")
  cat("check robustness to the analysis window via the time_window argument.\n")

  attr(result, "best_delta") <- best_by_g
  invisible(result)
}


#' Compare Grain-Size Profiles Across Two Language Groups
#'
#' Takes two \code{"phonActivR_sim"} objects (e.g., one for Japanese-English
#' bilinguals, one for Chinese-English bilinguals) and produces a side-by-side
#' comparison of their predicted asymmetry gradients. This enables researchers
#' to ask: do two L1 groups show the same or different prosodic constraint
#' profiles?
#'
#' @param sim_a A \code{"phonActivR_sim"} object for Language Group A.
#' @param sim_b A \code{"phonActivR_sim"} object for Language Group B.
#' @param label_a Character. Label for Group A (default: "Group A").
#' @param label_b Character. Label for Group B (default: "Group B").
#' @param delta_colors Named character vector for delta curve colors.
#'   Auto-generated if \code{NULL}.
#' @param title Character. Optional overall title. If \code{NULL} (the
#'   default) no overall title is drawn; the per-panel group labels remain.
#'
#' @return A list with components:
#'   \describe{
#'     \item{plot}{A patchwork ggplot2 object showing side-by-side asymmetry
#'       gradients; each panel carries its own delta legend when the two
#'       groups are simulated over different delta grids}
#'     \item{comparison}{Data frame comparing peak asymmetry, onset delay,
#'       and best-fitting delta for each group}
#'   }
#' @export
#' @examples
#' stim <- example_stimuli_jp()
#' sim_jp <- run_simulation(stim, delta_values = c(0, 10, 20, 30), verbose = FALSE)
#' sim_zh <- run_simulation(stim, delta_values = c(0, 5, 10, 15, 20), verbose = FALSE)
#' result <- compare_languages(sim_jp, sim_zh,
#'   label_a = "Japanese-English", label_b = "Chinese-English")
#' result$plot
#' result$comparison
compare_languages <- function(sim_a, sim_b,
                               label_a = "Group A",
                               label_b = "Group B",
                               delta_colors = NULL,
                               title = NULL) {

  stopifnot(inherits(sim_a, "phonActivR_sim"))
  stopifnot(inherits(sim_b, "phonActivR_sim"))

  # --- Side-by-side asymmetry plots ---
  p_a <- plot_asymmetry(sim_a, delta_colors = delta_colors,
                         title = label_a)
  p_b <- plot_asymmetry(sim_b, delta_colors = delta_colors,
                         title = label_b)

  # Panel titles (label_a / label_b) identify the two groups and are kept;
  # no overall title/subtitle is baked into the image (that text belongs in
  # the figure caption). A user-supplied title is honored. Each panel keeps
  # its own delta legend because the two groups may use different delta grids.
  combined_plot <- p_a + p_b
  if (!is.null(title)) {
    combined_plot <- combined_plot + patchwork::plot_annotation(
      title = title,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold")
      )
    )
  }

  # --- Summary comparison table ---
  summarise_sim <- function(sim, label) {
    deltas <- sim$params$delta_values
    peak_asyms <- sapply(deltas, function(d) {
      max(sim$results[[as.character(d)]]$asymmetry)
    })
    onset_delays <- sim$onsets$delay

    data.frame(
      group          = label,
      n_items        = sim$stimuli$n_items,
      grain_large    = sim$stimuli$large_type,
      grain_small    = sim$stimuli$small_type,
      delta_range    = paste0(min(deltas), "\u2013", max(deltas)),
      max_peak_asym  = round(max(peak_asyms), 4),
      delta_at_peak  = deltas[which.max(peak_asyms)],
      max_onset_delay = ifelse(all(is.na(onset_delays)), NA,
                                max(onset_delays, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
  }

  comparison <- dplyr::bind_rows(
    summarise_sim(sim_a, label_a),
    summarise_sim(sim_b, label_b)
  )

  cat("\n=== Cross-Linguistic Comparison ===\n\n")
  print(comparison, row.names = FALSE)
  cat("\nInterpretation: Because simulated peak asymmetry increases with delta,\n")
  cat("delta_at_peak necessarily equals the largest delta in each group's grid\n")
  cat("(it is a property of the simulation design, not a finding). To compare\n")
  cat("prosodic constraint strengths across groups, fit each group's EMPIRICAL\n")
  cat("data with goodness_of_fit() and compare the best-fitting delta values.\n")
  cat("\nComparability caveat: delta is expressed in normalized time steps\n")
  cat("(% of trial), and the linguistic unit it indexes differs by group\n")
  cat("(e.g., mora vs. syllable). Numeric delta values are therefore directly\n")
  cat("comparable only when the two groups were tested in the same paradigm\n")
  cat("with comparable trial durations and analysis windows. Across paradigms,\n")
  cat("compare each group's delta AGAINST ITS OWN delta = 0 baseline (i.e.,\n")
  cat("the presence and relative strength of a constraint), not raw values.\n")

  invisible(list(
    plot       = combined_plot,
    comparison = comparison
  ))
}
