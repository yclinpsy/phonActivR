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
#'       is preserved across all parameter combinations}
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

  # Check robustness: does the ordering of delta curves hold everywhere?
  robust <- TRUE
  for (i in seq_len(nrow(grid))) {
    sub <- summary_df[summary_df$grid_row == i, ]
    sub <- sub[order(sub$delta), ]
    if (!all(diff(sub$peak_asym) >= -1e-10)) {
      robust <- FALSE
      break
    }
  }

  if (verbose) {
    cli::cli_h1("Sensitivity Analysis Complete")
    if (robust) {
      cli::cli_alert_success(
        "Asymmetry gradient ordering is PRESERVED across all {nrow(grid)} parameter combinations."
      )
    } else {
      cli::cli_alert_warning(
        "Asymmetry gradient ordering is NOT preserved in all combinations."
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


#' Cohort and Rhyme Benchmark (Qualitative TRACE Sanity Check)
#'
#' Verifies that phonActivR reproduces two core properties of TRACE-style
#' interactive activation: (1) the cohort effect---words sharing onset segments
#' produce stronger competition than unrelated words; and (2) the rhyme
#' effect---onset competitors produce stronger competition than rhyme
#' competitors, as expected from the left-to-right processing priority in
#' TRACE. Also verifies the delta gating logic: at very high delta,
#' sub-moraic competitor activation should be near zero until the gating
#' period ends.
#'
#' @param delta_logic_value Integer. A high delta value to test gating
#'   behaviour (default: 50).
#' @param verbose Logical. Print results (default: TRUE).
#'
#' @return A list of class \code{"phonActivR_benchmark"} containing:
#'   \describe{
#'     \item{cohort_gt_unrelated}{Logical. Cohort competition > unrelated?}
#'     \item{cohort_gt_rhyme}{Logical. Cohort competition > rhyme competition?}
#'     \item{delta_gating_correct}{Logical. At high delta, sub-moraic
#'       activation = 0 until the gating period ends?}
#'     \item{all_pass}{Logical. All three checks passed?}
#'     \item{details}{Data frame with numerical results}
#'   }
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

  cohort_gt_unrelated <- peak_cohort > peak_unrelated
  cohort_gt_rhyme     <- peak_cohort > peak_rhyme

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

  all_pass <- cohort_gt_unrelated && cohort_gt_rhyme && delta_gating_correct

  details <- data.frame(
    check = c("Cohort > Unrelated", "Cohort > Rhyme", "Delta gating correct"),
    result = c(cohort_gt_unrelated, cohort_gt_rhyme, delta_gating_correct),
    value1 = c(round(peak_cohort, 4), round(peak_cohort, 4),
               round(max(pre_gate), 6)),
    value2 = c(round(peak_unrelated, 4), round(peak_rhyme, 4),
               delta_logic_value),
    stringsAsFactors = FALSE
  )
  names(details)[3:4] <- c("value_a", "value_b")

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
    cat("3. Delta gating logic (delta =", delta_logic_value, "):\n")
    cat("   Max activation before gate:", round(max(pre_gate), 6),
        "->", ifelse(delta_gating_correct, "PASS", "FAIL"), "\n\n")
    cat("Overall:", ifelse(all_pass, "ALL CHECKS PASSED", "SOME CHECKS FAILED"), "\n")
  }

  invisible(structure(
    list(
      cohort_gt_unrelated  = cohort_gt_unrelated,
      cohort_gt_rhyme      = cohort_gt_rhyme,
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
#' the best-fitting prosodic constraint hypothesis.
#'
#' @param sim A \code{"phonActivR_sim"} object.
#' @param empirical_data A data.frame with columns \code{time} and
#'   \code{asymmetry} (a single group's empirical CV-C asymmetry curve).
#'
#' @return A data.frame with columns: delta, rss, n_params, aic, delta_aic
#'   (difference from best model).
#' @export
#' @examples
#' stim <- example_stimuli_jp()
#' sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30), verbose = FALSE)
#' emp  <- example_empirical(sim, true_delta = 15)
#' exp_grp <- emp[emp$group == unique(emp$group)[1], ]
#' gof  <- goodness_of_fit(sim, exp_grp)
goodness_of_fit <- function(sim, empirical_data) {

  stopifnot(inherits(sim, "phonActivR_sim"))
  stopifnot(is.data.frame(empirical_data))
  stopifnot(all(c("time", "asymmetry") %in% names(empirical_data)))

  delta_vals <- sim$params$delta_values
  n <- nrow(empirical_data)

  rows <- list()
  for (d in delta_vals) {
    sim_asym <- sim$results[[as.character(d)]]$asymmetry
    # Normalize empirical to simulation scale
    sim_max <- max(sim_asym, na.rm = TRUE)
    emp_max <- max(abs(empirical_data$asymmetry), na.rm = TRUE)
    scale_f <- if (emp_max > 0) sim_max / emp_max else 1
    emp_scaled <- empirical_data$asymmetry * scale_f

    rss <- sum((emp_scaled - sim_asym)^2)
    # k = 1 for delta=0 (baseline), k = 2 for delta>0 (adds delta param)
    k <- if (d == 0) 1L else 2L
    aic <- n * log(rss / n) + 2 * k

    rows[[length(rows) + 1]] <- data.frame(
      delta    = d,
      rss      = round(rss, 6),
      n_params = k,
      aic      = round(aic, 2),
      stringsAsFactors = FALSE
    )
  }

  result <- dplyr::bind_rows(rows)
  result$delta_aic <- round(result$aic - min(result$aic), 2)

  cat("\n=== phonActivR Goodness-of-Fit ===\n")
  cat("Comparing", length(delta_vals), "models (n =", n, "time points)\n\n")
  print(result, row.names = FALSE)
  cat("\nBest model: delta =", result$delta[which.min(result$aic)],
      "(lowest AIC)\n")
  cat("Models with delta_AIC > 10 have essentially no support.\n")

  invisible(result)
}


#' Compare Grain-Size Profiles Across Two Language Groups
#'
#' Takes two \code{"phonActivR_sim"} objects (e.g., one for Japanese–English
#' bilinguals, one for Chinese–English bilinguals) and produces a side-by-side
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
#' @param title Character. Plot title.
#'
#' @return A list with components:
#'   \describe{
#'     \item{plot}{A patchwork ggplot2 object showing side-by-side asymmetry
#'       gradients with a shared legend}
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

  combined_plot <- p_a + p_b +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = title %||% "Cross-Linguistic Grain-Size Comparison",
      subtitle = paste0(
        label_a, " (", sim_a$stimuli$large_type, " vs ",
        sim_a$stimuli$small_type, ", ",
        length(sim_a$params$delta_values), " \u03b4 values) | ",
        label_b, " (", sim_b$stimuli$large_type, " vs ",
        sim_b$stimuli$small_type, ", ",
        length(sim_b$params$delta_values), " \u03b4 values)"
      ),
      theme = ggplot2::theme(
        plot.title    = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 9.5, color = "gray40")
      )
    )

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
  cat("\nInterpretation: If the two groups show different delta_at_peak values,\n")
  cat("this suggests different L1 prosodic constraint strengths. If they show\n")
  cat("the same peak delta, the prosodic bottleneck may be similar in magnitude\n")
  cat("despite originating from different L1 grain sizes (mora vs. syllable).\n")

  invisible(list(
    plot       = combined_plot,
    comparison = comparison
  ))
}
