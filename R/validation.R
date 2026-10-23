# =============================================================================
# phonActivR: Validation & Benchmarking Functions
# =============================================================================

#' Systematic Parameter Sensitivity Analysis
#'
#' Runs the simulation across a grid of activation parameters (alpha, decay,
#' gamma) and segment durations (\code{seg_steps}) and asks, for every grid
#' cell, how distinguishable each delta model is from the universalist
#' (delta = 0) model. Distinguishability is the root-mean-square difference
#' between the delta curve and the delta = 0 asymmetry curve
#' (\code{divergence}); a delta that leaves the curve unchanged has
#' divergence 0 and cannot be detected in that regime, whatever the data.
#'
#' Peak asymmetry and divergence are not monotone functions of delta: once
#' the gate delays sub-prosodic input past the larger-grain competitor's
#' disambiguation point, the two competitions no longer overlap in time, and
#' a larger delta only moves the sub-prosodic competition later. The analysis
#' therefore asks two questions per grid cell: is every delta > 0
#' distinguishable from delta = 0 (\code{divergence}), and is every delta
#' distinguishable from the next smaller delta in the grid
#' (\code{adjacent_divergence}, the RMS difference between the two curves)?
#' A value below \code{min_rms} counts as not distinguishable.
#'
#' @param stimuli A \code{"phonActivR_stimuli"} object from
#'   \code{\link{create_stimuli}}.
#' @param alpha_values Numeric vector. Activation growth rates to test
#'   (default: \code{c(0.04, 0.08, 0.12)}).
#' @param decay_values Numeric vector. Passive decay rates to test
#'   (default: \code{c(0.01, 0.02, 0.04)}).
#' @param gamma_values Numeric vector. Lateral inhibition strengths to test
#'   (default: \code{c(0.02, 0.04, 0.06)}).
#' @param seg_values Numeric vector. Segment durations (\code{seg_steps}) to
#'   test (default: \code{10}).
#' @param delta_values Integer vector. Delta values to test at each parameter
#'   combination; 0 is added if absent (default: \code{c(0, 10, 20, 30)}).
#' @param min_rms Numeric. RMS difference below which two curves count as
#'   indistinguishable (default 0.01 activation units).
#' @param verbose Logical. Print progress (default: TRUE).
#'
#' @return A list of class \code{"phonActivR_sensitivity"} containing:
#'   \describe{
#'     \item{grid}{Data frame of parameter combinations tested}
#'     \item{results}{List of simulation objects, one per grid row}
#'     \item{summary}{Data frame with peak asymmetry and divergence from the
#'       delta = 0 curve for each combination and delta value}
#'     \item{robust}{Logical. TRUE if, in every combination, every delta > 0
#'       is distinguishable both from delta = 0 and from the adjacent smaller
#'       delta}
#'     \item{n_undetectable}{Number of combinations in which some delta > 0
#'       is within \code{min_rms} of the delta = 0 curve}
#'     \item{n_unresolved}{Number of combinations in which some pair of
#'       adjacent delta values is within \code{min_rms} of each other}
#'   }
#' @export
#' @examples
#' stim <- example_stimuli_jp()
#' sens <- sensitivity_analysis(stim,
#'   alpha_values = c(0.04, 0.08),
#'   decay_values = c(0.02),
#'   gamma_values = c(0.04),
#'   delta_values = c(0, 20))
#' sens$summary
sensitivity_analysis <- function(stimuli,
                                  alpha_values = c(0.04, 0.08, 0.12),
                                  decay_values = c(0.01, 0.02, 0.04),
                                  gamma_values = c(0.02, 0.04, 0.06),
                                  seg_values   = 10,
                                  delta_values = c(0L, 10L, 20L, 30L),
                                  min_rms      = 0.01,
                                  verbose      = TRUE) {

  stopifnot(inherits(stimuli, "phonActivR_stimuli"))
  delta_values <- sort(unique(c(0, delta_values)))

  grid <- expand.grid(
    alpha     = alpha_values,
    decay     = decay_values,
    gamma     = gamma_values,
    seg_steps = seg_values,
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
    sim <- run_simulation(
      stimuli,
      delta_values = delta_values,
      alpha        = grid$alpha[i],
      gamma        = grid$gamma[i],
      decay        = grid$decay[i],
      seg_steps    = grid$seg_steps[i],
      verbose      = FALSE
    )
    results[[i]] <- sim
    base <- sim$results[["0"]]$asymmetry
    prev <- base

    for (d in delta_values) {
      a <- sim$results[[as.character(d)]]$asymmetry
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        grid_row   = i,
        alpha      = grid$alpha[i],
        decay      = grid$decay[i],
        gamma      = grid$gamma[i],
        seg_steps  = grid$seg_steps[i],
        delta      = d,
        peak_asym  = round(max(a), 5),
        divergence = round(sqrt(mean((a - base)^2)), 5),
        adjacent_divergence = round(sqrt(mean((a - prev)^2)), 5),
        stringsAsFactors = FALSE
      )
      prev <- a
    }
  }

  summary_df <- dplyr::bind_rows(summary_rows)

  n_undetectable <- 0L
  n_unresolved   <- 0L
  for (i in seq_len(nrow(grid))) {
    sub <- summary_df[summary_df$grid_row == i & summary_df$delta > 0, ]
    if (any(sub$divergence < min_rms)) n_undetectable <- n_undetectable + 1L
    if (any(sub$adjacent_divergence < min_rms)) n_unresolved <- n_unresolved + 1L
  }
  robust <- n_undetectable == 0L && n_unresolved == 0L

  if (verbose) {
    cli::cli_h1("Sensitivity Analysis Complete")
    cli::cli_alert_info(
      "{nrow(grid) - n_undetectable}/{nrow(grid)} combinations: every delta > 0 differs from delta = 0 by >= {min_rms} RMS.")
    cli::cli_alert_info(
      "{nrow(grid) - n_unresolved}/{nrow(grid)} combinations: every pair of adjacent delta values differs by >= {min_rms} RMS.")
  }

  structure(
    list(
      grid    = grid,
      results = results,
      summary = summary_df,
      robust  = robust,
      n_undetectable = n_undetectable,
      n_unresolved   = n_unresolved,
      min_rms = min_rms
    ),
    class = "phonActivR_sensitivity"
  )
}


#' @export
print.phonActivR_sensitivity <- function(x, ...) {
  cat("phonActivR sensitivity analysis: ",
      nrow(x$grid), " parameter combinations\n", sep = "")
  cat("All delta values distinguishable in every combination: ", x$robust,
      "\n", sep = "")
  cat("Combinations with a delta indistinguishable from delta = 0: ",
      x$n_undetectable, "\n", sep = "")
  cat("Combinations with indistinguishable adjacent delta values: ",
      x$n_unresolved, "\n", sep = "")
  invisible(x)
}


#' Parameter Recovery Test
#'
#' Checks whether the selection procedure of \code{\link{goodness_of_fit}}
#' recovers a known delta. On each replication, data are generated by running
#' the model at \code{true_delta} (via \code{\link{example_empirical}}, which
#' does not interpolate between grid curves) and adding temporally correlated
#' noise; the grid model with the lowest RSS (least-squares scale per model)
#' is then selected.
#'
#' When \code{true_delta} is one of the simulated grid values, success means
#' selecting exactly that value. When it lies between two grid values, the
#' model's curve at \code{true_delta} need not be closer to the numerically
#' nearer grid value (asymmetry curves are not linear in delta), so success
#' is defined as selecting either of the two bracketing grid values, and the
#' split between them is reported.
#'
#' Because the generating process is the model itself, this test establishes
#' only that the selection procedure can distinguish the model's own delta
#' predictions at a given noise level. It does not show that the model is
#' the correct account of real data.
#'
#' @param sim A \code{"phonActivR_sim"} object from \code{\link{run_simulation}}.
#' @param true_delta Numeric. The delta used to generate the data (default: 15).
#' @param noise_sd Numeric. SD of the Gaussian noise before 5-bin smoothing
#'   (default: 0.015, the mouse-tracking default of
#'   \code{\link{example_empirical}}).
#' @param n_replications Integer. Number of synthetic datasets (default: 100).
#' @param seed Integer. Base random seed (default: 1234); replication
#'   \code{r} uses \code{seed + r}, applied locally.
#' @param true_lag Integer. Response latency (time steps) added to the
#'   generated data by shifting the curve later (default 0).
#' @param lag Candidate latencies searched when fitting (default 0). Use a
#'   range (e.g., \code{0:50}) with \code{true_lag > 0} to check recovery
#'   under the latency estimation used for real data.
#' @param method \code{"asymmetry"} (default) fits the asymmetry curve with
#'   \code{\link{goodness_of_fit}}; \code{"components"} fits the CV and C
#'   curves jointly with \code{\link{fit_components}}, the estimator the
#'   tutorial recommends for real data.
#' @param generate_with Named list of \code{run_simulation()} settings used
#'   only to generate the data (e.g., \code{list(residual_input = 1)}),
#'   so that recovery can be checked when the fitted model is misspecified.
#'   Default: none (the fitted model generates the data).
#' @param verbose Logical. Print the summary (default TRUE).
#'
#' @return A list of class \code{"phonActivR_recovery"} containing:
#'   \describe{
#'     \item{true_delta}{The generating delta}
#'     \item{on_grid}{Logical: is \code{true_delta} a simulated grid value?}
#'     \item{bracket}{The grid value (on grid) or the two bracketing grid
#'       values (off grid) that count as correct recovery}
#'     \item{recovery_rate}{Proportion of replications selecting a value in
#'       \code{bracket}}
#'     \item{selection_table}{Counts of each selected delta}
#'     \item{best_deltas}{Selected delta on each replication}
#'     \item{rss_summary}{Mean and SD of RSS per grid delta}
#'   }
#' @export
#' @examples
#' stim <- example_stimuli_jp()
#' sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30), verbose = FALSE)
#' rec  <- parameter_recovery(sim, true_delta = 20, n_replications = 50)
#' rec$recovery_rate
parameter_recovery <- function(sim,
                                true_delta     = 15,
                                noise_sd       = 0.015,
                                n_replications = 100L,
                                seed           = 1234L,
                                true_lag       = 0L,
                                lag            = 0L,
                                method         = c("asymmetry", "components"),
                                generate_with  = list(),
                                verbose        = TRUE) {

  stopifnot(inherits(sim, "phonActivR_sim"))
  method <- match.arg(method)
  delta_vals <- sort(sim$params$delta_values)
  ts <- sim$params$time_steps
  stopifnot(true_delta >= min(delta_vals), true_delta <= max(delta_vals))

  on_grid <- true_delta %in% delta_vals
  bracket <- if (on_grid) true_delta else
    c(max(delta_vals[delta_vals < true_delta]),
      min(delta_vals[delta_vals > true_delta]))

  best_deltas <- numeric(n_replications)
  rss_all <- matrix(NA_real_, n_replications, length(delta_vals),
                    dimnames = list(NULL, as.character(delta_vals)))
  pad <- function(x) c(stats::rnorm(true_lag, 0, noise_sd / sqrt(5)), x)[seq_len(ts)]

  for (rep in seq_len(n_replications)) {
    # Local seed per replication: the global random stream is untouched.
    exp_data <- withr::with_seed(seed + rep, {
      emp <- .empirical_draw(sim, true_delta, noise_sd,
                             c("Generated", "Reference"), "mouse-tracking",
                             generate_with,
                             components = (method == "components"))
      d <- emp[emp$group == "Generated", c("time", "asymmetry", "cv_effect", "c_effect")]
      if (true_lag > 0) {
        d$asymmetry <- pad(d$asymmetry)
        if (method == "components") {
          d$cv_effect <- pad(d$cv_effect); d$c_effect <- pad(d$c_effect)
        }
      }
      d
    })
    fit <- if (method == "asymmetry") {
      suppressWarnings(goodness_of_fit(sim, exp_data[, c("time", "asymmetry")],
                                       lag = lag, verbose = FALSE))
    } else {
      suppressWarnings(fit_components(sim, exp_data, lag = lag, verbose = FALSE))
    }
    rss_all[rep, ] <- fit$rss[match(delta_vals, fit$delta)]
    best_deltas[rep] <- attr(fit, "best_delta")[[1]]
  }

  recovery_rate <- mean(best_deltas %in% bracket)
  sel_tab <- table(factor(best_deltas, levels = delta_vals))

  rss_summary <- data.frame(
    delta    = delta_vals,
    mean_rss = round(colMeans(rss_all), 6),
    sd_rss   = round(apply(rss_all, 2, stats::sd), 6)
  )

  result <- list(
    true_delta      = true_delta,
    on_grid         = on_grid,
    bracket         = bracket,
    noise_sd        = noise_sd,
    method          = method,
    generate_with   = generate_with,
    recovery_rate   = recovery_rate,
    selection_table = sel_tab,
    best_deltas     = best_deltas,
    rss_summary     = rss_summary
  )

  if (verbose) {
    cat("\n=== phonActivR Parameter Recovery ===\n")
    cat("Generating delta:", true_delta,
        if (on_grid) "(on the simulated grid)" else "(between grid values)", "\n")
    if (length(generate_with)) {
      cat("Data generated with:", paste(names(generate_with), unlist(generate_with),
                                        sep = " = ", collapse = ", "),
          "(differs from the fitted model)\n")
    }
    cat("Fitting method:", method, "| Counted as correct:",
        paste(bracket, collapse = " or "), "\n")
    cat("Noise SD:", noise_sd, "| Replications:", n_replications, "\n")
    cat("Recovery rate:", round(recovery_rate * 100, 1), "%\n")
    cat("Selected delta counts:\n"); print(sel_tab)
    cat("\nData are generated by the model itself: this checks the selection\n")
    cat("procedure, not whether the model describes real behavior.\n")
  }

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
#' @section Input timing:
#' The benchmark uses the target \emph{beaker} (/b iy k er/, four segments).
#' Each word receives bottom-up input only while the target segments it
#' matches are unfolding, with \code{seg_steps} time steps per segment: the
#' cohort competitor \emph{beetle} matches segments 1--2, the rhyme
#' competitor \emph{speaker} matches segments 2--4 (partial overlap, because
#' its onset mismatches), and the unrelated word matches none (weak overlap
#' throughout the word). The later start of rhyme input therefore follows
#' from where the shared segments sit in the word, as in TRACE; check (3)
#' confirms that the dynamics turn this position-specific input into the
#' expected later rhyme peak, not that the timing emerges from nothing. The
#' overlap values (0.9, 0.5, 0.15; controls 0.1) are fixed illustrative
#' inputs, not computed from transcriptions.
#'
#' @param delta_logic_value Integer. A high delta value to test gating
#'   behaviour (default: 50).
#' @param seg_steps Numeric. Time steps per segment (default 10).
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
                                    seg_steps = 10,
                                    verbose = TRUE) {

  # --- Test stimuli: beaker (target) /b iy k er/, four segments ---------
  # beetle  /b iy t .../    cohort: matches target segments 1-2
  # speaker /s p iy k er/   rhyme:  matches target segments 2-4 (partially)
  # unrelated               matches no segment (weak overlap)
  # Input windows follow the segment positions (seg_steps per segment).
  ts <- 101L
  seg_on  <- function(k) 1 + (k - 1) * seg_steps   # first step of segment k
  word_end <- seg_on(5)                             # after segment 4

  cohort_out <- run_activation(
    overlap_comp   = 0.9,
    overlap_ctrl   = 0.1,
    overlap_onset  = seg_on(1),
    overlap_offset = seg_on(3),
    time_steps     = ts
  )

  rhyme_out <- run_activation(
    overlap_comp   = 0.5,
    overlap_ctrl   = 0.1,
    overlap_onset  = seg_on(2),
    overlap_offset = word_end,
    time_steps     = ts
  )

  unrelated_out <- run_activation(
    overlap_comp   = 0.15,
    overlap_ctrl   = 0.1,
    overlap_onset  = seg_on(1),
    overlap_offset = word_end,
    time_steps     = ts
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
#' Compares how well each simulated delta curve accounts for an empirical
#' asymmetry curve, using the residual sum of squares (RSS) and the Akaike
#' Information Criterion (AIC). This replaces visual "eyeballing" of the
#' overlay figure with an explicit selection rule. It is Step 6a of the
#' recommended workflow; \code{\link{overlay_empirical}} calls it internally,
#' so the overlay figure and the formal selection always use the same
#' scaling.
#'
#' @section Scaling:
#' Simulated activations and empirical effects are on different scales, so
#' each delta model's curve \eqn{m} is multiplied by the non-negative
#' least-squares factor \eqn{b = \sum e_t m_t / \sum m_t^2} that brings it
#' closest to the empirical curve \eqn{e}; RSS \eqn{= \sum (e_t - b m_t)^2}
#' is then in the data's own units, and \code{r2} is \eqn{1 - RSS / \sum
#' (e_t - \bar{e})^2}. Every model gets its best possible scaling, and none is
#' favored by an arbitrary peak-matching choice. Because every model has the
#' same number of free parameters, AIC orders the models exactly as RSS does.
#'
#' @section Interpreting delta_AIC:
#' The AIC is computed as if the residuals at successive time bins were
#' independent. Time-course residuals are strongly autocorrelated, so the
#' effective number of observations is much smaller than the number of bins
#' and large delta_AIC values overstate the strength of evidence. Treat the
#' delta_AIC column as a ranking of the candidate models, not as a
#' likelihood ratio, and check that the selected delta is stable across
#' reasonable analysis windows (\code{time_window}) and, where participant-
#' level data exist, across bootstrap resamples of participants.
#'
#' @section Analysis window:
#' The delta estimate can depend on the analysis window the researcher chose
#' when extracting the empirical curve (e.g., eye-tracking analyses often
#' start 200 ms after word onset, whereas mouse-tracking includes motor
#' planning time). Use \code{time_window} to restrict the fit to a sub-window.
#' Because the delta models differ mainly early in the trial (the curves
#' converge in the second half), windows that exclude the early portion carry
#' little information about delta. Best-fitting delta values are most safely
#' compared \emph{within} a paradigm; see the tutorial section "Analysis
#' Windows and Cross-Paradigm Comparability".
#'
#' @param sim A \code{"phonActivR_sim"} object.
#' @param empirical_data A data.frame with columns \code{time} and
#'   \code{asymmetry}. If a \code{group} column is present, the model
#'   comparison is run separately for each group.
#' @param time_window Optional numeric vector of length 2,
#'   \code{c(first, last)}, giving the range of normalized time steps over
#'   which to compute the fit (default: the full curve).
#' @param lag Integer vector of candidate response latencies, in time steps
#'   (default \code{0:50}; use \code{0} for no latency). The model's time axis starts at word onset,
#'   whereas an empirical normalized time axis usually also contains
#'   pre-response and motor latency. When \code{lag} has more than one value,
#'   each delta model is shifted right by every candidate lag (padding with
#'   zero activation) and the lag with the lowest RSS is kept for that
#'   model, adding one free parameter (\code{n_params = 2}). Every model gets
#'   the same flexibility, so the comparison between delta values stays fair.
#'   The default range suits time-normalized mouse-tracking; widen it if a
#'   fitted latency lies on its edge (a warning is given).
#' @param verbose Logical. Print the model-comparison table (default TRUE).
#'
#' @return A data.frame with columns: \code{group}, \code{delta},
#'   \code{lag} (the selected latency), \code{scale} (the least-squares
#'   factor applied to the model curve), \code{r2} (proportion of the
#'   empirical curve's variance explained by the scaled, shifted model
#'   curve; an absolute measure of fit that the delta_AIC ranking lacks),
#'   \code{rss}, \code{n_params}, \code{aic}, \code{delta_aic}
#'   (difference from the best model within that group). The best-fitting
#'   delta per group is attached as the \code{"best_delta"} attribute (a
#'   named numeric vector).
#' @export
#' @examples
#' stim <- example_stimuli_jp()
#' sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30), verbose = FALSE)
#' emp  <- example_empirical(sim, true_delta = 15)
#' gof  <- goodness_of_fit(sim, emp)          # both groups at once
#' attr(gof, "best_delta")
#' gof_win <- goodness_of_fit(sim, emp, time_window = c(21, 101))  # robustness
goodness_of_fit <- function(sim, empirical_data, time_window = NULL,
                            lag = 0:50, verbose = TRUE) {

  stopifnot(inherits(sim, "phonActivR_sim"))
  stopifnot(is.data.frame(empirical_data))
  if (!"asymmetry" %in% names(empirical_data) &&
      all(c("cv_effect", "c_effect") %in% names(empirical_data))) {
    empirical_data$asymmetry <- empirical_data$cv_effect - empirical_data$c_effect
  }
  if (!all(c("time", "asymmetry") %in% names(empirical_data))) {
    stop("empirical_data needs a 'time' column and either an 'asymmetry' ",
         "column or both 'cv_effect' and 'c_effect' (asymmetry is then ",
         "computed as cv_effect - c_effect). See ?lin2023_english for the format.",
         call. = FALSE)
  }

  delta_vals <- sim$params$delta_values

  if (!"group" %in% names(empirical_data)) {
    empirical_data$group <- "Empirical"
  }
  groups <- unique(empirical_data$group)

  if (!is.null(time_window)) {
    stopifnot(length(time_window) == 2, time_window[1] < time_window[2])
  }

  all_rows  <- list()
  best_by_g <- stats::setNames(numeric(0), character(0))

  for (g in groups) {
    gd <- empirical_data[empirical_data$group == g, ]
    gd <- gd[order(gd$time), ]
    gd <- gd[!is.na(gd$asymmetry), ]
    .check_time(gd$time, sim$params$time_steps)

    keep_t <- if (is.null(time_window)) gd$time else
      gd$time[gd$time >= time_window[1] & gd$time <= time_window[2]]
    gd <- gd[gd$time %in% keep_t, ]
    n  <- nrow(gd)

    ts   <- sim$params$time_steps
    lags <- sort(unique(as.integer(lag)))
    stopifnot(all(lags >= 0), all(lags < ts))
    k    <- if (length(lags) > 1) 2L else 1L   # scale (+ lag if estimated)

    rows <- list()
    for (d in delta_vals) {
      m_full <- sim$results[[as.character(d)]]$asymmetry
      best <- NULL
      for (L in lags) {
        m_shift  <- c(rep(0, L), m_full)[seq_len(ts)]
        sim_asym <- m_shift[gd$time]
        # Scale the MODEL onto the data (least squares, b >= 0). RSS is in
        # the data's own units, so a flat model gets the worst possible RSS
        # (sum of squared data) rather than a spuriously small one.
        b   <- .ls_scale(sim_asym, gd$asymmetry)
        rss <- sum((gd$asymmetry - b * sim_asym)^2)
        if (is.null(best) || rss < best$rss) {
          e <- gd$asymmetry
          ss_tot <- sum((e - mean(e))^2)
          best <- list(lag = L, scale = b, rss = rss,
                       r2 = if (ss_tot > 0) 1 - rss / ss_tot else NA_real_)
        }
      }
      aic <- n * log(max(best$rss, .Machine$double.eps) / n) + 2 * k

      rows[[length(rows) + 1]] <- data.frame(
        group    = g,
        delta    = d,
        lag      = best$lag,
        scale    = signif(best$scale, 6),
        r2       = round(best$r2, 3),
        rss      = round(best$rss, 6),
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
  .warn_lag_boundary(result$lag, lag)

  if (verbose) {
    cat("\n=== phonActivR Goodness-of-Fit ===\n")
    if (!is.null(time_window)) {
      cat("Analysis window: time steps", time_window[1], "to", time_window[2], "\n")
    }
    for (g in groups) {
      gres <- result[result$group == g, ]
      cat("\nGroup:", g, "\n")
      print(gres[, setdiff(names(gres), "group")], row.names = FALSE)
      cat("Best model: delta =", best_by_g[g], "(lowest AIC = lowest RSS)\n")
    }
    cat("\nEach model is fitted with its own least-squares scale factor")
    if (length(unique(as.integer(lag))) > 1) cat(" and response latency (lag)")
    cat(".\n")
    cat("Time bins are autocorrelated, so treat delta_AIC as a ranking of the\n")
    cat("candidate models rather than a likelihood ratio, and check that the\n")
    cat("selected delta is stable across analysis windows (time_window).\n")
    cat("Before interpreting delta, fit the CV and C components jointly\n")
    cat("(fit_components()) and compare R^2 with shape_baseline().\n")
  }

  attr(result, "best_delta") <- best_by_g
  invisible(result)
}

# Internal: check that empirical time points are integer model steps.
.check_time <- function(tt, ts) {
  if (!is.numeric(tt) || any(is.na(tt)) || any(tt != round(tt)) ||
      any(tt < 1) || any(tt > ts)) {
    stop("empirical_data$time must hold integer normalized time steps from 1 to ",
         ts, " (the model's time_steps). Convert time in ms or time coded ",
         "0-100 first, e.g., resample each curve to 101 steps with ",
         "approx(time_ms, effect, n = 101) and set time = 1:101.",
         call. = FALSE)
  }
  if (anyDuplicated(tt)) stop("empirical_data$time has duplicated values within a group.",
                              call. = FALSE)
  invisible(TRUE)
}

# Non-negative least-squares scale b minimizing sum((y - b * x)^2).
.ls_scale <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  den <- sum(x[ok]^2)
  if (den <= 0) return(0)
  max(0, sum(x[ok] * y[ok]) / den)
}


#' Compare Two Simulations Side by Side
#'
#' Takes two \code{"phonActivR_sim"} objects and produces a side-by-side
#' comparison of their predicted asymmetry gradients. The two objects can be
#' two stimulus sets (e.g., the English and Mandarin sets) or two model
#' variants on the same items (e.g., the two gate modes). The comparison
#' describes the simulated grids; conclusions about listeners come from
#' fitting each group's data with \code{\link{fit_components}}.
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
#'     \item{comparison}{Data frame describing each simulated grid (not
#'       listeners): peak asymmetry and the delta at which it occurs, the
#'       largest C-onset delay, the gate mode, the peak C effect at the
#'       largest delta, and the delta values at which no C competition
#'       crosses the onset threshold. Fit each group's empirical data with
#'       \code{\link{fit_components}} to obtain a best-fitting delta. The
#'       function can also contrast two model variants (e.g.,
#'       \code{gate = "delay"} versus \code{"truncate"}) on the same items.}
#'   }
#' @export
#' @examples
#' stim <- example_stimuli()
#' sim_delay <- run_simulation(stim, delta_values = c(0, 10, 20, 30), verbose = FALSE)
#' sim_trunc <- run_simulation(stim, delta_values = c(0, 10, 20, 30),
#'                             gate = "truncate", verbose = FALSE)
#' result <- compare_languages(sim_delay, sim_trunc,
#'   label_a = "Buffered delay", label_b = "Signal-locked truncation")
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
      gate           = if (is.null(sim$params$gate)) "delay" else sim$params$gate,
      c_peak_at_max_delta = round(max(sim$results[[as.character(max(deltas))]]$small_effect), 4),
      deltas_without_c = paste(deltas[is.na(onset_delays)], collapse = ","),
      stringsAsFactors = FALSE
    )
  }

  comparison <- dplyr::bind_rows(
    summarise_sim(sim_a, label_a),
    summarise_sim(sim_b, label_b)
  )

  cat("\n=== Comparison of Two Simulations ===\n\n")
  print(comparison, row.names = FALSE)
  cat("\nInterpretation: max_peak_asym and delta_at_peak describe the simulated\n")
  cat("grids, not the listeners. They show where in each grid the predicted\n")
  cat("asymmetry is largest. To compare prosodic constraint strengths across\n")
  cat("groups, fit each group's EMPIRICAL CV and C curves with fit_components()\n")
  cat("and compare the selected delta values, with shape_baseline() and the\n")
  cat("window, latency, grid, and seg_steps checks (and, where possible, a\n")
  cat("participant bootstrap).\n")
  cat("\nComparability caveat: delta is expressed in normalized time steps\n")
  cat("(% of trial), and the linguistic unit it indexes differs by group\n")
  cat("(e.g., mora vs. syllable). Numeric delta values are therefore directly\n")
  cat("comparable only when the two groups were tested in the same paradigm\n")
  cat("with comparable response times and analysis windows (in time-normalized\n")
  cat("data, a step is a fixed fraction of each trial). Across paradigms,\n")
  cat("compare each group's delta AGAINST ITS OWN delta = 0 baseline (i.e.,\n")
  cat("the presence and relative strength of a constraint), not raw values.\n")

  invisible(list(
    plot       = combined_plot,
    comparison = comparison
  ))
}


# Warn when an estimated latency sits on the edge of the search range: the
# best latency may lie outside it, and every other fitted quantity is then
# conditional on an arbitrary bound.
.warn_lag_boundary <- function(fitted_lags, lag_range) {
  lr <- sort(unique(as.integer(lag_range)))
  if (length(lr) < 2) return(invisible(NULL))
  # A fitted latency of 0 is a natural lower bound (no negative latency),
  # so the lower edge counts only when the range starts above 0.
  at_edge <- fitted_lags == max(lr) | (fitted_lags == min(lr) & min(lr) > 0)
  if (any(at_edge)) {
    warning("Estimated latency lies on the edge of the search range (",
            min(lr), "-", max(lr), ") for ", sum(at_edge), " model(s); ",
            "widen the 'lag' range before interpreting the fit.",
            call. = FALSE)
  }
  invisible(NULL)
}


#' Fit Both Competition Components (CV and C) to Empirical Data
#'
#' \code{\link{goodness_of_fit}} compares models on the CV-minus-C asymmetry
#' curve. A good fit to that single curve can hide a poor fit to one of its
#' components, and the delta parameter acts only on the smaller-grain (C)
#' component. \code{fit_components()} therefore fits the model's larger-grain
#' and smaller-grain competition curves jointly to the empirical CV and C
#' effect curves, with one non-negative least-squares scale and one response
#' latency shared by both components (so their relative size and timing are
#' constrained by the model), and reports the fit of each component
#' separately.
#'
#' @param sim A \code{"phonActivR_sim"} object.
#' @param empirical_data A data.frame with columns \code{time},
#'   \code{cv_effect}, and \code{c_effect} (and optionally \code{group}),
#'   e.g., \code{\link{lin2023_english}}.
#' @param lag Integer vector of candidate response latencies in time steps
#'   (default \code{0:50}).
#' @param time_window Optional length-2 numeric vector restricting the fit.
#' @param shared_scale Logical. \code{TRUE} (default) fits one scale to both
#'   components, so the model's own CV/C amplitude ratio is tested;
#'   \code{FALSE} gives each component its own scale (one more parameter),
#'   so \code{r2_c} then measures the shape of the C curve alone.
#' @param verbose Logical. Print the table (default TRUE). Independently of
#'   \code{verbose}, a group whose C component is poorly fitted at the
#'   selected delta (\code{r2_c} below 0.5) raises an R warning, and the
#'   returned table carries a logical \code{"c_fit_ok"} attribute (one
#'   value per group), because delta should not be interpreted from such a
#'   fit.
#'
#' @return A data.frame with one row per group and delta: \code{lag},
#'   \code{scale} and \code{scale_c} (the CV and C scale factors; equal when
#'   \code{shared_scale = TRUE}), \code{r2} (both components jointly), \code{r2_cv},
#'   \code{r2_c} (each component separately), \code{rss}, \code{n_params}, \code{aic}, and
#'   \code{delta_aic}. The best delta per group (lowest joint RSS) is
#'   attached as the \code{"best_delta"} attribute. A low \code{r2_c} means
#'   the model does not describe the smaller-grain competition, and any
#'   conclusion about delta is then unsupported.
#' @export
#' @examples
#' sim <- run_simulation(example_stimuli(), delta_values = c(0, 10, 20),
#'                       verbose = FALSE)
#' fit_components(sim, lin2023_english)
fit_components <- function(sim, empirical_data, lag = 0:50,
                           time_window = NULL, shared_scale = TRUE,
                           verbose = TRUE) {
  stopifnot(inherits(sim, "phonActivR_sim"))
  stopifnot(all(c("time", "cv_effect", "c_effect") %in% names(empirical_data)))
  if (!"group" %in% names(empirical_data)) empirical_data$group <- "Empirical"
  ts   <- sim$params$time_steps
  lags <- sort(unique(as.integer(lag)))
  stopifnot(all(lags >= 0), all(lags < ts))
  k <- if (length(lags) > 1) 2L else 1L
  r2_of <- function(y, yhat) {
    sst <- sum((y - mean(y))^2)
    if (sst > 0) 1 - sum((y - yhat)^2) / sst else NA_real_
  }

  rows <- list(); best_by_g <- c()
  for (g in unique(empirical_data$group)) {
    gd <- empirical_data[empirical_data$group == g, ]
    gd <- gd[order(gd$time), ]
    .check_time(gd$time, ts)
    if (!is.null(time_window)) {
      gd <- gd[gd$time >= time_window[1] & gd$time <= time_window[2], ]
    }
    y <- c(gd$cv_effect, gd$c_effect); n <- length(y)
    k_g <- k + if (shared_scale) 0L else 1L
    grows <- list()
    for (d in sim$params$delta_values) {
      res <- sim$results[[as.character(d)]]
      best <- NULL
      for (L in lags) {
        mcv <- c(rep(0, L), res$large_effect)[seq_len(ts)][gd$time]
        mc  <- c(rep(0, L), res$small_effect)[seq_len(ts)][gd$time]
        m   <- c(mcv, mc)
        if (shared_scale) {
          b_cv <- b_c <- .ls_scale(m, y)
        } else {
          b_cv <- .ls_scale(mcv, gd$cv_effect); b_c <- .ls_scale(mc, gd$c_effect)
        }
        pred <- c(b_cv * mcv, b_c * mc)
        rss <- sum((y - pred)^2)
        if (is.null(best) || rss < best$rss) {
          best <- list(lag = L, scale = b_cv, scale_c = b_c, rss = rss,
                       r2 = r2_of(y, pred),
                       r2_cv = r2_of(gd$cv_effect, b_cv * mcv),
                       r2_c  = r2_of(gd$c_effect, b_c * mc))
        }
      }
      grows[[length(grows) + 1]] <- data.frame(
        group = g, delta = d, lag = best$lag, scale = signif(best$scale, 6),
        scale_c = signif(best$scale_c, 6),
        r2 = round(best$r2, 3), r2_cv = round(best$r2_cv, 3),
        r2_c = round(best$r2_c, 3), rss = round(best$rss, 6),
        n_params = k_g,
        aic = round(n * log(max(best$rss, .Machine$double.eps) / n) + 2 * k_g, 2),
        stringsAsFactors = FALSE)
    }
    gr <- dplyr::bind_rows(grows)
    gr$delta_aic <- round(gr$aic - min(gr$aic), 2)
    best_by_g[g] <- gr$delta[which.min(gr$aic)]
    rows[[g]] <- gr
  }
  out <- dplyr::bind_rows(rows)
  .warn_lag_boundary(out$lag, lag)
  if (verbose) {
    cat("\n=== phonActivR Component Fit (CV and C jointly) ===\n")
    for (g in unique(out$group)) {
      cat("\nGroup:", g, "\n")
      print(out[out$group == g, setdiff(names(out), "group")], row.names = FALSE)
    }
    cat("\nr2_c is the fit to the smaller-grain (C) competition, the component\n")
    cat("that delta acts on. If it is low for every delta, the data do not\n")
    cat("support a conclusion about delta. The selected delta is a relative\n")
    cat("timing parameter conditional on the model's input assumptions.\n")
  }
  # A poorly fitted C component (R^2 below .5 at the selected delta) is
  # reported as a real warning, independent of `verbose`, and recorded in
  # the "c_fit_ok" attribute, because delta should not be interpreted from
  # such a fit.
  c_ok <- vapply(unique(out$group), function(g) {
    og <- out[out$group == g, ]; rc <- og$r2_c[which.min(og$aic)]
    is.finite(rc) && rc >= 0.5 }, logical(1))
  for (g in names(c_ok)[!c_ok]) {
    og <- out[out$group == g, ]; rc <- og$r2_c[which.min(og$aic)]
    warning(g, ": the C component is poorly fitted at the selected delta ",
            "(r2_c = ", sprintf("%.2f", rc), "). Vary c_weight and residual_input ",
            "in run_simulation(), and compare shared_scale = FALSE, before ",
            "interpreting delta.", call. = FALSE)
  }
  attr(out, "c_fit_ok") <- c_ok
  attr(out, "best_delta") <- best_by_g
  invisible(out)
}


#' Shape Baseline: Fit a Single Gaussian Bump
#'
#' A model-free reference for judging an R-squared. Many empirical
#' competition curves rise and resolve once, and any hump-shaped curve can
#' then fit them well. \code{shape_baseline()} fits a scaled Gaussian bump
#' with free center and width (three parameters) to each group's curve and
#' reports its R-squared. A model whose R-squared does not exceed this
#' baseline has not shown that its dynamics, rather than the general shape,
#' account for the data.
#'
#' @param empirical_data A data.frame with \code{time} and the column named
#'   by \code{column} (and optionally \code{group}).
#' @param column Character. Which curve to fit (default \code{"asymmetry"}).
#' @param widths Numeric vector of candidate standard deviations, in time
#'   steps (default \code{3:25}).
#' @param time_window Optional length-2 numeric vector restricting the fit.
#'
#' @return A data.frame with one row per group: \code{center},
#'   \code{width}, \code{scale}, \code{r2}, \code{rss}, \code{n_params} (3),
#'   and \code{aic}, computed as in \code{\link{goodness_of_fit}} (whose delta
#'   models have 2 parameters plus the choice of delta), so the two AIC
#'   columns can be compared on the same data and window.
#' @export
#' @examples
#' shape_baseline(lin2023_english)
shape_baseline <- function(empirical_data, column = "asymmetry",
                           widths = 3:25, time_window = NULL) {
  stopifnot(all(c("time", column) %in% names(empirical_data)))
  if (!"group" %in% names(empirical_data)) empirical_data$group <- "Empirical"
  out <- list()
  for (g in unique(empirical_data$group)) {
    gd <- empirical_data[empirical_data$group == g, ]
    if (!is.null(time_window)) {
      gd <- gd[gd$time >= time_window[1] & gd$time <= time_window[2], ]
    }
    y <- gd[[column]]; tt <- gd$time
    sst <- sum((y - mean(y))^2)
    best <- NULL
    for (w in widths) for (mu in seq(min(tt), max(tt))) {
      m <- stats::dnorm(tt, mu, w)
      b <- .ls_scale(m, y)
      rss <- sum((y - b * m)^2)
      if (is.null(best) || rss < best$rss) best <- list(mu = mu, w = w, b = b, rss = rss)
    }
    n <- length(y)
    out[[g]] <- data.frame(group = g, center = best$mu, width = best$w,
                           scale = signif(best$b, 6),
                           r2 = round(1 - best$rss / sst, 3),
                           rss = round(best$rss, 6), n_params = 3L,
                           aic = round(n * log(max(best$rss, .Machine$double.eps) / n) + 6, 2),
                           stringsAsFactors = FALSE)
  }
  dplyr::bind_rows(out)
}


#' Participant Bootstrap of the Selected Delta
#'
#' Resamples participants with replacement, recomputes the group-mean
#' competition effects from participant-level mean trajectories, refits the
#' delta models to each resample, and tabulates the selected delta. This is
#' the check the tutorial recommends whenever participant-level data are
#' available: it shows how stable the selected delta is under participant
#' sampling (items are held fixed).
#'
#' @param sim A \code{"phonActivR_sim"} object (the delta grid to fit).
#' @param subject_curves A data.frame with one row per participant and
#'   condition: columns \code{subject}, \code{condition}, \code{n_trials}
#'   (number of trials behind the mean; used as the weight), the trajectory
#'   columns named in \code{time_cols} (default \code{X_1} to
#'   \code{X_<time_steps>}, the mean x-coordinate at each normalized step),
#'   and optionally \code{group}. The bundled files in
#'   \code{inst/extdata/lin2023_validation/} have this format.
#' @param conditions Named character vector giving the condition labels for
#'   the larger-grain competitor and its control and the smaller-grain
#'   competitor and its control (default
#'   \code{c(cv = "CV", cv_ctrl = "CV Control", c = "Onset", c_ctrl = "Onset Control")}).
#'   Effects are control minus competitor, as in \code{\link{lin2023_english}}.
#' @param n_boot Integer. Number of resamples (default 1000).
#' @param method \code{"components"} (default; \code{\link{fit_components}}),
#'   \code{"asymmetry"} (\code{\link{goodness_of_fit}}), or \code{"both"}.
#' @param lag,time_window,shared_scale Passed to the fitting function.
#' @param seed Integer. Seed applied locally (default 1).
#' @param time_cols Optional character vector naming the trajectory columns.
#' @param verbose Logical. Print a summary (default TRUE).
#'
#' @return An object of class \code{"phonActivR_bootstrap"}: a list with
#'   \code{full} (the delta selected on the full data, per group and
#'   method), \code{draws} (a data.frame with the selected delta in every
#'   resample), and \code{summary} (per group and method, the percentage of
#'   resamples selecting each delta). Printing the object shows the summary.
#' @export
#' @examples
#' \donttest{
#' sc <- read.csv(system.file("extdata", "lin2023_validation",
#'                            "lin2023_subject_curves.csv", package = "phonActivR"),
#'                check.names = FALSE)
#' sim <- run_simulation(example_stimuli(), delta_values = seq(0, 30, 5),
#'                       verbose = FALSE)
#' bootstrap_delta(sim, sc, n_boot = 50)
#' }
bootstrap_delta <- function(sim, subject_curves,
                            conditions = c(cv = "CV", cv_ctrl = "CV Control",
                                           c = "Onset", c_ctrl = "Onset Control"),
                            n_boot = 1000L,
                            method = c("components", "asymmetry", "both"),
                            lag = 0:50, time_window = NULL, shared_scale = TRUE,
                            seed = 1L, time_cols = NULL, verbose = TRUE) {
  stopifnot(inherits(sim, "phonActivR_sim"), is.data.frame(subject_curves))
  method <- match.arg(method)
  methods <- if (method == "both") c("components", "asymmetry") else method
  ts <- sim$params$time_steps
  if (is.null(time_cols)) time_cols <- paste0("X_", seq_len(ts))
  need <- c("subject", "condition", "n_trials", time_cols)
  miss <- setdiff(need, names(subject_curves))
  if (length(miss)) stop("subject_curves lacks column(s): ",
                         paste(utils::head(miss, 5), collapse = ", "), call. = FALSE)
  if (!all(conditions %in% subject_curves$condition))
    stop("conditions not found in subject_curves$condition: ",
         paste(setdiff(conditions, subject_curves$condition), collapse = ", "), call. = FALSE)
  if (!"group" %in% names(subject_curves)) subject_curves$group <- "Empirical"

  eff <- function(tab, w) {
    m <- function(k) { s <- tab$condition == k
      colSums(as.matrix(tab[s, time_cols]) * tab$n_trials[s] * w[s]) /
        sum(tab$n_trials[s] * w[s]) }
    cv <- m(conditions[["cv_ctrl"]]) - m(conditions[["cv"]])
    cc <- m(conditions[["c_ctrl"]]) - m(conditions[["c"]])
    data.frame(time = seq_len(ts), cv_effect = cv, c_effect = cc, asymmetry = cv - cc)
  }
  pick <- function(f) f$delta[which.min(f$aic)]
  fit_one <- function(em, m) {
    f <- if (m == "components")
      suppressWarnings(fit_components(sim, em, lag = lag, time_window = time_window,
                                      shared_scale = shared_scale, verbose = FALSE))
    else suppressWarnings(goodness_of_fit(sim, em, lag = lag, time_window = time_window,
                                          verbose = FALSE))
    pick(f)
  }
  draws <- list(); full <- list()
  for (g in unique(subject_curves$group)) {
    tab <- subject_curves[subject_curves$group == g, ]
    subs <- unique(tab$subject)
    em0 <- eff(tab, rep(1, nrow(tab)))
    for (m in methods) full[[length(full) + 1]] <-
      data.frame(group = g, method = m, delta = fit_one(em0, m))
    res <- withr::with_seed(seed, {
      lapply(seq_len(n_boot), function(b) {
        k <- table(factor(sample(subs, replace = TRUE), subs))
        w <- as.numeric(k[as.character(tab$subject)])
        em <- eff(tab, w)
        data.frame(group = g, boot = b, method = methods,
                   delta = vapply(methods, function(m) fit_one(em, m), numeric(1)))
      })
    })
    draws[[g]] <- do.call(rbind, res)
  }
  draws <- do.call(rbind, draws); rownames(draws) <- NULL
  full <- do.call(rbind, full)
  grid <- sort(sim$params$delta_values)
  summ <- do.call(rbind, lapply(split(draws, list(draws$group, draws$method), drop = TRUE),
    function(d) {
      p <- 100 * prop.table(table(factor(d$delta, grid)))
      data.frame(group = d$group[1], method = d$method[1], delta = grid,
                 percent = as.numeric(p), stringsAsFactors = FALSE) }))
  rownames(summ) <- NULL
  out <- structure(list(full = full, draws = draws, summary = summ,
                        n_boot = n_boot, seed = seed),
                   class = "phonActivR_bootstrap")
  if (verbose) print(out)
  invisible(out)
}

#' @export
print.phonActivR_bootstrap <- function(x, ...) {
  cat("\n=== phonActivR Participant Bootstrap (", x$n_boot, " resamples, seed ",
      x$seed, ") ===\n", sep = "")
  for (g in unique(x$summary$group)) for (m in unique(x$summary$method)) {
    s <- x$summary[x$summary$group == g & x$summary$method == m, ]
    if (!nrow(s)) next
    f <- x$full$delta[x$full$group == g & x$full$method == m]
    cat("\n", g, " | ", m, " fit | full data: delta = ", f, "\n", sep = "")
    v <- stats::setNames(round(s$percent, 1), s$delta); print(v)
  }
  cat("\nPercent of resamples selecting each delta (items held fixed).\n")
  invisible(x)
}
