# =============================================================================
# phonActivR: Synthetic Empirical Data & Utility Functions
# =============================================================================

#' Generate Synthetic Empirical Data for Demonstration
#'
#' Creates a synthetic empirical dataset that mimics the structure of GAMM
#' smooth output from a time-course experiment (mouse-tracking or eye-tracking).
#'
#' @section Primary Demo Path:
#' \code{example_empirical()} is the \strong{recommended starting point} for
#' learning the overlay workflow. It generates realistic synthetic curves that
#' run identically to real empirical data, allowing the complete pipeline to
#' be demonstrated without external dependencies. This is not a fallback
#' "Option A": it is the intended primary demo. Once the workflow is understood,
#' researchers substitute their own \code{data.frame} (containing \code{time},
#' \code{asymmetry}, and optionally \code{se} and \code{group}) with no other
#' changes to the call. See the "Using Your Own Data" section in
#' \code{vignette("phonActivR-tutorial")}.
#'
#' @section Competing-Hypothesis Connection:
#' The \code{true_delta} argument encodes a specific hypothesis about prosodic
#' constraint strength. By generating curves at \code{true_delta = 0}
#' (universalist) and \code{true_delta > 0} (language-specific), researchers
#' can inspect which curve family the empirical data resembles -- a workflow
#' analogous to competing-contrast analysis (Henninger et al., 2025).
#' Specifying \code{true_delta} before data collection is a form of
#' quantitative pre-registration of expected effect structure.
#'
#' @param sim A \code{"phonActivR_sim"} object from \code{\link{run_simulation}}.
#' @param true_delta Numeric. The "true" delta value used as the data-generating
#'   process for the experimental group (default: 15). This simulates what the
#'   empirical data would look like if the actual prosodic constraint strength
#'   were delta = 15.
#' @param noise_sd Numeric. Standard deviation of Gaussian noise added to the
#'   curves to simulate empirical variability (default: 0.015).
#' @param group_labels Character vector of length 2. Labels for the experimental
#'   and control groups (default: \code{c("L2 bilinguals", "L1 monolinguals")}).
#' @param paradigm Character. Label for the experimental paradigm, used in
#'   documentation only (default: "mouse-tracking").
#' @param seed Integer. Random seed for reproducibility (default: 42).
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{time}{Integer. Normalized time step (1 to time_steps)}
#'     \item{asymmetry}{Numeric. Larger-grain minus smaller-grain effect}
#'     \item{se}{Numeric. Standard error estimate}
#'     \item{group}{Character. Group label}
#'   }
#'
#'   This data.frame is ready to pass directly to \code{\link{overlay_empirical}}.
#'
#' @export
#' @examples
#' stim <- example_stimuli_jp()
#' sim  <- run_simulation(stim, verbose = FALSE)
#'
#' # Generate synthetic mouse-tracking data
#' emp_mt <- example_empirical(sim, true_delta = 15,
#'   group_labels = c("JP bilinguals", "EN monolinguals"),
#'   paradigm = "mouse-tracking")
#'
#' # Generate synthetic eye-tracking data
#' emp_et <- example_empirical(sim, true_delta = 12,
#'   group_labels = c("L2 learners", "L1 controls"),
#'   paradigm = "eye-tracking", noise_sd = 0.02)
#'
#' # Use in overlay
#' overlay_empirical(sim, emp_mt)
example_empirical <- function(sim,
                              true_delta = 15,
                              noise_sd = 0.015,
                              group_labels = c("L2 bilinguals", "L1 monolinguals"),
                              paradigm = "mouse-tracking",
                              seed = 42L) {

  stopifnot(inherits(sim, "phonActivR_sim"))
  stopifnot(length(group_labels) == 2)

  set.seed(seed)
  ts <- sim$params$time_steps
  deltas <- sort(sim$params$delta_values)

  # Clamp true_delta to the range of simulated delta values
  true_delta <- max(min(deltas), min(max(deltas), true_delta))

  # Experimental group: interpolate between the two nearest delta curves
  # to simulate a "true" constraint at true_delta
  d_below <- max(deltas[deltas <= true_delta])
  d_above <- min(deltas[deltas >= true_delta])

  if (d_below == d_above) {
    asym_exp <- sim$results[[as.character(d_below)]]$asymmetry
  } else {
    w <- (true_delta - d_below) / (d_above - d_below)
    asym_below <- sim$results[[as.character(d_below)]]$asymmetry
    asym_above <- sim$results[[as.character(d_above)]]$asymmetry
    asym_exp <- (1 - w) * asym_below + w * asym_above
  }

  # Control group: use delta = 0 (no prosodic constraint)
  asym_ctrl <- sim$results[[as.character(deltas[1])]]$asymmetry

  # Add smoothed noise to simulate empirical variability
  smooth_noise <- function(n, sd_val) {
    raw <- stats::rnorm(n, 0, sd_val)
    # Apply a moving average to make noise temporally correlated
    # (as real GAMM smooths would be)
    kernel <- rep(1/5, 5)
    smoothed <- stats::filter(raw, kernel, sides = 2)
    smoothed[is.na(smoothed)] <- raw[is.na(smoothed)]
    as.numeric(smoothed)
  }

  exp_noisy  <- asym_exp  + smooth_noise(ts, noise_sd)
  ctrl_noisy <- asym_ctrl + smooth_noise(ts, noise_sd * 0.8)

  # Simulate SE (decreases with sample size, varies over time)
  se_exp  <- abs(asym_exp)  * 0.25 + noise_sd * 0.5 + abs(smooth_noise(ts, 0.003))
  se_ctrl <- abs(asym_ctrl) * 0.20 + noise_sd * 0.4 + abs(smooth_noise(ts, 0.003))

  data.frame(
    time      = rep(seq_len(ts), 2),
    asymmetry = c(exp_noisy, ctrl_noisy),
    se        = c(se_exp, se_ctrl),
    group     = rep(group_labels, each = ts),
    stringsAsFactors = FALSE
  )
}


#' Export Simulation Results to CSV
#'
#' Exports the full simulation results to CSV files for use in other
#' software or for supplementary materials.
#'
#' @param sim A \code{"phonActivR_sim"} object.
#' @param prefix Character. File name prefix (default: "phonActivR").
#' @param dir Character. Output directory (default: current working directory).
#'
#' @return Invisible character vector of file paths written.
#' @export
#' @examples
#' \dontrun{
#' sim <- run_simulation(example_stimuli_jp())
#' export_results(sim, prefix = "moraic_sim", dir = "output/")
#' }
export_results <- function(sim, prefix = "phonActivR", dir = ".") {

  stopifnot(inherits(sim, "phonActivR_sim"))

  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  # 1. Summary table
  summary_path <- file.path(dir, paste0(prefix, "_summary.csv"))
  utils::write.csv(sim$summary, summary_path, row.names = FALSE)

  # 2. Time-course data (all delta values)
  curves <- dplyr::bind_rows(lapply(sim$params$delta_values, function(d) {
    res <- sim$results[[as.character(d)]]
    data.frame(
      time         = seq_len(sim$params$time_steps),
      delta        = d,
      large_effect = res$large_effect,
      small_effect = res$small_effect,
      large_se     = res$large_se,
      small_se     = res$small_se,
      asymmetry    = res$asymmetry,
      asym_se      = res$asym_se
    )
  }))
  curves_path <- file.path(dir, paste0(prefix, "_curves.csv"))
  utils::write.csv(curves, curves_path, row.names = FALSE)

  # 3. Parameters
  params_path <- file.path(dir, paste0(prefix, "_parameters.csv"))
  params_df <- data.frame(
    parameter = c("time_steps", "alpha", "gamma", "decay",
                  "threshold", "ctrl_scaling", "n_items",
                  "large_type", "small_type", "delta_values"),
    value = c(sim$params$time_steps, sim$params$alpha, sim$params$gamma,
              sim$params$decay, sim$params$threshold, sim$params$ctrl_scaling,
              sim$stimuli$n_items, sim$stimuli$large_type, sim$stimuli$small_type,
              paste(sim$params$delta_values, collapse = ","))
  )
  utils::write.csv(params_df, params_path, row.names = FALSE)

  files <- c(summary_path, curves_path, params_path)
  cli::cli_alert_success("Exported {length(files)} files to {dir}/")
  invisible(files)
}


#' Estimate Minimum Detectable Delta Difference
#'
#' A lightweight power-guidance utility that uses the simulation output to
#' characterise how separable two delta hypotheses are, given a study design.
#' This supports quantitative pre-registration: researchers can report the
#' minimum delta difference their design is powered to detect before collecting
#' data.
#'
#' @section Four-Level Failure Framework:
#' Following You & Magnuson (2018), failures to detect a delta difference can
#' arise at four levels: (1) \emph{theory level} -- the Language-Specific Grain
#' Size hypothesis is wrong; (2) \emph{implementation level} -- phonActivR's
#' interactive activation equations misrepresent the target theory;
#' (3) \emph{parameter level} -- the default alpha/gamma/decay values do not
#' apply to the population studied; or (4) \emph{linking-hypothesis level} --
#' the empirical paradigm does not map cleanly onto the competition-effect
#' quantity. Use the sensitivity analysis workflow (Application 4 in the
#' tutorial) to probe levels 3 and 4 before concluding level 1 or 2.
#'
#' @param sim A \code{"phonActivR_sim"} object from \code{\link{run_simulation}}.
#' @param n_items Integer. Number of stimulus items per condition (default: 42).
#' @param n_participants Integer. Number of participants per group (default: 24).
#' @param effect_reliability Numeric in (0,1). Estimated test-retest reliability
#'   of the empirical asymmetry measure (default: 0.70, a conservative estimate
#'   for GAMM-based mouse-tracking data). Higher reliability increases power.
#'
#' @return A data.frame with one row per pair of adjacent delta values,
#'   showing the peak asymmetry difference, approximate standardised effect
#'   size (Cohen's d analogue), and a qualitative power rating
#'   ("likely sufficient", "marginal", or "likely insufficient") given the
#'   supplied design parameters.
#'
#' @references
#' You, H., & Magnuson, J. S. (2018). TISK 1.0: An easy-to-use Python
#' implementation of the time-invariant string kernel model of spoken word
#' recognition. \emph{Behavior Research Methods}, 50(2), 871--889.
#'
#' Henninger, F., Malejka, S., & Titz, J. (2025). Contrast analysis for
#' competing hypotheses: cofad. \emph{Behavior Research Methods}, 57, 326.
#'
#' @export
#' @examples
#' stim <- example_stimuli_jp()
#' sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30), verbose = FALSE)
#' power_guidance(sim, n_items = 42, n_participants = 24)
power_guidance <- function(sim,
                           n_items          = 42L,
                           n_participants   = 24L,
                           effect_reliability = 0.70) {

  stopifnot(inherits(sim, "phonActivR_sim"))

  delta_vals <- sim$params$delta_values
  if (length(delta_vals) < 2) {
    stop("power_guidance() requires at least 2 delta values in the simulation.")
  }

  rows <- list()
  for (i in seq_len(length(delta_vals) - 1)) {
    d1  <- delta_vals[i]
    d2  <- delta_vals[i + 1]
    r1  <- sim$results[[as.character(d1)]]
    r2  <- sim$results[[as.character(d2)]]

    peak_diff <- max(r2$asymmetry) - max(r1$asymmetry)
    # Approximate SD from SE at peak time of larger delta
    peak_t   <- which.max(r2$asymmetry)
    se_at_peak <- mean(c(r1$asym_se[peak_t], r2$asym_se[peak_t]))
    approx_sd   <- se_at_peak * sqrt(n_items)

    # Cohen's d analogue, corrected for reliability
    d_approx <- if (approx_sd > 0) {
      (peak_diff / approx_sd) * sqrt(effect_reliability)
    } else NA_real_

    # Qualitative power rating (heuristic for GAMM time-course)
    power_rating <- dplyr::case_when(
      is.na(d_approx)            ~ "indeterminate",
      d_approx >= 0.5 & n_participants >= 20 & n_items >= 30 ~ "likely sufficient",
      d_approx >= 0.3 & n_participants >= 30 & n_items >= 40 ~ "likely sufficient",
      d_approx >= 0.3            ~ "marginal",
      TRUE                       ~ "likely insufficient"
    )

    rows[[i]] <- data.frame(
      delta_low        = d1,
      delta_high       = d2,
      peak_asym_diff   = round(peak_diff, 4),
      approx_d         = round(d_approx, 3),
      n_items          = n_items,
      n_participants   = n_participants,
      power_rating     = power_rating,
      stringsAsFactors = FALSE
    )
  }

  result <- dplyr::bind_rows(rows)
  cat("\n=== phonActivR Power Guidance ===\n")
  cat("Design: ", n_items, " items x ", n_participants, " participants/group\n", sep = "")
  cat("Reliability assumption: ", effect_reliability, "\n\n", sep = "")
  cat("Interpretation note (four-level failure framework, You & Magnuson 2018):\n")
  cat("  'likely insufficient' power may indicate a parameter-level mismatch\n")
  cat("  (level 3) rather than a theory-level failure (level 1). Run\n")
  cat("  sensitivity analysis (Application 4 in the tutorial) before\n")
  cat("  concluding that the Language-Specific Grain Size hypothesis is wrong.\n\n")
  print(result, row.names = FALSE)
  invisible(result)
}
