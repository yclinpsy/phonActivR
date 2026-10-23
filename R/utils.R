# =============================================================================
# phonActivR: Synthetic Empirical Data & Utility Functions
# =============================================================================

#' Generate Synthetic Empirical Data for Demonstration
#'
#' Creates a synthetic empirical dataset that mimics the structure of GAMM
#' smooth output from a time-course experiment (mouse-tracking or eye-tracking).
#'
#' @section How the data are generated:
#' The experimental group's curve is produced by running the model itself at
#' \code{true_delta} (with the stimuli and parameters stored in \code{sim});
#' it is \emph{not} interpolated between the simulated grid curves, so an
#' off-grid value such as \code{true_delta = 15} yields the curve the model
#' actually predicts at delta = 15. The control group's curve is the
#' delta = 0 prediction. Temporally correlated Gaussian noise (a 5-bin moving
#' average) is then added to both. Because these data come from the model,
#' they are suitable for learning the workflow and for parameter-recovery
#' checks, but they are not evidence that the model fits real behavior.
#' Researchers substitute their own \code{data.frame} (containing
#' \code{time}, \code{asymmetry}, and optionally \code{se} and
#' \code{group}) with no other change to the call; see the section "From Raw
#' Data to the Fitting Format" in \code{vignette("phonActivR-tutorial")}.
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
#' @param noise_sd Numeric or \code{NULL}. Standard deviation of Gaussian noise
#'   added to the curves to simulate empirical variability. When \code{NULL}
#'   (the default), a paradigm-typical value is used: 0.015 for
#'   \code{"mouse-tracking"}, 0.020 for \code{"eye-tracking"}, and 0.030 for
#'   \code{"erp"} (reflecting the typically lower signal-to-noise ratio of ERP
#'   difference waves). Supply a number to override.
#' @param group_labels Character vector of length 2. Labels for the experimental
#'   and control groups (default: \code{c("L2 bilinguals", "L1 monolinguals")}).
#' @param paradigm Character. One of \code{"mouse-tracking"},
#'   \code{"eye-tracking"}, or \code{"erp"}. Determines the default
#'   \code{noise_sd} (see above) and is recorded in the returned data frame's
#'   \code{paradigm} column, so that synthetic datasets are self-documenting.
#' @param seed Integer. Random seed for the noise (default: 42). It is
#'   applied locally, so the global random-number stream is not changed.
#' @param generate_with Named list of \code{run_simulation()} settings that
#'   override those stored in \code{sim} when generating the data (e.g.,
#'   \code{list(residual_input = 1)} or \code{list(seg_steps = 14)}), to
#'   create data from a model that differs from the one being fitted
#'   (a misspecification check). Default: none.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{time}{Integer. Normalized time step (1 to time_steps)}
#'     \item{asymmetry}{Numeric. Larger-grain minus smaller-grain effect}
#'     \item{cv_effect, c_effect}{Numeric. The larger- and smaller-grain
#'       competition effects (\code{asymmetry = cv_effect - c_effect}), for
#'       use with \code{\link{fit_components}}. Noise with SD
#'       \code{noise_sd} (before smoothing) is added to the asymmetry and to
#'       the CV curve, and the C curve is their difference, so its noise SD is
#'       about \code{sqrt(2) * noise_sd} and correlated with the CV noise}
#'     \item{se}{Numeric. Standard error estimate}
#'     \item{group}{Character. Group label}
#'     \item{paradigm}{Character. The paradigm label supplied}
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
                              noise_sd = NULL,
                              group_labels = c("L2 bilinguals", "L1 monolinguals"),
                              paradigm = c("mouse-tracking", "eye-tracking", "erp"),
                              seed = 42L,
                              generate_with = list()) {
  stopifnot(inherits(sim, "phonActivR_sim"))
  stopifnot(length(group_labels) == 2)
  paradigm <- match.arg(paradigm)
  # A local seed: the user's global random-number stream is left unchanged.
  withr::with_seed(seed, .empirical_draw(sim, true_delta, noise_sd,
                                         group_labels, paradigm,
                                         generate_with))
}

# Internal: draw one synthetic dataset from the current random stream.
.empirical_draw <- function(sim, true_delta, noise_sd, group_labels,
                            paradigm, generate_with = list(),
                            components = TRUE) {
  # When noise_sd was not supplied, pick a paradigm-typical noise level
  # (mouse-tracking curves from GAMM smooths are relatively smooth; ERP
  # difference waves are noisier).
  if (is.null(noise_sd)) {
    noise_sd <- switch(paradigm,
      "mouse-tracking" = 0.015,
      "eye-tracking"   = 0.020,
      "erp"            = 0.030
    )
  }

  ts <- sim$params$time_steps
  deltas <- sort(sim$params$delta_values)

  # Clamp true_delta to the range of simulated delta values
  true_delta <- max(min(deltas), min(max(deltas), true_delta))

  # Experimental group: the model's own prediction at true_delta (re-run
  # when true_delta is not a grid value, or when generate_with asks for a
  # different generating model). Control group: delta = min(grid).
  res_exp  <- .simulate_one(sim, true_delta, generate_with)
  res_ctrl <- .simulate_one(sim, deltas[1], generate_with)
  asym_exp <- res_exp$asymmetry
  asym_ctrl <- res_ctrl$asymmetry

  # Temporally correlated noise (5-bin moving average), as in GAMM smooths.
  smooth_noise <- function(n, sd_val) {
    raw <- stats::rnorm(n, 0, sd_val)
    kernel <- rep(1/5, 5)
    smoothed <- stats::filter(raw, kernel, sides = 2)
    smoothed[is.na(smoothed)] <- raw[is.na(smoothed)]
    as.numeric(smoothed)
  }

  exp_noisy  <- asym_exp  + smooth_noise(ts, noise_sd)
  ctrl_noisy <- asym_ctrl + smooth_noise(ts, noise_sd * 0.8)

  # Simulated SE (varies over time)
  se_exp  <- abs(asym_exp)  * 0.25 + noise_sd * 0.5 + abs(smooth_noise(ts, 0.003))
  se_ctrl <- abs(asym_ctrl) * 0.20 + noise_sd * 0.4 + abs(smooth_noise(ts, 0.003))

  # Component curves consistent with the asymmetry (asymmetry = cv - c):
  # noisy CV curve, and C = CV - asymmetry. The C curve's noise is therefore
  # the difference of two independent draws (SD about sqrt(2) * noise_sd)
  # and is correlated with the CV noise (documented in @return).
  # (Drawn after all other noise, so the asymmetry values do not depend on
  # whether components are requested.)
  if (components) {
    cv_exp  <- res_exp$large_effect  + smooth_noise(ts, noise_sd)
    cv_ctrl <- res_ctrl$large_effect + smooth_noise(ts, noise_sd * 0.8)
  } else {
    cv_exp <- cv_ctrl <- rep(NA_real_, ts)
  }

  data.frame(
    time      = rep(seq_len(ts), 2),
    asymmetry = c(exp_noisy, ctrl_noisy),
    cv_effect = c(cv_exp, cv_ctrl),
    c_effect  = c(cv_exp - exp_noisy, cv_ctrl - ctrl_noisy),
    se        = c(se_exp, se_ctrl),
    group     = rep(group_labels, each = ts),
    paradigm  = paradigm,
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
#' Files written: \code{_summary.csv}, \code{_curves.csv} (every delta),
#' \code{_parameters.csv} (all settings needed to re-run the simulation,
#' including \code{seg_steps}, \code{residual_input}, \code{gate}, and the
#' package version), \code{_similarity.csv} (only when a custom similarity
#' matrix was used), and \code{_sessionInfo.txt}.
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
  p <- sim$params
  or_na <- function(x, default) if (is.null(x)) default else x
  params_df <- data.frame(
    parameter = c("package_version", "time_steps", "alpha", "gamma", "decay",
                  "threshold", "ctrl_scaling", "seg_steps", "residual_input",
                  "gate", "c_weight", "n_items", "large_type", "small_type",
                  "delta_values", "similarity"),
    value = c(as.character(utils::packageVersion("phonActivR")),
              p$time_steps, p$alpha, p$gamma, p$decay, p$threshold,
              p$ctrl_scaling, or_na(p$seg_steps, 10), or_na(p$residual_input, 0),
              or_na(p$gate, "delay"), or_na(p$c_weight, 0.5),
              sim$stimuli$n_items, sim$stimuli$large_type, sim$stimuli$small_type,
              paste(p$delta_values, collapse = ","),
              if (is.null(p$similarity_matrix)) "trace_features()" else
                paste0("custom (", prefix, "_similarity.csv)"))
  )
  utils::write.csv(params_df, params_path, row.names = FALSE)
  files <- c(summary_path, curves_path, params_path)

  # 4. Custom similarity matrix, if one was used
  if (!is.null(p$similarity_matrix)) {
    sim_path <- file.path(dir, paste0(prefix, "_similarity.csv"))
    utils::write.csv(as.data.frame(p$similarity_matrix), sim_path)
    files <- c(files, sim_path)
  }

  # 5. Session information (R and package versions)
  info_path <- file.path(dir, paste0(prefix, "_sessionInfo.txt"))
  writeLines(utils::capture.output(utils::sessionInfo()), info_path)
  files <- c(files, info_path)
  cli::cli_alert_success("Exported {length(files)} files to {normalizePath(dir)}")
  invisible(files)
}


#' Screening Guidance: How Separable Are Adjacent Delta Hypotheses?
#'
#' A lightweight planning heuristic that uses the simulation output to
#' characterise how separable two adjacent delta hypotheses are, given a
#' study design. It is not a power analysis; for formal planning, simulate
#' the planned analysis on simulated participants.
#'
#' @section Units:
#' The difference between two predicted curves is in the model's activation
#' units, whereas a between-participant SD measured in data is in the data's
#' units (e.g., normalized x-coordinate). Supply \code{scale}, the
#' least-squares factor that maps model units onto data units (the
#' \code{scale} column of \code{\link{goodness_of_fit}} on pilot or
#' published data), together with an \code{empirical_sd} in data units. If
#' \code{scale} is \code{NULL}, \code{empirical_sd} must already be expressed
#' in model units.
#'
#' @section Four-Level Failure Framework:
#' Following Magnuson, Mirman, and Harris (2012; as applied to modeling tools
#' by You & Magnuson, 2018), failures to detect a delta difference can
#' arise at four levels: (1) \emph{theory level} -- the language-specific
#' grain-size hypothesis (after Cutler & Otake's, 1994, language-specific
#' listening proposal) is wrong; (2) \emph{implementation level} -- phonActivR's
#' equations misrepresent the target theory; (3) \emph{parameter level} --
#' the default alpha/gamma/decay values do not apply to the population
#' studied; or (4) \emph{linking-hypothesis level} -- the empirical paradigm
#' does not map cleanly onto the competition-effect quantity. Use
#' \code{\link{sensitivity_analysis}}, \code{\link{fit_components}}, and the
#' robustness checks of the tutorial vignette to probe levels 3 and 4 before
#' concluding level 1 or 2.
#'
#' @section How the rating is computed:
#' \code{approx_d} is the largest momentary difference between the two
#' asymmetry curves (converted to data units with \code{scale}) divided by
#' \code{empirical_sd} and attenuated by \code{sqrt(effect_reliability)}.
#' \code{expected_z} \eqn{= d \sqrt{n/2}} is the test statistic expected for
#' a comparison of two independent groups of \code{n_participants} each. The
#' rating uses the conventional benchmarks for a two-sided test at alpha =
#' .05: \code{expected_z >= 2.8} (about 80\% power) "likely sufficient",
#' \code{>= 2.0} "marginal", otherwise "likely insufficient". The largest
#' momentary difference overstates what a whole-curve analysis can detect,
#' and time-course analyses have their own error structure, so treat the
#' rating as a screening aid.
#'
#' @param sim A \code{"phonActivR_sim"} object from \code{\link{run_simulation}}.
#' @param n_items Integer. Number of stimulus items per condition (default:
#'   42). Reported in the output for documentation only: the number of
#'   items does not enter the computation. If item sampling matters, reflect
#'   it in \code{effect_reliability} (e.g., via Spearman--Brown from pilot
#'   item-level reliability).
#' @param n_participants Integer. Number of participants per group (default: 24).
#' @param effect_reliability Numeric in (0,1). Estimated reliability of the
#'   empirical asymmetry measure (default: 0.70).
#' @param empirical_sd Numeric > 0. Assumed between-participant SD of the
#'   empirical asymmetry at its peak (default: 0.04). In data units when
#'   \code{scale} is given; otherwise in model units.
#' @param scale Numeric > 0 or \code{NULL} (default). Model-to-data scale
#'   factor (see Units).
#'
#' @return A data.frame with one row per pair of adjacent delta values:
#'   \code{max_curve_diff} (model units), \code{max_diff_data} (data units,
#'   \code{NA} without \code{scale}), \code{approx_d}, \code{expected_z},
#'   the design values, and \code{power_rating}.
#'
#' @references
#' Magnuson, J. S., Mirman, D., & Harris, H. D. (2012). Computational models
#' of spoken word recognition. In M. Spivey, K. McRae, & M. Joanisse (Eds.),
#' \emph{The Cambridge handbook of psycholinguistics} (pp. 76--103).
#' Cambridge University Press.
#'
#' You, H., & Magnuson, J. S. (2018). TISK 1.0: An easy-to-use Python
#' implementation of the time-invariant string kernel model of spoken word
#' recognition. \emph{Behavior Research Methods}, 50(3), 871--889.
#' https://doi.org/10.3758/s13428-017-1012-5
#'
#' @export
#' @examples
#' stim <- example_stimuli()
#' sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30), verbose = FALSE)
#' # scale from a fit to pilot data; SD in the data's units
#' power_guidance(sim, n_participants = 40, empirical_sd = 0.04, scale = 0.25)
power_guidance <- function(sim,
                           n_items          = 42L,
                           n_participants   = 24L,
                           effect_reliability = 0.70,
                           empirical_sd     = 0.04,
                           scale            = NULL) {

  stopifnot(inherits(sim, "phonActivR_sim"))
  stopifnot(is.numeric(empirical_sd), empirical_sd > 0)
  stopifnot(is.null(scale) || (is.numeric(scale) && scale > 0))

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
    # Largest momentary difference between the two asymmetry curves (two
    # delta values can share a peak yet differ in timing).
    diff_model <- max(abs(r2$asymmetry - r1$asymmetry))
    diff_units <- if (is.null(scale)) diff_model else diff_model * scale
    d_approx   <- (diff_units / empirical_sd) * sqrt(effect_reliability)
    z_expected <- d_approx * sqrt(n_participants / 2)
    rating <- if (z_expected >= 2.8) "likely sufficient" else
      if (z_expected >= 2.0) "marginal" else "likely insufficient"
    rows[[i]] <- data.frame(
      delta_low      = d1,
      delta_high     = d2,
      max_curve_diff = round(diff_model, 4),
      max_diff_data  = if (is.null(scale)) NA_real_ else round(diff_units, 4),
      approx_d       = round(d_approx, 3),
      expected_z     = round(z_expected, 2),
      n_items        = n_items,
      n_participants = n_participants,
      power_rating   = rating,
      stringsAsFactors = FALSE
    )
  }

  result <- dplyr::bind_rows(rows)
  cat("\n=== phonActivR Screening Guidance ===\n")
  cat("Design: ", n_items, " items x ", n_participants, " participants/group\n", sep = "")
  cat("Reliability: ", effect_reliability, " | SD: ", empirical_sd,
      if (is.null(scale)) " (model units; no scale supplied)" else
        paste0(" (data units; model-to-data scale = ", scale, ")"), "\n\n", sep = "")
  print(result, row.names = FALSE)
  cat("\nA screening heuristic, not a power analysis: expected_z assumes a\n")
  cat("two-group comparison at the curves' largest difference. A low rating can\n")
  cat("reflect a parameter-level mismatch (Magnuson et al., 2012) rather than a\n")
  cat("wrong theory; run sensitivity_analysis() and fit_components() first.\n")
  invisible(result)
}
