# =============================================================================
# phonActivR: Stimulus Creation & Simulation Runner
# =============================================================================

#' Create a Stimulus Table for Simulation
#'
#' Constructs a standardized stimulus data frame for use with
#' \code{\link{run_simulation}}. Each row represents one experimental item
#' with its target, competitor, and control words specified as phoneme onset
#' transcriptions.
#'
#' @param targets Character vector. Target word labels.
#' @param large_comp Character vector or list. Larger-grain competitor word labels.
#' @param large_ctrl Character vector or list. Larger-grain control word labels.
#' @param small_comp Character vector or list. Smaller-grain competitor word labels.
#' @param small_ctrl Character vector or list. Smaller-grain control word labels.
#' @param onsets A named list of character vectors providing phoneme onset
#'   transcriptions for every word. Names must match word labels. Each entry
#'   is a character vector of phonemes (e.g., \code{c("b", "eh")} for "bench").
#' @param large_type Character. Overlap type for larger-grain competitors
#'   (default: "CV"). See \code{\link{compute_overlap}}.
#' @param small_type Character. Overlap type for smaller-grain competitors
#'   (default: "C").
#'
#' @return A list of class \code{"phonActivR_stimuli"} containing:
#'   \describe{
#'     \item{items}{Data frame with word labels}
#'     \item{onsets}{Named list of phoneme transcriptions}
#'     \item{large_type}{Overlap type for larger grain}
#'     \item{small_type}{Overlap type for smaller grain}
#'     \item{n_items}{Number of stimulus items}
#'   }
#' @export
#' @examples
#' # Minimal example with 2 items
#' onsets <- list(
#'   "bench" = c("b", "eh"), "bell" = c("b", "eh"), "cell" = c("s", "eh"),
#'   "bark" = c("b"), "dark" = c("d"),
#'   "bitter" = c("b", "ih"), "bill" = c("b", "ih"), "hill" = c("hh", "ih"),
#'   "bank" = c("b"), "tank" = c("t")
#' )
#' stim <- create_stimuli(
#'   targets    = c("bench", "bitter"),
#'   large_comp = c("bell", "bill"),
#'   large_ctrl = c("cell", "hill"),
#'   small_comp = c("bark", "bank"),
#'   small_ctrl = c("dark", "tank"),
#'   onsets     = onsets
#' )
create_stimuli <- function(targets, large_comp, large_ctrl,
                           small_comp, small_ctrl,
                           onsets,
                           large_type = "CV",
                           small_type = "C") {

  n <- length(targets)
  stopifnot(
    "All word vectors must have equal length" =
      all(lengths(list(large_comp, large_ctrl, small_comp, small_ctrl)) == n),
    "onsets must be a named list" = is.list(onsets) && !is.null(names(onsets))
  )

  # Verify all words have onset transcriptions
  all_words <- unique(c(targets, large_comp, large_ctrl, small_comp, small_ctrl))
  missing <- setdiff(all_words, names(onsets))
  if (length(missing) > 0) {
    cli::cli_alert_warning(
      "Missing onset transcriptions for {length(missing)} word(s): {paste(missing, collapse = ', ')}. These will use default overlap = 0.3."
    )
  } else {
    cli::cli_alert_success("All {length(all_words)} words verified: onset transcriptions complete.")
  }

  items <- data.frame(
    item       = seq_len(n),
    target     = targets,
    large_comp = large_comp,
    large_ctrl = large_ctrl,
    small_comp = small_comp,
    small_ctrl = small_ctrl,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      items      = items,
      onsets     = onsets,
      large_type = large_type,
      small_type = small_type,
      n_items    = n
    ),
    class = "phonActivR_stimuli"
  )
}


#' Run Full Simulation Across All Items and Delta Values
#'
#' The main simulation engine. Runs the interactive activation model for every
#' stimulus item at each specified delta value, computing item-level competition
#' effects and then averaging across items to produce predicted competition
#' curves.
#'
#' @param stimuli A \code{"phonActivR_stimuli"} object from
#'   \code{\link{create_stimuli}}.
#' @param delta_values Integer vector. Prosodic delay parameter values to test
#'   (default: \code{c(0, 10, 20, 30)}).
#' @param time_steps Integer. Number of normalized time steps (default: 101).
#' @param alpha Numeric. Activation growth rate (default: 0.08).
#' @param gamma Numeric. Lateral inhibition strength (default: 0.04).
#' @param decay Numeric. Passive decay rate (default: 0.02).
#' @param threshold Numeric. Competition onset detection threshold (default: 0.04).
#' @param ctrl_scaling Numeric. Control word overlap scaling factor (default: 0.15).
#' @param verbose Logical. Print progress messages (default: TRUE).
#' @param seed Integer or NULL. Random seed for reproducibility (default: NULL).
#'   Set to an integer (e.g., \code{seed = 1234}) to produce identical results
#'   across sessions. When \code{NULL}, each call may produce slightly different
#'   onset timing values due to floating-point order.
#'
#' @return A list of class \code{"phonActivR_sim"} containing:
#'   \describe{
#'     \item{results}{Named list (keyed by delta value) with mean effects, SEs,
#'       and asymmetry curves for each delta}
#'     \item{item_results}{List of item-level competition effect matrices}
#'     \item{onsets}{Data frame of onset timing for each delta}
#'     \item{stimuli}{The input stimuli object}
#'     \item{params}{List of simulation parameters}
#'     \item{summary}{Summary data frame}
#'   }
#' @export
#' @examples
#' \dontrun{
#' sim <- run_simulation(my_stimuli, delta_values = c(0, 10, 20, 30))
#' summary(sim)
#' plot_competition(sim, delta = 0)
#' }
run_simulation <- function(stimuli,
                           delta_values = c(0L, 10L, 20L, 30L),
                           time_steps   = 101L,
                           alpha        = 0.08,
                           gamma        = 0.04,
                           decay        = 0.02,
                           threshold    = 0.04,
                           ctrl_scaling = 0.15,
                           verbose      = TRUE,
                           seed         = NULL) {

  if (!is.null(seed)) set.seed(seed)

  stopifnot(inherits(stimuli, "phonActivR_stimuli"))

  n_items <- stimuli$n_items
  items   <- stimuli$items
  onsets  <- stimuli$onsets

  # Record start time for efficiency reporting
  t_start <- proc.time()[["elapsed"]]

  if (verbose) {
    cli::cli_h1("phonActivR Simulation")
    cli::cli_alert_info("Items: {n_items} | Delta values: {paste(delta_values, collapse = ', ')} | Time steps: {time_steps}")
  }

  results      <- list()
  item_results <- list()
  onset_data   <- list()

  for (delta in delta_values) {
    large_matrix <- matrix(NA, nrow = n_items, ncol = time_steps)
    small_matrix <- matrix(NA, nrow = n_items, ncol = time_steps)

    if (verbose) cli::cli_alert_info("Running delta = {delta}...")

    for (i in seq_len(n_items)) {
      row <- items[i, ]

      tgt_onset   <- onsets[[row$target]]     %||% c("?")
      lg_comp_on  <- onsets[[row$large_comp]] %||% c("?")
      lg_ctrl_on  <- onsets[[row$large_ctrl]] %||% c("?")
      sm_comp_on  <- onsets[[row$small_comp]] %||% c("?")
      sm_ctrl_on  <- onsets[[row$small_ctrl]] %||% c("?")

      item_out <- run_item(
        target_onset     = tgt_onset,
        large_comp_onset = lg_comp_on,
        large_ctrl_onset = lg_ctrl_on,
        small_comp_onset = sm_comp_on,
        small_ctrl_onset = sm_ctrl_on,
        large_type       = stimuli$large_type,
        small_type       = stimuli$small_type,
        delta            = delta,
        ctrl_scaling     = ctrl_scaling,
        time_steps       = time_steps,
        alpha            = alpha,
        gamma            = gamma,
        decay            = decay
      )

      large_matrix[i, ] <- item_out$large_effect
      small_matrix[i, ] <- item_out$small_effect
    }

    large_mean <- colMeans(large_matrix)
    small_mean <- colMeans(small_matrix)
    if (n_items > 1) {
      large_se <- apply(large_matrix, 2, sd) / sqrt(n_items)
      small_se <- apply(small_matrix, 2, sd) / sqrt(n_items)
    } else {
      # Single item: SE is undefined, use zero to avoid NA propagation
      large_se <- rep(0, time_steps)
      small_se <- rep(0, time_steps)
    }

    results[[as.character(delta)]] <- list(
      large_effect = large_mean,
      small_effect = small_mean,
      large_se     = large_se,
      small_se     = small_se,
      asymmetry    = large_mean - small_mean,
      asym_se      = sqrt(large_se^2 + small_se^2)
    )

    item_results[[as.character(delta)]] <- list(
      large_matrix = large_matrix,
      small_matrix = small_matrix
    )

    lg_on <- find_onset(large_mean, threshold)
    sm_on <- find_onset(small_mean, threshold)

    onset_data[[as.character(delta)]] <- data.frame(
      delta       = delta,
      large_onset = lg_on,
      small_onset = sm_on,
      delay       = if (is.na(lg_on) || is.na(sm_on)) NA_integer_ else sm_on - lg_on
    )

    if (verbose) {
      cli::cli_alert_success(
        "  delta={delta} | Large onset={ifelse(is.na(lg_on),'NA',lg_on)}% | Small onset={ifelse(is.na(sm_on),'NA',sm_on)}% | Delay={ifelse(is.na(lg_on)||is.na(sm_on),'NA',sm_on-lg_on)}%"
      )
    }
  }

  onset_df <- dplyr::bind_rows(onset_data)

  # Summary table
  summary_df <- dplyr::bind_rows(lapply(delta_values, function(d) {
    res <- results[[as.character(d)]]
    on  <- onset_df[onset_df$delta == d, ]
    data.frame(
      delta           = d,
      large_onset     = on$large_onset,
      small_onset     = on$small_onset,
      onset_delay     = on$delay,
      max_large       = round(max(res$large_effect), 4),
      max_small       = round(max(res$small_effect), 4),
      max_asymmetry   = round(max(res$asymmetry), 4)
    )
  }))

  if (verbose) {
    t_elapsed <- round(proc.time()[["elapsed"]] - t_start, 2)
    cli::cli_h1("Simulation Complete")
    cli::cli_alert_success(
      "Completed {n_items} items x {length(delta_values)} delta values in {t_elapsed}s. ",
      "To benchmark workflow efficiency vs. jTRACE batch scripting, see vignette('phonActivR-tutorial')."
    )
  }

  structure(
    list(
      results      = results,
      item_results = item_results,
      onsets       = onset_df,
      stimuli      = stimuli,
      params       = list(
        delta_values = delta_values,
        time_steps   = time_steps,
        alpha        = alpha,
        gamma        = gamma,
        decay        = decay,
        threshold    = threshold,
        ctrl_scaling = ctrl_scaling
      ),
      elapsed_sec  = round(proc.time()[["elapsed"]] - t_start, 2),
      summary      = summary_df
    ),
    class = "phonActivR_sim"
  )
}


#' @export
summary.phonActivR_sim <- function(object, ...) {
  cat("\n=== phonActivR Simulation Summary ===\n\n")
  cat("Items:", object$stimuli$n_items, "\n")
  cat("Grain sizes:", object$stimuli$large_type, "vs",
      object$stimuli$small_type, "\n")
  cat("Delta values:", paste(object$params$delta_values, collapse = ", "), "\n")
  cat("Parameters: alpha =", object$params$alpha,
      ", gamma =", object$params$gamma,
      ", decay =", object$params$decay, "\n")
  if (!is.null(object$elapsed_sec)) {
    cat("Elapsed time:", object$elapsed_sec, "seconds\n")
  }
  cat("\n")
  print(object$summary, row.names = FALSE)
  # Power guidance: the max_asymmetry column gives the expected effect size
  # at each delta, which can be used to power studies to discriminate delta values.
  cat("\nNote: 'max_asymmetry' is the expected peak competition asymmetry at each\n")
  cat("delta value. Use these as effect-size inputs when planning sample sizes\n")
  cat("(items and participants) to discriminate between competing delta hypotheses.\n")
  cat("A difference in max_asymmetry >= 0.02 between delta values typically\n")
  cat("requires ~30+ items and ~20+ participants per group for 80% power in\n")
  cat("a GAMM-based time-course analysis. See vignette('phonActivR-tutorial')\n")
  cat("for the sensitivity analysis workflow.\n\n")
  invisible(object$summary)
}


#' @export
print.phonActivR_sim <- function(x, ...) {
  cat("phonActivR simulation: ",
      x$stimuli$n_items, " items x ",
      length(x$params$delta_values), " delta values (",
      paste(x$params$delta_values, collapse = ","), ")\n", sep = "")
  cat("Use summary() for detailed results, plot_competition() for figures.\n")
  invisible(x)
}

