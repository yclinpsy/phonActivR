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
    warning("Missing onset transcriptions for ", length(missing), " word(s): ",
            paste(missing, collapse = ", "),
            ". These receive the placeholder \"?\" (similarity 0.3 with every ",
            "phoneme); add their transcriptions before simulating.",
            call. = FALSE)
  } else {
    cli::cli_alert_success("All {length(all_words)} unique words across the {n} items verified: onset transcriptions complete.")
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
#' @param features Feature matrix used for phoneme similarity
#'   (default \code{trace_features()}). Supply a custom named list of feature
#'   vectors for languages whose phonemes are not in the built-in English set.
#' @param similarity_matrix Optional. A symmetric matrix of direct pairwise
#'   phoneme similarities built with \code{\link{custom_similarity}}; entries
#'   in this matrix take precedence over the feature-based computation. This
#'   is the recommended route for cross-linguistic stimulus sets containing
#'   phonemes outside the built-in feature matrix.
#' @param seg_steps Numeric. Normalized time steps per phonological segment
#'   (default 10). Sets the disambiguation point of each competitor: it
#'   receives input only while the segments it shares with the target unfold
#'   (see \code{\link{run_item}}), so competition rises and then resolves.
#'   The default assumes that one segment occupies about 10\% of a
#'   normalized trial (a 3--4-segment word of roughly 400--500 ms within
#'   trials of about 1.0--1.3 s); check the sensitivity of conclusions to
#'   this value (e.g., 6--14). \code{Inf} reproduces the
#'   0.2.x dynamics, in which competitor input never ends.
#' @param residual_input Numeric in [0, 1] (default 0). Fraction of each
#'   competitor's (and control's) input that continues for one more segment
#'   after its shared segments end. 0 stops input at the disambiguation point;
#'   1 means a competitor is rejected only one segment after its first
#'   mismatching segment begins. In held-out-item tests on Lin et al. (2023),
#'   a value of 1 improved the fit to the C competition for English
#'   monolinguals; see the manuscript, Step 6b.
#' @param c_weight Numeric in (0, 1]. Weight of a C (consonant-only) match
#'   relative to a full consonant match (default 0.5): the C overlap is the
#'   consonant similarity times \code{c_weight}; see
#'   \code{\link{compute_overlap}}. It sets the strength of the C
#'   competitor's input and should be varied as a robustness check.
#' @param small_ctrl_type \code{NULL} (default) or an overlap type
#'   (\code{"C"}, \code{"CV"}, \code{"CVC"}, \code{"syllable"}) used to
#'   score the smaller-grain competitor's control. By default the control is
#'   scored with \code{small_type}, so a C control's overlap is its onset
#'   similarity times \code{c_weight}. Set \code{"CV"} when the C control
#'   shares the target's vowel (as in the Mandarin set,
#'   \code{\link{example_stimuli_zh}}), so that its vowel match enters its
#'   input; a sensitivity check for such designs.
#' @param gate Character, \code{"delay"} (default) or \code{"truncate"}.
#'   How the prosodic gate treats the smaller-grain competitor: a buffered
#'   delay (the whole support window moves with delta) or a signal-locked
#'   truncation (only the part of the shared segment heard after the gate
#'   opens supports the competitor). See \code{\link{run_item}}.
#' @param verbose Logical. Print progress messages (default: TRUE).
#' @param seed Ignored. The simulation is deterministic (identical input
#'   always gives identical output), so no random seed is needed; the
#'   argument is kept so that code written for earlier versions still runs,
#'   and it does not change the global random-number stream.
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
                           features     = trace_features(),
                           similarity_matrix = NULL,
                           seg_steps    = 10,
                           residual_input = 0,
                           gate         = c("delay", "truncate"),
                           c_weight     = 0.5,
                           small_ctrl_type = NULL,
                           verbose      = TRUE,
                           seed         = NULL) {

  gate <- match.arg(gate)
  # The model is deterministic; `seed` is accepted for backward
  # compatibility only and does not touch the random-number stream.

  stopifnot(inherits(stimuli, "phonActivR_stimuli"))

  n_items <- stimuli$n_items
  items   <- stimuli$items
  onsets  <- stimuli$onsets

  # Warn if any stimulus phonemes are outside the active feature/similarity
  # set: such phonemes would receive the default similarity (0.3) with every
  # other phoneme, which flattens cross-linguistic similarity structure.
  # NOTE: warn = TRUE unconditionally (a true R warning), so the fallback can
  # never operate silently even when verbose = FALSE; only the success
  # message is tied to verbosity.
  check_phoneme_coverage(stimuli, features = features,
                         similarity_matrix = similarity_matrix,
                         warn = TRUE, announce = verbose)

  # Record start time for efficiency reporting
  t_start <- proc.time()[["elapsed"]]

  if (verbose) {
    cli::cli_h1("phonActivR Simulation")
    cli::cli_alert_info("Items: {n_items} | Delta values: {paste(delta_values, collapse = ', ')} | Time steps: {time_steps}")
  }

  results      <- list()
  item_results <- list()
  onset_data   <- list()

  # Words without a transcription would silently receive the placeholder
  # "?" (similarity 0.3 with everything); make this an R warning so that it
  # cannot pass unnoticed in verbose = FALSE loops.
  all_words <- unique(c(items$target, items$large_comp, items$large_ctrl,
                        items$small_comp, items$small_ctrl))
  no_transcr <- all_words[!all_words %in% names(onsets)]
  if (length(no_transcr) > 0) {
    warning(length(no_transcr), " word(s) have no transcription in 'onsets' ",
            "and receive the placeholder \"?\" (e.g., ",
            paste(utils::head(no_transcr, 5), collapse = ", "),
            "). Add their transcriptions before interpreting the simulation.",
            call. = FALSE)
  }

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
        features         = features,
        similarity_matrix = similarity_matrix,
        seg_steps        = seg_steps,
        residual_input   = residual_input,
        gate             = gate,
        c_weight         = c_weight,
        small_ctrl_type  = small_ctrl_type,
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
        "  delta={delta} | Large onset: step {ifelse(is.na(lg_on),'NA',lg_on)} | Small onset: step {ifelse(is.na(sm_on),'NA',sm_on)} | Delay: {ifelse(is.na(lg_on)||is.na(sm_on),'NA',sm_on-lg_on)} steps"
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
      "Completed {n_items} items x {length(delta_values)} delta values in {t_elapsed}s."
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
        ctrl_scaling = ctrl_scaling,
        features     = features,
        similarity_matrix = similarity_matrix,
        seg_steps    = seg_steps,
        residual_input = residual_input,
        gate         = gate,
        c_weight     = c_weight,
        small_ctrl_type = small_ctrl_type
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
  cat("\nNote: 'max_asymmetry' is the model's peak competition asymmetry at each\n")
  cat("delta value, in model activation units. Differences between delta values\n")
  cat("show how strongly the hypotheses diverge; see power_guidance() for a\n")
  cat("coarse screening heuristic (not a power analysis) and\n")
  cat("vignette('phonActivR-tutorial') for the sensitivity-analysis workflow.\n\n")
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


#' @export
print.phonActivR_stimuli <- function(x, ...) {
  cat("phonActivR stimuli: ", x$n_items, " items | grain sizes: ",
      x$large_type, " (large) vs ", x$small_type, " (small)\n", sep = "")
  cat("Use summary() for the item table, run_simulation() to simulate.\n")
  invisible(x)
}


#' @export
summary.phonActivR_stimuli <- function(object, ...) {
  cat("phonActivR stimulus set\n")
  cat("  Items:       ", object$n_items, "\n", sep = "")
  cat("  Large grain: ", object$large_type,
      " (competitor column: large_comp)\n", sep = "")
  cat("  Small grain: ", object$small_type,
      " (competitor column: small_comp)\n", sep = "")
  cat("  Onset transcriptions: ", length(object$onsets),
      " words\n\n", sep = "")
  cat("First items:\n")
  print(utils::head(object$items, 5), row.names = FALSE)
  if (object$n_items > 5) {
    cat("... and ", object$n_items - 5, " more rows (object$items).\n",
        sep = "")
  }
  invisible(object$items)
}



# Internal: re-run the model with the settings stored in `sim` at a single
# delta value (used to generate data at an off-grid delta without
# interpolating between grid curves). `overrides` replaces stored settings
# (e.g., list(residual_input = 1)) to generate data from a different model.
# Returns the full result list (large_effect, small_effect, asymmetry, ...).
.simulate_one <- function(sim, delta, overrides = list()) {
  key <- as.character(delta)
  if (length(overrides) == 0 && !is.null(sim$results[[key]])) {
    return(sim$results[[key]])
  }
  p <- sim$params
  get <- function(nm, default) {
    if (!is.null(overrides[[nm]])) overrides[[nm]] else
      if (!is.null(p[[nm]])) p[[nm]] else default
  }
  one <- run_simulation(sim$stimuli,
                        delta_values = delta,
                        time_steps   = get("time_steps", 101L),
                        alpha        = get("alpha", 0.08),
                        gamma        = get("gamma", 0.04),
                        decay        = get("decay", 0.02),
                        threshold    = get("threshold", 0.04),
                        ctrl_scaling = get("ctrl_scaling", 0.15),
                        features     = get("features", trace_features()),
                        similarity_matrix = get("similarity_matrix", NULL),
                        seg_steps    = get("seg_steps", 10),
                        residual_input = get("residual_input", 0),
                        gate         = get("gate", "delay"),
                        c_weight     = get("c_weight", 0.5),
                        verbose      = FALSE)
  one$results[[key]]
}
.simulate_asymmetry <- function(sim, delta, overrides = list()) {
  .simulate_one(sim, delta, overrides)$asymmetry
}
