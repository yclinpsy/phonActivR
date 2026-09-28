# =============================================================================
# phonActivR: Interactive Activation Engine
# =============================================================================

#' Run Interactive Activation Simulation for a Single Competitor Pair
#'
#' Implements compact bounded lateral-inhibition competition dynamics (interactive-activation family; not TRACE) for one target word
#' with one competitor and one control. The model tracks activation of all
#' three candidates across normalized time steps, applying lateral inhibition
#' and passive decay at each step.
#'
#' @param overlap_comp Numeric. Phonological overlap of the competitor with the
#'   target word, in [0, 1].
#' @param overlap_ctrl Numeric. Phonological overlap of the control word with
#'   the target, in [0, 1].
#' @param overlap_onset Integer. Time step at which competitor phonological
#'   features become available. Set to 1 for mora-aligned units (CV competitors)
#'   or \code{1 + delta} for sub-moraic units under a prosodic constraint.
#' @param time_steps Integer. Total number of normalized time steps (default: 101).
#' @param alpha Numeric. Activation growth rate (default: 0.08).
#' @param gamma Numeric. Lateral inhibition strength (default: 0.04).
#' @param decay Numeric. Passive decay rate (default: 0.02).
#' @param max_act Numeric. Activation ceiling (default: 1.0).
#' @param min_act Numeric. Activation floor (default: 0.0).
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{time}{Time step (1 to \code{time_steps})}
#'     \item{act_comp}{Competitor activation level}
#'     \item{act_ctrl}{Control word activation level}
#'     \item{act_target}{Target word activation level}
#'     \item{competition_effect}{Competitor minus control activation
#'       (mirrors empirical mouse-tracking/eye-tracking DVs)}
#'   }
#' @export
#' @examples
#' # Simulate CV competitor with high overlap, no delay
#' result <- run_activation(overlap_comp = 0.8, overlap_ctrl = 0.12, overlap_onset = 1)
#' plot(result$time, result$competition_effect, type = "l")
#'
#' # Simulate C competitor with delay (moraic constraint, delta = 20)
#' result_delayed <- run_activation(overlap_comp = 0.4, overlap_ctrl = 0.06,
#'                                   overlap_onset = 21)
run_activation <- function(overlap_comp, overlap_ctrl,
                           overlap_onset = 1,
                           time_steps = 101L,
                           alpha = 0.08,
                           gamma = 0.04,
                           decay = 0.02,
                           max_act = 1.0,
                           min_act = 0.0) {

  # Pre-allocate activation vectors; all candidates start at activation 0.
  act_comp   <- numeric(time_steps)   # competitor activation a_comp(t)
  act_ctrl   <- numeric(time_steps)   # control activation a_ctrl(t)
  act_target <- numeric(time_steps)   # target activation a_target(t)

  # Each pass through this loop applies the discrete-time update equation
  #   a_i(t) = a_i(t-1)                              [carry-over]
  #          + alpha * input_i * (max_act - a_i(t-1)) [bounded excitatory growth]
  #          - gamma * sum_{j != i} a_j(t-1)          [lateral inhibition]
  #          - decay * a_i(t-1)                       [passive decay]
  # to each of the three candidates, then clips activation into
  # [min_act, max_act].
  for (t in 2:time_steps) {
    # --- Target word -------------------------------------------------------
    # The target matches its own input perfectly, so input_target = 1.0 and
    # its input is available from the first time step (never gated).
    act_target[t] <- act_target[t-1] +
      alpha * 1.0 * (max_act - act_target[t-1]) -   # excitatory growth toward ceiling
      gamma * act_comp[t-1] -                        # inhibition from competitor
      gamma * act_ctrl[t-1] -                        # inhibition from control
      decay * act_target[t-1]                        # passive decay
    act_target[t] <- max(min_act, min(max_act, act_target[t]))  # clip to bounds

    # --- Competitor --------------------------------------------------------
    # The competitor's input is gated by the prosodic constraint: before
    # time step `overlap_onset` its input is 0 (a hard temporal gate); from
    # `overlap_onset` onward, the full overlap value applies.
    inp_comp <- ifelse(t >= overlap_onset, overlap_comp, 0.0)
    act_comp[t] <- act_comp[t-1] +
      alpha * inp_comp * (max_act - act_comp[t-1]) - # gated excitatory growth
      gamma * act_target[t-1] -                      # inhibition from target
      gamma * act_ctrl[t-1] -                        # inhibition from control
      decay * act_comp[t-1]                          # passive decay
    act_comp[t] <- max(min_act, min(max_act, act_comp[t]))

    # --- Control word ------------------------------------------------------
    # The control has minimal overlap with the input and is never delayed;
    # it provides the baseline against which competition is measured.
    act_ctrl[t] <- act_ctrl[t-1] +
      alpha * overlap_ctrl * (max_act - act_ctrl[t-1]) -
      gamma * act_target[t-1] -
      gamma * act_comp[t-1] -
      decay * act_ctrl[t-1]
    act_ctrl[t] <- max(min_act, min(max_act, act_ctrl[t]))
  }

  data.frame(
    time               = seq_len(time_steps),
    act_comp           = act_comp,
    act_ctrl           = act_ctrl,
    act_target         = act_target,
    competition_effect = act_comp - act_ctrl
  )
}


#' Run Simulation for a Single Stimulus Item
#'
#' Computes competition effects for both CV and C (or other overlap type)
#' competitor-control pairs for one target word, applying the specified
#' prosodic delay to the smaller-grain competitor.
#'
#' @param target_onset Character vector. Target word onset phonemes.
#' @param large_comp_onset Character vector. Larger-grain competitor onset (e.g., CV).
#' @param large_ctrl_onset Character vector. Larger-grain control onset.
#' @param small_comp_onset Character vector. Smaller-grain competitor onset (e.g., C).
#' @param small_ctrl_onset Character vector. Smaller-grain control onset.
#' @param large_type Character. Overlap type for the larger grain (default: "CV").
#' @param small_type Character. Overlap type for the smaller grain (default: "C").
#' @param delta Integer. Prosodic delay parameter. The number of time steps by
#'   which the smaller-grain competitor activation is withheld (default: 0).
#' @param ctrl_scaling Numeric. Scaling factor for control word overlap to
#'   approximate baseline neighbourhood activation (default: 0.15).
#' @param features Feature matrix used for phoneme similarity
#'   (default \code{trace_features()}).
#' @param similarity_matrix Optional custom similarity matrix
#'   (from \code{\link{custom_similarity}}); takes precedence over
#'   \code{features} for pairs it covers.
#' @param ... Additional arguments passed to \code{\link{run_activation}}.
#'
#' @return A list with components:
#'   \describe{
#'     \item{large_effect}{Numeric vector. Larger-grain competition effect curve.}
#'     \item{small_effect}{Numeric vector. Smaller-grain competition effect curve.}
#'     \item{large_overlap}{Numeric. Computed overlap for larger-grain competitor.}
#'     \item{small_overlap}{Numeric. Computed overlap for smaller-grain competitor.}
#'   }
#' @export
run_item <- function(target_onset,
                     large_comp_onset, large_ctrl_onset,
                     small_comp_onset, small_ctrl_onset,
                     large_type = "CV", small_type = "C",
                     delta = 0L,
                     ctrl_scaling = 0.15,
                     features = trace_features(),
                     similarity_matrix = NULL,
                     ...) {

  # --- Step 1: compute word-level phonological overlaps --------------------
  # Competitor overlaps are used at full strength; control overlaps are
  # multiplied by ctrl_scaling to approximate the weak baseline activation
  # of a phonologically unrelated word.
  ovl_large_comp <- compute_overlap(target_onset, large_comp_onset, large_type,
                                    features = features,
                                    similarity_matrix = similarity_matrix)
  ovl_large_ctrl <- compute_overlap(target_onset, large_ctrl_onset, large_type,
                                    features = features,
                                    similarity_matrix = similarity_matrix) *
    ctrl_scaling
  ovl_small_comp <- compute_overlap(target_onset, small_comp_onset, small_type,
                                    features = features,
                                    similarity_matrix = similarity_matrix)
  ovl_small_ctrl <- compute_overlap(target_onset, small_ctrl_onset, small_type,
                                    features = features,
                                    similarity_matrix = similarity_matrix) *
    ctrl_scaling

  # --- Step 2: run the activation dynamics for each competitor pair --------
  # Larger-grain (prosodically aligned) competitors: gate always open, so
  # their input is available from the first time step (onset = 1).
  large_out <- run_activation(ovl_large_comp, ovl_large_ctrl,
                               overlap_onset = 1L, ...)
  # Smaller-grain (sub-prosodic) competitors: input withheld for the first
  # delta time steps (onset = 1 + delta), implementing the prosodic gate.
  small_out <- run_activation(ovl_small_comp, ovl_small_ctrl,
                               overlap_onset = 1L + delta, ...)

  list(
    large_effect  = large_out$competition_effect,
    small_effect  = small_out$competition_effect,
    large_overlap = ovl_large_comp,
    small_overlap = ovl_small_comp
  )
}


#' Detect Competition Effect Onset
#'
#' Finds the first time step at which the competition effect exceeds a given
#' threshold, indicating the emergence of significant lexical competition.
#'
#' @param effect_curve Numeric vector. The competition effect time course
#'   (competitor activation minus control activation).
#' @param threshold Numeric. Minimum activation difference to count as
#'   competition onset (default: 0.04).
#'
#' @return Integer. The time step of competition onset, or \code{NA} if the
#'   threshold is never exceeded.
#' @export
#' @examples
#' curve <- c(rep(0, 10), seq(0.01, 0.5, length.out = 91))
#' find_onset(curve)
find_onset <- function(effect_curve, threshold = 0.04) {
  idx <- which(effect_curve > threshold)
  if (length(idx) == 0) return(NA_integer_)
  idx[1]
}
