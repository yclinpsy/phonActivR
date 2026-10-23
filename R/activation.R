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
#' @param overlap_offset Numeric. Time step at which the competitor's (and the
#'   control's) bottom-up input ends because the unfolding target has
#'   diverged from it (the disambiguation point). Input is present for
#'   \code{overlap_onset <= t < overlap_offset}. After this point the
#'   competitor receives no further support and its activation falls under
#'   decay and lateral inhibition from the target, so competition resolves.
#'   \code{Inf} (the default here) keeps the input on for the whole trial,
#'   which reproduces the phonActivR 0.2.x dynamics.
#' @param residual_input Numeric in [0, 1]. Fraction of the competitor's (and
#'   the control's) input that continues for \code{residual_steps} steps after
#'   \code{overlap_offset} (default 0: input stops at the disambiguation
#'   point). A value of 1 means listeners reject a competitor only one
#'   segment after its first mismatching segment begins.
#' @param residual_steps Numeric. Duration of the residual input, in time
#'   steps (default 0).
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
                           overlap_offset = Inf,
                           residual_input = 0,
                           residual_steps = 0,
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
  stopifnot(residual_input >= 0, residual_input <= 1, residual_steps >= 0)
  # Input weight after the disambiguation point: residual_input for
  # residual_steps steps, then 0 (weight 1 before the point).
  resid_w <- function(t) {
    if (t >= overlap_offset && t < overlap_offset + residual_steps) residual_input else 0
  }

  # Each pass through this loop applies the discrete-time update equation
  #   a_i(t) = a_i(t-1)                              [carry-over]
  #          + alpha * input_i * (max_act - a_i(t-1)) [bounded excitatory growth]
  #          - gamma * sum_{j != i} a_j(t-1)          [lateral inhibition]
  #          - decay * a_i(t-1)                       [passive decay]
  # to each of the three candidates, then clips activation into
  # [min_act, max_act].
  # Step 1 is computed from the resting state (all activations 0), so the
  # input that is available at step 1 acts at step 1: a window [onset, offset)
  # supplies exactly offset - onset steps of input, whatever delta is.
  pt <- pc <- pk <- 0
  for (t in seq_len(time_steps)) {
    # --- Target word -------------------------------------------------------
    # The target matches its own input perfectly, so input_target = 1.0 and
    # its input is available from the first time step (never gated).
    act_target[t] <- pt +
      alpha * 1.0 * (max_act - pt) -   # excitatory growth toward ceiling
      gamma * pc -                        # inhibition from competitor
      gamma * pk -                        # inhibition from control
      decay * pt                        # passive decay
    act_target[t] <- max(min_act, min(max_act, act_target[t]))  # clip to bounds

    # --- Competitor --------------------------------------------------------
    # The competitor's input is gated by the prosodic constraint: before
    # time step `overlap_onset` its input is 0 (a hard temporal gate); from
    # `overlap_onset` onward, the full overlap value applies.
    in_window <- t >= overlap_onset && t < overlap_offset
    # Residual input (if any) follows the disambiguation point, and only
    # once the competitor's gate has opened.
    inp_comp <- if (in_window) overlap_comp else
      if (t >= overlap_onset) overlap_comp * resid_w(t) else 0.0
    act_comp[t] <- pc +
      alpha * inp_comp * (max_act - pc) - # gated excitatory growth
      gamma * pt -                      # inhibition from target
      gamma * pk -                        # inhibition from control
      decay * pc                          # passive decay
    act_comp[t] <- max(min_act, min(max_act, act_comp[t]))

    # --- Control word ------------------------------------------------------
    # The control has minimal overlap with the input and is never delayed;
    # it provides the baseline against which competition is measured.
    # The control's weak input spans the same window as its competitor
    # (matched items), so the competitor-minus-control effect returns to
    # zero once both lose bottom-up support.
    inp_ctrl <- if (t < overlap_offset) overlap_ctrl else overlap_ctrl * resid_w(t)
    act_ctrl[t] <- pk +
      alpha * inp_ctrl * (max_act - pk) -
      gamma * pt -
      gamma * pc -
      decay * pk
    act_ctrl[t] <- max(min_act, min(max_act, act_ctrl[t]))
    pt <- act_target[t]; pc <- act_comp[t]; pk <- act_ctrl[t]
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
#' @param seg_steps Numeric. Normalized time steps per phonological segment
#'   (default: 10). Each competitor receives input for as long as the segments
#'   it shares with the target take to unfold: \code{shared_segments(type)
#'   * seg_steps} steps (C = 1 segment, CV = 2, CVC = 3, syllable = the
#'   number of transcribed segments), starting at its gate-open time (1 for
#'   the larger grain, \code{1 + delta} for the smaller grain). After that
#'   the target has diverged from the competitor and its input stops. Use
#'   \code{Inf} for the 0.2.x behavior (input never ends).
#' @param residual_input Numeric in [0, 1] (default 0). Fraction of each
#'   competitor's input that continues for one more segment
#'   (\code{seg_steps} steps) after its shared segments end; see
#'   \code{\link{run_activation}}. Applied identically to both grain sizes.
#' @param c_weight Numeric in (0, 1]. Weight of a C match relative to a full
#'   consonant match in \code{\link{compute_overlap}} (default 0.5).
#' @param small_ctrl_type \code{NULL} (default: \code{small_type}) or the
#'   overlap type used to score the smaller-grain competitor's control; see
#'   \code{\link{run_simulation}}.
#' @param gate Character. How the prosodic gate treats the smaller-grain
#'   competitor. \code{"delay"} (default) is a buffered delay: the
#'   sub-prosodic evidence is held until \code{1 + delta} and then supports
#'   the competitor for its full shared-segment duration, so the
#'   disambiguation point moves with delta (delayed C competition).
#'   \code{"truncate"} is signal-locked: the gate opens at \code{1 + delta}
#'   but the disambiguation point stays where the signal puts it
#'   (\code{1 + shared_segments * seg_steps}), so the competitor receives
#'   input only for the part of its shared segment that is still unfolding
#'   after the gate opens; a delta of at least one segment removes its
#'   support entirely (attenuated or abolished C competition). The control's
#'   input window follows the competitor's disambiguation point in both
#'   modes.
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
                     seg_steps = 10,
                     residual_input = 0,
                     gate = c("delay", "truncate"),
                     c_weight = 0.5,
                     small_ctrl_type = NULL,
                     ...) {
  gate <- match.arg(gate)
  if (is.null(small_ctrl_type)) small_ctrl_type <- small_type

  # --- Step 1: compute word-level phonological overlaps --------------------
  # Competitor overlaps are used at full strength; control overlaps are
  # multiplied by ctrl_scaling to approximate the weak baseline activation
  # of a phonologically unrelated word.
  ovl_large_comp <- compute_overlap(target_onset, large_comp_onset, large_type,
                                    features = features,
                                    similarity_matrix = similarity_matrix, c_weight = c_weight)
  ovl_large_ctrl <- compute_overlap(target_onset, large_ctrl_onset, large_type,
                                    features = features,
                                    similarity_matrix = similarity_matrix, c_weight = c_weight) *
    ctrl_scaling
  ovl_small_comp <- compute_overlap(target_onset, small_comp_onset, small_type,
                                    features = features,
                                    similarity_matrix = similarity_matrix, c_weight = c_weight)
  ovl_small_ctrl <- compute_overlap(target_onset, small_ctrl_onset, small_ctrl_type,
                                    features = features,
                                    similarity_matrix = similarity_matrix, c_weight = c_weight) *
    ctrl_scaling

  # --- Step 2: run the activation dynamics for each competitor pair --------
  # Larger-grain (prosodically aligned) competitors: gate always open, so
  # their input is available from the first time step (onset = 1).
  # Each competitor's input lasts for as long as the segments it shares with
  # the target take to unfold (the disambiguation point).
  n_large <- shared_segments(large_type, large_comp_onset)
  n_small <- shared_segments(small_type, small_comp_onset)
  large_out <- run_activation(ovl_large_comp, ovl_large_ctrl,
                               overlap_onset  = 1L,
                               overlap_offset = 1 + n_large * seg_steps,
                               residual_input = residual_input,
                               residual_steps = if (is.finite(seg_steps)) seg_steps else 0,
                               ...)
  # Smaller-grain (sub-prosodic) competitors: input withheld for the first
  # delta time steps (onset = 1 + delta), implementing the prosodic gate.
  # gate = "delay": the evidence is buffered and, once released, supports
  # the competitor for its full shared-segment duration.
  # gate = "truncate": the disambiguation point is fixed by the signal, so
  # the gate only cuts off the part of the shared segment heard before it.
  small_out <- run_activation(ovl_small_comp, ovl_small_ctrl,
                               overlap_onset  = 1L + delta,
                               overlap_offset = if (gate == "delay")
                                 1 + delta + n_small * seg_steps else
                                 1 + n_small * seg_steps,
                               residual_input = residual_input,
                               residual_steps = if (is.finite(seg_steps)) seg_steps else 0,
                               ...)

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


#' Number of Segments a Competitor Shares With the Target
#'
#' Returns how many phonological segments the overlap of a given type spans,
#' which (times \code{seg_steps}) sets how long a competitor receives
#' bottom-up input before the unfolding target diverges from it.
#'
#' @param overlap_type One of \code{"C"}, \code{"CV"}, \code{"CVC"},
#'   \code{"syllable"}.
#' @param competitor_onset Character vector of the competitor's transcribed
#'   segments (used for \code{"syllable"}).
#' @return Integer number of segments (C = 1, CV = 2, CVC = 3, syllable =
#'   number of transcribed segments, at least 1).
#' @export
#' @examples
#' shared_segments("CV")
#' shared_segments("syllable", c("b", "eh", "l"))
shared_segments <- function(overlap_type = c("C", "CV", "CVC", "syllable"),
                            competitor_onset = NULL) {
  overlap_type <- match.arg(overlap_type)
  switch(overlap_type,
    "C"   = 1L,
    "CV"  = 2L,
    "CVC" = 3L,
    "syllable" = max(1L, length(competitor_onset)))
}
