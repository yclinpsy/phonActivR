#' phonActivR: Phonological Activation Simulator for Spoken Word Recognition
#'
#' A lightweight, pure-R interactive activation simulator for modeling
#' phonological competition during spoken word recognition. Implements a
#' TRACE-style architecture with built-in phonological overlap computation,
#' customizable prosodic constraint parameters, and tools for overlaying
#' predictions onto empirical time-course data.
#'
#' @section Competing-Hypothesis Framework:
#' The delta (delta) parameter operationalizes competing hypotheses about
#' cross-linguistic prosodic transfer. Setting \code{delta = 0} generates
#' predictions under the universalist (phoneme-centric) hypothesis; setting
#' \code{delta > 0} generates predictions under the language-specific grain
#' size hypothesis, with the magnitude of delta encoding the strength of the
#' prosodic bottleneck. This reframes hypothesis testing from a binary
#' question into a quantitative estimation problem: which predicted curve
#' family -- the universalist curve or a prosodically constrained curve --
#' better matches the observed data (cf. Henninger et al., 2025)?
#' Generating delta prediction curves before data collection constitutes a
#' form of quantitative pre-registration of effect structure (specifying
#' not only the direction but the temporal shape of the expected competition
#' asymmetry), consistent with TOP Guidelines Level 2.
#'
#' @section Core Workflow:
#' \enumerate{
#'   \item Define stimuli with \code{\link{create_stimuli}}
#'   \item Run simulation with \code{\link{run_simulation}}
#'   \item Visualize predictions with \code{\link{plot_competition}},
#'     \code{\link{plot_asymmetry}}, \code{\link{plot_onset_timing}}
#'   \item Overlay empirical data with \code{\link{overlay_empirical}}
#' }
#'
#' @section Key Functions:
#' \describe{
#'   \item{\code{\link{trace_features}}}{TRACE 7-feature phoneme matrix}
#'   \item{\code{\link{phoneme_similarity}}}{Feature-based phoneme similarity}
#'   \item{\code{\link{compute_overlap}}}{Phonological overlap computation}
#'   \item{\code{\link{run_activation}}}{Single-pair activation dynamics}
#'   \item{\code{\link{run_item}}}{Single-item two-condition simulation}
#'   \item{\code{\link{run_simulation}}}{Full multi-item, multi-delta simulation}
#'   \item{\code{\link{find_onset}}}{Competition onset detection}
#' }
#'
#' @docType package
#' @name phonActivR-package
"_PACKAGE"
