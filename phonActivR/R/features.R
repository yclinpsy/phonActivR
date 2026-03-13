# =============================================================================
# phonActivR: Phoneme Feature Matrix & Similarity Functions
# =============================================================================

#' TRACE 7-Feature Phoneme Matrix
#'
#' The complete phoneme feature matrix from McClelland & Elman (1986, Table 1).
#' Features are: Power (Pow), Vocalic (Voc), Diffuse (Dif), Acute (Acu),
#' Consonantal (Con), Voiced (Voi), Burst (Bur). Values are +1 (present) or
#' -1 (absent).
#'
#' @return A named list of numeric vectors, each of length 7.
#' @references
#' McClelland, J. L., & Elman, J. L. (1986). The TRACE model of speech
#' perception. \emph{Cognitive Psychology}, 18(1), 1--86.
#' @export
#' @examples
#' features <- trace_features()
#' features[["b"]]
#' # [1] -1 -1  1 -1  1  1  1
trace_features <- function() {
  list(
    # Consonants
    #        Pow  Voc  Dif  Acu  Con  Voi  Bur
    "p"  = c( -1,  -1,  +1,  -1,  +1,  -1,  +1),
    "b"  = c( -1,  -1,  +1,  -1,  +1,  +1,  +1),
    "t"  = c( -1,  -1,  -1,  +1,  +1,  -1,  +1),
    "d"  = c( -1,  -1,  -1,  +1,  +1,  +1,  +1),
    "k"  = c( -1,  -1,  -1,  -1,  +1,  -1,  +1),
    "g"  = c( -1,  -1,  -1,  -1,  +1,  +1,  +1),
    "f"  = c( -1,  -1,  +1,  +1,  +1,  -1,  -1),
    "v"  = c( -1,  -1,  +1,  +1,  +1,  +1,  -1),
    "s"  = c( -1,  -1,  -1,  +1,  +1,  -1,  -1),
    "z"  = c( -1,  -1,  -1,  +1,  +1,  +1,  -1),
    "sh" = c( -1,  -1,  +1,  -1,  +1,  -1,  -1),
    "zh" = c( -1,  -1,  +1,  -1,  +1,  +1,  -1),
    "m"  = c( -1,  +1,  +1,  -1,  +1,  +1,  -1),
    "n"  = c( -1,  +1,  -1,  +1,  +1,  +1,  -1),
    "ng" = c( -1,  +1,  -1,  -1,  +1,  +1,  -1),
    "l"  = c( +1,  +1,  +1,  +1,  +1,  +1,  -1),
    "r"  = c( +1,  +1,  -1,  +1,  +1,  +1,  -1),
    "w"  = c( +1,  +1,  +1,  -1,  -1,  +1,  -1),
    "y"  = c( +1,  +1,  -1,  +1,  -1,  +1,  -1),
    "hh" = c( -1,  -1,  -1,  -1,  -1,  -1,  -1),
    "ch" = c( -1,  -1,  +1,  -1,  +1,  -1,  +1),
    "jh" = c( -1,  -1,  +1,  -1,  +1,  +1,  +1),
    "th" = c( -1,  -1,  +1,  +1,  +1,  -1,  -1),
    "dh" = c( -1,  -1,  +1,  +1,  +1,  +1,  -1),
    # Vowels
    "ae" = c( +1,  +1,  +1,  +1,  -1,  +1,  -1),
    "ah" = c( +1,  +1,  -1,  -1,  -1,  +1,  -1),
    "ao" = c( +1,  +1,  -1,  -1,  -1,  +1,  -1),
    "aw" = c( +1,  +1,  -1,  -1,  -1,  +1,  -1),
    "ay" = c( +1,  +1,  +1,  +1,  -1,  +1,  -1),
    "eh" = c( +1,  +1,  -1,  +1,  -1,  +1,  -1),
    "er" = c( +1,  +1,  -1,  +1,  -1,  +1,  -1),
    "ey" = c( +1,  +1,  -1,  +1,  -1,  +1,  -1),
    "ih" = c( +1,  +1,  +1,  +1,  -1,  +1,  -1),
    "iy" = c( +1,  +1,  +1,  +1,  -1,  +1,  -1),
    "ow" = c( +1,  +1,  -1,  -1,  -1,  +1,  -1),
    "oy" = c( +1,  +1,  -1,  -1,  -1,  +1,  -1),
    "uh" = c( +1,  +1,  -1,  -1,  -1,  +1,  -1),
    "uw" = c( +1,  +1,  -1,  -1,  -1,  +1,  -1),
    "aa" = c( +1,  +1,  -1,  -1,  -1,  +1,  -1)
  )
}


#' Compute Phoneme Similarity Using TRACE Features
#'
#' Calculates the proportion of matching feature values between two phonemes
#' using the TRACE 7-feature matrix. Returns a value in [0, 1].
#'
#' @param p1 Character. First phoneme in CMU/TRACE notation (e.g., "b", "ae").
#' @param p2 Character. Second phoneme.
#' @param features Optional. A custom feature matrix (named list of numeric vectors).
#'   Defaults to \code{trace_features()}.
#' @param default_similarity Numeric. Value returned when a phoneme is not found
#'   in the feature matrix. Default is 0.3 (conservative estimate).
#'
#' @return Numeric value in [0, 1] representing feature overlap proportion.
#' @export
#' @examples
#' phoneme_similarity("b", "p")  # High similarity (differ only in voicing)
#' phoneme_similarity("b", "s")  # Lower similarity
#' phoneme_similarity("ae", "eh") # Vowel comparison
phoneme_similarity <- function(p1, p2,
                                features = trace_features(),
                                default_similarity = 0.3) {
  if (!(p1 %in% names(features)) || !(p2 %in% names(features))) {
    return(default_similarity)
  }
  n_features <- length(features[[p1]])
  sum(features[[p1]] == features[[p2]]) / n_features
}


#' Compute Phonological Overlap Between Two Words
#'
#' Calculates phonological overlap between a target word and a competitor/control
#' word based on their onset transcriptions and overlap type. Uses the TRACE
#' 7-feature matrix for phoneme-level similarity computation.
#'
#' @param target_onset Character vector. Phoneme transcription of the target
#'   word onset (e.g., \code{c("b", "eh")} for "bench").
#' @param competitor_onset Character vector. Phoneme transcription of the
#'   competitor/control word onset.
#' @param overlap_type Character. One of:
#'   \describe{
#'     \item{"C"}{Consonant-only overlap (single onset consonant shared)}
#'     \item{"CV"}{Consonant-vowel overlap (onset CV unit shared)}
#'     \item{"CVC"}{Consonant-vowel-consonant overlap}
#'     \item{"syllable"}{Full syllable overlap (all segments compared)}
#'   }
#' @param features Optional. Custom feature matrix. Defaults to \code{trace_features()}.
#'
#' @return Numeric value in [0, 1] representing phonological overlap.
#' @export
#' @examples
#' # CV overlap: "bench" (b, eh) vs "bell" (b, eh) -> high overlap
#' compute_overlap(c("b", "eh"), c("b", "eh"), "CV")
#'
#' # C overlap: "bench" (b, eh) vs "bark" (b) -> consonant only
#' compute_overlap(c("b", "eh"), c("b"), "C")
compute_overlap <- function(target_onset, competitor_onset,
                            overlap_type = c("C", "CV", "CVC", "syllable"),
                            features = trace_features()) {
  overlap_type <- match.arg(overlap_type)

  switch(overlap_type,
    "C" = {
      # Only first consonant compared, scaled by 0.5 (one of two onset
      # positions shared)
      phoneme_similarity(target_onset[1], competitor_onset[1],
                         features = features) * 0.5
    },
    "CV" = {
      c_sim <- phoneme_similarity(target_onset[1], competitor_onset[1],
                                   features = features)
      v_sim <- if (length(target_onset) > 1 && length(competitor_onset) > 1) {
        phoneme_similarity(target_onset[2], competitor_onset[2],
                           features = features)
      } else { 0.5 }
      (c_sim + v_sim) / 2.0
    },
    "CVC" = {
      sims <- mapply(phoneme_similarity,
                     target_onset[seq_len(min(3, length(target_onset)))],
                     competitor_onset[seq_len(min(3, length(competitor_onset)))],
                     MoreArgs = list(features = features))
      mean(sims)
    },
    "syllable" = {
      n_seg <- min(length(target_onset), length(competitor_onset))
      sims <- mapply(phoneme_similarity,
                     target_onset[seq_len(n_seg)],
                     competitor_onset[seq_len(n_seg)],
                     MoreArgs = list(features = features))
      mean(sims) * (n_seg / max(length(target_onset),
                                 length(competitor_onset)))
    }
  )
}
