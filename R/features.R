# =============================================================================
# phonActivR: Phoneme Feature Matrix & Similarity Functions
# =============================================================================

#' Binary Phoneme Feature Matrix (Adaptation of the TRACE II Dimensions)
#'
#' A phoneme feature matrix built on the seven feature dimensions of TRACE II
#' (McClelland & Elman, 1986): Power (Pow), Vocalic (Voc), Diffuse (Dif),
#' Acute (Acu), Consonantal (Con), Voiced (Voi), Burst (Bur).
#'
#' @section Provenance (please cite accurately):
#' This matrix is a \strong{binary adaptation}, not a reproduction, of the
#' McClelland-Elman feature system. The original TRACE II specification
#' treats each dimension as a continuum divided into eight value ranges
#' (values 1-8) and covers only the 15 phonemes used in that model's
#' simulations (McClelland & Elman, 1986, Tables 1-2). phonActivR instead
#' assigns a simplified binary value (+1/-1) on each of the same seven
#' dimensions, with assignments constructed by the package authors to cover
#' 24 consonants and 15 vowels in CMU Pronouncing Dictionary notation. The
#' matrix is used solely as a similarity metric (proportion of matching
#' values in \code{\link{phoneme_similarity}}); it is not a feature layer,
#' and no claim is made that it reproduces TRACE's similarity structure.
#' For phonemes or contrasts it cannot express, supply direct pairwise
#' values via \code{\link{custom_similarity}}.
#'
#' @return A named list of numeric vectors, each of length 7 (+1/-1 values).
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


#' Build a Custom Phoneme Similarity Lookup From Pairwise Values
#'
#' Constructs a symmetric similarity lookup matrix from user-specified pairwise
#' similarity values. This is the recommended, user-friendly route for
#' researchers working with phonemes that are not covered by the
#' McClelland-Elman feature matrix (which was designed for English): rather
#' than constructing a full binary feature matrix, users directly enter
#' similarity values between 0 and 1 for exactly the phoneme pairs that occur
#' in their stimulus set.
#'
#' @param pairs A data.frame with three columns: \code{p1} (character, first
#'   phoneme), \code{p2} (character, second phoneme), and \code{similarity}
#'   (numeric in [0, 1]). Each row specifies the similarity for one unordered
#'   phoneme pair; the pair is stored symmetrically, so \code{(p1, p2)} and
#'   \code{(p2, p1)} need not both be listed. Self-similarities default to 1
#'   and need not be listed.
#'
#' @return A symmetric numeric matrix with phoneme labels as row and column
#'   names, suitable for the \code{similarity_matrix} argument of
#'   \code{\link{phoneme_similarity}}, \code{\link{compute_overlap}}, and
#'   \code{\link{run_simulation}}. Pairs not specified are \code{NA} and fall
#'   back to the feature-based computation (or \code{default_similarity}).
#' @export
#' @examples
#' # A Korean-oriented example: directly specify similarity for phonemes
#' # outside the built-in English feature set
#' my_pairs <- data.frame(
#'   p1         = c("k*", "k*", "t*"),
#'   p2         = c("k",  "g",  "t"),
#'   similarity = c(0.85, 0.60, 0.85)
#' )
#' sim_mat <- custom_similarity(my_pairs)
#' phoneme_similarity("k*", "k", similarity_matrix = sim_mat)  # 0.85
custom_similarity <- function(pairs) {
  # --- Input validation: require the three named columns -------------------
  stopifnot(
    "pairs must be a data.frame" = is.data.frame(pairs),
    "pairs must have columns p1, p2, similarity" =
      all(c("p1", "p2", "similarity") %in% names(pairs)),
    "similarity values must lie in [0, 1]" =
      all(pairs$similarity >= 0 & pairs$similarity <= 1)
  )

  # --- Collect every phoneme label mentioned in either column --------------
  phones <- unique(c(as.character(pairs$p1), as.character(pairs$p2)))

  # --- Initialize an NA-filled square matrix; NA means "not specified",
  #     which signals downstream code to fall back to the feature matrix ----
  mat <- matrix(NA_real_, nrow = length(phones), ncol = length(phones),
                dimnames = list(phones, phones))

  # --- Self-similarity is 1 by definition ----------------------------------
  diag(mat) <- 1

  # --- Fill in each user-specified pair symmetrically ----------------------
  for (i in seq_len(nrow(pairs))) {
    a <- as.character(pairs$p1[i])
    b <- as.character(pairs$p2[i])
    mat[a, b] <- pairs$similarity[i]
    mat[b, a] <- pairs$similarity[i]
  }
  mat
}


#' Compute Phoneme Similarity Using Features or a Custom Similarity Matrix
#'
#' Calculates the similarity between two phonemes. By default, this is the
#' proportion of matching feature values in the McClelland-Elman (1986)
#' 7-feature matrix, returning a value in [0, 1]. Alternatively, a
#' user-supplied \code{similarity_matrix} (see \code{\link{custom_similarity}})
#' takes precedence, allowing direct specification of cross-linguistic
#' similarities for phonemes outside the built-in English feature set.
#'
#' @section Cross-linguistic use:
#' The built-in feature matrix covers 24 English consonants and 15 vowels.
#' Phonemes outside this set (e.g., Korean tense stops, palatal nasals,
#' ejectives, clicks) receive \code{default_similarity} with \emph{any} other
#' phoneme, which flattens real similarity structure. For any language whose
#' phonemes are not in \code{names(trace_features())}, supply either a custom
#' \code{features} matrix or -- more simply -- direct pairwise values via
#' \code{similarity_matrix = custom_similarity(...)}. \code{\link{run_simulation}}
#' warns when stimulus phonemes are missing from the active feature set.
#'
#' @param p1 Character. First phoneme in CMU/TRACE notation (e.g., "b", "ae").
#' @param p2 Character. Second phoneme.
#' @param features Optional. A custom feature matrix (named list of numeric vectors).
#'   Defaults to \code{trace_features()}.
#' @param default_similarity Numeric. Value returned when a phoneme is not found
#'   in the feature matrix (and no \code{similarity_matrix} entry exists).
#'   Default is 0.3 (conservative estimate).
#' @param similarity_matrix Optional. A symmetric numeric matrix of direct
#'   pairwise similarities (from \code{\link{custom_similarity}}). When the
#'   requested pair has a non-\code{NA} entry, that value is returned and the
#'   feature matrix is bypassed.
#'
#' @return Numeric value in [0, 1] representing phoneme similarity.
#' @export
#' @examples
#' phoneme_similarity("b", "p")  # High similarity (differ only in voicing)
#' phoneme_similarity("b", "s")  # Lower similarity
#' phoneme_similarity("ae", "eh") # Vowel comparison
phoneme_similarity <- function(p1, p2,
                                features = trace_features(),
                                default_similarity = 0.3,
                                similarity_matrix = NULL) {
  # --- Route 1: direct user-specified similarity takes precedence ----------
  # If a similarity matrix was supplied and contains a non-NA entry for this
  # pair, return it immediately (bypassing the feature computation).
  if (!is.null(similarity_matrix) &&
      p1 %in% rownames(similarity_matrix) &&
      p2 %in% colnames(similarity_matrix)) {
    val <- similarity_matrix[p1, p2]
    if (!is.na(val)) return(val)
  }

  # --- Route 2: feature-based computation ----------------------------------
  # If either phoneme is missing from the feature matrix, fall back to the
  # conservative default (see the Cross-linguistic use section above).
  if (!(p1 %in% names(features)) || !(p2 %in% names(features))) {
    return(default_similarity)
  }
  # Similarity = proportion of the 7 feature values that match.
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
#' @param similarity_matrix Optional. A symmetric matrix of direct pairwise
#'   phoneme similarities (from \code{\link{custom_similarity}}), passed to
#'   \code{\link{phoneme_similarity}}. Entries in this matrix take precedence
#'   over the feature-based computation.
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
                            features = trace_features(),
                            similarity_matrix = NULL) {
  overlap_type <- match.arg(overlap_type)

  # Small wrapper so every phoneme comparison below uses the same feature
  # matrix and (optional) custom similarity lookup.
  psim <- function(a, b) {
    phoneme_similarity(a, b, features = features,
                       similarity_matrix = similarity_matrix)
  }

  switch(overlap_type,
    "C" = {
      # Consonant-only overlap: compare just the first (onset) consonant.
      # The result is scaled by 0.5 because only one of the two onset
      # positions (C but not V) is shared with the target.
      psim(target_onset[1], competitor_onset[1]) * 0.5
    },
    "CV" = {
      # Consonant-vowel overlap: average the similarity of the onset
      # consonant pair and the vowel pair (equal weighting).
      c_sim <- psim(target_onset[1], competitor_onset[1])
      v_sim <- if (length(target_onset) > 1 && length(competitor_onset) > 1) {
        psim(target_onset[2], competitor_onset[2])
      } else { 0.5 }  # If a vowel is missing, use a neutral 0.5
      (c_sim + v_sim) / 2.0
    },
    "CVC" = {
      # CVC overlap: mean similarity over the first three segment positions
      # (or fewer, if either transcription is shorter).
      sims <- mapply(psim,
                     target_onset[seq_len(min(3, length(target_onset)))],
                     competitor_onset[seq_len(min(3, length(competitor_onset)))])
      mean(sims)
    },
    "syllable" = {
      # Full-syllable overlap: mean position-wise similarity over the shared
      # segment positions, down-weighted by the proportion of segments the
      # two words have in common (length mismatch penalty).
      n_seg <- min(length(target_onset), length(competitor_onset))
      sims <- mapply(psim,
                     target_onset[seq_len(n_seg)],
                     competitor_onset[seq_len(n_seg)])
      mean(sims) * (n_seg / max(length(target_onset),
                                 length(competitor_onset)))
    }
  )
}


#' Check Phoneme Coverage of a Stimulus Set
#'
#' Reports which phonemes in a stimulus set are not covered by the active
#' feature matrix (or custom similarity matrix). Uncovered phonemes silently
#' receive \code{default_similarity} (0.3) with every other phoneme, which
#' flattens real similarity structure and can make different stimulus sets
#' look artificially alike -- an important pitfall for cross-linguistic work.
#'
#' @param stimuli A \code{"phonActivR_stimuli"} object from
#'   \code{\link{create_stimuli}}.
#' @param features A feature matrix (default \code{trace_features()}).
#' @param similarity_matrix Optional custom similarity matrix
#'   (from \code{\link{custom_similarity}}).
#' @param warn Logical. Emit a true R \code{warning()} listing uncovered
#'   phonemes (default TRUE). This warning is independent of any verbosity
#'   setting, so the default-similarity fallback can never operate silently:
#'   \code{\link{run_simulation}} runs this check with \code{warn = TRUE}
#'   even when called with \code{verbose = FALSE}.
#' @param announce Logical. Print a success message when coverage is complete
#'   (default: same as \code{warn}; \code{run_simulation()} ties this to its
#'   \code{verbose} argument so quiet batch runs stay quiet on success).
#'
#' @return Invisibly, a character vector of uncovered phoneme labels. When
#'   a \code{similarity_matrix} is supplied, the result also carries a
#'   \code{"missing_pairs"} attribute listing compared phoneme pairs that
#'   would silently receive the default similarity (see Details).
#'   (length 0 when coverage is complete).
#' @export
#' @examples
#' stim <- example_stimuli_jp()
#' check_phoneme_coverage(stim)  # Complete coverage for the built-in set
check_phoneme_coverage <- function(stimuli,
                                   features = trace_features(),
                                   similarity_matrix = NULL,
                                   warn = TRUE,
                                   announce = warn) {
  stopifnot(inherits(stimuli, "phonActivR_stimuli"))

  # All phoneme labels used anywhere in the stimulus transcriptions
  used <- unique(unlist(stimuli$onsets, use.names = FALSE))

  # A phoneme is covered if it is in the feature matrix OR appears in the
  # custom similarity matrix
  covered <- used %in% names(features)
  if (!is.null(similarity_matrix)) {
    covered <- covered | used %in% rownames(similarity_matrix)
  }
  missing <- setdiff(used[!covered], "?")

  if (warn && length(missing) > 0) {
    # A genuine R warning (not a console message), so it surfaces regardless
    # of verbosity settings and can be caught or escalated by callers.
    warning(
      length(missing), " phoneme(s) not covered by the feature matrix: ",
      paste(missing, collapse = ", "),
      ". These will receive the default similarity (0.3) with ALL other ",
      "phonemes, which flattens similarity structure. For non-English ",
      "phonemes, supply direct pairwise values via ",
      "similarity_matrix = custom_similarity(...) or a custom `features` matrix.",
      call. = FALSE
    )
  } else if (announce && length(missing) == 0) {
    cli::cli_alert_success(
      "All {length(used)} phonemes covered by the active feature/similarity set."
    )
  }

  # --- Pair-level check for custom similarity matrices -----------------------
  # Per-phoneme coverage is necessary but not sufficient: a phoneme can be
  # "covered" because it appears somewhere in similarity_matrix, yet a
  # SPECIFIC compared pair involving it may have no matrix entry. Such a
  # pair silently receives default_similarity (0.3) in phoneme_similarity()
  # (Route 1 misses; Route 2 fails because the phoneme is not in the feature
  # matrix). Here we enumerate the aligned phoneme pairs the engine will
  # actually compare for these stimuli and warn about any pair that (a)
  # involves at least one non-feature-matrix phoneme and (b) has no non-NA
  # entry in similarity_matrix -- the silent-0.3 case a cross-linguistic
  # user would otherwise never see.
  missing_pairs <- character(0)
  if (!is.null(similarity_matrix)) {
    items <- stimuli$items
    pair_set <- new.env(parent = emptyenv())
    add_pairs <- function(w1, w2) {
      o1 <- stimuli$onsets[[w1]]; o2 <- stimuli$onsets[[w2]]
      if (is.null(o1) || is.null(o2)) return(invisible())
      for (k in seq_len(min(length(o1), length(o2)))) {
        a <- o1[k]; b <- o2[k]
        key <- paste(sort(c(a, b)), collapse = "-")
        assign(key, c(a, b), envir = pair_set)
      }
    }
    for (i in seq_len(nrow(items))) {
      for (col in c("large_comp", "large_ctrl", "small_comp", "small_ctrl")) {
        add_pairs(items$target[i], items[[col]][i])
      }
    }
    for (key in ls(pair_set)) {
      ab <- get(key, envir = pair_set)
      a <- ab[1]; b <- ab[2]
      in_feat <- (a %in% names(features)) && (b %in% names(features))
      if (in_feat) next  # feature route handles it
      has_entry <- a %in% rownames(similarity_matrix) &&
        b %in% colnames(similarity_matrix) &&
        !is.na(similarity_matrix[a, b])
      if (!has_entry) missing_pairs <- c(missing_pairs, key)
    }
    if (warn && length(missing_pairs) > 0) {
      shown <- utils::head(sort(missing_pairs), 8)
      warning(
        length(missing_pairs), " compared phoneme pair(s) involve a phoneme ",
        "outside the feature matrix but have NO entry in similarity_matrix ",
        "(e.g., ", paste(shown, collapse = ", "),
        if (length(missing_pairs) > 8) ", ..." else "",
        "). These pairs silently receive the default similarity (0.3). ",
        "Add them to custom_similarity() -- a symptom of this problem is a ",
        "max_asymmetry column that is flat across delta values.",
        call. = FALSE
      )
    }
  }

  invisible(structure(missing, missing_pairs = missing_pairs))
}
