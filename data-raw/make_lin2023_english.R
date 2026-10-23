# =============================================================================
# make_lin2023_english.R --- Builds the bundled `lin2023_english` dataset
# =============================================================================
# Source: Lin, Y.-C., Lin, P.-Y., & Yeh, L.-H. (2023). Syllable or phoneme?
# A mouse-tracking investigation of phonological units in Mandarin Chinese
# and English spoken word recognition. Journal of Experimental Psychology:
# Learning, Memory, and Cognition, 49(1), 130-176.
# https://doi.org/10.1037/xlm0001128
#
# Experiments 3 and 4 of that study used the 42 English stimulus items that
# phonActivR ships as example_stimuli_jp(): Experiment 3 tested
# Chinese-English bilinguals (n = 41) and Experiment 4 English monolinguals
# (n = 49). The two CSV files in inst/extdata hold the group-mean competition
# effects at 101 normalized time steps (correct trials only):
#   cv_effect = CV control x-coordinate - CV competitor x-coordinate
#   c_effect  = C  control x-coordinate - C  competitor x-coordinate
# Positive values mean the trajectory was attracted toward the competitor.
# These are group means; no participant-level data are included.
#
# Run from the package root:  Rscript data-raw/make_lin2023_english.R
# =============================================================================
exp3 <- utils::read.csv("inst/extdata/lin2023_exp3_bilinguals_english.csv")
exp4 <- utils::read.csv("inst/extdata/lin2023_exp4_monolinguals_english.csv")

build <- function(d, label, n) {
  data.frame(time       = d$time,
             cv_effect  = d$cv_effect,
             c_effect   = d$c_effect,
             asymmetry  = d$cv_effect - d$c_effect,
             group      = label,
             n_subjects = n,
             stringsAsFactors = FALSE)
}
lin2023_english <- rbind(
  build(exp3, "Chinese-English bilinguals", 41L),
  build(exp4, "English monolinguals",       49L))

save(lin2023_english, file = "data/lin2023_english.rda", compress = "bzip2")
cat("Wrote data/lin2023_english.rda:", nrow(lin2023_english), "rows\n")
