# =============================================================================
# make_lin2023_chinese.R --- Builds the bundled `lin2023_chinese` dataset
# =============================================================================
# Source: Lin, Lin, & Yeh (2023), Experiment 1: Chinese-English bilinguals
# (n = 41) listening to 24 Mandarin monosyllables (example_stimuli_zh()).
# inst/extdata/lin2023_exp1_bilinguals_chinese.csv holds the group-mean
# effects (control minus competitor x-coordinate, correct trials only,
# trial-weighted means; CV effect from the 12 CV targets, C effect from the
# 12 C targets). Run from the package root:
#   Rscript data-raw/make_lin2023_chinese.R
# =============================================================================
exp1 <- utils::read.csv("inst/extdata/lin2023_exp1_bilinguals_chinese.csv")
lin2023_chinese <- data.frame(time = exp1$time, cv_effect = exp1$cv_effect,
                              c_effect = exp1$c_effect,
                              asymmetry = exp1$cv_effect - exp1$c_effect,
                              group = "Chinese-English bilinguals (Mandarin)",
                              n_subjects = 41L, stringsAsFactors = FALSE)
save(lin2023_chinese, file = "data/lin2023_chinese.rda", compress = "bzip2")
cat("Wrote data/lin2023_chinese.rda:", nrow(lin2023_chinese), "rows\n")
