# =============================================================================
# phonActivR: jTRACE Correspondence Script (template)
# =============================================================================
# This script provides the phonActivR half of a quantitative correspondence
# check between phonActivR competition onset timings and jTRACE simulation
# outputs. The tutorial's Validation 6 reports a completed item-level
# correspondence study against the ORIGINAL C implementation of TRACE
# (see inst/extdata/trace_correspondence/, which reproduces every number
# in that section). This template covers the analogous check against
# jTRACE (the Java reimplementation): the tutorial does not report jTRACE
# correlations; it ships this script so that researchers with access to
# jTRACE can compute them for their own stimuli as an additional layer of
# convergent validity.
#
# To perform the full check:
#   1. Run the phonActivR simulation below (runs standalone).
#   2. Run jTRACE simulations for the same 42-item stimulus set via the
#      jtracer R interface (Garcia-Castro & Siow, 2021), extracting the
#      competitor-activation onset time for each item.
#   3. Correlate the per-item onset timings from the two models (Step 3
#      below shows the exact code to uncomment once jTRACE output is in
#      hand).
#
# NOTE: Step 2 requires a local jTRACE installation and the jtracer package
#       (https://github.com/gongcastro/jtracer). Steps 1 and this script's
#       phonActivR output run with phonActivR alone.
# =============================================================================

# --- Load phonActivR (installed copy, or from source if run from the
#     package tree) -----------------------------------------------------------
if (!requireNamespace("phonActivR", quietly = TRUE)) {
  if (file.exists("DESCRIPTION") &&
      requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(".", quiet = TRUE)
  } else {
    stop("phonActivR is not installed. Install it first:\n",
         "  devtools::install_github('yclinpsy/phonActivR')")
  }
} else {
  library(phonActivR)
}

# --- Step 1: Run phonActivR simulation with TRACE-comparable parameters ------
stim <- example_stimuli_jp()

sim <- run_simulation(
  stim,
  delta_values  = c(0),       # correspondence check at delta = 0 (baseline
                              # architecture, before the delta extension)
  alpha         = 0.08,       # package default activation growth rate
  gamma         = 0.04,       # package default lateral inhibition
  decay         = 0.02,       # package default passive decay
  threshold     = 0.04,       # onset detection threshold
  verbose       = TRUE,
  seed          = 1234
)

# --- Step 2: Extract phonActivR onset timings --------------------------------
# Aggregate onsets are in sim$onsets; item-level competition-effect
# matrices (rows = items, columns = time steps) are in
# sim$item_results[["0"]]$large_matrix / $small_matrix. Applying
# find_onset() to each item's row gives the per-item onset timings that
# the correspondence correlation uses.
cat("phonActivR simulation onset summary:\n")
print(sim$onsets)

item_mats <- sim$item_results[["0"]]
phonactivr_cv <- apply(item_mats$large_matrix, 1, find_onset,
                       threshold = 0.04)
phonactivr_c  <- apply(item_mats$small_matrix, 1, find_onset,
                       threshold = 0.04)
cat("\nPer-item onsets extracted:",
    sum(!is.na(phonactivr_cv)), "CV and",
    sum(!is.na(phonactivr_c)), "C values across",
    nrow(item_mats$large_matrix), "items.\n")

# --- Step 3: Compare against jTRACE (requires jtracer + jTRACE) --------------
# Once you have per-item jTRACE onset timings for the same items, uncomment:
#
# jtrace_onsets_cv <- # numeric vector: jTRACE onset per item, CV competitor
# jtrace_onsets_c  <- # numeric vector: jTRACE onset per item, C competitor
#
# cor_cv <- cor(phonactivr_cv, jtrace_onsets_cv, use = "complete.obs")
# cor_c  <- cor(phonactivr_c,  jtrace_onsets_c,  use = "complete.obs")
# cat(sprintf("CV correspondence: r = %.2f\n", cor_cv))
# cat(sprintf("C  correspondence: r = %.2f\n", cor_c))

cat("\nFor the full procedure, see the tutorial's Validation 6 and the\n")
cat("jtracer documentation: Garcia-Castro, G., & Siow, S. (2021). jtracer:\n")
cat("An R interface to jTRACE [Computer software]. GitHub.\n")
cat("https://github.com/gongcastro/jtracer\n")
