# =============================================================================
# phonActivR: jTRACE Correspondence Validation Script
# =============================================================================
# This script documents the quantitative correspondence check between
# phonActivR competition onset timings and jTRACE simulation outputs.
#
# As reported in the tutorial (Validation section):
#   - Pearson r = .97 for CV competitors (across 42 items)
#   - Pearson r = .95 for C competitors
#   - Parameters: alpha=0.08, gamma=0.04, decay=0.02, threshold=0.04
#
# To reproduce:
#   1. Run jTRACE simulations for the 42-item stimulus set via the jtracer
#      R interface (Gong Castro et al., 2023), saving onset timings per item.
#   2. Run phonActivR simulations with matching parameters (below).
#   3. Compute Pearson correlations between onset timings from both models.
#
# NOTE: Full reproduction requires a local jTRACE installation and the
#       jtracer R package. phonActivR simulations below run standalone.
# =============================================================================

library(phonActivR)

# --- Step 1: Run phonActivR simulation with TRACE-matched parameters ----------
stim <- example_stimuli_jp()

sim <- run_simulation(
  stim,
  delta_values  = c(0),       # correspondence check at delta = 0
  alpha         = 0.08,       # matches TRACE default activation rate
  gamma         = 0.04,       # matches TRACE lateral inhibition
  decay         = 0.02,       # matches TRACE decay
  threshold     = 0.04,       # onset detection threshold
  verbose       = TRUE,
  seed          = 1234
)

# --- Step 2: Extract phonActivR onset timings per item -----------------------
# Item-level onset timings are in sim$item_results
# (Aggregate onsets are in sim$onsets)
cat("phonActivR simulation onset summary:\n")
print(sim$onsets)

# --- Step 3: Compare against jTRACE (requires jtracer + jTRACE) --------------
# If you have jtracer installed, uncomment and run:
#
# library(jtracer)
# jtrace_onsets_cv <- # run jTRACE for each item's CV competitor, extract onset
# jtrace_onsets_c  <- # run jTRACE for each item's C competitor, extract onset
#
# phonactivr_cv <- sim_cv$item_onset_times   # CV competition onset per item
# phonactivr_c  <- sim_c$item_onset_times    # C competition onset per item
#
# cor_cv <- cor(phonactivr_cv, jtrace_onsets_cv, use = "complete.obs")
# cor_c  <- cor(phonactivr_c,  jtrace_onsets_c,  use = "complete.obs")
# cat(sprintf("CV correspondence: r = %.2f\n", cor_cv))
# cat(sprintf("C  correspondence: r = %.2f\n", cor_c))

cat("\nFor full reproduction, see tutorial Validation section and jtracer documentation.\n")
cat("Gong Castro, R. et al. (2023). jtracer. Behavior Research Methods, 55, 2718-2732.\n")
