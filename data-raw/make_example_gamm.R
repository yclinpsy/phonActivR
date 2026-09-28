# =============================================================================
# make_example_gamm.R --- Generates the bundled `example_gamm` dataset
# =============================================================================
# This script creates the example GAMM-smooth dataset shipped with phonActivR
# (data/example_gamm.rda and inst/extdata/example_gamm_smooths.csv). The
# dataset has exactly the structure a researcher would export after fitting
# GAMMs to mouse-tracking data: one row per time bin per group, with separate
# CV and C competition-effect columns and their standard errors.
#
# The curves are generated from the phonActivR simulation itself (JP
# bilinguals: generating delta = 15; EN monolinguals: generating delta = 0)
# with temporally correlated noise, so that the tutorial's Application 1 can
# be run end-to-end by every reader without access to raw experimental data.
# The generating delta values are stated here and in ?example_gamm so that
# readers can verify that the workflow recovers them.
#
# Run from the package root:  Rscript data-raw/make_example_gamm.R
# (The block below also locates the package root automatically, so the
# script runs correctly from any working directory.)
# =============================================================================

# --- Locate the package root (the directory containing DESCRIPTION) ---------
# Works whether the script is run via Rscript from any directory or via
# source(); walks upward from the script's own location.
find_pkg_root <- function() {
  # Where is this script? (Rscript: from --file; source(): from sys.frames)
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  script_dir <- if (length(file_arg) == 1) {
    dirname(normalizePath(file_arg))
  } else {
    getwd()
  }
  dir <- script_dir
  for (i in 1:5) {
    if (file.exists(file.path(dir, "DESCRIPTION"))) return(dir)
    dir <- dirname(dir)
  }
  stop("Could not locate the phonActivR package root (no DESCRIPTION found).\n",
       "Run this script from the package root: Rscript data-raw/make_example_gamm.R")
}
setwd(find_pkg_root())

# Load the package functions (from source, so this works pre-installation)
pkgload::load_all(".", quiet = TRUE)

set.seed(1234)  # Reproducibility: the shipped dataset is exactly reproducible

# --- Step 1: run the simulation that defines the generating curves ----------
stim <- example_stimuli_jp()
sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30),
                       verbose = FALSE, seed = 1234)

# --- Step 2: generate the two groups' asymmetry curves ----------------------
# JP bilinguals: generating delta = 15 (moraic constraint present)
# EN monolinguals: generating delta = 0 (no prosodic constraint)
emp <- example_empirical(sim, true_delta = 15,
                         group_labels = c("JP bilinguals", "EN monolinguals"),
                         paradigm = "mouse-tracking", seed = 1234)

# --- Step 3: reshape into GAMM-export format --------------------------------
# Real GAMM exports contain the two component effects (CV and C), not the
# asymmetry directly. We reconstruct plausible component curves by pairing
# each group's asymmetry with the simulation's component curves at its
# generating delta, plus matched noise.
ts <- sim$params$time_steps
smooth_noise <- function(n, sd_val) {
  raw <- stats::rnorm(n, 0, sd_val)
  k <- rep(1 / 5, 5)
  sm <- stats::filter(raw, k, sides = 2)
  sm[is.na(sm)] <- raw[is.na(sm)]
  as.numeric(sm)
}

make_group <- function(gen_delta_lo, gen_delta_hi, w, label) {
  lo <- sim$results[[as.character(gen_delta_lo)]]
  hi <- sim$results[[as.character(gen_delta_hi)]]
  cv <- (1 - w) * lo$large_effect + w * hi$large_effect
  cc <- (1 - w) * lo$small_effect + w * hi$small_effect
  data.frame(
    time      = seq_len(ts),
    CV_effect = cv + smooth_noise(ts, 0.010),
    C_effect  = cc + smooth_noise(ts, 0.010),
    CV_se     = abs(cv) * 0.20 + 0.006 + abs(smooth_noise(ts, 0.002)),
    C_se      = abs(cc) * 0.20 + 0.006 + abs(smooth_noise(ts, 0.002)),
    group     = label,
    stringsAsFactors = FALSE
  )
}

example_gamm <- rbind(
  make_group(10, 20, 0.5, "JP bilinguals"),   # generating delta = 15
  make_group(0,  0,  0.0, "EN monolinguals")  # generating delta = 0
)

# --- Step 4: save into the package -------------------------------------------
dir.create("data", showWarnings = FALSE)
save(example_gamm, file = "data/example_gamm.rda", compress = "bzip2")
utils::write.csv(example_gamm, "inst/extdata/example_gamm_smooths.csv",
                 row.names = FALSE)
cat("Wrote data/example_gamm.rda and inst/extdata/example_gamm_smooths.csv\n")
cat("Rows:", nrow(example_gamm), "| Groups:",
    paste(unique(example_gamm$group), collapse = ", "), "\n")
