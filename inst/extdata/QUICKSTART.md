# phonActivR Quick-Start Guide

## Installation

```r
# Install from GitHub (requires devtools)
install.packages("devtools")  # if not already installed
devtools::install_github("yclinpsy/phonActivR@v0.3.0", build_vignettes = TRUE)
library(phonActivR)
```

## 5-Minute Demo (copy-paste and run)

```r
library(phonActivR)
library(ggplot2)   # for ggsave()

# Load example stimuli and run simulation (deterministic: no seed needed)
stim <- example_stimuli()
sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30))
summary(sim)

# Generate the three core figures (all 15 tutorial figures:
# source generate_figures.R)
fig1 <- plot_competition(sim)
fig2 <- plot_asymmetry(sim)
fig3 <- plot_onset_timing(sim, mean_trial_ms = 1200)

# Fit to the bundled real data (Lin et al., 2023): components first
fc   <- fit_components(sim, lin2023_english)   # CV and C jointly; read r2_c
shape_baseline(lin2023_english)                # single-bump reference
gof  <- goodness_of_fit(sim, lin2023_english)  # asymmetry fit
fig4 <- overlay_empirical(sim, lin2023_english)

# Save figures
ggsave("Fig1_competition.png", fig1, width = 14, height = 6.5, dpi = 300)
ggsave("Fig2_asymmetry.png",   fig2, width = 12, height = 6.5, dpi = 300)
ggsave("Fig3_onset.png",       fig3, width = 11, height = 6.5, dpi = 300)
ggsave("Fig4_overlay.png",     fig4, width = 12, height = 6.5, dpi = 300)

# Export CSV results
export_results(sim, prefix = "my_sim", dir = "output/")
```

## Using Your Own Stimuli

```r
# 1. Transcribe the initial segments of every word (CMU notation)
onsets <- list(
  "target_word" = c("consonant", "vowel"),   # e.g., c("b", "eh") for "bench"
  "cv_comp"     = c("consonant", "vowel"),   # CV competitor
  "cv_ctrl"     = c("consonant", "vowel"),   # CV control (same V, different C)
  "c_comp"      = c("consonant"),            # C competitor
  "c_ctrl"      = c("consonant")             # C control (different C)
)

# 2. Create stimulus table
stim <- create_stimuli(
  targets    = c("target_word", ...),
  large_comp = c("cv_comp", ...),      # Larger-grain competitors
  large_ctrl = c("cv_ctrl", ...),      # Larger-grain controls
  small_comp = c("c_comp", ...),       # Smaller-grain competitors
  small_ctrl = c("c_ctrl", ...),       # Smaller-grain controls
  onsets     = onsets,
  large_type = "CV",                   # or "CVC", "syllable"
  small_type = "C"                     # or "CV", "C"
)

# 3. Run and visualize (gate = "delay" or "truncate")
sim <- run_simulation(stim)
plot_asymmetry(sim)
```

## Using Your Own Empirical Data

```r
# Your data needs these columns:
emp <- data.frame(
  time      = 1:101,        # integer normalized time steps 1..101
  cv_effect = my_cv,        # CV control minus CV competitor
  c_effect  = my_c,         # C control minus C competitor
  group     = "My Group"    # group label (optional)
)
emp$asymmetry <- emp$cv_effect - emp$c_effect

fit_components(sim, emp)    # primary estimator; read r2_c first
shape_baseline(emp)         # single-bump reference
goodness_of_fit(sim, emp)   # asymmetry fit; read r2 before the ranking
overlay_empirical(sim, emp)
```

Check that the selected delta survives other analysis windows
(`time_window`), latency ranges (`lag`), a finer delta grid, and other
segment durations (`seg_steps` in `run_simulation()`) before interpreting
it.

## Key Functions Reference

| Function | Purpose |
|----------|---------|
| `create_stimuli()` | Define your stimulus set |
| `run_simulation()` | Run the simulation |
| `summary()` | View numerical results |
| `plot_competition()` | CV vs C competition curves |
| `plot_asymmetry()` | Asymmetry gradient (main figure) |
| `plot_onset_timing()` | Onset delay function |
| `fit_components()` | Fit the CV and C curves jointly (primary estimator) |
| `shape_baseline()` | Single-Gaussian reference fit |
| `goodness_of_fit()` | Fit every delta model to the asymmetry curve |
| `overlay_empirical()` | Draw empirical data as fitted on the asymmetry plot |
| `example_stimuli()` | Built-in 42-item stimulus set (Lin et al., 2023) |
| `lin2023_english` | Real mouse-tracking data for those items |
| `example_stimuli_zh()`, `lin2023_chinese` | The same bilinguals listening to Mandarin (24 items; use `mandarin_similarity()`) |
| `example_empirical()` | Generate synthetic empirical data |
| `export_results()` | Save results to CSV |

## CMU Phoneme Notation

Common consonants: p, b, t, d, k, g, f, v, s, z, sh, m, n, ng, l, r, w, y, hh, ch, jh, th, dh
Common vowels: ae (/æ/), eh (/ɛ/), ih (/ɪ/), ah (/ʌ/), ao (/ɔ/), uw (/u/), iy (/i/), ey (/eɪ/), ay (/aɪ/)
