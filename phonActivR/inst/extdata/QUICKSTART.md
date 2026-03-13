# phonActivR Quick-Start Guide

## Installation

```r
# Install from GitHub (requires devtools)
install.packages("devtools")  # if not already installed
devtools::install_github("author/phonActivR")
library(phonActivR)
```

## 5-Minute Demo (copy-paste and run)

```r
library(phonActivR)

# Load example stimuli and run simulation
stim <- example_stimuli_jp()
sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30))
summary(sim)

# Generate all three publication figures
fig1 <- plot_competition(sim)
fig2 <- plot_asymmetry(sim)
fig3 <- plot_onset_timing(sim, mean_trial_ms = 1500)

# Overlay synthetic empirical data
emp  <- example_empirical(sim, true_delta = 15,
  group_labels = c("JP bilinguals", "EN monolinguals"))
fig4 <- overlay_empirical(sim, emp)

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
# 1. Define phoneme onsets for every word (CMU Pronouncing Dictionary notation)
onsets <- list(
  "target_word" = c("consonant", "vowel"),   # e.g., c("b", "eh") for "bench"
  "cv_comp"     = c("consonant", "vowel"),   # CV competitor
  "cv_ctrl"     = c("consonant", "vowel"),   # CV control (different C)
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

# 3. Run and visualize
sim <- run_simulation(stim)
plot_asymmetry(sim)
```

## Using Your Own Empirical Data

```r
# Your data needs these columns:
emp <- data.frame(
  time      = 1:101,        # Normalized time steps (must match sim)
  asymmetry = my_cv - my_c, # Large-grain minus small-grain effect
  se        = my_se,        # Standard error (optional)
  group     = "My Group"    # Group label (optional)
)

overlay_empirical(sim, emp)
```

## Key Functions Reference

| Function | Purpose |
|----------|---------|
| `create_stimuli()` | Define your stimulus set |
| `run_simulation()` | Run the simulation |
| `summary()` | View numerical results |
| `plot_competition()` | CV vs C competition curves |
| `plot_asymmetry()` | Asymmetry gradient (main figure) |
| `plot_onset_timing()` | Onset delay function |
| `overlay_empirical()` | Add empirical data to asymmetry plot |
| `example_stimuli_jp()` | Built-in 42-item example |
| `example_empirical()` | Generate synthetic empirical data |
| `export_results()` | Save results to CSV |

## CMU Phoneme Notation

Common consonants: p, b, t, d, k, g, f, v, s, z, sh, m, n, ng, l, r, w, y, hh, ch, jh, th, dh
Common vowels: ae (/æ/), eh (/ɛ/), ih (/ɪ/), ah (/ʌ/), ao (/ɔ/), uw (/u/), iy (/i/), ey (/eɪ/), ay (/aɪ/)
