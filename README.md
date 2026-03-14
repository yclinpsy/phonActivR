# phonActivR <img src="inst/extdata/figures/figure_0_pipeline.png" align="right" width="300"/>

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R >= 4.0.0](https://img.shields.io/badge/R-%3E%3D%204.0.0-brightgreen.svg)](https://cran.r-project.org/)

**A Lightweight Interactive Activation Simulator for Modeling Cross-Linguistic Phonological Competition in Spoken Word Recognition**

## Authors

- **Yu-Cheng Lin** — Department of Psychological Science, University of Texas Rio Grande Valley
- **Hitomi Kambara** — Department of Bilingual and Literacy Studies, University of Texas Rio Grande Valley

## Overview

`phonActivR` is a pure-R package that implements a TRACE-style interactive activation architecture for generating predictions about phonological competition in spoken word recognition. It requires no Java or external software.

### Key Features

- **δ (delta) parameter**: Operationalizes cross-linguistic prosodic processing constraints (moraic, syllabic, tonal) as temporal delays on sub-unit activation, providing a unified framework for testing cross-linguistic processing hypotheses.
- **Item-level simulation**: Runs across full experimental stimulus sets (not just single words), with automatic phonological overlap computation from the TRACE 7-feature phoneme matrix.
- **Paradigm-agnostic overlay**: Normalizes and superimposes model predictions onto empirical time-course data from mouse-tracking, eye-tracking, ERP, or any paradigm producing a time-varying competition measure.
- **Multi-level validation**: Qualitative TRACE benchmarks, systematic parameter sensitivity analysis, parameter recovery, and AIC-based model selection.
- **Publication-ready figures**: All visualizations are ggplot2 objects, customizable with standard ggplot2 grammar.

## Installation

```r
# Install from GitHub
devtools::install_github("yclinpsy/phonActivR")
```

No Java, Python, or external software required.

## Quick Start

```r
library(phonActivR)
stim <- example_stimuli_jp()                              # Load built-in 42-item stimulus set
sim  <- run_simulation(stim, delta_values = c(0,10,20,30)) # Run simulation
plot_competition(sim)                                      # Visualize predictions
overlay_empirical(sim, example_empirical(sim))             # Overlay synthetic empirical data
```

## Tutorial

A comprehensive step-by-step tutorial is available as a package vignette:

```r
vignette("phonActivR-tutorial")
```

The tutorial demonstrates the complete workflow from stimulus specification through simulation, visualization, and empirical comparison, illustrated with a worked example from a Japanese–English bilingual spoken word recognition study.

## Reproducing Paper Figures

All figures in the accompanying *Behavior Research Methods* tutorial paper can be reproduced:

```r
source(system.file("extdata", "generate_figures.R", package = "phonActivR"))
```

All scripts use `set.seed(1234)` for exact reproducibility.

## Functions

| Function | Purpose |
|----------|---------|
| `create_stimuli()` | Organize stimulus sets with onset transcriptions |
| `run_simulation()` | Run interactive activation model across items and δ values |
| `plot_competition()` | Competition effect curves for competing hypotheses |
| `plot_asymmetry()` | CV–C asymmetry gradient (primary hypothesis-adjudication figure) |
| `plot_onset_timing()` | Predicted competition onset as a function of δ |
| `overlay_empirical()` | Overlay empirical data onto model predictions |
| `example_stimuli_jp()` | Built-in 42-item Japanese–English stimulus set |
| `example_empirical()` | Generate synthetic empirical data for demonstrations |
| `export_results()` | Save simulation output to CSV |
| `power_guidance()` | Estimate minimum detectable δ difference |
| `sensitivity_analysis()` | Multi-parameter robustness analysis |
| `parameter_recovery()` | Recover known δ from synthetic data |
| `cohort_rhyme_benchmark()` | Qualitative TRACE benchmark validation |
| `goodness_of_fit()` | AIC-based model comparison |
| `compare_languages()` | Cross-linguistic grain-size comparison |

## Citation

If you use phonActivR in your research, please cite:

> Lin, Y.-C., & Kambara, H. (2026). phonActivR: A lightweight interactive activation simulator for modeling cross-linguistic phonological competition in spoken word recognition. *Behavior Research Methods*. [Tutorial Collection]

## Related Package

[**grainSizeR**](https://github.com/yuchenglin/grainSizeR) — A companion package for *inverse* modeling of phonological grain size. While phonActivR generates predictions (forward simulation), grainSizeR estimates the best-fitting grain-size parameter from empirical data (inverse estimation).

**Recommended workflow**: phonActivR for exploratory hypothesis generation → grainSizeR for confirmatory estimation and statistical testing.

## License

MIT License © 2026 Yu-Cheng Lin and Hitomi Kambara
