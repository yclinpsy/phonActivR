# phonActivR 0.2.0

Revision release responding to peer review at *Behavior Research Methods*.

## New features

* `custom_similarity()` — build a symmetric phoneme-similarity lookup from
  user-specified pairwise values (0–1), so cross-linguistic stimulus sets no
  longer require constructing a full binary feature matrix. Accepted by
  `phoneme_similarity()`, `compute_overlap()`, `run_item()`, and
  `run_simulation()` via the new `similarity_matrix` argument.
* `check_phoneme_coverage()` — reports stimulus phonemes not covered by the
  active feature/similarity set; `run_simulation()` now warns automatically,
  so the silent 0.3 default-similarity fallback can no longer flatten
  cross-linguistic similarity structure unnoticed.
* `overlay_empirical(fit = TRUE)` (new default) — the best-fitting delta for
  each empirical group is now selected formally by RSS/AIC model comparison
  (via `goodness_of_fit()`), replacing visual curve-matching. The selection
  is reported in the console and the full AIC table is attached as
  `attr(plot, "fit")`; no text is drawn inside the image, keeping figures
  publication-clean.
* `theme_phonactivr()` — an exported publication ggplot2 theme (clean white
  panel, recessive horizontal grid, no in-image titles/subtitles) applied by
  every plotting function and usable on any user plot.
* `phonactivr_colors()` — the package-wide colorblind-safe Okabe–Ito
  categorical palette (cf. Wong, 2011, *Nature Methods*); delta values are
  mapped to it in fixed increasing order and always paired with distinct
  line types or point shapes, so no figure relies on color alone.
* `plot_competition()` two-panel display now shares a single y-axis scale
  across panels; `plot_onset_timing()` draws a proper mapped legend (the
  series key formerly lived in subtitle text).
* `print()` and `summary()` methods for `phonActivR_stimuli` objects.
* `power_guidance()` — the Cohen's d analogue is now computed against a
  conservative assumed between-participant SD of the empirical peak
  asymmetry (new `empirical_sd` argument, default 0.04) rather than the
  simulation's own near-zero item-level SEs, which had inflated the
  standardized values meaninglessly; console attribution corrected to
  Magnuson et al. (2012).
* `sensitivity_analysis()` — robustness is now described precisely as
  "never reversed (non-decreasing)"; exact ties under saturating parameter
  regimes are counted and reported separately in the console output.
* `inst/extdata/generate_figures.R` now reproduces **all 16** tutorial
  figures (the workflow, architecture, and data-format diagrams included,
  drawn with base R grid graphics) from a single `source()` call — the
  package and all tutorial figures are 100% R, with no Python dependency.
  (The supplementary original-C-TRACE correspondence study in
  `inst/extdata/trace_correspondence/` includes an optional Python batch
  driver used only to re-drive the compiled C binary; it is not needed to
  install or use the package, and the correspondence statistics reproduce
  from the shipped CSVs with R alone.)
* `goodness_of_fit()` — now group-aware (accepts a `group` column and fits
  each group separately), gains a `time_window` argument for analysis-window
  robustness checks, and attaches the best delta per group as the
  `"best_delta"` attribute.
* `example_empirical()` — the `paradigm` argument is now functional: it is
  validated (`"mouse-tracking"`, `"eye-tracking"`, `"erp"`), sets a
  paradigm-typical default `noise_sd` (0.015 / 0.020 / 0.030), and is
  recorded in the returned data frame.
* New bundled dataset `example_gamm` (`data(example_gamm)`) with the exact
  structure of exported GAMM smooth estimates, so the real-data workflow
  (tutorial Application 1) is fully runnable by every reader. Also shipped
  as `inst/extdata/example_gamm_smooths.csv`; generated reproducibly by
  `data-raw/make_example_gamm.R`.
* `cohort_rhyme_benchmark()` — two additional qualitative checks aligned
  with classic interactive-activation findings: rhyme competition peaks
  later than cohort competition (Allopenna et al., 1998, pattern), and the
  target out-competes all competitors by trial end.
* `compare_languages()` — output now includes an explicit comparability
  caveat: delta values are directly comparable across groups only within a
  paradigm with comparable trial durations and analysis windows.

## Accuracy and accessibility fixes (post-review verification)

* `trace_features()` documentation now states the matrix's provenance
  precisely: a binary (+1/-1) adaptation of the seven TRACE II feature
  dimensions constructed by the package authors for 39 CMU phonemes -- the
  original McClelland-Elman specification is an 8-level continuum over 15
  phonemes. The matrix is a similarity metric only.
* `plot_asymmetry()` (and every figure built on it, including overlays and
  `compare_languages()`) now maps delta to line type as well as color, so
  figures are black-and-white and color-vision-deficiency safe.
* The four-level failure framework is now attributed to Magnuson, Mirman,
  and Harris (2012), with You and Magnuson (2018) as the application to
  modeling tools; the theory-level hypothesis is referred to as the
  language-specific grain-size hypothesis (after Cutler & Otake's, 1994,
  language-specific listening proposal).

## Documentation

* Full roxygen2-generated help pages (`man/`) are now included, so
  `?run_simulation` etc. work after installation.
* Installation instructions now use
  `devtools::install_github("yclinpsy/phonActivR", build_vignettes = TRUE)`
  so that `vignette("phonActivR-tutorial")` is available.
* README gains a complete function inventory table.
* Expanded line-by-line comments throughout the computational core
  (`run_activation()`, `compute_overlap()`, `goodness_of_fit()`).

# phonActivR 0.1.1

Documentation-only release. Package-level help, function docstrings, DESCRIPTION,
and README were reworded to describe the software accurately: the delta parameter
is an external prosodic-constraint parameter (a temporal gate on sub-prosodic
input), the bundled competition model is a compact lateral-inhibition model in
the interactive-activation family (not TRACE), and the McClelland-Elman feature
matrix is used as a similarity metric only.
No computational changes; all functions return results identical to 0.1.0.

# phonActivR 0.1.0

* Initial release.
* Core interactive activation engine with TRACE-style dynamics.
* TRACE 7-feature phoneme matrix (39 phonemes: 24 consonants, 15 vowels).
* Phonological overlap computation for C, CV, CVC, and syllable grain sizes.
* Generalizable δ (delta) prosodic constraint parameter.
* Item-level simulation across full stimulus sets.
* Built-in 42-item Japanese–English bilingual example (`example_stimuli_jp()`).
* Synthetic empirical data generator (`example_empirical()`).
* Publication-ready visualization: `plot_competition()`, `plot_asymmetry()`,
  `plot_onset_timing()`.
* Empirical overlay function (`overlay_empirical()`) with automatic scale
  normalization.
* CSV export utility (`export_results()`).
* Comprehensive test suite via `testthat`.
