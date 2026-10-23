# phonActivR 0.3.0

Model and fitting revision prompted by fitting the model to real data for
the first time.

## Model

* **Input acts from step 1.** `run_activation()` now computes step 1 from
  the resting state, so a window `[overlap_onset, overlap_offset)` supplies
  exactly `overlap_offset - overlap_onset` steps of input at every delta.
  Previously the step-1 input was never applied, which shortened the
  delta = 0 window (and every larger-grain window) by one step relative to
  gated windows, handicapping delta = 0 in the fits. All simulated curves
  therefore differ slightly from earlier 0.3.0 builds (onsets one step
  earlier; the tutorial's tables were regenerated). A unit test now checks
  that a delayed curve equals the undelayed curve shifted by delta when
  inhibition and decay are off.
* **Disambiguation point.** Each competitor (and its matched control) now
  receives bottom-up input only while the segments it shares with the target
  unfold: `shared_segments(type) * seg_steps` time steps from its gate-open
  time (C = 1 segment, CV = 2, CVC = 3). Competition therefore rises and then
  resolves, as in behavioral time courses. In 0.2.x the input never ended,
  so competition never resolved and the model could not reproduce real data
  (R^2 up to about .5 on Lin et al., 2023, versus .73--.97 now).
* New argument `seg_steps` (default 10, time steps per segment) in
  `run_simulation()`, `run_item()`, `cohort_rhyme_benchmark()` and
  `sensitivity_analysis()`; `seg_steps = Inf` reproduces the 0.2.x dynamics.
  New arguments `overlap_offset` in `run_activation()`; new exported helper
  `shared_segments()`.
* `cohort_rhyme_benchmark()`: input windows follow segment positions in the
  word (rhyme input starts with the target's second segment) instead of a
  hard-coded rhyme onset at cycle 30.

* New argument `gate` in `run_simulation()` and `run_item()`: `"delay"`
  (default; buffered delay) or `"truncate"`
  (signal-locked: the disambiguation point stays where the signal puts it,
  so delta removes the part of the sub-prosodic input heard before the gate
  opens and a delta of one segment or more abolishes C competition).
* The simulation is documented as deterministic: `run_simulation()`'s
  `seed` argument is kept for compatibility but ignored, and no function
  changes the global random-number stream any more (`example_empirical()`
  and `parameter_recovery()` use local seeds via withr, now in Imports).
* New argument `c_weight` (default 0.5) in `run_simulation()`, `run_item()`
  and `compute_overlap()`: the weight of a consonant-only match, which sets
  the strength of C input (C overlap = consonant similarity x c_weight). The
  default keeps earlier results unchanged; larger values fit the C curves of
  Lin et al. (2023) better and should be varied as a robustness check.
* Under `gate = "truncate"`, a delta of one segment or more abolishes C
  competition only when `residual_input = 0`; residual input arrives after
  the gate has opened and is used.
* `?trace_features` lists the phonemes that share a feature vector (22
  distinct vectors for 39 phonemes); `?compute_overlap` documents the
  overlap rules. `check_phoneme_coverage()` (and hence `run_simulation()`,
  when verbose) reports compared pairs of different phonemes that the feature
  matrix does not distinguish (e.g., b-jh in the built-in stimuli), in its
  new `"identical_pairs"` attribute.
* `run_simulation()` prints onsets in time steps (it printed step numbers
  followed by "%").

## Fitting

* `goodness_of_fit()` scales each **model** curve onto the data with a
  non-negative least-squares factor (0.2.x rescaled the data by peak
  matching, which admitted degenerate near-zero fits). New `lag` argument estimates a
  response latency per model; new `r2` column reports absolute fit. Console
  output now describes delta_AIC as a ranking, because time bins are
  autocorrelated.
* `overlay_empirical()` draws each group exactly as it was fitted (same
  scale and latency as the selected delta), so the overlay and the formal
  selection can no longer disagree. New `time_window` and `lag` arguments.
* `example_empirical()` generates data by re-running the model at
  `true_delta`; it no longer interpolates between grid curves.
* `parameter_recovery()` rewritten on model-generated data: on-grid deltas
  must be recovered exactly; off-grid deltas count as recovered when either
  bracketing grid value is selected, and the split is reported.
* `sensitivity_analysis()` now asks whether every delta is distinguishable
  from delta = 0 and from its grid neighbor (RMS curve difference), since
  peak asymmetry is no longer monotone in delta; new `seg_values` and
  `min_rms` arguments.
* New `fit_components()`: fits the CV and C effect curves jointly (shared
  scale and latency) and reports `r2`, `r2_cv`, `r2_c`, so a good asymmetry
  fit can be checked against the components that produce it.
* New `shape_baseline()`: a two-parameter-shape Gaussian bump fitted the same
  way, as a descriptive benchmark for any R^2 the model achieves.
* New argument `residual_input` (default 0) in `run_simulation()`,
  `run_item()` and `run_activation()`: the fraction of each competitor's
  (and control's) input that continues for one segment after its shared
  segments end. The default keeps the results of the tutorial unchanged.
* New folder `inst/extdata/lin2023_validation/`: item- and participant-level
  mean trajectories for Lin et al. (2023), Experiments 3 and 4 (they
  reproduce `lin2023_english`), and `validation.R`, which runs the
  held-out-item test of `residual_input` and a participant bootstrap of the
  selected delta (manuscript, Step 6b).
* New script `inst/extdata/linking_tests/linking_tests.R`: tests whether a
  response-stage linking function (leaky integrator, Luce choice rule) or
  longer competitor support repairs the fit to the C component (Step 6b).
* `goodness_of_fit()` and `fit_components()` warn when the estimated
  latency lies on the upper edge of its search range, or on the lower edge
  when that edge is above 0 (a latency of 0 is a natural bound, not a sign
  that the range is too narrow).
* `overlay_empirical()` gains `show_pre_onset`: empirical points that the
  latency shift moves before the model's time 1 can be drawn on an extended
  axis, so an effect that precedes the model's onset stays visible; its
  console message points to `fit_components()`.
* `parameter_recovery()` gains `true_lag` and `lag`, so recovery can be run
  with the latency estimated.
* `goodness_of_fit()` and `overlay_empirical()` now search latencies
  0:50 by default (as `fit_components()` does), print a reminder to check
  the component fit, and, like `fit_components()`, stop with an explanatory
  message if `time` is not integer model steps (e.g., milliseconds or 0-100).
* `fit_components()` gains `shared_scale = FALSE` (separate CV and C
  scales) and reports `scale_c` and `n_params`; `shape_baseline()` reports
  RSS and AIC (k = 3).
* `parameter_recovery()` gains `method = "components"` (recovery with the
  joint fit), `generate_with` (generate data from a misspecified model),
  and `verbose`; `example_empirical()` returns `cv_effect` and `c_effect`
  (consistent with `asymmetry`) and gains `generate_with`.
* `power_guidance()` gains `scale` (model-to-data conversion; without it
  the SD must be in model units), and its rating now uses the expected
  two-group test statistic `expected_z = d * sqrt(n / 2)`, so the number of
  participants enters the calculation.
* `export_results()` writes `seg_steps`, `residual_input`, `gate`, the
  package version, any custom similarity matrix, and the session info.
* `compare_languages()` reports the gate mode, the peak C effect at the
  largest delta, and the delta values with no C competition, and can
  compare two model variants on the same items.
* `power_guidance()` uses the largest momentary difference between adjacent
  delta curves (column `max_curve_diff`).

## Data and scripts

* `inst/extdata/lin2023_validation/` adds participants' mean response
  times; `validation.R` now uses the package's own fitting functions and
  reproduces every Step 6b number (held-out items, participant bootstrap and
  group difference, model variants, recovery under misspecification at a
  low and a realistic noise level, and the sensitivity of the joint fit to
  `c_weight` and `ctrl_scaling`, with and without a separate C scale).
* The time-axis error message now recommends resampling to 101 steps with
  `approx()`.
* `power_guidance()` documents that `n_items` does not enter the
  computation; `example_empirical()` documents the noise of its C curve.
* `inst/extdata/worked_example.R`: the manuscript's Appendix script.
* `trace_correspondence/correspondence_analysis.R` also reports Kendall's
  tau-b and the class-only and within-class correlations; its results CSV
  is refreshed.
* Removed the stale `inst/extdata/simulation_summary.csv` (legacy output of
  `original_simulation.R`).
* GitHub Actions workflow for R CMD check on macOS, Windows, and Linux.

* New dataset `lin2023_english`: real group-mean mouse-tracking competition
  effects from Lin, Lin, and Yeh (2023), Experiments 3 and 4, which used the
  same 42 items as `example_stimuli_jp()`.
* New `bootstrap_delta()`: resamples participants from a table of
  participant-level mean trajectories, refits the delta models, and
  tabulates the selected delta (the check recommended in Step 6b, as a
  function rather than a script).
* New argument `small_ctrl_type` in `run_simulation()` and `run_item()`:
  the overlap type used to score the smaller-grain competitor's control,
  for designs in which that control shares the target's vowel.
* `fit_components()` raises a real warning (not only a console message) and
  sets a `"c_fit_ok"` attribute when the C component is poorly fitted at the
  selected delta.
* New dataset `lin2023_chinese` (the same 41 bilinguals listening to Mandarin;
  Lin et al., 2023, Experiment 1), with its 24-item stimulus set
  `example_stimuli_zh()` and an illustrative similarity table
  `mandarin_similarity()` for its Pinyin-based phoneme codes. Participant-level
  means are in `inst/extdata/lin2023_validation/`, and `validation.R` section
  H bootstraps the selected delta under both gate readings (tutorial,
  Application 2, Table 7).
* Removed `example_gamm` (synthetic data blended from two model curves).
* `overlay_empirical()` gains `facet_groups`: one panel per group, each
  aligned to its own selected delta curve (the layout of Figure 6A-B), and
  computes `asymmetry` from `cv_effect` and `c_effect` when it is absent;
  `goodness_of_fit()` does the same instead of stopping with a bare
  assertion.
* Words without a transcription now raise an R warning in
  `create_stimuli()` and `run_simulation()` (they used to receive the
  placeholder "?" after a console alert only). The uncovered-pair warning
  no longer refers to a flat `max_asymmetry` column (which is a legitimate
  pattern; see Table 4 of the tutorial).
* `validation.R` gains section I: a paired participant bootstrap comparing
  English and Mandarin within the same 41 participants (Experiments 3 and 1).
  `generate_figures.R` now writes the tutorial's 11 main figures and
  Figures S1-S3 (the two data-format figures were replaced by printed
  output, two analysis figures moved to the Supplemental Material, and
  Figure S3 shows raw competitor and control trajectories). Script comments
  follow the tutorial's final numbering of validations, applications,
  tables, and supplemental sections.
* `inst/extdata/linking_tests/linking_tests.R` reimplements the step-1
  update so that its reference runs match `run_simulation()` exactly.

## Other

* New `example_stimuli()`, identical to `example_stimuli_jp()` (kept for
  backward compatibility): the 42 items are the English stimuli of Lin et
  al. (2023), so the language-neutral name is now the documented one.
* `summary()` no longer prints a sample-size rule presented as a power
  result. Documentation of `compare_languages()` corrected.

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
