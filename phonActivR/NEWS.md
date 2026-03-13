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
