# Quantitative correspondence with the ORIGINAL C implementation of TRACE

This folder contains the complete, reproducible correspondence study
reported in the tutorial's Validation 6: phonActivR's item-level
competition predictions against the **original C implementation of
TRACE** (McClelland & Elman, 1986), run with the classic p86 parameter
file and the full 213-word `slex` lexicon.

## What happened to the C code

The correspondence uses a 2005-modified copy of the original TRACE C
distribution. As distributed, that copy cannot run any simulation: two
defects introduced by the 2005 modification silently erase every input
specification. We repaired them (see `ctrace_bugfix.patch`, 3 changes):

1. `util2.c` `inputcomp()`: a zeroing loop ran to `FSLICES` although
   `infet[]` is dimensioned `[MAXITEMS][...]`; the ~11.6 KB overflow reached and
   zeroed the global `ninputs` (stored ~720 bytes past `infet[]`), so all simulations stayed at resting
   level.
2. `util2.c` `inputcomp()`: when `-i` is parsed on the command line the
   phoneme table is not yet initialized; a guard defers input
   compilation to after `initialize()`.
3. `command.c`: an added `wmax <word>` command prints the maximum
   word-token activation at the current cycle at full float precision,
   for machine-readable batch probing. No model equation was touched.

The TRACE distribution itself is McClelland & Elman's code and is NOT
redistributed here; the patch, the drivers, and all extracted data are.

## Files

- `pairs_24.csv` -- the 24 target/competitor/control triples (8 cohort
  sharing the initial two phonemes, 8 rhyme differing only in the
  initial phoneme, 8 unrelated), drawn from the slex lexicon itself.
- `trace_feature_similarity.csv` -- pairwise similarities of the 15
  TRACE phonemes, computed as cosines between the ORIGINAL feature
  vectors in `fetvals.c` (7 continua x 9 values per phoneme).
- `run_ctrace_batch.py` -- drives the patched C binary; produces
  `ctrace_trajectories.csv` (byte-reproducible).
- `ctrace_trajectories.csv` -- max word-token activation of target,
  competitor, and control at each of 90 cycles, all 24 items.
- `correspondence_analysis.R` -- reproduces every reported number from
  the two CSVs alone, including the k = 4–10 and raw-activation
  robustness sweep (no C binary needed): Luce choice-rule linking on the
  TRACE side (fixed k = 7; Allopenna et al., 1998, introduced the Luce
  linking for TRACE with a time-growing k — results are insensitive to
  this choice: rho = .82 across k = 4–10, .85 on raw activations); the package's own
  competitor-type conventions with TRACE-feature-derived overlaps on the
  phonActivR side.
- `correspondence_results.csv` -- the per-item measures.

## Headline results (reproduced by the analysis script)

- Item-level peak competition magnitude: Spearman rho = .82,
  p = 7.5e-07 (Pearson r = .61, p = .0015), n = 24.
- Competition timing ordering reproduced: cohort competition early in
  both models; the one rhyme item for which original TRACE generates
  measurable competition (lid-rid, featurally similar onsets) peaks
  late in both models (TRACE onset cycle 34 of 90; phonActivR step 32
  of 101).
- Documented divergence: on the full 213-word lexicon, original
  TRACE's raw word activations show essentially no rhyme competition
  (7 of 8 rhyme items at floor even after Luce linking) -- consistent
  with Allopenna et al.'s (1998) demonstration that raw TRACE
  underpredicts empirical rhyme competition. phonActivR's rhyme
  convention instead follows the empirically observed late rhyme time
  course, which is its stated purpose.
