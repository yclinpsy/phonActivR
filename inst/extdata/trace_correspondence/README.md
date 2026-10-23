# Quantitative correspondence with the ORIGINAL C implementation of TRACE

This folder contains the complete, reproducible correspondence study
reported in the tutorial's Validation 1: phonActivR's item-level
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
  this choice: rho = .82 across k = 4–10, .84 on raw activations); the package's own
  competitor-type conventions with TRACE-feature-derived overlaps on the
  phonActivR side.
- `correspondence_results.csv` -- the per-item measures.

## Headline results (reproduced by the analysis script; package 0.3.0)

- Item-level peak competition: Spearman rho = .82 (Pearson r = .67),
  n = 24. This reflects the class ordering (cohort > rhyme > unrelated),
  not item-level correspondence: a code for competitor class alone
  correlates rho = .86 with the TRACE peaks, and within classes the two
  models do not agree (cohort rho = .05, rhyme rho = .16).
- Rhyme magnitude diverges: phonActivR predicts rhyme peaks of .33-.48
  (61-88% of its cohort peaks); original TRACE shows measurable rhyme
  competition for only one of eight items (lid-rid).
- Robustness: rho = .82 for Luce k = 4-10, .84 on raw activations, and
  .80-.83 for segment durations of 6-14 steps (set SEG_STEPS before
  sourcing the script).
