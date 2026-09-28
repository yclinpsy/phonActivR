# =============================================================================
# correspondence_analysis.R -- phonActivR vs. ORIGINAL C TRACE (p86, slex)
# =============================================================================
# Reproduces every number reported in the tutorial's Validation 6 from the
# two data files shipped in this folder. It does NOT require the C binary:
# ctrace_trajectories.csv holds the original-TRACE word-activation
# trajectories (produced by run_ctrace_batch.py; byte-reproducible from the
# patched distribution), and trace_feature_similarity.csv holds the
# pairwise phoneme similarities computed as cosines between the ORIGINAL
# TRACE feature vectors (fetvals.c: 15 phonemes x 7 continua x 9 values).
#
# Design: 24 target/competitor/control triples from the classic 213-word
# slex lexicon; 8 cohort (share the initial two phonemes), 8 rhyme (differ only in the
# initial phoneme), 8 unrelated. TRACE side: Luce choice-rule response
# probabilities over the three probed words (fixed k = 7; Allopenna,
# Magnuson, & Tanenhaus, 1998, introduced Luce linking for TRACE with a
# time-growing k; results are insensitive: rho = .82 for k in 4..10 and
# .85 on raw activations), competition effect = P(competitor) - P(control).
# phonActivR side: the package's own competitor-type conventions
# (cohort_rhyme_benchmark) with item-level overlaps computed from the
# original TRACE feature similarities via custom_similarity().
# Onset rule (both models): first time point at which the competition
# effect reaches 20% of its own peak (undefined when the peak is
# negligible: < .005 TRACE / < .02 phonActivR).
# =============================================================================
library(phonActivR)

pairs <- read.csv("pairs_24.csv", stringsAsFactors = FALSE)
smat  <- custom_similarity(read.csv("trace_feature_similarity.csv"))
traj  <- read.csv("ctrace_trajectories.csv", stringsAsFactors = FALSE)

K <- 7  # fixed Luce separation constant; see header note on robustness
luce_eff <- function(it) {
  sapply(1:90, function(cy) {
    a <- c(t = it$act[it$role == "targ" & it$cycle == cy],
           c = it$act[it$role == "comp" & it$cycle == cy],
           u = it$act[it$role == "ctrl" & it$cycle == cy])
    s <- exp(K * pmax(a, 0))   # floor negatives at 0, as is standard
    p <- s / sum(s)
    p["c"] - p["u"]
  })
}
onset_rule <- function(eff, floor) {
  pk <- max(eff)
  if (pk < floor) return(NA_integer_)
  which(eff >= 0.2 * pk)[1]
}

res <- do.call(rbind, lapply(seq_len(nrow(pairs)), function(k) {
  pr <- pairs[k, ]
  tp <- strsplit(pr$target, "")[[1]]
  cp <- strsplit(pr$comp,   "")[[1]]
  up <- strsplit(pr$ctrl,   "")[[1]]
  if (pr$class == "cohort") {
    ov_c <- compute_overlap(tp[1:2], cp[1:2], "CV", similarity_matrix = smat)
    on_c <- 1L
  } else if (pr$class == "rhyme") {
    ov_c <- compute_overlap(tp, cp, "syllable", similarity_matrix = smat)
    on_c <- 30L
  } else {
    ov_c <- compute_overlap(tp, cp, "syllable", similarity_matrix = smat)
    on_c <- 1L
  }
  ov_u  <- compute_overlap(tp, up, "syllable", similarity_matrix = smat)
  eff_p <- run_activation(ov_c, ov_u, overlap_onset = on_c,
                          time_steps = 101L)$competition_effect
  eff_t <- luce_eff(traj[traj$item == pr$item, ])
  data.frame(class = pr$class, target = pr$target,
             p_onset = onset_rule(eff_p, .02),  p_peak = max(eff_p),
             t_onset = onset_rule(eff_t, .005), t_peak = max(eff_t))
}))

print(res, row.names = FALSE, digits = 3)
ok <- complete.cases(res$p_peak, res$t_peak)
pk <- cor.test(res$p_peak[ok], res$t_peak[ok])
sp <- suppressWarnings(cor.test(res$p_peak[ok], res$t_peak[ok],
                                method = "spearman"))
cat(sprintf("\nPEAK  (n=%d): Pearson r = %.3f, p = %.2e | Spearman rho = %.3f, p = %.2e\n",
            sum(ok), pk$estimate, pk$p.value, sp$estimate, sp$p.value))
on_ok <- complete.cases(res$p_onset, res$t_onset)
onr <- cor.test(res$p_onset[on_ok], res$t_onset[on_ok])
cat(sprintf("ONSET (n=%d, items with realized competition in both models): Pearson r = %.3f, p = %.2e\n",
            sum(on_ok), onr$estimate, onr$p.value))
write.csv(res, "correspondence_results.csv", row.names = FALSE)

# --- Robustness of the Luce linking constant (reported in Validation 6) ---
peak_for_k <- function(K2) sapply(seq_len(nrow(pairs)), function(k) {
  it <- traj[traj$item == pairs$item[k], ]
  max(sapply(1:90, function(cy) {
    a <- c(t = it$act[it$role=="targ" & it$cycle==cy],
           c = it$act[it$role=="comp" & it$cycle==cy],
           u = it$act[it$role=="ctrl" & it$cycle==cy])
    s <- exp(K2 * pmax(a, 0)); p <- s/sum(s); p["c"] - p["u"] }))
})
for (K2 in c(4, 7, 10)) cat(sprintf(
  "k = %2d: Spearman rho = %.3f\n", K2,
  suppressWarnings(cor(res$p_peak, peak_for_k(K2), method = "spearman"))))
raw_peak <- sapply(seq_len(nrow(pairs)), function(k) {
  it <- traj[traj$item == pairs$item[k], ]
  max(sapply(1:90, function(cy)
    it$act[it$role=="comp" & it$cycle==cy] -
    it$act[it$role=="ctrl" & it$cycle==cy])) })
cat(sprintf("raw activations: Spearman rho = %.3f\n",
  suppressWarnings(cor(res$p_peak, raw_peak, method = "spearman"))))
