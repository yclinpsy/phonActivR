test_that("phoneme_similarity returns correct values", {
  feats <- trace_features()

  # Identical phonemes should have similarity 1.0
  expect_equal(phoneme_similarity("b", "b", feats), 1.0)

  # b and p differ only in voicing -> 6/7

  expect_equal(phoneme_similarity("b", "p", feats), 6/7)

  # Unknown phoneme -> default
  expect_equal(phoneme_similarity("xx", "b", feats), 0.3)
})

test_that("compute_overlap handles C and CV types", {
  # CV overlap with identical segments -> high
  ov_cv <- compute_overlap(c("b", "eh"), c("b", "eh"), "CV")
  expect_equal(ov_cv, 1.0)

  # C overlap with identical consonant
  ov_c <- compute_overlap(c("b", "eh"), c("b"), "C")
  expect_equal(ov_c, 0.5)  # scaled by 0.5
})

test_that("run_activation returns correct structure", {
  result <- run_activation(0.5, 0.1, overlap_onset = 1, time_steps = 50)
  expect_equal(nrow(result), 50)
  expect_true(all(c("time", "act_comp", "act_ctrl", "act_target",
                     "competition_effect") %in% names(result)))
  # Competition effect should be positive at end
  expect_gt(result$competition_effect[50], 0)
})

test_that("find_onset detects threshold crossing", {
  curve <- c(rep(0, 10), seq(0.01, 0.5, length.out = 40))
  onset <- find_onset(curve, threshold = 0.04)
  expect_true(!is.na(onset))
  expect_true(onset > 10)

  # No crossing
  flat <- rep(0.01, 50)
  expect_true(is.na(find_onset(flat, threshold = 0.04)))
})

test_that("run_simulation produces valid output", {
  onsets <- list(
    "bench" = c("b", "eh"), "bell" = c("b", "eh"), "cell" = c("s", "eh"),
    "bark"  = c("b"),        "dark" = c("d")
  )
  stim <- create_stimuli(
    targets    = "bench",
    large_comp = "bell",
    large_ctrl = "cell",
    small_comp = "bark",
    small_ctrl = "dark",
    onsets     = onsets
  )
  sim <- run_simulation(stim, delta_values = c(0, 10), verbose = FALSE)

  expect_s3_class(sim, "phonActivR_sim")
  expect_equal(length(sim$results), 2)
  expect_equal(nrow(sim$summary), 2)

  # Delta > 0 should produce later small onset
  s0  <- sim$results[["0"]]
  s10 <- sim$results[["10"]]
  onset_0  <- find_onset(s0$small_effect)
  onset_10 <- find_onset(s10$small_effect)
  if (!is.na(onset_0) && !is.na(onset_10)) {
    expect_gte(onset_10, onset_0)
  }
})

test_that("example_stimuli_jp returns valid stimuli", {
  stim <- example_stimuli_jp()
  expect_s3_class(stim, "phonActivR_stimuli")
  expect_equal(stim$n_items, 42)
  expect_equal(stim$large_type, "CV")
  expect_equal(stim$small_type, "C")
  expect_equal(nrow(stim$items), 42)
})

test_that("example_empirical produces valid overlay data", {
  stim <- example_stimuli_jp()
  sim  <- run_simulation(stim, delta_values = c(0, 10, 20), verbose = FALSE)
  emp  <- example_empirical(sim, true_delta = 15, seed = 123)

  expect_true(is.data.frame(emp))
  expect_true(all(c("time", "asymmetry", "se", "group") %in% names(emp)))
  # Two groups, each with time_steps rows
  expect_equal(nrow(emp), 2 * sim$params$time_steps)
  expect_equal(length(unique(emp$group)), 2)
})

test_that("example_empirical is reproducible with seed", {
  stim <- example_stimuli_jp()
  sim  <- run_simulation(stim, delta_values = c(0, 10), verbose = FALSE)
  emp1 <- example_empirical(sim, seed = 42)
  emp2 <- example_empirical(sim, seed = 42)
  expect_identical(emp1, emp2)
})

test_that("compute_overlap handles CVC and syllable types", {
  ov_cvc <- compute_overlap(c("b", "eh", "n"), c("b", "eh", "n"), "CVC")
  expect_equal(ov_cvc, 1.0)

  ov_syll <- compute_overlap(c("b", "eh"), c("b", "eh"), "syllable")
  expect_equal(ov_syll, 1.0)

  # Syllable with length mismatch should be penalized
  ov_short <- compute_overlap(c("b", "eh", "n"), c("b", "eh"), "syllable")
  expect_lt(ov_short, 1.0)
})

test_that("activation values stay within bounds", {
  result <- run_activation(0.9, 0.1, overlap_onset = 1, time_steps = 200)
  expect_true(all(result$act_comp >= 0))
  expect_true(all(result$act_comp <= 1))
  expect_true(all(result$act_target >= 0))
  expect_true(all(result$act_target <= 1))
})

test_that("asymmetry SE uses error propagation", {
  stim <- example_stimuli_jp()
  sim  <- run_simulation(stim, delta_values = c(0), verbose = FALSE)
  res  <- sim$results[["0"]]
  # asym_se should equal sqrt(large_se^2 + small_se^2)
  expected_se <- sqrt(res$large_se^2 + res$small_se^2)
  expect_equal(res$asym_se, expected_se)
})

test_that("example_empirical handles out-of-range true_delta", {
  stim <- example_stimuli_jp()
  sim  <- run_simulation(stim, delta_values = c(0, 10, 20), verbose = FALSE)
  # true_delta > max should not crash (clamped to max)
  emp_high <- example_empirical(sim, true_delta = 100, seed = 1)
  expect_true(is.data.frame(emp_high))
  expect_equal(nrow(emp_high), 2 * sim$params$time_steps)
  # true_delta < min should not crash (clamped to min)
  emp_low <- example_empirical(sim, true_delta = -10, seed = 2)
  expect_true(is.data.frame(emp_low))
})

test_that("run_simulation works with single item", {
  onsets <- list(
    "bench" = c("b", "eh"), "bell" = c("b", "eh"), "cell" = c("s", "eh"),
    "bark"  = c("b"),        "dark" = c("d")
  )
  stim <- create_stimuli(
    targets = "bench", large_comp = "bell", large_ctrl = "cell",
    small_comp = "bark", small_ctrl = "dark", onsets = onsets
  )
  sim <- run_simulation(stim, delta_values = c(0, 10), verbose = FALSE)
  expect_s3_class(sim, "phonActivR_sim")
  # SE should be 0 (not NA) for single item
  expect_true(all(sim$results[["0"]]$large_se == 0))
  expect_true(all(!is.na(sim$results[["0"]]$asym_se)))
})

test_that("overlay_empirical handles single group without group column", {
  stim <- example_stimuli_jp()
  sim  <- run_simulation(stim, delta_values = c(0, 10), verbose = FALSE)
  # Data without group column should default to "Empirical"
  emp <- data.frame(
    time      = 1:101,
    asymmetry = sin(seq(0, pi, length.out = 101)) * 0.3
  )
  # Should not error
  p <- overlay_empirical(sim, emp)
  expect_true(inherits(p, "gg"))
})

# ---- New tests implementing editorial recommendations ----

test_that("run_simulation records elapsed time in returned object", {
  stim <- example_stimuli_jp()
  sim  <- run_simulation(stim, delta_values = c(0, 10), verbose = FALSE)
  expect_true(!is.null(sim$elapsed_sec))
  expect_true(is.numeric(sim$elapsed_sec))
  expect_true(sim$elapsed_sec >= 0)
})

test_that("power_guidance returns correct structure for 4 delta values", {
  stim <- example_stimuli_jp()
  sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30), verbose = FALSE)
  pg   <- power_guidance(sim, n_items = 42, n_participants = 24)
  # Should return one row per adjacent delta pair
  expect_equal(nrow(pg), 3)
  expect_true(all(c("delta_low", "delta_high", "peak_asym_diff",
                    "approx_d", "power_rating") %in% names(pg)))
  expect_true(all(pg$power_rating %in%
    c("likely sufficient", "marginal", "likely insufficient", "indeterminate")))
})

test_that("power_guidance errors with fewer than 2 delta values", {
  stim <- example_stimuli_jp()
  sim  <- run_simulation(stim, delta_values = c(0), verbose = FALSE)
  expect_error(power_guidance(sim), "at least 2 delta values")
})

test_that("example_empirical at true_delta=0 resembles universalist curve", {
  stim <- example_stimuli_jp()
  sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30), verbose = FALSE)
  emp  <- example_empirical(sim, true_delta = 0, seed = 1)
  ctrl_group <- emp[emp$group == unique(emp$group)[2], ]
  # Control group (delta=0 generating model) should have small peak asymmetry
  expect_lt(max(ctrl_group$asymmetry), 0.10)
})

test_that("competing-hypothesis: delta=0 and delta=30 produce distinguishable curves", {
  # This is the core competing-hypothesis test analogous to cofad's contrast logic
  stim <- example_stimuli_jp()
  sim  <- run_simulation(stim, delta_values = c(0, 30), verbose = FALSE)
  asym_0  <- sim$results[["0"]]$asymmetry
  asym_30 <- sim$results[["30"]]$asymmetry
  # The peak asymmetry difference should be meaningfully positive
  peak_diff <- max(asym_30) - max(asym_0)
  expect_gt(peak_diff, 0.01)
  # Onset delay: small-grain competitor should activate later at delta=30
  onset_0  <- find_onset(sim$results[["0"]]$small_effect)
  onset_30 <- find_onset(sim$results[["30"]]$small_effect)
  if (!is.na(onset_0) && !is.na(onset_30)) {
    expect_gte(onset_30, onset_0)
  }
})
