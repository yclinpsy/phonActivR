# =============================================================================
# phonActivR: Tests for Validation & Benchmarking Functions
# =============================================================================

test_that("sensitivity_analysis preserves delta ordering", {
  stim <- example_stimuli_jp()
  sens <- sensitivity_analysis(stim,
    alpha_values = c(0.04, 0.08),
    decay_values = c(0.01, 0.02),
    gamma_values = c(0.04),
    delta_values = c(0, 20),
    verbose = FALSE)
  expect_s3_class(sens, "phonActivR_sensitivity")
  expect_true(sens$robust)
  expect_equal(nrow(sens$grid), 4)
})

test_that("parameter_recovery recovers known delta", {
  stim <- example_stimuli_jp()
  sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30), verbose = FALSE)
  rec  <- parameter_recovery(sim, true_delta = 10, n_replications = 20,
                              noise_sd = 0.005, seed = 42)
  # With low noise and exact delta, recovery should be high

  expect_gt(rec$recovery_rate, 0.5)
  expect_true(rec$r_squared > 0.8)
})

test_that("cohort_rhyme_benchmark all checks pass", {
  bench <- cohort_rhyme_benchmark(verbose = FALSE)
  expect_true(bench$cohort_gt_unrelated)
  expect_true(bench$cohort_gt_rhyme)
  expect_true(bench$delta_gating_correct)
  expect_true(bench$all_pass)
})

test_that("goodness_of_fit identifies correct delta for synthetic data", {
  stim <- example_stimuli_jp()
  sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30), verbose = FALSE)
  emp  <- example_empirical(sim, true_delta = 20, noise_sd = 0.005, seed = 100)
  exp_grp <- emp[emp$group == unique(emp$group)[1], ]
  gof  <- goodness_of_fit(sim, exp_grp)
  # Best-fitting delta should be 20 (the true value)
  best <- gof$delta[which.min(gof$aic)]
  expect_equal(best, 20)
})

test_that("sensitivity_analysis handles single parameter values", {
  stim <- example_stimuli_jp()
  sens <- sensitivity_analysis(stim,
    alpha_values = c(0.08),
    decay_values = c(0.02),
    gamma_values = c(0.04),
    delta_values = c(0, 20),
    verbose = FALSE)
  expect_equal(nrow(sens$grid), 1)
  expect_true(sens$robust)
})

test_that("compare_languages produces valid output", {
  stim <- example_stimuli_jp()
  sim_a <- run_simulation(stim, delta_values = c(0, 10, 20), verbose = FALSE)
  sim_b <- run_simulation(stim, delta_values = c(0, 5, 10, 15), verbose = FALSE)
  result <- compare_languages(sim_a, sim_b,
    label_a = "Japanese", label_b = "Chinese")
  expect_true(inherits(result$plot, "gg") || inherits(result$plot, "patchwork"))
  expect_true(is.data.frame(result$comparison))
  expect_equal(nrow(result$comparison), 2)
  expect_true(all(c("group", "max_peak_asym", "delta_at_peak") %in% names(result$comparison)))
})
