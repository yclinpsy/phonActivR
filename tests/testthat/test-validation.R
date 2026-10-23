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

test_that("parameter_recovery recovers an on-grid delta", {
  stim <- example_stimuli_jp()
  sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30), verbose = FALSE)
  rec  <- parameter_recovery(sim, true_delta = 10, n_replications = 20,
                              noise_sd = 0.005, seed = 42)
  expect_true(rec$on_grid)
  expect_equal(rec$bracket, 10)
  expect_gt(rec$recovery_rate, 0.8)
})

test_that("parameter_recovery treats an off-grid delta via its bracket", {
  stim <- example_stimuli_jp()
  sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30), verbose = FALSE)
  rec  <- parameter_recovery(sim, true_delta = 15, n_replications = 10,
                              noise_sd = 0.005, seed = 7)
  expect_false(rec$on_grid)
  expect_equal(rec$bracket, c(10, 20))
  expect_equal(sum(rec$selection_table), 10)
})

test_that("example_empirical uses the model's own curve, not interpolation", {
  stim <- example_stimuli_jp()
  sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30), verbose = FALSE)
  direct <- run_simulation(stim, delta_values = 15, verbose = FALSE, seed = 1234L)
  expect_equal(phonActivR:::.simulate_asymmetry(sim, 15),
               direct$results[["15"]]$asymmetry)
  lin <- 0.5 * sim$results[["10"]]$asymmetry + 0.5 * sim$results[["20"]]$asymmetry
  expect_false(isTRUE(all.equal(phonActivR:::.simulate_asymmetry(sim, 15), lin)))
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

test_that("fit_components reports joint and per-component fit", {
  sim <- run_simulation(example_stimuli(), delta_values = c(0, 10),
                        verbose = FALSE, seed = 1234)
  fc <- suppressWarnings(fit_components(sim, lin2023_english, lag = 0:50, verbose = FALSE))
  expect_true(all(c("r2", "r2_cv", "r2_c", "lag", "delta_aic") %in% names(fc)))
  expect_equal(nrow(fc), 4)
  expect_true(all(fc$r2_cv > fc$r2_c))   # C competition is fitted worse
  expect_error(fit_components(sim, data.frame(time = 1:101, asymmetry = 0)))
})

test_that("shape_baseline fits a single bump", {
  d <- data.frame(time = 1:101, asymmetry = dnorm(1:101, 60, 8))
  b <- shape_baseline(d)
  expect_equal(b$center, 60); expect_equal(b$width, 8)
  expect_gt(b$r2, 0.999)
})

test_that("a latency on the edge of the search range triggers a warning", {
  sim <- run_simulation(example_stimuli(), delta_values = c(0, 10),
                        verbose = FALSE, seed = 1234)
  expect_warning(goodness_of_fit(sim, lin2023_english, lag = 0:20,
                                 verbose = FALSE), "edge of the search range")
})

test_that("parameter_recovery works with a latency", {
  sim <- run_simulation(example_stimuli(), delta_values = c(0, 10, 20, 30),
                        verbose = FALSE, seed = 1234)
  rec <- parameter_recovery(sim, true_delta = 20, noise_sd = 0.015,
                            n_replications = 10, true_lag = 30, lag = 0:50)
  expect_gt(rec$recovery_rate, 0.8)
})

test_that("gate = 'truncate' is signal-locked: a delta of one segment removes C support", {
  st <- example_stimuli()
  tr <- run_simulation(st, delta_values = c(0, 5, 10, 20), gate = "truncate", verbose = FALSE)
  de <- run_simulation(st, delta_values = c(0, 5, 10, 20), verbose = FALSE)
  # identical at delta = 0
  expect_equal(tr$results[["0"]]$small_effect, de$results[["0"]]$small_effect)
  # truncation shrinks C competition; one full segment removes it
  expect_lt(max(tr$results[["5"]]$small_effect), max(tr$results[["0"]]$small_effect))
  # (only the control's weak input remains: a tiny negative effect)
  expect_lt(max(tr$results[["10"]]$small_effect), 1e-3)
  expect_lt(max(tr$results[["20"]]$small_effect), 1e-3)
  expect_gt(min(tr$results[["20"]]$small_effect), -0.005)
  # the delay mode instead shifts C competition later
  expect_gt(which.max(de$results[["20"]]$small_effect), which.max(de$results[["0"]]$small_effect))
  expect_equal(tr$params$gate, "truncate")
  expect_error(run_simulation(st, gate = "nonsense", verbose = FALSE))
})

test_that("the simulation is deterministic and leaves the global RNG alone", {
  st <- example_stimuli()
  set.seed(7); u1 <- runif(1)
  set.seed(7); s1 <- run_simulation(st, delta_values = 10, seed = 1, verbose = FALSE); u2 <- runif(1)
  s2 <- run_simulation(st, delta_values = 10, seed = 999, verbose = FALSE)
  expect_identical(s1$results, s2$results)
  expect_identical(u1, u2)
  set.seed(7); e <- example_empirical(s1, true_delta = 10, seed = 3); u3 <- runif(1)
  expect_identical(u1, u3)
  expect_true(all(c("cv_effect", "c_effect") %in% names(e)))
  expect_equal(e$asymmetry, e$cv_effect - e$c_effect)
})

test_that("parameter_recovery supports the joint fit and misspecified generation", {
  sim <- run_simulation(example_stimuli(), delta_values = c(0, 10, 20), verbose = FALSE)
  r1 <- parameter_recovery(sim, true_delta = 10, n_replications = 10,
                           method = "components", verbose = FALSE)
  expect_equal(r1$recovery_rate, 1)
  r2 <- parameter_recovery(sim, true_delta = 0, n_replications = 10, method = "components",
                           generate_with = list(residual_input = 1), lag = 0:50,
                           verbose = FALSE)
  expect_equal(r2$method, "components")
  expect_equal(r2$generate_with$residual_input, 1)
  expect_length(r2$best_deltas, 10)
})

test_that("fitting functions reject time axes that are not model steps", {
  sim <- run_simulation(example_stimuli(), delta_values = c(0, 10), verbose = FALSE)
  bad <- data.frame(time = 0:100, asymmetry = 0, cv_effect = 0, c_effect = 0)
  expect_error(goodness_of_fit(sim, bad, verbose = FALSE), "integer normalized time steps")
  expect_error(fit_components(sim, bad, verbose = FALSE), "integer normalized time steps")
  ms <- data.frame(time = seq(0, 2000, 20), asymmetry = 0)
  expect_error(goodness_of_fit(sim, ms, verbose = FALSE), "integer normalized time steps")
})

test_that("power_guidance converts model units with the supplied scale", {
  sim <- run_simulation(example_stimuli(), delta_values = c(0, 10), verbose = FALSE)
  a <- power_guidance(sim, n_participants = 40, empirical_sd = 0.04, scale = 0.25)
  expect_equal(a$max_diff_data, round(a$max_curve_diff * 0.25, 4), tolerance = 1e-3)
  expect_equal(a$expected_z, round(a$approx_d * sqrt(20), 2), tolerance = 0.02)
  b <- power_guidance(sim, n_participants = 40, empirical_sd = 0.04)
  expect_true(is.na(b$max_diff_data))
  expect_gt(b$approx_d, a$approx_d)
})

test_that("export_results writes every setting needed to re-run", {
  sim <- run_simulation(example_stimuli(), delta_values = c(0, 10), seg_steps = 12,
                        residual_input = 0.5, gate = "truncate", verbose = FALSE)
  d <- tempfile(); f <- export_results(sim, prefix = "t", dir = d)
  p <- utils::read.csv(file.path(d, "t_parameters.csv"))
  expect_equal(p$value[p$parameter == "seg_steps"], "12")
  expect_equal(p$value[p$parameter == "residual_input"], "0.5")
  expect_equal(p$value[p$parameter == "gate"], "truncate")
  expect_true(file.exists(file.path(d, "t_sessionInfo.txt")))
})

test_that("component fit options and baseline AIC", {
  sim <- run_simulation(example_stimuli(), delta_values = c(0, 10), verbose = FALSE)
  a <- suppressWarnings(fit_components(sim, lin2023_english, verbose = FALSE))
  b <- suppressWarnings(fit_components(sim, lin2023_english, shared_scale = FALSE, verbose = FALSE))
  expect_true(all(b$r2_c >= a$r2_c - 1e-8))
  expect_true(all(b$n_params == a$n_params + 1))
  sb <- shape_baseline(lin2023_english)
  expect_true(all(c("aic", "n_params") %in% names(sb)))
})

test_that("published values are reproduced (regression pins)", {
  sim <- run_simulation(example_stimuli(), delta_values = c(0, 10, 20, 30), verbose = FALSE)
  s <- sim$summary
  expect_equal(round(sapply(sim$results, function(r) max(r$small_effect)), 3),
               c(`0` = 0.218, `10` = 0.117, `20` = 0.081, `30` = 0.069))
  expect_equal(round(max(sim$results[["0"]]$large_effect), 3), 0.543)
  g <- goodness_of_fit(sim, lin2023_english, lag = 0:50, verbose = FALSE)
  expect_equal(g$r2, c(.965, .957, .962, .970, .731, .834, .780, .763))
  expect_equal(g$lag, c(37, 38, 39, 39, 37, 39, 39, 39))
  fc <- suppressWarnings(fit_components(sim, lin2023_english, lag = 0:50, verbose = FALSE))
  expect_equal(fc$r2_c, c(.256, .011, -.444, -.527, .300, .282, -.054, -.272))
  expect_equal(unname(attr(fc, "best_delta")), c(0, 10))
})

test_that("c_weight sets the strength of C competitor input", {
  expect_equal(compute_overlap(c("b", "eh"), "b", "C"), 0.5)
  expect_equal(compute_overlap(c("b", "eh"), "b", "C", c_weight = 0.8), 0.8)
  expect_error(compute_overlap(c("b", "eh"), "b", "C", c_weight = 0))
  st <- example_stimuli()
  a <- run_simulation(st, delta_values = 0, verbose = FALSE)
  b <- run_simulation(st, delta_values = 0, c_weight = 0.8, verbose = FALSE)
  expect_gt(max(b$results[["0"]]$small_effect), max(a$results[["0"]]$small_effect))
  expect_equal(a$results[["0"]]$large_effect, b$results[["0"]]$large_effect)
  expect_equal(b$params$c_weight, 0.8)
})

test_that("fit_components() flags a poorly fitted C component", {
  sim <- run_simulation(example_stimuli(), verbose = FALSE)
  expect_warning(f <- fit_components(sim, lin2023_english, verbose = FALSE),
                 "poorly fitted")
  expect_false(any(attr(f, "c_fit_ok")))
})

test_that("bootstrap_delta() resamples participants and tabulates delta", {
  sc <- read.csv(system.file("extdata", "lin2023_validation",
                             "lin2023_subject_curves.csv", package = "phonActivR"),
                 check.names = FALSE)
  sim <- run_simulation(example_stimuli(), delta_values = c(0, 10, 20, 30),
                        verbose = FALSE)
  b <- bootstrap_delta(sim, sc, n_boot = 5, seed = 7, verbose = FALSE)
  expect_s3_class(b, "phonActivR_bootstrap")
  expect_equal(nrow(b$draws), 10L)
  expect_true(all(b$draws$delta %in% c(0, 10, 20, 30)))
  expect_equal(sum(b$summary$percent[b$summary$group == b$summary$group[1]]), 100)
  b2 <- bootstrap_delta(sim, sc, n_boot = 5, seed = 7, verbose = FALSE)
  expect_equal(b$draws$delta, b2$draws$delta)   # local seed, reproducible
})

test_that("the input window supplies the same number of steps at every delta", {
  # Without inhibition or decay, the competitor's activation depends only on
  # its own input, so the delayed curve must equal the undelayed curve
  # shifted by delta (the step-1 input acts at step 1).
  a0 <- run_activation(0.5, 0, overlap_onset = 1, overlap_offset = 11,
                       gamma = 0, decay = 0)$act_comp
  a7 <- run_activation(0.5, 0, overlap_onset = 8, overlap_offset = 18,
                       gamma = 0, decay = 0)$act_comp
  expect_equal(a7[8:101], a0[1:94])
  expect_equal(sum(diff(c(0, a0)) > 0), 10L)
  expect_equal(sum(diff(c(0, a7)) > 0), 10L)
})

test_that("the Mandarin stimulus set, similarity table, and data are consistent", {
  stim <- example_stimuli_zh()
  expect_equal(nrow(stim$items), 24L)
  sm <- mandarin_similarity()
  cov <- check_phoneme_coverage(stim, similarity_matrix = sm, warn = FALSE, announce = FALSE)
  expect_length(attr(cov, "missing_pairs"), 0)
  expect_equal(sm["B", "P"], 0.7)
  expect_equal(sm["T", "H"], 0.3)
  data(lin2023_chinese)
  expect_equal(nrow(lin2023_chinese), 101L)
  expect_lt(max(lin2023_chinese$c_effect) / max(lin2023_chinese$cv_effect), 0.1)
  # Regression pins for Application 2 (Table 7): both gate readings put delta
  # at one segment or more on the Mandarin data.
  st <- run_simulation(stim, delta_values = seq(0, 30, 5), gate = "truncate",
                       similarity_matrix = sm, verbose = FALSE)
  ft <- suppressWarnings(fit_components(st, lin2023_chinese, verbose = FALSE))
  expect_equal(unname(attr(ft, "best_delta")), 10)
  expect_equal(round(ft$r2[ft$delta == 10], 3), 0.941)
  sd_ <- run_simulation(stim, delta_values = seq(0, 30, 5), similarity_matrix = sm,
                        verbose = FALSE)
  fd <- suppressWarnings(fit_components(sd_, lin2023_chinese, verbose = FALSE))
  expect_equal(unname(attr(fd, "best_delta")), 15)
  expect_equal(round(fd$r2[fd$delta == 15], 3), 0.943)
})
