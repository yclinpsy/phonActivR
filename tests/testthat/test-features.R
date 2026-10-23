
test_that("compared pairs that the feature matrix does not distinguish are reported", {
  stim <- example_stimuli()
  x <- check_phoneme_coverage(stim, warn = FALSE, announce = FALSE)
  expect_equal(attr(x, "identical_pairs"), "b-jh")
  # A custom similarity for the pair removes it from the report
  cs <- custom_similarity(data.frame(p1 = "b", p2 = "jh", similarity = 0.3))
  y <- check_phoneme_coverage(stim, similarity_matrix = cs,
                              warn = FALSE, announce = FALSE)
  expect_length(attr(y, "identical_pairs"), 0)
})
