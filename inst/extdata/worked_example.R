# phonActivR: complete worked example (v0.3.0)

# Step 1: install and load
# devtools::install_github("yclinpsy/phonActivR@v0.3.0",
#                          build_vignettes = TRUE)
library(phonActivR)

# Steps 2-3: the built-in 42-item stimulus set
stim <- example_stimuli()

# Quick Start: a successful run on synthetic data
sim5 <- run_simulation(stim, delta_values = seq(0, 30, 5))
synth <- example_empirical(sim5, true_delta = 10,
           paradigm = "mouse-tracking", seed = 1)
attr(fit_components(sim5, synth), "best_delta")
#  L2 bilinguals L1 monolinguals
#             10               0

# Step 4a: simulate the delta grid (Table 4)
sim <- run_simulation(stim,
                      delta_values = c(0, 10, 20, 30))
summary(sim)

# Step 4b: screen the design
power_guidance(sim, n_items = 42, n_participants = 41,
               empirical_sd = 0.156, scale = 0.40)

# Step 5: predictions (Figures 3-5)
plot_competition(sim)
plot_asymmetry(sim)
plot_onset_timing(sim, mean_trial_ms = 1200)

# Step 6a: asymmetry fit (Table 5, left; Figure 6A-B)
data(lin2023_english)
goodness_of_fit(sim, lin2023_english)
overlay_empirical(sim, lin2023_english,
                  show_pre_onset = TRUE,
                  facet_groups = TRUE)
shape_baseline(lin2023_english)

# Step 6b: joint component fit (Table 5, right),
# then on the finer grid, and the checks (Table 6)
fit_components(sim, lin2023_english)
fit_components(sim5, lin2023_english)
goodness_of_fit(sim, lin2023_english,
                time_window = c(50, 101))
for (ss in c(8, 10, 12, 14)) {
  sim_ss <- run_simulation(stim, seg_steps = ss,
                           delta_values = seq(0, 30, 5),
                           verbose = FALSE)
  print(attr(fit_components(sim_ss, lin2023_english,
                            verbose = FALSE),
             "best_delta"))
}
for (cw in c(0.5, 0.8)) {
  sim_cw <- run_simulation(stim, c_weight = cw,
                           delta_values = seq(0, 30, 5),
                           verbose = FALSE)
  print(attr(fit_components(sim_cw, lin2023_english,
                            verbose = FALSE),
             "best_delta"))
}
fit_components(sim5, lin2023_english,  # separate C scale
               shared_scale = FALSE)
# Participant bootstrap of the selected delta (Table 6)
subj <- read.csv(system.file("extdata/lin2023_validation",
  "lin2023_subject_curves.csv", package = "phonActivR"))
bootstrap_delta(sim5, subj, n_boot = 200)  # 1000 in Table 6

# Validations 2, 4, and 5
cohort_rhyme_benchmark()
sensitivity_analysis(stim,
  alpha_values = c(0.04, 0.08, 0.12),
  decay_values = c(0.01, 0.02, 0.04),
  gamma_values = c(0.02, 0.04, 0.06),
  seg_values   = c(8, 10, 12))
parameter_recovery(sim, true_delta = 15,
                   noise_sd = 0.03)

# Application 1: delay versus truncation (Table S3)
sim_trunc <- run_simulation(stim, gate = "truncate",
                            delta_values = c(0, 10, 20, 30))
res <- compare_languages(sim, sim_trunc,
  label_a = "Buffered delay",
  label_b = "Signal-locked truncation")
res$comparison
fit_components(sim_trunc, lin2023_english)
# Application 2: the same bilinguals listening to Mandarin (Table 7)
stim_zh <- example_stimuli_zh()
sm_zh <- mandarin_similarity()
for (g in c("delay", "truncate")) {
  sim_zh <- run_simulation(stim_zh, gate = g,
    delta_values = seq(0, 30, 5),
    similarity_matrix = sm_zh, verbose = FALSE)
  print(attr(fit_components(sim_zh, lin2023_chinese,
                            verbose = FALSE),
             "best_delta"))
}

# Step 7: export
export_results(sim, prefix = "lin2023_sim",
               dir = "output/")
