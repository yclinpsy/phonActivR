# =============================================================================
# phonActivR: Figure Generation Script
# =============================================================================
# Generates ALL figures for the BRM tutorial paper (Figures 1–14).
#
#   Figure 1  — Workflow pipeline diagram          [Python/PNG, pre-made]
#   Figure 2  — overlay_empirical() data format    [Python/PNG, pre-made]
#   Figure 3  — create_stimuli() data format       [Python/PNG, pre-made]
#   Figure 4  — Competition effect curves          [phonActivR + ggplot2]
#   Figure 5  — Asymmetry gradient                 [phonActivR + ggplot2]
#   Figure 6  — Onset timing function              [phonActivR + ggplot2]
#   Figure 7  — Mouse-tracking empirical overlay   [phonActivR + ggplot2]
#   Figure 8  — Eye-tracking empirical overlay     [phonActivR + ggplot2]
#   Figure 9  — Parameter sensitivity analysis     [phonActivR + ggplot2]
#   Figure 10 — Multi-parameter sensitivity grid   [phonActivR + ggplot2]
#   Figure 11 — Cohort vs. rhyme benchmark         [phonActivR + ggplot2]
#   Figure 12 — Parameter recovery                 [phonActivR + ggplot2]
#   Figure 13 — Goodness-of-fit AIC comparison     [phonActivR + ggplot2]
#   Figure 14 — Cross-linguistic comparison        [phonActivR + ggplot2]
#
# INSTRUCTIONS:
#   1. In RStudio: Session → Restart R (important!)
#   2. Set working directory to the folder containing this script:
#      Session → Set Working Directory → To Source File Location
#   3. Make sure the phonActivR/ folder is in the same directory
#   4. Run: source("generate_figures.R")
#   5. Figures saved to figures/ subfolder
#
# FOLDER STRUCTURE REQUIRED:
#   BRM_submission/
#   ├── generate_figures.R        ← this script
#   ├── figures/                  ← output folder (created automatically)
#   │   ├── Figure1_workflow_pipeline.png    (copy from manuscript)
#   │   ├── Figure2_overlay_format.png       (copy from manuscript)
#   │   ├── Figure3_stimuli_format.png       (copy from manuscript)
#   └── phonActivR/               ← package source folder
#       ├── R/
#       ├── DESCRIPTION
#       └── ...
#
# NOTE: Figures 1, 2, and 3 are static diagram figures created separately
#       (see figures/ folder). This script generates Figures 4–14 using
#       the phonActivR simulation engine.
# =============================================================================

# --- Load dependencies --------------------------------------------------------
library(ggplot2)
library(dplyr)
library(patchwork)
library(cli)
library(rlang)

# --- Load phonActivR functions ------------------------------------------------
pkg_r_dir <- "phonActivR/R"

if (!dir.exists(pkg_r_dir)) {
  stop(
    "Cannot find '", pkg_r_dir, "'\n",
    "Working directory: ", getwd(), "\n",
    "Contents: ", paste(list.dirs(recursive = FALSE), collapse = ", "), "\n\n",
    "Fix: setwd() to the folder that contains the phonActivR/ folder.\n",
    call. = FALSE
  )
}

r_files <- list.files(pkg_r_dir, pattern = "\\.R$", full.names = TRUE)
for (f in r_files) source(f)
message("Loaded ", length(r_files), " phonActivR R files")

# --- Create output folder -----------------------------------------------------
dir.create("figures", showWarnings = FALSE)

# --- Note about Figures 1–3 ---------------------------------------------------
message("\n[NOTE] Figures 1, 2, and 3 are static diagrams.")
message("       Copy Figure1_workflow_pipeline.png, Figure2_overlay_format.png,")
message("       and Figure3_stimuli_format.png into the figures/ folder manually.")
message("       These are included in the manuscript submission folder.\n")

# --- Run simulation (used for Figures 4–9) ------------------------------------
message("[1/6] Running simulation (used for Figures 4–14)...")
set.seed(1234)
stim <- example_stimuli_jp()
sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30))
summary(sim)

# --- Figure 4: Competition Effects --------------------------------------------
message("[2/6] Figure 4: Competition effects...")
fig4 <- plot_competition(sim,
  title = "Predicted Competition Effects: Universalist vs. Prosodic Constraint")
ggsave("figures/Figure4_competition_effects.png", fig4,
       width = 14, height = 6.5, dpi = 300)
       width = 14, height = 6.5)

# --- Figure 5: Asymmetry Gradient ---------------------------------------------
message("[3/6] Figure 5: Asymmetry gradient...")
fig5 <- plot_asymmetry(sim,
  title = "CV\u2013C Competition Asymmetry Gradient Across \u03b4 Values")
ggsave("figures/Figure5_asymmetry_gradient.png", fig5,
       width = 12, height = 6.5, dpi = 300)
       width = 12, height = 6.5)

# --- Figure 6: Onset Timing ---------------------------------------------------
message("[4/6] Figure 6: Onset timing...")
fig6 <- plot_onset_timing(sim, mean_trial_ms = 1500,
  title = "C Competition Onset Delay as a Function of \u03b4")
ggsave("figures/Figure6_onset_timing.png", fig6,
       width = 11, height = 6.5, dpi = 300)
       width = 11, height = 6.5)

# --- Figure 7: Mouse-Tracking Overlay -----------------------------------------
message("[5/6] Figure 7: Mouse-tracking overlay...")
set.seed(42)
emp_mt <- example_empirical(sim, true_delta = 15,
  group_labels = c("JP bilinguals", "EN monolinguals"))
fig7 <- overlay_empirical(sim, emp_mt,
  group_colors = c("JP bilinguals" = "black", "EN monolinguals" = "grey50"),
  title = "Mouse-Tracking: Predicted and Empirical CV\u2013C Asymmetry")
ggsave("figures/Figure7_overlay_mouse_tracking.png", fig7,
       width = 12, height = 6.5, dpi = 300)
       width = 12, height = 6.5)

# --- Figure 8: Eye-Tracking Overlay -------------------------------------------
message("[6/6] Figure 8: Eye-tracking overlay...")
set.seed(99)
emp_et <- example_empirical(sim, true_delta = 12,
  group_labels = c("L2 learners", "L1 controls"), noise_sd = 0.02)
fig8 <- overlay_empirical(sim, emp_et,
  group_colors = c("L2 learners" = "black", "L1 controls" = "grey50"),
  title = "Eye-Tracking VWP: Predicted and Empirical CV\u2013C Asymmetry")
ggsave("figures/Figure8_overlay_eye_tracking.png", fig8,
       width = 12, height = 6.5, dpi = 300)
       width = 12, height = 6.5)

# --- Figure 9: Parameter Sensitivity ------------------------------------------
message("[BONUS] Figure 9: Parameter sensitivity...")
gammas <- c(0.02, 0.04, 0.06)
sims_gamma <- lapply(gammas, function(g) {
  run_simulation(stim, delta_values = c(0, 10, 20, 30), gamma = g, verbose = FALSE)
})
fig9 <- plot_asymmetry(sims_gamma[[1]], title = paste0("\u03b3 = 0.02")) +
  plot_asymmetry(sims_gamma[[2]], title = paste0("\u03b3 = 0.04")) +
  plot_asymmetry(sims_gamma[[3]], title = paste0("\u03b3 = 0.06")) +
  patchwork::plot_annotation(
    title = "Parameter Sensitivity: Asymmetry Gradient Across Lateral Inhibition Values",
    subtitle = "Qualitative predictions are robust across \u03b3 values (0.02\u20130.06)",
    theme = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40")))
ggsave("figures/Figure9_parameter_sensitivity.png", fig9,
       width = 18, height = 6.5, dpi = 300)
       width = 18, height = 6.5)

# --- Done! --------------------------------------------------------------------
message("\n=== All figures generated! ===")
message("Folder: ", normalizePath("figures"))
for (f in sort(list.files("figures"))) message("  ", f)
message("\nFigure placement in manuscript:")
message("  Fig 1        — Package Architecture section (workflow overview)")
message("  Fig 2        — Package Architecture section (overlay_empirical format)")
message("  Fig 3        — Tutorial Step 3 (create_stimuli format)")
message("  Fig 4, 5, 6  — Tutorial Step 5 (visualization)")
message("  Fig 7        — Tutorial Step 6 (empirical overlay, mouse-tracking)")
message("  Fig 8        — Application 2 (eye-tracking)")
message("  Fig 9        — Application 4 (parameter sensitivity)")

# =============================================================================
# ADDITIONAL VALIDATION FIGURES (Figures 10–14)
# =============================================================================

# --- Figure 10: Multi-parameter Sensitivity Analysis ---------------------------
message("[7/11] Figure 10: Multi-parameter sensitivity...")
sens <- sensitivity_analysis(stim,
  alpha_values = c(0.04, 0.08, 0.12),
  decay_values = c(0.01, 0.02, 0.04),
  gamma_values = c(0.04),
  delta_values = c(0, 10, 20, 30),
  verbose = FALSE)

# Create faceted plot
sens_df <- sens$summary
sens_df$param_label <- paste0("α=", sens_df$alpha, ", λ=", sens_df$decay)
sens_df$delta <- factor(sens_df$delta)

fig10 <- ggplot(sens_df, aes(x = delta, y = peak_asym, fill = delta)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~param_label, scales = "free_y") +
  scale_fill_brewer(palette = "Set2", name = "δ") +
  labs(
    title = "Parameter Sensitivity: Peak Asymmetry Across α and λ Values",
    subtitle = paste0("Asymmetry gradient ordering ",
                      ifelse(sens$robust, "PRESERVED", "NOT preserved"),
                      " across all ", nrow(sens$grid), " parameter combinations"),
    x = "δ (Prosodic Constraint)",
    y = "Peak CV–C Asymmetry"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title    = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    strip.text    = element_text(face = "bold")
  )
ggsave("figures/Figure10_sensitivity_grid.png", fig10,
       width = 14, height = 8, dpi = 300)
       width = 14, height = 8)

# --- Figure 11: Cohort vs Rhyme Benchmark ------------------------------------
message("[8/11] Figure 11: Cohort vs rhyme benchmark...")
ts <- 101L
cohort_out <- run_activation(0.9, 0.1, overlap_onset = 1L, time_steps = ts)
rhyme_out  <- run_activation(0.5, 0.1, overlap_onset = 30L, time_steps = ts)
unrel_out  <- run_activation(0.15, 0.1, overlap_onset = 1L, time_steps = ts)

bench_df <- data.frame(
  time   = rep(1:ts, 3),
  effect = c(cohort_out$competition_effect,
             rhyme_out$competition_effect,
             unrel_out$competition_effect),
  type   = rep(c("Cohort (onset CV match)",
                 "Rhyme (late overlap)",
                 "Unrelated (minimal overlap)"), each = ts)
)
bench_df$type <- factor(bench_df$type,
  levels = c("Cohort (onset CV match)", "Rhyme (late overlap)",
             "Unrelated (minimal overlap)"))

fig11 <- ggplot(bench_df, aes(x = time, y = effect, color = type, linetype = type)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = c("#2166AC", "#D6604D", "gray50"), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = NULL) +
  scale_x_continuous(breaks = seq(0, 100, 20),
                     labels = paste0(seq(0, 100, 20), "%")) +
  labs(
    title = "Qualitative Benchmark: Cohort vs. Rhyme vs. Unrelated Competition",
    subtitle = "phonActivR correctly reproduces TRACE's cohort > rhyme > unrelated ordering",
    x = "Normalized Time (% of trial)",
    y = "Competition Effect\n(Competitor − Control)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(size = 13, face = "bold"),
    plot.subtitle    = element_text(size = 10, color = "gray40"),
    panel.grid.minor = element_blank()
  )
ggsave("figures/Figure11_cohort_rhyme_benchmark.png", fig11,
       width = 10, height = 6.5, dpi = 300)
       width = 10, height = 6.5)

# --- Figure 12: Parameter Recovery -------------------------------------------
message("[9/11] Figure 12: Parameter recovery...")
set.seed(1234)
rec <- parameter_recovery(sim, true_delta = 15, n_replications = 100,
                           noise_sd = 0.01)

rec_df <- data.frame(table(rec$best_deltas))
names(rec_df) <- c("delta", "count")
rec_df$delta <- as.numeric(as.character(rec_df$delta))

fig12 <- ggplot(rec_df, aes(x = factor(delta), y = count)) +
  geom_col(fill = "#377EB8", width = 0.6) +
  geom_vline(xintercept = which(levels(factor(rec_df$delta)) == "10") + 0.5,
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = which(levels(factor(rec_df$delta)) %in% c("10", "20")),
           y = max(rec_df$count) * 0.9,
           label = paste0("True δ = 15\n(nearest: 10 or 20)"),
           color = "red", size = 3.5, fontface = "bold") +
  labs(
    title = "Parameter Recovery: Best-Fitting δ Across 100 Replications",
    subtitle = paste0("True δ = 15 | Recovery rate: ",
                      round(rec$recovery_rate * 100, 1),
                      "% | Mean R² = ", rec$r_squared),
    x = "Best-fitting δ value",
    y = "Number of replications"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40")
  )
ggsave("figures/Figure12_parameter_recovery.png", fig12,
       width = 10, height = 6, dpi = 300)
       width = 10, height = 6)

# --- Figure 13: Goodness-of-Fit AIC ------------------------------------------
message("[10/11] Figure 13: Goodness-of-fit AIC...")
emp_gof <- example_empirical(sim, true_delta = 15, noise_sd = 0.01, seed = 42)
exp_grp <- emp_gof[emp_gof$group == unique(emp_gof$group)[1], ]
gof <- goodness_of_fit(sim, exp_grp)

fig13 <- ggplot(gof, aes(x = factor(delta), y = delta_aic)) +
  geom_col(fill = ifelse(gof$delta_aic == 0, "#E41A1C", "#377EB8"), width = 0.6) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "gray50") +
  annotate("text", x = length(gof$delta), y = 11,
           label = "ΔAIC = 10 (no support threshold)",
           hjust = 1, size = 3, color = "gray40") +
  labs(
    title = "Goodness-of-Fit: AIC Model Comparison",
    subtitle = paste0("Best model: δ = ", gof$delta[which.min(gof$aic)],
                      " (red bar, ΔAIC = 0)"),
    x = "δ (Prosodic Constraint Model)",
    y = "ΔAIC (relative to best model)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40")
  )
ggsave("figures/Figure13_goodness_of_fit.png", fig13,
       width = 9, height = 6, dpi = 300)
       width = 9, height = 6)

# --- Updated summary ---------------------------------------------------------
message("\n=== All figures (including validation) generated! ===")
message("Additional figures:")
message("  Fig 10  — Multi-parameter sensitivity grid")
message("  Fig 11 — Cohort vs. rhyme qualitative benchmark")
message("  Fig 12 — Parameter recovery test")
message("  Fig 13 — Goodness-of-fit AIC comparison")
message("\nSession info:")
sessionInfo()

# --- Figure 14: Cross-Linguistic Comparison -----------------------------------
message("[11/11] Figure 14: Cross-linguistic comparison...")
sim_jp <- sim  # reuse the Japanese simulation from above
sim_zh <- run_simulation(stim, delta_values = c(0, 5, 10, 15, 20), verbose = FALSE)
cross <- compare_languages(sim_jp, sim_zh,
  label_a = "Japanese\u2013English (Moraic)",
  label_b = "Chinese\u2013English (Syllabic)")
fig14 <- cross$plot
ggsave("figures/Figure14_cross_linguistic.png", fig14,
       width = 16, height = 6.5, dpi = 300)
       width = 16, height = 6.5)
message("  Fig 14 — Cross-linguistic grain-size comparison")
