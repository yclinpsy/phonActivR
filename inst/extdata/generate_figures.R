# =============================================================================
# phonActivR: Figure Generation Script (revision, package v0.3.0)
# =============================================================================
# Generates ALL 11 main figures and Figures S1-S3 for the BRM tutorial paper and prints the
# numbers of Tables 4, 5, 7, and S3, of Steps 4-6a, Validations 2-5, and
# Applications 1 and 2, and of Supplemental Material S1.2-S1.3 and S2.7.
# The remaining numbers come from three companion scripts:
#   lin2023_validation/validation.R   (Step 6b held-out items, bootstrap,
#                                      model variants, misspecification,
#                                      c_weight and ctrl_scaling; Table 6 and
#                                      Supplemental Material S2 and S5, Tables S1
#                                      and S4; Mandarin bootstrap for Table 7)
#   linking_tests/linking_tests.R     (Step 6b linking functions)
#   trace_correspondence/correspondence_analysis.R  (Validation 1, Table S2)
# The simulation is deterministic; seeds are needed only for generated noise.
#
# Figures 1-2 are diagrams drawn with base grid graphics;
# Figures 3-11 and S1-S3 are generated from package output with ggplot2. No figure
# bakes a title or subtitle into the image: per APA style, figure titles and
# notes live in the manuscript captions. Panel identifiers (e.g., group
# labels) are drawn because they identify panels.
#
# FIGURE MAP (revised manuscript numbering):
#   Figure 1  - Workflow pipeline diagram                    [grid]
#   Figure 2  - Model architecture (one simulation)          [grid]
#   Figure 3  - Competition effect curves (delta 0 vs 30)    [simulation]
#   Figure 4  - Asymmetry gradient                           [simulation]
#   Figure 5  - Onset timing function                        [simulation]
#   Figure 6  - Fit to real data: asymmetry and components   [simulation + data]
#   Figure 7  - Cohort/rhyme/unrelated benchmark             [simulation]
#   Figure 8 - Distinguishability across the parameter grid [simulation]
#   Figure 9 - Parameter recovery                           [simulation]
#   Figure 10 - Delay versus truncation (compare_languages)  [simulation]
#   Figure 11 - Mandarin listening: fit of both gate readings  [simulation + data]
#   Figure S1 - Absolute fit to real data, 0.2.x vs 0.3.0    [simulation + data]
#   Figure S2 - Identification of delta (seg_steps x delta)  [simulation + data]
#   Figure S3 - Raw trajectories by condition, both languages [data]
#
# HOW TO RUN:
#   1. Install the package:
#      devtools::install_github("yclinpsy/phonActivR@v0.3.0",
#                               build_vignettes = TRUE)
#   2. source() this script from any working directory; figures are written
#      to a figures/ subfolder of the current working directory.
# =============================================================================

# --- Load dependencies -------------------------------------------------------
# The script uses the installed package (preferred). If you are working from
# the package source tree instead, replace the library() call with
# pkgload::load_all("path/to/phonActivR").
library(phonActivR)
library(ggplot2)
library(patchwork)
library(grid)      # base R; used for the diagram/format figures 1-4

# --- Output folder ------------------------------------------------------------
dir.create("figures", showWarnings = FALSE)

# --- Shared delta palette ----------------------------------------------------
# One palette for every figure that colors curves/bars by delta: all figures
# sharing the standard delta = 0/10/20/30 grid share one mapping (delta = 0
# green, 10 blue, 20 orange, 30 vermilion red); both panels of Figure 10 use
# the same grid. Figures 8 and 11 color segment durations, not delta, with a
# separate palette. This is the package-wide
# colorblind-safe Okabe-Ito palette; see ?phonactivr_colors.
delta_pal <- stats::setNames(phonactivr_colors(4), c("0", "10", "20", "30"))

# =============================================================================
# Figure 1: Workflow pipeline diagram (grid graphics)
# =============================================================================
# Six stages, numbered; the functions implementing each stage are shown in a
# white monospaced box; the S3 objects passed between stages are labeled
# beneath the connecting arrows.
{
  png("figures/Figure1_workflow_pipeline.png", width = 11, height = 7.4,
      units = "in", res = 300)
  grid.newpage()

  stages <- list(
    list(num = "1", title = "Stimuli",
         sub = "targets, CV and C\ncompetitors, controls",
         fns = "create_stimuli()\nexample_stimuli()"),
    list(num = "2", title = "Overlap",
         sub = "feature matrix or\ncustom similarities",
         fns = "compute_overlap()\ncustom_similarity()"),
    list(num = "3", title = "Simulation",
         sub = "\u03b4 grid, seg_steps,\ngate; all items",
         fns = "run_simulation()"),
    list(num = "4", title = "Predictions",
         sub = "CV and C curves,\nasymmetry",
         fns = "plot_competition()\nplot_asymmetry()"),
    list(num = "5", title = "Fit to Data",
         sub = "components jointly;\nsingle-bump reference",
         fns = "fit_components()\nshape_baseline()\ngoodness_of_fit()"),
    list(num = "6", title = "Robustness",
         sub = "window, latency, grid,\nseg_steps, bootstrap",
         fns = "sensitivity_analysis()\nparameter_recovery()")
  )
  box_w <- 0.29; box_h <- 0.42
  xc_all <- c(0.17, 0.5, 0.83)
  yc_all <- c(0.74, 0.26)
  greys <- grey(seq(0.97, 0.86, length.out = 6))
  for (i in 1:6) {
    s  <- stages[[i]]
    xc <- xc_all[(i - 1) %% 3 + 1]; yc <- yc_all[(i - 1) %/% 3 + 1]
    grid.roundrect(x = xc, y = yc, width = box_w, height = box_h,
                   r = unit(2.5, "mm"),
                   gp = gpar(fill = greys[i], col = "grey25", lwd = 2.2))
    grid.circle(x = xc - box_w/2 + 0.03, y = yc + box_h/2 - 0.055, r = 0.022,
                gp = gpar(fill = "grey15", col = "white", lwd = 2))
    grid.text(s$num, x = xc - box_w/2 + 0.03, y = yc + box_h/2 - 0.055,
              gp = gpar(col = "white", fontface = "bold", fontsize = 16))
    grid.text(s$title, x = xc, y = yc + box_h/2 - 0.055,
              gp = gpar(fontface = "bold", fontsize = 20))
    grid.text(s$sub, x = xc, y = yc + 0.045,
              gp = gpar(fontsize = 15, col = "grey20", lineheight = 1.05))
    grid.roundrect(x = xc, y = yc - 0.115, width = box_w * 0.9, height = 0.15,
                   r = unit(1.5, "mm"),
                   gp = gpar(fill = "white", col = "grey45", lwd = 1.2))
    grid.text(s$fns, x = xc, y = yc - 0.115,
              gp = gpar(fontfamily = "mono", fontsize = 14.5, lineheight = 1.1))
  }
  arr <- function(x0, y0, x1, y1)
    grid.segments(x0, y0, x1, y1, arrow = arrow(length = unit(3.5, "mm"), type = "closed"),
                  gp = gpar(col = "grey20", lwd = 2.6, fill = "grey20"))
  arr(0.17 + box_w/2 + 0.005, 0.74, 0.5 - box_w/2 - 0.005, 0.74)
  arr(0.5 + box_w/2 + 0.005, 0.74, 0.83 - box_w/2 - 0.005, 0.74)
  # stage 3 -> stage 4 wraps to the second row
  grid.lines(x = c(0.83, 0.83, 0.17, 0.17),
             y = c(0.74 - box_h/2 - 0.005, 0.5, 0.5, 0.26 + box_h/2 + 0.012),
             arrow = arrow(length = unit(3.5, "mm"), type = "closed"),
             gp = gpar(col = "grey20", lwd = 2.6, fill = "grey20"))
  arr(0.17 + box_w/2 + 0.005, 0.26, 0.5 - box_w/2 - 0.005, 0.26)
  arr(0.5 + box_w/2 + 0.005, 0.26, 0.83 - box_w/2 - 0.005, 0.26)
  dev.off()
}

# =============================================================================
# Figure 2: Model architecture of one simulation (grid graphics)
# =============================================================================
# Three units (target, competitor, control); shunted excitatory input with
# the delta gate on the sub-prosodic competitor's input; mutual lateral
# inhibition; passive decay; output note. CV and C competitor types are
# separate runs.
{
  png("figures/Figure2_model_architecture.png", width = 12.2, height = 5.9,
      units = "in", res = 300)
  grid.newpage()
  # y range starts at 0.9: the area below the control unit's input arrow is
  # not used (the figure note in the manuscript carries all explanation)
  pushViewport(viewport(xscale = c(0.2, 12.4), yscale = c(0.9, 6.8)))

  unit_box <- function(x, y, label, sub, fill) {
    grid.roundrect(x = unit(x, "native"), y = unit(y, "native"),
                   width = unit(2.5, "native"), height = unit(1.1, "native"),
                   r = unit(2, "mm"),
                   gp = gpar(fill = fill, col = "black", lwd = 2))
    grid.text(label, x = unit(x, "native"), y = unit(y + 0.17, "native"),
              gp = gpar(fontface = "bold", fontsize = 17))
    grid.text(sub, x = unit(x, "native"), y = unit(y - 0.25, "native"),
              gp = gpar(fontsize = 13, fontface = "italic", col = "#444444"))
  }
  nat_arrow <- function(x0, y0, x1, y1, col, lwd = 2.4, lty = "solid",
                        both = FALSE) {
    grid.segments(x0 = unit(x0, "native"), y0 = unit(y0, "native"),
                  x1 = unit(x1, "native"), y1 = unit(y1, "native"),
                  arrow = arrow(length = unit(3.5, "mm"), type = "closed",
                                ends = if (both) "both" else "last"),
                  gp = gpar(col = col, fill = col, lwd = lwd, lty = lty))
  }

  # Units
  unit_box(2.2, 4.6, "Target",     "input = 1.0",               "#dbe8f6")
  unit_box(9.0, 4.6, "Competitor", "input = overlap (windowed)", "#f6e3db")
  unit_box(5.6, 2.2, "Control",    "input = overlap \u00d7 0.15",    "#e8e8e8")

  # Excitatory input arrows (green)
  nat_arrow(2.2, 5.95, 2.2, 5.2, "#1a6b32")
  grid.text("excitatory input\n\u03b1 \u00b7 input \u00b7 (1 \u2212 a)",
            x = unit(2.2, "native"), y = unit(6.12, "native"),
            just = "bottom", gp = gpar(fontsize = 12.5, col = "#1a6b32"))
  nat_arrow(5.6, 1.18, 5.6, 1.62, "#1a6b32")
  grid.text("excitatory input (not gated; ends with the competitor's)",
            x = unit(5.8, "native"), y = unit(1.4, "native"),
            just = "left", gp = gpar(fontsize = 12.5, col = "#1a6b32"))

  # Delta gate on the competitor's input
  grid.roundrect(x = unit(9.0, "native"), y = unit(5.9, "native"),
                 width = unit(1.4, "native"), height = unit(0.55, "native"),
                 r = unit(1.5, "mm"),
                 gp = gpar(fill = "#fff2cc", col = "#b8860b", lwd = 2))
  grid.text("\u03b4 gate", x = unit(9.0, "native"), y = unit(5.9, "native"),
            gp = gpar(fontface = "bold", fontsize = 14, col = "#7a5a00"))
  nat_arrow(9.0, 5.62, 9.0, 5.2, "#1a6b32")
  grid.text(paste0("sub-prosodic competitors: opens\n",
                   "after \u03b4 steps (delay or truncate);\n",
                   "aligned competitors: open at once"),
            x = unit(9.85, "native"), y = unit(5.9, "native"),
            just = "left", gp = gpar(fontsize = 11.5, col = "#7a5a00"))

  # Lateral inhibition (red dashed, double-headed)
  nat_arrow(3.55, 4.6, 7.65, 4.6, "#a02020", lwd = 2.2, lty = "42", both = TRUE)
  nat_arrow(2.90, 3.95, 4.75, 2.85, "#a02020", lwd = 2.2, lty = "42", both = TRUE)
  nat_arrow(8.30, 3.95, 6.45, 2.85, "#a02020", lwd = 2.2, lty = "42", both = TRUE)
  grid.text("lateral inhibition  \u2212\u03b3 \u00b7 \u03a3 a",
            x = unit(5.6, "native"), y = unit(4.88, "native"),
            gp = gpar(fontsize = 13, col = "#a02020"))

  # Passive decay labels (one per unit, matching the caption's "applied to
  # every unit")
  grid.text("\u2212\u03bb \u00b7 a\n(decay)", x = unit(0.62, "native"),
            y = unit(4.6, "native"), gp = gpar(fontsize = 11.5, col = "#555555"))
  grid.text("\u2212\u03bb \u00b7 a\n(decay)", x = unit(10.75, "native"),
            y = unit(3.85, "native"), gp = gpar(fontsize = 11.5, col = "#555555"))
  grid.text("\u2212\u03bb \u00b7 a\n(decay)", x = unit(3.75, "native"),
            y = unit(1.75, "native"), gp = gpar(fontsize = 11.5, col = "#555555"))

  popViewport()
  dev.off()
}

# =============================================================================
# Base simulation used by most figures
# =============================================================================
# 42-item stimulus set of Lin et al. (2023, Exps 3-4); delta grid 0/10/20/30;
# default parameters (alpha = .08, gamma = .04, decay = .02, seg_steps = 10).
stim <- example_stimuli()          # built-in 42-item stimulus set
sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30))
summary(sim)                       # Table 4 in the paper
data(lin2023_english)              # real data for the same 42 items
LAGS <- 0:50                       # response-latency search range (steps)

# --- Figure 3: Competition Effects -------------------------------------------
fig5 <- plot_competition(sim)
ggsave("figures/Figure3_competition_effects.png", fig5,
       width = 14, height = 6.5, dpi = 300)

# --- Figure 4: Asymmetry Gradient ---------------------------------------------
fig6 <- plot_asymmetry(sim)
ggsave("figures/Figure4_asymmetry_gradient.png", fig6,
       width = 12, height = 6.5, dpi = 300)

# --- Figure 5: Onset Timing ---------------------------------------------------
fig7 <- plot_onset_timing(sim, mean_trial_ms = 1200)
ggsave("figures/Figure5_onset_timing.png", fig7,
       width = 11, height = 6.8, dpi = 300)

# --- Figure 6: Fit to Real Data (Tutorial Steps 6a-6b) -----------------------
# Top row: the asymmetry fit (goodness_of_fit(), drawn by overlay_empirical()).
# Bottom row: the joint component fit (fit_components()): empirical CV and C
# effects with 95% participant-bootstrap bands (from the participant-level
# curves in lin2023_validation/) and the jointly fitted model components for
# the default model and for residual_input = 1, each at its selected delta.
grp <- unique(lin2023_english$group)
top <- lapply(grp, function(g) {
  p <- overlay_empirical(sim, lin2023_english[lin2023_english$group == g, ],
                         lag = LAGS, group_colors = stats::setNames("black", g),
                         title = g, show_pre_onset = TRUE)
  # the panel title names the group; drop the in-plot group label
  p$layers <- Filter(function(l) !inherits(l$geom, c("GeomText", "GeomLabel")), p$layers)
  p
})

sc <- utils::read.csv(system.file("extdata", "lin2023_validation",
                                  "lin2023_subject_curves.csv", package = "phonActivR"),
                      check.names = FALSE)
xc <- paste0("X_", 1:101)
eff_w <- function(tab, w) {
  m <- function(k) { s <- tab$condition == k
    colSums(as.matrix(tab[s, xc]) * tab$n_trials[s] * w[s]) / sum(tab$n_trials[s] * w[s]) }
  cbind(cv = m("CV Control") - m("CV"), c = m("Onset Control") - m("Onset"))
}
bands <- do.call(rbind, lapply(grp, function(g) {
  tab <- sc[sc$group == g, ]; subs <- unique(tab$subject)
  boot <- withr::with_seed(2023, replicate(1000, {
    k <- table(factor(sample(subs, replace = TRUE), subs))
    eff_w(tab, as.numeric(k[as.character(tab$subject)])) }))
  q <- apply(boot, c(1, 2), stats::quantile, c(.025, .975))
  data.frame(group = g, time = rep(1:101, 2),
             component = rep(c("CV competition", "C competition"), each = 101),
             lo = c(q[1, , 1], q[1, , 2]), hi = c(q[2, , 1], q[2, , 2]))
}))
emp_long <- do.call(rbind, lapply(grp, function(g) {
  d <- lin2023_english[lin2023_english$group == g, ]
  data.frame(group = g, time = rep(d$time, 2),
             component = rep(c("CV competition", "C competition"), each = nrow(d)),
             y = c(d$cv_effect, d$c_effect))
}))
sim_r1 <- run_simulation(stim, delta_values = seq(0, 30, 5), residual_input = 1,
                         verbose = FALSE)
sim_f  <- run_simulation(stim, delta_values = seq(0, 30, 5), verbose = FALSE)
model_long <- do.call(rbind, lapply(list(list(sim_f, "Default model"),
                                         list(sim_r1, "residual_input = 1")), function(v) {
  fc <- fit_components(v[[1]], lin2023_english, lag = LAGS, verbose = FALSE)
  do.call(rbind, lapply(grp, function(g) {
    b <- fc[fc$group == g, ]; b <- b[which.min(b$aic), ]
    r <- v[[1]]$results[[as.character(b$delta)]]
    sh <- function(x) c(rep(0, b$lag), x)[1:101]
    data.frame(group = g, time = rep(1:101, 2),
               component = rep(c("CV competition", "C competition"), each = 101),
               y = c(b$scale * sh(r$large_effect), b$scale_c * sh(r$small_effect)),
               model = v[[2]], delta = b$delta)
  }))
}))
comp_cols <- c("CV competition" = "#0072B2", "C competition" = "#D55E00")
y_lim_cd <- range(c(bands$lo, bands$hi, model_long$y), na.rm = TRUE)   # same y-axis in C and D
bottom <- lapply(grp, function(g) {
  ggplot() +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_ribbon(data = bands[bands$group == g, ],
                aes(x = time, ymin = lo, ymax = hi, fill = component), alpha = 0.22) +
    geom_line(data = emp_long[emp_long$group == g, ],
              aes(x = time, y = y, color = component), linewidth = 1.3) +
    geom_line(data = model_long[model_long$group == g, ],
              aes(x = time, y = y, color = component, linetype = model),
              linewidth = 0.9) +
    scale_color_manual(values = comp_cols, name = NULL) +
    scale_fill_manual(values = comp_cols, guide = "none") +
    scale_linetype_manual(values = c("Default model" = "22",
                                     "residual_input = 1" = "dotted"),
                          name = NULL) +
    scale_x_continuous(breaks = seq(0, 100, 20), labels = paste0(seq(0, 100, 20), "%")) +
    scale_y_continuous(limits = y_lim_cd, breaks = seq(-0.05, 0.25, 0.05)) +  # shared axis
    labs(x = "Normalized Time (% of trial)",
         y = "Competition Effect\n(Control \u2212 Competitor x)") +
    theme_phonactivr(base_size = 14) +
    theme(legend.position = "bottom")
})
print(unique(model_long[, c("group", "model", "delta")]))   # deltas in Figure 6
fig8 <- (top[[1]] + top[[2]] + patchwork::plot_layout(guides = "collect") &
           theme(legend.position = "bottom")) /
        (bottom[[1]] + bottom[[2]] + patchwork::plot_layout(guides = "collect") &
           theme(legend.position = "bottom")) +
        patchwork::plot_annotation(tag_levels = "A")
ggsave("figures/Figure6_real_data_fit.png", fig8,
       width = 14, height = 11.5, dpi = 300)

# --- Numbers for Table 5 and Step 6 ------------------------------------------
gof_real <- goodness_of_fit(sim, lin2023_english, lag = LAGS)   # Table 5, left
sb <- shape_baseline(lin2023_english); print(sb)               # Step 6a reference
print(shape_baseline(lin2023_english, column = "cv_effect"))
print(shape_baseline(lin2023_english, column = "c_effect"))
fc_real <- fit_components(sim, lin2023_english, lag = LAGS)     # Table 5, right
fc_sep  <- fit_components(sim, lin2023_english, lag = LAGS, shared_scale = FALSE,
                          verbose = FALSE)
print(fc_sep[, c("group", "delta", "r2_c")])                   # C with its own scale
# R^2 within the competition window (bins where |empirical| > 10% of its peak)
for (g in grp) {
  d <- lin2023_english[lin2023_english$group == g, ]
  keep <- abs(d$asymmetry) > 0.1 * max(abs(d$asymmetry))
  for (dl in c(0, 10, 20, 30)) {
    r <- gof_real[gof_real$group == g & gof_real$delta == dl, ]
    m <- r$scale * c(rep(0, r$lag), sim$results[[as.character(dl)]]$asymmetry)[1:101]
    e <- d$asymmetry[keep]
    cat(g, "delta", dl, "window R2 =",
        round(1 - sum((e - m[keep])^2) / sum((e - mean(e))^2), 3),
        "(", sum(keep), "bins )\n")
  }
}
for (w in list(c(21, 101), c(40, 101), c(50, 101), c(1, 70), c(1, 80))) {  # windows
  print(attr(goodness_of_fit(sim, lin2023_english, lag = LAGS,
                             time_window = w, verbose = FALSE), "best_delta"))
}
for (lr in list(0:30, 0:40, 0:60, 10:70)) {                      # latency ranges
  print(attr(suppressWarnings(goodness_of_fit(sim, lin2023_english, lag = lr,
                                              verbose = FALSE)), "best_delta"))
}
g_fine <- goodness_of_fit(sim_f, lin2023_english, lag = LAGS, verbose = FALSE)
print(g_fine[, c("group", "delta", "r2")])                      # finer grid
# Step 4b: screening with the scale of the joint component fit and the
# between-participant SD of the bilinguals' asymmetry at its peak
# (0.156; lin2023_validation/validation.R, section E)
power_guidance(sim, n_items = 42, n_participants = 41, empirical_sd = 0.156,
               scale = round(mean(fc_real$scale[fc_real$group ==
                                                  unique(fc_real$group)[1]]), 2))

# =============================================================================
# Validation figures (Figures 7-9) and Figure S1
# =============================================================================

# --- Figure 7: Cohort / Rhyme / Unrelated Benchmark (Validation 2) -----------
# Same segment-timed input windows as cohort_rhyme_benchmark(): target
# /b iy k er/; cohort matches segments 1-2, rhyme segments 2-4, unrelated
# none (weak input over the whole word); seg_steps = 10.
ts <- 101L; SEG <- 10
seg_on <- function(k) 1 + (k - 1) * SEG
cohort_out <- run_activation(0.90, 0.1, overlap_onset = seg_on(1),
                             overlap_offset = seg_on(3), time_steps = ts)
rhyme_out  <- run_activation(0.50, 0.1, overlap_onset = seg_on(2),
                             overlap_offset = seg_on(5), time_steps = ts)
unrel_out  <- run_activation(0.15, 0.1, overlap_onset = seg_on(1),
                             overlap_offset = seg_on(5), time_steps = ts)
bench_df <- data.frame(
  time   = rep(1:ts, 3),
  effect = c(cohort_out$competition_effect, rhyme_out$competition_effect,
             unrel_out$competition_effect),
  type   = factor(rep(c("Cohort (matches segments 1-2)",
                        "Rhyme (matches segments 2-4)",
                        "Unrelated (no matching segment)"), each = ts),
                  levels = c("Cohort (matches segments 1-2)",
                             "Rhyme (matches segments 2-4)",
                             "Unrelated (no matching segment)")))
fig9 <- ggplot(bench_df, aes(x = time, y = effect, color = type,
                             linetype = type)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "grey45"), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = NULL) +
  scale_x_continuous(breaks = seq(0, 100, 20),
                     labels = paste0(seq(0, 100, 20), "%")) +
  labs(x = "Normalized Time (% of trial)",
       y = "Competition Effect\n(Competitor \u2212 Control)") +
  theme_phonactivr(base_size = 12.5) +
  theme(legend.position = "bottom")
ggsave("figures/Figure7_cohort_rhyme_benchmark.png", fig9,
       width = 10, height = 6.2, dpi = 300)
bench <- cohort_rhyme_benchmark()          # the five PASS/FAIL checks

# --- Figure 8: Distinguishability Across the Parameter Grid (Validation 4) --
# 81 combinations: alpha x decay x gamma (3 x 3 x 3) x seg_steps (8, 10, 12).
# For each combination and delta > 0: RMS difference from the delta = 0 curve
# and from the adjacent smaller delta. The figure shows the adjacent
# difference (the harder criterion) for every combination.
sens <- sensitivity_analysis(stim,
  alpha_values = c(0.04, 0.08, 0.12),
  decay_values = c(0.01, 0.02, 0.04),
  gamma_values = c(0.02, 0.04, 0.06),
  seg_values   = c(8, 10, 12),
  delta_values = c(0, 10, 20, 30))
print(sens)
sens_df <- sens$summary[sens$summary$delta > 0, ]
sens_df$pair  <- factor(paste0(c("0", "10", "20")[match(sens_df$delta, c(10, 20, 30))],
                               " vs ", sens_df$delta),
                        levels = c("0 vs 10", "10 vs 20", "20 vs 30"))
sens_df$gamma_lab <- factor(paste0("\u03b3 = ", sens_df$gamma))
sens_df$alpha_lab <- factor(paste0("\u03b1 = ", sens_df$alpha))
fig10 <- ggplot(sens_df, aes(x = pair, y = adjacent_divergence,
                             color = factor(seg_steps), shape = factor(decay))) +
  geom_hline(yintercept = sens$min_rms, linetype = "dashed", color = "grey40") +
  geom_point(size = 2.6, position = position_dodge(width = 0.6), stroke = 0.9) +
  facet_grid(gamma_lab ~ alpha_lab) +
  scale_color_manual(values = c("8" = "#000000", "10" = "#CC79A7", "12" = "#56B4E9"),
                     name = "seg_steps") +
  scale_shape_manual(values = c(16, 17, 15), name = "\u03bb (decay)") +
  labs(x = "Adjacent \u03b4 values compared",
       y = "RMS difference between curves") +
  theme_phonactivr(base_size = 13) +
  theme(legend.position = "right")
ggsave("figures/Figure8_sensitivity_grid.png", fig10,
       width = 13, height = 8.5, dpi = 300)

# --- Figure 9: Parameter Recovery (Validation 5) ----------------------------
# Data generated by re-running the model at a known delta, with temporally
# correlated noise at three levels; 100 replications per cell.
rec_grid <- expand.grid(true_delta = c(0, 10, 20, 30, 15, 25),
                        noise_sd = c(0.015, 0.03, 0.06))
rec_grid$rate <- NA_real_
rec_list <- list()
for (r in seq_len(nrow(rec_grid))) {
  rc <- parameter_recovery(sim, true_delta = rec_grid$true_delta[r],
                           noise_sd = rec_grid$noise_sd[r],
                           n_replications = 100, seed = 1234)
  rec_grid$rate[r] <- rc$recovery_rate
  rec_list[[r]] <- rc
}
print(rec_grid)
rec_grid$on_grid <- factor(ifelse(rec_grid$true_delta %in% c(0, 10, 20, 30),
                                  "On the grid: exact recovery",
                                  "Off the grid: bracket"),
                           levels = c("On the grid: exact recovery",
                                      "Off the grid: bracket"))
rec_grid$true_lab <- factor(paste0("\u03b4 = ", rec_grid$true_delta),
                            levels = paste0("\u03b4 = ", c(0, 10, 20, 30, 15, 25)))
fig11 <- ggplot(rec_grid, aes(x = true_lab, y = rate,
                              fill = factor(noise_sd))) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7,
           color = "grey25", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.0f", 100 * rate)),
            position = position_dodge(width = 0.75), vjust = -0.4, size = 3.4) +
  facet_grid(~ on_grid, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = c("0.015" = "#56B4E9", "0.03" = "#0072B2",
                               "0.06" = "#003A63"), name = "Noise SD") +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%"),
                     limits = c(0, 1.08), breaks = seq(0, 1, 0.25)) +
  labs(x = "Generating \u03b4", y = "Recovery rate (100 replications)") +
  theme_phonactivr(base_size = 12) +
  theme(legend.position = "right")
# Panel B: the recommended estimator (joint component fit) on the finer grid,
# across noise levels up to the level matched to the real data (SD .18
# before smoothing; Step 6b). With the standard error of a group mean
# falling as 1/sqrt(n), a noise level of .06 corresponds to about nine
# times the participants of Lin et al. (2023), Experiment 3.
sim_f5 <- run_simulation(stim, delta_values = seq(0, 30, 5), verbose = FALSE)
rec_j <- expand.grid(true_delta = c(0, 5, 10), noise_sd = c(0.03, 0.06, 0.09, 0.12, 0.18))
rec_j$rate <- NA_real_
for (r in seq_len(nrow(rec_j))) {
  rc <- parameter_recovery(sim_f5, true_delta = rec_j$true_delta[r],
                           noise_sd = rec_j$noise_sd[r], n_replications = 100,
                           method = "components", lag = 0:50, seed = 1234,
                           verbose = FALSE)
  rec_j$rate[r] <- rc$recovery_rate
}
cat("\nJoint-fit recovery (finer grid, latency 0-50) by noise level:\n"); print(rec_j)
rec_j$n_equiv <- round(41 * (0.18 / rec_j$noise_sd)^2)
cat("Participants per group implied by each noise level (41 at .18):\n")
print(unique(rec_j[, c("noise_sd", "n_equiv")]), row.names = FALSE)
rec_j$true_lab <- factor(paste0("\u03b4 = ", rec_j$true_delta), levels = paste0("\u03b4 = ", c(0, 5, 10)))
fig11b <- ggplot(rec_j, aes(x = factor(noise_sd), y = rate, group = true_lab,
                            color = true_lab, shape = true_lab)) +
  geom_hline(yintercept = 0.9, linetype = "dotted", color = "grey50") +
  geom_line(linewidth = 0.9) + geom_point(size = 3) +
  scale_color_manual(values = phonactivr_colors(3), name = "Generating \u03b4") +
  scale_shape_manual(values = c(16, 17, 15), name = "Generating \u03b4") +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%"), limits = c(0, 1.02)) +
  labs(x = "Noise SD before smoothing (.18 = Lin et al., 2023, n = 41)",
       y = "Exact recovery, joint fit (100 replications)") +
  theme_phonactivr(base_size = 12) + theme(legend.position = "right")
fig11 <- (fig11 + labs(tag = "A")) / (fig11b + labs(tag = "B"))
ggsave("figures/Figure9_parameter_recovery.png", fig11,
       width = 12, height = 10.5, dpi = 300)

# --- Figure S1: Absolute Fit to Real Data, 0.2.x vs 0.3.0 (Supplement S1.3) --
# Same data, same fitting procedure (scale + latency), two model versions:
# seg_steps = Inf (0.2.x dynamics: competitor input never ends) and the
# default seg_steps = 10 (disambiguation point).
fit_version <- function(ss, label) {
  s <- run_simulation(stim, delta_values = c(0, 10, 20, 30), seg_steps = ss,
                      verbose = FALSE)
  g <- goodness_of_fit(s, lin2023_english, lag = LAGS, verbose = FALSE)
  g$version <- label
  g
}
fitv <- rbind(fit_version(Inf, "Without disambiguation point (earlier versions)"),
              fit_version(10,  "With disambiguation point (current)"))
print(fitv[, c("version", "group", "delta", "lag", "r2")], row.names = FALSE)
fitv$version <- factor(fitv$version, levels = unique(fitv$version))
fig12 <- ggplot(fitv, aes(x = factor(delta), y = r2, fill = version)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_col(position = position_dodge(width = 0.75), width = 0.7,
           color = "grey25", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.2f", r2),
                vjust = ifelse(r2 >= 0, -0.4, 1.3)),
            position = position_dodge(width = 0.75), size = 3.4) +
  facet_wrap(~ group) +
  scale_fill_manual(values = c("grey70", "#0072B2"), name = NULL) +
  scale_y_continuous(limits = c(min(0, min(fitv$r2) - 0.1), 1.08),
                     breaks = seq(-1, 1, 0.25)) +
  labs(x = "\u03b4", y = expression(R^2 ~ "(variance of the empirical curve explained)")) +
  theme_phonactivr(base_size = 12) +
  theme(legend.position = "bottom")
ggsave("figures/FigureS1_real_data_model_versions.png", fig12,
       width = 12, height = 6, dpi = 300)

# =============================================================================
# Figure S2 and application figures (Figures 10-11)
# =============================================================================

# --- Figure S2: Is delta identified? (Supplement S2.7) ------------------------
# Fit quality (R^2) of every delta in a fine grid, for four segment
# durations and both groups, with the latency re-estimated for every model.
ident <- do.call(rbind, lapply(c(8, 10, 12, 14), function(ss) {
  s <- run_simulation(stim, delta_values = seq(0, 30, 5), seg_steps = ss,
                      verbose = FALSE)
  do.call(rbind, lapply(list(c(1, 101), c(21, 101)), function(w) {
    g <- goodness_of_fit(s, lin2023_english, lag = LAGS, time_window = w,
                         verbose = FALSE)
    g$seg_steps <- ss; g$window <- paste0(w[1], "-", w[2]); g
  }))
}))
best_ident <- do.call(rbind, lapply(split(ident, list(ident$group, ident$seg_steps,
                                                      ident$window)), function(x)
  x[which.min(x$aic), c("group", "seg_steps", "window", "delta", "lag", "r2")]))
print(best_ident[order(best_ident$group, best_ident$window, best_ident$seg_steps), ],
      row.names = FALSE)
id_full <- ident[ident$window == "1-101", ]
fig13 <- ggplot(id_full, aes(x = delta, y = r2, color = factor(seg_steps),
                             linetype = factor(seg_steps))) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_point(data = merge(id_full, best_ident[best_ident$window == "1-101", ]),
             shape = 21, size = 6, stroke = 1.6, fill = NA, color = "black") +
  facet_wrap(~ group) +
  scale_color_manual(values = c("8" = "#000000", "10" = "#CC79A7",
                                "12" = "#56B4E9", "14" = "#999999"),
                     name = "seg_steps") +
  scale_linetype_manual(values = c("8" = "solid", "10" = "22",
                                   "12" = "42", "14" = "1141"),
                        name = "seg_steps") +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  labs(x = "\u03b4", y = expression(R^2 ~ "(fit to the empirical curve)")) +
  theme_phonactivr(base_size = 14) +
  theme(legend.position = "bottom")
ggsave("figures/FigureS2_delta_identification.png", fig13,
       width = 10, height = 5.6, dpi = 300)

# --- Figure 10: Delay versus truncation (Application 1) ----------------------
# compare_languages() applied to two readings of the prosodic gate on the same
# items and delta grid: a buffered delay and a signal-locked truncation.
sim_trunc <- run_simulation(stim, delta_values = c(0, 10, 20, 30), gate = "truncate",
                            verbose = FALSE)
cross <- compare_languages(sim, sim_trunc,
  label_a = "Buffered delay (gate = \"delay\")",
  label_b = "Signal-locked truncation (gate = \"truncate\")")   # Table S3
fig14 <- cross$plot
ggsave("figures/Figure10_delay_vs_truncation.png", fig14,
       width = 16, height = 6.5, dpi = 300)

# --- Figure 11 and Table 7: Mandarin listening (Application 2) ---------------
# The same bilinguals' Mandarin-listening data (Experiment 1; lin2023_chinese),
# fitted jointly under both gate readings on the finer grid. Bootstrap bands
# as in Figure 6; the bootstrap distributions of delta are from validation.R
# (section H).
stim_zh <- example_stimuli_zh(); sm_zh <- mandarin_similarity()
simz <- list(delay = run_simulation(stim_zh, delta_values = seq(0, 30, 5),
                                    similarity_matrix = sm_zh, verbose = FALSE),
             truncate = run_simulation(stim_zh, delta_values = seq(0, 30, 5), gate = "truncate",
                                       similarity_matrix = sm_zh, verbose = FALSE))
sime <- list(delay = sim_f, truncate = run_simulation(stim, delta_values = seq(0, 30, 5),
                                                      gate = "truncate", verbose = FALSE))
ebil <- lin2023_english[lin2023_english$group == "Chinese-English bilinguals", ]
cat("\nTable 7: joint fit of both gate readings, same bilinguals, two languages\n")
t8 <- do.call(rbind, lapply(c("delay", "truncate"), function(g) rbind(
  { f <- fit_components(sime[[g]], ebil, lag = LAGS, verbose = FALSE); b <- f[which.min(f$aic), ]
    data.frame(language = "English (Exp. 3)", gate = g, delta = b$delta, r2 = b$r2, r2_cv = b$r2_cv, r2_c = b$r2_c, lag = b$lag) },
  { f <- fit_components(simz[[g]], lin2023_chinese, lag = LAGS, verbose = FALSE); b <- f[which.min(f$aic), ]
    data.frame(language = "Mandarin (Exp. 1)", gate = g, delta = b$delta, r2 = b$r2, r2_cv = b$r2_cv, r2_c = b$r2_c, lag = b$lag) })))
print(t8, digits = 3)
cat("Mandarin C/CV peak ratio:", round(max(lin2023_chinese$c_effect) / max(lin2023_chinese$cv_effect), 3),
    "| English bilinguals:", round(max(ebil$c_effect) / max(ebil$cv_effect), 3), "\n")
fz <- fit_components(simz$truncate, lin2023_chinese, lag = LAGS, verbose = FALSE)
cat("Mandarin truncation, delta_aic for delta 0 and 5:", fz$delta_aic[fz$delta %in% c(0, 5)], "\n")
fzd <- fit_components(simz$delay, lin2023_chinese, lag = LAGS, verbose = FALSE)
cat("Mandarin, best AIC delay minus truncation:", round(min(fzd$aic) - min(fz$aic), 2), "\n")
cat("Mandarin single-bump R2: asymmetry", round(shape_baseline(lin2023_chinese)$r2, 3),
    "CV", round(shape_baseline(lin2023_chinese, column = "cv_effect")$r2, 3),
    "C", round(shape_baseline(lin2023_chinese, column = "c_effect")$r2, 3), "\n")
scz <- utils::read.csv(system.file("extdata", "lin2023_validation",
                                   "lin2023_subject_curves_chinese.csv", package = "phonActivR"),
                       check.names = FALSE)
subs_z <- unique(scz$subject)
bootz <- withr::with_seed(2023, replicate(1000, {
  k <- table(factor(sample(subs_z, replace = TRUE), subs_z))
  eff_w(scz, as.numeric(k[as.character(scz$subject)])) }))
qz <- apply(bootz, c(1, 2), stats::quantile, c(.025, .975))
bands_z <- data.frame(time = rep(1:101, 2),
                      component = rep(c("CV competition", "C competition"), each = 101),
                      lo = c(qz[1, , 1], qz[1, , 2]), hi = c(qz[2, , 1], qz[2, , 2]))
emp_z <- data.frame(time = rep(1:101, 2),
                    component = rep(c("CV competition", "C competition"), each = 101),
                    y = c(lin2023_chinese$cv_effect, lin2023_chinese$c_effect))
model_z <- do.call(rbind, lapply(c("truncate", "delay"), function(g) {
  f <- fit_components(simz[[g]], lin2023_chinese, lag = LAGS, verbose = FALSE)
  b <- f[which.min(f$aic), ]; r <- simz[[g]]$results[[as.character(b$delta)]]
  sh <- function(x) c(rep(0, b$lag), x)[1:101]
  data.frame(time = rep(1:101, 2),
             component = rep(c("CV competition", "C competition"), each = 101),
             y = c(b$scale * sh(r$large_effect), b$scale_c * sh(r$small_effect)),
             model = paste0(if (g == "truncate") "Truncation" else "Delay", " (\u03b4 = ", b$delta, ")"))
}))
fig15 <- ggplot() +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_ribbon(data = bands_z, aes(x = time, ymin = lo, ymax = hi, fill = component), alpha = 0.22) +
  geom_line(data = emp_z, aes(x = time, y = y, color = component), linewidth = 1.3) +
  geom_line(data = model_z, aes(x = time, y = y, color = component, linetype = model), linewidth = 0.9) +
  scale_color_manual(values = comp_cols, name = NULL) +
  scale_fill_manual(values = comp_cols, guide = "none") +
  scale_linetype_manual(values = stats::setNames(c("22", "dotted"), unique(model_z$model)), name = NULL) +
  scale_x_continuous(breaks = seq(0, 100, 20), labels = paste0(seq(0, 100, 20), "%")) +
  labs(x = "Normalized Time (% of trial)", y = "Competition Effect\n(Control \u2212 Competitor x)") +
  theme_phonactivr(base_size = 12.5) + theme(legend.position = "bottom")
ggsave("figures/Figure11_mandarin_fit.png", fig15, width = 8, height = 5.2, dpi = 300)

# --- Figure S3: raw trajectories by condition (Supplemental Material S5) ------
# Mean horizontal position on competitor and control trials, plotted
# separately rather than as a difference, for the same 41 participants in
# Mandarin (Experiment 1) and English (Experiment 3). The Mandarin control
# shares the target's rime; the English C control shares nothing.
sce <- utils::read.csv(system.file("extdata", "lin2023_validation",
                                   "lin2023_subject_curves.csv", package = "phonActivR"),
                       check.names = FALSE)
sce <- sce[sce$group == "Chinese-English bilinguals", ]
raw_cond <- function(tab, lang) do.call(rbind, lapply(unique(tab$condition), function(k) {
  s <- tab$condition == k
  data.frame(language = lang, condition = k, time = 1:101,
             x = colSums(as.matrix(tab[s, paste0("X_", 1:101)]) * tab$n_trials[s]) / sum(tab$n_trials[s])) }))
raw_s1 <- rbind(raw_cond(scz, "Mandarin (Experiment 1)"), raw_cond(sce, "English (Experiment 3)"))
raw_s1$condition <- factor(raw_s1$condition, levels = c("CV", "CV Control", "Onset", "Onset Control"),
                           labels = c("CV competitor", "CV control", "C competitor", "C control"))
raw_s1$language <- factor(raw_s1$language, levels = c("Mandarin (Experiment 1)", "English (Experiment 3)"))
figS1 <- ggplot(raw_s1[raw_s1$time >= 30 & raw_s1$time <= 85, ],
                aes(x = time - 1, y = x, color = condition, linetype = condition)) +
  geom_line(linewidth = 0.9) + facet_wrap(~ language) +
  scale_color_manual(values = c("#0072B2", "#0072B2", "#D55E00", "#D55E00"), name = NULL) +
  scale_linetype_manual(values = c("solid", "22", "solid", "22"), name = NULL) +
  scale_x_continuous(labels = function(v) paste0(v, "%")) +
  labs(x = "Normalized Time (% of trial)", y = "Mean horizontal position") +
  theme_phonactivr(base_size = 12.5) + theme(legend.position = "bottom")
ggsave("figures/FigureS3_raw_trajectories.png", figS1, width = 10, height = 5.2, dpi = 300)
for (st in c(50, 56, 62)) { rr <- function(tab, k) { r <- raw_cond(tab, ""); r$x[r$condition == k & r$time == st] }
  cat("Figure S3, step", st, ": C control minus CV control: Mandarin", round(rr(scz, "Onset Control") - rr(scz, "CV Control"), 4),
      "| English", round(rr(sce, "Onset Control") - rr(sce, "CV Control"), 4), "\n") }

# --- Separate networks for competitor and control (Package Architecture) -----
# The competitor and its control share one network with the target. Here each
# is simulated in its own network with the target, for every item at
# delta = 0, and the change in the competition effect is expressed relative
# to the peak effect.
{
  it <- stim$items; on <- stim$onsets
  dev <- sapply(seq_len(nrow(it)), function(i) {
    sapply(list(c("large_comp", "large_ctrl", "CV", 2),
                c("small_comp", "small_ctrl", "C", 1)), function(k) {
      oc <- compute_overlap(on[[it$target[i]]], on[[it[[k[1]]][i]]], k[3])
      ol <- compute_overlap(on[[it$target[i]]], on[[it[[k[2]]][i]]], k[3]) * 0.15
      off <- 1 + as.numeric(k[4]) * 10
      joint <- run_activation(oc, ol, overlap_onset = 1,
                              overlap_offset = off)$competition_effect
      a <- run_activation(oc, 0, overlap_onset = 1, overlap_offset = off)$act_comp
      b <- run_activation(0, ol, overlap_onset = 1, overlap_offset = off)$act_ctrl
      max(abs(joint - (a - b))) / max(joint) }) })
  cat(sprintf("Separate networks: max change = %.1f%% (CV), %.1f%% (C) of the peak effect\n",
              100 * max(dev[1, ]), 100 * max(dev[2, ])))
}

# --- Done ---------------------------------------------------------------------
message("\n=== All 11 figures and Figures S1-S3 generated ===")
for (f in sort(list.files("figures"))) message("  ", f)
sessionInfo()
writeLines(utils::capture.output(sessionInfo()), "sessionInfo.txt")
