# =============================================================================
# phonActivR: Figure Generation Script (revision, package v0.2.0)
# =============================================================================
# Generates ALL 16 figures for the BRM tutorial paper. Running this single
# script from top to bottom reproduces every figure exactly (set.seed calls
# are fixed; the simulation itself is deterministic).
#
# Figures 1-4 are diagram/format figures drawn with base grid graphics;
# Figures 5-16 are generated from package simulation output with ggplot2.
# No figure bakes a title or subtitle into the image: per APA style, figure
# titles and notes live in the manuscript captions. Panel identifiers
# (e.g., "delta = 0", group labels) are drawn because they identify panels.
#
# FIGURE MAP (revised manuscript numbering):
#   Figure 1  - Workflow pipeline diagram               [grid, below]
#   Figure 2  - Model architecture (one simulation)     [grid, below]
#   Figure 3  - overlay_empirical() data format         [grid, below]
#   Figure 4  - create_stimuli() data format            [grid, below]
#   Figure 5  - Competition effect curves               [simulation]
#   Figure 6  - Asymmetry gradient                      [simulation]
#   Figure 7  - Onset timing function                   [simulation]
#   Figure 8  - Mouse-tracking empirical overlay        [simulation]
#   Figure 9  - Cohort/rhyme/unrelated benchmark        [simulation]
#   Figure 10 - Multi-parameter sensitivity grid        [simulation]
#   Figure 11 - Parameter recovery                      [simulation]
#   Figure 12 - Goodness-of-fit AIC comparison          [simulation]
#   Figure 13 - Application 1: bundled GAMM overlay     [simulation]
#   Figure 14 - Eye-tracking empirical overlay          [simulation]
#   Figure 15 - Sensitivity to lateral inhibition       [simulation]
#   Figure 16 - Cross-linguistic comparison             [simulation]
#
# HOW TO RUN:
#   1. Install the package:
#      devtools::install_github("yclinpsy/phonActivR", build_vignettes = TRUE)
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
# green, 10 blue, 20 orange, 30 vermilion red). Figure 16's right panel is
# simulated over a different grid (0/5/10/15/20) and therefore maps its own
# ordered delta values, as its caption states. This is the package-wide
# colorblind-safe Okabe-Ito palette; see ?phonactivr_colors.
delta_pal <- stats::setNames(phonactivr_colors(4), c("0", "10", "20", "30"))

# =============================================================================
# Figure 1: Workflow pipeline diagram (grid graphics)
# =============================================================================
# Six stages, numbered; the functions implementing each stage are shown in a
# white monospaced box; the S3 objects passed between stages are labeled
# beneath the connecting arrows.
{
  png("figures/Figure1_workflow_pipeline.png", width = 16, height = 4.8,
      units = "in", res = 300)
  grid.newpage()

  stages <- list(
    list(num = "1", title = "Experimental\nStimuli",
         sub = "targets, competitors\ncontrols, onsets",
         fns = "create_stimuli()"),
    list(num = "2", title = "Phonological\nFeatures",
         sub = "binary 7-dimension\nfeature matrix",
         fns = "trace_features()\ncompute_overlap()"),
    list(num = "3", title = "Competition\nSimulation",
         sub = "δ values, α, γ, λ\nall items",
         fns = "run_simulation()"),
    list(num = "4", title = "Competition\nTime Course",
         sub = "CV vs C curves\nasymmetry gradient",
         fns = "plot_competition()\nplot_asymmetry()"),
    list(num = "5", title = "Empirical\nData Overlay",
         sub = "mouse-tracking\neye-tracking / ERP",
         fns = "overlay_empirical()"),
    list(num = "6", title = "Hypothesis\nEvaluation",
         sub = "δ curve fitting\npre-registration",
         fns = "goodness_of_fit()\npower_guidance()")
  )
  n     <- length(stages)
  box_w <- 0.125                      # box width (npc)
  gap   <- (1 - n * box_w - 0.02) / (n - 1)
  x0    <- 0.01
  y_mid <- 0.52
  box_h <- 0.78
  greys <- grey(seq(0.97, 0.84, length.out = n))  # subtle L-to-R deepening

  # Pass 1: all boxes (so that arrow/object labels drawn in pass 2 are
  # never covered by a later box).
  for (i in seq_len(n)) {
    s  <- stages[[i]]
    xl <- x0 + (i - 1) * (box_w + gap)
    xc <- xl + box_w / 2
    grid.roundrect(x = xc, y = y_mid, width = box_w, height = box_h,
                   r = unit(2.5, "mm"),
                   gp = gpar(fill = greys[i], col = "grey25", lwd = 2.2))
    # number badge
    grid.circle(x = xl + 0.016, y = y_mid + box_h/2 - 0.075, r = 0.026,
                gp = gpar(fill = "grey15", col = "white", lwd = 2))
    grid.text(s$num, x = xl + 0.016, y = y_mid + box_h/2 - 0.075,
              gp = gpar(col = "white", fontface = "bold", fontsize = 13))
    # stage title
    grid.text(s$title, x = xc, y = y_mid + 0.185,
              gp = gpar(fontface = "bold", fontsize = 13.5))
    # separator
    grid.segments(x0 = xc - box_w * 0.36, x1 = xc + box_w * 0.36,
                  y0 = y_mid + 0.055, y1 = y_mid + 0.055,
                  gp = gpar(col = "grey55", lwd = 1))
    # descriptive sublabel
    grid.text(s$sub, x = xc, y = y_mid - 0.04,
              gp = gpar(fontsize = 10.2, col = "grey25"))
    # white function box (monospace)
    grid.roundrect(x = xc, y = y_mid - 0.245, width = box_w * 0.92,
                   height = 0.19, r = unit(1.5, "mm"),
                   gp = gpar(fill = "white", col = "grey45", lwd = 1.2))
    grid.text(s$fns, x = xc, y = y_mid - 0.245,
              gp = gpar(fontfamily = "mono", fontsize = 9.0))
  }
  # Pass 2: arrows between stages, with the S3 object passed along the
  # pipeline labeled beneath the two arrows where a new object is created.
  for (i in seq_len(n - 1)) {
    xl  <- x0 + (i - 1) * (box_w + gap)
    ax0 <- xl + box_w + 0.004
    ax1 <- ax0 + gap - 0.008
    grid.segments(x0 = ax0, x1 = ax1, y0 = y_mid, y1 = y_mid,
                  arrow = arrow(length = unit(3.2, "mm"), type = "closed"),
                  gp = gpar(col = "grey20", lwd = 2.6, fill = "grey20"))
    s3lab <- if (i == 1) "phonActivR_stimuli" else
             if (i == 3) "phonActivR_sim"     else NULL
    if (!is.null(s3lab)) {
      # Rotated so the class name fits inside the narrow gap between boxes.
      grid.text(s3lab, x = (ax0 + ax1)/2, y = y_mid + 0.26, rot = 90,
                gp = gpar(fontfamily = "mono", fontsize = 8.2,
                          col = "grey30", fontface = "italic"))
    }
  }
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
  png("figures/Figure2_model_architecture.png", width = 13, height = 6.9,
      units = "in", res = 300)
  grid.newpage()
  pushViewport(viewport(xscale = c(0, 13), yscale = c(0, 6.9)))

  unit_box <- function(x, y, label, sub, fill) {
    grid.roundrect(x = unit(x, "native"), y = unit(y, "native"),
                   width = unit(2.1, "native"), height = unit(1.04, "native"),
                   r = unit(2, "mm"),
                   gp = gpar(fill = fill, col = "black", lwd = 2))
    grid.text(label, x = unit(x, "native"), y = unit(y + 0.14, "native"),
              gp = gpar(fontface = "bold", fontsize = 14))
    grid.text(sub, x = unit(x, "native"), y = unit(y - 0.25, "native"),
              gp = gpar(fontsize = 10, fontface = "italic", col = "#444444"))
  }
  nat_arrow <- function(x0, y0, x1, y1, col, lwd = 2.4, lty = "solid",
                        both = FALSE) {
    grid.segments(x0 = unit(x0, "native"), y0 = unit(y0, "native"),
                  x1 = unit(x1, "native"), y1 = unit(y1, "native"),
                  arrow = arrow(length = unit(3, "mm"), type = "closed",
                                ends = if (both) "both" else "last"),
                  gp = gpar(col = col, fill = col, lwd = lwd, lty = lty))
  }

  # Units
  unit_box(2.0, 4.6, "Target",     "input = 1.0",               "#dbe8f6")
  unit_box(9.0, 4.6, "Competitor", "input = overlap (gated)",   "#f6e3db")
  unit_box(5.5, 2.15, "Control",   "input = overlap × 0.15", "#e8e8e8")

  # Excitatory input arrows (green)
  nat_arrow(2.0, 5.95, 2.0, 5.25, "#1a6b32")
  grid.text("excitatory input\nα · input · (1 − a)",
            x = unit(2.0, "native"), y = unit(6.15, "native"),
            just = "bottom", gp = gpar(fontsize = 10, col = "#1a6b32"))
  nat_arrow(5.5, 0.95, 5.5, 1.55, "#1a6b32")
  grid.text("excitatory input (never gated)",
            x = unit(6.75, "native"), y = unit(1.15, "native"),
            just = "left", gp = gpar(fontsize = 10, col = "#1a6b32"))

  # Delta gate on the competitor's input
  grid.roundrect(x = unit(9.0, "native"), y = unit(5.85, "native"),
                 width = unit(1.15, "native"), height = unit(0.5, "native"),
                 r = unit(1.5, "mm"),
                 gp = gpar(fill = "#fff2cc", col = "#b8860b", lwd = 2))
  grid.text("δ gate", x = unit(9.0, "native"), y = unit(5.85, "native"),
            gp = gpar(fontface = "bold", fontsize = 11, col = "#7a5a00"))
  nat_arrow(9.0, 5.6, 9.0, 5.25, "#1a6b32")
  grid.text(paste0("closed for first δ cycles\n",
                   "(sub-prosodic competitors);\n",
                   "always open (prosodically\naligned competitors)"),
            x = unit(9.75, "native"), y = unit(5.85, "native"),
            just = "left", gp = gpar(fontsize = 9, col = "#7a5a00"))

  # Lateral inhibition (red dashed, double-headed)
  nat_arrow(3.10, 4.6, 7.90, 4.6, "#a02020", lwd = 2, lty = "42", both = TRUE)
  nat_arrow(2.60, 4.00, 4.70, 2.60, "#a02020", lwd = 2, lty = "42", both = TRUE)
  nat_arrow(8.40, 4.00, 6.30, 2.60, "#a02020", lwd = 2, lty = "42", both = TRUE)
  grid.text("lateral inhibition  −γ · Σ a",
            x = unit(5.5, "native"), y = unit(4.85, "native"),
            gp = gpar(fontsize = 10.5, col = "#a02020"))

  # Passive decay labels (one per unit, matching the caption's "applied to
  # every unit")
  grid.text("−λ · a\n(decay)", x = unit(0.55, "native"),
            y = unit(4.6, "native"), gp = gpar(fontsize = 9, col = "#555555"))
  grid.text("−λ · a\n(decay)", x = unit(10.55, "native"),
            y = unit(3.9, "native"), gp = gpar(fontsize = 9, col = "#555555"))
  grid.text("−λ · a\n(decay)", x = unit(3.7, "native"),
            y = unit(1.55, "native"), gp = gpar(fontsize = 9, col = "#555555"))

  # Output note
  grid.roundrect(x = unit(6.5, "native"), y = unit(0.35, "native"),
                 width = unit(11.6, "native"), height = unit(0.75, "native"),
                 r = unit(2, "mm"),
                 gp = gpar(fill = "#f7f7f7", col = "#999999", lwd = 1.4))
  grid.text(paste0("Output: competition effect = competitor − control ",
                   "activation, over 101 normalized time steps.\n",
                   "CV and C competitor types are simulated as separate runs ",
                   "(separate trial types); activation bounded in [0, 1]."),
            x = unit(6.5, "native"), y = unit(0.35, "native"),
            gp = gpar(fontsize = 10.5))
  popViewport()
  dev.off()
}

# =============================================================================
# Helper for the two data-format figures (grid graphics)
# =============================================================================
draw_format_table <- function(file, headers, rows, width_in, height_in,
                              col_widths = NULL, footnote = NULL,
                              brackets = NULL, table_right = 0.97) {
  png(file, width = width_in, height = height_in, units = "in", res = 300)
  grid.newpage()
  nr <- length(rows) + 1                 # + header row
  top <- 0.96; bottom <- if (is.null(footnote)) 0.04 else 0.14
  row_h <- (top - bottom) / nr
  left <- 0.02; right <- table_right
  if (is.null(col_widths)) col_widths <- rep(1, length(headers))
  col_widths <- col_widths / sum(col_widths) * (right - left)
  col_x <- left + cumsum(c(0, col_widths))

  for (r in seq_len(nr)) {
    y_top <- top - (r - 1) * row_h
    y_c   <- y_top - row_h / 2
    cells <- if (r == 1) headers else rows[[r - 1]]
    is_ell <- !is.null(cells) && all(cells == "...")
    fill <- if (r == 1) "grey88" else if (r %% 2 == 0) "white" else "grey96"
    grid.rect(x = (left + right)/2, y = y_c, width = right - left,
              height = row_h, gp = gpar(fill = fill, col = "grey75",
                                        lwd = 0.7))
    for (cidx in seq_along(cells)) {
      grid.text(cells[cidx],
                x = (col_x[cidx] + col_x[cidx + 1]) / 2, y = y_c,
                gp = gpar(fontfamily = "mono",
                          fontsize = if (r == 1) 13 else 12,
                          fontface = if (r == 1) "bold" else "plain",
                          col = if (is_ell) "grey55" else "black"))
    }
  }
  # column separators + outer border (heavier)
  for (cx in col_x) grid.segments(cx, bottom, cx, top,
                                  gp = gpar(col = "grey60", lwd = 0.8))
  grid.rect(x = (left + right)/2, y = (top + bottom)/2,
            width = right - left, height = top - bottom,
            gp = gpar(fill = NA, col = "black", lwd = 2.2))
  grid.segments(left, top - row_h, right, top - row_h,
                gp = gpar(col = "black", lwd = 1.8))
  # optional right-hand row-range brackets ("Group 1 (101 rows)" etc.)
  if (!is.null(brackets)) {
    for (b in brackets) {
      y1 <- top - b$from_row * row_h        # top edge of first data row
      y2 <- top - (b$to_row + 1) * row_h    # bottom edge of last data row
      bx <- right + 0.008
      grid.segments(bx, y1, bx + 0.008, y1, gp = gpar(col = "grey40", lwd = 1.6))
      grid.segments(bx + 0.008, y1, bx + 0.008, y2, gp = gpar(col = "grey40", lwd = 1.6))
      grid.segments(bx, y2, bx + 0.008, y2, gp = gpar(col = "grey40", lwd = 1.6))
      grid.text(b$label, x = bx + 0.016, y = (y1 + y2)/2, just = "left",
                gp = gpar(fontsize = 11, fontface = "italic", col = "grey30"))
    }
  }
  if (!is.null(footnote)) {
    grid.text(footnote, x = (left + right)/2, y = bottom/2 + 0.02,
              gp = gpar(fontsize = 11, fontface = "italic", col = "grey30"))
  }
  dev.off()
}

# =============================================================================
# Figure 3: overlay_empirical() data format
# =============================================================================
draw_format_table(
  file    = "figures/Figure3_overlay_format.png",
  headers = c("time", "asymmetry", "se", "group"),
  rows = list(
    c("1",   "0.004", "0.002", "JP bilinguals"),
    c("2",   "0.009", "0.003", "JP bilinguals"),
    c("3",   "0.015", "0.003", "JP bilinguals"),
    c("...", "...",   "...",   "..."),
    c("101", "0.041", "0.008", "JP bilinguals"),
    c("1",   "0.001", "0.002", "EN monolinguals"),
    c("2",   "0.003", "0.002", "EN monolinguals"),
    c("...", "...",   "...",   "..."),
    c("101", "0.012", "0.005", "EN monolinguals")
  ),
  width_in = 9.6, height_in = 6.2,
  col_widths = c(0.9, 1.3, 0.9, 1.9),
  table_right = 0.80,
  brackets = list(
    list(from_row = 1, to_row = 5, label = "Group 1\n(101 rows)"),
    list(from_row = 6, to_row = 9, label = "Group 2\n(101 rows)")
  )
)

# =============================================================================
# Figure 4: create_stimuli() data format
# =============================================================================
draw_format_table(
  file    = "figures/Figure4_stimuli_format.png",
  headers = c("item", "target", "large_comp", "large_ctrl", "small_comp",
              "small_ctrl", "target_onset", "large_comp_onset"),
  rows = list(
    c("1",   "bench",  "bell", "cell", "bark", "dark", "b eh", "b eh"),
    c("2",   "bitter", "bill", "hill", "bank", "tank", "b ih", "b ih"),
    c("3",   "bottle", "box",  "fox",  "bat",  "rat",  "b ao", "b ao"),
    c("...", "...",    "...",  "...",  "...",  "...",  "...",  "..."),
    c("42",  "wish",   "wit",  "bit",  "wax",  "tax",  "w ih", "w ih")
  ),
  width_in = 12.6, height_in = 4.7,
  col_widths = c(0.65, 0.95, 1.25, 1.25, 1.25, 1.25, 1.4, 1.75),
  footnote = paste0("(+ large_ctrl_onset, small_comp_onset, ",
                    "small_ctrl_onset columns, same format)")
)

# =============================================================================
# Base simulation used by most figures
# =============================================================================
# 42-item Japanese-English stimulus set; delta grid 0/10/20/30; default
# activation parameters (alpha = .08, gamma = .04, decay = .02).
set.seed(1234)                     # fixed seed: full reproducibility
stim <- example_stimuli_jp()       # built-in validated stimulus set
sim  <- run_simulation(stim, delta_values = c(0, 10, 20, 30), seed = 1234)
summary(sim)                       # prints the Table-4 summary used in the paper

# --- Figure 5: Competition Effects -------------------------------------------
# Two panels: delta = 0 (universalist) vs delta = 30 (strong constraint).
# No overall title: the per-panel delta labels identify the panels and the
# manuscript caption carries the title.
fig5 <- plot_competition(sim)
ggsave("figures/Figure5_competition_effects.png", fig5,
       width = 14, height = 6.5, dpi = 300)

# --- Figure 6: Asymmetry Gradient ---------------------------------------------
# The primary hypothesis-adjudication figure: CV-C asymmetry for all deltas.
fig6 <- plot_asymmetry(sim)
ggsave("figures/Figure6_asymmetry_gradient.png", fig6,
       width = 12, height = 6.5, dpi = 300)

# --- Figure 7: Onset Timing ---------------------------------------------------
# Predicted C-competition onset as a function of delta, annotated in ms
# (assuming a 1,500 ms mouse-tracking trial). The series legend is drawn
# by plot_onset_timing() itself.
fig7 <- plot_onset_timing(sim, mean_trial_ms = 1500)
ggsave("figures/Figure7_onset_timing.png", fig7,
       width = 11, height = 6.8, dpi = 300)

# --- Figure 8: Mouse-Tracking Overlay (Tutorial Step 6) ----------------------
# Synthetic two-group data at generating delta = 15; fit = TRUE (default)
# prints the AIC-selected best delta per group to the console and attaches
# the full table as attr(plot, "fit").
emp_mt <- example_empirical(sim, true_delta = 15,
  group_labels = c("JP bilinguals", "EN monolinguals"),
  paradigm = "mouse-tracking", seed = 42)
fig8 <- overlay_empirical(sim, emp_mt,
  group_colors = c("JP bilinguals" = "black", "EN monolinguals" = "grey50"))
ggsave("figures/Figure8_overlay_mouse_tracking.png", fig8,
       width = 12, height = 6.5, dpi = 300)

# =============================================================================
# Validation figures (Figures 9-12)
# =============================================================================

# --- Figure 9: Cohort / Rhyme / Unrelated Benchmark (Validation 1) -----------
# Reconstructs the three benchmark competitor types with the same overlap
# values used by cohort_rhyme_benchmark() and plots their competition curves.
ts <- 101L
cohort_out <- run_activation(0.90, 0.1, overlap_onset = 1L,  time_steps = ts)
rhyme_out  <- run_activation(0.50, 0.1, overlap_onset = 30L, time_steps = ts)
unrel_out  <- run_activation(0.15, 0.1, overlap_onset = 1L,  time_steps = ts)

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

fig9 <- ggplot(bench_df, aes(x = time, y = effect,
                             color = type, linetype = type)) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "grey45"), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = NULL) +
  scale_x_continuous(breaks = seq(0, 100, 20),
                     labels = paste0(seq(0, 100, 20), "%")) +
  labs(
    x = "Normalized Time (% of trial)",
    y = "Competition Effect\n(Competitor − Control)"
  ) +
  theme_phonactivr(base_size = 12.5) +
  theme(legend.position = "bottom")
ggsave("figures/Figure9_cohort_rhyme_benchmark.png", fig9,
       width = 10, height = 6.2, dpi = 300)

# Also run the formal 5-check benchmark and print the PASS/FAIL table
bench <- cohort_rhyme_benchmark()

# --- Figure 10: Multi-Parameter Sensitivity Grid (Validation 3) ---------------
# 27-combination factorial grid over alpha, decay, gamma; the figure shows
# the alpha x decay panels at gamma = .04 (the full grid is tested below).
# Bars use the same shared delta palette as the curve figures.
sens <- sensitivity_analysis(stim,
  alpha_values = c(0.04, 0.08, 0.12),
  decay_values = c(0.01, 0.02, 0.04),
  gamma_values = c(0.04),
  delta_values = c(0, 10, 20, 30),
  verbose = FALSE)

sens_df <- sens$summary
sens_df$param_label <- paste0("α=", sens_df$alpha,
                              ", λ=", sens_df$decay)
sens_df$delta <- factor(sens_df$delta)

fig10 <- ggplot(sens_df, aes(x = delta, y = peak_asym, fill = delta)) +
  geom_col(position = "dodge", width = 0.7, color = "grey30",
           linewidth = 0.3) +
  facet_wrap(~param_label, scales = "free_y") +
  scale_fill_manual(values = delta_pal, name = "δ") +
  labs(
    x = "δ (Prosodic Constraint)",
    y = "Peak CV–C Asymmetry"
  ) +
  theme_phonactivr(base_size = 11.5) +
  theme(legend.position = "right")
ggsave("figures/Figure10_sensitivity_grid.png", fig10,
       width = 14, height = 8, dpi = 300)

# The full 27-combination grid reported in the text:
sens_full <- sensitivity_analysis(stim,
  alpha_values = c(0.04, 0.08, 0.12),
  decay_values = c(0.01, 0.02, 0.04),
  gamma_values = c(0.02, 0.04, 0.06),
  delta_values = c(0, 10, 20, 30),
  verbose = FALSE)
cat("Full 27-combination grid: robust (non-decreasing ordering) =",
    sens_full$robust, "\n")

# --- Figure 11: Parameter Recovery (Validation 4) ----------------------------
# 100 synthetic datasets at true delta = 15 (between grid values 10 and 20);
# the recovery procedure selects the RSS-minimizing simulated curve.
# All four grid values are shown on the x-axis (delta = 0 and 30 at zero
# counts make "never selected" visible); the dashed line marks the true
# generating value; per-bar labels give counts and percentages.
set.seed(1234)
rec <- parameter_recovery(sim, true_delta = 15, n_replications = 100,
                          noise_sd = 0.01)

grid_deltas <- sim$params$delta_values
rec_tab <- table(factor(rec$best_deltas, levels = grid_deltas))
rec_df  <- data.frame(delta = factor(grid_deltas, levels = grid_deltas),
                      count = as.integer(rec_tab))
rec_df$pct_label <- ifelse(
  rec_df$count > 0,
  paste0(rec_df$count, " (", round(100 * rec_df$count /
                                     sum(rec_df$count)), "%)"),
  "0")

# x-position of true delta = 15 on the discrete axis: halfway between the
# 2nd (delta = 10) and 3rd (delta = 20) category positions.
true_x <- 2.5

fig11 <- ggplot(rec_df, aes(x = delta, y = count, fill = delta)) +
  geom_col(width = 0.62, color = "grey25", linewidth = 0.4) +
  geom_vline(xintercept = true_x, linetype = "dashed",
             color = "grey35", linewidth = 0.8) +
  annotate("text", x = true_x + 0.07, y = max(rec_df$count) * 1.06,
           label = "true δ = 15", color = "grey25", size = 4.1,
           fontface = "italic", hjust = 0) +
  geom_text(aes(label = pct_label), vjust = -0.55, size = 4.3,
            fontface = "bold", color = "grey15") +
  scale_fill_manual(values = delta_pal, guide = "none") +
  scale_y_continuous(limits = c(0, max(rec_df$count) * 1.14),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    x = "Best-fitting δ value (simulation grid)",
    y = "Number of replications (of 100)"
  ) +
  theme_phonactivr(base_size = 13)
ggsave("figures/Figure11_parameter_recovery.png", fig11,
       width = 10, height = 6, dpi = 300)

# --- Figure 12: Goodness-of-Fit AIC (Validation 5) ---------------------------
# Formal model selection on a synthetic single-group curve (true delta = 15).
# Delta-AIC values are labeled above every bar; the best model necessarily
# has delta-AIC = 0 (no bar), so it is marked with a filled diamond at zero
# and an explicit label rather than an invisible zero-height bar.
emp_gof <- example_empirical(sim, true_delta = 15, noise_sd = 0.01, seed = 42)
exp_grp <- emp_gof[emp_gof$group == unique(emp_gof$group)[1], ]
gof <- goodness_of_fit(sim, exp_grp)

gof_df <- data.frame(delta = factor(gof$delta, levels = gof$delta),
                     delta_aic = gof$delta_aic)
gof_df$is_best <- gof_df$delta_aic == 0
best_delta <- gof$delta[which.min(gof$aic)]

fig12 <- ggplot(gof_df, aes(x = delta, y = delta_aic, fill = delta)) +
  geom_col(width = 0.58, color = "grey25", linewidth = 0.4) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "gray45") +
  annotate("text", x = nrow(gof_df) + 0.38, y = 13,
           label = "ΔAIC = 10\n(no-support\nthreshold)",
           hjust = 0, vjust = 0, size = 3.3, color = "gray35") +
  geom_text(data = gof_df[!gof_df$is_best, ],
            aes(label = sprintf("%.1f", delta_aic)),
            vjust = -0.5, size = 4.2, fontface = "bold", color = "grey15") +
  geom_point(data = gof_df[gof_df$is_best, ], shape = 23, size = 4.6,
             fill = "#0072B2", color = "grey15", stroke = 0.7) +
  geom_text(data = gof_df[gof_df$is_best, ],
            aes(y = 27, label = paste0("best model\n(ΔAIC = 0)")),
            vjust = 0, size = 4.0, fontface = "bold", color = "grey15") +
  scale_fill_manual(values = delta_pal, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  coord_cartesian(clip = "off") +
  labs(
    x = "δ (Prosodic Constraint Model)",
    y = "ΔAIC (relative to best model)"
  ) +
  theme_phonactivr(base_size = 13) +
  theme(plot.margin = margin(8, 70, 8, 8))
ggsave("figures/Figure12_goodness_of_fit.png", fig12,
       width = 9.5, height = 6, dpi = 300)

# =============================================================================
# Application figures (Figures 13-16)
# =============================================================================

# --- Figure 13: Application 1 - bundled GAMM overlay -------------------------
# Fully runnable real-data workflow: the bundled example_gamm dataset has the
# exact structure of exported GAMM smooth estimates (see ?example_gamm).
data(example_gamm)
emp_real <- data.frame(
  time      = example_gamm$time,
  asymmetry = example_gamm$CV_effect - example_gamm$C_effect,
  se        = sqrt(example_gamm$CV_se^2 + example_gamm$C_se^2),
  group     = example_gamm$group
)
fig13 <- overlay_empirical(sim, emp_real,
  group_colors = c("JP bilinguals" = "black", "EN monolinguals" = "grey50"))
ggsave("figures/Figure13_application1_gamm_overlay.png", fig13,
       width = 12, height = 6.5, dpi = 300)

# Formal selection for Application 1 (reported in the text), including the
# analysis-window robustness check:
gof_app1 <- goodness_of_fit(sim, emp_real)
gof_app1_win <- goodness_of_fit(sim, emp_real, time_window = c(21, 101))

# --- Figure 14: Application 2 - eye-tracking overlay -------------------------
emp_et <- example_empirical(sim, true_delta = 12,
  group_labels = c("L2 learners", "L1 controls"),
  paradigm = "eye-tracking", noise_sd = 0.02, seed = 99)
fig14 <- overlay_empirical(sim, emp_et,
  group_colors = c("L2 learners" = "black", "L1 controls" = "grey50"))
ggsave("figures/Figure14_overlay_eye_tracking.png", fig14,
       width = 12, height = 6.5, dpi = 300)

# --- Figure 15: Application 4 - sensitivity to lateral inhibition ------------
# Per-panel gamma labels identify the panels; no overall title/subtitle.
gammas <- c(0.02, 0.04, 0.06)
sims_gamma <- lapply(gammas, function(g) {
  run_simulation(stim, delta_values = c(0, 10, 20, 30), gamma = g,
                 verbose = FALSE)
})
fig15 <- plot_asymmetry(sims_gamma[[1]], title = "γ = 0.02") +
  plot_asymmetry(sims_gamma[[2]], title = "γ = 0.04") +
  plot_asymmetry(sims_gamma[[3]], title = "γ = 0.06")
ggsave("figures/Figure15_parameter_sensitivity.png", fig15,
       width = 18, height = 6.5, dpi = 300)

# --- Figure 16: Application 5 - cross-linguistic comparison ------------------
# Each panel keeps its own delta legend (different delta grids); the panel
# titles are the group labels. No overall title/subtitle.
sim_jp <- sim  # reuse the Japanese-English simulation
sim_zh <- run_simulation(stim, delta_values = c(0, 5, 10, 15, 20),
                         verbose = FALSE, seed = 1234)
cross <- compare_languages(sim_jp, sim_zh,
  label_a = "Japanese–English (Moraic)",
  label_b = "Chinese–English (Syllabic)")
fig16 <- cross$plot
ggsave("figures/Figure16_cross_linguistic.png", fig16,
       width = 16, height = 6.5, dpi = 300)

# --- Done ---------------------------------------------------------------------
message("\n=== All 16 figures generated ===")
for (f in sort(list.files("figures"))) message("  ", f)
sessionInfo()
