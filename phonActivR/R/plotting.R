# =============================================================================
# phonActivR: Visualization Functions
# =============================================================================

#' Plot Competition Effects (Figure 1 style)
#'
#' Plots predicted larger-grain and smaller-grain competition effect curves
#' for a specified delta value, with 95\% CI bands across items.
#'
#' @param sim A \code{"phonActivR_sim"} object from \code{\link{run_simulation}}.
#' @param delta Integer. Which delta value to plot. If \code{NULL}, plots two
#'   panels: delta = 0 (left) and the largest non-zero delta (right).
#' @param colors Named character vector with elements \code{"large"} and
#'   \code{"small"} specifying line colours. Defaults to blue/red.
#' @param large_label Character. Legend label for larger-grain effect
#'   (default: "CV Competition Effect").
#' @param small_label Character. Legend label for smaller-grain effect
#'   (default: "C Competition Effect").
#' @param title Character. Plot title. Auto-generated if \code{NULL}.
#'
#' @return A ggplot2 object.
#' @export
plot_competition <- function(sim,
                             delta = NULL,
                             colors = c(large = "#2166AC", small = "#D6604D"),
                             large_label = NULL,
                             small_label = NULL,
                             title = NULL) {

  stopifnot(inherits(sim, "phonActivR_sim"))

  lt <- sim$stimuli$large_type
  st <- sim$stimuli$small_type
  if (is.null(large_label)) large_label <- paste0(lt, " Competition Effect")
  if (is.null(small_label)) small_label <- paste0(st, " Competition Effect")

  make_panel <- function(d) {
    res  <- sim$results[[as.character(d)]]
    time <- seq_len(sim$params$time_steps)

    df <- data.frame(
      time      = rep(time, 2),
      effect    = c(res$large_effect, res$small_effect),
      lower     = c(res$large_effect - 1.96 * res$large_se,
                    res$small_effect - 1.96 * res$small_se),
      upper     = c(res$large_effect + 1.96 * res$large_se,
                    res$small_effect + 1.96 * res$small_se),
      condition = rep(c(large_label, small_label),
                      each = sim$params$time_steps)
    )
    df$condition <- factor(df$condition, levels = c(large_label, small_label))

    ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$effect,
                                      color = .data$condition,
                                      fill = .data$condition)) +
      ggplot2::geom_hline(yintercept = 0, color = "gray70", linewidth = 0.7) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                            alpha = 0.15, color = NA) +
      ggplot2::geom_line(ggplot2::aes(linetype = .data$condition),
                          linewidth = 1.2) +
      ggplot2::scale_color_manual(
        values = stats::setNames(colors[c("large", "small")],
                                  c(large_label, small_label)),
        name = NULL) +
      ggplot2::scale_fill_manual(
        values = stats::setNames(colors[c("large", "small")],
                                  c(large_label, small_label)),
        name = NULL) +
      ggplot2::scale_linetype_manual(
        values = stats::setNames(c("solid", "dashed"),
                                  c(large_label, small_label)),
        name = NULL) +
      ggplot2::scale_x_continuous(
        breaks = seq(0, 100, 20),
        labels = paste0(seq(0, 100, 20), "%")) +
      ggplot2::labs(
        title = paste0("\u03b4 = ", d),
        x     = "Normalized Time (% of trial)",
        y     = "Competition Effect\n(Competitor \u2212 Control)") +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(
        legend.position  = "bottom",
        legend.text      = ggplot2::element_text(size = 10),
        panel.grid.minor = ggplot2::element_blank()
      )
  }

  if (is.null(delta)) {
    deltas <- sim$params$delta_values
    d0 <- deltas[1]
    d_max <- deltas[length(deltas)]
    p1 <- make_panel(d0)
    p2 <- make_panel(d_max)
    combined <- p1 + p2 +
      patchwork::plot_annotation(
        title = title %||% "Predicted Competition Effects",
        subtitle = paste0(
          "phonActivR Simulation (N = ", sim$stimuli$n_items,
          " items, 95% CI)\nLeft: \u03b4 = ", d0,
          " | Right: \u03b4 = ", d_max
        ),
        theme = ggplot2::theme(
          plot.title    = ggplot2::element_text(size = 13, face = "bold"),
          plot.subtitle = ggplot2::element_text(size = 9.5, color = "gray40")
        )
      )
    return(combined)
  }

  p <- make_panel(delta)
  if (!is.null(title)) p <- p + ggplot2::labs(title = title)
  p
}


#' Plot Asymmetry Gradient Across Delta Values (Figure 2 style)
#'
#' Plots the larger-grain minus smaller-grain competition effect asymmetry
#' curves for all tested delta values. This is the primary figure for
#' adjudicating between theoretical hypotheses.
#'
#' @param sim A \code{"phonActivR_sim"} object.
#' @param delta_colors Named character vector of colors keyed by delta values.
#'   Auto-generated if \code{NULL}.
#' @param title Character. Plot title.
#'
#' @return A ggplot2 object.
#' @export
plot_asymmetry <- function(sim,
                           delta_colors = NULL,
                           title = NULL) {

  stopifnot(inherits(sim, "phonActivR_sim"))

  deltas <- sim$params$delta_values
  if (is.null(delta_colors)) {
    pal <- c("#4DAF4A", "#377EB8", "#FF7F00", "#E41A1C",
             "#984EA3", "#A65628", "#F781BF", "#999999")
    delta_colors <- stats::setNames(pal[seq_along(deltas)],
                                     as.character(deltas))
  }

  asym_df <- dplyr::bind_rows(lapply(deltas, function(d) {
    res <- sim$results[[as.character(d)]]
    data.frame(
      time      = seq_len(sim$params$time_steps),
      asymmetry = res$asymmetry,
      asym_lo   = res$asymmetry - 1.96 * res$asym_se,
      asym_hi   = res$asymmetry + 1.96 * res$asym_se,
      delta     = factor(d, levels = as.character(deltas))
    )
  }))

  lt <- sim$stimuli$large_type
  st <- sim$stimuli$small_type

  ggplot2::ggplot(asym_df, ggplot2::aes(x = .data$time, y = .data$asymmetry,
                                          color = .data$delta,
                                          fill = .data$delta)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray60",
                         linewidth = 0.8, linetype = "dotted") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$asym_lo,
                                       ymax = .data$asym_hi),
                          alpha = 0.10, color = NA) +
    ggplot2::geom_line(linewidth = 1.3) +
    ggplot2::scale_color_manual(
      values = delta_colors,
      labels = paste0("\u03b4 = ", names(delta_colors)),
      name   = NULL) +
    ggplot2::scale_fill_manual(
      values = delta_colors,
      labels = paste0("\u03b4 = ", names(delta_colors)),
      name   = NULL) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 100, 20),
      labels = paste0(seq(0, 100, 20), "%")) +
    ggplot2::labs(
      title = title %||% paste0(lt, "\u2212", st,
                                 " Asymmetry Across \u03b4 Values"),
      subtitle = paste0(
        "phonActivR Simulation (N = ", sim$stimuli$n_items,
        " items, 95% CI)\nY-axis = ", lt, " effect \u2212 ", st,
        " effect at each time step"
      ),
      x = "Normalized Time (% of trial)",
      y = paste0(lt, " Effect \u2212 ", st, " Effect")
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      legend.position  = "right",
      legend.text      = ggplot2::element_text(size = 9.5),
      legend.key.width = ggplot2::unit(1.8, "cm"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(size = 13, face = "bold"),
      plot.subtitle    = ggplot2::element_text(size = 9.5, color = "gray40")
    )
}


#' Plot Onset Timing as Function of Delta (Figure 3 style)
#'
#' Plots the predicted competition onset for both grain sizes across all
#' delta values, showing how the prosodic delay shifts the smaller-grain
#' onset progressively later.
#'
#' @param sim A \code{"phonActivR_sim"} object.
#' @param mean_trial_ms Numeric. Mean trial duration in milliseconds for
#'   converting onset delay to ms (default: 1000).
#' @param colors Named character vector with \code{"large"} and \code{"small"}.
#' @param title Character. Plot title.
#'
#' @return A ggplot2 object.
#' @export
plot_onset_timing <- function(sim,
                              mean_trial_ms = 1000,
                              colors = c(large = "#2166AC", small = "#D6604D"),
                              title = NULL) {

  stopifnot(inherits(sim, "phonActivR_sim"))

  onset_df <- sim$onsets
  lt <- sim$stimuli$large_type
  st <- sim$stimuli$small_type

  delay_labels <- onset_df[!is.na(onset_df$delay), ]
  delay_labels$label <- paste0(
    "+", delay_labels$delay, "% (\u2248",
    round(delay_labels$delay * mean_trial_ms / 100), " ms)"
  )

  ggplot2::ggplot(onset_df, ggplot2::aes(x = .data$delta)) +
    ggplot2::geom_ribbon(
      data = onset_df[!is.na(onset_df$small_onset) & !is.na(onset_df$large_onset), ],
      ggplot2::aes(ymin = .data$large_onset, ymax = .data$small_onset),
      fill = "gray80", alpha = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = .data$large_onset),
                        color = colors["large"], linewidth = 1.4,
                        linetype = "dashed") +
    ggplot2::geom_point(ggplot2::aes(y = .data$large_onset),
                         color = colors["large"], size = 5, shape = 15) +
    ggplot2::geom_line(ggplot2::aes(y = .data$small_onset),
                        color = colors["small"], linewidth = 1.8) +
    ggplot2::geom_point(ggplot2::aes(y = .data$small_onset),
                         color = colors["small"], size = 6) +
    ggplot2::geom_text(data = delay_labels,
                        ggplot2::aes(x = .data$delta + 1, y = .data$small_onset + 2.5,
                                      label = .data$label),
                        color = colors["small"], size = 3.8,
                        fontface = "bold", hjust = 0) +
    ggplot2::scale_x_continuous(
      breaks = sim$params$delta_values,
      labels = paste0("\u03b4 = ", sim$params$delta_values)) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 100, 5),
      labels = function(x) paste0(x, "%")) +
    ggplot2::labs(
      title = title %||% paste0(st, " Competition Onset as Function of \u03b4"),
      subtitle = paste0(
        "phonActivR Simulation (N = ", sim$stimuli$n_items, " items)\n",
        "Red = ", st, " onset | Blue = ", lt, " onset (reference) | ",
        "ms estimates assume ", mean_trial_ms, " ms trial"
      ),
      x = "Prosodic Constraint Parameter (\u03b4)",
      y = "Competition Onset (% of trial)"
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(size = 13, face = "bold"),
      plot.subtitle    = ggplot2::element_text(size = 9.5, color = "gray40"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(size = 11, face = "bold")
    )
}


#' Overlay Empirical Data on Asymmetry Plot
#'
#' Adds empirical time-course data (e.g., from GAMM difference smooths)
#' to an asymmetry gradient plot, enabling visual comparison between
#' model predictions and observed behavior.
#'
#' @param sim A \code{"phonActivR_sim"} object.
#' @param empirical_data A data.frame with columns:
#'   \describe{
#'     \item{time}{Numeric. Normalized time (1 to time_steps)}
#'     \item{asymmetry}{Numeric. Empirical larger-grain minus smaller-grain effect}
#'     \item{se}{Numeric. Standard error of the asymmetry (optional)}
#'     \item{group}{Character. Group label (optional; for multiple groups)}
#'   }
#' @param group_colors Named character vector. Colors for each group in
#'   \code{empirical_data$group}. If \code{NULL}, defaults to black for
#'   the first group and grey for the second.
#' @param delta_colors Named character vector for predicted curves. Passed to
#'   \code{\link{plot_asymmetry}}.
#' @param title Character. Plot title.
#'
#' @return A ggplot2 object.
#' @export
overlay_empirical <- function(sim,
                              empirical_data,
                              group_colors = NULL,
                              delta_colors = NULL,
                              title = NULL) {

  stopifnot(inherits(sim, "phonActivR_sim"))
  stopifnot(is.data.frame(empirical_data))
  stopifnot("time" %in% names(empirical_data))
  stopifnot("asymmetry" %in% names(empirical_data))

  # Normalize empirical data to simulation scale
  sim_max <- max(sim$results[[as.character(max(sim$params$delta_values))]]$asymmetry,
                 na.rm = TRUE)

  if (!"group" %in% names(empirical_data)) {
    empirical_data$group <- "Empirical"
  }

  groups <- unique(empirical_data$group)
  if (is.null(group_colors)) {
    group_colors <- stats::setNames(
      c("black", "grey50", "#E7298A", "#66A61E")[seq_along(groups)],
      groups
    )
  }

  # Normalize each group
  emp_norm <- dplyr::bind_rows(lapply(groups, function(g) {
    gd <- empirical_data[empirical_data$group == g, ]
    emp_max <- max(abs(gd$asymmetry), na.rm = TRUE)
    scale_factor <- if (emp_max > 0) sim_max / emp_max else 1

    out <- data.frame(
      time      = gd$time,
      asymmetry = gd$asymmetry * scale_factor,
      group     = g
    )
    if ("se" %in% names(gd)) {
      out$asym_lo <- out$asymmetry - gd$se * scale_factor
      out$asym_hi <- out$asymmetry + gd$se * scale_factor
    }
    out
  }))

  # Build base asymmetry plot
  p <- plot_asymmetry(sim, delta_colors = delta_colors, title = title)

  # Add empirical layers
  for (g in groups) {
    gd <- emp_norm[emp_norm$group == g, ]
    lw <- if (g == groups[1]) 2.0 else 0.8
    lt <- if (g == groups[1]) "solid" else "dashed"

    if ("asym_lo" %in% names(gd)) {
      p <- p + ggplot2::geom_ribbon(
        data = gd,
        ggplot2::aes(x = .data$time, ymin = .data$asym_lo,
                      ymax = .data$asym_hi),
        fill = group_colors[g], alpha = 0.15, inherit.aes = FALSE
      )
    }

    p <- p + ggplot2::geom_line(
      data = gd,
      ggplot2::aes(x = .data$time, y = .data$asymmetry),
      color = group_colors[g], linewidth = lw,
      linetype = lt, inherit.aes = FALSE
    ) +
      ggplot2::annotate("text",
        x = gd$time[which.max(gd$asymmetry)] + 3,
        y = max(gd$asymmetry, na.rm = TRUE) + 0.02,
        label = g,
        color = group_colors[g], size = 3.2, hjust = 0, fontface = "bold"
      )
  }

  p + ggplot2::labs(
    caption = paste0(
      "Empirical lines normalized to simulation scale (shape preserved, ",
      "amplitude rescaled).\n",
      "The \u03b4 curve the empirical line most closely tracks = ",
      "estimated prosodic constraint strength."
    )
  )
}
