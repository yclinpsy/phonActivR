# =============================================================================
# phonActivR: Visualization Functions
# =============================================================================

# Legend and axis labels of the form "delta = 0" are built with plotmath, so
# that the Greek letter is drawn from the symbol font on every graphics device.
# The base pdf() device cannot encode a Unicode delta in a text string, and
# recent versions of R stop with an error instead of substituting a dot.
.delta_labels <- function(x, bold = FALSE) {
  lab <- paste0("delta == ", x)
  if (bold) lab <- paste0("bold(", lab, ")")
  parse(text = lab)
}

#' Plot Competition Effects (Two Panels: Smallest and Largest Delta)
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
#' @param title Character. Optional overall title. If \code{NULL} (the
#'   default) no title is drawn: for publication figures the title belongs in
#'   the manuscript's figure caption, not inside the image (per APA / journal
#'   figure guidelines). The per-panel delta labels are always shown because
#'   they identify the panels.
#'
#' @return A ggplot2 object.
#' @export
plot_competition <- function(sim,
                             delta = NULL,
                             colors = c(large = "#0072B2", small = "#D55E00"),
                             large_label = NULL,
                             small_label = NULL,
                             title = NULL) {

  stopifnot(inherits(sim, "phonActivR_sim"))

  lt <- sim$stimuli$large_type
  st <- sim$stimuli$small_type
  if (is.null(large_label)) large_label <- paste0(lt, " Competition Effect")
  if (is.null(small_label)) small_label <- paste0(st, " Competition Effect")

  make_panel <- function(d, ylim = NULL) {
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
        title = bquote(bold(delta == .(d))),
        x     = "Normalized Time (% of trial)",
        y     = "Competition Effect\n(Competitor - Control)") +
      { if (is.null(ylim)) ggplot2::geom_blank()
        else ggplot2::coord_cartesian(ylim = ylim) } +
      theme_phonactivr(base_size = 11.5) +
      ggplot2::theme(legend.position = "bottom")
  }

  if (is.null(delta)) {
    deltas <- sim$params$delta_values
    d0 <- deltas[1]
    d_max <- deltas[length(deltas)]
    # Shared y-axis limits across the two panels: the figure's job is a
    # side-by-side comparison of two hypotheses, so both panels must be
    # read on the same scale.
    rng <- range(unlist(lapply(c(d0, d_max), function(d) {
      r <- sim$results[[as.character(d)]]
      c(r$large_effect + 1.96 * r$large_se,
        r$small_effect - 1.96 * r$small_se, 0)
    })), na.rm = TRUE)
    p1 <- make_panel(d0, ylim = rng)
    p2 <- make_panel(d_max, ylim = rng)
    combined <- p1 + p2
    # No overall title/subtitle is baked into the image: per-panel delta
    # labels identify the panels, and descriptive titles belong in the
    # manuscript caption. A user-supplied title is honored.
    if (!is.null(title)) {
      combined <- combined + patchwork::plot_annotation(
        title = title,
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(size = 13, face = "bold")
        )
      )
    }
    return(combined)
  }

  p <- make_panel(delta)
  if (!is.null(title)) p <- p + ggplot2::labs(title = title)
  p
}


#' Plot Asymmetry Gradient Across Delta Values
#'
#' Plots the larger-grain minus smaller-grain competition effect asymmetry
#' curves for all tested delta values. This is the primary figure for
#' adjudicating between theoretical hypotheses.
#'
#' @param sim A \code{"phonActivR_sim"} object.
#' @param delta_colors Named character vector of colors keyed by delta values.
#'   Auto-generated if \code{NULL}.
#' @param title Character. Optional title. If \code{NULL} (the default) no
#'   title or subtitle is drawn inside the image; figure titles belong in the
#'   manuscript caption.
#'
#' @return A ggplot2 object.
#' @export
plot_asymmetry <- function(sim,
                           delta_colors = NULL,
                           title = NULL) {

  stopifnot(inherits(sim, "phonActivR_sim"))

  deltas <- sim$params$delta_values
  if (is.null(delta_colors)) {
    # Package-wide colorblind-safe palette (see ?phonactivr_colors): the
    # same delta value gets the same color in every figure, and color is
    # always paired with a distinct line type, so no plot relies on hue
    # alone.
    pal <- if (length(deltas) <= 8) {
      phonactivr_colors(length(deltas))
    } else {
      grDevices::hcl.colors(length(deltas), "Dark 3")
    }
    delta_colors <- stats::setNames(pal, as.character(deltas))
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

  # Line types distinguish the delta curves in addition to color, so the
  # figure remains readable in black-and-white print and for readers with
  # color-vision deficiencies.
  lt_pool <- c("solid", "longdash", "dashed", "dotdash",
               "dotted", "twodash", "1F", "4C88C488")
  delta_linetypes <- stats::setNames(lt_pool[seq_along(deltas)],
                                     as.character(deltas))

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
    ggplot2::geom_line(ggplot2::aes(linetype = .data$delta), linewidth = 1.3) +
    ggplot2::scale_color_manual(
      values = delta_colors,
      labels = .delta_labels(names(delta_colors)),
      name   = NULL) +
    ggplot2::scale_fill_manual(
      values = delta_colors,
      labels = .delta_labels(names(delta_colors)),
      name   = NULL) +
    ggplot2::scale_linetype_manual(
      values = delta_linetypes,
      labels = .delta_labels(names(delta_linetypes)),
      name   = NULL) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 100, 20),
      labels = paste0(seq(0, 100, 20), "%")) +
    ggplot2::labs(
      # No auto-generated title/subtitle: titles belong in the figure
      # caption, not inside the image. A user-supplied title is honored.
      title = title,
      x = "Normalized Time (% of trial)",
      y = bquote(.(paste(lt, "Effect")) - .(paste(st, "Effect")))
    ) +
    theme_phonactivr(base_size = 12.5) +
    ggplot2::theme(
      legend.position  = "right",
      legend.key.width = ggplot2::unit(1.8, "cm")
    )
}


#' Plot Onset Timing as a Function of Delta
#'
#' Plots the predicted competition onset for both grain sizes across all
#' delta values, showing how the prosodic delay shifts the smaller-grain
#' onset progressively later.
#'
#' @param sim A \code{"phonActivR_sim"} object.
#' @param mean_trial_ms Numeric. Mean trial duration in milliseconds for
#'   converting onset delay to ms (default: 1000).
#' @param colors Named character vector with \code{"large"} and \code{"small"}.
#' @param title Character. Optional title. If \code{NULL} (the default) no
#'   title is drawn inside the image; a series legend is always drawn.
#'
#' @return A ggplot2 object.
#' @export
plot_onset_timing <- function(sim,
                              mean_trial_ms = 1000,
                              colors = c(large = "#0072B2", small = "#D55E00"),
                              title = NULL) {

  stopifnot(inherits(sim, "phonActivR_sim"))

  onset_df <- sim$onsets
  # Onsets are stored as time steps; step k corresponds to (k - 1)% of the
  # trial, so convert to percent for plotting (delays are unaffected).
  onset_df$small_onset <- onset_df$small_onset - 1
  onset_df$large_onset <- onset_df$large_onset - 1
  lt <- sim$stimuli$large_type
  st <- sim$stimuli$small_type

  delay_labels <- onset_df[!is.na(onset_df$delay), ]
  delay_labels$label <- paste0(
    "+", delay_labels$delay, "% (~",
    round(delay_labels$delay * mean_trial_ms / 100), " ms)"
  )

  # Series labels drive a proper in-panel legend (the legend used to live in
  # the subtitle text; a mapped legend is clearer and survives caption-only
  # titling). Colors are keyed by series so both print and colorblind-safe
  # shape/linetype cues distinguish them.
  lab_small <- paste0(st, " onset")
  lab_large <- paste0(lt, " onset (reference)")
  series_colors    <- stats::setNames(unname(colors[c("small", "large")]),
                                      c(lab_small, lab_large))
  series_linetypes <- stats::setNames(c("solid", "dashed"),
                                      c(lab_small, lab_large))
  series_shapes    <- stats::setNames(c(16, 15), c(lab_small, lab_large))

  long_df <- rbind(
    data.frame(delta = onset_df$delta, onset = onset_df$small_onset,
               series = lab_small),
    data.frame(delta = onset_df$delta, onset = onset_df$large_onset,
               series = lab_large)
  )
  long_df$series <- factor(long_df$series, levels = c(lab_small, lab_large))

  ggplot2::ggplot(long_df,
                  ggplot2::aes(x = .data$delta, y = .data$onset,
                               color = .data$series)) +
    ggplot2::geom_ribbon(
      data = onset_df[!is.na(onset_df$small_onset) & !is.na(onset_df$large_onset), ],
      ggplot2::aes(x = .data$delta, ymin = .data$large_onset,
                   ymax = .data$small_onset),
      fill = "gray80", alpha = 0.6, inherit.aes = FALSE) +
    ggplot2::geom_line(ggplot2::aes(linetype = .data$series),
                       linewidth = 1.5) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$series), size = 5) +
    ggplot2::geom_text(data = delay_labels,
                        ggplot2::aes(x = .data$delta + 1, y = .data$small_onset + 2.5,
                                      label = .data$label),
                        color = colors["small"], size = 3.8,
                        fontface = "bold", hjust = 0, inherit.aes = FALSE) +
    ggplot2::scale_color_manual(values = series_colors, name = NULL) +
    ggplot2::scale_linetype_manual(values = series_linetypes, name = NULL) +
    ggplot2::scale_shape_manual(values = series_shapes, name = NULL) +
    ggplot2::scale_x_continuous(
      breaks = sim$params$delta_values,
      labels = .delta_labels(sim$params$delta_values, bold = TRUE),
      # Generous right-hand expansion keeps the "+X% (~Y ms)" annotations
      # (drawn to the right of the last point) inside the panel.
      expand = ggplot2::expansion(mult = c(0.07, 0.24))) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 100, 5),
      labels = function(x) paste0(x, "%")) +
    ggplot2::labs(
      # No auto title/subtitle inside the image; a supplied title is honored.
      title = title,
      x = expression("Prosodic Constraint Parameter (" * delta * ")"),
      y = "Competition Onset (% of trial)"
    ) +
    theme_phonactivr(base_size = 12.5) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x     = ggplot2::element_text(size = 11, face = "bold")
    )
}


#' Overlay Empirical Data on Asymmetry Plot
#'
#' Adds empirical time-course data (e.g., from GAMM difference smooths)
#' to an asymmetry gradient plot, enabling visual comparison between
#' model predictions and observed behavior.
#'
#' Each group is drawn exactly as \code{\link{goodness_of_fit}} fitted it:
#' in model units (the data divided by the least-squares scale of the delta
#' model selected for that group) and, when a response latency is estimated,
#' shifted by that group's fitted lag. The selected curve is therefore the
#' one the data are fitted to, and visual inspection and formal selection
#' use the same scaling.
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
#' @param title Character. Optional title. If \code{NULL} (the default) no
#'   title is drawn inside the image; figure titles belong in the caption.
#' @param fit Logical. If \code{TRUE} (the default), the selected delta
#'   for each group is printed to the console and the full
#'   \code{\link{goodness_of_fit}} table is attached to the returned plot as
#'   the \code{"fit"} attribute (no text is drawn inside the image). The
#'   scaling is the same whether or not \code{fit} is \code{TRUE}.
#' @param time_window Optional length-2 numeric vector passed to
#'   \code{\link{goodness_of_fit}}; the scale factor (and the reported
#'   selection) is then estimated on that window only.
#' @param lag Candidate response latencies passed to
#'   \code{\link{goodness_of_fit}} (default \code{0:50}). When a latency is estimated,
#'   each group's empirical curve is drawn shifted left by its fitted lag, so
#'   its time axis is aligned with the model's (word onset = time 1).
#' @param show_pre_onset Logical. If \code{FALSE} (the default), empirical
#'   points that the latency shift moves before the model's time 1 are not
#'   drawn, because the model makes no prediction there. If \code{TRUE}, they
#'   are drawn, the x-axis is extended to negative model time, and a dotted
#'   line marks the model's time 1, so any empirical effect that precedes the
#'   model's onset stays visible.
#' @param facet_groups Logical. If \code{FALSE} (the default), all groups are
#'   drawn in one panel. If \code{TRUE} and \code{empirical_data} has more
#'   than one group, one panel is drawn per group (each group aligned to its
#'   own selected delta curve) and the panels are combined side by side with
#'   patchwork and tagged A, B, ...; this is the layout of Figure 6A-B in the
#'   tutorial.
#'
#' @return A ggplot2 object (or a patchwork object when
#'   \code{facet_groups = TRUE} with several groups). When \code{fit = TRUE},
#'   the object carries a \code{"fit"} attribute (the
#'   \code{\link{goodness_of_fit}} table, whose own \code{"best_delta"}
#'   attribute gives the selected delta per group).
#' @export
overlay_empirical <- function(sim,
                              empirical_data,
                              group_colors = NULL,
                              delta_colors = NULL,
                              title = NULL,
                              fit = TRUE,
                              time_window = NULL,
                              lag = 0:50,
                              show_pre_onset = FALSE,
                              facet_groups = FALSE) {

  stopifnot(inherits(sim, "phonActivR_sim"))
  stopifnot(is.data.frame(empirical_data))
  stopifnot("time" %in% names(empirical_data))
  if (!"asymmetry" %in% names(empirical_data) &&
      all(c("cv_effect", "c_effect") %in% names(empirical_data))) {
    empirical_data$asymmetry <- empirical_data$cv_effect - empirical_data$c_effect
  }
  stopifnot("asymmetry" %in% names(empirical_data))

  if (!"group" %in% names(empirical_data)) {
    empirical_data$group <- "Empirical"
  }

  # One panel per group: draw each group on its own, then combine.
  if (isTRUE(facet_groups) && length(unique(empirical_data$group)) > 1) {
    groups <- unique(empirical_data$group)
    panels <- lapply(groups, function(g) {
      overlay_empirical(sim, empirical_data[empirical_data$group == g, ],
                        group_colors = group_colors, delta_colors = delta_colors,
                        title = title, fit = FALSE, time_window = time_window,
                        lag = lag, show_pre_onset = show_pre_onset,
                        facet_groups = FALSE)
    })
    fit_table <- if (isTRUE(fit)) {
      goodness_of_fit(sim, empirical_data, time_window = time_window,
                      lag = lag, verbose = FALSE)
    } else NULL
    p <- patchwork::wrap_plots(panels, ncol = 2, guides = "collect") +
      patchwork::plot_annotation(tag_levels = "A")
    attr(p, "fit") <- fit_table
    return(p)
  }

  groups <- unique(empirical_data$group)
  if (is.null(group_colors)) {
    group_colors <- stats::setNames(
      c("black", "grey45", "#CC79A7", "#56B4E9")[seq_along(groups)],
      groups
    )
  }

  # Fit every delta model to every group (least-squares scale per model);
  # each group is drawn with the scale factor of its selected delta model,
  # so the overlay shows exactly what was fitted.
  fit_table <- goodness_of_fit(sim, empirical_data,
                               time_window = time_window, lag = lag,
                               verbose = FALSE)
  best <- attr(fit_table, "best_delta")

  emp_norm <- dplyr::bind_rows(lapply(groups, function(g) {
    gd <- empirical_data[empirical_data$group == g, ]
    ft <- fit_table[fit_table$group == g, ]
    b   <- ft$scale[ft$delta == best[[g]]]
    lg  <- ft$lag[ft$delta == best[[g]]]
    # Draw the data in model units (divide by the model's fitted scale) and
    # shift it left by the fitted response latency, so the group sits on
    # its selected delta curve exactly as it was fitted.
    scale_factor <- if (b > 0) 1 / b else 1

    out <- data.frame(
      time      = gd$time - lg,
      asymmetry = gd$asymmetry * scale_factor,
      group     = g
    )
    if ("se" %in% names(gd)) {
      out$asym_lo <- out$asymmetry - gd$se * scale_factor
      out$asym_hi <- out$asymmetry + gd$se * scale_factor
    }
    out
  }))
  # Points shifted before word onset carry no model prediction; they are
  # dropped unless show_pre_onset = TRUE.
  if (!isTRUE(show_pre_onset)) emp_norm <- emp_norm[emp_norm$time >= 1, ]

  # Build base asymmetry plot
  p <- plot_asymmetry(sim, delta_colors = delta_colors, title = title)
  if (isTRUE(show_pre_onset) && min(emp_norm$time) < 1) {
    lo <- 20 * floor((min(emp_norm$time) - 1) / 20)
    p <- suppressMessages(p + ggplot2::scale_x_continuous(
      breaks = seq(lo, 100, 20), labels = paste0(seq(lo, 100, 20), "%"))) +
      ggplot2::geom_vline(xintercept = 1, linetype = "dotted",
                          color = "grey40")
  }

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

    # Place the group label near the curve peak, but keep it inside the
    # panel (a peak at the right edge would otherwise clip the label).
    x_lab <- min(gd$time[which.max(gd$asymmetry)] + 3,
                 max(gd$time, na.rm = TRUE) - 12)
    p <- p + ggplot2::geom_line(
      data = gd,
      ggplot2::aes(x = .data$time, y = .data$asymmetry),
      color = group_colors[g], linewidth = lw,
      linetype = lt, inherit.aes = FALSE
    ) +
      ggplot2::annotate("text",
        x = x_lab,
        y = max(gd$asymmetry, na.rm = TRUE) + 0.02,
        label = g,
        color = group_colors[g], size = 3.2, hjust = 0, fontface = "bold"
      )
  }

  # --- Report the selection (console only; nothing drawn in the image) ---
  if (isTRUE(fit)) {
    best_txt <- paste0(names(best), ": best-fitting \u03b4 = ", best,
                       collapse = " | ")
    cli::cli_alert_info(
      paste0("Model selection (least-squares scale, lowest RSS/AIC) \u2014 ",
             best_txt, " (full table: attr(plot, 'fit')). ",
             "Check the C component with fit_components() before ",
             "interpreting \u03b4."))
  } else {
    fit_table <- NULL
  }

  attr(p, "fit") <- fit_table
  p
}
