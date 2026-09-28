# =============================================================================
# phonActivR: Publication Theme and Color Palette
# =============================================================================
# Every plotting function in the package draws from the two helpers below, so
# that any figure a user produces is publication-ready by default: one
# consistent, colorblind-safe categorical palette (Okabe-Ito) and one
# journal-style ggplot2 theme. Users can override both (all plotting
# functions accept color arguments and return ordinary ggplot2 objects).

#' Colorblind-Safe Categorical Palette
#'
#' Returns the package's categorical color palette, based on the widely used
#' Okabe-Ito palette for colorblind-safe scientific figures. The first four
#' colors (green, blue, orange, vermilion red) are assigned to a
#' simulation's delta values in increasing order, so all figures that share
#' one delta grid share one color mapping (a figure simulated over a
#' different grid maps its own ordered delta values). Colors are always
#' paired with distinct line types or point
#' shapes in the package's plots, so no figure relies on color alone.
#'
#' @param n Integer. Number of colors to return (1 to 8).
#'
#' @return A character vector of n hex colors.
#'
#' @export
#' @examples
#' phonactivr_colors(4)
phonactivr_colors <- function(n = 4) {
  pal <- c(
    "#009E73",  # bluish green   (delta 1: e.g., delta = 0)
    "#0072B2",  # blue           (delta 2)
    "#E69F00",  # orange         (delta 3)
    "#D55E00",  # vermilion red  (delta 4)
    "#CC79A7",  # reddish purple (delta 5)
    "#56B4E9",  # sky blue       (delta 6)
    "#8C510A",  # brown          (delta 7)
    "#666666"   # grey           (delta 8)
  )
  stopifnot(is.numeric(n), n >= 1, n <= length(pal))
  pal[seq_len(n)]
}

#' Publication Theme for phonActivR Figures
#'
#' A ggplot2 theme used by every plotting function in the package, designed
#' for journal figures: clean white panel, recessive light horizontal grid
#' (no minor grid, no vertical clutter), dark axis text and titles, subtle
#' axis line and ticks, transparent legend, and bold facet strips on a
#' plain background. No titles or subtitles are drawn inside the image;
#' figure titles belong in the manuscript caption.
#'
#' @param base_size Numeric. Base font size in points (default: 12.5).
#' @param grid Character. Which major grid lines to keep: \code{"horizontal"}
#'   (the default; y-axis reference lines only), \code{"both"}, or
#'   \code{"none"}.
#'
#' @return A ggplot2 theme object; add it to any plot with \code{+}.
#'
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_phonactivr()
theme_phonactivr <- function(base_size = 12.5, grid = c("horizontal", "both", "none")) {
  grid <- match.arg(grid)
  ink      <- "grey15"   # primary text
  ink_soft <- "grey30"   # axis titles / secondary text
  gridcol  <- "grey90"   # recessive reference lines
  axiscol  <- "grey40"   # axis line + ticks

  th <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      # --- text ---------------------------------------------------------
      text             = ggplot2::element_text(color = ink),
      axis.text        = ggplot2::element_text(color = ink, size = ggplot2::rel(0.88)),
      axis.title       = ggplot2::element_text(color = ink_soft, size = ggplot2::rel(0.96)),
      axis.title.x     = ggplot2::element_text(margin = ggplot2::margin(t = 9)),
      axis.title.y     = ggplot2::element_text(margin = ggplot2::margin(r = 9)),
      plot.title       = ggplot2::element_text(size = ggplot2::rel(1.05), face = "bold",
                                               margin = ggplot2::margin(b = 8)),
      # --- panel --------------------------------------------------------
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = gridcol, linewidth = 0.45),
      axis.line.x      = ggplot2::element_line(color = axiscol, linewidth = 0.5),
      axis.ticks       = ggplot2::element_line(color = axiscol, linewidth = 0.5),
      axis.ticks.length = ggplot2::unit(3, "pt"),
      # --- legend -------------------------------------------------------
      legend.background = ggplot2::element_blank(),
      legend.key        = ggplot2::element_blank(),
      legend.text       = ggplot2::element_text(color = ink, size = ggplot2::rel(0.85)),
      legend.title      = ggplot2::element_text(color = ink_soft, face = "bold",
                                                size = ggplot2::rel(0.9)),
      # --- facets -------------------------------------------------------
      strip.background = ggplot2::element_rect(fill = "grey96", color = NA),
      strip.text       = ggplot2::element_text(face = "bold", color = ink,
                                               size = ggplot2::rel(0.92),
                                               margin = ggplot2::margin(4, 4, 4, 4)),
      plot.margin      = ggplot2::margin(10, 12, 8, 8)
    )

  if (grid == "horizontal") {
    th <- th + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  } else if (grid == "none") {
    th <- th + ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  }
  th
}
