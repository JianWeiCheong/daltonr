#' The daltonr ggplot2 theme
#'
#' A ggplot2 theme in a more modern scientific and CVD-friendly style:
#' - White panel, black axes with outside ticks, and grey grid lines
#' - Borderless white facet strips
#' - Petroff colour palettes for categorical data
#' - Crameri/viridis/cividis colour maps (default `batlow`) for continuous data
#' - Optional modern scales with `set_scales = TRUE`
#' - Optional no axis padding on raster/filled-contour plots with `auto_coord = TRUE`
#' - Optional automatic `linetype` for line plots
#'
#' @param base_size Base font size in pt. Defaults to 11.
#' @param base_family Base font family. Defaults to `"sans"`.
#' @param set_scales If `TRUE` (default), also set default colour scales.
#' @param auto_coord If `TRUE` (default), automatically add
#'   `coord_cartesian(expand = FALSE)` to plots with raster or filled-contour
#'   layers, so they fill the axis frame.
#' @param auto_linetype If `TRUE` (default), automatically add linetype to
#'   line layers with discrete colour.
#' @param grid Which panel grid lines: `"both"` (default), `"major"`, or `"none"`.
#' @param border If `TRUE` (default), draw full panel border.
#' @param axis_ticks `"outside"` (default), `"inside"`, or `"none"`.
#' @param title_align `"center"` (default) or `"left"` for title and subtitle.
#' @param na.value Set colour (default "grey80") for missing values.
#' @param colourbar `"fill"` (default) or `"fixed"` for colourbar height.
#' @param colourbar_title `"right"` (default) or `"top"` for title position.
#'
#' @return A complete ggplot2 theme object.
#'
#' @export
#'
#' @examples
#' ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy, colour = cyl > 4)) +
#'   ggplot2::geom_point() +
#'   theme_daltonr()
#'
#' # With the theme set, scales are picked automatically:
#' ggplot2::ggplot(ggplot2::faithfuld, ggplot2::aes(waiting, eruptions, fill = density)) +
#'   ggplot2::geom_raster()
#'
#' ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(x = class, fill = drv)) +
#'   ggplot2::geom_bar() # 3 groups -> petroff6
theme_daltonr <- function(base_size = 11, base_family = "sans", set_scales = TRUE,
                          auto_coord = TRUE,
                          auto_linetype = TRUE,
                          grid = c("both", "major", "none"),
                          border = TRUE,
                          axis_ticks = c("outside", "inside", "none"),
                          title_align = c("center", "left"),
                          na.value = "grey80",
                          colourbar = c("fill", "fixed"),
                          colourbar_title = c("right", "top")) {
  grid <- match.arg(grid)
  axis_ticks <- match.arg(axis_ticks)
  title_align <- match.arg(title_align)
  colourbar <- match.arg(colourbar)
  colourbar_title <- match.arg(colourbar_title)
  if (isTRUE(set_scales)) {
    options(
      ggplot2.discrete.colour = function(...) scale_colour_petroff(...),
      ggplot2.discrete.fill = function(...) scale_fill_petroff(...),
      ggplot2.continuous.colour = function(...) scale_colour_daltonr(...),
      ggplot2.continuous.fill = function(...) scale_fill_daltonr(...)
    )
  }
  options(
    daltonr.na.value = na.value,
    daltonr.colourbar = colourbar,
    daltonr.colourbar_title = colourbar_title
  )
  grid_major <- if (grid %in% c("both", "major")) {
    ggplot2::element_line(colour = "grey90", linewidth = 0.4)
  } else {
    ggplot2::element_blank()
  }
  grid_minor <- if (identical(grid, "both")) {
    ggplot2::element_line(colour = "grey95", linewidth = 0.25)
  } else {
    ggplot2::element_blank()
  }
  panel_border <- if (isTRUE(border)) {
    ggplot2::element_rect(fill = NA, colour = "black", linewidth = 0.6)
  } else {
    ggplot2::element_blank()
  }
  axis_line <- if (isTRUE(border)) {
    ggplot2::element_blank()
  } else {
    ggplot2::element_line(colour = "black", linewidth = 0.5)
  }
  axis_tick_line <- if (identical(axis_ticks, "none")) {
    ggplot2::element_blank()
  } else {
    ggplot2::element_line(colour = "black", linewidth = 0.5)
  }
  # negative tick length draws the ticks inside the panel
  axis_tick_length <- ggplot2::unit(
    switch(axis_ticks, outside = 2.5, inside = -2.5, none = 0),
    "pt"
  )
  title_hjust <- if (identical(title_align, "center")) 0.5 else 0
  theme <- ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border = panel_border,
      axis.line = axis_line,
      panel.grid.major = grid_major,
      panel.grid.minor = grid_minor,
      axis.text = ggplot2::element_text(colour = "black"),
      axis.title = ggplot2::element_text(colour = "black"),
      axis.ticks = axis_tick_line,
      axis.ticks.length = axis_tick_length,
      plot.title = ggplot2::element_text(face = "bold", hjust = title_hjust),
      plot.subtitle = ggplot2::element_text(hjust = title_hjust),
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "panel",
      plot.caption.position = "plot",
      strip.background = ggplot2::element_rect(
        fill = "white", colour = NA
      ),
      strip.text = ggplot2::element_text(colour = "black", face = "bold"),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(face = "bold")
    )
  attr(theme, "daltonr_auto_coord") <- isTRUE(auto_coord)
  attr(theme, "daltonr_auto_linetype") <- isTRUE(auto_linetype)
  class(theme) <- c("daltonr_theme", class(theme))
  theme
}

# Theme hook: auto coord on raster/contour plots, auto linetype on lines
#' @export
#' @noRd
ggplot_add.daltonr_theme <- function(object, plot, ...) {
  plot <- NextMethod()
  if (inherits(plot, "ggplot")) {
    if (isTRUE(attr(object, "daltonr_auto_coord")) && needs_expand_false(plot)) {
      plot <- plot + ggplot2::coord_cartesian(expand = FALSE)
    }
    if (isTRUE(attr(object, "daltonr_auto_linetype"))) {
      plot <- add_auto_linetype(plot)
    }
  }
  plot
}

# Raster/contour layers
needs_expand_false <- function(plot) {
  coord <- plot$coordinates
  if (!identical(class(coord)[1], "CoordCartesian") || !isTRUE(coord$default)) {
    return(FALSE)
  }
  any(vapply(
    plot$layers,
    function(layer) {
      inherits(layer$geom, "GeomRaster") || inherits(layer$stat, "StatContourFilled")
    },
    logical(1)
  ))
}

#' Use the daltonr theme for the rest of the session
#'
#' @param ... Arguments passed to [theme_daltonr()].
#'
#' @return Invisibly, the theme that was set.
#' @export
use_daltonr <- function(...) {
  theme <- theme_daltonr(...)
  ggplot2::theme_set(theme)
  invisible(theme)
}
