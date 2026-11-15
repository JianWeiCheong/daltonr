#' daltonr colour scales.
#'
#' Scales that map the scientific colour maps (Crameri, viridis, cividis) to
#' a matplotlib-like aesthetics. Pick the map with `palette`.
#'
#' `scale_colour_daltonr()` / `scale_fill_daltonr()` are continuous scales
#' (gradient with a [guide_daltonr_colourbar()]); the `_d` variants are
#' discrete.
#'
#' @param ... Other arguments passed to the underlying ggplot2 scale functions
#'   (`name`, `breaks`, `labels`, `limits`, `na.value`, `guide`, ...). Missing
#'   values default to the `daltonr.na.value` option.
#' @param palette Name of the colour map. See [crameri_names()] for available maps.
#'   Defaults to `"batlow"`.
#' @param midpoint Midpoint to centre diverging scales on. Default (`NA`) scales
#'   linearly across the full colour map.
#' @param bounds For continuous scales: `"data"` (default) uses exact data limits,
#'   `"pretty"` rounds to nice values.
#' @inheritParams crameri
#'
#' @return A ggplot2 `Scale` object.
#'
#' @name scale-daltonr
#' @rdname scale-daltonr
#'
#' @examples
#' volcano_df <- data.frame(
#'   x = rep(seq_len(ncol(volcano)), each = nrow(volcano)),
#'   y = rep(seq_len(nrow(volcano)), ncol(volcano)),
#'   height = as.vector(volcano)
#' )
#'
#' ggplot2::ggplot(volcano_df, ggplot2::aes(x, y, fill = height)) +
#'   ggplot2::geom_raster() +
#'   scale_fill_daltonr(palette = "tokyo")
#'
#' ggplot2::ggplot(volcano_df, ggplot2::aes(x, y, z = height)) +
#'   ggplot2::geom_contour_filled() +
#'   scale_fill_daltonr_d(palette = "batlow", guide = guide_daltonr_colourbar("discrete"))
#'
#' ggplot2::ggplot(volcano_df, ggplot2::aes(x, y, fill = height - mean(volcano))) +
#'   ggplot2::geom_raster() +
#'   scale_fill_daltonr(palette = "broc", midpoint = 0)
NULL

# Center diverging scales on `mid`
mid_rescaler <- function(mid) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    lim <- max(abs(from - mid))
    if (!is.finite(lim) || lim == 0) {
      return(rep.int(mean(to), length(x)))
    }
    (x - mid) / lim * diff(to) / 2 + sum(to) / 2
  }
}

# Colourbar full height, ticks on the label side
colourbar_guide_theme <- function() {
  fill <- !identical(getOption("daltonr.colourbar", "fill"), "fixed")
  title_position <- match.arg(
    getOption("daltonr.colourbar_title", "right"),
    c("right", "top")
  )
  theme <- ggplot2::theme(
    legend.key.height = if (fill) {
      ggplot2::unit(1, "null")
    } else {
      ggplot2::unit(5, "lines")
    },
    legend.ticks = ggplot2::element_line(colour = "black", linewidth = 0.5),
    legend.ticks.length = ggplot2::unit(c(-2.5, 0), "pt"),
    legend.margin = ggplot2::margin(0),
    legend.box.margin = ggplot2::margin(0)
  )
  if (identical(title_position, "right")) {
    theme + ggplot2::theme(
      legend.title.position = "right",
      legend.title = ggplot2::element_text(angle = -90, hjust = 0.5)
    )
  } else {
    theme + ggplot2::theme(
      legend.title.position = "top",
      legend.title = ggplot2::element_text(hjust = 0.5)
    )
  }
}

#' daltonr colourbar guide
#'
#' A colourbar guide with daltonr defaults: panel-height bars, ticks outside
#' on the label side, and labelled limits. Works with any ggplot2 scale.
#'
#' @param type Either `"continuous"` (default) for smooth colourbar or
#'   `"discrete"` for stepped colourbar (e.g., for [ggplot2::geom_contour_filled()]).
#'
#' @return A ggplot2 guide object.
#' @export
guide_daltonr_colourbar <- function(type = c("continuous", "discrete")) {
  type <- match.arg(type)
  if (identical(type, "discrete")) {
    ggplot2::guide_coloursteps(
      show.limits = TRUE,
      theme = colourbar_guide_theme()
    )
  } else {
    ggplot2::guide_colourbar(theme = colourbar_guide_theme())
  }
}

pretty_limits <- function(limits) {
  limits <- range(limits, na.rm = TRUE)
  if (!all(is.finite(limits))) {
    return(limits)
  }
  breaks <- pretty(limits, n = 5)
  c(min(breaks), max(breaks))
}

# Label the limits
breaks_with_limits <- function(n = 5) {
  function(limits) {
    limits <- limits[is.finite(limits)]
    if (length(limits) == 0) {
      return(numeric(0))
    }
    if (length(limits) == 1) {
      return(limits)
    }
    lo <- min(limits)
    hi <- max(limits)
    if (lo == hi) {
      return(lo)
    }
    breaks <- pretty(c(lo, hi), n = n)
    breaks <- breaks[breaks > lo & breaks < hi]
    if (length(breaks) > 0) {
      # Drop breaks sitting almost on a limit (label collision)
      tol <- 0.02 * (hi - lo)
      breaks <- breaks[
        abs(breaks - lo) >= tol & abs(breaks - hi) >= tol
      ]
    }
    sort(unique(c(lo, breaks, hi)))
  }
}

crameri_scale_continuous <- function(aesthetics, palette, alpha = NULL, begin = 0,
                                     end = 1, direction = 1, midpoint = NA,
                                     bounds = c("data", "pretty"), ...) {
  bounds <- match.arg(bounds)
  cols <- crameri(
    256,
    palette = palette,
    alpha = alpha,
    begin = begin,
    end = end,
    direction = direction
  )
  scale_fn <- if (identical(aesthetics, "fill")) {
    ggplot2::scale_fill_gradientn
  } else {
    ggplot2::scale_colour_gradientn
  }
  dots <- list(...)
  if (is.null(dots$na.value)) {
    dots$na.value <- getOption("daltonr.na.value", "grey80")
  }
  if (identical(bounds, "pretty") &&
      (is.null(dots$limits) || inherits(dots$limits, "waiver"))) {
    dots$limits <- pretty_limits
  }
  if (is.null(dots$breaks) || inherits(dots$breaks, "waiver")) {
    dots$breaks <- breaks_with_limits()
  }
  trans <- dots$trans
  identity_trans <- is.null(trans) || identical(trans, "identity")
  if ((is.null(dots$labels) || inherits(dots$labels, "waiver")) && identity_trans) {
    dots$labels <- scales::label_number()
  }
  guide <- dots$guide
  use_default_guide <- is.null(guide) ||
    (is.character(guide) && guide %in% c("colourbar", "colorbar"))
  if (use_default_guide) {
    dots$guide <- guide_daltonr_colourbar("continuous")
  }
  args <- c(list(colours = cols, aesthetics = aesthetics), dots)
  if (!is.na(midpoint)) {
    args$rescaler <- mid_rescaler(midpoint)
  }
  do.call(scale_fn, args, quote = TRUE)
}

crameri_scale_discrete <- function(aesthetics, palette, alpha = 1, begin = 0,
                                   end = 1, direction = 1, ...) {
  palette_fn <- function(n) {
    crameri(n, palette = palette, alpha = alpha, begin = begin, end = end, direction = direction)
  }
  dots <- list(...)
  if (is.null(dots$na.value)) {
    dots$na.value <- getOption("daltonr.na.value", "grey80")
  }
  do.call(
    ggplot2::discrete_scale,
    c(list(aesthetics = aesthetics, palette = palette_fn), dots),
    quote = TRUE
  )
}

#' @rdname scale-daltonr
#' @export
scale_colour_daltonr <- function(..., alpha = NULL, begin = 0, end = 1,
                                 direction = 1, palette = "batlow", midpoint = NA,
                                 bounds = c("data", "pretty")) {
  crameri_scale_continuous(
    "colour", palette, alpha, begin, end, direction, midpoint,
    bounds = bounds, ...
  )
}

#' @rdname scale-daltonr
#' @export
scale_fill_daltonr <- function(..., alpha = NULL, begin = 0, end = 1,
                               direction = 1, palette = "batlow", midpoint = NA,
                               bounds = c("data", "pretty")) {
  crameri_scale_continuous(
    "fill", palette, alpha, begin, end, direction, midpoint,
    bounds = bounds, ...
  )
}

#' @rdname scale-daltonr
#' @export
scale_colour_daltonr_d <- function(..., alpha = 1, begin = 0, end = 1,
                                   direction = 1, palette = "batlow") {
  crameri_scale_discrete("colour", palette, alpha, begin, end, direction, ...)
}

#' @rdname scale-daltonr
#' @export
scale_fill_daltonr_d <- function(..., alpha = 1, begin = 0, end = 1,
                                 direction = 1, palette = "batlow") {
  crameri_scale_discrete("fill", palette, alpha, begin, end, direction, ...)
}

#' Petroff colour scales. 
#'
#' Discrete colour scales using the Petroff colour palettes [petroff6], [petroff8],
#' and [petroff10]. With `palette = NULL` (the default) the scale picks the
#' palette automatically from the number of groups: up to 6, 7-8, or 9-10. More than 10
#' groups recycle `petroff10` with a warning.
#'
#' @param palette One of `"petroff6"`, `"petroff8"`, `"petroff10"`, or `NULL` to
#'   auto-select based on number of groups.
#' @param direction Either `1` or `-1` to reverse the colour order.
#' @param quiet If `TRUE`, suppress the recycling warning for >10 groups.
#' @param ... Other arguments passed to [ggplot2::discrete_scale()].
#'
#' @return A ggplot2 `Scale` object.
#'
#' @name scale-petroff
#' @rdname scale-petroff
#'
#' @examples
#' ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, hwy, colour = class)) +
#'   ggplot2::geom_point(size = 2) +
#'   scale_colour_petroff()
NULL

#' @importFrom utils head
petroff_palette <- function(n, palette = NULL, quiet = FALSE) {
  if (is.null(palette)) {
    if (n <= 6) {
      palette <- "petroff6"
    } else if (n <= 8) {
      palette <- "petroff8"
    } else if (n <= 10) {
      palette <- "petroff10"
    } else {
      if (!isTRUE(quiet)) {
        warning(
          "More than 10 groups: recycling the 10-colour Petroff palette. ",
          "Colour alone may not distinguish every group; consider faceting.",
          call. = FALSE
        )
      }
      return(rep(PETROFF[["petroff10"]], length.out = n))
    }
  } else {
    palette <- match.arg(palette, c("petroff6", "petroff8", "petroff10"))
    if (n > length(PETROFF[[palette]])) {
      stop(
        sprintf(
          "%s only contains %d colours, but %d groups were requested.",
          palette, length(PETROFF[[palette]]), n
        ),
        call. = FALSE
      )
    }
  }
  head(PETROFF[[palette]], n)
}

petroff_scale <- function(aesthetics, palette, direction, quiet = FALSE, ...) {
  if (abs(direction) != 1) {
    stop("`direction` must be 1 or -1", call. = FALSE)
  }
  dots <- list(...)
  if (is.null(dots$na.value)) {
    dots$na.value <- getOption("daltonr.na.value", "grey80")
  }
  args <- c(
    list(
      aesthetics = aesthetics,
      palette = function(n) {
        cols <- petroff_palette(n, palette, quiet = quiet)
        if (direction == -1) rev(cols) else cols
      }
    ),
    dots
  )
  do.call(ggplot2::discrete_scale, args, quote = TRUE)
}

#' @rdname scale-petroff
#' @export
scale_colour_petroff <- function(..., palette = NULL, direction = 1,
                                 quiet = FALSE) {
  petroff_scale("colour", palette, direction, quiet = quiet, ...)
}

#' @rdname scale-petroff
#' @export
scale_fill_petroff <- function(..., palette = NULL, direction = 1,
                               quiet = FALSE) {
  petroff_scale("fill", palette, direction, quiet = quiet, ...)
}
