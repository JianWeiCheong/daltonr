#' Linetypes for discrete group mappings
#'
#' Ten linetypes used by [theme_daltonr()] automatically.
#'
#' @format A character vector of linetype specifications (names like
#'   `"solid"` or hex strings like `"F8"`).
#' @export
daltonr_linetypes <- c(
  "solid", "dotted", "dashed", "longdash", "dotdash", "twodash",
  "F8", "11", "F282", "12223242"
)

# Recycle with a warning past 10 lines
daltonr_linetype_pal <- function(n) {
  if (n > length(daltonr_linetypes)) {
    warning(
      "More than ", length(daltonr_linetypes), " groups: recycling the ",
      "daltonr linetypes. Linetype alone may not distinguish every group; ",
      "consider faceting.",
      call. = FALSE
    )
    return(rep(daltonr_linetypes, length.out = n))
  }
  utils::head(daltonr_linetypes, n)
}

is_discrete_value <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

layer_colour_mapping <- function(plot, layer) {
  mapping <- layer$mapping$colour
  if (is.null(mapping) && isTRUE(layer$inherit.aes)) {
    mapping <- plot$mapping$colour
  }
  mapping
}

has_linetype_scale <- function(plot) {
  any(vapply(
    plot$scales$scales,
    function(scale) "linetype" %in% scale$aesthetics,
    logical(1)
  ))
}

# Auto add linetype to the discrete colour variable on line layers
# a daltonr_linetypes scale is added if the plot has none yet
add_auto_linetype <- function(plot) {
  added <- FALSE
  for (i in seq_along(plot$layers)) {
    layer <- plot$layers[[i]]
    if (!inherits(layer$geom, "GeomLine")) {
      next
    }
    if (!is.null(layer$mapping$linetype) ||
        !is.null(layer$aes_params$linetype) ||
        (isTRUE(layer$inherit.aes) && !is.null(plot$mapping$linetype))) {
      next
    }
    colour <- layer_colour_mapping(plot, layer)
    if (is.null(colour)) {
      next
    }
    data <- if (inherits(layer$data, "waiver")) plot$data else layer$data
    if (is.null(data) || inherits(data, "waiver")) {
      next
    }
    value <- tryCatch(rlang::eval_tidy(colour, data = data), error = function(e) NULL)
    if (is.null(value) || !is_discrete_value(value)) {
      next
    }
    if (is.null(layer$mapping)) {
      layer$mapping <- ggplot2::aes()
    }
    layer$mapping$linetype <- colour
    plot$layers[[i]] <- layer
    added <- TRUE
  }
  if (added && !has_linetype_scale(plot)) {
    plot <- plot + ggplot2::discrete_scale(
      aesthetics = "linetype",
      palette = daltonr_linetype_pal
    )
  }
  plot
}
