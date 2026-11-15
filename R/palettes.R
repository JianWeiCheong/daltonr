#' Sample colours from scientific colour maps
#'
#' `crameri()` samples Fabio Crameri's scientific colour maps plus viridis and
#' cividis. `crameri_names()` lists everything available.
#'
#' The Petroff colour palettes (Petroff 2021) are available as [petroff6], [petroff8],
#' and [petroff10].
#'
#' @param n Number of colours.
#' @param palette Name of the colour map. See [crameri_names()].
#' @param alpha Opacity of colours. If specified, rgba values are generated.
#' @param begin,end Interval within the colour map to sample from, in `[0, 1]`.
#' @param direction Either `1` or `-1`. If `-1`, the order is reversed.
#'
#' @return A character vector of hex-encoded rgb(a) colour values.
#'
#' @references
#' Crameri, F. (2018). Geodynamic diagnostics, scientific visualisation and
#' StagLab 3.0. Geosci. Model Dev., 11, 2541-2562.
#' \doi{10.5194/gmd-11-2541-2018}
#'
#' Crameri, F., Shephard, G.E. & Heron, P.J. (2020). The misuse of colour in
#' science communication. Nat. Commun. 11, 5444. \doi{10.1038/s41467-020-19122-7}
#'
#' Petroff, M.A. (2021). Accessible Color Sequences for Data Visualization.
#' arXiv:2107.02270. \doi{10.48550/arXiv.2107.02270}
#'
#' @export
#' @examples
#'
#' # Use the default colour map (batlow)
#' crameri(15)
#'
#' # Flip the direction
#' crameri(15, direction = -1)
#'
#' # Take a subset of a colour map
#' crameri(15, begin = 0.3, end = 0.6, palette = "berlin")
#'
#' # The viridis and cividis colour maps
#' crameri(15, palette = "viridis")
#' crameri(15, palette = "cividis", direction = -1)
crameri <- function(n, palette = "batlow", alpha = NULL, begin = 0, end = 1, direction = 1) {
  if (begin < 0 || begin > 1 || end < 0 || end > 1) {
    stop("`begin` and `end` must be in [0, 1]", call. = FALSE)
  }
  if (abs(direction) != 1) {
    stop("`direction` must be 1 or -1", call. = FALSE)
  }
  if (!is.null(alpha) && (alpha < 0 || alpha > 1)) {
    stop("`alpha` must be in [0, 1]", call. = FALSE)
  }
  if (n <= 0) {
    return(character(0))
  }
  if (direction == -1) {
    tmp <- begin
    begin <- end
    end <- tmp
  }

  map_cols <- crameri_palettes[[match.arg(palette, crameri_names())]]
  fn_cols <- grDevices::colorRamp(map_cols, space = "Lab", interpolate = "spline")
  cols <- fn_cols(seq(begin, end, length.out = n))
  grDevices::rgb(
    cols[, 1], cols[, 2], cols[, 3],
    alpha = if (is.null(alpha)) NULL else alpha * 255,
    maxColorValue = 255
  )
}

#' @rdname crameri
#' @export
crameri_names <- function() {
  names(crameri_palettes)
}

#' Petroff colour palettes for categorical data
#'
#' Six-, eight-, and ten-colour palettes from Petroff (2021),
#' \doi{10.48550/arXiv.2107.02270}.
#'
#' @format Character vectors of hex-encoded sRGB colour values.
#' @references Petroff, M.A. (2021). *Accessible Color Sequences for Data Visualization*.
#' arXiv:2107.02270. \doi{10.48550/arXiv.2107.02270}
#' @name petroff
#' @export
petroff6 <- c(
  "#5790FC", "#F89C20", "#E42536", "#964A8B", "#9C9CA1", "#7A21DD"
)

#' @rdname petroff
#' @export
petroff8 <- c(
  "#1845FB", "#FF5E02", "#C91F16", "#C849A9", "#ADAD7D", "#86C8DD", "#578DFF", "#656364"
)

#' @rdname petroff
#' @export
petroff10 <- c(
  "#3F90DA", "#FFA90E", "#BD1F01", "#94A4A2", "#832DB6", "#A96B59",
  "#E76300", "#B9AC70", "#717581", "#92DADD"
)

PETROFF <- list(petroff6 = petroff6, petroff8 = petroff8, petroff10 = petroff10)
