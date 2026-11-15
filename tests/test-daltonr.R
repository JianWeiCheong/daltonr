library(daltonr)
library(ggplot2)

# palette constructors

stopifnot(
  length(crameri(10)) == 10,
  length(crameri(5, palette = "viridis")) == 5,
  length(crameri(5, palette = "cividis")) == 5,
  all(grepl("^#[0-9A-F]{6}([0-9A-F]{2})?$", crameri(10))),
  identical(crameri(3), rev(crameri(3, direction = -1))),
  identical(crameri(0), character(0)),
  length(crameri_names()) == 41,
  all(c("viridis", "cividis") %in% crameri_names()),
  all(c("batlow", "roma", "vik", "brocO", "batlowK", "devon") %in% crameri_names())
)

# colour maps stored as plain hex vectors in R/colormaps.R

stopifnot(
  all(vapply(daltonr:::crameri_palettes, function(m) {
    is.character(m) && length(m) == 256 && all(grepl("^#[0-9A-F]{6}$", m))
  }, logical(1)))
)

# petroff vectors

stopifnot(
  length(petroff6) == 6,
  length(petroff8) == 8,
  length(petroff10) == 10,
  identical(petroff6[1], "#5790FC"),
  identical(petroff8[1], "#1845FB"),
  identical(petroff10[1], "#3F90DA")
)

# daltonr linetypes

stopifnot(
  length(daltonr_linetypes) == 10,
  identical(daltonr_linetypes[1], "solid"),
  length(unique(daltonr_linetypes)) == 10
)

# continuous Crameri scale on geom_raster

p_raster <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_raster() +
  scale_fill_daltonr(palette = "batlow")
b <- ggplot_build(p_raster)
stopifnot(nrow(b$data[[1]]) > 100)
stopifnot(inherits(b$plot$guides$get_guide("fill"), "GuideColourbar"))

# Colourbar labels both bounds; ticks on the label side only
sc <- b$plot$scales$get_scales("fill")
brks <- sc$get_breaks()
stopifnot(isTRUE(all.equal(brks[c(1, length(brks))], sc$get_limits())))
tick_length <- b$plot$guides$get_guide("fill")$params$theme$legend.ticks.length
stopifnot(length(tick_length) == 2, as.numeric(tick_length[1]) < 0, as.numeric(tick_length[2]) == 0)

# discrete Crameri scale on geom_contour_filled, with the stepped colourbar

p_contour_d <- ggplot(faithfuld, aes(waiting, eruptions, z = density)) +
  geom_contour_filled() +
  scale_fill_daltonr_d(palette = "batlow", guide = guide_daltonr_colourbar("discrete"))
b <- ggplot_build(p_contour_d)
stopifnot(nrow(b$data[[1]]) > 0)
guide <- b$plot$guides$get_guide("fill")
stopifnot(inherits(guide, "GuideColoursteps"))
stopifnot(isTRUE(guide$params$show.limits))

# categorical discrete scales keep a normal legend

b <- ggplot_build(
  ggplot(mpg, aes(class, fill = class)) +
    geom_bar() +
    scale_fill_daltonr_d(palette = "batlow")
)
stopifnot(inherits(b$plot$guides$get_guide("fill"), "GuideLegend"))

# theme drops axis padding on raster plots: they fill the frame

b <- ggplot_build(
  ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
    geom_raster() +
    theme_daltonr()
)
pp <- b$layout$panel_params[[1]]
d <- b$data[[1]]
stopifnot(isFALSE(b$plot$coordinates$expand))
stopifnot(isTRUE(all.equal(pp$x.range, range(c(d$xmin, d$xmax)))))
stopifnot(isTRUE(all.equal(pp$y.range, range(c(d$ymin, d$ymax)))))

# ...and on filled-contour plots
b <- ggplot_build(
  ggplot(faithfuld, aes(waiting, eruptions, z = density)) +
    geom_contour_filled() +
    theme_daltonr()
)
stopifnot(isFALSE(b$plot$coordinates$expand))

# ...but not on plain scatter plots
b <- ggplot_build(
  ggplot(mpg, aes(displ, hwy)) + geom_point() + theme_daltonr()
)
stopifnot(isTRUE(b$plot$coordinates$expand))

# auto_coord = FALSE switches it off
b <- ggplot_build(
  ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
    geom_raster() +
    theme_daltonr(auto_coord = FALSE)
)
pp <- b$layout$panel_params[[1]]
d <- b$data[[1]]
stopifnot(isTRUE(b$plot$coordinates$expand))
stopifnot(!isTRUE(all.equal(pp$x.range, range(c(d$xmin, d$xmax)))))

# an explicit coord is left alone
b <- ggplot_build(
  ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
    geom_raster() +
    coord_cartesian(expand = TRUE) +
    theme_daltonr()
)
stopifnot(isTRUE(b$plot$coordinates$expand))

# viridis and cividis scales

for (palette in c("viridis", "cividis")) {
  b <- ggplot_build(
    ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
      geom_raster() +
      scale_fill_daltonr(palette = palette)
  )
  stopifnot(nrow(b$data[[1]]) > 100)
}

# diverging map with midpoint

df_z <- data.frame(x = 1:10, y = 1:10, z = c(-2, -1, 1, 2, 0, -1, 1, -2, 2, 0))
b <- ggplot_build(
  ggplot(df_z, aes(x, y, fill = z)) +
    geom_raster() +
    scale_fill_daltonr(palette = "broc", midpoint = 0)
)
stopifnot(nrow(b$data[[1]]) > 0)

# theme + automatic scale selection

invisible(theme_daltonr()) # sets the ggplot2 default scale options

df_k <- function(k) data.frame(x = seq_len(k), g = factor(seq_len(k)))

get_fills <- function(k) {
  unique(as.character(ggplot_build(
    ggplot(df_k(k), aes(x, fill = g)) + geom_bar()
  )$data[[1]]$fill))
}

# <= 6 groups -> petroff6; 7-8 -> petroff8; 9-10 -> petroff10
stopifnot(identical(get_fills(3), head(petroff6, 3)))
stopifnot(identical(get_fills(6), petroff6))
stopifnot(identical(get_fills(7), head(petroff8, 7)))
stopifnot(identical(get_fills(9), head(petroff10, 9)))

# > 10 groups -> recycle petroff10 with a warning
warns <- character()
fills12 <- withCallingHandlers(
  get_fills(12),
  warning = function(w) {
    warns <<- c(warns, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
stopifnot(any(grepl("recycling", warns)))
stopifnot(setequal(fills12, petroff10))

# continuous default -> crameri "batlow"
b <- ggplot_build(
  ggplot(faithfuld, aes(waiting, eruptions, fill = density)) + geom_raster()
)
stopifnot(any(unique(as.character(b$data[[1]]$fill)) %in% crameri(256, palette = "batlow")))

# explicit petroff scales

b <- ggplot_build(
  ggplot(df_k(7), aes(x, fill = g)) + geom_bar() + scale_fill_petroff()
)
stopifnot(identical(
  unique(as.character(b$data[[1]]$fill)),
  head(petroff8, 7)
))

b <- ggplot_build(
  ggplot(df_k(4), aes(x, fill = g)) + geom_bar() + scale_fill_petroff(palette = "petroff10")
)
stopifnot(identical(
  unique(as.character(b$data[[1]]$fill)),
  head(petroff10, 4)
))

# forced palette with too many groups errors
err <- tryCatch(
  ggplot_build(
    ggplot(df_k(8), aes(x, fill = g)) + geom_bar() + scale_fill_petroff(palette = "petroff6")
  ),
  error = function(e) e
)
stopifnot(inherits(err, "error"))

# invalid arguments error
stopifnot(inherits(tryCatch(crameri(5, begin = 2), error = function(e) e), "error"))
stopifnot(inherits(tryCatch(crameri(5, direction = 0), error = function(e) e), "error"))
stopifnot(inherits(tryCatch(crameri(5, palette = "nope"), error = function(e) e), "error"))

# auto linetypes use daltonr_linetypes

b <- ggplot_build(
  ggplot(economics_long, aes(date, value01, colour = variable)) +
    geom_line() +
    theme_daltonr()
)
stopifnot(
  length(unique(b$data[[1]]$linetype)) == length(unique(economics_long$variable))
)
stopifnot(setequal(
  unique(as.character(b$data[[1]]$linetype)),
  head(daltonr_linetypes, length(unique(economics_long$variable)))
))

# explicit linetype mappings are left alone
b <- ggplot_build(
  ggplot(economics_long, aes(date, value01, colour = variable, linetype = variable)) +
    geom_line() +
    theme_daltonr()
)
stopifnot(
  length(unique(b$data[[1]]$linetype)) == length(unique(economics_long$variable))
)

# a continuous colour mapping is not turned into a linetype mapping
b <- tryCatch(
  ggplot_build(
    ggplot(faithfuld, aes(waiting, eruptions, colour = density)) +
      geom_line() +
      theme_daltonr()
  ),
  error = function(e) e
)
stopifnot(!inherits(b, "error"))

# a constant linetype is respected
b <- ggplot_build(
  ggplot(mpg, aes(displ, hwy, colour = class)) +
    geom_line(linetype = "dashed") +
    theme_daltonr()
)
stopifnot(all(b$data[[1]]$linetype == "dashed"))

# auto_linetype = FALSE switches it off
b <- ggplot_build(
  ggplot(economics_long, aes(date, value01, colour = variable)) +
    geom_line() +
    theme_daltonr(auto_linetype = FALSE)
)
stopifnot(length(unique(b$data[[1]]$linetype)) == 1L)

# theme options

th <- theme_daltonr(
  grid = "none", border = FALSE, axis_ticks = "none", title_align = "left"
)
stopifnot(
  inherits(th$panel.grid.major, "element_blank"),
  inherits(th$panel.grid.minor, "element_blank"),
  inherits(th$panel.border, "element_blank"),
  inherits(th$axis.ticks, "element_blank"),
  identical(th$plot.title$hjust, 0)
)

# axis_ticks: outside (default, positive), inside (negative), none (blank)

th_out <- theme_daltonr()
stopifnot(
  inherits(th_out$axis.ticks, "element_line"),
  as.numeric(th_out$axis.ticks.length) > 0
)
th_in <- theme_daltonr(axis_ticks = "inside")
stopifnot(
  inherits(th_in$axis.ticks, "element_line"),
  as.numeric(th_in$axis.ticks.length) < 0
)
b <- ggplot_build(
  ggplot(mpg, aes(displ, hwy)) + geom_point() + theme_daltonr(axis_ticks = "inside")
)
stopifnot(nrow(b$data[[1]]) > 0)

# colourbar options and helpers

stopifnot(inherits(guide_daltonr_colourbar(), "GuideColourbar"))
stopifnot(inherits(guide_daltonr_colourbar("discrete"), "GuideColoursteps"))

b <- ggplot_build(
  ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
    geom_raster() +
    scale_fill_daltonr(bounds = "pretty")
)
fill_limits <- b$plot$scales$get_scales("fill")$get_limits()
stopifnot(isTRUE(all.equal(fill_limits, c(0, 0.04))))

# colourbar = "fixed" / colourbar_title = "top" via the theme options
invisible(theme_daltonr(colourbar = "fixed", colourbar_title = "top"))
b <- ggplot_build(
  ggplot(faithfuld, aes(waiting, eruptions, fill = density)) + geom_raster()
)
g <- b$plot$guides$get_guide("fill")
stopifnot(
  identical(g$params$theme$legend.title.position, "top"),
  identical(as.character(g$params$theme$legend.key.height), "5lines")
)
options(daltonr.colourbar = NULL, daltonr.colourbar_title = NULL)

# missing values and quiet Petroff recycling

df_na <- data.frame(x = 1:3, y = 1:3, z = c(1, NA, 3))
b <- ggplot_build(
  ggplot(df_na, aes(x, y, fill = z)) +
    geom_point(shape = 21, size = 3) +
    scale_fill_daltonr()
)
stopifnot("grey80" %in% as.character(b$data[[1]]$fill))

warns <- character()
invisible(withCallingHandlers(
  ggplot_build(
    ggplot(df_k(12), aes(x, fill = g)) +
      geom_bar() +
      scale_fill_petroff(quiet = TRUE)
  ),
  warning = function(w) {
    warns <<- c(warns, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
))
stopifnot(length(warns) == 0L)

# theme_daltonr() sets the session default scale options

old_fill <- getOption("ggplot2.discrete.fill")
invisible(theme_daltonr())
stopifnot(!is.null(getOption("ggplot2.discrete.fill")))
options(ggplot2.discrete.fill = old_fill)

# the theme uses bottom/left axes only

b <- ggplot_build(
  ggplot(mpg, aes(displ, hwy)) + geom_point() + theme_daltonr()
)
has_secondary <- function(axis) {
  sec <- b$plot$scales$get_scales(axis)$secondary.axis
  !is.null(sec) && !inherits(sec, "waiver")
}
stopifnot(!has_secondary("x"), !has_secondary("y"))

# reset the session options

options(
  ggplot2.discrete.colour = NULL,
  ggplot2.discrete.fill = NULL,
  ggplot2.continuous.colour = NULL,
  ggplot2.continuous.fill = NULL
)

cat("All daltonr smoke tests passed.\n")
