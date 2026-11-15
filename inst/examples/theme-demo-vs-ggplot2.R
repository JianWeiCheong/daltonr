library(ggplot2)
library(daltonr)
source("inst/examples/demo-utils.R")

options(
  ggplot2.discrete.colour = NULL, ggplot2.discrete.fill = NULL,
  ggplot2.continuous.colour = NULL, ggplot2.continuous.fill = NULL,
  daltonr.na.value = NULL, daltonr.colourbar = NULL,
  daltonr.colourbar_title = NULL
)
theme_set(theme_grey())
stopifnot(
  is.null(getOption("ggplot2.discrete.colour")),
  is.null(getOption("ggplot2.discrete.fill")),
  is.null(getOption("ggplot2.continuous.colour")),
  is.null(getOption("ggplot2.continuous.fill")),
  identical(class(theme_get()), class(theme_grey()))
)

save_two_grobs <- function(left_grob, right, out, width = 13, height = 5.5) {
  png(out, width = width, height = height, units = "in", res = 150)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
  grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  grid::grid.draw(left_grob)
  grid::popViewport()
  grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
  grid::grid.draw(ggplotGrob(right))
  grid::popViewport(2)
  dev.off()
  message("Wrote ", normalizePath(out, mustWork = FALSE))
}

n_series <- length(daltonr_linetypes)
line_df <- data.frame(
  x = rep(seq(0, 10, length.out = 200), n_series),
  series = factor(
    rep(paste("Series", seq_len(n_series)), each = 200),
    levels = paste("Series", seq_len(n_series))
  )
)
k <- as.integer(factor(line_df$series))
line_df$y <- sin(line_df$x * (0.35 + 0.1 * (k - 1))) + 2.2 * (k - 1)

volcano_df <- data.frame(
  x = rep(seq_len(nrow(datasets::volcano)), times = ncol(datasets::volcano)),
  y = rep(seq_len(ncol(datasets::volcano)), each = nrow(datasets::volcano)),
  height = as.vector(datasets::volcano)
)

base_lines <- ggplot(line_df, aes(x, y, colour = series)) +
  geom_line(linewidth = 0.9) +
  labs(title = "Base ggplot2 (theme_grey)", x = "x", y = "y", colour = "Series") +
  theme_grey()

base_raster <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_raster() +
  geom_contour(
    aes(x = waiting, y = eruptions, z = density),
    inherit.aes = FALSE,
    colour = "white",
    alpha = 0.35,
    linewidth = 0.25
  ) +
  labs(
    title = "Base ggplot2 (theme_grey)",
    x = "Waiting time to next eruption (min)",
    y = "Eruption duration (min)",
    fill = "Density"
  ) +
  theme_grey()

base_contours <- ggplot(volcano_df, aes(x, y, z = height)) +
  geom_contour_filled() +
  geom_contour(
    aes(x = x, y = y, z = height),
    inherit.aes = FALSE,
    colour = "white",
    alpha = 0.35,
    linewidth = 0.25
  ) +
  labs(
    title = "Base ggplot2 (theme_grey)",
    x = "Grid row", y = "Grid column", fill = "Elevation (m)"
  ) +
  theme_grey()

base_facets <- ggplot(mpg, aes(displ, hwy, colour = drv)) +
  geom_point(size = 2, alpha = 0.9) +
  facet_wrap(~ class, ncol = 4) +
  labs(
    title = "Base ggplot2 (theme_grey)",
    x = "Engine displacement (L)", y = "Highway miles per gallon",
    colour = "Drive"
  ) +
  theme_grey() +
  theme(aspect.ratio = 1) # square panels

base_grobs <- list(
  lines = ggplotGrob(base_lines),
  raster = ggplotGrob(base_raster),
  contours = ggplotGrob(base_contours),
  facets = ggplotGrob(base_facets)
)

d_lines <- ggplot(line_df, aes(x, y, colour = series)) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "theme_daltonr()",
    x = "x", y = "y", colour = "Series", linetype = "Series"
  ) +
  theme_daltonr()

d_raster <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_raster() +
  geom_contour(
    aes(x = waiting, y = eruptions, z = density),
    inherit.aes = FALSE,
    colour = "white",
    alpha = 0.35,
    linewidth = 0.25
  ) +
  labs(
    title = "theme_daltonr()",
    x = "Waiting time to next eruption (min)",
    y = "Eruption duration (min)",
    fill = "Density"
  ) +
  theme_daltonr()

d_contours <- ggplot(volcano_df, aes(x, y, z = height)) +
  geom_contour_filled() +
  geom_contour(
    aes(x = x, y = y, z = height),
    inherit.aes = FALSE,
    colour = "white",
    alpha = 0.35,
    linewidth = 0.25
  ) +
  scale_fill_daltonr_d(
    palette = "lajolla",
    name = "Elevation (m)",
    guide = guide_daltonr_colourbar("discrete")
  ) +
  labs(
    title = "theme_daltonr()",
    x = "Grid row", y = "Grid column"
  ) +
  theme_daltonr()

d_facets <- ggplot(mpg, aes(displ, hwy, colour = drv)) +
  geom_point(size = 2, alpha = 0.9) +
  facet_wrap(~ class, ncol = 4) +
  labs(
    title = "theme_daltonr()",
    x = "Engine displacement (L)", y = "Highway miles per gallon",
    colour = "Drive"
  ) +
  theme_daltonr() +
  theme(aspect.ratio = 1) # square panels

save_two_grobs(base_grobs$lines, d_lines,
  demo_out("inst/examples/theme-demo-vs-ggplot2-lines.png"))
save_two_grobs(base_grobs$raster, d_raster,
  demo_out("inst/examples/theme-demo-vs-ggplot2-raster.png"))
save_two_grobs(base_grobs$contours, d_contours,
  demo_out("inst/examples/theme-demo-vs-ggplot2-contours.png"))
save_two_grobs(base_grobs$facets, d_facets,
  demo_out("inst/examples/theme-demo-vs-ggplot2-facets.png"), height = 6)
