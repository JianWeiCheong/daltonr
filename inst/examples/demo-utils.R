# Demo helpers.

demo_out <- function(default) {
  out <- Sys.getenv("DALTONR_DEMO_OUT", unset = default)
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  out
}

save_two_panel <- function(left, right, out, width = 13, height = 5.5) {
  png(out, width = width, height = height, units = "in", res = 150)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
  grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  grid::grid.draw(ggplotGrob(left))
  grid::popViewport()
  grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
  grid::grid.draw(ggplotGrob(right))
  grid::popViewport(2)
  dev.off()
  message("Wrote ", normalizePath(out, mustWork = FALSE))
}
