#' @title Get legend from ggplot2 ver 3.5
#'
#' @description Calculates Hs using default chemical properties at the
#'   subsurface system temperature. Code is from https://github.com/wilkelab/cowplot/issues/202
#'
#' @param plot ggplot2 object to have legend extracted
#'
#' @param legend_number which plot to extract
#'
#' @export

getLegend <- function(plot, legend_number = 1) {
  # find all legend candidates
  legends <- get_plot_component(plot, "guide-box", return_all = TRUE)
  # find non-zero legends
  idx <- which(vapply(legends, \(x) !inherits(x, "zeroGrob"), TRUE))
  # return either the chosen or the first non-zero legend if it exists,
  # and otherwise the first element (which will be a zeroGrob)
  if (length(idx) >= legend_number) {
    return(legends[[idx[legend_number]]])
  } else if (length(idx) >= 0) {
    return(legends[[idx[1]]])
  } else {
    return(legends[[1]])
  }
}
