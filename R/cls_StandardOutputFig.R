#' @title Parent class for creating standard output figures
#'
#' @description A parent R6 Class used to build standard vapintr output figures
#'   at recommended sizes. It has three properties: one for the image to be
#'   rendered (fig_image), one for the recommended width (fig_width), and one
#'   for the recommended height (fig_height). The object's
#'   renderImageInNewWindow() function will render the image saved in the
#'   fig_image property in a window at the dimensions specified in the object's
#'   fig_width and fig_height properties.
#'
#' @examples
#' StandardOutputFig$new()
#'
#' @export

StandardOutputFig <- R6::R6Class(
  classname = "StandardOutputFig",
  public = list(

    #' @field fig_image The figure image to be rendered
    #' @field fig_width The recommended width of the figure, in inches
    #' @field fig_height The recommended height of the figure, in inches

    fig_image = NULL,
    fig_width = NULL,
    fig_height = NULL,

    #' @description Helper function for creating images in a new window of the
    #'   recommended size in RStudio
    renderImageInNewWindow = function(){

      plot.new()
      dev.new(width = self$fig_width, height = self$fig_height, unit = "in", noRStudioGD = TRUE)
      plot(self$fig_image)
    }
  )
)


