#' @title Parent class for measured and modeled data figures
#'
#' @description An R6 parent class used to create `ModDataFig` and `MeasDataFig`
#'   objects. The class contains common properties and methods needed to
#'   generate figures with the two child classes. See the following vignette
#'   for more information.
#'
#'   `vignette("ModelResultFigures", package = "vapintr")`
#'
#' @examples
#' #Run the JEM using the example stochastic data
#' jem_results <- runTemplateData(stoc_jem_sim_example_data, "Trichloroethylene")
#'
#' #Create a new output figure from the JEM Monte Carlo results
#' model_result_figure <- ModelResultFig$new(jem_results[[1]]$JEMResults,
#' "Trichloroethylene", "79-01-6", "ug/m3", "groundwater")
#'
#' #The object stored in the model_result_figure$mod_data_fig attribute is a
#' #ModDataFig object, which uses the ConcDataFig as its parent class
#' model_result_figure$mod_data_fig
#'
#' @export

ConcDataFig <- R6::R6Class(
  classname = "ConcDataFig",
  public = list(

    #' @field data_plot Primary ggplot of concentration data associated with the object
    #' @field title Figure title
    #' @field x_title Title label for the x-axis
    #' @field x_min Minimum value of data on the x-axis
    #' @field x_max Maximum value of data on the x-axis
    #' @field y_title Title label for the y-axis
    #' @field y_min Minimum value of data on the y-axis
    #' @field y_max Maximum value of data on the y-axis
    #' @field label_units Units formatted to appear in the y-axis concentration label
    #' @field factor_indices Indices associated with the standard data type factors used in the figure
    #' @field factor_names Names associated with the standard data type factors used in the figure
    #' @field factor_line_formats Line formats associated with the standard data type factors used in the figure
    #' @field factor_shape_formats Shape formats associated with the standard data type factors used in the figure
    #' @field factor_color_formats Color formats associated with the standard data type factors used in the figure
    #' @field format_constants Formatting constants for generating figures

    data_plot = NULL,
    title = NULL,
    x_title = NULL,
    x_min = NULL,
    x_max = NULL,
    y_title = NULL,
    y_min = NULL,
    y_max = NULL,
    label_units = NULL,
    factor_indices = NULL,
    factor_names = NULL,
    factor_line_formats = NULL,
    factor_shape_formats = NULL,
    factor_color_formats = NULL,
    format_constants = NULL,

    #' @description Function to create a new `ConcDataFig` object
    initialize = function(){

      self$data_plot <- NULL
      self$format_constants <- self$getStandardFormatConstants()
      self$factor_indices <- self$getStandardFactorIndices()
      self$factor_names <- self$getStandardFactorNames()
      self$factor_line_formats <- self$getStandardFactorLineFormats()
      self$factor_shape_formats <- self$getStandardFactorShapeFormats()
      self$factor_color_formats <- self$getStandardFactorColorFormats()
      self$title <- ""
      self$x_title <- ""
      self$x_min <- Inf
      self$x_max <- -Inf
      self$y_title <- ""
      self$y_min <- Inf
      self$y_max <- -Inf
      self$label_units <- ""

    },
    #' @description Gets a named list of standard figure format constants
    getStandardFormatConstants = function(){

      #Set figure constants dictionary
      constants <- list(
        PlotTitleTextSize = 9,
        PlotLegendTextSize = 8,
        AxisTextSize = 9,
        AxisTitleSize = 9,
        AnnotationTextSize = 8/.pt, #need to divide by the global variable .pt so that the annotation text scales correctly with the other variables
        CaptionTextSize = 8,
        LegendDirection = "vertical",
        LegendSpacingCM = -1,
        SymbolSize = 3,
        StrokeSize = 1,
        LineWidth = 0.75,
        MarginSize = 5,
        ExpandedMarginSize = 15
      )

      return(constants)
    },
    #' @description Gets a named list of indices associated with the figure factors
    getStandardFactorIndices = function(){

      factor_indices <- list(

        MeasuredIndoorAirDetect = 1,
        MeasuredOutdoorAirDetect = 2,
        MeasuredIndoorAirNondetect = 3,
        MeasuredOutdoorAirNondetect = 4,
        ReferenceConcentration1 = 5,
        ReferenceConcentration2 = 6,
        ReferenceConcentration3 = 7,
        ReferenceConcentration4 = 8,
        ModeledIndoorAirViolinPlotData = 9,
        ModeledIndoorAir50thPercentile = 10,
        ModeledIndoorAir95thPercentile = 11

      )

      return(factor_indices)
    },
    #' @description Gets a vector of standard line formats associated with the figure factors
    getStandardFactorLineFormats = function()
    {
      factor_indices <- self$getStandardFactorIndices()

      factor_line_formats <- c()

      factor_line_formats[factor_indices$MeasuredIndoorAirDetect] <- "blank"
      factor_line_formats[factor_indices$MeasuredOutdoorAirDetect] <- "blank"
      factor_line_formats[factor_indices$MeasuredIndoorAirNondetect] <- "blank"
      factor_line_formats[factor_indices$MeasuredOutdoorAirNondetect] <- "blank"
      factor_line_formats[factor_indices$ReferenceConcentration1] <- "dotdash"
      factor_line_formats[factor_indices$ReferenceConcentration2] <- "dashed"
      factor_line_formats[factor_indices$ReferenceConcentration3] <- "twodash"
      factor_line_formats[factor_indices$ReferenceConcentration4] <- "longdash"
      factor_line_formats[factor_indices$ModeledIndoorAirViolinPlotData] <- "solid"
      factor_line_formats[factor_indices$ModeledIndoorAir50thPercentile] <- "solid"
      factor_line_formats[factor_indices$ModeledIndoorAir95thPercentile] <- "dotted"

      return(factor_line_formats)

    },
    #' @description Gets a vector of standard shape formats associated with the figure factors
    getStandardFactorShapeFormats = function()
    {
      factor_indices <- self$getStandardFactorIndices()

      factor_shape_formats <- c()

      factor_shape_formats[factor_indices$MeasuredIndoorAirDetect] <- 16
      factor_shape_formats[factor_indices$MeasuredOutdoorAirDetect] <- 15
      factor_shape_formats[factor_indices$MeasuredIndoorAirNondetect] <- 1
      factor_shape_formats[factor_indices$MeasuredOutdoorAirNondetect] <- 0
      factor_shape_formats[factor_indices$ReferenceConcentration1] <- NA
      factor_shape_formats[factor_indices$ReferenceConcentration2] <- NA
      factor_shape_formats[factor_indices$ReferenceConcentration3] <- NA
      factor_shape_formats[factor_indices$ReferenceConcentration4] <- NA
      factor_shape_formats[factor_indices$ModeledIndoorAirViolinPlotData] <- NA
      factor_shape_formats[factor_indices$ModeledIndoorAir50thPercentile] <- NA
      factor_shape_formats[factor_indices$ModeledIndoorAir95thPercentile] <- NA

      return(factor_shape_formats)

    },
    #' @description Gets a vector of names associated with the figure factors
    getStandardFactorNames = function()
    {
      factor_indices <- self$getStandardFactorIndices()

      factor_names <- c()

      factor_names[factor_indices$MeasuredIndoorAirDetect] <- "Measured Indoor Air Detect"
      factor_names[factor_indices$MeasuredOutdoorAirDetect] <- "Measured Outdoor Air Detect"
      factor_names[factor_indices$MeasuredIndoorAirNondetect] <- "Measured Indoor Air Nondetect"
      factor_names[factor_indices$MeasuredOutdoorAirNondetect] <- "Measured Outdoor Air Nondetect"
      factor_names[factor_indices$ReferenceConcentration1] <- "Reference Concentration 1"
      factor_names[factor_indices$ReferenceConcentration2] <- "Reference Concentration 2"
      factor_names[factor_indices$ReferenceConcentration3] <- "Reference Concentration 3"
      factor_names[factor_indices$ReferenceConcentration4] <- "Reference Concentration 4"
      factor_names[factor_indices$ModeledIndoorAirViolinPlotData] <- "Modeled Indoor Air Violin Plot"
      factor_names[factor_indices$ModeledIndoorAir50thPercentile] <- "Modeled Indoor Air 50th Percentile" #"Modeled Indoor Air 50\u1D57\u02B0 Percentile"
      factor_names[factor_indices$ModeledIndoorAir95thPercentile] <- "Modeled Indoor Air 95th Percentile" #"Modeled Indoor Air 95\u1D57\u02B0 Percentile"

      return(factor_names)
    },
    #' @description Gets a vector of standard colors associated with the figure factors
    getStandardFactorColorFormats = function()
    {
      factor_indices <- self$getStandardFactorIndices()

      factor_color_formats <- c()

      factor_color_formats[factor_indices$MeasuredIndoorAirDetect] <- "blue"
      factor_color_formats[factor_indices$MeasuredOutdoorAirDetect] <- "orange"
      factor_color_formats[factor_indices$MeasuredIndoorAirNondetect] <- "blue"
      factor_color_formats[factor_indices$MeasuredOutdoorAirNondetect] <- "orange"
      factor_color_formats[factor_indices$ReferenceConcentration1] <- "black"
      factor_color_formats[factor_indices$ReferenceConcentration2] <- "black"
      factor_color_formats[factor_indices$ReferenceConcentration3] <- "black"
      factor_color_formats[factor_indices$ReferenceConcentration4] <- "black"
      factor_color_formats[factor_indices$ModeledIndoorAirViolinPlotData] <- "black"
      factor_color_formats[factor_indices$ModeledIndoorAir50thPercentile] <- "black"
      factor_color_formats[factor_indices$ModeledIndoorAir95thPercentile] <- "black"

      return(factor_color_formats)
    },
    #' @description Adds labels to the plot title, x-axis, and y-axis
    addPlotLabels = function(){
      self$data_plot <- self$data_plot + labs(title = self$title, x = self$x_title, y = self$y_title)
    }
  ),
  private = list(
    initializeDataPlot = function(){
      #Create standard formatted plot
      data_plot <- ggplot () +
        cowplot::theme_cowplot()+
        cowplot::background_grid() +
        theme(plot.title = element_text(size=self$format_constants$PlotTitleTextSize, face = "bold"),
              axis.text.y = element_text( hjust = 0.95, vjust = 0.2, size = self$format_constants$AxisTextSize),
              axis.title.y = element_text(size = self$format_constants$AxisTitleSize),
              axis.text.x = element_text(size = self$format_constants$AxisTextSize),
              axis.title.x = element_text(size = self$format_constants$AxisTitleSize),
              plot.margin = margin(self$format_constants$MarginSize,
                                   self$format_constants$MarginSize,
                                   self$format_constants$MarginSize,
                                   self$format_constants$MarginSize))

      return(data_plot)
    },
    initializeLegend = function(){
      #Create standard formatted legends
      data_legend <- ggplot() +
        theme(legend.text=element_text(size=self$format_constants$PlotLegendTextSize),
              legend.direction = self$format_constants$LegendDirection,
              legend.spacing.y = unit(self$format_constants$LegendSpacingCM, "cm"),
              legend.key = element_rect(fill = "white"))

      return(data_legend)
    },
    setYAxisScale = function(){

      #Use a log scale if the max concentration is greater than 10x the min concentration
      if (self$y_max > 10 * self$y_min){
        use_log_scale <- TRUE
      } else {
        use_log_scale <- FALSE
      }

      #Get concentration bounds
      if (use_log_scale){
        y_scale_max <- 10^(ceiling(log10(self$y_max)))
        y_scale_min <- 10^(floor(log10(self$y_min)))
        y_axis_breaks <- 10^(log10(y_scale_min):log10(y_scale_max))
      } else {
        max_ceiling <- 10^(ceiling(log10(self$y_max)))

        if (max_ceiling / 5 > self$y_max){
          y_scale_max <- max_ceiling / 5
        } else if (max_ceiling / 2 > self$y_max){
          y_scale_max <- max_ceiling / 2
        } else {
          y_scale_max <- max_ceiling
        }

        y_scale_min <- 0
      }

      if(use_log_scale){
        self$data_plot <- self$data_plot +
          scale_y_continuous(trans='log10',
                             breaks=y_axis_breaks,
                             labels = trans_format("log10", math_format(10^.x)),
                             limits = c(min(y_axis_breaks), max(y_axis_breaks))) +
          annotation_logticks(sides = "l")
      } else {
        self$data_plot <- self$data_plot +
          scale_y_continuous(limits = c(y_scale_min, y_scale_max))
      }

    }
  )
)


