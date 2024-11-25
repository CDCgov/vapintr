#' @title Class for data in measured air concentration scatter plots
#'
#' @description An R6 Class used to build scatter plots of measured indoor and
#'   outdoor air concentration data with lines for reference concentrations.
#'   Most functions for building the plots are inherited from the parent
#'   `ConcDataFig` class. This class just contains methods and attributes unique
#'   to the measured data plots. See `vignette("ModelResultFigures", package =
#'   "vapintr")` for examples and more information on how this class is used
#'   within vapintr.
#'
#' @examples
#' #Run the JEM using the example stochastic data
#' jem_results <- runTemplateData(stoc_jem_sim_example_data,
#' "Trichloroethylene")
#'
#' #Create a new output figure from the JEM Monte Carlo results
#' #using the measured data in the example dataset
#' model_result_figure <- ModelResultFig$new(jem_results[[1]]$JEMResults,
#' "Trichloroethylene", "79-01-6", "ug/m3", "groundwater",
#' stoc_jem_sim_example_data[[1]])
#'
#' #The object stored in the model_result_figure$meas_data_fig attribute is a
#' #MeasDataFig object
#' model_result_figure$meas_data_fig
#'
#' @export

MeasDataFig <- R6::R6Class(
  classname = "MeasDataFig",
  inherit = ConcDataFig,
  public = list(

    #' @field measured_data_legend A ggplot for the legend associated with the measured data
    #' @field reference_data_legend A ggplot for the legend associated with the reference data
    #' @field measured_data A data frame of measured data
    #' @field reference_data A data frame of reference data
    #' @field x_axis_break_interval The break interval for displaying date labels on the x-axis

    measured_data_legend = NULL,
    reference_data_legend = NULL,
    measured_data = NULL,
    reference_data = NULL,
    x_axis_break_interval = NULL,

    #' @description Assembles all elements required to build the measured and reference data plot
    buildPlotElements = function(){

      #Initialize data plot
      self$data_plot <- super$initializeDataPlot()

      #Initialize measured data legend
      self$measured_data_legend <- super$initializeLegend()

      #Assign factors to the measured and reference data types
      private$assignFactors()

      #Create measured data plot
      private$createMeasuredDataPlot()

      #Set y scale in normal or lognormal space
      super$setYAxisScale()

      #Set scale for x axis
      private$setXAxisScale()

      #Add plot labels
      super$addPlotLabels()

      #Create measured data legend
      private$createMeasuredDataLegend()

      if(!is.null(self$reference_data)){

        #Initialize reference data legend
        self$reference_data_legend <- super$initializeLegend()

        #Add reference data plot and legend
        private$addReferenceData()

      }
    },
    #' @description Sets the x- and y-axis limits for the measured and reference data plot
    #'
    #' @param mod_data_fig A `ModDataFig` object that will be displayed along with the measured and reference data in a combined figure.
    #' Providing a value for this parameter will make both plots use the same y-axis.
    setAxisLimits = function(mod_data_fig = NULL){

      #Get concentration data limits (assumes all concentration data have the same units)

      #Have both measured and reference data
      if (!is.null(self$measured_data) && !is.null(self$reference_data)){
        self$y_min <- min(self$measured_data$concentrations_to_plot, min(self$reference_data$Concentration))
        self$y_max <- max(self$measured_data$concentrations_to_plot, max(self$reference_data$Concentration))
      }
      #Just have measured data
      else if(!is.null(self$measured_data)){
        self$y_min <- min(self$measured_data$concentrations_to_plot)
        self$y_max <- max(self$measured_data$concentrations_to_plot)
      }
      #Just have reference data
      else if(!is.null(self$reference_data)){
        self$y_min <- min(self$reference_data$Concentration)
        self$y_max <- max(self$reference_data$Concentration)
      }

      #Compare against modeled data if provided
      if(!is.null(mod_data_fig)){
        self$y_min <- min(self$y_min, min(mod_data_fig$modeled_data$concentrations_to_plot))
        self$y_max <- max(self$y_max, max(mod_data_fig$modeled_data$concentrations_to_plot))
      }

      #Get min and max dates for x axis labels
      if(!is.null(self$measured_data)){
        self$x_min <- as.Date(min(self$measured_data$SampleDate), origin = "1900-01-01")
        self$x_max <- as.Date(max(self$measured_data$SampleDate), origin = "1900-01-01")
      }
      else
      {
        self$x_min <- Sys.Date()
        self$x_max <- Sys.Date() - 1
      }
    }
  ),
  private = list(
    assignFactors = function(){

      #Create the data type field if it doesn't already exist
      if(!("DataType" %in% names(self$measured_data))){
        detectText <- ifelse(self$measured_data$DetectedFlag == FALSE, "Nondetect", ifelse (self$measured_data$DetectedFlag == TRUE, "Detect", NA))
        self$measured_data$DataType <- paste("Measured", self$measured_data$Medium, detectText, sep=" ")
      }

      self$measured_data$DataType <- factor(self$measured_data$DataType, levels = self$factor_names)

      if (!is.null(self$reference_data)){

        #Assign data type factors to reference data
        if (!("DataType" %in% names(self$reference_data))){
          self$reference_data <- self$reference_data %>%
            mutate(DataType = paste("Reference Concentration", row_number()))
        }

        self$reference_data$DataType <- factor(self$reference_data$DataType, levels = self$factor_names)
      }
    },
    setXAxisScale = function(){

      #Set x axis breaks depending based on the number of years in the time interval
      #if default interval is applied, assign the dates based on the time span length
      dateAxisFromDate <- self$x_min
      dateAxisToDate <- self$x_max

      if(is.null(self$x_axis_break_interval))
      {
        self$x_axis_break_interval <- "default"
      }

      if(self$x_axis_break_interval == "default"){

        if (time_length(interval(dateAxisFromDate,dateAxisToDate), "years") > 10)
        {
          break_interval = "5 years"
        }
        else if (time_length(interval(dateAxisFromDate,dateAxisToDate), "years") > 5)
        {
          break_interval = "2 years"
        }
        else if (time_length(interval(dateAxisFromDate,dateAxisToDate), "years") > 2)
        {
          break_interval = "years"
        }
        else if (time_length(interval(dateAxisFromDate, dateAxisToDate), "years") > 1)
        {
          break_interval <- "6 months"
        }
        else if (time_length(interval(dateAxisFromDate, dateAxisToDate), "months") > 8)
        {
          break_interval <- "4 months"
        }
        else if (time_length(interval(dateAxisFromDate, dateAxisToDate), "months") > 6)
        {
          break_interval <- "3 months"
        }
        else if (time_length(interval(dateAxisFromDate, dateAxisToDate), "months") > 4)
        {
          break_interval <- "2 months"
        }
        else if (time_length(interval(dateAxisFromDate, dateAxisToDate), "months") > 2)
        {
          break_interval <- "months"
        }
        else if (time_length(interval(dateAxisFromDate, dateAxisToDate), "months") > 1)
        {
          break_interval <- "14 days"
        }
        else if (time_length(interval(dateAxisFromDate, dateAxisToDate), "days") > 14)
        {
          break_interval <- "7 days"
        }
        else if (time_length(interval(dateAxisFromDate, dateAxisToDate), "days") > 5)
        {
          break_interval <- "2 days"
        }
        else
        {
          break_interval <- "days"
        }
      }
      else
      {
        break_interval <- self$x_axis_break_interval
      }

      #Get format for date labels
      if (grepl("years", break_interval, fixed = TRUE)){
        date_label_format = "%Y"
      }
      else if (grepl("months", break_interval, fixed = TRUE)){
        date_label_format = "%b-%Y"
      }
      else {
        date_label_format = "%m/%d/%Y"
      }

      roundedMinDate <- floor_date(dateAxisFromDate, break_interval)
      roundedMaxDate <- ceiling_date(dateAxisToDate, break_interval)
      dateAxisBreaks <- seq(roundedMinDate, roundedMaxDate, break_interval)

      self$data_plot <- self$data_plot +
        scale_x_date(breaks = dateAxisBreaks,
                     date_labels = date_label_format,
                     limits=c(roundedMinDate, roundedMaxDate))

      if (grepl("days", break_interval, fixed = TRUE) || grepl("months", break_interval, fixed = TRUE)){
        self$data_plot <- self$data_plot +
          theme(plot.margin = margin(self$format_constants$MarginSize,
                                     self$format_constants$ExpandedMarginSize,
                                     self$format_constants$MarginSize,
                                     self$format_constants$MarginSize))
      }
    },
    createMeasuredDataPlot = function(){

      #Create measured data image
      self$data_plot <- self$data_plot +
        geom_point (data = self$measured_data,
                    aes (x = as.Date(SampleDate, origin = "1900-01-01"),
                         y = concentrations_to_plot,
                         shape = DataType,
                         color = DataType),
                    size = self$format_constants$SymbolSize,
                    stroke = self$format_constants$StrokeSize,
                    show.legend = FALSE) +
        scale_linetype_manual("", values = self$factor_line_formats, drop = FALSE) +
        scale_shape_manual(" ", values = self$factor_shape_formats, drop = FALSE) +
        scale_color_manual(" ", values = self$factor_color_formats, drop = FALSE) +
        guides(shape=guide_legend(nrow=2,byrow =FALSE),
               color=guide_legend(nrow=2,byrow =FALSE))
    },
    createMeasuredDataLegend = function(){

      measured_data_legend_data <- data.frame(DataType = unique(self$measured_data$DataType)) %>%
        mutate(x = 0, y = 0)
      measured_data_legend_data$DataType <- factor (measured_data_legend_data$DataType, levels = self$factor_names)

      measured_data_legend_data <- measured_data_legend_data[order(measured_data_legend_data$DataType), ]

      legend_shape_scale <- sapply(1:nrow(measured_data_legend_data), function(i){
        factor_name <- measured_data_legend_data$DataType[i]
        factor_name_index <- min(which(self$factor_names == factor_name))
        factor_index <- min(which(self$factor_indices == factor_name_index))
        return(self$factor_shape_formats[factor_index])
      })

      legend_color_scale <- sapply(1:nrow(measured_data_legend_data), function(i){
        factor_name <- measured_data_legend_data$DataType[i]
        factor_name_index <- min(which(self$factor_names == factor_name))
        factor_index <- min(which(self$factor_indices == factor_name_index))
        return(self$factor_color_formats[factor_index])
      })

      self$measured_data_legend <- self$measured_data_legend +
        geom_point (data = measured_data_legend_data,
                    aes (x = x,
                         y = y,
                         shape = DataType,
                         color = DataType),
                    size = self$format_constants$SymbolSize,
                    stroke = self$format_constants$StrokeSize,
                    show.legend = TRUE) +
        scale_shape_manual(" ", values = legend_shape_scale) +
        scale_color_manual(" ", values = legend_color_scale)
    },
    addReferenceData = function(){
      #Add reference data to the plot if it isn't null
      #Assuming the reference values are in the correct units
      if(!is.null(self$reference_data)){

        #Add reference data to measured data plot
        self$data_plot <- self$data_plot +
          geom_hline( data = self$reference_data,
                      aes(yintercept = Concentration,
                          linetype = DataType,
                          col = DataType),
                      size = self$format_constants$LineWidth,
                      show.legend = FALSE)

        reference_data_legend_data <- data.frame(DataType = unique(self$reference_data$DataType)) %>%
          mutate(x = 0, y = 0)
        reference_data_legend_data$DataType <- factor (reference_data_legend_data$DataType, levels = self$factor_names)

        reference_data_legend_data <- reference_data_legend_data[order(reference_data_legend_data$DataType), ]

        legend_line_scale <- sapply(1:nrow(reference_data_legend_data), function(i){
          factor_name <- reference_data_legend_data$DataType[i]
          factor_name_index <- min(which(self$factor_names == factor_name))
          factor_index <- min(which(self$factor_indices == factor_name_index))
          return(self$factor_line_formats[factor_index])
        })

        legend_color_scale <- sapply(1:nrow(reference_data_legend_data), function(i){
          factor_name <- reference_data_legend_data$DataType[i]
          factor_name_index <- min(which(self$factor_names == factor_name))
          factor_index <- min(which(self$factor_indices == factor_name_index))
          return(self$factor_color_formats[factor_index])
        })

        self$reference_data_legend <- self$reference_data_legend +
          theme(legend.key.width = unit(2.5, "line")) +
          geom_hline( data = self$reference_data,
                      aes(yintercept = Concentration,
                          linetype = DataType,
                          col = DataType),
                      size = self$format_constants$LineWidth,
                      show.legend = TRUE) +
          scale_color_manual(name = "", labels = self$reference_data$ReferenceConcentrationName, values = legend_color_scale) +
          scale_linetype_manual(name = "", labels = self$reference_data$ReferenceConcentrationName, values = legend_line_scale)
      }
    }
  )
)


