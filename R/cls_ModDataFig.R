#' @title Class for data in modeled air concentration violin plots
#'
#' @description An R6 Class used to build violin plots of modeled data. Most
#'   functions for building the plots are inherited from the parent `ConcDataFig`
#'   class. This class just contains methods and attributes unique to the
#'   modeled data violin plots. See `vignette("ModelResultFigures", package =
#'   "vapintr")` for examples and more information on how this class is used
#'   within vapintr.
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
#' #ModDataFig object
#' model_result_figure$mod_data_fig
#'
#' @export

ModDataFig <- R6::R6Class(
  classname = "ModDataFig",
  inherit = ConcDataFig,
  public = list(

    #' @field violin_plot_legend A ggplot for the legend associated with the violin plot data
    #' @field percentiles_legend A ggplot for the legend associated with the violin plot percentiles
    #' @field modeled_data A data frame of modeled indoor air concentration data
    #' @field percentile_50 The 50<sup>th</sup> percentile value of the modeled indoor air concentration data
    #' @field percentile_95 The 95<sup>th</sup> percentile value of the modeled indoor air concentration data

    violin_plot_legend = NULL,
    percentiles_legend = NULL,
    modeled_data = NULL,
    percentile_50 = NULL,
    percentile_95 = NULL,
    #' @description Assembles all elements required to build the modeled data violin plot
    buildPlotElements = function(){

      #Initialize data plot
      self$data_plot <- super$initializeDataPlot()

      #Initialize modeled data legend
      self$violin_plot_legend <- super$initializeLegend()

      #Initialize percentile data legend
      self$percentiles_legend <- super$initializeLegend()

      #Build modeled data plot
      private$createModeledDataPlot()

      #Set y scale in normal or lognormal space
      super$setYAxisScale()

      #Add and annotate violin plot percentiles
      private$addAndAnnotateViolinPlotPercentiles()

      #Add plot labels
      super$addPlotLabels()

      #Build violin plot legend
      private$createViolinPlotLegend()

      #Build percentiles legend
      private$createPercentilesLegend()
    },
    #' @description Sets the y-axis limits for the modeled data violin plot
    #'
    #' @param meas_data_fig A `MeasDataFig` object that will be displayed along with the modeled data in a combined figure.
    #' Providing a value for this parameter will make both plots use the same y-axis.
    setAxisLimits = function(meas_data_fig = NULL){

      #Get concentration data limits (assumes all concentration data have the same units)
      self$y_min <- min(self$modeled_data$concentrations_to_plot)
      self$y_max <- max(self$modeled_data$concentrations_to_plot)

      #Compare against the measured and reference air data if provided
      if (!is.null(meas_data_fig)){
        if(!is.null(meas_data_fig$measured_data) && !is.null(meas_data_fig$reference_data)){
          {
            self$y_min <- min(self$y_min,
                              min(meas_data_fig$measured_data$concentrations_to_plot),
                              min(meas_data_fig$reference_data$Concentration))
            self$y_max <- max(self$y_max,
                              max(meas_data_fig$measured_data$concentrations_to_plot),
                              max(meas_data_fig$reference_data$Concentration))
          }
        }
        else if (!is.null(meas_data_fig$measured_data)){
          self$y_min <- min(self$y_min, min(meas_data_fig$measured_data$concentrations_to_plot))
          self$y_max <- max(self$y_max, max(meas_data_fig$measured_data$concentrations_to_plot))
        }
        else if (!is.null(meas_data_fig$reference_data)){
          self$y_min <- min(self$y_min, min(meas_data_fig$reference_data$Concentration))
          self$y_max <- max(self$y_max, max(meas_data_fig$reference_data$Concentration))
        }
      }
    }
  ),
  private = list(
    createModeledDataPlot = function(){

      #Create violin plot data figure
      self$data_plot <- self$data_plot+
        geom_violin (data = self$modeled_data,
                     aes (x = contaminant,
                          y =  (concentrations_to_plot),
                          linetype = DataType,
                          alpha=0.5),
                     fill = c("lightsteelblue"),
                     linetype = self$factor_line_formats[self$factor_indices$ModeledIndoorAirViolinPlotData],
                     size = self$format_constants$LineWidth,
                     show.legend = FALSE)
    },
    createViolinPlotLegend = function(){
      #Create violin plot legend object
      self$violin_plot_legend <- self$violin_plot_legend +
        geom_violin (data = self$modeled_data,
                     aes (x = contaminant,
                          y =  concentrations_to_plot,
                          fill = data_type),
                     alpha=0.5,
                     size = self$format_constants$LineWidth,
                     linetype = self$factor_line_formats[self$factor_indices$ModeledIndoorAirViolinPlotData],
                     show.legend = TRUE) +
        scale_fill_manual("", values = c("lightsteelblue")) +
        scale_linetype_manual("", values = self$factor_line_formats, drop = FALSE)
    },
    createPercentilesLegend = function(){

      #Create violin plot percentiles legend object
      percentile_labels = c(expression(Modeled~Indoor~Air~50^th~Percentile),
                            expression(Modeled~Indoor~Air~95^th~Percentile))

      #Create dummy dataset for percentile lines
      dummy_df <- data.frame(x=c(0, 0), y=c(0, 0), data_type = c(self$factor_names[self$factor_indices$ModeledIndoorAir50thPercentile],
                                                           self$factor_names[self$factor_indices$ModeledIndoorAir95thPercentile]))
      dummy_df$data_type <- factor (dummy_df$data_type, levels = c(self$factor_names[self$factor_indices$ModeledIndoorAir50thPercentile],
                                                       self$factor_names[self$factor_indices$ModeledIndoorAir95thPercentile]))

      self$percentiles_legend <- self$percentiles_legend +
        geom_hline (data = dummy_df,
                    aes (yintercept =  y,
                         linetype = data_type,
                         color = data_type),
                    size = self$format_constants$LineWidth,
                    show.legend = TRUE) +
        scale_color_manual(name = "",
                           labels = percentile_labels,
                           values = c(self$factor_color_formats[self$factor_indices$ModeledIndoorAir50thPercentile],
                                      self$factor_color_formats[self$factor_indices$ModeledIndoorAir95thPercentile])) +
        scale_linetype_manual(name = "",
                              labels = percentile_labels,
                              values = c(self$factor_line_formats[self$factor_indices$ModeledIndoorAir50thPercentile],
                                         self$factor_line_formats[self$factor_indices$ModeledIndoorAir95thPercentile]))
    },
    addAndAnnotateViolinPlotPercentiles = function(){

      # identify 50th and 95th percentile lines for modeled data plot
      self$percentile_50 <- quantile(self$modeled_data$concentrations_to_plot, probs = 0.5)[[1]]
      self$percentile_95 <- quantile(self$modeled_data$concentrations_to_plot, probs = 0.95)[[1]]

      #Create ggplot_build object to extract violin plot widths
      plot_build_object <- ggplot_build(self$data_plot)

      #Flag whether the violin plot y-axis uses a log scale
      #The widths are different if it uses a log-scale vs. otherwise, so we can't add the lines until the y-axis is set
      is_log_scale <- plot_build_object$plot$scales$scales[[1]]$trans$name == "log-10"

      #Get the max width applied for the violin plot along the x axis
      max_width <- plot_build_object$data[[1]]$width

      #Get width of the violin plot at the 50th percentile
      violin_width_50 <- approx(plot_build_object$data[[1]]$y,
                                plot_build_object$data[[1]]$violinwidth,
                                #If using a log scale, transform the data percentiles to a log
                                ifelse(is_log_scale, log10(self$percentile_50), self$percentile_50)
      )$y
      violin_width_95 <- approx(plot_build_object$data[[1]]$y,
                                plot_build_object$data[[1]]$violinwidth,
                                #If using a log scale, transform the data percentiles to a log
                                ifelse(is_log_scale, log10(self$percentile_95), self$percentile_95)
      )$y

      #Add lines to violin plot
      self$data_plot <- self$data_plot +
        geom_segment(aes(x = (1 - violin_width_50/2*max_width),
                         y = self$percentile_50,
                         xend = (1 + violin_width_50/2*max_width),
                         yend = self$percentile_50),
                     linetype = self$factor_line_formats[self$factor_indices$ModeledIndoorAir50thPercentile],
                     size = self$format_constants$LineWidth,
                     color = self$factor_color_formats[self$factor_indices$ModeledIndoorAir50thPercentile]) +
        geom_segment(aes(x = (1 - violin_width_95/2*max_width),
                         y = self$percentile_95,
                         xend = (1 + violin_width_95/2*max_width),
                         yend = self$percentile_95),
                     linetype = self$factor_line_formats[self$factor_indices$ModeledIndoorAir95thPercentile],
                     size = self$format_constants$LineWidth,
                     color = self$factor_color_formats[self$factor_indices$ModeledIndoorAir95thPercentile])

      #Add annotations for the 50th and 95th percentile
      perc50label <- paste("italic(P)[50]~", "`=`~", signif(self$percentile_50, 3), "~", if_else(self$label_units == "ppb", "ppb", "mu*g/m^3"), sep = "")
      perc95label <- paste("italic(P)[95]~", "`=`~", signif(self$percentile_95, 3), "~", if_else(self$label_units == "ppb", "ppb", "mu*g/m^3"), sep = "")

      self$data_plot <- self$data_plot +
        annotate("label",
                 x = 1,
                 y = self$percentile_50,
                 label = perc50label,
                 parse = TRUE,
                 size = self$format_constants$AnnotationTextSize,
                 hjust = "right",
                 vjust = "top") +
        annotate("label",
                 x = 1,
                 y = self$percentile_95,
                 label = perc95label,
                 parse = TRUE,
                 size = self$format_constants$AnnotationTextSize,
                 hjust = "right",
                 vjust = "bottom")
    }
  )
)


