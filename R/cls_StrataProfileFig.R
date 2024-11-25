#' @title Create soil strata log figures
#'
#' @description An R6 Class used to build standard soil strata log figures
#'   based on the soil strata inputs to `runJE()`. For more information on
#'   this class and how to use it, see the following vignette.
#'
#'   `vignette("ConceptualSiteModelAndSoilStrataFigures", package = "vapintr")`
#'
#' @examples
#' #Get soil strata data from the example data
#' strata_logs_data <- det_jem_sim_example_data[[4]]
#'
#' #Initialize the soil strata logs figure object
#' strata_logs_figure <- StrataProfileFig$new(strata_logs_data)
#'
#' #Create the standard image using the StrataProfileFig object
#' strata_logs_figure$createStandardFigure()
#'
#' #Plot the standard image
#' plot(strata_logs_figure$fig_image)
#' @export

StrataProfileFig <- R6::R6Class(
  classname = "StrataProfileFig",
  inherit = StandardOutputFig,
  public = list(

    #' @field strata_logs_dfx Data frame of strata log data. Should have the
    #'   same format as the strata_logs_data input parameter to `runJE()`.
    #' @field strata_log_figs_lx List of ggplot strata log figures produced
    #'   by the `StrataProfileFig` object
    #' @field maximum_depth Maximum depth of any of the strata logs in the figure

    strata_logs_dfx = NULL,
    strata_log_figs_lx = NULL,
    maximum_depth = NULL,

    #' @description Function to create a new `StrataProfileFig` object
    #'
    #' @param strata_logs_dfx Data frame of strata log data. Should have the
    #'   same format as the strata_logs_data input parameter to `runJE()`.
    initialize = function(strata_logs_dfx){

      self$strata_logs_dfx <- strata_logs_dfx
      private$buildStrataLogFigs()

    },
    #' @description Assigns values to the standard properties inherited from the
    #'   `StandardOutputFig` class used to render the figure at a recommended size.
    #' @param plot_orientation Controls whether the output figure will have a
    #'   "portrait", "landscape", or "default" orientation.
    createStandardFigure = function(plot_orientation = "default"){

      nlogs <- length(self$strata_log_figs_lx)

      maximum_depth <- self$maximum_depth


      #Size plots based on the number of strata logs
      if (plot_orientation == "default"){
        plot_orientation <- ifelse(nlogs <= 3, "portrait", "landscape")
      }
      else
      {
        #Throw an error if the orientation is not portrait or landscape
        if(!(plot_orientation %in% c("portrait", "landscape")))
        {
          stop("Error: Unsupported value assigned to the plot_orientation parameter. Use either \"default\", \"portrait\", or \"landscape\"")
        }
      }

      #Assume 8.5" x 11" paper, so 6.5" x 9" once margins are removed (word)
      #Note, leave 0.5 inch off the top of each plot for text
      #Can plot up to 3 plots in portrait orientation and 6 in landscape orientation
      if(plot_orientation == "portrait")
      {
        self$fig_height <- 8.5
        self$fig_width <- 1.6*nlogs + 1.6




        plot_list_elements <- lapply(1:nlogs, function(logID_index){
            return(self$strata_log_figs_lx[[logID_index]] +
                     theme(legend.position = "none") +
                     coord_fixed(ratio= 20/self$maximum_depth) + scale_y_reverse(limits = c(maximum_depth, 0))
                   )
        })
      }
      else
      {
        self$fig_height <- 6
        self$fig_width <- 1.5*nlogs + 1.5

        plot_list_elements <- lapply(1:nlogs, function(logID_index){
          return(self$strata_log_figs_lx[[logID_index]] +
                   theme(legend.position = "none") +
                   coord_fixed(ratio= 13/self$maximum_depth) +
                   scale_y_reverse(limits = c(maximum_depth, 0))
          )
        })
      }

      footnote_text = "The 12 SCS soil types\nshown in the legend\nare standard and may\ninclude soil types not\nrepresented in the figure\n\nUSDA = United States\nDepartment of Agriculture\n\nSCS = Soil Conservation\nService"

      gridded_plot <- plot_grid(plotlist = plot_list_elements, nrow = 1, ncol = nlogs)

      soil_type_legend <- getLegend(self$strata_log_figs_lx[[1]] + theme(
        legend.key.spacing.x = unit(10, "pt"),
        legend.key.spacing.y = unit(5, "pt")))
      if(plot_orientation == "landscape"){soil_type_legend + theme(legend.justification = "top")}

      label_plot <- ggplot() + theme(panel.background = element_blank()) +
        draw_label(footnote_text, x= -0.05, y = 0.5, hjust = 0, size = 8)

      if(plot_orientation == "portrait"){
        legend_label_plot <- plot_grid(NULL, soil_type_legend, label_plot, nrow = 3, ncol = 1, rel_heights = c(1, 2, 1))
      }else{
        legend_label_plot <- plot_grid(soil_type_legend, label_plot, nrow = 2, ncol = 1, rel_heights = c(2, 1))
      }

      self$fig_image <- plot_grid(gridded_plot, legend_label_plot, nrow = 1, ncol = 2, rel_widths = c(nlogs, 1))
    }
  ),
  private = list(
    buildStrataLogFigs = function(){
      #Define styles for use in the boring log outputs
      soil_type_styles <- data.frame(
        soil_type = c("Clay", "Clay Loam", "Loam", "Loamy Sand",
                      "Sand", "Sandy Clay", "Sandy Clay Loam", "Sandy Loam",
                      "Silt", "Silt Loam", "Silty Clay", "Silty Clay Loam"),
        fill = c("red", "red3", "black", "khaki3",
                 "khaki1", "tan1", "orange3", "khaki3",
                 "tan", "tan4", "lightcoral", "sienna4"),
        color = c("red", "red3", "black", "grey20",
                  "grey20", "tan1", "orange3", "khaki3",
                  "tan", "tan4", "lightcoral", "sienna4"),
        pattern = c("stripe", "stripe", "crosshatch", "circle",
                    "circle", "stripe", "crosshatch", "crosshatch",
                    "stripe", "stripe", "stripe", "crosshatch"),
        density = c(0.3, 0.3, 0.3, 0.6, 0.6, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3),
        spacing = c(0.1, 0.1, 0.1, 0.15, 0.15, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
        angle = c(0, 15, 45, 45, 0, 45, 75, 0, 90, 105, 135, 15))

      #soil_type_styles$spacing = 0.03/soil_type_styles$density

      #Assume output is from importTemplateData and has already passed the requisite checks.
      #All units should be in m

      #Get a list of the unique logIDs in the strata file
      logIDs <- unique(self$strata_logs_dfx$LogID)
      nlogs <- length(logIDs)

      maximum_depth <- 0

      #Get a list of data for each boring log along with the maximum depth
      boring_log_data_lx <- lapply(1:nlogs, function(logID_index){

        boring_log_data <- self$strata_logs_dfx %>% filter(LogID == logIDs[logID_index])

        #Assign additional data needed for boring log calculations
        boring_log_data <- boringLogSetup(boring_log_data)

        return(boring_log_data)
      })

      #Get the deepest depth associated with a boring log
      for(logID_index in 1:nlogs){
        maximum_depth <- max(maximum_depth, boring_log_data_lx[[logID_index]]$lower_depth)
      }

      self$maximum_depth <- maximum_depth

      #legend info
      legend_title = "USDA SCS Soil Types"

      #Generate the plot for each boring log
      self$strata_log_figs_lx <- lapply(1:nlogs, function(logID_index){

        boring_log_data <- boring_log_data_lx[[logID_index]]

        strata_df <- data.frame(matrix(ncol = 4, nrow = 0))
        colnames(strata_df) <- c("x", "y", "layer_number", "soil_type_factor")

        #Add polygons for soil layers
        for (layer_index in 1:nrow(boring_log_data))
        {
          stratum_layer <- boring_log_data %>% filter(LayerOrder == layer_index)

          stratum_layer_df <- data.frame(x = c(-1, -1, 1, 1),
                                         y = c(stratum_layer$upper_depth,
                                               stratum_layer$lower_depth,
                                               stratum_layer$lower_depth,
                                               stratum_layer$upper_depth),
                                         layer_number = stratum_layer$LayerOrder,
                                         soil_type_factor = stratum_layer$SoilType)

          strata_df <- rbind(strata_df, stratum_layer_df)

        }

        strata_df$soil_type_factor <- factor(strata_df$soil_type_factor, levels = soil_type_styles$soil_type)

        #Get standard breaks
        break_interval <- 10^(floor(log10(maximum_depth)))
        max_plot_depth <- break_interval*ceiling(maximum_depth/break_interval)

        if(max_plot_depth / break_interval == 1){
          break_interval = break_interval / 10
        } else if(max_plot_depth / break_interval == 2){
          break_interval = break_interval / 5
        } else if(max_plot_depth / break_interval == 3){
          break_interval = break_interval / 2
        }

        proposed_breaks <- seq(0,max_plot_depth,break_interval)
        count_above_max_depth <- sum(proposed_breaks > maximum_depth)
        values_to_remove <- count_above_max_depth - 1

        if(values_to_remove > 0){
          proposed_breaks <- proposed_breaks[1:(length(proposed_breaks)-values_to_remove)]
        }

        #Generate plot
        boring_log_plot <- ggplot() +
          cowplot::theme_cowplot() +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                legend.key.width = unit(1, "cm"),
                legend.key.height = unit(0.5, "cm"),
                legend.spacing.y = unit(0.2, "cm"),
                legend.text = element_text(size = 8),
                legend.title = element_text(size = 9, face = "bold"),
                axis.text.y = element_text(hjust = 0.95, vjust = 0.2, size = 9),
                axis.title.y = element_text(size = 9),
                axis.title.x = element_text(size = 9)) +
          labs(x = boring_log_data$LogID[[1]], y = "Depth (meters)") +
          geom_polygon_pattern(data = strata_df,
                               aes(x = x, y = y,
                                   group = layer_number,
                                   color = soil_type_factor,
                                   fill = soil_type_factor,
                                   pattern = soil_type_factor,
                                   pattern_density = soil_type_factor,
                                   pattern_spacing = soil_type_factor,
                                   pattern_fill = soil_type_factor,
                                   pattern_color = soil_type_factor,
                                   pattern_angle = soil_type_factor),
                               pattern_key_scale_factor = 0.4,
                               show.legend = TRUE) + #,
                               #show.legend = (logID_index == length(logIDs))) +
          scale_color_manual(" ", values = rep("grey20", nrow(soil_type_styles)), drop = FALSE) +
          scale_fill_manual(" ", values = rep("white", nrow(soil_type_styles)), drop = FALSE) +
          scale_pattern_manual(" ", values = soil_type_styles$pattern, drop = FALSE) +
          scale_pattern_density_manual(" ", values = soil_type_styles$density, drop = FALSE) +
          scale_pattern_spacing_manual(" ", values = soil_type_styles$spacing, drop = FALSE) +
          scale_pattern_fill_manual(" ", values = soil_type_styles$fill, drop = FALSE) +
          scale_pattern_color_manual(" ", values = soil_type_styles$color, drop = FALSE) +
          scale_pattern_angle_manual(" ", values = soil_type_styles$angle, drop = FALSE) +
          guides(
            color = guide_legend(byrow = TRUE, title = legend_title),
            fill = guide_legend(byrow = TRUE, title = legend_title),
            pattern = guide_legend(byrow = TRUE, title = legend_title),
            pattern_density = guide_legend(byrow = TRUE, title = legend_title),
            pattern_spacing = guide_legend(byrow = TRUE, title = legend_title),
            pattern_fill = guide_legend(byrow = TRUE, title = legend_title),
            pattern_color = guide_legend(byrow = TRUE, title = legend_title),
            pattern_angle = guide_legend(byrow = TRUE, title = legend_title)
          ) +
          scale_y_reverse(limits = c(max(proposed_breaks), 0), breaks = proposed_breaks)


        return(boring_log_plot)

      })
    }
  )
)


