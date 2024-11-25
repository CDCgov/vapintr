#' @title Create standard modeled and measured air concentration figures
#'
#' @description An R6 class used to build standard output figures from Johnson
#'   and Ettinger model stochastic simulation results. See
#'   `vignette("ModelResultFigures", package = "vapintr")` for examples and more
#'   information on using this class.
#'
#' @examples
#' #Run the JEM using the example stochastic data
#' jem_results <- runTemplateData(stoc_jem_sim_example_data, "Trichloroethylene")
#'
#' #Create a new output figure from the JEM Monte Carlo results
#' model_result_figure <- ModelResultFig$new(jem_results[[1]]$JEMResults,
#' "Trichloroethylene", "79-01-6", "ug/m3", "groundwater")
#'
#' #Create the standard plot image from the model_result_figure object
#' model_result_figure$createStandardFigure()
#'
#' @export

ModelResultFig <- R6::R6Class(
  classname = "ModelResultFig",
  inherit = StandardOutputFig,
  public = list(

    #' @field contaminant_name The name of the modeled contaminant
    #' @field contaminant_casrn The contaminant's Chemical Abstracts Service Registry Number
    #' @field concentration_units The units used to display concentrations on the y-axis ("ug/m3" or "ppb")
    #' @field source_medium The contaminant source medium
    #' @field meas_data_fig `MeasDataFig` object for the measured and reference data image (if included)
    #' @field mod_data_fig `ModDataFig` object for the modeled data image

    contaminant_name = NULL,
    contaminant_casrn = NULL,
    concentration_units = NULL,
    source_medium = NULL,
    mod_data_fig = NULL,
    meas_data_fig = NULL,

    #' @description Function to create a new ModelResultFig object
    #'
    #' @param jemMCResults Monte Carlo simulation results from a stochastic simulation using `runJE()`
    #' @param contaminant_name The name of the modeled contaminant for display in the figure images
    #' @param contaminant_casrn The contaminant's Chemical Abstracts Service Registry Number
    #' @param concentration_units The units used to display concentrations on the y-axis ("ug/m3" or "ppb")
    #' @param source_medium The contaminant source medium
    #' @param measured_data_dfx A data frame of measured data, typically imported from the vapintr data import template
    #' @param reference_data_dfx A data frame of reference concentrations. Must include the fields "Name" and "Concentration".
    initialize = function(jemMCResults, contaminant_name, contaminant_casrn, concentration_units, source_medium, measured_data_dfx = NULL, reference_data_dfx = NULL){

      #Check that concentration units for output figure are either ug/m3 or ppb
      if(!(concentration_units %in% c("ug/m3", "ppb")))
      {
        stop("Error: Unsupported value assigned to the concentration_units input parameter. Use either \"ug/m3\" or \"ppb\"")
      }

      self$contaminant_name <- contaminant_name
      self$contaminant_casrn <- contaminant_casrn
      self$concentration_units <- concentration_units
      self$source_medium <- source_medium

      private$buildModeledDataPlot(jemMCResults)
      private$buildMeasuredDataPlot(measured_data_dfx, reference_data_dfx)

    },
    #' @description Creates a standard figure using the modeled and measured data figures
    #' assigned to the object's mod_data_fig and meas_data_fig attributes
    createStandardFigure = function(){
      #browser()

      model_result_figure <- NA

      #Assemble the plot elements
      self$buildPlotElements()

      #Create plot for modeled data only
      if(is.null(self$meas_data_fig))
      {
        violin_plot_legend <-getLegend(self$mod_data_fig$violin_plot_legend + theme(legend.position = c(0.33, -0.05),
                                                                                    legend.justification = c(0, 0),
                                                                                    legend.background = element_rect(fill = 'transparent')))

        percentiles_legend <- getLegend(self$mod_data_fig$percentiles_legend + theme(legend.position = c(0.18, -0.18),
                                                                                     legend.justification = c(0, 0),
                                                                                     legend.background = element_rect(fill = 'transparent')))

        full_plot_legend <- plot_grid(violin_plot_legend, percentiles_legend, nrow = 1, ncol=2)
        model_result_figure <- plot_grid(self$mod_data_fig$data_plot, full_plot_legend, nrow = 3, rel_heights = c(1,0.1,0.1))
      }
      else
      {
        #Create plot for measured and modeled data without reference data
        if(is.null(self$meas_data_fig$reference_data)){

          #Get legends and standardize their position beneath the violin plot
          violin_plot_legend <-getLegend(self$mod_data_fig$violin_plot_legend + theme(legend.position = c(0.1, 0.02),
                                                                                      legend.justification = c(0, 0),
                                                                                          legend.background = element_rect(fill = 'transparent')))

          percentiles_legend <- getLegend(self$mod_data_fig$percentiles_legend + theme(legend.position = c(0.1, -0.10),
                                                                                       legend.justification = c(0, 0),
                                                                                           legend.background = element_rect(fill = 'transparent')))

          combined_violin_plot_legend <- plot_grid(violin_plot_legend, percentiles_legend, nrow = 2, ncol=1)

          measured_data_legend <- getLegend(self$meas_data_fig$measured_data_legend + theme(legend.position = c(0.15, -0.10),
                                                                                            legend.justification = c(0, 0),
                                                                                            legend.background = element_rect(fill = 'transparent')))

          #Build full legend
          full_plot_legend <- plot_grid(NULL, combined_violin_plot_legend, measured_data_legend, nrow = 1, ncol = 3,
                                        rel_widths = c(0.1,1,1))

          #Group together the two plot images
          plot_images <- plot_grid(self$mod_data_fig$data_plot, self$meas_data_fig$data_plot, nrow = 1, ncol = 2)

          #Build the final output figure
          model_result_figure <- plot_grid(plot_images, full_plot_legend, nrow = 3, rel_heights = c(1,0.21, 0.1))

          #Create plot for measured and modeled data with reference data
        } else {
          #Get legends and standardize their position beneath the violin plot
          violin_plot_legend <- getLegend(self$mod_data_fig$violin_plot_legend +
                                                 theme(legend.position = c(0.01, 0.04),
                                                       legend.justification = c(0, 0),
                                                       legend.background = element_rect(fill = 'transparent')))

          percentiles_legend <- getLegend(self$mod_data_fig$percentiles_legend +
                                                 theme(legend.position = c(0.01, -0.15),
                                                       legend.justification = c(0, 0),
                                                       legend.background = element_rect(fill = 'transparent')))

          combined_violin_plot_legend <- plot_grid(violin_plot_legend, percentiles_legend, nrow = 2, ncol=1)

          measured_data_legend <- getLegend(self$meas_data_fig$measured_data_legend +
                                              theme(legend.position = c(0, -0.12),
                                                    legend.justification = c(0, 0),
                                                    legend.background = element_rect(fill = 'transparent')))

          reference_data_legend <- getLegend(self$meas_data_fig$reference_data_legend +
                                               theme(legend.position = c(0, -0.12),
                                                     legend.justification = c(0, 0),
                                                     legend.background = element_rect(fill = 'transparent')))

          full_plot_legend <- plot_grid(combined_violin_plot_legend, reference_data_legend, measured_data_legend, nrow = 1, ncol = 3, rel_widths = c(0.95, 1.05, 1))
          plot_images <- plot_grid(self$mod_data_fig$data_plot, self$meas_data_fig$data_plot, nrow = 1, ncol = 2)

          model_result_figure <- plot_grid(plot_images, full_plot_legend, nrow = 3, rel_heights = c(1,0.21, 0.1))
        }

      }

      #Add annotation
      format_constants <- self$mod_data_fig$getStandardFormatConstants()
      plot_message <- "Wider sections of the violin plot (light blue area) represent a higher probability of model-predicted indoor air concentrations."
      model_result_figure <- model_result_figure + draw_label(plot_message, x= 0.03, y = 0.05, hjust = 0, size = format_constants$CaptionTextSize)

      plot_message <- paste(if_else(self$concentration_units == "ppb", "ppb~`=`~parts~per~billion", "mu*g/m^3~`=`~micrograms~per~cubic~meter"), "*`;`~", sep="")
      plot_message <- paste(plot_message, "italic(P)[95]~`=`~95^th~percentile*`;`~", sep="")
      plot_message <- paste(plot_message, "italic(P)[50]~`=`~50^th~percentile", sep="")

      model_result_figure <- model_result_figure + draw_plot_label(plot_message, x = 0.03, y = 0.05, hjust = 0, size = format_constants$CaptionTextSize, parse = TRUE)

      self$fig_image <- model_result_figure
      self$fig_width <- 6.5 #Use a standard width of 6.5 inches for all model result figures
      self$fig_height <- 6 #Use a standard height of 6 inches for all model result figures
    },
    #' @description Sets the axes and calls the buildPlotElements() functions for the measured and modeled data figure objects
    buildPlotElements = function(){

      #Get concentrations in the correct units
      private$setPlotConcentrationUnits()

      #Build the plot elements
      self$mod_data_fig$setAxisLimits(self$meas_data_fig)
      self$mod_data_fig$buildPlotElements()

      if(!is.null(self$meas_data_fig)){
        self$meas_data_fig$setAxisLimits(self$mod_data_fig)
        self$meas_data_fig$buildPlotElements()
      }
    }
  ),
  private = list(
    buildModeledDataPlot = function(jemMCResults){

      #No additional error checking occurs on the mcResults input for this function.
      #Assumption is that it was created as output from runJE.R
      #Modify JEM Monte Carlo results output to prepare for plotting

      mcResults_dfx <- as.data.frame(mc2d::unmc(jemMCResults))

      violin_plot_data <- data.frame(Cia_ugm3 = mcResults_dfx$Cia, Cia_ppb = mcResults_dfx$Cia_ppb) %>%
          mutate(data_type = "Modeled Indoor Air Violin Plot") %>%
          mutate(contaminant = self$contaminant_name)

      #Create JEMPlotInfo object
      self$mod_data_fig <- ModDataFig$new()

      self$mod_data_fig$modeled_data <- violin_plot_data

      self$mod_data_fig$title <- "Modeled Indoor Air Data"
      self$mod_data_fig$label_units <- ifelse(self$concentration_units == "ppb", self$concentration_units, "\u03bcg/m\u00b3")
      self$mod_data_fig$x_title <- paste("Modeled Using", str_to_title(self$source_medium),"Data",sep=" ")
      self$mod_data_fig$y_title <- paste("Concentration (", self$mod_data_fig$label_units ,")", sep = "")
    },
    buildMeasuredDataPlot = function(measured_data_dfx = NULL, reference_data_dfx = NULL){

      #No additional error checking on the measured_data_dfx data frame in this function.
      #Assumption is that it was created as output from importTemplateData.R

      #Set the measured plot to null if there's no measured data
      if(is.null(measured_data_dfx) && is.null(reference_data_dfx)){
        self$meas_data_fig <- NULL
      }
      #Otherwise create the figure object
      else
      {
        #Subset input concentrations to only those records of interest
        if(!is.null(measured_data_dfx)){
          #Remove leading zeros if present from measured data and the contaminant casrn
          measured_data_dfx$CASRN <- gsub("^0+", "",measured_data_dfx$CASRN)

          air_data_dfx <- measured_data_dfx %>%
            filter (CASRN == gsub("^0+", "",self$contaminant_casrn)) %>% #filter to contaminant of interest
            filter (Medium == "Indoor Air" | Medium == "Outdoor Air") %>% #filter to only indoor and outdoor air data.
            filter (!is.na(SampleDate)) #filter to only those records with sample dates
        }

        if(nrow(air_data_dfx) == 0){
          air_data_dfx <- NULL
        }

        if(!is.null(reference_data_dfx)){

          #filter to contaminant of interest
          reference_data_dfx <- reference_data_dfx %>%
            mutate (CASRN = gsub("^0+", "",.data$CASRN)) %>% #remove leading zeros from CASRNs
            filter (CASRN == gsub("^0+", "",self$contaminant_casrn)) #filter to contaminant of interest

          if(nrow(reference_data_dfx) > 0){
            #If there are more than four reference values, keep only the first four
            if(nrow(reference_data_dfx) > 4){
              warning(paste("Five or more reference concentrations were provided for the contaminant with CASRN: ", self$contaminant_casrn, ". Standard output figures only support up to four reference concentrations. Keeping only the first four reference concentrations for the figure.", sep = ""))
              reference_data_dfx <- reference_data_dfx[1:4, ]
            }

            #Convert units to desired units
            #Get chemical properties
            chem_prop_dfx <- getChemicalProperties(CAS = self$contaminant_casrn)

            #Assign molecular weight to a variable because it gets used a lot
            MW <- chem_prop_dfx$MW

            #Set concentration units
            reference_data_dfx <- reference_data_dfx %>%
              rowwise %>%
              mutate(Concentration = concUnitConvs(.data$Concentration, .data$Units, self$concentration_units, "Air", 25, MW)) %>%
              mutate(Units = self$concentration_units) %>%
              ungroup()
          }
          else
          {
            reference_data_dfx <- NULL
          }

        }

        #Create JEMPlotInfo object and assemble elements
        #Plot with reference data values only generates if measured data are available
        if(!is.null(air_data_dfx)){
          self$meas_data_fig <- MeasDataFig$new()

          self$meas_data_fig$measured_data <- air_data_dfx

          self$meas_data_fig$reference_data <- reference_data_dfx

          self$meas_data_fig$title <- if_else(is.null(reference_data_dfx), "Measured Air Data", "Measured and Reference Air Data")
          self$meas_data_fig$label_units <- ifelse(self$concentration_units == "ppb", self$concentration_units, "\u03bcg/m\u00b3")
          self$meas_data_fig$x_title <- paste(self$contaminant_name, "Sample Date", sep=" ")
          self$meas_data_fig$y_title <- paste("Concentration (", self$meas_data_fig$label_units ,")", sep = "")
        }
        else {
          self$meas_data_fig <- NULL
        }
      }
    },
    setPlotConcentrationUnits = function(){

      #Set concentration units for modeled data
      if(self$concentration_units == "ppb"){
        self$mod_data_fig$modeled_data$concentrations_to_plot <- self$mod_data_fig$modeled_data$Cia_ppb
      }
      else
      {
        self$mod_data_fig$modeled_data$concentrations_to_plot <- self$mod_data_fig$modeled_data$Cia_ugm3
      }

      #Set concentration units for measured data
      if(!is.null(self$meas_data_fig)){
        #Get chemical properties
        chem_prop_dfx <- getChemicalProperties(CAS = self$contaminant_casrn)

        #Convert units of input concentrations to concentration units for display
        #Uses a default of Ts = 25 because that's the temperature used in the runJE output
        if(!is.null(self$meas_data_fig$measured_data)){
          self$meas_data_fig$measured_data$concentrations_to_plot <- sapply(1:nrow(self$meas_data_fig$measured_data), function(i){
            concUnitConvs(self$meas_data_fig$measured_data$Concentration[i], self$meas_data_fig$measured_data$Units[i], self$concentration_units, "Air", 25, chem_prop_dfx$MW)
          })
        }
      }
    }
  )
)


