#' @title Get the contaminant concentration input for modeling
#'
#' @description Creates the contaminant concentration input required to run the
#'   Johnson and Ettinger model. This function returns a data frame of
#'   concentrations to model for deterministic simulations and a list of
#'   concentration distribution inputs to model for stochastic simulations. See
#'   the following vignettes for more information.
#'
#'   `vignette("DeterministicSimulations", package ="vapintr")`
#'
#'   `vignette("StochasticSimulations", package = "vapintr")`
#'
#' @param contaminant_data_dfx A data frame of measured contaminant
#'   concentration data imported from the data import template. This data frame
#'   should have the same fields as the concentration data frame exported by
#'   `importTemplateData()`.
#'
#' @param contaminant The name of the contaminant to be simulated. If inputs
#' are desired for more than one contaminant, enter `NA`.
#'
#' @param source_medium The contaminant source medium. Choose from
#'   "Groundwater", "Exterior Soil Gas", or "Subslab Soil Gas".
#'
#' @param simulation_type The simulation type. Choose from "DET" for
#'   deterministic simulations or "MC" for stochastic simulations.
#'
#' @param use_aggregate_data_in_det_simulation True/False parameter for
#' deterministic simulations that indicates whether to aggregate the data by
#' contaminant and sample location ID (True) or to keep contaminant records
#' separate (False). See `vignette("DeterministicSimulations", package =
#'   "vapintr")` for more information.
#'
#' @return For a deterministic simulation, the function will return a data frame
#' of concentration data with the numeric value and units of the concentration
#' to simulate stored in the `Cmedium` and `Units` fields, respectively. If
#' contaminant data were aggregated, the data frame will contain one row per
#' contaminant with detections in the input data set. If the data were not
#' aggregated, the data frame will contain one row per detected contaminant
#' record.
#'
#' For a stochastic simulation, the output is a list of `JEMParamDist` objects,
#' each representing a distribution of concentrations for one contaminant. The
#' contaminant associated with each object is identified in the object's `name`
#' property.
#'
#' @examples
#' #Assign example concentration data to a data frame
#' concentration_data <- stoc_jem_sim_example_data[[1]]
#'
#' #Get deterministic simulation concentration inputs for individual records
#' getInputConc(concentration_data, NA, "Groundwater", "DET", FALSE)
#'
#' #Get deterministic simulation concentration inputs aggregated by contaminant
#' getInputConc(concentration_data, NA, "Groundwater", "DET", TRUE)
#'
#' #Get an output list of concentration inputs for a stochastic simulation
#' getInputConc(concentration_data, NA, "Groundwater", "MC")
#'
#' @export

getInputConc <- function(contaminant_data_dfx, contaminant, source_medium, simulation_type, use_aggregate_data_in_det_simulation = FALSE){

  #Filter to just the records in the source medium of interest
  contaminant_data_dfx <- contaminant_data_dfx %>% filter(.data$Medium == source_medium)

  #If a contaminant is provided, preprocess the data to get just the records for the contaminant of interest
  if(!is.na(contaminant)){

    # #produce an error if the contaminant specified in the arguments is not in the list of standard chemical names
    # if (!(contaminant %in% chem_data$Chemical)){
    #   stop("Error: The entered contaminant name was not found in the list of supported chemicals. Supported chemical names may be found in the `Chemical` field of the `vapintr::chem_data` data frame.")
    # }

    #produce an error if the contaminant specified in the arguments is not in the contaminant data frame.
    if (!(contaminant %in% contaminant_data_dfx$Contaminant)){
      stop(paste("Error: The imported contaminant data set does not contain records for ", contaminant, sep=""))
    }

    #Filter to just the contaminant records in the source medium of interest
    contaminant_data_dfx <- contaminant_data_dfx %>% filter(.data$Contaminant == contaminant)
  }

  #Confirm that contaminant units are all the same
  unique_units <- unique(contaminant_data_dfx$Units)
  if (!length(unique_units) == 1){
    stop(paste("Error: The ", ifelse(is.na(contaminant), "", paste0(tolower(contaminant), " ")), tolower(source_medium), " records passed to the getInputConc function have more than one unit.", sep=""))
  }

  #Do some data cleaning to match contaminant name and CASRN formatting in the JEM
  contaminant_data_dfx <- contaminant_data_dfx %>%
    dplyr::mutate(cleanedCASRN = gsub("^0+", "",.data$CASRN)) %>%
    dplyr::mutate(CASRN = .data$cleanedCASRN)

  for(cont_index in seq_along(contaminant_data_dfx$cleanedCASRN)) {

    standard_contaminant_name <- tryCatch({
      chem_props_dfx <- getChemicalProperties(Chemical = NA, CAS = contaminant_data_dfx$cleanedCASRN[cont_index])
      chem_props_dfx$Chemical
    }, error = function(err){
      return(contaminant_data_dfx$Contaminant[cont_index])
    })

    contaminant_data_dfx$Contaminant[cont_index] <- standard_contaminant_name
  }

  contaminant_data_dfx <- contaminant_data_dfx %>% dplyr::select(-"cleanedCASRN")

  #Transform the input data into a distribution

  #Get the subset of detected records
  detected_records_dfx <- contaminant_data_dfx %>%
    dplyr::filter(.data$DetectedFlag == TRUE)

  #If 0 throw an error, model isn't set up to handle all non-detects
  if (nrow(detected_records_dfx) == 0){
    errorMessage <- "Error: The imported contaminant data set does not include any detections"
    errorMessage <- paste(errorMessage, if(is.na(contaminant)) "." else paste(" for", tolower(contaminant), sep = " "), sep = "")
    stop(errorMessage)
  }

  #For deterministic simulations only, return all records for running sims across each record individually
  if(simulation_type == "DET" && !use_aggregate_data_in_det_simulation)
  {
    contaminant_data <- detected_records_dfx %>%
      mutate(Cmedium = .data$Concentration)
  }
  #Monte Carlo simulations or deterministic sims where aggregation is used
  else
  {

    contaminant_dist_data_dfx <- detected_records_dfx %>%
          group_by(.data$Contaminant, .data$CASRN,  .data$SampleLocationID) %>%
          dplyr::summarise(min = min(.data$Concentration, na.rm = TRUE),
                           median = median(.data$Concentration, na.rm = TRUE),
                           max = max(.data$Concentration, na.rm = TRUE)) %>%
          group_by(.data$Contaminant, .data$CASRN) %>%
          dplyr::summarise(min = min(.data$min),
                           median = median(.data$median),
                           max = max(.data$max))

    #Build a dataframe with the contaminant information and use median of the medians as the concentration
    if(simulation_type == "DET")
    {
      contaminant_data <- contaminant_dist_data_dfx %>%
        mutate(Cmedium = .data$median, Units = unique_units) %>%
        select(.data$Contaminant, .data$Cmedium, .data$Units)
    }
    #Build a list of JEMParamDist objects for a stochastic simulation
    else
    {
      contaminant_data <- lapply(1:nrow(contaminant_dist_data_dfx), function(cont_index){

        cont_record <- contaminant_dist_data_dfx[cont_index, ]

        #If min = median = max, assign a constant
        if (cont_record$min[1] == cont_record$median[1] &
            cont_record$median[1] == cont_record$max[1]) {

          contaminant_data <- JEMParamDist$new("Cmedium",
                                               name = cont_record$Contaminant[1],
                                               units = unique_units,
                                               dist_type = "Constant",
                                               constant = cont_record$median[1])

          #Else, use a PERT distribution with the median set as the mode
        } else {
          contaminant_data <- JEMParamDist$new("Cmedium",
                                               name = cont_record$Contaminant[1],
                                               units = unique_units,
                                               dist_type = "PERT",
                                               minimum = cont_record$min[1],
                                               mode = cont_record$median[1],
                                               maximum = cont_record$max[1])
        }

      })
    }
  }

  return(contaminant_data)

}
