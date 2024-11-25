#' @title Create all simulation data to export
#'
#' @description Creates the data table exported in the shiny app with results
#'   from all JEM simulations
#'
#' @param data_upload Data uploaded to the shiny app
#'
#' @param jem_data Calculated JEM results within the shiny app
#'
#' @returns A list with one data frame per sheet to be written in the output
#'   Excel file. Exported sheets include the main data table, a Read Me sheet,
#'   and (in stochastic simulations) a sheet of simulation errors and warnings.
#'
#' @noRd

createAllSimulationDataExport <- function(data_upload, jem_data){

  settings_data_dfx <- data_upload()[[5]]
  strata_logs_dfx <- data_upload()[[4]]
  unique_logs <- unique(strata_logs_dfx$LogID)

  ####################### Build read me tab ##########################
  isDET <- settings_data_dfx$simulation_type == "DET" #Is the simulation deterministic?
  isGW <- settings_data_dfx$source_medium == "Groundwater" #Did the simulation have a groundwater source?

  read_me_dfx <- data.frame(Field = "Contaminant", Definition = "The contaminant name")

  #Record info specific to each simulation type
  if(isDET){
    sourceConcentrationText <- ifelse(isGW, "GroundwaterConcentration", "SoilGasConcentration")
    read_me_dfx <- rbind(read_me_dfx, c(sourceConcentrationText, paste0("The numeric value of the sampled ", ifelse(isGW, "groundwater", "soil gas")," concentration")))
    read_me_dfx <- rbind(read_me_dfx, c(paste0(sourceConcentrationText, "Units"), paste0("The units associated with the sampled ", ifelse(isGW, "groundwater", "soil gas")," concentration")))
    read_me_dfx <- rbind(read_me_dfx, c("SampleLocationID", "The ID of the location where the sample was collected"))
    read_me_dfx <- rbind(read_me_dfx, c("SampleDate", "The date when the sample was collected"))
    read_me_dfx <- rbind(read_me_dfx, c("SourceMedium", "The medium in which the sample was collected and the source medium for the Johnson and Ettinger model simulation"))
    read_me_dfx <- rbind(read_me_dfx, c("SoilStrataLogID", "The ID of the soil strata log used in the simulation"))
  }
  else
  {
    read_me_dfx <- rbind(read_me_dfx, c("MonteCarloIteration", "The Monte Carlo iteration number"))
    read_me_dfx <- rbind(read_me_dfx, c("SoilStrataLogID", "The ID of the soil strata log used in the Monte Carlo iteration"))
  }

  #Add standard definitions included in all output files
  read_me_dfx <- rbind(read_me_dfx, getStandardReadMeDefinitions())

  ####################### Build deterministic data frame #####################
  if(isDET){

    #Get number of records processed
    nResults <- length(jem_data())

    #Create a list with all the elements of each simulation stored in a single dataframe
    results_lx <- lapply(1:nResults, function(result_index){

      #Get the record of interest
      res <-jem_data()[[result_index]]

      #Process cases with errors
      if(!is.null(res$Error)){
        res$JEMResults <- assignErrorResultsValues(settings_data_dfx)
      }

      #Process nulls in warnings and errors, since otherwise the dataframe will be empty
      if(is.null(res$Error)) {
        res$Error <- ""
      }
      if(is.null(res$Warning)) {
        res$Warning <- ""
      }

      #Convert the needed data into a format that will allow for combining into a single data frame
      sample_info <- data.frame(res$Contaminant, res$Concentration, res$Units, res$SampleLocationID, res$SampleDate)
      jem_results <- as.data.frame(res$JEMResults)
      errors_and_warnings <- data.frame(res$Error, res$Warning)

      #Clean up the column names
      colnames(sample_info) <- sub("res.", "", colnames(sample_info))
      colnames(errors_and_warnings) <- sub("res.", "", colnames(errors_and_warnings))

      #Combine the results into a single data frame
      return(cbind(sample_info, jem_results, errors_and_warnings))

    })

    #Convert the list results into a data frame
    results_dfx <- as.data.frame(do.call(rbind, results_lx))

    #Clean up the data frame properties
    results_dfx <- results_dfx %>%
      dplyr::rename(!!ifelse(isGW, "GroundwaterConcentration", "SoilGasConcentration") := .data$Concentration) %>%
      dplyr::rename(!!ifelse(isGW, "GroundwaterConcentrationUnits", "SoilGasConcentrationUnits") := .data$Units) %>%
      dplyr::mutate(SampleDate = format(as.Date(.data$SampleDate, origin = "1899-12-30"), "%m-%d-%Y")) %>%
      dplyr::mutate(SourceMedium = settings_data_dfx$source_medium) %>%
      dplyr::mutate(SoilStrataLogID = unique_logs[results_dfx$sampled_log_index]) %>%
      dplyr::relocate(.data$SourceMedium, .after = .data$SampleDate) %>%
      dplyr::relocate(.data$SoilStrataLogID, .after = .data$SourceMedium) %>%
      dplyr::select(-.data$is_groundwater_source, -.data$sampled_log_index)

    #Add units to the parameter columns
    results_dfx <- addAllSimulationDataUnits(results_dfx)

    #return the results in a format useable within write_xlsx
    return(list("AllSimulationData" = results_dfx,
                "ReadMe" = read_me_dfx))
  }
  ##################### Build stochastic data frame ##########################
  else
  {

    #Get the number of contaminants simulated
    nContaminants <- length(jem_data())

    #Create a list with all the information to be output stored in a single data frame
    results_lx <- lapply(1:nContaminants, function(result_index){

      #Get the record of interest
      res <-jem_data()[[result_index]]

      #Get the contaminant
      contaminant <- data.frame(res$Contaminant)

      if(!is.null(res$Error)){
        res$JEMResults <- assignErrorResultsValues(settings_data_dfx)
      }

      jem_results <- as.data.frame(unmc(res$JEMResults))

      #Clean up the field names
      colnames(contaminant) <- sub("res.", "", colnames(contaminant))

      #Combine into a single data frame
      return(cbind(contaminant, data.frame(MonteCarloIteration = 1:nrow(jem_results)), jem_results))

    })

    #Combine the list results into one data frame
    results_dfx <- as.data.frame(do.call(rbind, results_lx))

    #Clean up the data parameters
    results_dfx <- results_dfx %>%
      dplyr::mutate(SoilStrataLogID = unique_logs[.data$sampled_log_index]) %>%
      dplyr::relocate(.data$SoilStrataLogID, .after = .data$MonteCarloIteration) %>%
      dplyr::select(-.data$is_groundwater_source, -.data$sampled_log_index)

    #Add units to the parameter columns
    results_dfx <- addAllSimulationDataUnits(results_dfx)

    #Make a list of errors and warnings in each simulation
    errors_and_warnings_lx <- lapply(1:nContaminants, function(result_index){

      #Get the record of interest
      res <-jem_data()[[result_index]]

      #Fill in something for null values so that the dataframe is populated
      if(is.null(res$Error)) {res$Error <- ""}
      if(is.null(res$Warning)) {res$Warning <- ""}

      #Get data frames of results
      contaminant <- data.frame(res$Contaminant)
      errors_and_warnings <- data.frame(res$Error, res$Warning)

      #Clean up column names
      colnames(contaminant) <- sub("res.", "", colnames(contaminant))
      colnames(errors_and_warnings) <- sub("res.", "", colnames(errors_and_warnings))

      #Return the data frame
      return(cbind(contaminant, errors_and_warnings))

    })

    #Convert the list of errors and warnings into a data frame
    errors_and_warnings_dfx <- as.data.frame(do.call(rbind, errors_and_warnings_lx))

    #Return a list with the elements needed in write_xlsx
    return(list("AllSimulationData" = results_dfx,
                "ErrorsAndWarnings" = errors_and_warnings_dfx,
                "ReadMe" = read_me_dfx))
  }

}

#' @title Add all simulation data units
#'
#' @description Helper function for adding units to the parameter column names
#'
#' @param results_dfx Data frame of JEM simulation output data
#'
#' @returns The results_dfx data frame with units added to the column names
#'
#' @noRd

addAllSimulationDataUnits <- function(results_dfx){

  results_dfx <- results_dfx %>%
    rename(!!"alpha (-)" :=	.data$alpha) %>%
    rename(!!"Cia (ug/m3)" :=	.data$Cia) %>%
    rename(!!"Cia_ppb (ppbv)" :=	.data$Cia_ppb) %>%
    rename(!!"Css (ug/m3)" :=	.data$Css) %>%
    rename(!!"Css_ppb (ppbv)" := .data$Css_ppb) %>%
    rename(!!"Aparam (-)" := .data$Aparam) %>%
    rename(!!"Bparam (-)" :=	.data$Bparam) %>%
    rename(!!"Cparam (-)" := .data$Cparam) %>%
    rename(!!"huz (m)" :=	.data$huz) %>%
    rename(!!"hcz (m)" :=	.data$hcz) %>%
    rename(!!"DeffUZ (cm2/s)" :=	.data$DeffUZ) %>%
    rename(!!"DeffCZ (cm2/s)" := .data$DeffCZ) %>%
    rename(!!"DeffT (cm2/s)" :=	.data$DeffT) %>%
    rename(!!"DeffBF (cm2/s)" := .data$DeffBF) %>%
    rename(!!"ach (1/hr)" :=	.data$ach) %>%
    rename(!!"Hb (m)" := .data$Hb) %>%
    rename(!!"eta (-)" :=	.data$eta) %>%
    rename(!!"Abf (m2)" := .data$Abf) %>%
    rename(!!"Lb (m)" :=	.data$Lb) %>%
    rename(!!"Lf (m)" := .data$Lf) %>%
    rename(!!"Qb (m3/hr)" := .data$Qb) %>%
    rename(!!"Qsoil (m3/hr)" :=	.data$Qsoil) %>%
    rename(!!"Cmedium (ug/L)" :=	.data$Cmedium) %>%
    rename(!!"Cs (ug/m3)" := .data$Cs) %>%
    rename(!!"%Sat (%)" := .data$percent_sat) %>%
    rename(!!"Ls (m)" := .data$Ls) %>%
    rename(!!"Ts (deg C)" := .data$Ts)

  return(results_dfx)
}

#' @title Get standard read me definitions
#'
#' @description Gets standard definitions for the parameters in the Read Me
#'   sheet of the exported simulation data Excel file
#'
#' @returns A data frame with definitions for the Excel file Read Me sheet
#'
#' @noRd

getStandardReadMeDefinitions <- function(){

  read_me_dfx <- data.frame(Field = "alpha (-)", Definition = "The calculated attenuation factor, which relates the estimated contaminant indoor air concentration (Cia) to the contaminant source concentration (Cs)")
  read_me_dfx <- rbind(read_me_dfx, c("Cia (ug/m3)", "The contaminant concentration in indoor air predicted by the Johnson and Ettinger model using the calculated attenuation factor (alpha) and contaminant source concentration (Cs), in units of micrograms per cubic meter (ug/m3)"))
  read_me_dfx <- rbind(read_me_dfx, c("Cia_ppb (ppbv)", "The model-predicted indoor air concentration in units of parts per billion by volume (ppbv)"))
  read_me_dfx <- rbind(read_me_dfx, c("Css (ug/m3)","For buildings with a slab foundation, Css is the model-predicted contaminant concentration in subslab soil gas directly beneath the foundation, in units of micrograms per cubic meter (ug/m3). Css is not calculated for buildings with a dirt floor foundation."))
  read_me_dfx <- rbind(read_me_dfx, c("Css_ppb (ppbv)","The model-predicted subslab soil gas concentration in units of parts per billion by volume (ppbv)"))
  read_me_dfx <- rbind(read_me_dfx, c("Aparam (-)","A dimensionless internal parameter calculated to solve the Johnson and Ettinger model equation that represents the coefficient of diffusive transport for a basement with a dirt floor"))
  read_me_dfx <- rbind(read_me_dfx, c("Bparam (-)","A dimensionless internal parameter calculated to solve the Johnson and Ettinger model equation that represents the Peclet number for transport through the building foundation"))
  read_me_dfx <- rbind(read_me_dfx, c("Cparam (-)","A dimensionless internal parameter calculated to solve the Johnson and Ettinger model equation that represents convective transport from the subslab to the building"))
  read_me_dfx <- rbind(read_me_dfx, c("DeffT (cm2/s)","The total effective diffusivity of the contaminant from the subsurface source to the building foundation, in units of square centimeters per second (cm2/s)"))
  read_me_dfx <- rbind(read_me_dfx, c("DeffBF (cm2/s)","The contaminant's effective diffusivity in the soil layer immediately beneath the building foundation, in units of square centimeters per second (cm2/s)"))
  read_me_dfx <- rbind(read_me_dfx, c("DeffCZ (cm2/s)","The contaminant's effective diffusivity in the capillary zone, in units of square centimeters per second (cm2/s). Applicable only in groundwater simulations in which the capillary zone is simulated."))
  read_me_dfx <- rbind(read_me_dfx, c("DeffUZ (cm2/s)","The contaminant's effective diffusivity in the unsaturated zone, in units of square centimeters per second (cm2/s)"))
  read_me_dfx <- rbind(read_me_dfx, c("hcz (m)","The height of the capillary zone above the contaminant source, in meters (m). Applies only in groundwater simulations in which the capillary zone is simulated."))
  read_me_dfx <- rbind(read_me_dfx, c("huz (m)","The height of the unsaturated zone above the contaminant source, in meters (m)"))
  read_me_dfx <- rbind(read_me_dfx, c("ach (1/hr)","The rate of building air exchange with the outdoors, in units of air changes per hour (1/hr)"))
  read_me_dfx <- rbind(read_me_dfx, c("Hb (m)","The height of the interior building volume where contaminated air from the subsurface mixes with indoor air, in units of meters (m)"))
  read_me_dfx <- rbind(read_me_dfx, c("eta (-)","The dimensionless fraction of the foundation area with cracks"))
  read_me_dfx <- rbind(read_me_dfx, c("Abf (m2)","The surface area of the subsurface floor and walls, in units of square meters (m2)"))
  read_me_dfx <- rbind(read_me_dfx, c("Lb (m)","The depth from ground surface to the base of the building foundation, in units of meters (m)"))
  read_me_dfx <- rbind(read_me_dfx, c("Lf (m)","The thickness of the building foundation, in units of meters (m)"))
  read_me_dfx <- rbind(read_me_dfx, c("Qb (m3/hr)","The building ventilation rate, in units of cubic meters per hour (m3/hr)"))
  read_me_dfx <- rbind(read_me_dfx, c("Qsoil (m3/hr)","The average vapor flowrate into the building, in units of cubic meters per hour (m3/hr)"))
  read_me_dfx <- rbind(read_me_dfx, c("Cmedium (ug/L)","The contaminant concentration in the source medium. The value is reported in units of micrograms per liter (ug/L) for groundwater sources and micrgrams per cubic meter (ug/m3) for soil gas sources."))
  read_me_dfx <- rbind(read_me_dfx, c("Cs (ug/m3)","The gas-phase source concentration, in units of micrograms per cubic meter (ug/m3)"))
  read_me_dfx <- rbind(read_me_dfx, c("%Sat (%)","The percent (%) saturation of the contaminant in gas, calculated as Cs divided by the contaminant's pure saturated vapor concentration"))
  read_me_dfx <- rbind(read_me_dfx, c("Ls (m)","For groundwater simulations, this parameter represents the depth below ground surface to the top of the water table. For soil gas simulations, it represents the depth below ground surface at which the soil gas sample was collected. Ls is reported in meters (m)."))
  read_me_dfx <- rbind(read_me_dfx, c("Ts (deg C)","The average groundwater temperature in groundwater simulations or the average vadose zone temperature in soil gas simulations, in units of degrees Celsius (deg C)"))
  read_me_dfx <- rbind(read_me_dfx, c("Error","Error message issued by the application during the contaminant simulation"))
  read_me_dfx <- rbind(read_me_dfx, c("Warning","Warning message(s) issued by the application during the contaminant simulation"))

  return(read_me_dfx)
}
