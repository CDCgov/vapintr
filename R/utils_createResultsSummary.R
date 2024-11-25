#' @title Create the results summary table
#'
#' @description Creates the main data table displayed in the shiny app with
#'   summary statistics from the completed JEM simulations
#'
#' @param settings_data A data frame of simulation settings
#'
#' @param jem_data Calculated JEM results within the shiny app
#'
#' @returns A data frame with JEM simulation output results for display within
#'   the shiny app
#'
#' @noRd

createResultsSummary <- function(settings_data, jem_data){

  #Set output number of sig figs
  sigfigs <- 3

  #Get number of contaminants
  n_contaminants <- length(jem_data)

  #deterministic simulation
  if(settings_data$simulation_type == "DET"){

    source_medium <- settings_data$source_medium

    col_names <- c("Contaminant",
                   paste(source_medium, "Concentration"),
                   paste(source_medium, "Concentration Units"),
                   "Sample Location ID",
                   "Sample Date",
                   "Indoor Air Concentration<br/>(&mu;g/m<sup>3</sup>)",
                   "Indoor Air Concentration<br/>(ppb)",
                   "Warnings or Errors")

    results_summary <- data.frame(matrix(NA, nrow = n_contaminants, ncol = length(col_names)))
    names(results_summary) <- col_names

    for(cont_index in 1:n_contaminants){

      cont_data <- jem_data[[cont_index]]

      if(!is.null(cont_data$Error)){
        error_warning_message <- paste("ERRORFLAG_",cont_data$Error,sep="")
        cont_data$JEMResults <- assignErrorResultsValues(settings_data)
      }
      else if (!is.null(cont_data$Warning)){
        error_warning_message <- cont_data$Warning
      }
      else {
        error_warning_message <- ""
      }

      contaminant_results <- pullResults(cont_data$JEMResults)$output

      results_summary[cont_index, col_names[1]] <- cont_data$Contaminant
      results_summary[cont_index, col_names[2]] <- cont_data$Concentration
      results_summary[cont_index, col_names[3]] <- gsub("3", "<sup>3</sup>", gsub("u", "&mu;", cont_data$Units))
      results_summary[cont_index, col_names[4]] <- cont_data$SampleLocationID
      results_summary[cont_index, col_names[5]] <- format(as.Date(cont_data$SampleDate, origin = "1899-12-30"), "%m-%d-%Y")
      results_summary[cont_index, col_names[6]] <- signif(contaminant_results["Cia", "Value"], sigfigs)
      results_summary[cont_index, col_names[7]] <- signif(contaminant_results["Cia_ppb", "Value"], sigfigs)
      results_summary[cont_index, col_names[8]] <- error_warning_message
    }
  }
  #stochastic simulation
  else
  {
    #Create an array for storing the results with one row per contaminant
    #jem_data should be a list

    col_names <- c("Contaminant",
                   "Indoor Air Concentration<br/>50th Percentile<br/>(&mu;g/m<sup>3</sup>)",
                   "Indoor Air Concentration<br/>95th Percentile<br/>(&mu;g/m<sup>3</sup>)",
                   "Indoor Air Concentration<br/>50th Percentile<br/>(ppb)",
                   "Indoor Air Concentration<br/>95th Percentile<br/>(ppb)",
                   "Warnings or Errors")

    results_summary <- data.frame(matrix(NA, nrow = n_contaminants, ncol = length(col_names)))
    names(results_summary) <- col_names

    for(cont_index in 1:n_contaminants){

      cont_data <- jem_data[[cont_index]]

      contaminant_name <- cont_data$Contaminant

      if(!is.null(cont_data$Error)){
        error_warning_message <- paste("ERRORFLAG_",cont_data$Error,sep="")
        cont_data$JEMResults <- assignErrorResultsValues(settings_data)
      }
      else if (!is.null(cont_data$Warning)){
        error_warning_message <- cont_data$Warning
      }
      else {
        error_warning_message <- ""
      }

      contaminant_results <- pullResults(cont_data$JEMResults)$output

      results_summary[cont_index, col_names[1]] <- contaminant_name
      results_summary[cont_index, col_names[2]] <- signif(contaminant_results["Cia", "Median"], sigfigs)
      results_summary[cont_index, col_names[3]] <- signif(contaminant_results["Cia", "Percentile95"], sigfigs)
      results_summary[cont_index, col_names[4]] <- signif(contaminant_results["Cia_ppb", "Median"], sigfigs)
      results_summary[cont_index, col_names[5]] <- signif(contaminant_results["Cia_ppb", "Percentile95"], sigfigs)
      results_summary[cont_index, col_names[6]] <- error_warning_message
    }
  }

  #Replace any NA results with "No Value"
  results_summary[is.na(results_summary)] <- "No Value"

  return(results_summary)

}
