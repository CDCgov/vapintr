#' @title Create export summary statistics
#'
#' @description Creates the data table exported in the shiny app with summary
#'   statistics from JEM stochastic simulations
#'
#' @param data_upload Data uploaded to the shiny app
#'
#' @param jem_data Calculated JEM results within the shiny app
#'
#' @returns A list with one data frame per sheet to be written in the output
#'   Excel file. Exported sheets include the main data table, a Read Me sheet,
#'   and a sheet of simulation errors and warnings.
#'
#' @noRd

createExportSummaryStatistics <- function(data_upload, jem_data){

  settings_data_dfx <- data_upload()[[5]]

  #Statistics only calculated for Monte Carlo simulations
  if(settings_data_dfx$simulation_type == "MC"){

    #Get the number of contaminants simulated
    nContaminants <- length(jem_data())

    #Create lists of the output table statistics for each contaminant
    statistics_lx <- lapply(1:nContaminants, function(cont_index){

      #Get the record of interest
      res <-jem_data()[[cont_index]]

      if(!is.null(res$Error)){
        res$JEMResults <- assignErrorResultsValues(settings_data_dfx)
      }

      #Get statistics calculated by pullResults for each contaminant
      pull_results_output <- pullResults(res$JEMResults)

      #Build a data frame with the results
      result_data_dfx <- rbind(pull_results_output$output,
                               pull_results_output$abc_parameter,
                               pull_results_output$subsurface,
                               pull_results_output$building,
                               pull_results_output$source)

      #Create new column names
      new_column_names <- paste(result_data_dfx$Parameter, paste("(", result_data_dfx$Units, ")", sep = ""))

      #transpose data
      result_data_dfx <- t(result_data_dfx)

      #clean column names
      colnames(result_data_dfx) <- new_column_names

      #Add a statistics field
      result_data_dfx <- cbind(data.frame("Statistic" = rownames(result_data_dfx)),result_data_dfx)

      #Remove unneeded rows
      rows_to_remove <- c("Parameter", "Units")
      result_data_dfx <- result_data_dfx[!(row.names(result_data_dfx) %in% rows_to_remove),]

      #Get the contaminant as a data frame
      contaminant <- data.frame(res$Contaminant)

      #Clean up the contaminant field name
      colnames(contaminant) <- sub("res.", "", colnames(contaminant))

      #Return a data frame with the contaminant statistics
      return(cbind(contaminant, result_data_dfx))

    })

    #Combine the list results into one data frame
    statistics_dfx <- as.data.frame(do.call(rbind, statistics_lx))

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

    #Create the read me sheet
    read_me_dfx <- data.frame(Field = "Contaminant", Definition = "The contaminant name")
    read_me_dfx <- rbind(read_me_dfx, c("Statistic", "The reported statistic for the output variables in this row"))

    #Add standard definitions included in all output files
    read_me_dfx <- rbind(read_me_dfx, getStandardReadMeDefinitions())

    #Return a list with the elements needed in write_xlsx
    return(list("SimulationStatistics" = statistics_dfx,
                "ErrorsAndWarnings" = errors_and_warnings_dfx,
                "ReadMe" = read_me_dfx))
  }
  else
  {
    return(NULL)
  }
}
