#' @title Run a Johnson and Ettinger model simulation using the vapintr data
#'   import template
#'
#' @description This function is a wrapper function for `runJE()` that supports
#'   simulations using vapintr's data import template. See the following
#'   vignettes for more information.
#'
#'   `vignette("DeterministicSimulations", package = "vapintr")`
#'
#'   `vignette("StochasticSimulations", package = "vapintr")`
#'
#' @param template_data A list of data frames with parameter inputs for the
#'   Johnson and Ettinger model. This list can be produced using
#'   `importTemplateData()` and is the same as the output from that function.
#'
#' @param contaminant An optional parameter identifying the name of the
#'   contaminant to be simulated from the imported data. The default value is
#'   `NA`. If a contaminant name is not provided, the function will attempt to
#'   run the JEM for all contaminants in the imported template data.
#'
#' @param logID The LogID of a soil strata log from the imported data set to use
#'   in the simulation. This parameter is optional and has a default value of
#'   `NA`. If it is left as `NA` in a deterministic simulation, the simulation
#'   will use the first LogID identified in the soil strata log data frame
#'   imported from the template. If `NA` in a stochastic simulation, the
#'   simulation will randomly sample from the supplied LogIDs for each Monte
#'   Carlo iteration.
#'
#' @param use_aggregate_data_in_det_simulation True/False parameter for
#' deterministic simulations that indicates whether to aggregate the data by
#' contaminant and sample location ID (True) or to keep contaminant records
#' separate (False). See `vignette("DeterministicSimulations", package =
#' "vapintr")` for more information.
#'
#' @param capture_warnings_and_errors True/False parameter indicating whether to
#'   capture warnings and errors associated with individual simulations or write
#'   them to the console. Default is FALSE.
#'
#' @returns A named output list with information to identify each JEM simulation
#'   run. In both deterministic and stochastic simulations, the JEM results from
#'   `runJE()` are stored in the `JEMResults` field of the output list elements,
#'   and the `Error` and `Warning` fields identify any errors or warnings thrown
#'   during each simulation. The other fields in each list element depend on the
#'   type of simulation performed and identify inputs associated with the
#'   results.
#'
#' @examples
#'
#' #Run a deterministic simulation
#' #Get import data file path
#' import_data_file_path <- system.file("extdata",
#' "Deterministic_Simulation_Example_Data.xlsx", package = "vapintr")
#'
#' #Import the file data
#' template_data <- importTemplateData(import_data_file_path)
#'
#' #Run the simulation
#' runTemplateData(template_data)
#'
#' #Run a stochastic simulation
#' import_data_file_path <- system.file("extdata",
#' "Stochastic_Simulation_Example_Data.xlsx", package = "vapintr")
#'
#' #Import the file data
#' template_data <- importTemplateData(import_data_file_path)
#'
#' #Run the simulation
#' runTemplateData(template_data)
#'
#' @export

runTemplateData = function(template_data, contaminant = NA, logID = NA, use_aggregate_data_in_det_simulation = FALSE, capture_warnings_and_errors = FALSE){

  #produce an error if template_data is not a list
  if(!is.list(template_data) || !length(template_data) == 6){
      stop("Error: \"template_data\" input parameter must be a list with six elements")
  }

  #extract the contaminant data
  contaminant_data_dfx <- template_data[[1]]
  #extract the building data
  building_data <- template_data[[2]]
  #extract the contaminant source data
  vadose_zone_data <- template_data[[3]]
  #extract the strata log data
  strata_logs_dfx <- template_data[[4]]
  #extract the settings
  settings_dfx <- template_data[[5]]
  #don't need to extract the reference air concentrations since we don't use them for anything in this function

  #Process the imported contaminant data
  contaminant_data <- getInputConc(contaminant_data_dfx, contaminant, settings_dfx$source_medium, settings_dfx$simulation_type, use_aggregate_data_in_det_simulation)

  det_strata_logs_warning_message <- NA

  #Subset the data down to just one boring log in a deterministic simulation or if a log was provided
  if(settings_dfx$simulation_type == "DET" || !is.na(logID)){
    #capture a flag indicating that a warning should be triggered if
    # 1) it's a deterministic simulation
    # 2) no logID was provided, and
    # 3) the data frame included more than one log ID
    if(settings_dfx$simulation_type == "DET" && is.na(logID) && length(unique(strata_logs_dfx$LogID)) > 1){
      det_strata_logs_warning_message <- paste("Warning: More than one unique LogID was loaded in the data import template's StrataLogs sheet. Only the first LogID recorded (",strata_logs_dfx$LogID[[1]],") was used in the simulation.", sep="")
    }

    #For a deterministic simulation, if the boring log isn't specified, take the first one from strata_logs_dfx
    strata_logs_dfx <- filter(strata_logs_dfx, .data$LogID == ifelse(!is.na(logID), logID, strata_logs_dfx$LogID[[1]]))
  }

  if(settings_dfx$simulation_type == "DET"){

    jem_results <- lapply(1:nrow(contaminant_data), function(cont_index){

      cont_record <- contaminant_data[cont_index, ]

      # initialize variables
      errorMessage <- NULL
      warningMessage <- NULL

      if(capture_warnings_and_errors){

        runJEOutput <- withCallingHandlers(

          tryCatch({

            runJE(cont_record, building_data, vadose_zone_data, strata_logs_dfx, settings_dfx)

          }, error = function(err){

            errorMessage <<- err$message

          })#end of trycatch call
          , warning = function(war){

            warningMessage <<- war$message
            invokeRestart("muffleWarning")
          }
        )

        #Add warning message for multiple strata logs in deterministic simulations to tracked messages
        if(!is.na(det_strata_logs_warning_message)){
          warningMessage <- ifelse(is.null(warningMessage), det_strata_logs_warning_message, paste(warningMessage, det_strata_logs_warning_message, " \n\n", sep = ""))
        }
      }
      else
      {
        #Throw warning message for multiple strata logs in deterministic simulations
        if(!is.na(det_strata_logs_warning_message)){warning(det_strata_logs_warning_message)}
        runJEOutput <- runJE(cont_record, building_data, vadose_zone_data, strata_logs_dfx, settings_dfx)
      }

      if(!use_aggregate_data_in_det_simulation){
        cont_results <- list(Contaminant = cont_record$Contaminant,
             Concentration = cont_record$Concentration,
             Units = cont_record$Units,
             SampleLocationID = cont_record$SampleLocationID,
             SampleDate = cont_record$SampleDate,
             JEMResults = runJEOutput,
             Error = errorMessage,
             Warning = warningMessage
        )
      }
      else
      {
        cont_results <- list(Contaminant = cont_record$Contaminant,
                             Concentration = cont_record$Cmedium,
                             Units = cont_record$Units,
                             JEMResults = runJEOutput,
                             Error = errorMessage,
                             Warning = warningMessage
        )
      }

      return(cont_results)
    })
  }
  else
  {
    jem_results <- lapply(1:length(contaminant_data), function(cont_index){

      # initialize variables
      errorMessage <- NULL
      warningMessage <- NULL

      if(capture_warnings_and_errors){

        runJEOutput <- withCallingHandlers(

        tryCatch({

           runJE(contaminant_data[[cont_index]], building_data, vadose_zone_data, strata_logs_dfx, settings_dfx)

        }, error = function(err){

          errorMessage <<- err$message

        })#end of trycatch call
        , warning = function(war){

          warningMessage <<- war$message
          invokeRestart("muffleWarning")
        })

      } else {
        runJEOutput <- runJE(contaminant_data[[cont_index]], building_data, vadose_zone_data, strata_logs_dfx, settings_dfx)
      }

      cont_results <- list(Contaminant = contaminant_data[[cont_index]]$getDataFrameOfProperties()$name,
                           JEMResults = runJEOutput,
                           Error = errorMessage,
                           Warning = warningMessage)
      return(cont_results)
    })
  }

  return(jem_results)
}
