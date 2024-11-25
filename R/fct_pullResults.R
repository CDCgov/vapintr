#' Parse output from `runJE()` into multiple thematic tables.
#'
#' @description Takes output from `runJE()` and provides a list of
#'   five tables that summarize the inputs, intermediate calculations, and
#'   output of the Johnson and Ettinger model.
#'
#' @param simobj A data frame or mc object output from `runJE()`.
#'
#' @param i The Monte Carlo iteration number. Entering a value for i allows
#'   for the results of a specific Monte Carlo iteration to be selected. If this
#'   parameter is provided for a stochastic simulation, the tables produced are
#'   the same as those produced for a deterministic simulation.
#'
#' @return A list of five data frames:
#' * "source" describes source characteristics
#' * "building" describes building characteristics
#' * "subsurface" describes intermediate subsurface calculations
#' * "abc_parameter" describes the Johnson and Ettinger model A, B, and C
#' parameters
#' * "output" identifies the attenuation factor (alpha) and the final
#' concentration outputs
#'
#' @examples
#' #Get source data
#' contaminant_data <- det_jem_sim_example_data[[1]]
#' building_data <- det_jem_sim_example_data[[2]]
#' vadose_zone_data <- det_jem_sim_example_data[[3]]
#' strata_logs_data <- det_jem_sim_example_data[[4]]
#' settings_data <- det_jem_sim_example_data[[5]]
#'
#' #Run the Johnson and Ettinger Model
#' runJE_output <- runJE(contaminant_data, building_data, vadose_zone_data,
#' strata_logs_data, settings_data)
#'
#' #Get the output tables
#' pullResults(runJE_output)
#'
#' @export


pullResults = function(simobj, i = NA){

  if(is(simobj, "mc")){
    #Get summary data and include the min, 95th percentile, and max
    mc_summary_lx <- summary(simobj, lim = c(0, 0.95, 1))

    #Turn the object into a data frame and get just the needed columns
    mc_summary_dfx <- data.frame(matrix(unlist(mc_summary_lx), nrow=length(mc_summary_lx), byrow = TRUE))
    rownames(mc_summary_dfx) <- names(mc_summary_lx)
    colnames(mc_summary_dfx) <- c("Median", "Mean", "Minimum", "Percentile95", "Maximum")
    mc_summary_dfx <- mc_summary_dfx[,c(3, 1, 4, 5)]

    simulation_type <- "MC"
  }
  else
  {
    simulation_type <- "DET"
    i <- ifelse(is.na(i), 1, i)
  }

  ################ Source characteristics #############
  parameter_names <- c("Cmedium", "Cs", "percent_sat", "Ls", "Ts")

  source_characteristics_dfx <- data.frame(
    Parameter = gsub("percent_sat", "%Sat", parameter_names),
    Units = c(ifelse(simobj$is_groundwater_source[[1]],"ug/L","ug/m3"), "ug/m3", "%", "m", "deg C")
  )

  if(simulation_type == "DET" || !is.na(i)){
    output_values <- data.frame(Value = sapply(parameter_names, function(par_name){simobj[[par_name]][[i]]}))
  } else {
    output_values <- mc_summary_dfx[parameter_names, ]
  }

  output_values[sapply(output_values,is.infinite)] <- NA
  source_characteristics_dfx <- cbind(source_characteristics_dfx, output_values)

  ################ Building characteristics ###############
  parameter_names <- c("ach", "Hb", "eta", "Abf", "Lb", "Lf", "Qb", "Qsoil")

  building_inputs_dfx <- data.frame(
    Parameter = parameter_names,
    Units = c("1/hr", "m", "-", "m2", "m", "m", "m3/hr", "m3/hr")
  )

  if(simulation_type == "DET" || !is.na(i)){
    output_values <- data.frame(Value = sapply(parameter_names, function(par_name){simobj[[par_name]][[i]]}))
  } else {
    output_values <- mc_summary_dfx[parameter_names, ]
  }

  output_values[sapply(output_values,is.infinite)] <- NA
  building_inputs_dfx <- cbind(building_inputs_dfx, output_values)

  ################# Subsurface zone heights and diffusivities ############
  parameter_names <- c("DeffT", "DeffBF", "DeffCZ", "DeffUZ", "hcz", "huz")

  subsurface_calcs_dfx <- data.frame(
    Parameter = parameter_names,
    Units = c("cm2/s", "cm2/s", "cm2/s", "cm2/s", "m", "m")
  )

  if(simulation_type == "DET" || !is.na(i)){
    output_values <- data.frame(Value = sapply(parameter_names, function(par_name){simobj[[par_name]][[i]]}))
  } else {
    output_values <- mc_summary_dfx[parameter_names, ]
  }

  output_values[sapply(output_values,is.infinite)] <- NA
  subsurface_calcs_dfx <- cbind(subsurface_calcs_dfx, output_values)

  ################# Calculated ABC Parameter values ######################
  parameter_names <- c("Aparam", "Bparam", "Cparam")

  abc_parameter_dfx <- data.frame(
    Parameter = parameter_names,
    Units = c("-", "-", "-")
  )

  if(simulation_type == "DET" || !is.na(i)){
    output_values <- data.frame(Value = sapply(parameter_names, function(par_name){simobj[[par_name]][[i]]}))
  } else {
    output_values <- mc_summary_dfx[parameter_names, ]
  }

  output_values[sapply(output_values,is.infinite)] <- NA
  abc_parameter_dfx <- cbind(abc_parameter_dfx, output_values)

  ################# Calculated Output values ############################
  parameter_names <- c("alpha", "Cia", "Cia_ppb", "Css", "Css_ppb")

  output_dfx <- data.frame(
    Parameter = parameter_names,
    Units = c("-", "ug/m3", "ppbv", "ug/m3", "ppbv")
  )

  if(simulation_type == "DET" || !is.na(i)){
    output_values <- data.frame(Value = sapply(parameter_names, function(par_name){simobj[[par_name]][[i]]}))
  } else {
    output_values <- mc_summary_dfx[parameter_names, ]
  }

  output_values[sapply(output_values,is.infinite)] <- NA
  output_dfx <- cbind(output_dfx, output_values)

  ################# Create list summarizing outputs #####################
  list(source = source_characteristics_dfx,
       building = building_inputs_dfx,
       subsurface = subsurface_calcs_dfx,
       abc_parameter = abc_parameter_dfx,
       output = output_dfx)
}
