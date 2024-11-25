#' @title Confirm that all inputs to `runJE()` are formatted correctly
#'
#' @description Checks that all required inputs are present to run the Johnson
#'   and Ettinger model using `runJE()`, and that all inputs are
#'   valid and in the proper format. The function input parameters are the
#'   same as those used by `runJE()`.
#'
#' @inheritParams runJE
#'
#' @returns The `checkModelInputs()` function will throw an error anytime the
#'   input parameters would prevent `runJE()` from executing successfully. In
#'   addition, it will also throw warnings if the parameter values are outside
#'   of typical ranges, and in other instances. The value returned by
#'   `checkModelInputs()` is a string that identifies the thrown warning
#'   messages. If there are not any warnings, the string returned will state "No
#'   errors or warnings in model inputs".
#'
#' @examples
#'   #Get example inputs to runJE()
#'   contaminant_data <- det_jem_sim_example_data[[1]]
#'   building_data <- det_jem_sim_example_data[[2]]
#'   vadose_zone_data <- det_jem_sim_example_data[[3]]
#'   strata_logs_data <- det_jem_sim_example_data[[4]]
#'   settings_data <- det_jem_sim_example_data[[5]]
#'
#'   #Run checkModelInputs with no warnings issued
#'   checkModelInputs(contaminant_data, building_data, vadose_zone_data,
#'   strata_logs_data, settings_data)
#'
#'   #Set the building air exchange rate to a large value so that
#'   #checkModelInputs issues a warning
#'   building_data$ach <- 5
#'
#'   #Run checkModelInputs and generate the warning message
#'   checkModelInputs(contaminant_data, building_data, vadose_zone_data,
#'   strata_logs_data, settings_data)
#'
#' @export

checkModelInputs = function(contaminant_data, building_data, vadose_zone_data, strata_logs_data, settings_data){

  # Track warning messages in this variable
  warningMessages <- NULL

  ################################# settings_data #######################################

  #Check if data are assigned
  if(all(is.na(settings_data)) || all(is.null(settings_data))){
    stop("Error: no values assigned for the settings_data input to the runJE function")
  }

  #Check if settings_data is a data frame
  if(!(is(settings_data,"data.frame"))){
    stop("Error: the settings_data input to the runJE function must be a data frame for the function to proceed")
  }

  #Check if the needed fields are assigned
  settings_data_required_field_names <- c("simulation_type", "source_medium", "building_setting", "foundation_type")

  if(!(all(settings_data_required_field_names %in% names(settings_data))))
  {
    stop("Error: One or more required fields are missing from the \"settings_data\" data frame in the runJE function")
  }

  #Check if capillary zone setting is assigned for groundwater simulations
  if (settings_data$source_medium == "Groundwater"){
    if(!("simulate_capillary_zone" %in% names(settings_data))){
      stop("Error: The \"simulate_capillary_zone\" field is missing from the \"settings_data\" data frame in the runJE function")
    }
  }

  #Check if number of Monte Carlo simulations is assigned for stochastic simulations
  if (settings_data$simulation_type == "MC"){
    if(!("number_of_monte_carlo_iterations" %in% names(settings_data))){
      stop("Error: The \"number_of_monte_carlo_iterations\" field is missing from the \"settings_data\" data frame in the runJE function")
    }
  }

  #Check that all parameters have a known accepted value
  simulation_type_options <- c("DET", "MC")
  source_medium_options <- c("Groundwater", "Exterior Soil Gas", "Subslab Soil Gas")
  building_setting_options <- c("Residential", "Commercial")
  foundation_type_options <- c("Basement-slab", "Basement-dirt", "Slab-grade", "Crawlspace-slab", "Crawlspace-dirt")
  simulate_capillary_zone_options <- c(TRUE, FALSE)

  if (!(settings_data$simulation_type %in% simulation_type_options))
  {
    stop("Error: The value of the \"settings_data$simulation_type\" parameter is not an accepted value. Use one of the following: \"DET\" or \"MC\"")
  }

  if (!(settings_data$source_medium %in% source_medium_options))
  {
    stop("Error: The value of the \"settings_data$source_medium\" parameter is not an accepted value. Use one of the following: \"Groundwater\", \"Exterior Soil Gas\" or \"Subslab Soil Gas\"")
  }

  if (!(settings_data$building_setting %in% building_setting_options))
  {
    stop("Error: The value of the \"settings_data$building_setting\" parameter is not an accepted value. Use one of the following: \"Residential\" or \"Commercial\"")
  }

  if (!(settings_data$foundation_type %in% foundation_type_options))
  {
    stop("Error: The value of the \"settings_data$foundation_type\" parameter is not an accepted value. Use one of the following: \"Basement-slab\", \"Basement-dirt\", \"Slab-grade\", \"Crawlspace-slab\", \"Crawlspace-dirt\"")
  }

  if (!(settings_data$simulate_capillary_zone %in% simulate_capillary_zone_options))
  {
    stop("Error: The value of the \"settings_data$simulate_capillary_zone\" parameter is not an accepted value. Use one of the following: TRUE or FALSE")
  }

  if (settings_data$simulation_type == "MC")
  {
    number_of_monte_carlo_iterations <- suppressWarnings(as.numeric(settings_data$number_of_monte_carlo_iterations))

    monte_carlo_iterations_error_message <- "Error: The value of the \"settings_data$number_of_monte_carlo_iterations\" parameter is not an accepted value. For stochastic simulations, the number of Monte Carlo iterations must be an integer greater than zero"
    if (is.na(number_of_monte_carlo_iterations)){
      stop(monte_carlo_iterations_error_message)
    }

    if (!(number_of_monte_carlo_iterations%%1==0)){
      stop(monte_carlo_iterations_error_message)
    }
  }


  #################################### building_data #########################################

  #Check that building_data is of the correct class
  if (settings_data$simulation_type == "DET" && !(is(building_data,"data.frame")))
  {
    stop("Error: In a deterministic simulation, the \"building_data\" input to the runJE function must be a data frame for the function to proceed")
  }
  else if (settings_data$simulation_type == "MC" && !(is(building_data,"list")))
  {
    stop("Error: In a stochastic simulation, the \"building_data\" input to the runJE function must be a list for the function to proceed")
  }

  #Check that all list objects are of the JEMParamDist class
  if (settings_data$simulation_type == "MC"){
    if(!all(sapply(1:length(building_data), function(i){is(building_data[[i]],"JEMParamDist")})))
    {
      stop("Error: In a stochastic simulation, all items in the \"building_data\" input list must be R6 JEMParamDist objects")
    }
  }

  #Check that all required parameters are included in building_data
  building_data_required_field_names <- c("Lb", "Lf", "eta", "Abf", "Hb", "ach", "Qsoil_Qb")

  if(settings_data$simulation_type == "DET")
  {
    if(!(all(building_data_required_field_names %in% names(building_data))))
    {
      stop("Error: One or more parameters required to execute the runJE function is missing from the \"building_data\" data frame")
    }

    #Check that the data frame includes only one row
    if(nrow(building_data) > 1){
      stop("Error: The \"building_data\" data frame has more than one row. Only one value can be applied for each building parameter in a deterministic simulation.")
    }

    #Check that none of the required parameters are NA
    #Note that dirt floor foundations can accept NAs for Qsoil_Qb, per the default parameters allowed in the JEM v6 spreadsheet
    if(any(is.na(building_data$Lb)) ||
       any(is.na(building_data$Lf)) ||
       any(is.na(building_data$eta)) ||
       any(is.na(building_data$Abf)) ||
       any(is.na(building_data$Hb)) ||
       any(is.na(building_data$ach)) ||
       (any(is.na(building_data$Qsoil_Qb) && !(settings_data$foundation_type %in% c("Basement-dirt", "Crawlspace-dirt")))))
    {
      stop("Error: The \"building_data\" data input to the runJE function cannot include NA values in any of the required fields (Lb, Lf, eta, Abf, Hb, ach, and Qsoil_Qb)")
    }

    #Check that all of the required parameters are numeric
    if(any(!is.numeric(building_data$Lb)) ||
       any(!is.numeric(building_data$Lf)) ||
       any(!is.numeric(building_data$eta)) ||
       any(!is.numeric(building_data$Abf)) ||
       any(!is.numeric(building_data$Hb)) ||
       any(!is.numeric(building_data$ach)) ||
       (any(!is.numeric(building_data$Qsoil_Qb) && !(settings_data$foundation_type %in% c("Basement-dirt", "Crawlspace-dirt")))))
    {
      stop("Error: One or more of the values in the \"building_data\" data frame required fields (Lb, Lf, eta, Abf, Hb, ach, and Qsoil_Qb) is non-numeric")
    }

    #Check that all of the required parameters are non-negative
    if(any(building_data$Lb < 0) ||
       any(building_data$Lf < 0) ||
       any(building_data$eta < 0) ||
       any(building_data$Abf < 0) ||
       any(building_data$Hb < 0) ||
       any(building_data$ach < 0) ||
       (any(building_data$Qsoil_Qb < 0) && !(settings_data$foundation_type %in% c("Basement-dirt", "Crawlspace-dirt"))))
    {
      stop("Error: One or more of the values in the \"building_data\" data frame required fields (Lb, Lf, eta, Abf, Hb, ach, and Qsoil_Qb) is negative")
    }
  }
  else if (settings_data$simulation_type == "MC")
  {
    if(!(all(building_data_required_field_names %in% sapply(1:length(building_data), function(i){building_data[[i]]$getSymbol()}))))
    {
      stop("Error: One or more parameters required to execute the runJE function are missing from the \"building_data\" list")
    }

    #Check that each parameter distribution is present only once
    if (length(Filter(function(x) x$getSymbol() == "Lb", building_data)) > 1 ||
        length(Filter(function(x) x$getSymbol() == "Lf", building_data)) > 1 ||
        length(Filter(function(x) x$getSymbol() == "eta", building_data)) > 1 ||
        length(Filter(function(x) x$getSymbol() == "Abf", building_data)) > 1 ||
        length(Filter(function(x) x$getSymbol() == "Hb", building_data)) > 1 ||
        length(Filter(function(x) x$getSymbol() == "ach", building_data)) > 1 ||
        length(Filter(function(x) x$getSymbol() == "Qsoil_Qb", building_data)) > 1)
    {
      stop("Error: One or more parameters has multiple JEMParamDist objects included in the \"building_data\" list")
    }

    #Check that each distribution has non-negative values (zeros are allowed)
    Filter(function(x) x$getSymbol() == "Lb", building_data)[[1]]$checkParamsAreNonNegative()
    Filter(function(x) x$getSymbol() == "Lf", building_data)[[1]]$checkParamsAreNonNegative()
    Filter(function(x) x$getSymbol() == "eta", building_data)[[1]]$checkParamsAreNonNegative()
    Filter(function(x) x$getSymbol() == "Abf", building_data)[[1]]$checkParamsAreNonNegative()
    Filter(function(x) x$getSymbol() == "Hb", building_data)[[1]]$checkParamsAreNonNegative()
    Filter(function(x) x$getSymbol() == "ach", building_data)[[1]]$checkParamsAreNonNegative()

    if (!(settings_data$foundation_type %in% c("Basement-dirt", "Crawlspace-dirt"))){
      Filter(function(x) x$getSymbol() == "Qsoil_Qb", building_data)[[1]]$checkParamsAreNonNegative()
    }
  }

  #Check that foundation thickness is zero for buildings with dirt floors
  if (settings_data$foundation_type %in% c("Basement-dirt", "Crawlspace-dirt"))
  {
      if(settings_data$simulation_type == "DET"){
        if(!building_data$Lf == 0){
            stop("Error: Foundation thickness (Lf) must equal zero for buildings with dirt floors")
        }
      } else {
        Lf_dist <- Filter(function(x) x$getSymbol() == "Lf", building_data)[[1]]

        if(Lf_dist$paramsAreGreaterThanLowerBound(0, FALSE) || Lf_dist$paramsAreLessThanUpperBound(0, FALSE)){
          stop("Error: Foundation thickness (Lf) must equal zero for buildings with dirt floors")
        }
      }
  }
  #If it has a foundation, check that the thickness is within a reasonable range
  else
  {
    minThickness <- 0.1
    maxThickness <- 0.25

    if (settings_data$simulation_type == "DET"){
      if (building_data$Lf > maxThickness || building_data$Lf < minThickness)
      {
        warningMessages[length(warningMessages) + 1] <- paste("Warning: The value input for foundation thickness (Lf) is outside of the expected range (", minThickness, " - ", maxThickness, " meters)", sep="")
      }
    } else {
      Lf_dist <- Filter(function(x) x$getSymbol() == "Lf", building_data)[[1]]

      if (!Lf_dist$paramsAreLessThanUpperBound(maxThickness, TRUE) || !Lf_dist$paramsAreGreaterThanLowerBound(minThickness, TRUE))
      {
        warningMessages[length(warningMessages) + 1] <- paste("Warning: One or more parameter values input for foundation thickness (Lf) is outside of the expected range (", minThickness, " - ", maxThickness, " meters)", sep="")
      }
    }
  }

  #Check that the fraction of foundation area with cracks is within a reasonable range
  if (settings_data$foundation_type %in% c("Basement-slab", "Slab-grade", "Crawlspace-slab"))
  {
    if (settings_data$foundation_type %in% c("Basement-slab", "Crawlspace-slab"))
    {
      minFraction <- 0.0001
      maxFraction <- 0.001
    }
    else
    {
      minFraction <- 0.00019
      maxFraction <- 0.0019
    }

    if (settings_data$simulation_type == "DET"){
      if (building_data$eta > maxFraction || building_data$eta < minFraction)
      {
        warningMessages[length(warningMessages) + 1] <- paste("Warning: The value input for the fraction of the foundation area with cracks (eta) is outside of the expected range (", format(minFraction, scientific = FALSE), " - ", maxFraction, ")", sep="")
      }
    } else {
      eta_dist <- Filter(function(x) x$getSymbol() == "eta", building_data)[[1]]

      if (!eta_dist$paramsAreLessThanUpperBound(maxFraction, TRUE) || !eta_dist$paramsAreGreaterThanLowerBound(minFraction, TRUE))
      {
        warningMessages[length(warningMessages) + 1] <- paste("Warning: One or more parameter values input for the fraction of the foundation area with cracks (eta) is outside of the expected range (", format(minFraction, scientific = FALSE), " - ", maxFraction, ")", sep="")
      }
    }
  }

  #Check that the floor area is within a reasonable range
  if (settings_data$building_setting == "Residential")
  {
    minArea <- 80
    maxArea <- 200
  }
  else
  {
    minArea <- 80
    maxArea <- 1500
  }

  if (settings_data$simulation_type == "DET"){
    if (building_data$Abf > maxArea || building_data$Abf < minArea)
    {
      warningMessages[length(warningMessages) + 1] <- paste("Warning: The value input for the enclosed space floor area (Abf) is outside of the expected range (", minArea, " - ", maxArea, " square meters)", sep="")
    }
  } else {
    Abf_dist <- Filter(function(x) x$getSymbol() == "Abf", building_data)[[1]]

    if (!Abf_dist$paramsAreLessThanUpperBound(maxArea, TRUE) || !Abf_dist$paramsAreGreaterThanLowerBound(minArea, TRUE))
    {
      warningMessages[length(warningMessages) + 1] <- paste("Warning: One or more parameter values input for the enclosed space floor area (Abf) is outside of the expected range (", minArea, " - ", maxArea, " square meters)", sep="")
    }
  }

  #Check that the indoor air exchange rate is within a reasonable range
  if (settings_data$building_setting == "Residential")
  {
    minACH <- 0.15
    maxACH <- 1.26
  }
  else
  {
    minACH <- 0.3
    maxACH <- 4.1
  }

  if (settings_data$simulation_type == "DET"){
    if (building_data$ach > maxACH || building_data$ach < minACH)
    {
      warningMessages[length(warningMessages) + 1] <- paste("Warning: The value input for the indoor air exchange rate (ach) is outside of the expected range (", minACH, " - ", maxACH, " air changes per hour)", sep="")
    }
  } else {
    ach_dist <- Filter(function(x) x$getSymbol() == "ach", building_data)[[1]]

    if (!ach_dist$paramsAreLessThanUpperBound(maxACH, TRUE) || !ach_dist$paramsAreGreaterThanLowerBound(minACH, TRUE))
    {
      warningMessages[length(warningMessages) + 1] <- paste("Warning: One or more parameter values input for the indoor air exchange rate (ach) is outside of the expected range (", minACH, " - ", maxACH, " air changes per hour)", sep="")
    }
  }

  #Check that the Qsoil/Qbuilding ratio is within a reasonable range
  #Applying to all foundation types based on p.52 of the EPA JEM spreadsheet user's guide
  if(!(settings_data$foundation_type %in% c("Basement-dirt", "Crawlspace-dirt"))){

    minQsoil_Qb <- 0.0001
    maxQsoil_Qb <- 0.05


    if (settings_data$simulation_type == "DET"){
      if (building_data$Qsoil_Qb > maxQsoil_Qb || building_data$Qsoil_Qb < minQsoil_Qb)
      {
        warningMessages[length(warningMessages) + 1] <- paste("Warning: The value input for the Qsoil/Qbuilding ratio (Qsoil_Qb) is outside of the expected range (", format(minQsoil_Qb, scientific = FALSE), " - ", maxQsoil_Qb, ")", sep="")
      }
    } else {
      Qsoil_Qb_dist <- Filter(function(x) x$getSymbol() == "Qsoil_Qb", building_data)[[1]]

      if (!Qsoil_Qb_dist$paramsAreLessThanUpperBound(maxQsoil_Qb, TRUE) || !Qsoil_Qb_dist$paramsAreGreaterThanLowerBound(minQsoil_Qb, TRUE))
      {
        warningMessages[length(warningMessages) + 1] <- paste("Warning: One or more parameter values input for the Qsoil/Qbuilding ratio (Qsoil_Qb) is outside of the expected range (", format(minQsoil_Qb, scientific = FALSE), " - ", maxQsoil_Qb, ")", sep="")
      }
    }
  }

  #Check that the enclosed space mixing height is within a reasonable range
  if (settings_data$foundation_type %in% c("Crawlspace-slab", "Crawlspace-dirt")){
    minHeight <- 0.5
    maxHeight <- 1.3
  } else if (settings_data$foundation_type %in% c("Basement-slab", "Basement-dirt")) {
    minHeight <- 2.44
    maxHeight <- 4.88
  } else { #"Slab-grade"
    minHeight <- 2.13
    maxHeight <- 3.05
  }

  if (settings_data$simulation_type == "DET"){
    if (building_data$Hb > maxHeight || building_data$Hb < minHeight)
    {
      warningMessages[length(warningMessages) + 1] <- paste("Warning: The value input for the enclosed space mixing height (Hb) is outside of the expected range (", minHeight, " - ", maxHeight, " meters)", sep="")
    }
  } else {
    Hb_dist <- Filter(function(x) x$getSymbol() == "Hb", building_data)[[1]]

    if (!Hb_dist$paramsAreLessThanUpperBound(maxHeight, TRUE) || !Hb_dist$paramsAreGreaterThanLowerBound(minHeight, TRUE))
    {
      warningMessages[length(warningMessages) + 1] <- paste("Warning: One or more parameter values input for the enclosed space mixing height (Hb) is outside of the expected range (", minHeight, " - ", maxHeight, " meters)", sep="")
    }
  }

  #Check that depth to foundation is within a reasonable range
  #Only groundwater and exterior soil gas are checked here
  #The subslab soil gas test depends on Ls, so it is applied in the Ls code after that variable has been checked.
  if (settings_data$source_medium %in% c("Groundwater", "Exterior Soil Gas")){
    minDepth <- 0.1
    maxDepth <- 2.44

    if (settings_data$simulation_type == "DET"){
      if (building_data$Lb > maxDepth || building_data$Lb < minDepth)
      {
        warningMessages[length(warningMessages) + 1] <- paste("Warning: The value input for the depth below grade to base of foundation (Lb) is outside of the expected range (", minDepth, " - ", maxDepth, " meters)", sep="")
      }
    } else {
      Lb_dist <- Filter(function(x) x$getSymbol() == "Lb", building_data)[[1]]

      if (!Lb_dist$paramsAreLessThanUpperBound(maxDepth, TRUE) || !Lb_dist$paramsAreGreaterThanLowerBound(minDepth, TRUE))
      {
        warningMessages[length(warningMessages) + 1] <- paste("Warning: One or more parameter values input for the depth below grade to base of foundation (Lb) is outside of the expected range (", minDepth, " - ", maxDepth, " meters)", sep="")
      }
    }
  }

  #################################### vadose_zone_data #########################################

  #Check that vadose_zone_data is of the correct class
  if (settings_data$simulation_type == "DET" && !(is(vadose_zone_data,"data.frame")))
  {
    stop("Error: In a deterministic simulation, the \"vadose_zone_data\" input to the runJE function must be a data frame for the function to proceed")
  }
  else if (settings_data$simulation_type == "MC" && !(is(vadose_zone_data, "list")))
  {
    stop("Error: In a stochastic simulation, the \"vadose_zone_data\" input to the runJE function must be a list for the function to proceed")
  }

  #Check that all list objects are of the JEMParamDist class
  if (settings_data$simulation_type == "MC"){
    if(!all(sapply(1:length(vadose_zone_data), function(i){is(vadose_zone_data[[i]], "JEMParamDist")})))
    {
      stop("Error: In a stochastic simulation, all items in the \"vadose_zone_data\" input list must be R6 JEMParamDist objects")
    }
  }

  #Check that all required parameters are included in vadose_zone_data
  vadose_zone_data_required_field_names <- c("Ls", "Ts")

  if(settings_data$simulation_type == "DET")
  {
    if(!(all(vadose_zone_data_required_field_names %in% names(vadose_zone_data))))
    {
      stop("Error: One or more parameters required to execute the runJE function are missing from the \"vadose_zone_data\" data frame")
    }

    if(is.na(vadose_zone_data$Ls) | is.na(vadose_zone_data$Ts)){
      stop("Error: The \"vadose_zone_data\" data input to the runJE function cannot include NA values in the Ls or Ts fields")
    }

    if(!is.numeric(vadose_zone_data$Ls) | !is.numeric(vadose_zone_data$Ts))
    {
      stop("Error: The value in the Ls or Ts field in the \"vadoze_zone_data\" data frame is not a number")
    }

    #Ts can be less than zero (in theory), so no need to test for that here
    if(vadose_zone_data$Ls <= 0)
    {
      stop("Error: The value of Ls in the \"vadoze_zone_data\" data frame must be greater than zero")
    }
  }
  else if (settings_data$simulation_type == "MC")
  {
    if(!(all(vadose_zone_data_required_field_names %in% sapply(1:length(vadose_zone_data), function(i){vadose_zone_data[[i]]$getSymbol()}))))
    {
      stop("Error: One or more parameters required to execute the runJE function are missing from the \"vadose_zone_data\" list")
    }

    #Check that each parameter distribution is present only once
    if (length(Filter(function(x) x$getSymbol() == "Ls", vadose_zone_data)) > 1 ||
        length(Filter(function(x) x$getSymbol() == "Ts", vadose_zone_data)) > 1)
    {
      stop("Error: One or more parameters has multiple JEMParamDist objects included in the \"vadose_zone_data\" list")
    }

    Filter(function(x) x$getSymbol() == "Ls", vadose_zone_data)[[1]]$checkParamsAreNonNegative()
  }

  # Check spreadsheet temperature warnings
  if (settings_data$source_medium == "Groundwater"){
      minTemp <- 3 # deg C
      maxTemp <- 25 #deg C
  } else {
      minTemp <- 3 #deg C
      maxTemp <- 30 #deg C
  }

  if (settings_data$simulation_type == "DET"){
    if (vadose_zone_data$Ts > maxTemp || vadose_zone_data$Ts < minTemp)
    {
      warningMessages[length(warningMessages) + 1] <- paste("Warning: The value input for the source temperature (Ts) is outside of the expected range (", minTemp, " - ", maxTemp, " degrees Celsius)", sep="")
    }
  } else {
    Ts_dist <- Filter(function(x) x$getSymbol() == "Ts", vadose_zone_data)[[1]]

    if (!Ts_dist$paramsAreLessThanUpperBound(maxTemp, TRUE) || !Ts_dist$paramsAreGreaterThanLowerBound(minTemp, TRUE))
    {
      warningMessages[length(warningMessages) + 1] <- paste("Warning: One or more parameter values input for the source temperature (Ts) is outside of the expected range (", minTemp, " - ", maxTemp, " degrees Celsius)", sep="")
    }
  }

  #Check that the depth below grade to source is greater than the maximum depth to base of foundation
  if(settings_data$simulation_type == "DET"){

    minDepth <- building_data$Lb

    if (vadose_zone_data$Ls < minDepth)
    {
      stop(paste("Error: The depth below grade to source (Ls = ",vadose_zone_data$Ls, " m) is less than the depth below grade to base of foundation (Lb = ", building_data$Lb," m)",sep = ""))
    }
  } else {

    Lb_dist <- Filter(function(x) x$getSymbol() == "Lb", building_data)[[1]]

    minDepth <- Lb_dist$getDistributionMaximum()

    Ls_dist <- Filter(function(x) x$getSymbol() == "Ls", vadose_zone_data)[[1]]

    if ( Ls_dist$getDistributionMinimum() < minDepth){
      stop(paste("Error: The minimum depth below grade to source (Ls_min = ", Ls_dist$getDistributionMinimum(), " m) is less than the maximum depth below grade to base of foundation (Lb_max = ", minDepth, " m)", sep=""))
    }
  }

  #Check spreadsheet depth to source warnings
  if (settings_data$source_medium == "Subslab Soil Gas"){

    if(settings_data$simulation_type == "DET"){

      minDepth <- building_data$Lb
      maxDepth <- building_data$Lb + 1

      if (vadose_zone_data$Ls > maxDepth || vadose_zone_data$Ls < minDepth)
      {
        message <- "Warning: For subslab soil gas sources, the depth below grade to source (Ls) should be within one meter of the depth to building foundation (Lb)."
        message <- paste(message, " The value input for Ls is outside of the expected range (", minDepth, " - ", maxDepth, " meters) based on the input value for Lb.", sep = "")

        warningMessages[length(warningMessages) + 1] <- message
      }
    } else {

      Lb_dist <- Filter(function(x) x$getSymbol() == "Lb", building_data)[[1]]

      minDepth <- Lb_dist$getDistributionMinimum()
      maxDepth <- Lb_dist$getDistributionMaximum() + 1

      Ls_dist <- Filter(function(x) x$getSymbol() == "Ls", vadose_zone_data)[[1]]

      if (Ls_dist$getDistributionMaximum() > maxDepth || Ls_dist$getDistributionMinimum() < minDepth){

        message <- "Warning: For subslab soil gas sources, the depth below grade to source (Ls) should be within one meter of the depth to building foundation (Lb)."
        message <- paste(message, " One or more values input for the Ls distribution are outside of the expected range (", minDepth, " - ", maxDepth, " meters) based on the input values for Lb.", sep = "")

        warningMessages[length(warningMessages) + 1] <- message

      }
    }

  } else { #Exterior soil gas and groundwater
    if(settings_data$simulation_type == "DET"){

      maxDepth <- building_data$Lb + 1

      if (vadose_zone_data$Ls < maxDepth)
      {
        message <- "Warning: For exterior soil gas and groundwater sources, the depth below grade to source (Ls) should be at least one meter deeper than the depth to building foundation (Lb)."
        message <- paste(message, " The value input for Ls is shallower than the expected minimum depth (", maxDepth, " meters) based on the input value for Lb.", sep = "")

        warningMessages[length(warningMessages) + 1] <- message
      }

    } else {

      Lb_dist <- Filter(function(x) x$getSymbol() == "Lb", building_data)[[1]]

      maxDepth <- Lb_dist$getDistributionMaximum() + 1

      Ls_dist <- Filter(function(x) x$getSymbol() == "Ls", vadose_zone_data)[[1]]

      if (Ls_dist$getDistributionMinimum() < maxDepth){

        message <- "Warning: For exterior soil gas and groundwater sources, the depth below grade to source (Ls) should be at least one meter deeper than the depth to building foundation (Lb)."
        message <- paste(message, " One or more values input for the Ls distribution are shallower than the expected minimum depth (", maxDepth, " meters) based on the input values for Lb.", sep = "")

        warningMessages[length(warningMessages) + 1] <- message

      }
    }
  }

  #################################### strata_logs_data #########################################

  #Check that strata_logs_data is of the correct class
  if(!(is(strata_logs_data, "data.frame")))
  {
    stop("Error: The \"strata_logs_data\" input to the runJE function must be a data frame for the function to proceed")
  }

  #Check that all required parameters are included in strata_logs_data
  strata_logs_data_required_field_names <- c("LogID", "SoilType", "LayerOrder", "Thickness")

  if(!(all(strata_logs_data_required_field_names %in% names(strata_logs_data))))
  {
    stop("Error: One or more parameters required to execute the runJE function are missing from the \"strata_logs_data\" data frame")
  }

  #Remove any values with "Not Present" in the SoilType column since they get removed later on
  strata_logs_data <- strata_logs_data[strata_logs_data$SoilType != "Not Present", ]

  #Check that there are no NAs in the key columns
  if(any(is.na(strata_logs_data$LogID)) || any(is.na(strata_logs_data$SoilType)) || any(is.na(strata_logs_data$LayerOrder)) || any(is.na(strata_logs_data$Thickness))){
    stop("Error: The \"strata_logs_data\" data frame input to the runJE function cannot include NA values in the LogID, SoilType, LayerOrder, or Thickness fields, except where the SoilType is listed as \"Not Present\"")
  }

  #Check that the SoilTypes are one of the 12 allowable options
  #Check that each type is one of the allowed distributions
  soil_type_options <- c("Clay", "Clay Loam", "Loam", "Loamy Sand", "Sand", "Sandy Clay", "Sandy Clay Loam", "Sandy Loam", "Silt", "Silt Loam", "Silty Clay", "Silty Clay Loam")

  if(!(all(strata_logs_data$SoilType %in% soil_type_options)))
  {
    stop("Error: Unsupported soil type assigned in the \"strata_logs_data\" data frame input to the runJE function")
  }

  #Check that the LayerOrder and Thickness values are numeric
  if(any(!is.numeric(strata_logs_data$LayerOrder)) || any(!is.numeric(strata_logs_data$Thickness)))
  {
    stop("Error: A value in the LayerOrder or Thickness fields in the \"strata_logs_data\" data frame is not a number")
  }

  #Check that the LayerOrder and Thickness values are greater than zero
  if(any(strata_logs_data$LayerOrder <= 0) || any(strata_logs_data$Thickness <= 0))
  {
    stop("Error: A value in the LayerOrder or Thickness fields in the \"strata_logs_data\" is less than or equal to zero")
  }

  #Check that only one LogID was passed to the runJE function if it is a deterministic simulation
  if(settings_data$simulation_type == "DET"){
    if(length(unique(strata_logs_data$LogID)) > 1)
    {
      stop("Error: The strata_logs_data LogID field includes more than one unique value. The runJE function can evaluate only one LogID at a time in deterministic simulations")
    }
  }

  #Get a list of the unique logIDs in the strata file
  logIDs <- unique(strata_logs_data$LogID)
  nlogs <- length(logIDs)

  #Perform tests on each boring log input into the function (may be more than one for stochastic simulations)
  for (logID_index in 1:nlogs){

    logID <- logIDs[logID_index]
    boring_log_data <- strata_logs_data %>% filter(.data$LogID == logID)

    #check that LayerOrder inputs are sequential starting at 1
    if(!all(1:length(boring_log_data$LayerOrder) %in% boring_log_data$LayerOrder))
    {
      stop(paste("Error: Values in the \"strata_logs_data\" LayerOrder field must be sequential integers starting at 1 for each unique LogID. One or more sequential integers is missing from the boring log with LogID = ", logID, sep = ""))
    }

    boring_log_data <- boringLogSetup(boring_log_data)
    log_thickness <- sum(boring_log_data$Thickness)

    #Check that the depth to source is less than the total depth of the boring log
    if(settings_data$simulation_type == "DET")
    {
      if (vadose_zone_data$Ls > log_thickness){
        stop(paste("Error: The input depth to source (Ls = ", vadose_zone_data$Ls , " m) is deeper than the total depth of the boring log with LogID = ", logID, " (total depth = ", log_thickness, " m)", sep = ""))
      }
    }
    else
    {
      Ls_dist <- Filter(function(x) x$getSymbol() == "Ls", vadose_zone_data)[[1]]

      #maximum depth to water
      Ls_max <- Ls_dist$getDistributionMaximum()

      #deal with constant distribution
      if (Ls_max > log_thickness)
      {
        stop(paste("Error: The maximum depth to source (Ls_max = ",Ls_max ," m) is deeper than the total depth of the boring log with LogID = ", logID, " (total depth = ", log_thickness, " m)", sep = ""))
      }
    }

    #For groundwater simulations based on capillary rise, check hcz in water table layers to make sure they don't potentially cross into the building foundation
    if(settings_data$simulate_capillary_zone == TRUE && settings_data$source_medium == "Groundwater")
    {
      if(settings_data$simulation_type == "DET")
      {
        hcz <- hczCalc(boring_log_data, vadose_zone_data$Ls)

        #JEM spreadsheet throws a warning in this case, but we're making it an error since it's violating the assumptions for the model
        #If the if condition is true, it's saying that capillary rise has brought water up above the depth of the building foundation
        if (building_data$Lb > vadose_zone_data$Ls - hcz){
            stop(paste("Error: Based on the soil properties for the boring log with LogID = ", logID, ", the input depth to water (Ls = ",vadose_zone_data$Ls," m) would result in capillary rise (hcz = ",hcz ," m) above the input depth to building foundation (Lb = ", building_data$Lb," m), which violates the model assumptions.", sep = ""))
        }
      }
      else
      {
        Lb_dist <- Filter(function(x) x$getSymbol() == "Lb", building_data)[[1]]
        Ls_dist <- Filter(function(x) x$getSymbol() == "Ls", vadose_zone_data)[[1]]

        #maximum building foundation depth
        Lb_max <- Lb_dist$getDistributionMaximum()

        #minimum depth to water
        Ls_min <- Ls_dist$getDistributionMinimum()

        #maximum depth to water
        Ls_max <- Ls_dist$getDistributionMaximum()

        #deal with constant distribution
        if (Ls_min == Ls_max)
        {
          hcz <- hczCalc(boring_log_data, Ls_min)

          if (Lb_max > Ls_min - hcz){
            stop(paste("Error: Based on the soil properties for the boring log with LogID = ", logID, ", the minimum depth to water (Ls_min = ",Ls_min ," m) may result in capillary rise (hcz = ", hcz," m) above the maximum depth to building foundation (Lb_max = ", Lb_max," m), which violates the model assumptions.", sep = ""))
          }
        }
        else
        {
          saturated_zone_data <- getBoringLogSubzone(boring_log_data, Ls_min, Ls_max)

          #Check that the hcz from the upper depth of each layer in the saturated zone is less than the maximum depth to building foundation
          hcz <- sapply(1:nrow(saturated_zone_data), function(i){
            hczCalc(saturated_zone_data, saturated_zone_data$upper_depth[[i]])
          })

          if(any(Lb_max > saturated_zone_data$upper_depth - hcz)){
            stop(paste("Error: Based on the soil properties for the boring log with LogID = ", logID, ", the minimum depth to water (Ls_min = ", Ls_min," m) may result in capillary rise (hcz = ", hcz," m) above the maximum depth to building foundation (Lb_max = ", Lb_max," m), which violates the model assumptions.", sep = ""))
          }
        }
      }
    }
  }

  #################################### contaminant_data #########################################

  if (settings_data$simulation_type == "DET" && !(is(contaminant_data, "data.frame")))
  {
    stop("Error: In a deterministic simulation, the \"contaminant_data\" input to the runJE function must be a data frame for the function to proceed")
  }
  else if (settings_data$simulation_type == "MC" && !(any(is(contaminant_data,"JEMParamDist"))))
  {
    stop("Error: In a stochastic simulation, the \"contaminant_data\" input to the runJE function must be an R6 JEMParamDist object for the function to proceed")
  }

  #Check that only one contaminant is passed to the function
  if(settings_data$simulation_type == "DET")
  {
    if(nrow(contaminant_data) > 1){
      stop("Error: The \"contaminant_data\" data frame has more than one row. Only one contaminant can be evaluated at a time using runJE")
    }
  }

  #Check that all required parameters are included in contaminant_data
  if(settings_data$simulation_type == "DET")
  {
    contaminant_data_required_field_names <- c("Contaminant", "Cmedium", "Units")

    if(!(all(contaminant_data_required_field_names %in% names(contaminant_data))))
    {
      stop("Error: One or more parameters required to execute the runJE function are missing from the \"contaminant_data\" data frame")
    }

    if(any(is.na(contaminant_data$Contaminant)) || any(is.na(contaminant_data$Cmedium)) || any(is.na(contaminant_data$Units))){
      stop("Error: The \"contaminant_data\" data input to the runJE function cannot include NA values in the Contaminant, Cmedium, or Units fields")
    }

    if(!is.numeric(contaminant_data$Cmedium))
    {
      stop("Error: The value in the Cmedium field in the \"contaminant_data\" data frame is not a number")
    }

    if(contaminant_data$Cmedium <= 0)
    {
      stop("Error: The input concentration is less than or equal to zero")
    }
  }
  else if (settings_data$simulation_type == "MC")
  {
    if(!(contaminant_data$getSymbol() == "Cmedium"))
    {
      stop("Error: The \"contaminant_data\" JEMParamDist object must have an assigned symbol of \"Cmedium\" for the simulation to proceed")
    }

    if(!(contaminant_data$paramsAreGreaterThanLowerBound(0, TRUE)))
    {
      stop("Error: One or more parameter values in the input contaminant concentration distribution was less than zero. All concentration parameters must be greater than or equal to zero.")
    }

  }

  #Check that concentration units are supported
  contaminant_units <- if(settings_data$simulation_type == "DET") {contaminant_data$Units} else {contaminant_data$getDataFrameOfProperties()$units}

  if(settings_data$source_medium %in% c("Subslab Soil Gas", "Exterior Soil Gas")){
    if(!(contaminant_units %in% c("ppb", "ug/m3"))) {
      stop("Error: The units assigned to the input soil gas concentration are not supported in the runJE function. Soil gas concentrations must be entered in units of \"ppb\" or \"ug/m3\")")
    }
  } else {
    if(!(contaminant_units %in% c("ppb", "ug/L"))) {
      stop("Error: The units assigned to the input groundwater concentration are not supported in the runJE function. Groundwater concentrations must be entered in units of \"ppb\" or \"ug/L\")")
    }
  }

  #Check that the contaminant is in the list of supported chemicals
  chemicalName <- if(settings_data$simulation_type == "DET") {contaminant_data$Contaminant} else {contaminant_data$getDataFrameOfProperties()$name}
  if (!chemicalName %in% chem_data$Chemical){
    stop(paste0("Error: The contaminant name '", chemicalName, "' was not found in the list of supported chemicals. Supported chemical names may be found in the `Chemical` field of the `vapintr::chem_data` data frame."))
  }

  #Get chemical properties
  chem_prop_dfx <- getChemicalProperties(chemicalName)

  #For groundwater data, check that the input concentration is less than the aqueous solubility limit of the contaminant
  if (settings_data$source_medium == "Groundwater"){

    solubilityLimit <- 1000*chem_prop_dfx$S

    if(settings_data$simulation_type == "DET"){
      if (contaminant_data$Cmedium > solubilityLimit){
        stop(paste("Error: The value input for the contaminant concentration exceeds the contaminant's aqueous solubility limit of ", solubilityLimit, " ppb.", sep=""))
      }
    } else {
      if (!contaminant_data$paramsAreLessThanUpperBound(solubilityLimit, TRUE))
      {
        stop("Error: One or more parameter values input for the contaminant concentration exceeds the contaminant's aqueous solubility limit.")
      }
    }
  }

  #Add warning message for petroleum hydrocarbon chemicals
  if(chem_prop_dfx$Pet_HC_Flag == "Yes"){
    warningMessages[length(warningMessages) + 1] <- "Warning: Predicted indoor air concentration may be overestimated; biodegredation was not considered for this contaminant."
  }

  #################################### return warnings  #########################################

  if (length (warningMessages) == 0){
    return ("No errors or warnings in model inputs")
  } else {

    #thrownWarningMessage <- "The checkModelInputs function threw the following warnings: \n\n"
    thrownWarningMessage <- ""

    for (messageIndex in 1:length(warningMessages)){
      thrownWarningMessage <- paste(thrownWarningMessage, warningMessages[messageIndex], " \n\n", sep = "")
    }

    warning(thrownWarningMessage)
    return(warningMessages)
  }
}
