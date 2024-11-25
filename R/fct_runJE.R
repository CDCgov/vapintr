#' @title Run the Johnson and Ettinger model
#'
#' @description Runs deterministic and stochastic simulations of the Johnson &
#'   Ettinger model. See the following vignettes for more information.
#'
#'   `vignette("DeterministicSimulations", package ="vapintr")`
#'
#'   `vignette("StochasticSimulations", package = "vapintr")`
#'
#' @param contaminant_data A data frame or `JEMParamDist` object with input
#'   contaminant data
#' @param building_data A data frame or list of `JEMParamDist` objects with input
#'   building data
#' @param vadose_zone_data A data frame or list of `JEMParamDist` objects with
#'   input vadose zone data
#' @param strata_logs_data A data frame with input data for the strata log or
#'   logs that characterize the subsurface soils beneath the building
#' @param settings_data A data frame containing input simulation settings
#'
#' @returns For deterministic simulations, `runJE()` returns a data frame
#'   summarizing the model input parameters, intermediate calculations, and
#'   output results. For stochastic simulations, `runJE()` returns an mc object
#'   with the same information for each Monte Carlo iteration in the simulation.
#'
#' @examples
#' #Run a deterministic simulation
#' contaminant_data <- det_jem_sim_example_data[[1]]
#' building_data <- det_jem_sim_example_data[[2]]
#' vadose_zone_data <- det_jem_sim_example_data[[3]]
#' strata_logs_data <- det_jem_sim_example_data[[4]]
#' settings_data <- det_jem_sim_example_data[[5]]
#'
#' runJE(contaminant_data, building_data, vadose_zone_data, strata_logs_data,
#' settings_data)
#'
#' #Run a stochastic simulation
#' contaminant_data_lx <- getInputConc(stoc_jem_sim_example_data[[1]], NA,
#' "Groundwater", "MC")
#'
#' contaminant_data <- contaminant_data_lx[[1]]
#' building_data <- stoc_jem_sim_example_data[[2]]
#' vadose_zone_data <- stoc_jem_sim_example_data[[3]]
#' strata_logs_data <- stoc_jem_sim_example_data[[4]]
#' settings_data <- stoc_jem_sim_example_data[[5]]
#'
#' runJE(contaminant_data, building_data, vadose_zone_data, strata_logs_data,
#' settings_data)
#'
#' @export

runJE = function(contaminant_data, building_data, vadose_zone_data, strata_logs_data, settings_data){

  #Verify structure and format of input parameters
  warningMessages <- checkModelInputs(contaminant_data, building_data, vadose_zone_data, strata_logs_data, settings_data)

  #assign settings variables to their own values since they are used alot
  source_medium <- settings_data$source_medium
  simulation_type <- settings_data$simulation_type
  foundation_type <- settings_data$foundation_type
  simulate_capillary_zone <- settings_data$simulate_capillary_zone

  #Get number of iterations associated with the simulation
  if (simulation_type == "DET")
  {
    nIterations <- 1
  }
  else
  {
    #Monte Carlo currently implemented using uncertainty iterations only (no variability iterations)
    nIterations <- settings_data$number_of_monte_carlo_iterations
    ndunc(nIterations)
  }

  #Assign building parameters

  #Fraction of foundation area with cracks
  eta <- if(simulation_type == "DET") {building_data$eta} else {Filter(function(x) x$getSymbol() == "eta", building_data)[[1]]$getDistributionData()}

  #Indoor air exchange rate
  ach <- if(simulation_type == "DET") {building_data$ach} else {Filter(function(x) x$getSymbol() == "ach", building_data)[[1]]$getDistributionData()}

  #Enclosed space mixing height
  Hb <- if(simulation_type == "DET") {building_data$Hb} else {Filter(function(x) x$getSymbol() == "Hb", building_data)[[1]]$getDistributionData()}

  #Enclosed space floor area
  Abf <- if(simulation_type == "DET") {building_data$Abf} else {Filter(function(x) x$getSymbol() == "Abf", building_data)[[1]]$getDistributionData()}

  #Set foundation thickness
  Lf <- if(simulation_type == "DET") {building_data$Lf} else {Filter(function(x) x$getSymbol() == "Lf", building_data)[[1]]$getDistributionData()}

  #Set depth below grade to base of foundation
  Lb <- if(simulation_type == "DET") {building_data$Lb} else {Filter(function(x) x$getSymbol() == "Lb", building_data)[[1]]$getDistributionData()}

  #Set flow rate ratio
  Qsoil_Qb <- if(simulation_type == "DET") {building_data$Qsoil_Qb} else {Filter(function(x) x$getSymbol() == "Qsoil_Qb", building_data)[[1]]$getDistributionData()}

  #Calculate Qb and Qsoil
  Qb <- QbCalc(Abf, Hb, ach)
  Qsoil <- QsoilCalc(Qsoil_Qb, Qb, foundation_type)

  if(simulation_type == "MC" && any(is.na(Qsoil))) {
    Qsoil <- mc2d::mcdata(data = -Inf, type = "U")
  }

  #Assign vadose zone parameters

  #Get depth to groundwater
  Ls <- if(simulation_type == "DET") {vadose_zone_data$Ls} else {Filter(function(x) x$getSymbol() == "Ls", vadose_zone_data)[[1]]$getDistributionData()}

  #Groundwater source -> Average groundwater temperature
  #Soil gas source -> Average vadose zone temperature
  Ts <- if(simulation_type == "DET") {vadose_zone_data$Ts} else {Filter(function(x) x$getSymbol() == "Ts", vadose_zone_data)[[1]]$getDistributionData()}

  #Assign contaminant parameters

  #Contaminant
  Chem <- if(simulation_type == "DET") {contaminant_data$Contaminant} else {contaminant_data$getDataFrameOfProperties()$name}

  #Concentration
  Cmedium <- if(simulation_type == "DET") {contaminant_data$Cmedium} else {contaminant_data$getDistributionData()}

  #Get chemical properties
  chem_prop_dfx <- getChemicalProperties(Chem)

  #Assign molecular weight to a variable because it gets used a lot
  MW <- chem_prop_dfx$MW

  #Convert soil gas concentrations in ppb to ug/m3 if needed
  if (source_medium %in% c("Subslab Soil Gas", "Exterior Soil Gas")){
    contaminantUnits <- if(simulation_type == "DET") {contaminant_data$Units} else {contaminant_data$getDataFrameOfProperties()$units}
    if(contaminantUnits == "ppb")
    {
      Cmedium <- concUnitConvs(Cmedium, "ppb", "ug/m3", "Air", Ts, MW)
    }
  }

  #Get Henry's law constant
  calcHsOutput <- calcHs(Chem = Chem, Ts = Ts, simulation_type = simulation_type)
  Hs <- calcHsOutput[[1]]

  #Capture warning message for compounds that couldn't calculate temperature-dependent Henry's Law Constants
  if (!is.na(calcHsOutput[[2]]))
  {
    warningMessages[length(warningMessages) + 1] <- calcHsOutput[[2]]
  }

  #Convert groundwater concentration to air concentration (no adjustment required for soil gas)
  Cs <- CsCalc(Cmedium, Hs, source_medium)

  #Calculate percent of pure component saturated vapor concentration
  percent_sat <- 100*PercSatCalc(Cs, chem_prop_dfx$Vc)

  #Adjust Cs by factor of 10 if source type is groundwater
  if (source_medium == "Groundwater" && !simulate_capillary_zone){
    Cs <- Cs/10
  }

  Dair <- chem_prop_dfx$Da
  Dwater <- chem_prop_dfx$Dw

  #Get a list of the unique logIDs in the strata file
  unique_logIDs <- unique(strata_logs_data$LogID)
  nlogs <- length(unique_logIDs)

  #Get a list of data for each boring log
  boring_log_data_lx <- lapply(1:nlogs, function(logID_index){

    logID_data <- strata_logs_data %>% filter(.data$LogID == unique_logIDs[logID_index])

    #Assign additional data needed for boring log calculations
    logID_data <- boringLogSetup(logID_data)

    return(logID_data)
  })

  #Randomly sample from the boring logs provided. For determinisitic simulations, there will be only one log per a check in checkModelInputs
  if(simulation_type == "DET"){
    sampled_log_index <- 1
  } else {
    #randomly sample for boring logs. If there are four boring logs, this will randomly sample from the numbers 1 through 4.
    sampled_log_index <- mcstoc(rempiricalD, values = (1:nlogs), type = "U")
  }

  #Create a list of the randomly sampled logs and remove the foundation depth below ground surface from the boring log
  boring_log_beneath_building_lx <- lapply(1:nIterations, function(i){
    sampled_log_data <- boring_log_data_lx[[sampled_log_index[i]]]
    getBoringLogSubzone(sampled_log_data, Lb[i], sum(sampled_log_data$Thickness))
  })

  #Calculate the capillary zone parameters for groundwater if simulating the capillary zone
  if (source_medium == "Groundwater" && simulate_capillary_zone)
  {
    #Calculate hcz (capillary zone height)
    #hcz <- hczCalc(boring_log_beneath_building_dfx, Ls)
    hcz_lx <- lapply(1:nIterations, function(i){
      hczCalc(boring_log_beneath_building_lx[[i]], Ls[i])
    })

    #Get the subset of the boring log that corresponds with the capillary zone
    #capillary_zone_dfx <- getBoringLogSubzone(boring_log_beneath_building_dfx, Ls - hcz, Ls)
    capillary_zone_lx <- lapply(1:nIterations, function(i){
      getBoringLogSubzone(boring_log_beneath_building_lx[[i]], Ls[i] - hcz_lx[[i]], Ls[i])
    })

    #Calculate Dcz (capillary zone effective diffusion coefficient)
    #DeffCZ <- DeffMultiLayerCalc(capillary_zone_dfx, Dair, Dwater, Hs, TRUE)
    DeffCZ_lx <- lapply(1:nIterations, function(i){
      DeffMultiLayerCalc(capillary_zone_lx[[i]], Dair, Dwater, Hs[i], TRUE)
    })
  }
  else
  {
    hcz_lx <- as.list(rep(0, nIterations))
    DeffCZ_lx <- as.list(rep(-Inf, nIterations))
  }

  #Calculate huz (unsaturated zone height)
  #huz <- Ls - Lb - hcz
  huz_lx <- lapply(1:nIterations, function(i){
    Ls[i] - Lb[i] - hcz_lx[[i]]
  })

  #Get the subset of the boring log that corresponds with the unsaturated zone
  #unsaturated_zone_dfx <- getBoringLogSubzone(boring_log_beneath_building_dfx, Lb, Ls - hcz)
  unsaturated_zone_lx <- lapply(1:nIterations, function(i){
    getBoringLogSubzone(boring_log_beneath_building_lx[[i]], Lb[i], Ls[i] - hcz_lx[[i]])
  })

  #Calculate Deffuz
  #DeffUZ <- DeffMultiLayerCalc(unsaturated_zone_dfx, Dair, Dwater, Hs, simulation_type, FALSE)
  DeffUZ_lx <- lapply(1:nIterations, function(i){
    DeffMultiLayerCalc(unsaturated_zone_lx[[i]], Dair, Dwater, Hs[i], FALSE)
  })

  #Calculate DeffT
  # DeffT <- DeffTCalc(DeffUZ, DeffCZ, huz, hcz, simulation_type, simulate_capillary_zone)
  DeffT_lx <- lapply(1:nIterations, function(i){
    DeffTCalc(DeffUZ_lx[[i]], DeffCZ_lx[[i]], huz_lx[[i]], hcz_lx[[i]], simulate_capillary_zone)
  })

  #Calculate DeffBF (effective diffusivity from the layer immediately beneath the foundation)
  #In the JEM spreadsheet this is called DeffA
  #DeffBF <- DeffMultiLayerCalc(boring_log_beneath_building_dfx[1, ], Dair, Dwater, Hs, simulation_type, FALSE)
  DeffBL_lx <- lapply(1:nIterations, function(i){
    DeffMultiLayerCalc(boring_log_beneath_building_lx[[i]][1, ], Dair, Dwater, Hs[i], FALSE)
  })

  #Assign variables for remaining calculations and output
  DeffT <- if (simulation_type == "DET") {DeffT_lx[[1]]} else {mc2d::mcdata(unlist(DeffT_lx), type = "U")}
  DeffBF <- if (simulation_type == "DET") {DeffBL_lx[[1]]} else {mc2d::mcdata(unlist(DeffBL_lx), type = "U")}
  hcz <- if (simulation_type == "DET") {hcz_lx[[1]]} else {mc2d::mcdata(unlist(hcz_lx), type = "U")}
  DeffCZ <- if (simulation_type == "DET") {DeffCZ_lx[[1]]} else {mc2d::mcdata(unlist(DeffCZ_lx), type = "U")}
  huz <- if (simulation_type == "DET") {huz_lx[[1]]} else {mc2d::mcdata(unlist(huz_lx), type = "U")}
  DeffUZ <- if (simulation_type == "DET") {DeffUZ_lx[[1]]} else {mc2d::mcdata(unlist(DeffUZ_lx), type= "U")}

  #Calculate the parameters used to calculate the attenuation factor (alpha)
  Aparam <- AparamCalc(DeffT, Abf, Lb, Qb, Ls)

  if (foundation_type %in% c("Basement-dirt", "Crawlspace-dirt")) {
    if (simulation_type == "DET") {
      Bparam <- NA
      Cparam <- NA
    } else {
      Bparam <- mc2d::mcdata(data = -Inf, type = "U")
      Cparam <- mc2d::mcdata(data = -Inf, type = "U")
    }
  } else {
    Bparam <- BparamCalc(Qsoil_Qb, Qb, Lf, DeffBF, eta, Abf, Lb)
    Cparam <- CparamCalc(Qsoil_Qb)
  }

  #Calculate the attenuation factor
  alpha <- alphaCalc(Aparam, Bparam, Cparam, Qsoil_Qb, foundation_type, source_medium)

  #Calculate the final indoor air concentration
  Cia <- CiaCalc(Cs, alpha)

  #Convert the final concentration to parts per billion
  #Assumes an indoor air temperature of 25 deg C
  #Using 24.46/MW instead of the concUnitConvs function so that the result exactly matches the EPA spreadsheet
  #Cia_ppb <- concUnitConvs(Cia, "ug/m3", "ppb", "Air", 25, MW)
  Cia_ppb <- Cia_ppbCalc(Cs, alpha, MW)

  #Calculate a subslab soil gas concentration for buildings with a slab
  Css <- CssCalc(Cia, Qb, Qsoil, foundation_type)
  Css_ppb <- Css_ppbCalc(Cia, Qb, Qsoil, MW, foundation_type)

  if (simulation_type == "MC" && any(is.na(Css))) {
    Css <- mc2d::mcdata(data = -Inf, type = "U")
    Css_ppb <- mc2d::mcdata(data = -Inf, type = "U")
  }

  #Set flag for whether the source is a groundwater source. Used for processing outputs
  is_groundwater_source <- source_medium == "Groundwater"
  if (simulation_type == "MC"){
    is_groundwater_source <- mc2d::mcdata(data = source_medium == "Groundwater", type = "U")
  }

  #Return outputs and calculated parameters
  if (simulation_type == "DET"){
  output <-
    data.frame(
      alpha, Cia, Cia_ppb, Css, Css_ppb, #output variables
      Aparam, Bparam, Cparam, #abc_parameters
      DeffT, DeffBF, DeffCZ, DeffUZ, hcz, huz, #vadose parameters
      ach, Hb, eta, Qb, Abf, Lb, Lf, Qsoil, #building parameters
      Cmedium, Cs, percent_sat, Ls, Ts, #source parameters
      is_groundwater_source, sampled_log_index #other parameters
    )
  }
  else
  {
    #collate everything into a mc object
    output <- mc2d::mc(
      alpha, Cia, Cia_ppb, Css, Css_ppb, #output variables
      Aparam, Bparam, Cparam, #abc_parameters
      DeffT, DeffBF, DeffCZ, DeffUZ, hcz, huz, #vadose parameters
      ach, Hb, eta, Abf, Lb, Lf, Qb, Qsoil, #building parameters
      Cmedium, Cs, percent_sat, Ls, Ts, #source parameters
      is_groundwater_source, sampled_log_index #other parameters
    )
  }

  return(output)
}
