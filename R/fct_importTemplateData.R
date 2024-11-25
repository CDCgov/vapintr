#' @title Import data using the vapintr data import template
#'
#' @description Reads in data from the vapintr Excel data import template and
#'   passes the data to `processImportedTemplateData()`, which prepares the data
#'   for simulation.
#'
#' @param template_file_path Path to a copy of the vapintr data import template
#'   with data to simulate
#'
#' @returns A list with six elements, one for each type of parameter data
#'   required to run the Johnson and Ettinger model and build output figures in
#'   vapintr. The output of this function is the same as the output from
#'   `processImportedTemplateData()`.
#'
#' @examples
#' #Get import data file path
#' import_data_file_path <- system.file("extdata",
#' "Stochastic_Simulation_Example_Data.xlsx", package = "vapintr")
#'
#' #Import the file data
#' importTemplateData(import_data_file_path)
#'
#' @export

importTemplateData = function(template_file_path){

  ################################ Setup ##########################################


  #Confirm file exists
  if (!file.exists(template_file_path))
  {
    stop(paste("Error: Could not find file at ", template_file_path, sep = ""))
  }

  templateWorkbook <- openxlsx::loadWorkbook(template_file_path)

  #Template data sheet names
  settings_sheet <- "Settings"
  building_info_sheet <- "BuildingInfo"
  vadose_zone_info_sheet <- "VadoseZoneInfo"
  strata_logs_sheet <- "StrataLogs"
  contaminant_data_sheet <- "ContaminantData"
  reference_air_conc_sheet <- "ReferenceAirConcentrations"

  #Confirm template has these six data sheets
  templateWorkbook <- openxlsx::loadWorkbook(template_file_path)
  worksheetNames <- c(settings_sheet, building_info_sheet, vadose_zone_info_sheet, strata_logs_sheet, contaminant_data_sheet, reference_air_conc_sheet)

  if(!all(worksheetNames %in% names(templateWorkbook)))
  {
    stop("Error: Imported file is missing a required worksheet")
  }

  #Import simulation settings
  settings_dfx <- openxlsx::read.xlsx(template_file_path, sheet = settings_sheet)

  #Import building info settings
  building_info_dfx <- openxlsx::read.xlsx(template_file_path, sheet = building_info_sheet, fillMergedCells = TRUE)

  #Import vadose zone info settings
  vadose_zone_info_dfx <- openxlsx::read.xlsx(template_file_path, sheet = vadose_zone_info_sheet, fillMergedCells = TRUE)

  # Import strata log settings
  strata_logs_dfx <- openxlsx::read.xlsx(template_file_path, sheet = strata_logs_sheet)

  # Import contaminant data settings
  contaminant_data_dfx <- openxlsx::read.xlsx(template_file_path, sheet = contaminant_data_sheet)

  # Import reference air concentrations
  reference_air_conc_dfx <- openxlsx::read.xlsx(template_file_path, sheet = reference_air_conc_sheet)

  #Create list of data frames
  imported_data_lx <- list (contaminant_data_dfx, building_info_dfx, vadose_zone_info_dfx, strata_logs_dfx, settings_dfx, reference_air_conc_dfx)

  return(processImportedTemplateData(imported_data_lx))
}

#' @title Process imported data from the vapintr data import template
#'
#' @description Converts data imported from the vapintr data import template
#'   into the form required by vapintr to run the Johnson and Ettinger model and
#'   produce output figures. This function is not typically accessed directly,
#'   but rather is called from `importTemplateData()`.
#'
#' @param imported_data_lx A list with six elements, typically created within
#'   `importTemplateData()`. In order, the six list elements represent data
#'    read from the following sheets in the data import template:
#'
#'   * ContaminantData
#'   * BuildingInfo
#'   * VadoseZoneInfo
#'   * StrataLogs
#'   * Settings
#'   * ReferenceAirConcentrations
#'
#' @returns A list with six elements, one for each type of parameter data
#'   required to run the Johnson and Ettinger model in vapintr. In order, the
#'   list elements contain the following types of data:
#'
#'   * Contaminant concentration data
#'   * Building parameter data
#'   * Vadoze zone parameter data
#'   * Soil strata data
#'   * Simulation settings
#'   * Reference air concentration data
#'
#' @examples
#' #Get the data import template file path
#' import_data_file_path <- system.file("extdata",
#' "Stochastic_Simulation_Example_Data.xlsx", package = "vapintr")
#'
#' #Import simulation settings
#' settings_dfx <- openxlsx::read.xlsx(import_data_file_path,
#' sheet = "Settings")
#'
#' #Import building info
#' building_info_dfx <- openxlsx::read.xlsx(import_data_file_path,
#' sheet = "BuildingInfo", fillMergedCells = TRUE)
#'
#' #Import vadose zone info
#' vadose_zone_info_dfx <- openxlsx::read.xlsx(import_data_file_path,
#' sheet = "VadoseZoneInfo", fillMergedCells = TRUE)
#'
#' #Import soil strata log data
#' strata_logs_dfx <- openxlsx::read.xlsx(import_data_file_path,
#' sheet = "StrataLogs")
#'
#' #Import contaminant data
#' contaminant_data_dfx <- openxlsx::read.xlsx(import_data_file_path,
#' sheet = "ContaminantData")
#'
#' #Import reference air concentration data
#' reference_data_dfx <- openxlsx::read.xlsx(import_data_file_path,
#' sheet = "ReferenceAirConcentrations")
#'
#' #Create list of data frames
#' imported_data_lx <- list (contaminant_data_dfx, building_info_dfx,
#' vadose_zone_info_dfx, strata_logs_dfx, settings_dfx, reference_data_dfx)
#'
#' #Process the imported template data
#' processImportedTemplateData(imported_data_lx)
#'
#' @export

processImportedTemplateData = function(imported_data_lx){

  #Assign some parameters that get reused in various sheets

  #Distribution options
  distribution_options <- c("Constant", "Uniform", "Triangular", "PERT", "Truncated Normal", "Truncated Lognormal")

  #Distribution parameters
  constant_dist_parameters <- c("Constant")
  uniform_dist_parameters <- c("Maximum", "Minimum")
  traingular_dist_parameters <- c("Maximum", "Mode", "Minimum")
  pert_dist_parameters <- c("Maximum", "Mode", "Minimum")
  truncated_normal_dist_parameters <- c("Maximum", "Mean", "Standard Deviation", "Minimum")
  truncated_lognormal_dist_parameters <- c("Maximum", "Geometric Mean", "Geometric Standard Deviation", "Minimum")

  #Unit parameters
  length_units <- c("m", "ft")
  area_units <- c("m2", "ft2")
  air_exchange_rate_units <- c("1/hr")
  dimensionless_units <- c("-")
  temperature_units <- c("deg C", "deg F")
  concentration_units <- c("mg/L", "mg/m3", "ug/L", "ug/m3", "ppm", "ppb")

  #Soil type options
  soil_type_options <- c("Clay", "Clay Loam", "Loam", "Loamy Sand", "Sand", "Sandy Clay", "Sandy Clay Loam", "Sandy Loam", "Silt", "Silt Loam", "Silty Clay", "Silty Clay Loam")

  ################################# Get data frames ################################

  #Import contaminant data settings
  contaminant_data_dfx <- imported_data_lx[[1]]

  #Import building info settings
  building_info_dfx <- imported_data_lx[[2]]

  #Import vadose zone info settings
  vadose_zone_info_dfx <- imported_data_lx[[3]]

  #Import strata log settings
  strata_logs_dfx <- imported_data_lx[[4]]

  #Import simulation settings
  settings_dfx <- imported_data_lx[[5]]

  #Import reference air concentrations
  reference_air_conc_dfx <- imported_data_lx[[6]]

  ################################# Settings #######################################

  #Check that the data frame includes the needed columns and settings
  settings_required_columns <- c("Parameter", "Value")
  settings_required_parameter_names <- c("Simulation type", "Source medium", "Apply default building parameters", "Building setting", "Foundation type", "Simulate capillary zone", "Number of Monte Carlo iterations")

  if(!(all(settings_required_columns %in% names(settings_dfx))))
  {
    stop("Error: One or more required columns are missing from the Settings sheet of the import file")
  }

  if(!(all(settings_required_parameter_names %in% settings_dfx$Parameter)))
  {
    stop("Error: One or more required parameters are missing from the Settings sheet of the import file")
  }

  #Drop the notes column if present
  if("Notes" %in% names(settings_dfx)){
    settings_dfx <- settings_dfx %>% select(-"Notes")
  }

  #Check that all parameters have a known accepted value
  simulation_type_options <- c("Deterministic", "Stochastic")
  source_medium_options <- c("Groundwater", "Exterior Soil Gas", "Subslab Soil Gas")
  apply_default_building_parameters_options <- c("Yes", "No")
  building_setting_options <- c("Residential", "Commercial")
  foundation_type_options <- c("Basement w/ Slab", "Basement w/ Dirt Floor", "Slab-on-grade", "Closed Crawl Space w/ Slab", "Closed Crawl Space w/ Dirt Floor")
  simulate_capillary_zone_options <- c("Yes", "No")

  simulation_type <- settings_dfx$Value[settings_dfx$Parameter == "Simulation type"]
  if (!(simulation_type %in% simulation_type_options))
  {
    stop("Error: The value of the \"Simulation type\" parameter in the import data template is not an accepted value. Use one of the following: \"Deterministic\" or \"Stochastic\"")
  }

  source_medium <- settings_dfx$Value[settings_dfx$Parameter == "Source medium"]
  if (!(source_medium %in% source_medium_options))
  {
    stop("Error: The value of the \"Source medium\" parameter in the import data template is not an accepted value. Use one of the following: \"Groundwater\", \"Exterior Soil Gas\" or \"Subslab Soil Gas\"")
  }

  apply_default_building_parameters <- settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"]
  if (!(apply_default_building_parameters %in% apply_default_building_parameters_options))
  {
    stop("Error: The value of the \"Apply default building parameters\" parameter in the import data template is not an accepted value. Use one of the following: \"Yes\" or \"No\"")
  }

  building_setting <- settings_dfx$Value[settings_dfx$Parameter == "Building setting"]
  if (!(building_setting %in% building_setting_options))
  {
    stop("Error: The value of the \"Building setting\" parameter in the import data template is not an accepted value. Use one of the following: \"Residential\" or \"Commercial\"")
  }

  foundation_type <- settings_dfx$Value[settings_dfx$Parameter == "Foundation type"]
  if (!(foundation_type %in% foundation_type_options))
  {
    stop("Error: The value of the \"Foundation type\" parameter in the import data template is not an accepted value. Use one of the following: \"Basement w/ Slab\", \"Basement w/ Dirt Floor\", \"Slab-on-grade\", \"Closed Crawl Space w/ Slab\", \"Closed Crawl Space w/ Dirt Floor\"")
  }

  simulate_capillary_zone <- settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"]
  if (!(simulate_capillary_zone %in% simulate_capillary_zone_options))
  {
    stop("Error: The value of the \"Simulate capillary zone\" parameter in the import data template is not an accepted value. Use one of the following: \"Yes\" or \"No\"")
  }

  #Number of Monte Carlo iterations must be numberic for stochastic simulations; it is ignored for deterministic ones
  if (simulation_type == "Stochastic")
  {
    number_of_monte_carlo_iterations <- suppressWarnings(as.numeric(settings_dfx$Value[settings_dfx$Parameter == "Number of Monte Carlo iterations"]))

    monte_carlo_iterations_error_message <- "Error: The value of the \"Number of Monte Carlo iterations\" parameter in the import data template is not an accepted value. For stochastic simulations, the number of Monte Carlo iterations must be an integer greater than zero"
    if (is.na(number_of_monte_carlo_iterations)){
      stop(monte_carlo_iterations_error_message)
    }

    if (!(number_of_monte_carlo_iterations%%1==0)){
      stop(monte_carlo_iterations_error_message)
    }
  }

  #Convert settings to values used within the runJE program
  old_parameter_names <- settings_dfx$Parameter
  settings_dfx <- data.frame(t(settings_dfx[ , - 1]))
  colnames(settings_dfx) <- old_parameter_names

  #Transpose data into a data frame with columns named after the parameter values and rename the fields
  settings_dfx <- settings_dfx %>%
    dplyr::rename(simulation_type = "Simulation type",
                  source_medium = "Source medium",
                  apply_default_building_parameters = "Apply default building parameters",
                  building_setting = "Building setting",
                  foundation_type = "Foundation type",
                  simulate_capillary_zone = "Simulate capillary zone",
                  number_of_monte_carlo_iterations = "Number of Monte Carlo iterations")


  #Simulation type
  settings_dfx <- settings_dfx %>% dplyr::mutate(simulation_type = if_else(simulation_type == "Deterministic", "DET", "MC"))

  #Foundation type
  settings_dfx <- settings_dfx %>% dplyr::mutate(foundation_type = case_when(
    foundation_type == "Closed Crawl Space w/ Slab" ~ "Crawlspace-slab",
    foundation_type == "Basement w/ Slab" ~ "Basement-slab",
    foundation_type == "Basement w/ Dirt Floor" ~ "Basement-dirt",
    foundation_type == "Closed Crawl Space w/ Dirt Floor" ~ "Crawlspace-dirt",
    foundation_type == "Slab-on-grade" ~ "Slab-grade",
    TRUE ~ foundation_type))

  #Apply default building parameters
  settings_dfx <- settings_dfx %>% dplyr::mutate(apply_default_building_parameters = case_when(
    apply_default_building_parameters == "Yes" ~ TRUE,
    apply_default_building_parameters == "No" ~ FALSE,
    TRUE ~ FALSE))

  #Simulate capillary zone
  settings_dfx <- settings_dfx %>% dplyr::mutate(simulate_capillary_zone = case_when(
    simulate_capillary_zone == "Yes" ~ TRUE,
    simulate_capillary_zone == "No" ~ FALSE,
    TRUE ~ FALSE))

  #Set the number of monte carlo iterations to a number if it's currently a string
  if (simulation_type == "Stochastic"){
    settings_dfx$number_of_monte_carlo_iterations <- number_of_monte_carlo_iterations
  }


  #################################### BuildingInfo #########################################

  #Only import building info if the "Apply default building parameters" settings parameter is set to no
  if (apply_default_building_parameters == "Yes")
  {
    building_info_dfx <- getBuildingDefaults(settings_dfx$building_setting,
                                             settings_dfx$foundation_type,
                                             settings_dfx$simulation_type)
  }

  #Check that the data frame includes the needed columns and settings
  building_info_required_columns <- c("Parameter", "JEMSymbol", "Units", "Distribution", "DistributionParameterType", "DistributionParameterValue")
  building_info_required_parameter_names <- c("Depth below grade to base of foundation", "Foundation thickness", "Fraction of foundation area with cracks", "Enclosed space floor area", "Enclosed space mixing height", "Indoor air exchange rate", "Qsoil/Qbuilding")

  if(!(all(building_info_required_columns %in% names(building_info_dfx))))
  {
    stop("Error: One or more required columns are missing from the BuildingInfo sheet of the import file")
  }

  #Remove all rows with "Not Applicable" in the DistributionParameterType field
  building_info_dfx <- building_info_dfx[!building_info_dfx$DistributionParameterType == "Not Applicable", ]

  if(!(all(building_info_required_parameter_names %in% building_info_dfx$Parameter)))
  {
    stop("Error: One or more required parameters are missing from the BuildingInfo sheet of the import file")
  }

  #Check that each parameter has the correct symbol
  if(!all(all(building_info_dfx$JEMSymbol[building_info_dfx$Parameter == "Depth below grade to base of foundation"] == "Lb") &
          all(building_info_dfx$JEMSymbol[building_info_dfx$Parameter == "Foundation thickness"] == "Lf") &
          all(building_info_dfx$JEMSymbol[building_info_dfx$Parameter == "Fraction of foundation area with cracks"] == "eta") &
          all(building_info_dfx$JEMSymbol[building_info_dfx$Parameter == "Enclosed space floor area"] == "Abf") &
          all(building_info_dfx$JEMSymbol[building_info_dfx$Parameter == "Enclosed space mixing height"] == "Hb") &
          all(building_info_dfx$JEMSymbol[building_info_dfx$Parameter == "Indoor air exchange rate"] == "ach") &
          all(building_info_dfx$JEMSymbol[building_info_dfx$Parameter == "Qsoil/Qbuilding"] == "Qsoil_Qb")))
  {
    stop("Error: Incorrect JEM symbol assigned in the BuildingInfo sheet of the import file")
  }

  #Check that each parameter has acceptable units
  if(!all(all(building_info_dfx$Units[building_info_dfx$Parameter == "Depth below grade to base of foundation"] %in% length_units) &
          all(building_info_dfx$Units[building_info_dfx$Parameter == "Foundation thickness"] %in% length_units) &
          all(building_info_dfx$Units[building_info_dfx$Parameter == "Fraction of foundation area with cracks"] %in% dimensionless_units) &
          all(building_info_dfx$Units[building_info_dfx$Parameter == "Enclosed space floor area"] %in% area_units) &
          all(building_info_dfx$Units[building_info_dfx$Parameter == "Enclosed space mixing height"] %in% length_units) &
          all(building_info_dfx$Units[building_info_dfx$Parameter == "Indoor air exchange rate"] %in% air_exchange_rate_units) &
          all(building_info_dfx$Units[building_info_dfx$Parameter == "Qsoil/Qbuilding"] %in% dimensionless_units)))
  {
    stop("Error: Incorrect units assigned in the BuildingInfo sheet of the import file")
  }

  #Check that each parameter has one of the allowed distributions
  if(!(all(building_info_dfx$Distribution %in% distribution_options)))
  {
    stop("Error: Unsupported distribution assigned in the BuildingInfo sheet of the import file")
  }

  #Check that each distribution has the appropriate parameters
  if(!all(all(building_info_dfx$DistributionParameterType[building_info_dfx$Distribution == "Constant"] %in% constant_dist_parameters) &
          all(building_info_dfx$DistributionParameterType[building_info_dfx$Distribution == "Uniform"] %in% uniform_dist_parameters) &
          all(building_info_dfx$DistributionParameterType[building_info_dfx$Distribution == "Triangular"] %in% traingular_dist_parameters) &
          all(building_info_dfx$DistributionParameterType[building_info_dfx$Distribution == "PERT"] %in% pert_dist_parameters) &
          all(building_info_dfx$DistributionParameterType[building_info_dfx$Distribution == "Truncated Normal"] %in% truncated_normal_dist_parameters) &
          all(building_info_dfx$DistributionParameterType[building_info_dfx$Distribution == "Truncated Lognormal"] %in% truncated_lognormal_dist_parameters)))
  {
    stop("Error: Incorrect distribution parameter type assigned in the BuildingInfo sheet of the import file")
  }

  #Check that none of the parameter values are missing
  if(any(is.na(building_info_dfx$DistributionParameterValue) | !is.numeric(building_info_dfx$DistributionParameterValue)))
  {
    stop("Error: Missing or non-numeric parameter value assigned in the BuildingInfo sheet of the import file")
  }

  #Convert any units in ft to m
  building_info_dfx$DistributionParameterValue[building_info_dfx$Units == "ft"] <- paramUnitConvs(building_info_dfx$DistributionParameterValue[building_info_dfx$Units == "ft"], "ft", "m")
  building_info_dfx$Units[building_info_dfx$Units == "ft"] <- "m"

  #Convert any units in ft2 to m2
  building_info_dfx$DistributionParameterValue[building_info_dfx$Units == "ft2"] <- paramUnitConvs(building_info_dfx$DistributionParameterValue[building_info_dfx$Units == "ft2"], "ft2", "m2")
  building_info_dfx$Units[building_info_dfx$Units == "ft2"] <- "m2"

  #Create a dataframe with the parameter values for deterministic simulations
  if(settings_dfx$simulation_type == "DET")
  {
    #Check that only constants were assigned in the data import template
    if(!all(all(building_info_dfx$DistributionParameterType %in% constant_dist_parameters) &
            all(building_info_dfx$Distribution == "Constant")))
    {
      stop("Error: One or more parameters on the BuildingInfo sheet isn't assigned a constant value. Only constant building parameters are allowed in a deterministic simulation.")
    }

    #Check that each parameter is only in the data frame once
    if(!(sum(building_info_dfx$JEMSymbol == "Lb") == 1 &&
         sum(building_info_dfx$JEMSymbol == "Lf") == 1 &&
         sum(building_info_dfx$JEMSymbol == "eta") == 1 &&
         sum(building_info_dfx$JEMSymbol == "Abf") == 1 &&
         sum(building_info_dfx$JEMSymbol == "Hb") == 1 &&
         sum(building_info_dfx$JEMSymbol == "ach") == 1 &&
         sum(building_info_dfx$JEMSymbol == "Qsoil_Qb") == 1))
    {
      stop("Error: More than one value is assigned to a parameter on the BuildingInfo sheet. Only one constant value is allowed per building parameter in a deterministic simulation.")
    }

    #Build the data frame
    building_info_data <- data.frame(Lb = building_info_dfx$DistributionParameterValue[building_info_dfx$JEMSymbol == "Lb"],
                                Lf = building_info_dfx$DistributionParameterValue[building_info_dfx$JEMSymbol == "Lf"],
                                eta = building_info_dfx$DistributionParameterValue[building_info_dfx$JEMSymbol == "eta"],
                                Abf = building_info_dfx$DistributionParameterValue[building_info_dfx$JEMSymbol == "Abf"],
                                Hb = building_info_dfx$DistributionParameterValue[building_info_dfx$JEMSymbol == "Hb"],
                                ach = building_info_dfx$DistributionParameterValue[building_info_dfx$JEMSymbol == "ach"],
                                Qsoil_Qb = building_info_dfx$DistributionParameterValue[building_info_dfx$JEMSymbol == "Qsoil_Qb"])
  }
  #Create a list of JEMParamDist objects for stochastic simulations
  else
  {
    #Create and assign JEMParamDist objects for each building parameter
    Lb_dist <- JEMParamDist$new("Lb", template_data_dfx = building_info_dfx)
    Lf_dist <- JEMParamDist$new("Lf", template_data_dfx = building_info_dfx)
    eta_dist <- JEMParamDist$new("eta", template_data_dfx = building_info_dfx)
    Abf_dist <- JEMParamDist$new("Abf", template_data_dfx = building_info_dfx)
    Hb_dist <- JEMParamDist$new("Hb", template_data_dfx = building_info_dfx)
    ach_dist <- JEMParamDist$new("ach", template_data_dfx = building_info_dfx)
    Qsoil_Qb_dist <- JEMParamDist$new("Qsoil_Qb", template_data_dfx = building_info_dfx)

    building_info_data <- list(Lb_dist, Lf_dist, eta_dist, Abf_dist, Hb_dist, ach_dist, Qsoil_Qb_dist)
  }

  #################################### VadoseZoneInfo #########################################


  #Check that the data frame includes the needed columns and settings
  vadose_zone_info_required_columns <- c("Parameter", "JEMSymbol", "Units", "Distribution", "DistributionParameterType", "DistributionParameterValue")
  vadose_zone_info_required_parameter_names <- c("Depth below grade to source", "Average source temperature")

  if(!(all(vadose_zone_info_required_columns %in% names(vadose_zone_info_dfx))))
  {
    stop("Error: One or more required columns are missing from the VadoseZoneInfo sheet of the import file")
  }

  #Remove all rows with "Not Applicable" in the DistributionParameterType field
  vadose_zone_info_dfx <- vadose_zone_info_dfx[!vadose_zone_info_dfx$DistributionParameterType == "Not Applicable", ]

  if(!(all(vadose_zone_info_required_parameter_names %in% vadose_zone_info_dfx$Parameter)))
  {
    stop("Error: One or more required parameters are missing from the VadoseZoneInfo sheet of the import file")
  }

  #Check that each parameter has the correct symbol
  if(!all(all(vadose_zone_info_dfx$JEMSymbol[vadose_zone_info_dfx$Parameter == "Depth below grade to source"] == "Ls") &
          all(vadose_zone_info_dfx$JEMSymbol[vadose_zone_info_dfx$Parameter == "Average source temperature"] == "Ts")))
  {
    stop("Error: Incorrect JEM symbol assigned in the VadoseZoneInfo sheet of the import file")
  }

  #Check that each parameter has acceptable units
  if(!all(all(vadose_zone_info_dfx$Units[vadose_zone_info_dfx$Parameter == "Depth below grade to source"] %in% length_units) &
          all(vadose_zone_info_dfx$Units[vadose_zone_info_dfx$Parameter == "Average source temperature"] %in% temperature_units)))
  {
    stop("Error: Incorrect units assigned in the VadoseZoneInfo sheet of the import file")
  }

  #Check that each parameter is one of the allowed distributions
  if(!(all(vadose_zone_info_dfx$Distribution %in% distribution_options)))
  {
    stop("Error: Unsupported distribution assigned in the VadoseZoneInfo sheet of the import file")
  }

  #Check that each distribution has the appropriate parameters
  if(!all(all(vadose_zone_info_dfx$DistributionParameterType[vadose_zone_info_dfx$Distribution == "Constant"] %in% constant_dist_parameters) &
          all(vadose_zone_info_dfx$DistributionParameterType[vadose_zone_info_dfx$Distribution == "Uniform"] %in% uniform_dist_parameters) &
          all(vadose_zone_info_dfx$DistributionParameterType[vadose_zone_info_dfx$Distribution == "Triangular"] %in% traingular_dist_parameters) &
          all(vadose_zone_info_dfx$DistributionParameterType[vadose_zone_info_dfx$Distribution == "PERT"] %in% pert_dist_parameters) &
          all(vadose_zone_info_dfx$DistributionParameterType[vadose_zone_info_dfx$Distribution == "Truncated Normal"] %in% truncated_normal_dist_parameters) &
          all(vadose_zone_info_dfx$DistributionParameterType[vadose_zone_info_dfx$Distribution == "Truncated Lognormal"] %in% truncated_lognormal_dist_parameters)))
  {
    stop("Error: Incorrect distribution parameter type assigned in the VadoseZoneInfo sheet of the import file")
  }

  #Check that none of the parameter values are missing
  if(any(is.na(vadose_zone_info_dfx$DistributionParameterValue) | !is.numeric(vadose_zone_info_dfx$DistributionParameterValue)))
  {
    stop("Error: Missing or non-numeric parameter value assigned in the VadoseZoneInfo sheet of the import file")
  }

  #Convert any units in ft to m
  vadose_zone_info_dfx$DistributionParameterValue[vadose_zone_info_dfx$Units == "ft"] <- paramUnitConvs(vadose_zone_info_dfx$DistributionParameterValue[vadose_zone_info_dfx$Units == "ft"], "ft", "m")
  vadose_zone_info_dfx$Units[vadose_zone_info_dfx$Units == "ft"] <- "m"

  #Convert any units in deg F to deg C
  vadose_zone_info_dfx$DistributionParameterValue[vadose_zone_info_dfx$Units == "deg F"] <- paramUnitConvs(vadose_zone_info_dfx$DistributionParameterValue[vadose_zone_info_dfx$Units == "deg F"], "deg F", "deg C")
  vadose_zone_info_dfx$Units[vadose_zone_info_dfx$Units == "deg F"] <- "deg C"

  #Create a dataframe with the parameter values for deterministic simulations
  if(settings_dfx$simulation_type == "DET")
  {
    #Check that only constants were assigned in the data import template
    if(!all(all(vadose_zone_info_dfx$DistributionParameterType %in% constant_dist_parameters) &
            all(vadose_zone_info_dfx$Distribution == "Constant")))
    {
      stop("Error: One or more parameters on the VadoseZoneInfo sheet isn't assigned a constant value. Only constant vadose zone parameters are allowed in a deterministic simulation.")
    }

    #Check that each parameter is only in the data frame once
    if(!(sum(vadose_zone_info_dfx$JEMSymbol == "Ls") == 1 &&
         sum(vadose_zone_info_dfx$JEMSymbol == "Ts") == 1))
    {
      stop("Error: More than one value is assigned to a parameter on the VadoseZoneInfo sheet. Only one constant value is allowed per vadose zone parameter in a deterministic simulation.")
    }

    #Build the data frame
    vadose_zone_info_data <- data.frame(Ls = vadose_zone_info_dfx$DistributionParameterValue[vadose_zone_info_dfx$JEMSymbol == "Ls"],
                                Ts = vadose_zone_info_dfx$DistributionParameterValue[vadose_zone_info_dfx$JEMSymbol == "Ts"])
  }
  #Create a list of JEMParamDist objects for stochastic simulations
  else
  {
    #Create and assign JEMParamDist objects for each vadose zone parameter
    Ls_dist <- JEMParamDist$new("Ls", template_data_dfx = vadose_zone_info_dfx)
    Ts_dist <- JEMParamDist$new("Ts", template_data_dfx = vadose_zone_info_dfx)

    vadose_zone_info_data <- list(Ls_dist, Ts_dist)
  }

  #################################### StrataLogs #########################################


  #Check that the data frame includes the needed columns and settings
  strata_logs_required_columns <- c("LogID", "LayerOrder", "Thickness", "Units", "SoilType")

  if(!(all(strata_logs_required_columns %in% names(strata_logs_dfx))))
  {
    stop("Error: One or more required columns are missing from the StrataLogs sheet of the import file")
  }

  #Check that none of the parameter values are missing
  if(any(is.na(strata_logs_dfx)))
  {
    stop("Error: Missing parameter value in the StrataLogs sheet of the import file")
  }

  #Check that each entry has acceptable units
  if(!all(all(strata_logs_dfx$Units %in% length_units)))
  {
    stop("Error: Incorrect units assigned in the StrataLogs sheet of the import file")
  }

  #Check that all of the soil types are one of the allowed SCS soil types
  if(!(all(strata_logs_dfx$SoilType %in% soil_type_options)))
  {
    stop("Error: Unsupported soil type assigned in the StrataLogs sheet of the import file")
  }

  #Check that the LayerOrder and Thickness field values are all numeric
  if(any(!is.numeric(strata_logs_dfx$LayerOrder) | !is.numeric(strata_logs_dfx$Thickness)))
  {
    stop("Error: Non-numeric parameter value assigned in the LayerOrder or Thickness fields of the StrataLogs sheet of the import file")
  }

  #Convert any units in ft to m
  strata_logs_dfx$Thickness[strata_logs_dfx$Units == "ft"] <- paramUnitConvs(strata_logs_dfx$Thickness[strata_logs_dfx$Units == "ft"], "ft", "m")
  strata_logs_dfx$Units[strata_logs_dfx$Units == "ft"] <- "m"

  #################################### ContaminantData #########################################

  #Check that the data frame includes the needed columns and settings
  contaminant_data_required_columns <- c("Contaminant",	"CASRN", "Medium", "Concentration",	"Units", "DetectedFlag", "SampleLocationID", "SampleDate")

  if(!(all(contaminant_data_required_columns %in% names(contaminant_data_dfx))))
  {
    stop("Error: One or more required columns are missing from the ContaminantData sheet of the import file")
  }

  #Check that none of the parameter values are missing
  if(any(is.na(contaminant_data_dfx %>% select(-"SampleDate"))))
  {
    stop("Error: Missing parameter value in the ContaminantData sheet of the import file")
  }

  #Check that each contaminant name is assigned to only one CASRN and vice-versa
  # All unique contaminant names have only one unique CASRN and vice-versa
  uniq_casrn <- unique(contaminant_data_dfx$CASRN)
  uniq_contam <- unique(contaminant_data_dfx$Contaminant)

  # function to check each casrn has only one contaminant
  check_casrn = function(uniq_casrn){

    casrn_id <- contaminant_data_dfx %>%
      dplyr::filter(.data$CASRN == uniq_casrn)

    one_contam <- length(unique(casrn_id$Contaminant)) == 1

    return(one_contam)
  }

  check_casrn_list <- lapply(uniq_casrn, check_casrn)
  one_contam_per_casrn <- all(as.logical(check_casrn_list))

  if(!one_contam_per_casrn){
    stop("Error: More than one contaminant name was assigned for the same CASRN in the ContaminantData sheet")
  }

  # function to check each contaminant has only one casrn
  check_contam = function(uniq_contam){

    contam_id <- contaminant_data_dfx %>%
      dplyr::filter(.data$Contaminant == uniq_contam)

    one_casrn <- length(unique(contam_id$CASRN)) == 1

    return(one_casrn)
  }

  check_contam_list <- lapply(uniq_contam, check_contam)
  one_casrn_per_contam <- all(as.logical(check_contam_list))

  if(!one_casrn_per_contam){
    stop("Error: More than one CASRN was assigned for the same contaminant in the ContaminantData sheet")
  }

  #Check that each entry has acceptable units
  if(!all(all(contaminant_data_dfx$Units %in% concentration_units)))
  {
    stop("Error: Incorrect units assigned in the ContaminantData sheet of the import file")
  }

  #Check that the detected flags are correctly assigned
  detected_flag_options <- c("Yes", "No")
  if(!all(all(contaminant_data_dfx$DetectedFlag %in% detected_flag_options)))
  {
    stop("Error: Unsupported value assigned in the DetectedFlag field of the ContaminantData sheet of the import file")
  }

  #Check that the medium field values are correctly assigned
  medium_options <- c("Groundwater", "Exterior Soil Gas", "Subslab Soil Gas", "Indoor Air", "Outdoor Air")
  if(!all(all(contaminant_data_dfx$Medium %in% medium_options)))
  {
    stop("Error: Unsupported value assigned in the Medium field of the ContaminantData sheet of the import file")
  }

  #Check that the Concentration values are all numeric
  if(any(!is.numeric(contaminant_data_dfx$Concentration)))
  {
    stop("Error: Non-numeric parameter value assigned in the Concentration field of the ContaminantData sheet of the import file")
  }

  #Check that all air and soil gas data are in either mass per volume or parts per units
  #Distribution sampling won't work if we have a mix of both unit types
  #Groundwater samples should all be in ug/L at this stage if there weren't any errors
  mass_volume_air_data <- contaminant_data_dfx$Medium %in% c("Exterior Soil Gas", "Subslab Soil Gas", "Indoor Air", "Outdoor Air") &
    contaminant_data_dfx$Units %in% c("mg/L", "mg/m3", "ug/L", "ug/m3")

  parts_per_air_data <- contaminant_data_dfx$Medium %in% c("Exterior Soil Gas", "Subslab Soil Gas", "Indoor Air", "Outdoor Air") &
    contaminant_data_dfx$Units %in% c("ppm", "ppb")

  if (any(mass_volume_air_data) && any(parts_per_air_data)){
    stop("Error: Indoor air and soil gas data may be entered using either \"mass per volume\" units (mg/m3, ug/m3, etc.) or units in \"parts per\" notation (ppm, ppb, etc.), but not both")
  }

  #Check that there is at least one detected value for the medium being simulated, since otherwise the code will crash in getInputConc.R
  contaminant_data_to_simulate <- contaminant_data_dfx %>% filter(.data$Medium == settings_dfx$source_medium, .data$DetectedFlag == "Yes")

  if (nrow(contaminant_data_to_simulate) == 0){
    stop("Error: The imported contaminant data set does not include any detections for the medium being simulated")
  }

  #Convert units to those required by the runJE function
  #Water concentrations are required to be in units of ug/L
  water_data <- contaminant_data_dfx$Medium == "Groundwater"

  contaminant_data_dfx$Concentration <- sapply(1:length(water_data), function(i){
    ifelse(water_data[i], concUnitConvs(contaminant_data_dfx$Concentration[i], contaminant_data_dfx$Units[i], "ug/L", "Water"), contaminant_data_dfx$Concentration[i])
  })

  contaminant_data_dfx$Units[water_data] <- "ug/L"

  #Air concentrations must be in units of ug/m3 or ppb
  #They will be input using one of two systems: "mass/volume" or "parts per"
  #If the units are in ppb or ppm, they will be converted to ug/m3 within the runJE function
  #once the source temperature is established

  #Air data with mass/volume units
  mass_volume_air_data <- contaminant_data_dfx$Medium %in% c("Exterior Soil Gas", "Subslab Soil Gas", "Indoor Air", "Outdoor Air") &
    contaminant_data_dfx$Units %in% c("mg/L", "mg/m3", "ug/L", "ug/m3")

  contaminant_data_dfx$Concentration <- sapply(1:length(mass_volume_air_data), function(i){
    ifelse(mass_volume_air_data[i], concUnitConvs(contaminant_data_dfx$Concentration[i], contaminant_data_dfx$Units[i], "ug/m3", "Air"), contaminant_data_dfx$Concentration[i])
  })

  contaminant_data_dfx$Units[mass_volume_air_data] <- "ug/m3"

  #Air data with parts per units
  parts_per_air_data <- contaminant_data_dfx$Medium %in% c("Exterior Soil Gas", "Subslab Soil Gas", "Indoor Air", "Outdoor Air") &
    contaminant_data_dfx$Units %in% c("ppm", "ppb")

  contaminant_data_dfx$Concentration <- sapply(1:length(parts_per_air_data), function(i){
    ifelse(parts_per_air_data[i], concUnitConvs(contaminant_data_dfx$Concentration[i], contaminant_data_dfx$Units[i], "ppb", "Air"), contaminant_data_dfx$Concentration[i])
  })

  contaminant_data_dfx$Units[parts_per_air_data] <- "ppb"

  #Convert detected flag to true/false
  contaminant_data_dfx <- contaminant_data_dfx %>% dplyr::mutate(DetectedFlag = case_when(
    DetectedFlag == "Yes" ~ TRUE,
    DetectedFlag == "No" ~ FALSE,
    TRUE ~ FALSE))

  ########################### Reference Air Concentrations ############################

  #Check that the data frame includes the needed columns and settings
  reference_air_conc_data_required_columns <- c("Contaminant",	"CASRN", "ReferenceConcentrationName", "Concentration",	"Units")

  if(!(all(reference_air_conc_data_required_columns %in% names(reference_air_conc_dfx))))
  {
    stop("Error: One or more required columns are missing from the ReferenceAirConcentrations sheet of the import file")
  }

  #Do the next three checks only if data are present in the data frame
  if(nrow(reference_air_conc_dfx) > 0){

    #Check that none of the parameter values are missing
    if(any(is.na(reference_air_conc_dfx)))
    {
      stop("Error: Missing parameter value in the ReferenceAirConcentrations sheet of the import file")
    }

    #Check that each entry has acceptable units
    if(!all(all(reference_air_conc_dfx$Units %in% concentration_units)))
    {
      stop("Error: Incorrect units assigned in the ReferenceAirConcentrations sheet of the import file")
    }

    #Check that the Concentration values are all numeric
    if(any(!is.numeric(reference_air_conc_dfx$Concentration)))
    {
      stop("Error: Non-numeric parameter value assigned in the Concentration field of the ReferenceAirConcentrations sheet of the import file")
    }

    #Check that each contaminant name is assigned to only one CASRN and vice-versa
    # All unique contaminant names have only one unique CASRN and vice-versa
    uniq_casrn <- unique(reference_air_conc_dfx$CASRN)
    uniq_contam <- unique(reference_air_conc_dfx$Contaminant)

    # function to check each casrn has only one contaminant
    check_casrn = function(uniq_casrn){

      casrn_id <- reference_air_conc_dfx %>%
        dplyr::filter(.data$CASRN == uniq_casrn)

      one_contam <- length(unique(casrn_id$Contaminant)) == 1

      return(one_contam)
    }

    check_casrn_list <- lapply(uniq_casrn, check_casrn)
    one_contam_per_casrn <- all(as.logical(check_casrn_list))

    if(!one_contam_per_casrn){
      stop("Error: More than one contaminant name was assigned for the same CASRN in the ReferenceAirConcentrations sheet")
    }

    # function to check each contaminant has only one casrn
    check_contam = function(uniq_contam){

      contam_id <- reference_air_conc_dfx %>%
        dplyr::filter(.data$Contaminant == uniq_contam)

      one_casrn <- length(unique(contam_id$CASRN)) == 1

      return(one_casrn)
    }

    check_contam_list <- lapply(uniq_contam, check_contam)
    one_casrn_per_contam <- all(as.logical(check_contam_list))

    if(!one_casrn_per_contam){
      stop("Error: More than one CASRN was assigned for the same contaminant in the ReferenceAirConcentrations sheet")
    }

  }

  #################################### Output #########################################

  #We just created six data frames. Let's output them now as a list so that all six can be assigned to one object.
  imported_data_lx <- list (contaminant_data_dfx, building_info_data, vadose_zone_info_data, strata_logs_dfx, settings_dfx, reference_air_conc_dfx)
  return (imported_data_lx)

}
