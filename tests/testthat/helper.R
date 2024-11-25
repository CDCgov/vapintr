get_default_deterministic_test_data <- function()
{
  contam_dfx <- data.frame(
    Contaminant = "Tetrachloroethylene",
    Cmedium = 10,
    Units = "ug/L"
  )

  building_dfx <- data.frame(
    Abf = 150,
    ach = 0.45,
    eta = 0.001,
    Hb = 3.66,
    Lb = 2,
    Lf = 0.1,
    Qsoil_Qb = 0.003
  )

  source_dfx <- data.frame(
    Ls = 9.54,
    Ts = 25
  )

  boring_log_dfx <- data.frame(
    LogID = c(1, 1, 1),
    LayerOrder = c(1, 2, 3),
    SoilType = c("Sandy Clay", "Clay", "Not Present"),
    Thickness = c(2.54, 7, NA),
    check.names = FALSE
  )

  stratum_dfx <- boring_log_dfx

  boring_log_dfx <- data.frame(
    LogID = c(2, 2, 2),
    LayerOrder = c(1, 2, 3),
    SoilType = c("Sand", "Clay Loam", "Loamy Sand"),
    Thickness = c(3, 1, 1),
    check.names = FALSE
  )

  stratum_dfx <- rbind(stratum_dfx, boring_log_dfx)

  boring_log_dfx <- data.frame(
    LogID = c(3, 3, 3),
    LayerOrder = c(1, 2, 3),
    SoilType = c("Sand", "Not Present", "Not Present"),
    Thickness = c(2.5, NA, NA),
    check.names = FALSE
  )

  stratum_dfx <- rbind(stratum_dfx, boring_log_dfx)

  boring_log_dfx <- data.frame(
    LogID = c(4, 4, 4),
    LayerOrder = c(1, 2, 3),
    SoilType = c("Sand", "Clay Loam", "Loamy Sand"),
    Thickness = c(3, 1, 1),
    check.names = FALSE
  )

  stratum_dfx <- rbind(stratum_dfx, boring_log_dfx)

  settings_dfx <- data.frame(
    source_medium = "Groundwater",
    simulation_type = "DET",
    simulate_capillary_zone = TRUE,
    building_setting = "Residential",
    foundation_type = "Basement-slab",
    number_of_monte_carlo_iterations = NA
  )

  test_data <- list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx)
  return(test_data)
}

get_default_monte_carlo_test_data <- function()
{
  #Create JEMParamDist object for contaminant data
  contaminant_data <- JEMParamDist$new("Cmedium",
                                       name = "Tetrachloroethylene",
                                       units = "ppb",
                                       dist_type = "PERT",
                                       minimum = 8,
                                       mode = 10,
                                       maximum = 12)

  #Create JEMParamDist objects for building parameters
  Lb_dist <- JEMParamDist$new("Lb",
                              name = "Depth below grade to base of foundation",
                              units = "m",
                              dist_type = "Constant",
                              constant = 2)

  Lf_dist <- JEMParamDist$new("Lf",
                              name = "Foundation thickness",
                              units = "m",
                              dist_type = "Constant",
                              constant = 0.1)

  eta_dist <- JEMParamDist$new("eta",
                               name = "Fraction of foundation area with cracks",
                               units = "-",
                               dist_type = "PERT",
                               minimum = 0.00019,
                               mode = 0.001,
                               maximum = 0.0019)

  Abf_dist <- JEMParamDist$new("Abf",
                               name = "Enclosed space floor area",
                               units = "m²",
                               dist_type = "Constant",
                               constant = 150)

  Hb_dist <- JEMParamDist$new("Hb",
                              name = "Enclosed space mixing height",
                              units = "m",
                              dist_type = "Constant",
                              constant = 2.43)

  ach_dist <- JEMParamDist$new("ach",
                               name = "Indoor air exchange rate",
                               units = "1/hr",
                               dist_type = "PERT",
                               minimum = 0.15,
                               mode = 0.45,
                               maximum = 1.26)


  Qsoil_Qb_dist <- JEMParamDist$new("Qsoil_Qb",
                                    name = "Qsoil/Qbuilding",
                                    units = "-",
                                    dist_type = "Constant",
                                    constant = 0.003)

  building_data <- list(Lb_dist, Lf_dist, eta_dist, Abf_dist, Hb_dist, ach_dist, Qsoil_Qb_dist)

  #Create JEMParamDist objects for vadose zone parameters
  Ls_dist <- JEMParamDist$new("Ls",
                              name = "Depth below grade to source",
                              units = "m",
                              dist_type = "PERT",
                              minimum = 7,
                              mode = 10,
                              maximum = 13)

  Ts_dist <- JEMParamDist$new("Ts",
                              name = "Average source temperature",
                              units = "°C",
                              dist_type = "PERT",
                              minimum = 12,
                              mode = 18,
                              maximum = 24)

  vadose_zone_info_data <- list(Ls_dist, Ts_dist)

  boring_log_dfx <- data.frame(
    LogID = c(1, 1, 1),
    LayerOrder = c(1, 2, 3),
    SoilType = c("Sand", "Sandy Clay", "Clay Loam"),
    Thickness = c(5, 2.54, 7),
    check.names = FALSE
  )

  stratum_dfx <- boring_log_dfx

  boring_log_dfx <- data.frame(
    LogID = c(2, 2, 2, 2),
    LayerOrder = c(1, 2, 3, 4),
    SoilType = c("Sand", "Sandy Clay", "Clay Loam", "Loamy Sand"),
    Thickness = c(5, 2.54, 5, 3),
    check.names = FALSE
  )

  stratum_dfx <- rbind(stratum_dfx, boring_log_dfx)

  boring_log_dfx <- data.frame(
    LogID = c(3, 3, 3),
    LayerOrder = c(1, 2, 3),
    SoilType = c("Sand", "Not Present", "Not Present"),
    Thickness = c(15, NA, NA),
    check.names = FALSE
  )

  stratum_dfx <- rbind(stratum_dfx, boring_log_dfx)

  settings_dfx <- data.frame(
    source_medium = "Groundwater",
    simulation_type = "MC",
    simulate_capillary_zone = TRUE,
    building_setting = "Residential",
    foundation_type = "Slab-grade",
    number_of_monte_carlo_iterations = 5
  )

  test_data <- list (contaminant_data, building_data, vadose_zone_info_data, stratum_dfx, settings_dfx)
  return(test_data)
}

get_stochastic_test_template_data <- function()
{

  settings_dfx <- data.frame(
    Parameter = c("Simulation type",
                  "Source medium",
                  "Apply default building parameters",
                  "Building setting",
                  "Foundation type",
                  "Simulate capillary zone",
                  "Number of Monte Carlo iterations"),
    Value = c("Stochastic",
              "Groundwater",
              "No",
              "Residential",
              "Slab-on-grade",
              "Yes",
              5)
  )

  building_dfx <- data.frame (
    Parameter = c("Depth below grade to base of foundation",
                 "Foundation thickness",
                 "Foundation thickness",
                 "Fraction of foundation area with cracks",
                 "Fraction of foundation area with cracks",
                 "Fraction of foundation area with cracks",
                 "Enclosed space floor area",
                 "Enclosed space mixing height",
                 "Enclosed space mixing height",
                 "Indoor air exchange rate",
                 "Indoor air exchange rate",
                 "Indoor air exchange rate",
                 "Qsoil/Qbuilding"),
    Units = c("m",
              "m",
              "m",
              "-","-","-",
              "m2",
              "m","m",
              "1/hr","1/hr","1/hr",
              "-"),
    JEMSymbol = c("Lb",
                  "Lf",
                  "Lf",
                  "eta","eta","eta",
                  "Abf",
                  "Hb", "Hb",
                  "ach","ach","ach",
                  "Qsoil_Qb"),
    Distribution = c("Constant",
                     "Constant",
                     "Constant",
                     "PERT","PERT","PERT",
                     "Constant",
                     "Uniform","Uniform",
                     "PERT","PERT","PERT",
                     "Constant"),
    DistributionParameterType = c("Constant",
                      "Constant",
                      "Not Applicable",
                      "Maximum","Mode","Minimum",
                      "Constant",
                      "Maximum", "Minimum",
                      "Maximum", "Mode", "Minimum",
                      "Constant"),
    DistributionParameterValue = c(0.1524,
                      0.1524,
                      NA,
                      0.0019, 0.001, 0.00019,
                      175,
                      2.75, 2.43,
                      1.26, 0.45, 0.15,
                      0.003))

  source_dfx <- data.frame(
    Parameter= c("Depth below grade to source",
                "Depth below grade to source",
                "Depth below grade to source",
                "Average source temperature",
                "Average source temperature",
                "Average source temperature"),
    Units= c("m","m","m",
            "deg C","deg C","deg C"),
    JEMSymbol= c("Ls","Ls","Ls",
                 "Ts","Ts","Ts"),
    Distribution= c("PERT","PERT","PERT",
                    "PERT","PERT","PERT"),
    DistributionParameterType= c("Maximum", "Mode", "Minimum",
                      "Maximum", "Mode", "Minimum"),
    DistributionParameterValue = c(23.4, 15.4, 9.03,
                      24, 18, 12))

  stratum_dfx <- data.frame(
    LogID= c("IS05","IS05","IS05",
             "IS06","IS06","IS06",
             "35MW29B","35MW29B","35MW29B","35MW29B",
             "35MW38B","35MW38B","35MW38B","35MW38B","35MW38B"),
    LayerOrder= c(1,2,3,
                  1,2,3,
                  1,2,3,4,
                  1,2,3,4,5),
    Thickness= c(3,10,18,
                 3,5,21.5,
                 3,12.3,13,17.2,
                 3,1.7,1,21.5,1.7),
    Units= c("m","m","m",
             "m","m","m",
             "m","m","m","m",
             "m","m","m","m","m"),
    SoilType= c("Loamy Sand","Sandy Loam","Sand",
                "Loamy Sand","Sandy Loam","Sand",
                "Loamy Sand","Silt","Silt Loam","Sand",
                "Loamy Sand", "Sandy Loam", "Silt","Sand","Loamy Sand"))

  contam_dfx <- data.frame(
    Contaminant= c("Tetrachloroethylene",
                   "Tetrachloroethylene",
                   "Tetrachloroethylene",
                   "Tetrachloroethylene",
                   "Tetrachloroethylene",
                   "Tetrachloroethylene",
                   "Tetrachloroethylene",
                   "Tetrachloroethylene",
                   "Tetrachloroethylene",
                   "Tetrachloroethylene"),
    CASRN= c("000127-18-4",
             "000127-18-4",
             "000127-18-4",
             "000127-18-4",
             "000127-18-4",
             "000127-18-4",
             "000127-18-4",
             "000127-18-4",
             "000127-18-4",
             "000127-18-4"),
    Medium= c("Indoor Air",
              "Indoor Air",
              "Indoor Air",
              "Indoor Air",
              "Groundwater",
              "Groundwater",
              "Groundwater",
              "Groundwater",
              "Groundwater",
              "Groundwater"),
    Concentration= c(4,
                     3.7,
                     12.8,
                     12.85,
                     16,
                     19,
                     14,
                     16,
                     22,
                     18),
    Units= c("mg/m3",
             "mg/m3",
             "mg/m3",
             "mg/m3",
             "mg/m3",
             "mg/m3",
             "mg/m3",
             "mg/m3",
             "mg/m3",
             "mg/m3"),
    DetectedFlag= c("Yes","No","Yes","Yes","Yes","Yes","Yes","Yes","No","Yes"),
    SampleLocationID= c("MW-2",
                   "MW-4",
                   "MW-2",
                   "MW-5",
                   "MW-5",
                   "MW-2",
                   "MW-1",
                   "MW-1",
                   "MW-3",
                   "MW-5"),
    SampleDate= c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))

  ref_air_conc_dfx <- data.frame(
    Contaminant = c("Tetrachloroethylene", "Tetrachloroethylene", "Tetrachloroethylene", "Tetrachloroethylene"),
    CASRN = c("000127-18-4", "000127-18-4", "000127-18-4", "000127-18-4"),
    ReferenceConcentrationName = c("ATSDR Acute EMEG", "ATSDR Intermediate EMEG", "ATSDR Chronic EMEG", "ATSDR CREG"),
    Concentration = c(41, 41, 41, 3.8),
    Units = c("ug/m3", "ug/m3", "ug/m3", "ug/m3")
  )

  test_data <- list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)
  return(test_data)
}

get_deterministic_test_template_data <- function()
{

  settings_dfx <- data.frame(
    Parameter = c("Simulation type",
                  "Source medium",
                  "Apply default building parameters",
                  "Building setting",
                  "Foundation type",
                  "Simulate capillary zone",
                  "Number of Monte Carlo iterations"),
    Value = c("Deterministic",
              "Groundwater",
              "No",
              "Residential",
              "Slab-on-grade",
              "Yes",
              "NA")
  )

  building_dfx <- data.frame (
    Parameter = c("Depth below grade to base of foundation",
                 "Foundation thickness",
                 "Fraction of foundation area with cracks",
                 "Enclosed space floor area",
                 "Enclosed space mixing height",
                 "Indoor air exchange rate",
                 "Qsoil/Qbuilding"),
    Units = c("m",
              "m",
              "-",
              "m2",
              "m",
              "1/hr",
              "-"),
    JEMSymbol = c("Lb",
                  "Lf",
                  "eta",
                  "Abf",
                  "Hb",
                  "ach",
                  "Qsoil_Qb"),
    Distribution = c("Constant",
                     "Constant",
                     "Constant",
                     "Constant",
                     "Constant",
                     "Constant",
                     "Constant"),
    DistributionParameterType = c("Constant",
                      "Constant",
                      "Constant",
                      "Constant",
                      "Constant",
                      "Constant",
                      "Constant"),
    DistributionParameterValue = c(0.1524,
                      0.1524,
                      0.001,
                      175,
                      2.6,
                      0.45,
                      0.003))

  source_dfx <- data.frame(
    Parameter= c("Depth below grade to source",
                "Average source temperature"),
    Units= c("m",
             "deg C"),
    JEMSymbol= c("Ls",
                 "Ts"),
    Distribution= c("Constant",
                    "Constant"),
    DistributionParameterType = c("Constant",
                     "Constant"),
    DistributionParameterValue = c(15.4,
                      18))

  stratum_dfx <- data.frame(
    LogID= c("IS05","IS05","IS05",
             "IS06","IS06","IS06",
             "35MW29B","35MW29B","35MW29B","35MW29B",
             "35MW38B","35MW38B","35MW38B","35MW38B","35MW38B"),
    LayerOrder= c(1,2,3,
                  1,2,3,
                  1,2,3,4,
                  1,2,3,4,5),
    Thickness= c(3,10,18,
                 3,5,21.5,
                 3,12.3,13,17.2,
                 3,1.7,1,21.5,1.7),
    Units= c("m","m","m",
             "m","m","m",
             "m","m","m","m",
             "m","m","m","m","m"),
    SoilType= c("Loamy Sand","Sandy Loam","Sand",
                "Loamy Sand","Sandy Loam","Sand",
                "Loamy Sand","Silt","Silt Loam","Sand",
                "Loamy Sand", "Sandy Loam", "Silt","Sand","Loamy Sand"))

  contam_dfx <- data.frame(
    Contaminant= c("Tetrachloroethylene",
                   "Tetrachloroethylene",
                   "Tetrachloroethylene",
                   "Tetrachloroethylene",
                   "Tetrachloroethylene",
                   "Tetrachloroethylene",
                   "Tetrachloroethylene",
                   "Tetrachloroethylene",
                   "Tetrachloroethylene",
                   "Tetrachloroethylene"),
    CASRN= c("000127-18-4",
             "000127-18-4",
             "000127-18-4",
             "000127-18-4",
             "000127-18-4",
             "000127-18-4",
             "000127-18-4",
             "000127-18-4",
             "000127-18-4",
             "000127-18-4"),
    Medium= c("Groundwater",
              "Groundwater",
              "Groundwater",
              "Groundwater",
              "Groundwater",
              "Groundwater",
              "Groundwater",
              "Groundwater",
              "Groundwater",
              "Groundwater"),
    Concentration= c(4,
                     3.7,
                     12.8,
                     12.85,
                     16,
                     19,
                     14,
                     16,
                     22,
                     18),
    Units= c("mg/L",
             "ug/L",
             "mg/m3",
             "ug/m3",
             "mg/L",
             "mg/m3",
             "ug/L",
             "ug/m3",
             "ppm",
             "ppb"),
    DetectedFlag= c("Yes","No","Yes","Yes","Yes","Yes","Yes","Yes","No","Yes"),
    SampleLocationID= c("MW-2",
                   "MW-4",
                   "MW-2",
                   "MW-5",
                   "MW-5",
                   "MW-2",
                   "MW-1",
                   "MW-1",
                   "MW-3",
                   "MW-5"),
    SampleDate= c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))

  ref_air_conc_dfx <- data.frame(
    Contaminant = c("Tetrachloroethylene", "Tetrachloroethylene", "Tetrachloroethylene", "Tetrachloroethylene"),
    CASRN = c("000127-18-4", "000127-18-4", "000127-18-4", "000127-18-4"),
    ReferenceConcentrationName = c("ATSDR Acute EMEG", "ATSDR Intermediate EMEG", "ATSDR Chronic EMEG", "ATSDR CREG"),
    Concentration = c(41, 41, 41, 3.8),
    Units = c("ug/m3", "ug/m3", "ug/m3", "ug/m3")
  )

  test_data <- list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)
  return(test_data)
}
