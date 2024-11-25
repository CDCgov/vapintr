################################ settings_data #######################################

test_that("correct error thrown when no settings data are assigned", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  settings_dfx <- NA

  errorMessage <- "Error: no values assigned for the settings_data input to the runJE function"

  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when settings data are not a data frame", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  settings_dfx <- list(settings_dfx)

  errorMessage <- "Error: the settings_data input to the runJE function must be a data frame for the function to proceed"

  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when a settings data field is missing", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: One or more required fields are missing from the \"settings_data\" data frame in the runJE function"

  settings_dfx <- settings_dfx %>% select(-simulation_type)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  settings_dfx <- det_test_data[[5]]
  settings_dfx <- settings_dfx %>% select(-source_medium)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  settings_dfx <- det_test_data[[5]]
  settings_dfx <- settings_dfx %>% select(-building_setting)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  settings_dfx <- det_test_data[[5]]
  settings_dfx <- settings_dfx %>% select(-foundation_type)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  settings_dfx <- det_test_data[[5]]
  settings_dfx <- settings_dfx %>% select(-simulate_capillary_zone)
  errorMessage <- "Error: The \"simulate_capillary_zone\" field is missing from the \"settings_data\" data frame in the runJE function"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  settings_dfx <- det_test_data[[5]]
  settings_dfx$simulation_type <- "MC"
  settings_dfx <- settings_dfx %>% select(-number_of_monte_carlo_iterations)
  errorMessage <- "Error: The \"number_of_monte_carlo_iterations\" field is missing from the \"settings_data\" data frame in the runJE function"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when the settings_data$simulation_type parameter isn't a known value ", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  settings_dfx$simulation_type[[1]] <- "stochastic"

  errorMessage <- "Error: The value of the \"settings_data$simulation_type\" parameter is not an accepted value. Use one of the following: \"DET\" or \"MC\""

  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when the settings_data$source_medium parameter isn't a known value ", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  settings_dfx$source_medium[[1]] <- "Sub-slab soil gas"

  errorMessage <- "Error: The value of the \"settings_data$source_medium\" parameter is not an accepted value. Use one of the following: \"Groundwater\", \"Exterior Soil Gas\" or \"Subslab Soil Gas\""

  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when the settings_data$building_setting parameter isn't a known value ", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  settings_dfx$building_setting[[1]] <- "Industrial"

  errorMessage <- "Error: The value of the \"settings_data$building_setting\" parameter is not an accepted value. Use one of the following: \"Residential\" or \"Commercial\""

  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when the settings_data$foundation_type parameter isn't a known value ", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  settings_dfx$foundation_type[[1]] <- "Basement w/ Slab"

  errorMessage <- "Error: The value of the \"settings_data$foundation_type\" parameter is not an accepted value. Use one of the following: \"Basement-slab\", \"Basement-dirt\", \"Slab-grade\", \"Crawlspace-slab\", \"Crawlspace-dirt\""

  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when the settings_data$simulate_capillary_zone parameter isn't a known value ", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  settings_dfx$simulate_capillary_zone[[1]] <- "Yes"

  errorMessage <- "Error: The value of the \"settings_data$simulate_capillary_zone\" parameter is not an accepted value. Use one of the following: TRUE or FALSE"

  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when the settings_data$number_of_monte_carlo_iterations parameter isn't an integer", {

  test_template_data <- get_default_monte_carlo_test_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]

  errorMessage <- "Error: The value of the \"settings_data$number_of_monte_carlo_iterations\" parameter is not an accepted value. For stochastic simulations, the number of Monte Carlo iterations must be an integer greater than zero"

  settings_dfx$number_of_monte_carlo_iterations <- "NotAcceptedValue"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  settings_dfx$number_of_monte_carlo_iterations <- 100.5
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

#################################### building_data #########################################

test_that("det sim: correct error message is thrown when building_data is the wrong class", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  building_dfx <- list(building_dfx)

  errorMessage <- "Error: In a deterministic simulation, the \"building_data\" input to the runJE function must be a data frame for the function to proceed"

  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("mc sim: correct error message is thrown when building_data is the wrong class", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  mc_building_data <- data.frame()

  errorMessage <- "Error: In a stochastic simulation, the \"building_data\" input to the runJE function must be a list for the function to proceed"

  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("mc sim: correct error message is thrown when a building_data list item is not a JEMParamDist object", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  mc_building_data[[1]] <- data.frame()

  errorMessage <- "Error: In a stochastic simulation, all items in the \"building_data\" input list must be R6 JEMParamDist objects"

  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("det sim: correct error messages are thrown when parameters are missing from building_data", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: One or more parameters required to execute the runJE function is missing from the \"building_data\" data frame"

  building_dfx <- building_dfx %>% select(-Lb)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx <- building_dfx %>% select(-Lf)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx <- building_dfx %>% select(-eta)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx <- building_dfx %>% select(-Abf)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx <- building_dfx %>% select(-Hb)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx <- building_dfx %>% select(-ach)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx <- building_dfx %>% select(-Qsoil_Qb)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("det sim: correct error message thrown when building_data has more than one row", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The \"building_data\" data frame has more than one row. Only one value can be applied for each building parameter in a deterministic simulation."

  building_dfx <- rbind(building_dfx, building_dfx)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

})

test_that("det sim: correct error messages are thrown when a required parameter in building_data is NA", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The \"building_data\" data input to the runJE function cannot include NA values in any of the required fields (Lb, Lf, eta, Abf, Hb, ach, and Qsoil_Qb)"

  building_dfx$Lb <- NA
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$Lf <- NA
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$eta <- NA
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$Abf <- NA
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$Hb <- NA
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$ach <- NA
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$Qsoil_Qb <- NA
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("det sim: correct error messages are thrown when a required parameter in building_data is not numeric", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: One or more of the values in the \"building_data\" data frame required fields (Lb, Lf, eta, Abf, Hb, ach, and Qsoil_Qb) is non-numeric"

  building_dfx$Lb <- "text"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$Lf <- "text"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$eta <- "text"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$Abf <- "text"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$Hb <- "text"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$ach <- "text"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$Qsoil_Qb <- "text"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("det sim: correct error messages are thrown when a required parameter in building_data is negative", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: One or more of the values in the \"building_data\" data frame required fields (Lb, Lf, eta, Abf, Hb, ach, and Qsoil_Qb) is negative"

  building_dfx$Lb <- -1
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$Lf <- -1
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$eta <- -1
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$Abf <- -1
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$Hb <- -1
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$ach <- -1
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  building_dfx <- det_test_data[[2]]
  building_dfx$Qsoil_Qb <- -1
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("mc sim: correct error messages are thrown when parameters are missing from building_data", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  dummyDist <- JEMParamDist$new("dummySymbol", name = "dummyDist", units = "dummyUnits", dist_type = "Constant", constant = 1)

  errorMessage <- "Error: One or more parameters required to execute the runJE function are missing from the \"building_data\" list"

  #Lb
  mc_building_data[[1]] <- dummyDist
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  #Lf
  mc_building_data <- mc_test_data[[2]]
  mc_building_data[[2]] <- dummyDist
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  #eta
  mc_building_data <- mc_test_data[[2]]
  mc_building_data[[3]] <- dummyDist
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  #Abf
  mc_building_data <- mc_test_data[[2]]
  mc_building_data[[4]] <- dummyDist
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  #Hb
  mc_building_data <- mc_test_data[[2]]
  mc_building_data[[5]] <- dummyDist
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  #ach
  mc_building_data <- mc_test_data[[2]]
  mc_building_data[[6]] <- dummyDist
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  #Qsoil_Qb
  mc_building_data <- mc_test_data[[2]]
  mc_building_data[[7]] <- dummyDist
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

})

test_that("mc sim: correct error message thrown when a parameter distribution is represented more than once in building_data", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  mc_building_data <- append(mc_building_data, mc_building_data[[1]])

  errorMessage <- "Error: One or more parameters has multiple JEMParamDist objects included in the \"building_data\" list"

  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

})

test_that("mc sim: correct error messages are thrown when building_data parameters are negative", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  #Lb
  mc_building_data[[1]] <- JEMParamDist$new("Lb", name = "name", units = "units", dist_type = "Constant", constant = -1)
  errorMessage <- "Error: the assigned constant value in the 'Lb' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  #Lf
  mc_building_data <- mc_test_data[[2]]
  mc_building_data[[2]] <- JEMParamDist$new("Lf", name = "name", units = "units", dist_type = "Constant", constant = -1)
  errorMessage <- "Error: the assigned constant value in the 'Lf' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  #eta
  mc_building_data <- mc_test_data[[2]]
  mc_building_data[[3]] <- JEMParamDist$new("eta", name = "name", units = "units", dist_type = "Constant", constant = -1)
  errorMessage <- "Error: the assigned constant value in the 'eta' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  #Abf
  mc_building_data <- mc_test_data[[2]]
  mc_building_data[[4]] <- JEMParamDist$new("Abf", name = "name", units = "units", dist_type = "Constant", constant = -1)
  errorMessage <- "Error: the assigned constant value in the 'Abf' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  #Hb
  mc_building_data <- mc_test_data[[2]]
  mc_building_data[[5]] <- JEMParamDist$new("Hb", name = "name", units = "units", dist_type = "Constant", constant = -1)
  errorMessage <- "Error: the assigned constant value in the 'Hb' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  #ach
  mc_building_data <- mc_test_data[[2]]
  mc_building_data[[6]] <- JEMParamDist$new("ach", name = "name", units = "units", dist_type = "Constant", constant = -1)
  errorMessage <- "Error: the assigned constant value in the 'ach' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  #Qsoil_Qb
  mc_building_data <- mc_test_data[[2]]
  mc_building_data[[7]] <- JEMParamDist$new("Qsoil_Qb", name = "name", units = "units", dist_type = "Constant", constant = -1)
  errorMessage <- "Error: the assigned constant value in the 'Qsoil_Qb' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("det sim: correct error message thrown when foundation thickness isn't zero for dirt floors", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: Foundation thickness (Lf) must equal zero for buildings with dirt floors"

  settings_dfx$foundation_type <- "Basement-dirt"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  settings_dfx$foundation_type <- "Crawlspace-dirt"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("mc sim: correct error message thrown when foundation thickness isn't zero for dirt floors", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: Foundation thickness (Lf) must equal zero for buildings with dirt floors"

  settings_dfx$foundation_type <- "Basement-dirt"
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  settings_dfx$foundation_type <- "Crawlspace-dirt"
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

})

test_that("det sim: correct warning message thrown when foundation thickness isn't within a reasonable range", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  warningMessage <- "Warning: The value input for foundation thickness (Lf) is outside of the expected range (0.1 - 0.25 meters)"

  building_dfx$Lf <- 0.05
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  building_dfx$Lf <- 0.30
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)
})

test_that("mc sim: correct warning message thrown when foundation thickness isn't zero for dirt floors", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  warningMessage <- "Warning: One or more parameter values input for foundation thickness (Lf) is outside of the expected range (0.1 - 0.25 meters)"

  mc_building_data[[2]] <- JEMParamDist$new("Lf", name = "name", units = "m", dist_type = "Uniform", minimum = 0.05, maximum = 0.15)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  mc_building_data[[2]] <- JEMParamDist$new("Lf", name = "name", units = "m", dist_type = "Uniform", minimum = 0.15, maximum = 0.30)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

})

test_that("det sim: correct warning message thrown when fraction of foundation area with cracks isn't within a reasonable range", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  warningMessage <- "Warning: The value input for the fraction of the foundation area with cracks (eta) is outside of the expected range (0.0001 - 0.001)"

  building_dfx$eta <- 0.00001
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  building_dfx$eta <- 0.01
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  settings_dfx$foundation_type <- "Slab-grade"

  warningMessage <- "Warning: The value input for the fraction of the foundation area with cracks (eta) is outside of the expected range (0.00019 - 0.0019)"

  building_dfx$eta <- 0.00001
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  building_dfx$eta <- 0.01
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

})

test_that("mc sim: correct warning message thrown when fraction of foundation area with cracks isn't within a reasonable range", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  settings_dfx$foundation_type <- "Crawlspace-slab"

  warningMessage <- "Warning: One or more parameter values input for the fraction of the foundation area with cracks (eta) is outside of the expected range (0.0001 - 0.001)"

  mc_building_data[[3]] <- JEMParamDist$new("eta", name = "name", units = "-", dist_type = "Uniform", minimum = 0.00001, maximum = 0.0002)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  mc_building_data[[3]] <- JEMParamDist$new("eta", name = "name", units = "-", dist_type = "Uniform", minimum = 0.0002, maximum = 0.01)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  settings_dfx$foundation_type <- "Slab-grade"

  warningMessage <- "Warning: One or more parameter values input for the fraction of the foundation area with cracks (eta) is outside of the expected range (0.00019 - 0.0019)"

  mc_building_data[[3]] <- JEMParamDist$new("eta", name = "name", units = "-", dist_type = "Uniform", minimum = 0.00001, maximum = 0.0002)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  mc_building_data[[3]] <- JEMParamDist$new("eta", name = "name", units = "-", dist_type = "Uniform", minimum = 0.0002, maximum = 0.01)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

})

test_that("det sim: correct warning message thrown when building floor area isn't within a reasonable range", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  settings_dfx$building_setting <- "Residential"

  warningMessage <- "Warning: The value input for the enclosed space floor area (Abf) is outside of the expected range (80 - 200 square meters)"

  building_dfx$Abf <- 70
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  building_dfx$Abf <- 250
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  settings_dfx$building_setting <- "Commercial"

  warningMessage <- "Warning: The value input for the enclosed space floor area (Abf) is outside of the expected range (80 - 1500 square meters)"

  building_dfx$Abf <- 70
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  building_dfx$Abf <- 1600
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

})

test_that("mc sim: correct warning message thrown when building floor area isn't within a reasonable range", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  settings_dfx$building_setting <- "Residential"

  warningMessage <- "Warning: One or more parameter values input for the enclosed space floor area (Abf) is outside of the expected range (80 - 200 square meters)"

  mc_building_data[[4]] <- JEMParamDist$new("Abf", name = "name", units = "-", dist_type = "Uniform", minimum = 70, maximum = 90)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  mc_building_data[[4]] <- JEMParamDist$new("Abf", name = "name", units = "-", dist_type = "Uniform", minimum = 150, maximum = 210)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  settings_dfx$building_setting <- "Commercial"

  warningMessage <- "Warning: One or more parameter values input for the enclosed space floor area (Abf) is outside of the expected range (80 - 1500 square meters)"

  mc_building_data[[4]] <- JEMParamDist$new("Abf", name = "name", units = "-", dist_type = "Uniform", minimum = 70, maximum = 90)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  mc_building_data[[4]] <- JEMParamDist$new("Abf", name = "name", units = "-", dist_type = "Uniform", minimum = 1400, maximum = 1600)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

})

test_that("det sim: correct warning message thrown when the indoor air exchange rate isn't within a reasonable range", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  settings_dfx$building_setting <- "Residential"

  warningMessage <- "Warning: The value input for the indoor air exchange rate (ach) is outside of the expected range (0.15 - 1.26 air changes per hour)"

  building_dfx$ach <- 0.10
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  building_dfx$ach <- 1.5
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  settings_dfx$building_setting <- "Commercial"

  warningMessage <- "Warning: The value input for the indoor air exchange rate (ach) is outside of the expected range (0.3 - 4.1 air changes per hour)"

  building_dfx$ach <- 0.2
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  building_dfx$ach <- 5
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

})

test_that("mc sim: correct warning message thrown when building floor area isn't within a reasonable range", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  settings_dfx$building_setting <- "Residential"

  warningMessage <- "Warning: One or more parameter values input for the indoor air exchange rate (ach) is outside of the expected range (0.15 - 1.26 air changes per hour)"

  mc_building_data[[6]] <- JEMParamDist$new("ach", name = "name", units = "-", dist_type = "Uniform", minimum = 0.10, maximum = 0.20)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  mc_building_data[[6]] <- JEMParamDist$new("ach", name = "name", units = "-", dist_type = "Uniform", minimum = 1, maximum = 2)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  settings_dfx$building_setting <- "Commercial"

  warningMessage <- "Warning: One or more parameter values input for the indoor air exchange rate (ach) is outside of the expected range (0.3 - 4.1 air changes per hour)"

  mc_building_data[[6]] <- JEMParamDist$new("ach", name = "name", units = "-", dist_type = "Uniform", minimum = 0.2, maximum = 0.4)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  mc_building_data[[6]] <- JEMParamDist$new("ach", name = "name", units = "-", dist_type = "Uniform", minimum = 3, maximum = 5)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

})

test_that("det sim: correct warning message thrown when the Qsoil/Qbuilding ratio isn't within a reasonable range", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  warningMessage <- "Warning: The value input for the Qsoil/Qbuilding ratio (Qsoil_Qb) is outside of the expected range (0.0001 - 0.05)"

  building_dfx$Qsoil_Qb <- 0.00001
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  building_dfx$Qsoil_Qb <- 0.1
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

})

test_that("mc sim: correct warning message thrown when the Qsoil/Qbuilding ratio isn't within a reasonable range", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  warningMessage <- "Warning: One or more parameter values input for the Qsoil/Qbuilding ratio (Qsoil_Qb) is outside of the expected range (0.0001 - 0.05)"

  mc_building_data[[7]] <- JEMParamDist$new("Qsoil_Qb", name = "name", units = "-", dist_type = "Uniform", minimum = 0.00001, maximum = 0.003)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  mc_building_data[[7]] <- JEMParamDist$new("Qsoil_Qb", name = "name", units = "-", dist_type = "Uniform", minimum = 0.006, maximum = 0.1)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

})

test_that("det sim: correct warning message thrown when the enclosed space mixing height isn't within a reasonable range", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  settings_dfx$foundation_type <- "Slab-grade"

  warningMessage <- "Warning: The value input for the enclosed space mixing height (Hb) is outside of the expected range (2.13 - 3.05 meters)"

  building_dfx$Hb <- 1
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  building_dfx$Hb <- 4
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  settings_dfx$foundation_type <- "Basement-slab"

  warningMessage <- "Warning: The value input for the enclosed space mixing height (Hb) is outside of the expected range (2.44 - 4.88 meters)"

  building_dfx$Hb <- 1
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  building_dfx$Hb <- 5
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  settings_dfx$foundation_type <- "Crawlspace-slab"

  warningMessage <- "Warning: The value input for the enclosed space mixing height (Hb) is outside of the expected range (0.5 - 1.3 meters)"

  building_dfx$Hb <- 0.1
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  building_dfx$Hb <- 2
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

})

test_that("mc sim: correct warning message thrown when the enclosed space mixing height isn't within a reasonable range", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  settings_dfx$foundation_type <- "Slab-grade"

  mc_building_data[[3]] <- JEMParamDist$new("eta", name = "name", units = "-", dist_type = "Constant", constant = 0.001)

  warningMessage <- "Warning: One or more parameter values input for the enclosed space mixing height (Hb) is outside of the expected range (2.13 - 3.05 meters)"

  mc_building_data[[5]] <- JEMParamDist$new("Hb", name = "name", units = "m", dist_type = "Uniform", minimum = 1, maximum = 3)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  mc_building_data[[5]] <- JEMParamDist$new("Hb", name = "name", units = "m", dist_type = "Uniform", minimum = 3, maximum = 4)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  settings_dfx$foundation_type <- "Basement-slab"

  warningMessage <- "Warning: One or more parameter values input for the enclosed space mixing height (Hb) is outside of the expected range (2.44 - 4.88 meters)"

  mc_building_data[[5]] <- JEMParamDist$new("Hb", name = "name", units = "m", dist_type = "Uniform", minimum = 1, maximum = 3)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  mc_building_data[[5]] <- JEMParamDist$new("Hb", name = "name", units = "m", dist_type = "Uniform", minimum = 3, maximum = 5)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  settings_dfx$foundation_type <- "Crawlspace-slab"

  warningMessage <- "Warning: One or more parameter values input for the enclosed space mixing height (Hb) is outside of the expected range (0.5 - 1.3 meters)"

  mc_building_data[[5]] <- JEMParamDist$new("Hb", name = "name", units = "m", dist_type = "Uniform", minimum = 0.1, maximum = 1)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  mc_building_data[[5]] <- JEMParamDist$new("Hb", name = "name", units = "m", dist_type = "Uniform", minimum = 1, maximum = 2)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

})

test_that("det sim: correct warning message thrown when the depth to building foundation isn't within a reasonable range", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  warningMessage <- "Warning: The value input for the depth below grade to base of foundation (Lb) is outside of the expected range (0.1 - 2.44 meters)"

  building_dfx$Lb <- 0.01
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  building_dfx$Lb <- 2.5
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

})

test_that("mc sim: correct warning message thrown when the Qsoil/Qbuilding ratio isn't within a reasonable range", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  warningMessage <- "Warning: One or more parameter values input for the depth below grade to base of foundation (Lb) is outside of the expected range (0.1 - 2.44 meters)"

  mc_building_data[[1]] <- JEMParamDist$new("Lb", name = "name", units = "-", dist_type = "Uniform", minimum = 0.01, maximum = 0.5)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  mc_building_data[[1]] <- JEMParamDist$new("Lb", name = "name", units = "-", dist_type = "Uniform", minimum = 2, maximum = 3)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

})

# #################################### vadose_zone_data #########################################

test_that("det sim: correct error message is thrown when vadose_zone_data is the wrong class", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  source_dfx <- list(source_dfx)

  errorMessage <- "Error: In a deterministic simulation, the \"vadose_zone_data\" input to the runJE function must be a data frame for the function to proceed"

  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("mc sim: correct error message is thrown when vadose_zone_data is the wrong class", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  mc_vadose_zone_data <- data.frame()

  errorMessage <- "Error: In a stochastic simulation, the \"vadose_zone_data\" input to the runJE function must be a list for the function to proceed"

  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("mc sim: correct error message is thrown when a vadose_zone_data list item is not a JEMParamDist object", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  mc_vadose_zone_data[[1]] <- data.frame()

  errorMessage <- "Error: In a stochastic simulation, all items in the \"vadose_zone_data\" input list must be R6 JEMParamDist objects"

  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("det sim: correct error message is thrown when a vadose_zone_data parameter is missing", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: One or more parameters required to execute the runJE function are missing from the \"vadose_zone_data\" data frame"

  source_dfx <- source_dfx %>% select(-Ls)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  source_dfx <- det_test_data[[3]]
  source_dfx <- source_dfx %>% select(-Ts)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

})

test_that("det sim: correct error message is thrown when a vadose_zone_data parameter is NA", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The \"vadose_zone_data\" data input to the runJE function cannot include NA values in the Ls or Ts fields"

  source_dfx$Ls <- NA
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  source_dfx <- det_test_data[[3]]
  source_dfx$Ts <- NA
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

})

test_that("det sim: correct error message is thrown when a vadose_zone_data parameter is not a number", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The value in the Ls or Ts field in the \"vadoze_zone_data\" data frame is not a number"

  source_dfx$Ls <- "text"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  source_dfx <- det_test_data[[3]]
  source_dfx$Ts <- "text"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

})

test_that("det sim: correct error message is thrown when the depth to source in vadoze_zone_data is less than zero", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The value of Ls in the \"vadoze_zone_data\" data frame must be greater than zero"

  source_dfx$Ls <- -1
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("mc sim: correct error message is thrown when vadose_zone_data is the wrong class", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  mc_vadose_zone_data <- data.frame()

  errorMessage <- "Error: In a stochastic simulation, the \"vadose_zone_data\" input to the runJE function must be a list for the function to proceed"

  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("mc sim: correct error messages are thrown when parameters are missing from vadose_zone_data", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  dummyDist <- JEMParamDist$new("dummySymbol", name = "dummyDist", units = "dummyUnits", dist_type = "Constant", constant = 1)

  errorMessage <- "Error: One or more parameters required to execute the runJE function are missing from the \"vadose_zone_data\" list"

  #Ls
  mc_vadose_zone_data[[1]] <- dummyDist
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  #Ts
  mc_vadose_zone_data <- mc_test_data[[3]]
  mc_vadose_zone_data[[2]] <- dummyDist
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

})

test_that("mc sim: correct error message thrown when a parameter distribution is represented more than once in vadose_zone_data", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  mc_vadose_zone_data <- append(mc_vadose_zone_data, mc_vadose_zone_data[[1]])

  errorMessage <- "Error: One or more parameters has multiple JEMParamDist objects included in the \"vadose_zone_data\" list"

  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

})

test_that("mc sim: correct error messages are thrown when depth to source is negative in vadose_zone_data", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  #Ls
  mc_vadose_zone_data[[1]] <- JEMParamDist$new("Ls", name = "name", units = "units", dist_type = "Constant", constant = -1)
  errorMessage <- "Error: the assigned constant value in the 'Ls' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero."
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

})

test_that("det sim: correct warning message thrown when the source temperature isn't within a reasonable range", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  warningMessage <- "Warning: The value input for the source temperature (Ts) is outside of the expected range (3 - 25 degrees Celsius)"

  source_dfx$Ts <- 2
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  source_dfx$TS <- 27
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  settings_dfx$source_medium <- "Subslab Soil Gas"
  contam_dfx$Units <- "ppb"

  warningMessage <- "Warning: The value input for the source temperature (Ts) is outside of the expected range (3 - 30 degrees Celsius)"

  source_dfx$Ts <- 2
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  source_dfx$TS <- 35
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

})

test_that("mc sim: correct warning message thrown when the source temperature isn't within a reasonable range", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  warningMessage <- "Warning: One or more parameter values input for the source temperature (Ts) is outside of the expected range (3 - 25 degrees Celsius)"

  mc_vadose_zone_data[[2]] <- JEMParamDist$new("Ts", name = "name", units = "deg C", dist_type = "Uniform", minimum = 2, maximum = 10)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  mc_vadose_zone_data[[2]] <- JEMParamDist$new("Ts", name = "name", units = "deg C", dist_type = "Uniform", minimum = 10, maximum = 27)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  warningMessage <- "Warning: One or more parameter values input for the source temperature (Ts) is outside of the expected range (3 - 30 degrees Celsius)"

  settings_dfx$source_medium <- "Subslab Soil Gas"
  mc_contaminant_data <- JEMParamDist$new("Cmedium", name = "Tetrachloroethylene", units = "ppb", dist_type = "PERT", minimum = 8, mode = 10, maximum = 12)

  mc_vadose_zone_data[[2]] <- JEMParamDist$new("Ts", name = "name", units = "deg C", dist_type = "Uniform", minimum = 2, maximum = 10)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  mc_vadose_zone_data[[2]] <- JEMParamDist$new("Ts", name = "name", units = "deg C", dist_type = "Uniform", minimum = 10, maximum = 35)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)
})

test_that("det sim: correct error message thrown when the depth to source is less than the depth to building foundation", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The depth below grade to source (Ls = 1.5 m) is less than the depth below grade to base of foundation (Lb = 2 m)"

  source_dfx$Ls <- 1.5
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

})

test_that("mc sim: correct error message thrown when the minimum depth to source is less than the maximum depth to base of foundation", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  mc_building_data[[1]] <- JEMParamDist$new("Lb", name = "name", units = "m", dist_type = "PERT", minimum = 2, mode = 2.3, maximum = 2.5)

  errorMessage <- "Error: The minimum depth below grade to source (Ls_min = 2 m) is less than the maximum depth below grade to base of foundation (Lb_max = 2.5 m)"

  mc_vadose_zone_data[[1]] <- JEMParamDist$new("Ls", name = "name", units = "deg C", dist_type = "Uniform", minimum = 2, maximum = 3)
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("det sim: correct warning message thrown when the depth to subslab soil gas source isn't within a reasonable range", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  warningMessage <- "Warning: For subslab soil gas sources, the depth below grade to source (Ls) should be within one meter of the depth to building foundation (Lb)."
  warningMessage <- paste(warningMessage, " The value input for Ls is outside of the expected range (2 - 3 meters) based on the input value for Lb.", sep = "")

  settings_dfx$source_medium <- "Subslab Soil Gas"
  contam_dfx$Units <- "ppb"

  source_dfx$Ls <- 4
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

})

test_that("mc sim: correct warning message thrown when the depth to subslab soil gas source isn't within a reasonable range", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  settings_dfx$source_medium <- "Subslab Soil Gas"
  mc_contaminant_data <- JEMParamDist$new("Cmedium", name = "Tetrachloroethylene", units = "ppb", dist_type = "PERT", minimum = 8, mode = 10, maximum = 12)
  mc_building_data[[1]] <- JEMParamDist$new("Lb", name = "name", units = "m", dist_type = "PERT", minimum = 2, mode = 2.3, maximum = 2.5)

  warningMessage <- "Warning: For subslab soil gas sources, the depth below grade to source (Ls) should be within one meter of the depth to building foundation (Lb)."
  warningMessage <- paste(warningMessage, " One or more values input for the Ls distribution are outside of the expected range (2 - 3.5 meters) based on the input values for Lb.", sep = "")

  mc_vadose_zone_data[[1]] <- JEMParamDist$new("Ls", name = "name", units = "deg C", dist_type = "Uniform", minimum = 3, maximum = 4)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)
})

test_that("det sim: correct warning message thrown when the depth to groundwater or exterior soil gas source isn't within a reasonable range", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  warningMessage <- "Warning: For exterior soil gas and groundwater sources, the depth below grade to source (Ls) should be at least one meter deeper than the depth to building foundation (Lb)."
  warningMessage <- paste(warningMessage, " The value input for Ls is shallower than the expected minimum depth (3 meters) based on the input value for Lb.", sep = "")

  source_dfx$Ls <- 2.5
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  settings_dfx$source_medium <- "Exterior Soil Gas"
  contam_dfx$Units <- "ppb"

  source_dfx$Ls <- 2.5
  expect_equal(suppressWarnings(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

})

test_that("mc sim: correct warning message thrown when the depth to groundwater or exterior soil gas source isn't within a reasonable range", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  mc_building_data[[1]] <- JEMParamDist$new("Lb", name = "name", units = "m", dist_type = "PERT", minimum = 1, mode = 1.5, maximum = 2)

  warningMessage <- "Warning: For exterior soil gas and groundwater sources, the depth below grade to source (Ls) should be at least one meter deeper than the depth to building foundation (Lb)."
  warningMessage <- paste(warningMessage, " One or more values input for the Ls distribution are shallower than the expected minimum depth (3 meters) based on the input values for Lb.", sep = "")

  mc_vadose_zone_data[[1]] <- JEMParamDist$new("Ls", name = "name", units = "deg C", dist_type = "Uniform", minimum = 2.5, maximum = 4)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)

  settings_dfx$source_medium <- "Exterior Soil Gas"
  mc_contaminant_data <- JEMParamDist$new("Cmedium", name = "Tetrachloroethylene", units = "ppb", dist_type = "PERT", minimum = 8, mode = 10, maximum = 12)

  mc_vadose_zone_data[[1]] <- JEMParamDist$new("Ls", name = "name", units = "deg C", dist_type = "Uniform", minimum = 2.5, maximum = 4)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx))[1], warningMessage, fixed = TRUE)
})

#################################### strata_logs_data #########################################

test_that("correct error message is thrown when strata_logs_data is the wrong class", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  boring_log_dfx <- list(boring_log_dfx)

  errorMessage <- "Error: The \"strata_logs_data\" input to the runJE function must be a data frame for the function to proceed"

  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct error messages are thrown when parameters are missing from strata_logs_data", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: One or more parameters required to execute the runJE function are missing from the \"strata_logs_data\" data frame"

  boring_log_dfx <- boring_log_dfx %>% select(-LogID)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)
  boring_log_dfx <- boring_log_dfx %>% select(-SoilType)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)
  boring_log_dfx <- boring_log_dfx %>% select(-LayerOrder)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)
  boring_log_dfx <- boring_log_dfx %>% select(-Thickness)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct error messages are thrown when a required parameter is NA in strata_logs_data", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The \"strata_logs_data\" data frame input to the runJE function cannot include NA values in the LogID, SoilType, LayerOrder, or Thickness fields, except where the SoilType is listed as \"Not Present\""

  boring_log_dfx$LogID[1] <- NA
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)
  boring_log_dfx$SoilType[1] <- NA
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)
  boring_log_dfx$LayerOrder[1] <- NA
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)
  boring_log_dfx$Thickness[1] <- NA
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct error messages are thrown when an unsupported soil type is assigned in strata_logs_data", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: Unsupported soil type assigned in the \"strata_logs_data\" data frame input to the runJE function"

  boring_log_dfx$SoilType[1] <- "text"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct error messages are thrown when layer order or thickness parameters in strata_logs_data are not numeric", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: A value in the LayerOrder or Thickness fields in the \"strata_logs_data\" data frame is not a number"

  boring_log_dfx$LayerOrder[1] <- "text"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)
  boring_log_dfx$Thickness[1] <- "text"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct error messages are thrown when layer order or thickness parameters in strata_logs_data are not positive", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: A value in the LayerOrder or Thickness fields in the \"strata_logs_data\" is less than or equal to zero"

  boring_log_dfx$LayerOrder[1] <- 0
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)
  boring_log_dfx$Thickness[1] <- 0
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct error message thrown when more than one LogID passed in strata_logs_data", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The strata_logs_data LogID field includes more than one unique value. The runJE function can evaluate only one LogID at a time"

  boring_log_dfx$LogID[1] <- 2
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct error message thrown when LayerOrder entries don't have sequential numbers from 1 to n(layers) in strata_logs_data", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: Values in the \"strata_logs_data\" LayerOrder field must be sequential integers starting at 1 for each unique LogID. One or more sequential integers is missing from the boring log with LogID = 1"

  boring_log_dfx$LayerOrder[1] <- 2
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("det sim: correct error message thrown when the total boring log depth is less than the depth to source (Ls)", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The input depth to source (Ls = 9.54 m) is deeper than the total depth of the boring log with LogID = 1 (total depth = 6 m)"

  boring_log_dfx$Thickness <- 3
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("mc sim: correct error message thrown when the total boring log depth is less than the maximum depth to source", {
  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The maximum depth to source (Ls_max = 13 m) is deeper than the total depth of the boring log with LogID = 1 (total depth = 9 m)"

  boring_log_dfx$Thickness <- 3
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

})

test_that("correct warning thrown when Lb > hcz + Ls in deterministic groundwater simulation", {

   det_test_data <- get_default_deterministic_test_data()

   contam_dfx <- det_test_data[[1]]
   building_dfx <- det_test_data[[2]]
   source_dfx <- det_test_data[[3]]
   stratum_dfx <- det_test_data[[4]]
   settings_dfx <- det_test_data[[5]]

   boring_log_dfx <- filter(stratum_dfx, LogID == 1)

   boring_log_dfx$SoilType[[1]] <- "Silty Clay"
   building_dfx$Lb <- 1
   source_dfx$Ls <- 2.25

   errorMessage <- "Error: Based on the soil properties for the boring log with LogID = 1, the input depth to water (Ls = 2.25 m) would result in capillary rise (hcz = 1.92307692307692 m) above the input depth to building foundation (Lb = 1 m), which violates the model assumptions."

   expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct warning thrown when Lb > hcz + Ls in stochastic groundwater simulation when Ls is constant",{

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]

  mc_building_data[[1]] <- JEMParamDist$new("Lb",
                                            name = "Depth below grade to base of foundation",
                                            units = "m",
                                            dist_type = "Constant",
                                            constant = 1)

  mc_vadose_zone_data[[1]] <- JEMParamDist$new("Ls",
                                                name = "Depth below grade to source",
                                                units = "m",
                                                dist_type = "Constant",
                                                constant = 2.25)

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  boring_log_dfx$SoilType[[1]] <- "Silty Clay"

  errorMessage <- "Error: Based on the soil properties for the boring log with LogID = 1, the minimum depth to water (Ls_min = 2.25 m) may result in capillary rise (hcz = 1.92307692307692 m) above the maximum depth to building foundation (Lb_max = 1 m), which violates the model assumptions."

  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)


})

test_that("correct warning thrown when Lb > hcz + Ls in stochastic groundwater simulation when Ls is variable",{

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]

  mc_building_data[[1]] <- JEMParamDist$new("Lb",
                                            name = "Depth below grade to base of foundation",
                                            units = "m",
                                            dist_type = "Constant",
                                            constant = 1)

  mc_vadose_zone_data[[1]] <- JEMParamDist$new("Ls",
                                               name = "Depth below grade to source",
                                               units = "m",
                                               dist_type = "Uniform",
                                               minimum = 2.25,
                                               maximum= 10.25)

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  #Top layer triggers the warning
  boring_log_dfx$SoilType[[1]] <- "Silty Clay"

  errorMessage <- "Error: Based on the soil properties for the boring log with LogID = 1, the minimum depth to water (Ls_min = 2.25 m) may result in capillary rise (hcz = 1.92307692307692 m) above the maximum depth to building foundation (Lb_max = 1 m), which violates the model assumptions."

  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  #No warning - capillary rise in second layer based on top layer, not enough to bring it up to the foundation depth
  boring_log_dfx$SoilType[[1]] <- "Loamy Sand"
  boring_log_dfx$SoilType[[2]] <- "Silty Clay"

  warningMessage <- "No errors or warnings in model inputs"

  expect_equal(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), warningMessage)
})

#################################### contaminant_data #########################################

test_that("det sim: correct error message thrown when contaminant_data is the wrong class", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- list(contam_dfx)

  errorMessage <- "Error: In a deterministic simulation, the \"contaminant_data\" input to the runJE function must be a data frame for the function to proceed"

  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("mc sim: correct error message is thrown when contaminant_data is the wrong class", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  mc_contaminant_data <- data.frame()

  errorMessage <- "Error: In a stochastic simulation, the \"contaminant_data\" input to the runJE function must be an R6 JEMParamDist object for the function to proceed"

  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("det sim: correct error message thrown when contaminant_data has more than one row", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- rbind(contam_dfx, contam_dfx)

  errorMessage <- "Error: The \"contaminant_data\" data frame has more than one row. Only one contaminant can be evaluated at a time using runJE"

  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("det sim: correct error message is thrown when a contaminant_data parameter is missing", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: One or more parameters required to execute the runJE function are missing from the \"contaminant_data\" data frame"

  contam_dfx <- contam_dfx %>% select(-Contaminant)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  contam_dfx <- det_test_data[[1]]
  contam_dfx <- contam_dfx %>% select(-Cmedium)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  contam_dfx <- det_test_data[[1]]
  contam_dfx <- contam_dfx %>% select(-Units)
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

})

test_that("det sim: correct error message is thrown when a contaminant_data parameter is NA", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The \"contaminant_data\" data input to the runJE function cannot include NA values in the Contaminant, Cmedium, or Units fields"

  contam_dfx$Contaminant <- NA
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  contam_dfx <- det_test_data[[1]]
  contam_dfx$Cmedium <- NA
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  contam_dfx <- det_test_data[[1]]
  contam_dfx$Units <- NA
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

})

test_that("det sim: correct error message is thrown when the contaminant concentration is not a number", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The value in the Cmedium field in the \"contaminant_data\" data frame is not a number"

  contam_dfx$Cmedium <- "text"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

})

test_that("det sim: correct error message is thrown when the contaminant concentration is less than or equal to zero", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The input concentration is less than or equal to zero"

  contam_dfx$Cmedium <- 0
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

})

test_that("mc sim: correct error messages are thrown when parameters are missing from mc_contaminant_data", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  mc_contaminant_data <- JEMParamDist$new("dummySymbol", name = "dummyDist", units = "dummyUnits", dist_type = "Constant", constant = 1)

  errorMessage <- "Error: The \"contaminant_data\" JEMParamDist object must have an assigned symbol of \"Cmedium\" for the simulation to proceed"
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("mc sim: correct error messages are thrown when a concetration parameter is negative or zero", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  mc_contaminant_data <- JEMParamDist$new("Cmedium", name = "Benzene", units = "dummyUnits", dist_type = "PERT", minimum = -1, mode = 5, maximum = 10)

  errorMessage <- "Error: One or more parameter values in the input contaminant concentration distribution was less than zero. All concentration parameters must be greater than or equal to zero."
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("det sim: correct error message is thrown when unsupported soil gas units are provided", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The units assigned to the input soil gas concentration are not supported in the runJE function. Soil gas concentrations must be entered in units of \"ppb\" or \"ug/m3\")"

  contam_dfx$Units <- "ug/L"
  settings_dfx$source_medium = "Subslab Soil Gas"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  contam_dfx$Units <- "ppm"
  settings_dfx$source_medium = "Exterior Soil Gas"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  errorMessage <- "Error: The units assigned to the input groundwater concentration are not supported in the runJE function. Groundwater concentrations must be entered in units of \"ppb\" or \"ug/L\")"

  contam_dfx$Units <- "ug/m3"
  settings_dfx$source_medium = "Groundwater"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("mc sim: correct error messages are thrown when unsupported soil gas units are provided", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The units assigned to the input soil gas concentration are not supported in the runJE function. Soil gas concentrations must be entered in units of \"ppb\" or \"ug/m3\")"

  mc_contaminant_data <- JEMParamDist$new("Cmedium", name = "Benzene", units = "ppm", dist_type = "PERT", minimum = 1, mode = 5, maximum = 10)
  settings_dfx$source_medium = "Subslab Soil Gas"
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  mc_contaminant_data <- JEMParamDist$new("Cmedium", name = "Benzene", units = "ug/L", dist_type = "PERT", minimum = 1, mode = 5, maximum = 10)
  settings_dfx$source_medium = "Exterior Soil Gas"
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)

  errorMessage <- "Error: The units assigned to the input groundwater concentration are not supported in the runJE function. Groundwater concentrations must be entered in units of \"ppb\" or \"ug/L\")"

  mc_contaminant_data <- JEMParamDist$new("Cmedium", name = "Benzene", units = "ug/m3", dist_type = "PERT", minimum = 1, mode = 5, maximum = 10)
  settings_dfx$source_medium = "Groundwater"
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("det sim: correct error message when contaminant name is not in the list of supported chemicals", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The contaminant name 'DummyContaminant' was not found in the list of supported chemicals. Supported chemical names may be found in the `Chemical` field of the `vapintr::chem_data` data frame."

  contam_dfx$Contaminant <- "DummyContaminant"
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("mc sim: correct error message is thrown when the contaminant is not in the list of supported chemicals", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The contaminant name 'DummyContaminant' was not found in the list of supported chemicals. Supported chemical names may be found in the `Chemical` field of the `vapintr::chem_data` data frame."

  mc_contaminant_data <- JEMParamDist$new("Cmedium", name = "DummyContaminant", units = "ppb", dist_type = "PERT", minimum = 1, mode = 5, maximum = 10)
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("det sim: correct error message is thrown when the contaminant concentration is above the chemical's solubility limit", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: The value input for the contaminant concentration exceeds the contaminant's aqueous solubility limit of 206000 ppb."

  contam_dfx$Cmedium <- 300000
  expect_error(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("mc sim: correct error message is thrown when the contaminant concentration is above the chemical's solubility limit", {

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  errorMessage <- "Error: One or more parameter values input for the contaminant concentration exceeds the contaminant's aqueous solubility limit."

  mc_contaminant_data <- JEMParamDist$new("Cmedium", name = "Tetrachloroethylene", units = "ppb", dist_type = "PERT", minimum = 200000, mode = 250000, maximum = 350000)
  expect_error(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx), errorMessage, fixed = TRUE)
})

test_that("correct warning message returned for hydrocarbon chemicals",{

  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)


  warningMessage <- "Warning: Predicted indoor air concentration may be overestimated; biodegredation was not considered for this contaminant."

  mc_contaminant_data <- JEMParamDist$new("Cmedium", name = "Benzene", units = "ppb", dist_type = "PERT", minimum = 200, mode = 250, maximum = 350)
  expect_equal(suppressWarnings(checkModelInputs(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, boring_log_dfx, settings_dfx)), warningMessage)
})

#################################### Tests for thrown warning messages #####################################

test_that("det sim: correct warning message thrown for one warning", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  warningMessage <- "Warning: The value input for the depth below grade to base of foundation \\(Lb\\) is outside of the expected range \\(0.1 - 2.44 meters\\) \n\n$"

  building_dfx$Lb <- 0.01
  expect_warning(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), warningMessage)
})

test_that("det sim: correct warning message thrown for multiple warnings", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]
  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  message1 <- "Warning: The value input for the Qsoil/Qbuilding ratio \\(Qsoil_Qb\\) is outside of the expected range \\(0.0001 - 0.05\\) \n\n"
  message2 <- "Warning: The value input for the enclosed space mixing height \\(Hb\\) is outside of the expected range \\(2.44 - 4.88 meters\\) \n\n"
  message3 <- "Warning: The value input for the depth below grade to base of foundation \\(Lb\\) is outside of the expected range \\(0.1 - 2.44 meters\\) \n\n"

  warningMessage <- paste("^", message1, message2, message3, "$", sep = "")

  building_dfx$Lb <- 0.01
  building_dfx$Qsoil_Qb <- 0.1
  building_dfx$Hb <- 5

  expect_warning(checkModelInputs(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx), warningMessage)
})
