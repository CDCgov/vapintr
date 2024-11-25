test_that("correct error thrown when contaminant name is not in loaded data file", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  settings_dfx <- test_template_data[[5]]
  simulation_type <- settings_dfx[settings_dfx$Parameter == "Simulation type", ]$Value
  source_medium <- settings_dfx[settings_dfx$Parameter == "Source medium", ]$Value

  contaminant <- "Trichloroethylene"

  errorMessage <- paste0("Error: The imported contaminant data set does not contain records for ", contaminant)
  expect_error(getInputConc(contam_dfx, contaminant, source_medium, simulation_type), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when records passed with more than one unit with a contaminant specified", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  settings_dfx <- test_template_data[[5]]
  simulation_type <- settings_dfx[settings_dfx$Parameter == "Simulation type", ]$Value
  source_medium <- settings_dfx[settings_dfx$Parameter == "Source medium", ]$Value

  contaminant <- "Tetrachloroethylene"
  contam_dfx$Units[nrow(contam_dfx)] <- "ug/m3"

  errorMessage <- paste0("Error: The ", tolower(contaminant), " ", tolower(source_medium), " records passed to the getInputConc function have more than one unit.")
  expect_error(getInputConc(contam_dfx, contaminant, source_medium, simulation_type), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when records passed with more than one unit without a contaminant specified", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  settings_dfx <- test_template_data[[5]]
  simulation_type <- settings_dfx[settings_dfx$Parameter == "Simulation type", ]$Value
  source_medium <- settings_dfx[settings_dfx$Parameter == "Source medium", ]$Value

  contaminant <- NA
  contam_dfx$Units[nrow(contam_dfx)] <- "ug/m3"

  errorMessage <- paste0("Error: The ", tolower(source_medium), " records passed to the getInputConc function have more than one unit.")
  expect_error(getInputConc(contam_dfx, contaminant, source_medium, simulation_type), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when no detections passed with a contaminant specified", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  settings_dfx <- test_template_data[[5]]
  simulation_type <- settings_dfx[settings_dfx$Parameter == "Simulation type", ]$Value
  source_medium <- settings_dfx[settings_dfx$Parameter == "Source medium", ]$Value

  contaminant <- "Tetrachloroethylene"
  contam_dfx$DetectedFlag <- "No"

  errorMessage <- paste0("Error: The imported contaminant data set does not include any detections for ", tolower(contaminant))
  expect_error(getInputConc(contam_dfx, contaminant, source_medium, simulation_type), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when no detections passed without a contaminant specified", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  settings_dfx <- test_template_data[[5]]
  simulation_type <- settings_dfx[settings_dfx$Parameter == "Simulation type", ]$Value
  source_medium <- settings_dfx[settings_dfx$Parameter == "Source medium", ]$Value

  contaminant <- NA
  contam_dfx$DetectedFlag <- "No"

  errorMessage <- paste0("Error: The imported contaminant data set does not include any detections")
  expect_error(getInputConc(contam_dfx, contaminant, source_medium, simulation_type), errorMessage, fixed = TRUE)
})
