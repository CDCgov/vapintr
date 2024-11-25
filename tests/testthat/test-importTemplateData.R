test_that("correct error thrown when importTemplateData can't find the input file path", {

  template_file_path <- "C:/DummyPath"

  errorMessage <- "Error: Could not find file at C:/DummyPath"

  expect_error(importTemplateData(template_file_path), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when settings data sheet is missing a needed column", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: One or more required columns are missing from the Settings sheet of the import file"

  settings_dfx <- settings_dfx %>% select(-Parameter)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  settings_dfx <- test_template_data[[5]]
  settings_dfx <- settings_dfx %>% select(-Value)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

})

test_that("correct error thrown when settings data sheet is missing a needed parameter", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: One or more required parameters are missing from the Settings sheet of the import file"

  settings_dfx <- settings_dfx %>% filter(Parameter!="Simulation type")
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  settings_dfx <- test_template_data[[5]]

  settings_dfx <- settings_dfx %>% filter(Parameter!="Source medium")
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  settings_dfx <- test_template_data[[5]]

  settings_dfx <- settings_dfx %>% filter(Parameter!="Apply default building parameters")
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  settings_dfx <- test_template_data[[5]]

  settings_dfx <- settings_dfx %>% filter(Parameter!="Building setting")
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  settings_dfx <- test_template_data[[5]]

  settings_dfx <- settings_dfx %>% filter(Parameter!="Foundation type")
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  settings_dfx <- test_template_data[[5]]

  settings_dfx <- settings_dfx %>% filter(Parameter!="Simulate capillary zone")
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  settings_dfx <- test_template_data[[5]]

  settings_dfx <- settings_dfx %>% filter(Parameter!="Number of Monte Carlo iterations")
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

})

test_that("correct error thrown when the settings_data$simulation_type parameter isn't a known value", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulation type"] <- "NotAcceptedValue"

  errorMessage <- "Error: The value of the \"Simulation type\" parameter in the import data template is not an accepted value. Use one of the following: \"Deterministic\" or \"Stochastic\""

  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

})

test_that("correct error thrown when the settings_data$source_medium parameter isn't a known value ", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] <- "NotAcceptedValue"

  errorMessage <- "Error: The value of the \"Source medium\" parameter in the import data template is not an accepted value. Use one of the following: \"Groundwater\", \"Exterior Soil Gas\" or \"Subslab Soil Gas\""

  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

})

test_that("correct error thrown when the settings_data$apply_default_building_parameters parameter isn't a known value ", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "NotAcceptedValue"

  errorMessage <- "Error: The value of the \"Apply default building parameters\" parameter in the import data template is not an accepted value. Use one of the following: \"Yes\" or \"No\""

  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when the settings_data$building_setting parameter isn't a known value ", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] <- "NotAcceptedValue"

  errorMessage <- "Error: The value of the \"Building setting\" parameter in the import data template is not an accepted value. Use one of the following: \"Residential\" or \"Commercial\""

  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

})

test_that("correct error thrown when the settings_data$foundation_type parameter isn't a known value ", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] <- "NotAcceptedValue"

  errorMessage <- "Error: The value of the \"Foundation type\" parameter in the import data template is not an accepted value. Use one of the following: \"Basement w/ Slab\", \"Basement w/ Dirt Floor\", \"Slab-on-grade\", \"Closed Crawl Space w/ Slab\", \"Closed Crawl Space w/ Dirt Floor\""

  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})


test_that("correct error thrown when the settings_data$simulate_capillary_zone parameter isn't a known value ", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "NotAcceptedValue"

  errorMessage <- "Error: The value of the \"Simulate capillary zone\" parameter in the import data template is not an accepted value. Use one of the following: \"Yes\" or \"No\""

  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when the settings_data$number_of_monte_carlo_iterations parameter isn't an integer", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: The value of the \"Number of Monte Carlo iterations\" parameter in the import data template is not an accepted value. For stochastic simulations, the number of Monte Carlo iterations must be an integer greater than zero"

  settings_dfx$Value[settings_dfx$Parameter == "Number of Monte Carlo iterations"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  settings_dfx$Value[settings_dfx$Parameter == "Number of Monte Carlo iterations"] <- 100.5
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("Settings data transposed correctly into a data frame with columns named after the parameter values", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))

  settings_processed_data <- processed_data[[5]]

  expect_true(all(names(settings_processed_data) %in% c("simulation_type","source_medium","apply_default_building_parameters","building_setting","foundation_type","simulate_capillary_zone","number_of_monte_carlo_iterations")))
})

test_that("Simulation types are mapped to the correct codes in the imported settings data", {

  #Stochastic test
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))

  settings_processed_data <- processed_data[[5]]
  expect_equal(settings_processed_data$simulation_type, "MC")

  #Deterministic test
  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))

  settings_processed_data <- processed_data[[5]]
  expect_equal(settings_processed_data$simulation_type, "DET")
})

test_that("Foundation types are mapped to the correct codes in the imported settings data", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] <- "Closed Crawl Space w/ Slab"
  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  settings_processed_data <- processed_data[[5]]
  expect_equal(settings_processed_data$foundation_type, "Crawlspace-slab")

  settings_dfx <- test_template_data[[5]]
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] <- "Basement w/ Slab"
  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  settings_processed_data <- processed_data[[5]]
  expect_equal(settings_processed_data$foundation_type, "Basement-slab")

  settings_dfx <- test_template_data[[5]]
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] <- "Basement w/ Dirt Floor"
  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  settings_processed_data <- processed_data[[5]]
  expect_equal(settings_processed_data$foundation_type, "Basement-dirt")

  settings_dfx <- test_template_data[[5]]
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] <- "Closed Crawl Space w/ Dirt Floor"
  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  settings_processed_data <- processed_data[[5]]
  expect_equal(settings_processed_data$foundation_type, "Crawlspace-dirt")

  settings_dfx <- test_template_data[[5]]
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] <- "Slab-on-grade"
  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  settings_processed_data <- processed_data[[5]]
  expect_equal(settings_processed_data$foundation_type, "Slab-grade")

})

test_that("Values for the Apply Default Building Parameters setting are mapped to the correct codes in the imported settings data", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  settings_processed_data <- processed_data[[5]]
  expect_equal(settings_processed_data$apply_default_building_parameters, TRUE)

  settings_dfx <- test_template_data[[5]]
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "No"
  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  settings_processed_data <- processed_data[[5]]
  expect_equal(settings_processed_data$apply_default_building_parameters, FALSE)

})

test_that("Values for the Simulate capillary zone setting are mapped to the correct codes in the imported settings data", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  settings_processed_data <- processed_data[[5]]
  expect_equal(settings_processed_data$simulate_capillary_zone, TRUE)

  settings_dfx <- test_template_data[[5]]
  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "No"
  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  settings_processed_data <- processed_data[[5]]
  expect_equal(settings_processed_data$simulate_capillary_zone, FALSE)
})

#################################### BuildingInfo #########################################

# Deterministic tests

test_that("Deterministic simulation default building parameters load correctly for a residential building with a slab-on-grade foundation", {
  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Slab-on-grade"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  buildings_processed_data <- processed_data[[2]]

  expect_equal(buildings_processed_data$Lb, 0.1)
  expect_equal(buildings_processed_data$Lf, 0.1)
  expect_equal(buildings_processed_data$eta, 0.001)
  expect_equal(buildings_processed_data$Abf, 150)
  expect_equal(buildings_processed_data$Hb, 2.44)
  expect_equal(buildings_processed_data$ach, 0.45)
  expect_equal(buildings_processed_data$Qsoil_Qb, 0.003)
})

test_that("Deterministic simulation default building parameters load correctly for a residential building with a basement w/ slab foundation", {
  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Slab"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  buildings_processed_data <- processed_data[[2]]

  expect_equal(buildings_processed_data$Lb, 2)
  expect_equal(buildings_processed_data$Lf, 0.1)
  expect_equal(buildings_processed_data$eta, 0.001)
  expect_equal(buildings_processed_data$Abf, 150)
  expect_equal(buildings_processed_data$Hb, 3.66)
  expect_equal(buildings_processed_data$ach, 0.45)
  expect_equal(buildings_processed_data$Qsoil_Qb, 0.003)
})

test_that("Deterministic simulation default building parameters load correctly for a residential building with a basement w/ dirt floor", {
  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Dirt Floor"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  buildings_processed_data <- processed_data[[2]]

  expect_equal(buildings_processed_data$Lb, 2)
  expect_equal(buildings_processed_data$Lf, 0)
  expect_equal(buildings_processed_data$eta, 1)
  expect_equal(buildings_processed_data$Abf, 150)
  expect_equal(buildings_processed_data$Hb, 3.66)
  expect_equal(buildings_processed_data$ach, 0.45)
  expect_equal(buildings_processed_data$Qsoil_Qb, 0.003)
})

test_that("Deterministic simulation default building parameters load correctly for a residential building with a closed crawl space w/ slab foundation", {
  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Closed Crawl Space w/ Slab"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  buildings_processed_data <- processed_data[[2]]

  expect_equal(buildings_processed_data$Lb, 1)
  expect_equal(buildings_processed_data$Lf, 0.1)
  expect_equal(buildings_processed_data$eta, 0.001)
  expect_equal(buildings_processed_data$Abf, 150)
  expect_equal(buildings_processed_data$Hb, 1.3)
  expect_equal(buildings_processed_data$ach, 0.45)
  expect_equal(buildings_processed_data$Qsoil_Qb, 0.003)
})

test_that("Deterministic simulation default building parameters load correctly for a residential building with a closed crawl space w/ dirt floor foundation", {
  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Closed Crawl Space w/ Dirt Floor"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  buildings_processed_data <- processed_data[[2]]

  expect_equal(buildings_processed_data$Lb, 1)
  expect_equal(buildings_processed_data$Lf, 0)
  expect_equal(buildings_processed_data$eta, 1)
  expect_equal(buildings_processed_data$Abf, 150)
  expect_equal(buildings_processed_data$Hb, 1.3)
  expect_equal(buildings_processed_data$ach, 0.45)
  expect_equal(buildings_processed_data$Qsoil_Qb, 0.003)
})


test_that("Deterministic simulation default building parameters load correctly for a commercial building with a basement w/ slab foundation", {
  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Commercial"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Slab"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  buildings_processed_data <- processed_data[[2]]

  expect_equal(buildings_processed_data$Lb, 2)
  expect_equal(buildings_processed_data$Lf, 0.2)
  expect_equal(buildings_processed_data$eta, 0.001)
  expect_equal(buildings_processed_data$Abf, 1500)
  expect_equal(buildings_processed_data$Hb, 3)
  expect_equal(buildings_processed_data$ach, 1.5)
  expect_equal(buildings_processed_data$Qsoil_Qb, 0.003)
})

test_that("Deterministic simulation default building parameters load correctly for a commercial building with a basement w/ dirt floor foundation", {
  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Commercial"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Dirt Floor"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  buildings_processed_data <- processed_data[[2]]

  expect_equal(buildings_processed_data$Lb, 2)
  expect_equal(buildings_processed_data$Lf, 0)
  expect_equal(buildings_processed_data$eta, 1)
  expect_equal(buildings_processed_data$Abf, 1500)
  expect_equal(buildings_processed_data$Hb, 3)
  expect_equal(buildings_processed_data$ach, 1.5)
  expect_equal(buildings_processed_data$Qsoil_Qb, 0.003)
})

test_that("Deterministic simulation default building parameters load correctly for a commercial building with a slab-on-grade foundation", {
  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Commercial"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Slab-on-grade"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  buildings_processed_data <- processed_data[[2]]

  expect_equal(buildings_processed_data$Lb, 0.2)
  expect_equal(buildings_processed_data$Lf, 0.2)
  expect_equal(buildings_processed_data$eta, 0.001)
  expect_equal(buildings_processed_data$Abf, 1500)
  expect_equal(buildings_processed_data$Hb, 3)
  expect_equal(buildings_processed_data$ach, 1.5)
  expect_equal(buildings_processed_data$Qsoil_Qb, 0.003)
})

# Stochastic tests

test_that("Stochastic simulation default building parameters load correctly for a residential building with a slab-on-grade foundation", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Slab-on-grade"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  buildings_processed_data <- processed_data[[2]]

  Lb_dfx <- Filter(function(x) x$getSymbol() == "Lb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Lb_dfx$dist_type, "Triangular")
  expect_equal(Lb_dfx$maximum, 2.44)
  expect_equal(Lb_dfx$mode, 0.1)
  expect_equal(Lb_dfx$minimum, 0.1)
  expect_equal(Lb_dfx$units, "m")

  Lf_dfx <- Filter(function(x) x$getSymbol() == "Lf", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Lf_dfx$dist_type, "Triangular")
  expect_equal(Lf_dfx$maximum, 0.25)
  expect_equal(Lf_dfx$mode, 0.1)
  expect_equal(Lf_dfx$minimum, 0.1)
  expect_equal(Lf_dfx$units, "m")

  eta_dfx <- Filter(function(x) x$getSymbol() == "eta", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(eta_dfx$dist_type, "PERT")
  expect_equal(eta_dfx$maximum, 0.0019)
  expect_equal(eta_dfx$mode, 0.001)
  expect_equal(eta_dfx$minimum, 0.00019)
  expect_equal(eta_dfx$units, "-")

  Abf_dfx <- Filter(function(x) x$getSymbol() == "Abf", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Abf_dfx$dist_type, "PERT")
  expect_equal(Abf_dfx$maximum, 200)
  expect_equal(Abf_dfx$mode, 150)
  expect_equal(Abf_dfx$minimum, 80)
  expect_equal(Abf_dfx$units, "m2")

  Hb_dfx <- Filter(function(x) x$getSymbol() == "Hb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Hb_dfx$dist_type, "PERT")
  expect_equal(Hb_dfx$maximum, 3.05)
  expect_equal(Hb_dfx$mode, 2.44)
  expect_equal(Hb_dfx$minimum, 2.13)
  expect_equal(Hb_dfx$units, "m")

  ach_dfx <- Filter(function(x) x$getSymbol() == "ach", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(ach_dfx$dist_type, "PERT")
  expect_equal(ach_dfx$maximum, 1.26)
  expect_equal(ach_dfx$mode, 0.45)
  expect_equal(ach_dfx$minimum, 0.15)
  expect_equal(ach_dfx$units, "1/hr")

  Qsoil_Qb_dfx <- Filter(function(x) x$getSymbol() == "Qsoil_Qb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Qsoil_Qb_dfx$dist_type, "PERT")
  expect_equal(Qsoil_Qb_dfx$maximum, 0.05)
  expect_equal(Qsoil_Qb_dfx$mode, 0.003)
  expect_equal(Qsoil_Qb_dfx$minimum, 0.0001)
  expect_equal(Qsoil_Qb_dfx$units, "-")
})

test_that("Stochastic simulation default building parameters load correctly for a residential building with a basement-slab foundation", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Slab"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  buildings_processed_data <- processed_data[[2]]

  Lb_dfx <- Filter(function(x) x$getSymbol() == "Lb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Lb_dfx$dist_type, "PERT")
  expect_equal(Lb_dfx$maximum, 2.44)
  expect_equal(Lb_dfx$mode, 2)
  expect_equal(Lb_dfx$minimum, 0.1)
  expect_equal(Lb_dfx$units, "m")

  Lf_dfx <- Filter(function(x) x$getSymbol() == "Lf", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Lf_dfx$dist_type, "Triangular")
  expect_equal(Lf_dfx$maximum, 0.25)
  expect_equal(Lf_dfx$mode, 0.1)
  expect_equal(Lf_dfx$minimum, 0.1)
  expect_equal(Lf_dfx$units, "m")

  eta_dfx <- Filter(function(x) x$getSymbol() == "eta", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(eta_dfx$dist_type, "Triangular")
  expect_equal(eta_dfx$maximum, 0.001)
  expect_equal(eta_dfx$mode, 0.001)
  expect_equal(eta_dfx$minimum, 0.0001)
  expect_equal(eta_dfx$units, "-")

  Abf_dfx <- Filter(function(x) x$getSymbol() == "Abf", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Abf_dfx$dist_type, "PERT")
  expect_equal(Abf_dfx$maximum, 200)
  expect_equal(Abf_dfx$mode, 150)
  expect_equal(Abf_dfx$minimum, 80)
  expect_equal(Abf_dfx$units, "m2")

  Hb_dfx <- Filter(function(x) x$getSymbol() == "Hb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Hb_dfx$dist_type, "PERT")
  expect_equal(Hb_dfx$maximum, 4.88)
  expect_equal(Hb_dfx$mode, 3.66)
  expect_equal(Hb_dfx$minimum, 2.44)
  expect_equal(Hb_dfx$units, "m")

  ach_dfx <- Filter(function(x) x$getSymbol() == "ach", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(ach_dfx$dist_type, "PERT")
  expect_equal(ach_dfx$maximum, 1.26)
  expect_equal(ach_dfx$mode, 0.45)
  expect_equal(ach_dfx$minimum, 0.15)
  expect_equal(ach_dfx$units, "1/hr")

  Qsoil_Qb_dfx <- Filter(function(x) x$getSymbol() == "Qsoil_Qb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Qsoil_Qb_dfx$dist_type, "PERT")
  expect_equal(Qsoil_Qb_dfx$maximum, 0.05)
  expect_equal(Qsoil_Qb_dfx$mode, 0.003)
  expect_equal(Qsoil_Qb_dfx$minimum, 0.0001)
  expect_equal(Qsoil_Qb_dfx$units, "-")
})

test_that("Stochastic simulation default building parameters load correctly for a residential building with a basement-dirt foundation", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Dirt Floor"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  buildings_processed_data <- processed_data[[2]]

  Lb_dfx <- Filter(function(x) x$getSymbol() == "Lb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Lb_dfx$dist_type, "PERT")
  expect_equal(Lb_dfx$maximum, 2.44)
  expect_equal(Lb_dfx$mode, 2)
  expect_equal(Lb_dfx$minimum, 0.1)
  expect_equal(Lb_dfx$units, "m")

  Lf_dfx <- Filter(function(x) x$getSymbol() == "Lf", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Lf_dfx$dist_type, "Constant")
  expect_equal(Lf_dfx$constant, 0)
  expect_equal(Lf_dfx$units, "m")

  eta_dfx <- Filter(function(x) x$getSymbol() == "eta", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(eta_dfx$dist_type, "Constant")
  expect_equal(eta_dfx$constant, 1)
  expect_equal(eta_dfx$units, "-")

  Abf_dfx <- Filter(function(x) x$getSymbol() == "Abf", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Abf_dfx$dist_type, "PERT")
  expect_equal(Abf_dfx$maximum, 200)
  expect_equal(Abf_dfx$mode, 150)
  expect_equal(Abf_dfx$minimum, 80)
  expect_equal(Abf_dfx$units, "m2")

  Hb_dfx <- Filter(function(x) x$getSymbol() == "Hb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Hb_dfx$dist_type, "PERT")
  expect_equal(Hb_dfx$maximum, 4.88)
  expect_equal(Hb_dfx$mode, 3.6)
  expect_equal(Hb_dfx$minimum, 2.44)
  expect_equal(Hb_dfx$units, "m")

  ach_dfx <- Filter(function(x) x$getSymbol() == "ach", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(ach_dfx$dist_type, "PERT")
  expect_equal(ach_dfx$maximum, 1.26)
  expect_equal(ach_dfx$mode, 0.45)
  expect_equal(ach_dfx$minimum, 0.15)
  expect_equal(ach_dfx$units, "1/hr")

  Qsoil_Qb_dfx <- Filter(function(x) x$getSymbol() == "Qsoil_Qb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Qsoil_Qb_dfx$dist_type, "PERT")
  expect_equal(Qsoil_Qb_dfx$maximum, 0.05)
  expect_equal(Qsoil_Qb_dfx$mode, 0.003)
  expect_equal(Qsoil_Qb_dfx$minimum, 0.0001)
  expect_equal(Qsoil_Qb_dfx$units, "-")
})

test_that("Stochastic simulation default building parameters load correctly for a residential building with a closed crawl space with slab foundation", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Closed Crawl Space w/ Slab"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  buildings_processed_data <- processed_data[[2]]

  Lb_dfx <- Filter(function(x) x$getSymbol() == "Lb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Lb_dfx$dist_type, "PERT")
  expect_equal(Lb_dfx$maximum, 2.44)
  expect_equal(Lb_dfx$mode, 1)
  expect_equal(Lb_dfx$minimum, 0.1)
  expect_equal(Lb_dfx$units, "m")

  Lf_dfx <- Filter(function(x) x$getSymbol() == "Lf", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Lf_dfx$dist_type, "Triangular")
  expect_equal(Lf_dfx$maximum, 0.25)
  expect_equal(Lf_dfx$mode, 0.1)
  expect_equal(Lf_dfx$minimum, 0.1)
  expect_equal(Lf_dfx$units, "m")

  eta_dfx <- Filter(function(x) x$getSymbol() == "eta", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(eta_dfx$dist_type, "Triangular")
  expect_equal(eta_dfx$maximum, 0.001)
  expect_equal(eta_dfx$mode, 0.001)
  expect_equal(eta_dfx$minimum, 0.0001)
  expect_equal(eta_dfx$units, "-")

  Abf_dfx <- Filter(function(x) x$getSymbol() == "Abf", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Abf_dfx$dist_type, "PERT")
  expect_equal(Abf_dfx$maximum, 200)
  expect_equal(Abf_dfx$mode, 150)
  expect_equal(Abf_dfx$minimum, 80)
  expect_equal(Abf_dfx$units, "m2")

  Hb_dfx <- Filter(function(x) x$getSymbol() == "Hb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Hb_dfx$dist_type, "Triangular")
  expect_equal(Hb_dfx$maximum, 1.3)
  expect_equal(Hb_dfx$mode, 1.3)
  expect_equal(Hb_dfx$minimum, 0.5)
  expect_equal(Hb_dfx$units, "m")

  ach_dfx <- Filter(function(x) x$getSymbol() == "ach", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(ach_dfx$dist_type, "PERT")
  expect_equal(ach_dfx$maximum, 1.26)
  expect_equal(ach_dfx$mode, 0.45)
  expect_equal(ach_dfx$minimum, 0.15)
  expect_equal(ach_dfx$units, "1/hr")

  Qsoil_Qb_dfx <- Filter(function(x) x$getSymbol() == "Qsoil_Qb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Qsoil_Qb_dfx$dist_type, "PERT")
  expect_equal(Qsoil_Qb_dfx$maximum, 0.05)
  expect_equal(Qsoil_Qb_dfx$mode, 0.003)
  expect_equal(Qsoil_Qb_dfx$minimum, 0.0001)
  expect_equal(Qsoil_Qb_dfx$units, "-")
})

test_that("Stochastic simulation default building parameters load correctly for a residential building with a closed crawl space with dirt floor foundation", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Closed Crawl Space w/ Dirt Floor"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  buildings_processed_data <- processed_data[[2]]

  Lb_dfx <- Filter(function(x) x$getSymbol() == "Lb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Lb_dfx$dist_type, "PERT")
  expect_equal(Lb_dfx$maximum, 2.44)
  expect_equal(Lb_dfx$mode, 1)
  expect_equal(Lb_dfx$minimum, 0.1)
  expect_equal(Lb_dfx$units, "m")

  Lf_dfx <- Filter(function(x) x$getSymbol() == "Lf", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Lf_dfx$dist_type, "Constant")
  expect_equal(Lf_dfx$constant, 0)
  expect_equal(Lf_dfx$units, "m")

  eta_dfx <- Filter(function(x) x$getSymbol() == "eta", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(eta_dfx$dist_type, "Constant")
  expect_equal(eta_dfx$constant, 1)
  expect_equal(eta_dfx$units, "-")

  Abf_dfx <- Filter(function(x) x$getSymbol() == "Abf", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Abf_dfx$dist_type, "PERT")
  expect_equal(Abf_dfx$maximum, 200)
  expect_equal(Abf_dfx$mode, 150)
  expect_equal(Abf_dfx$minimum, 80)
  expect_equal(Abf_dfx$units, "m2")

  Hb_dfx <- Filter(function(x) x$getSymbol() == "Hb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Hb_dfx$dist_type, "Triangular")
  expect_equal(Hb_dfx$maximum, 1.3)
  expect_equal(Hb_dfx$mode, 1.3)
  expect_equal(Hb_dfx$minimum, 0.5)
  expect_equal(Hb_dfx$units, "m")

  ach_dfx <- Filter(function(x) x$getSymbol() == "ach", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(ach_dfx$dist_type, "PERT")
  expect_equal(ach_dfx$maximum, 1.26)
  expect_equal(ach_dfx$mode, 0.45)
  expect_equal(ach_dfx$minimum, 0.15)
  expect_equal(ach_dfx$units, "1/hr")

  Qsoil_Qb_dfx <- Filter(function(x) x$getSymbol() == "Qsoil_Qb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Qsoil_Qb_dfx$dist_type, "PERT")
  expect_equal(Qsoil_Qb_dfx$maximum, 0.05)
  expect_equal(Qsoil_Qb_dfx$mode, 0.003)
  expect_equal(Qsoil_Qb_dfx$minimum, 0.0001)
  expect_equal(Qsoil_Qb_dfx$units, "-")
})

test_that("Stochastic simulation default building parameters load correctly for a commercial building with a slab-on-grade foundation", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Commercial"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Slab-on-grade"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  buildings_processed_data <- processed_data[[2]]

  Lb_dfx <- Filter(function(x) x$getSymbol() == "Lb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Lb_dfx$dist_type, "PERT")
  expect_equal(Lb_dfx$maximum, 2.44)
  expect_equal(Lb_dfx$mode, 0.2)
  expect_equal(Lb_dfx$minimum, 0.1)
  expect_equal(Lb_dfx$units, "m")

  Lf_dfx <- Filter(function(x) x$getSymbol() == "Lf", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Lf_dfx$dist_type, "PERT")
  expect_equal(Lf_dfx$maximum, 0.25)
  expect_equal(Lf_dfx$mode, 0.2)
  expect_equal(Lf_dfx$minimum, 0.1)
  expect_equal(Lf_dfx$units, "m")

  eta_dfx <- Filter(function(x) x$getSymbol() == "eta", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(eta_dfx$dist_type, "PERT")
  expect_equal(eta_dfx$maximum, 0.0019)
  expect_equal(eta_dfx$mode, 0.001)
  expect_equal(eta_dfx$minimum, 0.00019)
  expect_equal(eta_dfx$units, "-")

  Abf_dfx <- Filter(function(x) x$getSymbol() == "Abf", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Abf_dfx$dist_type, "PERT")
  expect_equal(Abf_dfx$maximum, 100000)
  expect_equal(Abf_dfx$mode, 1500)
  expect_equal(Abf_dfx$minimum, 80)
  expect_equal(Abf_dfx$units, "m2")

  Hb_dfx <- Filter(function(x) x$getSymbol() == "Hb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Hb_dfx$dist_type, "PERT")
  expect_equal(Hb_dfx$maximum, 3.05)
  expect_equal(Hb_dfx$mode, 3)
  expect_equal(Hb_dfx$minimum, 2.13)
  expect_equal(Hb_dfx$units, "m")

  ach_dfx <- Filter(function(x) x$getSymbol() == "ach", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(ach_dfx$dist_type, "PERT")
  expect_equal(ach_dfx$maximum, 4.1)
  expect_equal(ach_dfx$mode, 1.5)
  expect_equal(ach_dfx$minimum, 0.3)
  expect_equal(ach_dfx$units, "1/hr")

  Qsoil_Qb_dfx <- Filter(function(x) x$getSymbol() == "Qsoil_Qb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Qsoil_Qb_dfx$dist_type, "PERT")
  expect_equal(Qsoil_Qb_dfx$maximum, 0.05)
  expect_equal(Qsoil_Qb_dfx$mode, 0.003)
  expect_equal(Qsoil_Qb_dfx$minimum, 0.0001)
  expect_equal(Qsoil_Qb_dfx$units, "-")
})

test_that("Stochastic simulation default building parameters load correctly for a commercial building with a basement with slab foundation", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Commercial"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Slab"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  buildings_processed_data <- processed_data[[2]]

  Lb_dfx <- Filter(function(x) x$getSymbol() == "Lb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Lb_dfx$dist_type, "PERT")
  expect_equal(Lb_dfx$maximum, 2.44)
  expect_equal(Lb_dfx$mode, 2)
  expect_equal(Lb_dfx$minimum, 0.1)
  expect_equal(Lb_dfx$units, "m")

  Lf_dfx <- Filter(function(x) x$getSymbol() == "Lf", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Lf_dfx$dist_type, "PERT")
  expect_equal(Lf_dfx$maximum, 0.25)
  expect_equal(Lf_dfx$mode, 0.2)
  expect_equal(Lf_dfx$minimum, 0.1)
  expect_equal(Lf_dfx$units, "m")

  eta_dfx <- Filter(function(x) x$getSymbol() == "eta", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(eta_dfx$dist_type, "Triangular")
  expect_equal(eta_dfx$maximum, 0.001)
  expect_equal(eta_dfx$mode, 0.001)
  expect_equal(eta_dfx$minimum, 0.0001)
  expect_equal(eta_dfx$units, "-")

  Abf_dfx <- Filter(function(x) x$getSymbol() == "Abf", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Abf_dfx$dist_type, "PERT")
  expect_equal(Abf_dfx$maximum, 100000)
  expect_equal(Abf_dfx$mode, 1500)
  expect_equal(Abf_dfx$minimum, 80)
  expect_equal(Abf_dfx$units, "m2")

  Hb_dfx <- Filter(function(x) x$getSymbol() == "Hb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Hb_dfx$dist_type, "PERT")
  expect_equal(Hb_dfx$maximum, 4.88)
  expect_equal(Hb_dfx$mode, 3)
  expect_equal(Hb_dfx$minimum, 2.44)
  expect_equal(Hb_dfx$units, "m")

  ach_dfx <- Filter(function(x) x$getSymbol() == "ach", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(ach_dfx$dist_type, "PERT")
  expect_equal(ach_dfx$maximum, 4.1)
  expect_equal(ach_dfx$mode, 1.5)
  expect_equal(ach_dfx$minimum, 0.3)
  expect_equal(ach_dfx$units, "1/hr")

  Qsoil_Qb_dfx <- Filter(function(x) x$getSymbol() == "Qsoil_Qb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Qsoil_Qb_dfx$dist_type, "PERT")
  expect_equal(Qsoil_Qb_dfx$maximum, 0.05)
  expect_equal(Qsoil_Qb_dfx$mode, 0.003)
  expect_equal(Qsoil_Qb_dfx$minimum, 0.0001)
  expect_equal(Qsoil_Qb_dfx$units, "-")
})

test_that("Stochastic simulation default building parameters load correctly for a commercial building with a basement with dirt floor foundation", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Commercial"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Dirt Floor"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  buildings_processed_data <- processed_data[[2]]

  Lb_dfx <- Filter(function(x) x$getSymbol() == "Lb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Lb_dfx$dist_type, "PERT")
  expect_equal(Lb_dfx$maximum, 2.44)
  expect_equal(Lb_dfx$mode, 2)
  expect_equal(Lb_dfx$minimum, 0.1)
  expect_equal(Lb_dfx$units, "m")

  Lf_dfx <- Filter(function(x) x$getSymbol() == "Lf", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Lf_dfx$dist_type, "Constant")
  expect_equal(Lf_dfx$constant, 0)
  expect_equal(Lf_dfx$units, "m")

  eta_dfx <- Filter(function(x) x$getSymbol() == "eta", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(eta_dfx$dist_type, "Constant")
  expect_equal(eta_dfx$constant, 1)
  expect_equal(eta_dfx$units, "-")

  Abf_dfx <- Filter(function(x) x$getSymbol() == "Abf", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Abf_dfx$dist_type, "PERT")
  expect_equal(Abf_dfx$maximum, 100000)
  expect_equal(Abf_dfx$mode, 1500)
  expect_equal(Abf_dfx$minimum, 80)
  expect_equal(Abf_dfx$units, "m2")

  Hb_dfx <- Filter(function(x) x$getSymbol() == "Hb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Hb_dfx$dist_type, "PERT")
  expect_equal(Hb_dfx$maximum, 4.88)
  expect_equal(Hb_dfx$mode, 3)
  expect_equal(Hb_dfx$minimum, 2.44)
  expect_equal(Hb_dfx$units, "m")

  ach_dfx <- Filter(function(x) x$getSymbol() == "ach", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(ach_dfx$dist_type, "PERT")
  expect_equal(ach_dfx$maximum, 4.1)
  expect_equal(ach_dfx$mode, 1.5)
  expect_equal(ach_dfx$minimum, 0.3)
  expect_equal(ach_dfx$units, "1/hr")

  Qsoil_Qb_dfx <- Filter(function(x) x$getSymbol() == "Qsoil_Qb", buildings_processed_data)[[1]]$getDataFrameOfProperties()
  expect_equal(Qsoil_Qb_dfx$dist_type, "PERT")
  expect_equal(Qsoil_Qb_dfx$maximum, 0.05)
  expect_equal(Qsoil_Qb_dfx$mode, 0.003)
  expect_equal(Qsoil_Qb_dfx$minimum, 0.0001)
  expect_equal(Qsoil_Qb_dfx$units, "-")
})

test_that("correct error thrown when buildinginfo data sheet is missing a needed column", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: One or more required columns are missing from the BuildingInfo sheet of the import file"

  building_dfx <- building_dfx %>% select(-Parameter)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx <- building_dfx %>% select(-JEMSymbol)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx <- building_dfx %>% select(-Units)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx <- building_dfx %>% select(-Distribution)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx <- building_dfx %>% select(-DistributionParameterType)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx <- building_dfx %>% select(-DistributionParameterValue)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("check that no rows still have Not Applicable written in the DistributionParameterType field",{
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  building_processed_data <- processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))[[2]]
  expect_true(all(building_processed_data$DistributionParameterType != "Not Applicable"))
})

test_that("correct error thrown when buildinginfo data sheet is missing a needed parameter name", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: One or more required parameters are missing from the BuildingInfo sheet of the import file"

  building_dfx <- building_dfx %>% filter(Parameter!="Depth below grade to base of foundation")
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx <- building_dfx %>% filter(Parameter!="Foundation thickness")
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx <- building_dfx %>% filter(Parameter!="Fraction of foundation area with cracks")
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx <- building_dfx %>% filter(Parameter!="Enclosed space floor area")
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx <- building_dfx %>% filter(Parameter!="Enclosed space mixing height")
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx <- building_dfx %>% filter(Parameter!="Indoor air exchange rate")
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx <- building_dfx %>% filter(Parameter!="Qsoil/Qbuilding")
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
  })

test_that("correct error thrown when parameters have unacceptable units", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Incorrect units assigned in the BuildingInfo sheet of the import file"

  building_dfx$Units[building_dfx$Parameter == "Depth below grade to base of foundation"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx$Units[building_dfx$Parameter == "Foundation thickness"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx$Units[building_dfx$Parameter == "Fraction of foundation area with cracks"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx$Units[building_dfx$Parameter == "Enclosed space floor area"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx$Units[building_dfx$Parameter == "Enclosed space mixing height"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx$Units[building_dfx$Parameter == "Indoor air exchange rate"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx$Units[building_dfx$Parameter == "Qsoil/Qbuilding"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when not all parameters have the correct symbol", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Incorrect JEM symbol assigned in the BuildingInfo sheet of the import file"

  building_dfx$JEMSymbol[building_dfx$Parameter == "Depth below grade to base of foundation"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx$JEMSymbol[building_dfx$Parameter == "Foundation thickness"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx$JEMSymbol[building_dfx$Parameter == "Fraction of foundation area with cracks"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx$JEMSymbol[building_dfx$Parameter == "Enclosed space floor area"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx$JEMSymbol[building_dfx$Parameter == "Enclosed space mixing height"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx$JEMSymbol[building_dfx$Parameter == "Indoor air exchange rate"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx$JEMSymbol[building_dfx$Parameter == "Qsoil/Qbuilding"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when one or more properties is not one of the allowed distributions", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Unsupported distribution assigned in the BuildingInfo sheet of the import file"

  building_dfx$Distribution <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})
#QA
test_that("correct error thrown when distribution does not have the appropriate parameters", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Incorrect distribution parameter type assigned in the BuildingInfo sheet of the import file"

  building_dfx$DistributionParameterType[building_dfx$Distribution== "Constant"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx$DistributionParameterType[building_dfx$Distribution== "Uniform"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx$DistributionParameterType[building_dfx$Distribution== "Triangular"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx$DistributionParameterType[building_dfx$Distribution== "PERT"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx$DistributionParameterType[building_dfx$Distribution== "Truncated Normal"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx$DistributionParameterType[building_dfx$Distribution== "Truncated Lognormal"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when parameter values are missing", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Missing or non-numeric parameter value assigned in the BuildingInfo sheet of the import file"

  building_dfx$DistributionParameterValue <- NA
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  building_dfx$DistributionParameterValue <- "NonNumericValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("units converted from ft or ft2 to m or m2", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  building_dfx$Units[building_dfx$Parameter == "Foundation thickness"] <- "ft"
  building_dfx$Units[building_dfx$Parameter == "Enclosed space floor area"] <- "ft2"

  building_dfx <- processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))[[2]]
  expect_true(all(building_dfx$Units %in% c("m", "m2")))
})

test_that("correct error thrown when not only constants were assigned in the data import template", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: One or more parameters on the BuildingInfo sheet isn't assigned a constant value. Only constant building parameters are allowed in a deterministic simulation."

  building_dfx$Distribution <- "Triangular"
  building_dfx$DistributionParameterType <- "Minimum"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when a parameter value occurs more than once in the data frame", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: More than one value is assigned to a parameter on the BuildingInfo sheet. Only one constant value is allowed per building parameter in a deterministic simulation."

  new_row_building_dfx <- data.frame (
    Parameter = c("Depth below grade to base of foundation"),
    Units = c("m"),
    JEMSymbol = c("Lb"),
    Distribution = c("Constant"),
    DistributionParameterType = c("Constant"),
    DistributionParameterValue = c(0.2))

  building_dfx <- rbind(building_dfx, new_row_building_dfx)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  new_row_building_dfx <- data.frame (
    Parameter = c("Foundation thickness"),
    Units = c("m"),
    JEMSymbol = c("Lf"),
    Distribution = c("Constant"),
    DistributionParameterType = c("Constant"),
    DistributionParameterValue = c(0.2))

  building_dfx <- rbind(building_dfx, new_row_building_dfx)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  new_row_building_dfx <- data.frame (
    Parameter = c("Fraction of foundation area with cracks"),
    Units = c("-"),
    JEMSymbol = c("eta"),
    Distribution = c("Constant"),
    DistributionParameterType = c("Constant"),
    DistributionParameterValue = c(0.2))

  building_dfx <- rbind(building_dfx, new_row_building_dfx)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  new_row_building_dfx <- data.frame (
    Parameter = c("Enclosed space floor area"),
    Units = c("m2"),
    JEMSymbol = c("Abf"),
    Distribution = c("Constant"),
    DistributionParameterType = c("Constant"),
    DistributionParameterValue = c(0.2))

  building_dfx <- rbind(building_dfx, new_row_building_dfx)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  new_row_building_dfx <- data.frame (
    Parameter = c("Enclosed space mixing height"),
    Units = c("m"),
    JEMSymbol = c("Hb"),
    Distribution = c("Constant"),
    DistributionParameterType = c("Constant"),
    DistributionParameterValue = c(0.2))

  building_dfx <- rbind(building_dfx, new_row_building_dfx)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  new_row_building_dfx <- data.frame (
    Parameter = c("Indoor air exchange rate"),
    Units = c("1/hr"),
    JEMSymbol = c("ach"),
    Distribution = c("Constant"),
    DistributionParameterType = c("Constant"),
    DistributionParameterValue = c(0.2))

  building_dfx <- rbind(building_dfx, new_row_building_dfx)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  building_dfx <- test_template_data[[2]]
  new_row_building_dfx <- data.frame (
    Parameter = c("Qsoil/Qbuilding"),
    Units = c("-"),
    JEMSymbol = c("Qsoil_Qb"),
    Distribution = c("Constant"),
    DistributionParameterType = c("Constant"),
    DistributionParameterValue = c(0.2))

  building_dfx <- rbind(building_dfx, new_row_building_dfx)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("check that deterministic values assigned correctly",{
  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  building_processed_data <- processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))[[2]]
  expect_equal(0.1524, building_processed_data$Lb[1])
  expect_equal(0.1524, building_processed_data$Lf[1])
  expect_equal(0.001, building_processed_data$eta[1])
  expect_equal(175, building_processed_data$Abf[1])
  expect_equal(2.6, building_processed_data$Hb[1])
  expect_equal(0.45, building_processed_data$ach[1])
  expect_equal(0.003, building_processed_data$Qsoil_Qb[1])
})

test_that("check that stochastic values assigned correctly",{
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  building_processed_data <- processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))[[2]]
  Lbdist <- building_processed_data[[1]]$getDataFrameOfProperties()
  expect_equal("Lb", Lbdist$symbol)
  expect_equal("Depth below grade to base of foundation", Lbdist$name)
  expect_equal("m", Lbdist$units)
  expect_equal("Constant", Lbdist$dist_type)
  expect_equal(0.1524, Lbdist$constant)

  Lfdist <- building_processed_data[[2]]$getDataFrameOfProperties()
  expect_equal("Lf", Lfdist$symbol)
  expect_equal("Foundation thickness", Lfdist$name)
  expect_equal("m", Lfdist$units)
  expect_equal("Constant", Lfdist$dist_type)
  expect_equal(0.1524, Lfdist$constant)

  etadist <- building_processed_data[[3]]$getDataFrameOfProperties()
  expect_equal("eta", etadist$symbol)
  expect_equal("Fraction of foundation area with cracks", etadist$name)
  expect_equal("-", etadist$units)
  expect_equal("PERT", etadist$dist_type)
  expect_equal(0.00019, etadist$minimum)
  expect_equal(0.001, etadist$mode)
  expect_equal(0.0019, etadist$maximum)

  Abfdist <- building_processed_data[[4]]$getDataFrameOfProperties()
  expect_equal("Abf", Abfdist$symbol)
  expect_equal("Enclosed space floor area", Abfdist$name)
  expect_equal("m2", Abfdist$units)
  expect_equal("Constant", Abfdist$dist_type)
  expect_equal(175, Abfdist$constant)

  Hbdist <- building_processed_data[[5]]$getDataFrameOfProperties()
  expect_equal("Hb", Hbdist$symbol)
  expect_equal("Enclosed space mixing height", Hbdist$name)
  expect_equal("m", Hbdist$units)
  expect_equal("Uniform", Hbdist$dist_type)
  expect_equal(2.43, Hbdist$minimum)
  expect_equal(2.75, Hbdist$maximum)

  achdist <- building_processed_data[[6]]$getDataFrameOfProperties()
  expect_equal("ach", achdist$symbol)
  expect_equal("Indoor air exchange rate", achdist$name)
  expect_equal("1/hr", achdist$units)
  expect_equal("PERT", achdist$dist_type)
  expect_equal(0.15, achdist$minimum)
  expect_equal(0.45, achdist$mode)
  expect_equal(1.26, achdist$maximum)

  QsoilQbdist <- building_processed_data[[7]]$getDataFrameOfProperties()
  expect_equal("Qsoil_Qb", QsoilQbdist$symbol)
  expect_equal("Qsoil/Qbuilding", QsoilQbdist$name)
  expect_equal("-", QsoilQbdist$units)
  expect_equal("Constant", QsoilQbdist$dist_type)
  expect_equal(0.003, QsoilQbdist$constant)
})

#################################### VadoseZoneInfo #########################################

test_that("correct error thrown when vadose zone info data sheet is missing a needed column", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: One or more required columns are missing from the VadoseZoneInfo sheet of the import file"

  source_dfx <- source_dfx %>% select(-Parameter)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  source_dfx <- test_template_data[[3]]
  source_dfx <- source_dfx %>% select(-JEMSymbol)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  source_dfx <- test_template_data[[3]]
  source_dfx <- source_dfx %>% select(-Units)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  source_dfx <- test_template_data[[3]]
  source_dfx <- source_dfx %>% select(-Distribution)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  source_dfx <- test_template_data[[3]]
  source_dfx <- source_dfx %>% select(-DistributionParameterType)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  source_dfx <- test_template_data[[3]]
  source_dfx <- source_dfx %>% select(-DistributionParameterValue)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
  })

test_that("check that no rows still have Not Applicable values",{
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  source_processed_data <- processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))[[3]]
  expect_true(all(source_processed_data$DistributionParameterType != "Not Applicable"))
})

test_that("correct error thrown when vadose zone info data sheet is missing a needed parameter name", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: One or more required parameters are missing from the VadoseZoneInfo sheet of the import file"

  source_dfx <- source_dfx %>% filter(Parameter!="Depth below grade to source")
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  source_dfx <- test_template_data[[3]]
  source_dfx <- source_dfx %>% filter(Parameter!="Average source temperature")
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when parameters have unacceptable units", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Incorrect units assigned in the VadoseZoneInfo sheet of the import file"

  source_dfx$Units[source_dfx$Parameter == "Depth below grade to source"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  source_dfx <- test_template_data[[3]]
  source_dfx$Units[source_dfx$Parameter == "Average source temperature"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when not all parameters have the correct symbol", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Incorrect JEM symbol assigned in the VadoseZoneInfo sheet of the import file"

  source_dfx$JEMSymbol[source_dfx$Parameter == "Depth below grade to source"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  source_dfx <- test_template_data[[3]]
  source_dfx$JEMSymbol[source_dfx$Parameter == "Average source temperature"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when one or more properties is not one of the allowed distributions", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Unsupported distribution assigned in the VadoseZoneInfo sheet of the import file"

  source_dfx$Distribution <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when distribution does not have the appropriate parameters", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Incorrect distribution parameter type assigned in the VadoseZoneInfo sheet of the import file"

  source_dfx$DistributionParameterType[source_dfx$Distribution == "PERT"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  #QA <- Add checks on the other distribution parameters
  source_dfx$DistributionParameterType[source_dfx$Distribution == "Constant"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  source_dfx$DistributionParameterType[source_dfx$Distribution == "Uniform"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  source_dfx$DistributionParameterType[source_dfx$Distribution == "Triangular"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  source_dfx$DistributionParameterType[source_dfx$Distribution == "PERT"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  source_dfx$DistributionParameterType[source_dfx$Distribution == "Truncated Normal"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  source_dfx$DistributionParameterType[source_dfx$Distribution == "Truncated Lognormal"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

})

test_that("correct error thrown when parameter values are missing", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Missing or non-numeric parameter value assigned in the VadoseZoneInfo sheet of the import file"

  source_dfx$DistributionParameterValue <- NA
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  source_dfx <- test_template_data[[3]]
  source_dfx$DistributionParameterValue <- "NonNumericValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("units converted from ft to m and deg F to deg C", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  source_dfx$Units[source_dfx$Parameter == "Depth below grade to source"] <- "ft"
  source_dfx$Units[source_dfx$Parameter == "Average source temperature"] <- "deg F"

  source_processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))[3]
  expect_true(all(source_processed_data$Units %in% c("m", "deg C")))
})

test_that("correct error thrown when not only constants were assigned in the data import template", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: One or more parameters on the VadoseZoneInfo sheet isn't assigned a constant value. Only constant vadose zone parameters are allowed in a deterministic simulation."

  source_dfx$Distribution <- "Triangular"
  source_dfx$DistributionParameterType <- "Minimum"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when a parameter value occurs more than once in the data frame", {

    test_template_data <- get_deterministic_test_template_data()

    contam_dfx <- test_template_data[[1]]
    building_dfx <- test_template_data[[2]]
    source_dfx <- test_template_data[[3]]
    stratum_dfx <- test_template_data[[4]]
    settings_dfx <- test_template_data[[5]]
    ref_air_conc_dfx <- test_template_data[[6]]

    errorMessage <- "Error: More than one value is assigned to a parameter on the VadoseZoneInfo sheet. Only one constant value is allowed per vadose zone parameter in a deterministic simulation."

    new_row_source_dfx <- data.frame (
      Parameter = c("Depth below grade to source"),
      Units = c("m"),
      JEMSymbol = c("Ls"),
      Distribution = c("Constant"),
      DistributionParameterType = c("Constant"),
      DistributionParameterValue = c(0.2))

    source_dfx <- rbind(source_dfx, new_row_source_dfx)
    expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

    source_dfx <- test_template_data[[3]]
    new_row_source_dfx <- data.frame (
      Parameter = c("Average source temperature"),
      Units = c("deg C"),
      JEMSymbol = c("Ts"),
      Distribution = c("Constant"),
      DistributionParameterType = c("Constant"),
      DistributionParameterValue = c(0.2))

    source_dfx <- rbind(source_dfx, new_row_source_dfx)
    expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})


test_that("check that deterministic values assigned correctly",{
  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  source_processed_data <- processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))[[3]]
  expect_equal(15.4, source_processed_data$Ls[1])
  expect_equal(18, source_processed_data$Ts[1])
})

test_that("check that stochastic values assigned correctly",{
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  source_processed_data <- processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))[[3]]
  Lsdist <- source_processed_data[[1]]$getDataFrameOfProperties()

  expect_equal("Ls", Lsdist$symbol)
  expect_equal("Depth below grade to source", Lsdist$name)
  expect_equal("m", Lsdist$units)
  expect_equal("PERT", Lsdist$dist_type)
  expect_equal(9.03, Lsdist$minimum)
  expect_equal(15.4, Lsdist$mode)
  expect_equal(23.4, Lsdist$maximum)

  Tsdist <- source_processed_data[[2]]$getDataFrameOfProperties()
  expect_equal("Ts", Tsdist$symbol)
  expect_equal("Average source temperature", Tsdist$name)
  expect_equal("deg C", Tsdist$units)
  expect_equal("PERT", Tsdist$dist_type)
  expect_equal(12, Tsdist$minimum)
  expect_equal(18, Tsdist$mode)
  expect_equal(24, Tsdist$maximum)
})

#################################### StrataLogs #########################################

test_that("correct error thrown when stratum logs data sheet is missing a needed column", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: One or more required columns are missing from the StrataLogs sheet of the import file"

  stratum_dfx <- stratum_dfx %>% select(-LogID)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  stratum_dfx <- test_template_data[[4]]
  stratum_dfx <- stratum_dfx %>% select(-LayerOrder)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  stratum_dfx <- test_template_data[[4]]
  stratum_dfx <- stratum_dfx %>% select(-Thickness)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  stratum_dfx <- test_template_data[[4]]
  stratum_dfx <- stratum_dfx %>% select(-Units)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  stratum_dfx <- test_template_data[[4]]
  stratum_dfx <- stratum_dfx %>% select(-SoilType)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
  })

test_that("correct error thrown when stratum log data sheet is missing a parameter value", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Missing parameter value in the StrataLogs sheet of the import file"

   stratum_dfx$LogID <- NA
   expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
  })

 test_that("correct error thrown when not all parameters have acceptable units", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Incorrect units assigned in the StrataLogs sheet of the import file"

  stratum_dfx$Units[stratum_dfx$LogID == "IS05"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  })

test_that("correct error thrown when an unsupported soil type is assigned", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Unsupported soil type assigned in the StrataLogs sheet of the import file"

  stratum_dfx$SoilType <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
 })

test_that("correct error thrown when the layer order or thickness are not numeric", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Non-numeric parameter value assigned in the LayerOrder or Thickness fields of the StrataLogs sheet of the import file"

  stratum_dfx$LayerOrder <- "NonNumericValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  stratum_dfx <- test_template_data[[4]]
  stratum_dfx$Thickness <- "NonNumericValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("units converted from ft to m", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  stratum_dfx$Units[stratum_dfx$LogID == "IS05"] <- "ft"

  stratum_processed_data <- processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))[4]
  expect_true(all(stratum_processed_data$Units %in% c("m")))
})

#################################### ContaminantData #########################################

test_that("correct error thrown when contaminant data sheet is missing a needed column", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: One or more required columns are missing from the ContaminantData sheet of the import file"

  contam_dfx <- contam_dfx %>% select(-Contaminant)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  contam_dfx <- test_template_data[[1]]
  contam_dfx <- contam_dfx %>% select(-CASRN)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  contam_dfx <- test_template_data[[1]]
  contam_dfx <- contam_dfx %>% select(-Medium)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  contam_dfx <- test_template_data[[1]]
  contam_dfx <- contam_dfx %>% select(-Concentration)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  contam_dfx <- test_template_data[[1]]
  contam_dfx <- contam_dfx %>% select(-Units)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  contam_dfx <- test_template_data[[1]]
  contam_dfx <- contam_dfx %>% select(-DetectedFlag)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  contam_dfx <- test_template_data[[1]]
  contam_dfx <- contam_dfx %>% select(-SampleLocationID)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  contam_dfx <- test_template_data[[1]]
  contam_dfx <- contam_dfx %>% select(-SampleDate)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when parameter values are missing", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Missing parameter value in the ContaminantData sheet of the import file"
  contam_dfx$Concentration <- NA
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when more than one contaminant is assigned for the same CASRN in the ContaminantData sheet", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: More than one contaminant name was assigned for the same CASRN in the ContaminantData sheet"
  contam_dfx$Contaminant[1] <- "NewContaminantName"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when more than one CASRN is assigned for the same contaminant in the ContaminantData sheet", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: More than one CASRN was assigned for the same contaminant in the ContaminantData sheet"
  contam_dfx$CASRN[1] <- "NewContaminantCASRN"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when incorrect units occur", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Incorrect units assigned in the ContaminantData sheet of the import file"

  contam_dfx$Units[contam_dfx$Contaminant == "Tetrachloroethylene"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
  })

test_that("correct error thrown when detected flags are not correctly assigned", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Unsupported value assigned in the DetectedFlag field of the ContaminantData sheet of the import file"

  contam_dfx$DetectedFlag[contam_dfx$Contaminant == "Tetrachloroethylene"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when medium field values not correctly assigned", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Unsupported value assigned in the Medium field of the ContaminantData sheet of the import file"

  contam_dfx$Medium[contam_dfx$Contaminant == "Tetrachloroethylene"] <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when concentration values are not all numeric", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Non-numeric parameter value assigned in the Concentration field of the ContaminantData sheet of the import file"

  contam_dfx$Concentration <- "NonNumericValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when no detected values are present", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: The imported contaminant data set does not include any detections for the medium being simulated"

  contam_dfx$DetectedFlag <- "No"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when unit types are mixed", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Indoor air and soil gas data may be entered using either \"mass per volume\" units (mg/m3, ug/m3, etc.) or units in \"parts per\" notation (ppm, ppb, etc.), but not both"
  contam_dfx$Units[2] <- "ppm"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("water concentrations converted to ug/L", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  contam_processed_data <- processed_data[[1]]
  contam_processed_data <- contam_processed_data %>% filter(Medium == "Groundwater")
  expect_equal("ug/L", contam_processed_data$Units[1])
  expect_equal("ug/L", contam_processed_data$Units[2])
  expect_equal("ug/L", contam_processed_data$Units[3])
  expect_equal("ug/L", contam_processed_data$Units[4])
  expect_equal("ug/L", contam_processed_data$Units[5])
  expect_equal("ug/L", contam_processed_data$Units[6])
})

test_that("Air concentrations converted from other mass/volume units to ug/m3", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  contam_processed_data <- processed_data[[1]]
  contam_processed_data <- contam_processed_data %>% filter(Medium == "Indoor Air")
  expect_equal("ug/m3", contam_processed_data$Units[1])
  expect_equal("ug/m3", contam_processed_data$Units[2])
  expect_equal("ug/m3", contam_processed_data$Units[3])
  expect_equal("ug/m3", contam_processed_data$Units[4])
})

test_that("correct error thrown when air concentrations are not converted from ppm to ppb", {
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  contam_dfx$Units[contam_dfx$Medium == "Indoor Air"] <- "ppm"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  contam_processed_data <- processed_data[[1]]
  contam_processed_data <- contam_processed_data %>% filter(Medium == "Indoor Air")
  expect_equal("ppb", contam_processed_data$Units[1])
  expect_equal("ppb", contam_processed_data$Units[2])
  expect_equal("ppb", contam_processed_data$Units[3])
  expect_equal("ppb", contam_processed_data$Units[4])
})

test_that("Detected flag converted from yes/no to TRUE/FALSE",{
  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx))
  contam_processed_data <- processed_data[[1]]
  expect_equal(8, sum(contam_processed_data$DetectedFlag))
  expect_equal(2, sum(!contam_processed_data$DetectedFlag))
})

#################################### ReferenceAirConcentrations #########################################

test_that("correct error thrown when reference air concentrations sheet is missing a needed column", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: One or more required columns are missing from the ReferenceAirConcentrations sheet of the import file"

  ref_air_conc_dfx <- ref_air_conc_dfx %>% select(-Contaminant)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  ref_air_conc_dfx <- test_template_data[[6]]
  ref_air_conc_dfx <- ref_air_conc_dfx %>% select(-CASRN)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  ref_air_conc_dfx <- test_template_data[[6]]
  ref_air_conc_dfx <- ref_air_conc_dfx %>% select(-ReferenceConcentrationName)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  ref_air_conc_dfx <- test_template_data[[6]]
  ref_air_conc_dfx <- ref_air_conc_dfx %>% select(-Concentration)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)

  ref_air_conc_dfx <- test_template_data[[6]]
  ref_air_conc_dfx <- ref_air_conc_dfx %>% select(-Units)
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when parameter values are missing", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Missing parameter value in the ReferenceAirConcentrations sheet of the import file"
  ref_air_conc_dfx$Concentration <- NA
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when incorrect units occur", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Incorrect units assigned in the ReferenceAirConcentrations sheet of the import file"

  ref_air_conc_dfx$Units <- "NotAcceptedValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when concentration values are not all numeric", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: Non-numeric parameter value assigned in the Concentration field of the ReferenceAirConcentrations sheet of the import file"

  ref_air_conc_dfx$Concentration <- "NonNumericValue"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when more than one contaminant is assigned for the same CASRN in the ReferenceAirConcentrations sheet", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: More than one contaminant name was assigned for the same CASRN in the ReferenceAirConcentrations sheet"
  ref_air_conc_dfx$Contaminant[1] <- "NewContaminantName"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

test_that("correct error thrown when more than one CASRN is assigned for the same contaminant in the ReferenceAirConcentrations sheet", {

  test_template_data <- get_stochastic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  ref_air_conc_dfx <- test_template_data[[6]]

  errorMessage <- "Error: More than one CASRN was assigned for the same contaminant in the ReferenceAirConcentrations sheet"
  ref_air_conc_dfx$CASRN[1] <- "NewContaminantCASRN"
  expect_error(processImportedTemplateData(list (contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, ref_air_conc_dfx)), errorMessage, fixed = TRUE)
})

