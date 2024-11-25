#################### Tests that results can be calculated ##################

test_that("deterministic results returned for no contaminant specified", {

   template_data <- get_deterministic_test_template_data()
   processed_data <- processImportedTemplateData(template_data)
   jem_results <- runTemplateData(processed_data, capture_warnings_and_errors = TRUE)

   contam_data <- template_data[[1]] %>% filter(
     DetectedFlag == "Yes"
   )

   expect_equal(length(jem_results), nrow(contam_data))
})

test_that("stochastic results returned for no contaminant specified", {

  template_data <- get_stochastic_test_template_data()
  processed_data <- processImportedTemplateData(template_data)
  jem_results <- runTemplateData(processed_data)

  contam_data <- template_data[[1]] %>% filter(
    Medium == processed_data[[5]]$source_medium
  )

  expect_equal(length(jem_results), length(unique(contam_data$CASRN)))

})

test_that("deterministic results when one contaminant is specified", {

  template_data <- get_deterministic_test_template_data()
  processed_data <- processImportedTemplateData(template_data)
  jem_results <- runTemplateData(processed_data, "Tetrachloroethylene", capture_warnings_and_errors = TRUE)

  contam_data <- template_data[[1]] %>% filter(
    DetectedFlag == "Yes",
    Contaminant == "Tetrachloroethylene"
  )

  expect_equal(length(jem_results), nrow(contam_data))

})

test_that("stochastic results when one contaminant is specified", {

  template_data <- get_stochastic_test_template_data()
  processed_data <- processImportedTemplateData(template_data)
  jem_results <- runTemplateData(processed_data, "Tetrachloroethylene")

  contam_data <- template_data[[1]] %>% filter(
    DetectedFlag == "Yes",
    Contaminant == "Tetrachloroethylene"
  )

  expect_equal(length(jem_results), 1)
})

####### Tests that errors and warnings are correctly returned #########

test_that("error returned from getInputConc", {

  template_data <- get_deterministic_test_template_data()
  processed_data <- processImportedTemplateData(template_data)

  errorMessage <- "Error: The imported contaminant data set does not contain records for Benzene"
  expect_error(runTemplateData(processed_data, "Benzene"), errorMessage, fixed = TRUE)
})

test_that("correct warning message thrown for deterministic simulations with multiple logs", {
  template_data <- get_deterministic_test_template_data()
  processed_data <- processImportedTemplateData(template_data)

  warningMessage <- "Warning: More than one unique LogID was loaded in the data import template's StrataLogs sheet. Only the first LogID recorded (IS05) was used in the simulation."
  expect_warning(runTemplateData(processed_data, use_aggregate_data_in_det_simulation = TRUE), warningMessage, fixed = TRUE)
})

test_that("correct warning messages thrown for deterministic simulations with multiple logs and messages", {
  template_data <- get_deterministic_test_template_data()
  processed_data <- processImportedTemplateData(template_data)

  processed_data[[1]]$Contaminant <- "Benzene"
  processed_data[[1]]$CASRN <- "000071-43-2"

  warningMessage <- "Warning: More than one unique LogID was loaded in the data import template's StrataLogs sheet. Only the first LogID recorded (IS05) was used in the simulation."
  expect_warning(runTemplateData(processed_data, use_aggregate_data_in_det_simulation = TRUE), warningMessage, fixed = TRUE)

  warningMessage <- "Warning: Predicted indoor air concentration may be overestimated; biodegredation was not considered for this contaminant. "
  expect_warning(runTemplateData(processed_data, use_aggregate_data_in_det_simulation = TRUE), warningMessage, fixed = TRUE)
})

test_that("correct error message thrown for deterministic simulation", {
  template_data <- get_deterministic_test_template_data()
  processed_data <- processImportedTemplateData(template_data)

  processed_data[[3]]$Ls <- 100

  errorMessage <- "Error: The input depth to source (Ls = 100 m) is deeper than the total depth of the boring log with LogID = IS05 (total depth = 31 m)"
  expect_error(runTemplateData(processed_data, logID = "IS05", use_aggregate_data_in_det_simulation = TRUE), errorMessage, fixed = TRUE)
})

test_that("correct warning message captured for deterministic simulations with multiple logs", {
  template_data <- get_deterministic_test_template_data()
  processed_data <- processImportedTemplateData(template_data)
  jemOutput <- runTemplateData(processed_data, use_aggregate_data_in_det_simulation = TRUE, capture_warnings_and_errors = TRUE)

  warningMessage <- "Warning: More than one unique LogID was loaded in the data import template's StrataLogs sheet. Only the first LogID recorded (IS05) was used in the simulation."
  expect_equal(jemOutput[[1]]$Warning, warningMessage, fixed = TRUE)
})

test_that("correct warning messages captured for deterministic simulations with multiple logs and messages", {
  template_data <- get_deterministic_test_template_data()
  processed_data <- processImportedTemplateData(template_data)

  processed_data[[1]]$Contaminant <- "Benzene"
  processed_data[[1]]$CASRN <- "000071-43-2"

  jemOutput <- runTemplateData(processed_data, use_aggregate_data_in_det_simulation = TRUE, capture_warnings_and_errors = TRUE)


  warningMessage <- "Warning: Predicted indoor air concentration may be overestimated; biodegredation was not considered for this contaminant. \n\n"
  warningMessage <- paste0(warningMessage, "Warning: More than one unique LogID was loaded in the data import template's StrataLogs sheet. Only the first LogID recorded (IS05) was used in the simulation. \n\n")
  expect_equal(jemOutput[[1]]$Warning, warningMessage, fixed = TRUE)

})

test_that("correct error message captured for deterministic simulation", {
  template_data <- get_deterministic_test_template_data()
  processed_data <- processImportedTemplateData(template_data)

  processed_data[[3]]$Ls <- 100
  jemOutput <- runTemplateData(processed_data, use_aggregate_data_in_det_simulation = TRUE, capture_warnings_and_errors = TRUE)

  errorMessage <- "Error: The input depth to source (Ls = 100 m) is deeper than the total depth of the boring log with LogID = IS05 (total depth = 31 m)"
  expect_equal(jemOutput[[1]]$Error, errorMessage, fixed = TRUE)
})

test_that("correct warning messages thrown for stochastic simulation", {
  template_data <- get_stochastic_test_template_data()
  processed_data <- processImportedTemplateData(template_data)

  processed_data[[1]]$Contaminant <- "Benzene"
  processed_data[[1]]$CASRN <- "000071-43-2"

  warningMessage <- "Warning: Predicted indoor air concentration may be overestimated; biodegredation was not considered for this contaminant. "
  expect_warning(runTemplateData(processed_data, use_aggregate_data_in_det_simulation = TRUE), warningMessage, fixed = TRUE)
})

test_that("correct error message thrown for stochastic simulation", {
  template_data <- get_stochastic_test_template_data()
  processed_data <- processImportedTemplateData(template_data)

  processed_data[[3]][[1]] <- JEMParamDist$new("Ls",
                                             name = "Depth below grade to source",
                                             units = "m",
                                             dist_type = "Constant",
                                             constant = 100)

  errorMessage <- "Error: The maximum depth to source (Ls_max = 100 m) is deeper than the total depth of the boring log with LogID = IS05 (total depth = 31 m)"
  expect_error(runTemplateData(processed_data, use_aggregate_data_in_det_simulation = TRUE), errorMessage, fixed = TRUE)
})

test_that("correct warning message captured for stochastic simulation", {
  template_data <- get_stochastic_test_template_data()
  processed_data <- processImportedTemplateData(template_data)

  processed_data[[1]]$Contaminant <- "Benzene"
  processed_data[[1]]$CASRN <- "000071-43-2"

  jemOutput <- runTemplateData(processed_data, use_aggregate_data_in_det_simulation = TRUE, capture_warnings_and_errors = TRUE)

  warningMessage <- "Warning: Predicted indoor air concentration may be overestimated; biodegredation was not considered for this contaminant. \n\n"
  expect_equal(jemOutput[[1]]$Warning, warningMessage, fixed = TRUE)

})

test_that("correct error message captured for stochastic simulation", {
  template_data <- get_stochastic_test_template_data()
  processed_data <- processImportedTemplateData(template_data)

  processed_data[[3]][[1]] <- JEMParamDist$new("Ls",
                                               name = "Depth below grade to source",
                                               units = "m",
                                               dist_type = "Constant",
                                               constant = 100)

  jemOutput <- runTemplateData(processed_data, use_aggregate_data_in_det_simulation = TRUE, capture_warnings_and_errors = TRUE)

  errorMessage <- "Error: The maximum depth to source (Ls_max = 100 m) is deeper than the total depth of the boring log with LogID = IS05 (total depth = 31 m)"
  expect_equal(jemOutput[[1]]$Error, errorMessage, fixed = TRUE)
})
