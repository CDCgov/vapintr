test_that("HLC calculation outputs the JEM spreadsheet result", {

  calcHsOutput <- calcHs("Benzene", Ts = 23, simulation_type = "DET")

  expect_equal(signif(calcHsOutput[[1]], 7), 0.2086004)
})

test_that("Stochastic HLC calculations match deterministic results", {

  #Run Monte Carlo simulations
  mc_test_data <- get_default_monte_carlo_test_data()

  contaminant_data <- mc_test_data[[1]]
  vadose_zone_data <- mc_test_data[[3]]
  settings_dfx <- mc_test_data[[5]]

  #Set number of Monte Carlo iterations (normally done in runJE, but that isn't called here)
  nIterations <- settings_dfx$number_of_monte_carlo_iterations
  ndunc(nIterations)

  Ts_mc <- Filter(function(x) x$getSymbol() == "Ts", vadose_zone_data)[[1]]$getDistributionData()

  chemical = contaminant_data$getDataFrameOfProperties()$name

  Hs_mcResults <- calcHs(Chem = chemical, Ts = Ts_mc, simulation_type = "MC")[[1]]
  Hs_mcResults <- unmc(Hs_mcResults)

  #Set up deterministic simulation settings
  settings_dfx$simulation_type <- "DET"

  #Build true/false object stating whether each value of Cia in the mcResults equals the equivalent deterministic value
  Hs_detResults <- sapply(seq_len(nIterations), function(mc_i){

    Ts_det <- Ts_mc[mc_i]
    Hs_det <- calcHs(Chem = chemical, Ts = Ts_det, simulation_type = "DET")[[1]]

    return(Hs_det)

  })

  expect_equal(nIterations, sum(Hs_mcResults == Hs_detResults))

})

test_that("Warning message correctly output when HLC conversion to system temperature is not available", {

  warningMessage <- "HLC conversion to system temperature not available for this compound"

  #Checks that a warning message is thrown and that the proper warning message is returned
  expect_warning(expect_equal(calcHs("Hexabromobenzene", Ts = 23, simulation_type = "DET")[[2]], warningMessage, fixed = TRUE), warningMessage, fixed = TRUE)
})

test_that("Error message correctly output when HLC at standard temperature is not available", {

  errorMessage <- "Error: Henry's Law constant at standard temperature is not available for this compound."

  expect_error(calcHs("Boron Trifluoride", Ts = 23, simulation_type = "DET"), errorMessage, fixed = TRUE)
})

