####################### Deterministic Results Match EPA Spreadsheet #######################

test_that("groundwater calculations match EPA spreadsheet", {

   det_test_data <- get_default_deterministic_test_data()

   contam_dfx <- det_test_data[[1]]
   building_dfx <- det_test_data[[2]]
   source_dfx <- det_test_data[[3]]
   stratum_dfx <- det_test_data[[4]]
   settings_dfx <- det_test_data[[5]]

   boring_log_dfx <- filter(stratum_dfx, LogID == 1)

   jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

   expect_equal(jemResults$Cia, 3.782648E-02)
   expect_equal(jemResults$Cia_ppb, 5.579423E-03)
   expect_equal(jemResults$alpha, 5.225822E-06)
})

test_that("exterior soil gas calculations match EPA spreadsheet", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 2)
  source_dfx$Ls <- 5

  settings_dfx$source_medium <- "Exterior Soil Gas"
  contam_dfx$Units <- "ug/m3"

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 5.359929E-03)
  expect_equal(jemResults$Cia_ppb, 7.905919E-04)
  expect_equal(jemResults$alpha, 5.359929E-04)

})

test_that("subslab soil gas calculations match EPA spreadsheet", {

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 3)
  source_dfx$Ls <- 2.5

  settings_dfx$source_medium <- "Subslab Soil Gas"
  contam_dfx$Units <- "ug/m3"

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 3.000000E-02)
  expect_equal(jemResults$Cia_ppb, 4.425014E-03)
  expect_equal(jemResults$alpha, 3.000000E-03)

})

# groundwater tests

test_that("groundwater calculations match EPA spreadsheet for a residential slab-on-grade building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Slab-on-grade"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Groundwater"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.034714258)
  expect_equal(jemResults$Cia_ppb, 0.005120369)
  expect_equal(jemResults$alpha, 4.79586E-06)
})

test_that("groundwater calculations match EPA spreadsheet for a residential basement with slab building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Slab"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Groundwater"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.037826483)
  expect_equal(jemResults$Cia_ppb, 0.005579423)
  expect_equal(jemResults$alpha, 5.22582E-06)
})

test_that("groundwater calculations match EPA spreadsheet for a residential basement with dirt floor building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Dirt Floor"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Groundwater"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.037892291)
  expect_equal(jemResults$Cia_ppb, 0.00558913)
  expect_equal(jemResults$alpha, 5.23491E-06)
})

test_that("groundwater calculations match EPA spreadsheet for a residential closed crawl space with slab building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Closed Crawl Space w/ Slab"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Groundwater"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.084336617)
  expect_equal(jemResults$Cia_ppb, 0.012439689)
  expect_equal(jemResults$alpha, 1.16513E-05)
})

test_that("groundwater calculations match EPA spreadsheet for a residential closed crawl space with dirt floor building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Closed Crawl Space w/ Dirt Floor"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Groundwater"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.084664448)
  expect_equal(jemResults$Cia_ppb, 0.012488044)
  expect_equal(jemResults$alpha, 1.16966E-05)
})

test_that("groundwater calculations match EPA spreadsheet for a commercial basement with slab building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Commercial"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Slab"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Groundwater"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.010117093)
  expect_equal(jemResults$Cia_ppb, 0.001492276)
  expect_equal(jemResults$alpha, 1.3977E-06)
})

test_that("groundwater calculations match EPA spreadsheet for a commercial basement with dirt floor building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Commercial"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Dirt Floor"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Groundwater"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.010121795)
  expect_equal(jemResults$Cia_ppb, 0.001492969)
  expect_equal(jemResults$alpha, 1.39835E-06)
})

test_that("groundwater calculations match EPA spreadsheet for a commercial slab on grade building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Commercial"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Slab-on-grade"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Groundwater"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.008391114)
  expect_equal(jemResults$Cia_ppb, 0.001237693)
  expect_equal(jemResults$alpha, 1.15925E-06)
})

# exterior soil gas tests

test_that("exterior soil gas calculations match EPA spreadsheet for a residential slab-on-grade building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Slab-on-grade"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Exterior Soil Gas"

  contam_dfx$Medium = "Exterior Soil Gas"
  contam_dfx$Units = "ug/m3"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- contam_dfx %>% mutate(Units = "ug/m3")

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.000652597)
  expect_equal(jemResults$Cia_ppb, 9.62584E-05)
  expect_equal(jemResults$alpha, 6.52597E-05)
})

test_that("exterior soil gas calculations match EPA spreadsheet for a residential basement w/ slab building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Slab"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Exterior Soil Gas"

  contam_dfx$Medium = "Exterior Soil Gas"
  contam_dfx$Units = "ug/m3"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- contam_dfx %>% mutate(Units = "ug/m3")

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.000966633)
  expect_equal(jemResults$Cia_ppb, 0.000142579)
  expect_equal(jemResults$alpha, 9.66633E-05)
})

test_that("exterior soil gas calculations match EPA spreadsheet for a residential basement w/ dirt floor building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Dirt Floor"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Exterior Soil Gas"

  contam_dfx$Medium = "Exterior Soil Gas"
  contam_dfx$Units = "ug/m3"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- contam_dfx %>% mutate(Units = "ug/m3")

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.000998716)
  expect_equal(jemResults$Cia_ppb, 0.000147311)
  expect_equal(jemResults$alpha, 9.98716E-05)
})

test_that("exterior soil gas calculations match EPA spreadsheet for a residential closed crawl space with slab building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Closed Crawl Space w/ Slab"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Exterior Soil Gas"

  contam_dfx$Medium = "Exterior Soil Gas"
  contam_dfx$Units = "ug/m3"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- contam_dfx %>% mutate(Units = "ug/m3")

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.001752978)
  expect_equal(jemResults$Cia_ppb, 0.000258565)
  expect_equal(jemResults$alpha, 0.000175298)
})

test_that("exterior soil gas calculations match EPA spreadsheet for a residential closed crawl space with dirt floor building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Closed Crawl Space w/ Dirt Floor"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Exterior Soil Gas"

  contam_dfx$Medium = "Exterior Soil Gas"
  contam_dfx$Units = "ug/m3"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- contam_dfx %>% mutate(Units = "ug/m3")

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.001861419)
  expect_equal(jemResults$Cia_ppb, 0.00027456)
  expect_equal(jemResults$alpha, 0.000186142)
})

test_that("exterior soil gas calculations match EPA spreadsheet for a commercial basement w/ slab building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Commercial"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Slab"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Exterior Soil Gas"

  contam_dfx$Medium = "Exterior Soil Gas"
  contam_dfx$Units = "ug/m3"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- contam_dfx %>% mutate(Units = "ug/m3")

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.000264451)
  expect_equal(jemResults$Cia_ppb, 3.90066E-05)
  expect_equal(jemResults$alpha, 2.64451E-05)
})

test_that("exterior soil gas calculations match EPA spreadsheet for a commercial basement w/ dirt floor building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Commercial"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Dirt Floor"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Exterior Soil Gas"

  contam_dfx$Medium = "Exterior Soil Gas"
  contam_dfx$Units = "ug/m3"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- contam_dfx %>% mutate(Units = "ug/m3")

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.000266796)
  expect_equal(jemResults$Cia_ppb, 3.93525E-05)
  expect_equal(jemResults$alpha, 2.66796E-05)
})

test_that("exterior soil gas calculations match EPA spreadsheet for a commercial slab on grade building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Commercial"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Slab-on-grade"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Exterior Soil Gas"

  contam_dfx$Medium = "Exterior Soil Gas"
  contam_dfx$Units = "ug/m3"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- contam_dfx %>% mutate(Units = "ug/m3")

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.000162467)
  expect_equal(jemResults$Cia_ppb, 2.39639E-05)
  expect_equal(jemResults$alpha, 1.62467E-05)
})

# subslab soil gas tests

test_that("subslab soil gas calculations match EPA spreadsheet for a residential slab-on-grade building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Slab-on-grade"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Subslab Soil Gas"

  contam_dfx$Medium = "Subslab Soil Gas"
  contam_dfx$Units = "ug/m3"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- contam_dfx %>% mutate(Units = "ug/m3")
  source_dfx <- source_dfx %>% mutate(Ls = building_dfx$Lb + 0.1)

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.03)
  expect_equal(jemResults$Cia_ppb, 0.004425014)
  expect_equal(jemResults$alpha, 0.003)
})

test_that("subslab soil gas calculations match EPA spreadsheet for a residential basement w/ slab building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Slab"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Subslab Soil Gas"

  contam_dfx$Medium = "Subslab Soil Gas"
  contam_dfx$Units = "ug/m3"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- contam_dfx %>% mutate(Units = "ug/m3")
  source_dfx <- source_dfx %>% mutate(Ls = building_dfx$Lb + 0.1)

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.03)
  expect_equal(jemResults$Cia_ppb, 0.004425014)
  expect_equal(jemResults$alpha, 0.003)
})

test_that("subslab soil gas calculations match EPA spreadsheet for a residential basement w/ dirt floor building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Dirt Floor"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Subslab Soil Gas"

  contam_dfx$Medium = "Subslab Soil Gas"
  contam_dfx$Units = "ug/m3"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- contam_dfx %>% mutate(Units = "ug/m3")
  source_dfx <- source_dfx %>% mutate(Ls = building_dfx$Lb + 0.1)

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.03)
  expect_equal(jemResults$Cia_ppb, 0.004425014)
  expect_equal(jemResults$alpha, 0.003)
})

test_that("subslab soil gas calculations match EPA spreadsheet for a residential closed crawl space w/ slab building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Closed Crawl Space w/ Slab"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Subslab Soil Gas"

  contam_dfx$Medium = "Subslab Soil Gas"
  contam_dfx$Units = "ug/m3"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- contam_dfx %>% mutate(Units = "ug/m3")
  source_dfx <- source_dfx %>% mutate(Ls = building_dfx$Lb + 0.1)

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.03)
  expect_equal(jemResults$Cia_ppb, 0.004425014)
  expect_equal(jemResults$alpha, 0.003)
})

test_that("subslab soil gas calculations match EPA spreadsheet for a residential closed crawl space w/ dirt floor", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Residential"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Closed Crawl Space w/ Dirt Floor"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Subslab Soil Gas"

  contam_dfx$Medium = "Subslab Soil Gas"
  contam_dfx$Units = "ug/m3"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- contam_dfx %>% mutate(Units = "ug/m3")
  source_dfx <- source_dfx %>% mutate(Ls = building_dfx$Lb + 0.1)

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.03)
  expect_equal(jemResults$Cia_ppb, 0.004425014)
  expect_equal(jemResults$alpha, 0.003)
})

test_that("subslab soil gas calculations match EPA spreadsheet for a commercial basement w/ slab building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Commercial"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Slab"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Subslab Soil Gas"

  contam_dfx$Medium = "Subslab Soil Gas"
  contam_dfx$Units = "ug/m3"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- contam_dfx %>% mutate(Units = "ug/m3")
  source_dfx <- source_dfx %>% mutate(Ls = building_dfx$Lb + 0.1)

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.03)
  expect_equal(jemResults$Cia_ppb, 0.004425014)
  expect_equal(jemResults$alpha, 0.003)
})

test_that("subslab soil gas calculations match EPA spreadsheet for a commercial basement w/ dirt floor building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Commercial"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Basement w/ Dirt Floor"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Subslab Soil Gas"

  contam_dfx$Medium = "Subslab Soil Gas"
  contam_dfx$Units = "ug/m3"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- contam_dfx %>% mutate(Units = "ug/m3")
  source_dfx <- source_dfx %>% mutate(Ls = building_dfx$Lb + 0.1)

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.03)
  expect_equal(jemResults$Cia_ppb, 0.004425014)
  expect_equal(jemResults$alpha, 0.003)
})

test_that("subslab soil gas calculations match EPA spreadsheet for a commercial slab-on-grade building", {

  test_template_data <- get_deterministic_test_template_data()

  contam_dfx <- test_template_data[[1]]
  building_dfx <- test_template_data[[2]]
  source_dfx <- test_template_data[[3]]
  stratum_dfx <- test_template_data[[4]]
  settings_dfx <- test_template_data[[5]]
  reference_dfx <- test_template_data[[6]]

  settings_dfx$Value[settings_dfx$Parameter == "Simulate capillary zone"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Apply default building parameters"] <- "Yes"
  settings_dfx$Value[settings_dfx$Parameter == "Building setting"] = "Commercial"
  settings_dfx$Value[settings_dfx$Parameter == "Foundation type"] = "Slab-on-grade"
  settings_dfx$Value[settings_dfx$Parameter == "Source medium"] = "Subslab Soil Gas"

  contam_dfx$Medium = "Subslab Soil Gas"
  contam_dfx$Units = "ug/m3"

  processed_data <- processImportedTemplateData(list(contam_dfx, building_dfx, source_dfx, stratum_dfx, settings_dfx, reference_dfx))

  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- processed_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- processed_data[[5]]

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  contam_dfx <- contam_dfx %>% mutate(Units = "ug/m3")
  source_dfx <- source_dfx %>% mutate(Ls = building_dfx$Lb + 0.1)

  jemResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemResults$Cia, 0.03)
  expect_equal(jemResults$Cia_ppb, 0.004425014)
  expect_equal(jemResults$alpha, 0.003)
})

####################### Monte Carlo Results Match Deterministic Results ################################################

test_that("groundwater Monte Carlo concentrations match deterministic outputs", {

  #Run Monte Carlo simulations
  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]

  mcResults <- runJE(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, stratum_dfx, settings_dfx)
  mcResults <- unmc(mcResults)

  #Set up deterministic simulation settings
  settings_dfx$simulation_type <- "DET"

  unique_logIDs <- unique(stratum_dfx$LogID)

  mc_iterations <- settings_dfx$number_of_monte_carlo_iterations

  #Build true/false object stating whether each value of Cia in the mcResults equals the equivalent deterministic value
  detResults_Cia <- sapply(seq_len(mc_iterations), function(mc_i){

    det_boring_log_dfx <- stratum_dfx %>% filter(LogID == unique_logIDs[mcResults$sampled_log_index[mc_i]])

    det_contaminant_dfx <- data.frame(
      Contaminant = mc_contaminant_data$getDataFrameOfProperties()$name,
      Cmedium = mcResults$Cmedium[[mc_i]],
      Units = mc_contaminant_data$getDataFrameOfProperties()$units
    )

    det_building_dfx <- data.frame(
      Abf = Filter(function(x) x$getSymbol() == "Abf", mc_building_data)[[1]]$getDataFrameOfProperties()$constant,
      ach = mcResults$ach[[mc_i]],
      eta = mcResults$eta[[mc_i]],
      Hb = mcResults$Hb[[mc_i]],
      Lb = mcResults$Lb[[mc_i]],
      Lf = mcResults$Lf[[mc_i]],
      Qsoil_Qb = Filter(function(x) x$getSymbol() == "Qsoil_Qb", mc_building_data)[[1]]$getDataFrameOfProperties()$constant
    )

    det_source_dfx <- data.frame(
      Ls = mcResults$Ls[[mc_i]],
      Ts = mcResults$Ts[[mc_i]]
    )

    detResults <- runJE(det_contaminant_dfx, det_building_dfx, det_source_dfx, det_boring_log_dfx, settings_dfx)

    return(detResults$Cia)

  })

  expect_equal(mc_iterations, sum(detResults_Cia == mcResults$Cia))

})

test_that("exterior soil gas Monte Carlo concentrations match deterministic outputs", {

  #Run Monte Carlo simulations
  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]

  settings_dfx$source_medium = "Exterior Soil Gas"

  mcResults <- runJE(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, stratum_dfx, settings_dfx)
  mcResults <- unmc(mcResults)

  #Set up deterministic simulation settings
  settings_dfx$simulation_type <- "DET"

  unique_logIDs <- unique(stratum_dfx$LogID)

  mc_iterations <- settings_dfx$number_of_monte_carlo_iterations

  #Build true/false object stating whether each value of Cia in the mcResults equals the equivalent deterministic value
  detResults_Cia <- sapply(seq_len(mc_iterations), function(mc_i){

    det_boring_log_dfx <- stratum_dfx %>% filter(LogID == unique_logIDs[mcResults$sampled_log_index[mc_i]])

    det_contaminant_dfx <- data.frame(
      Contaminant = mc_contaminant_data$getDataFrameOfProperties()$name,
      Cmedium = mcResults$Cmedium[[mc_i]],
      Units = "ug/m3"
    )

    det_building_dfx <- data.frame(
      Abf = Filter(function(x) x$getSymbol() == "Abf", mc_building_data)[[1]]$getDataFrameOfProperties()$constant,
      ach = mcResults$ach[[mc_i]],
      eta = mcResults$eta[[mc_i]],
      Hb = mcResults$Hb[[mc_i]],
      Lb = mcResults$Lb[[mc_i]],
      Lf = mcResults$Lf[[mc_i]],
      Qsoil_Qb = Filter(function(x) x$getSymbol() == "Qsoil_Qb", mc_building_data)[[1]]$getDataFrameOfProperties()$constant
    )

    det_source_dfx <- data.frame(
      Ls = mcResults$Ls[[mc_i]],
      Ts = mcResults$Ts[[mc_i]]
    )

    detResults <- runJE(det_contaminant_dfx, det_building_dfx, det_source_dfx, det_boring_log_dfx, settings_dfx)

    return(detResults$Cia)

  })

  expect_equal(mc_iterations, sum(detResults_Cia == mcResults$Cia))

})

test_that("subslab soil gas Monte Carlo concentrations match deterministic outputs", {

  #Run Monte Carlo simulations
  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]

  settings_dfx$source_medium = "Subslab Soil Gas"

  mc_vadose_zone_data[[1]] <- JEMParamDist$new("Ls",
                                                name = "Depth below grade to source",
                                                units = "m",
                                                dist_type = "PERT",
                                                minimum = 2.3,
                                                mode = 2.5,
                                                maximum = 2.7)

  mcResults <- runJE(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, stratum_dfx, settings_dfx)
  mcResults <- unmc(mcResults)

  #Set up deterministic simulation settings
  settings_dfx$simulation_type <- "DET"

  unique_logIDs <- unique(stratum_dfx$LogID)

  mc_iterations <- settings_dfx$number_of_monte_carlo_iterations

  #Build true/false object stating whether each value of Cia in the mcResults equals the equivalent deterministic value
  detResults_Cia <- sapply(seq_len(mc_iterations), function(mc_i){

    det_boring_log_dfx <- stratum_dfx %>% filter(LogID == unique_logIDs[mcResults$sampled_log_index[mc_i]])

    det_contaminant_dfx <- data.frame(
      Contaminant = mc_contaminant_data$getDataFrameOfProperties()$name,
      Cmedium = mcResults$Cmedium[[mc_i]],
      Units = "ug/m3"
    )

    det_building_dfx <- data.frame(
      Abf = Filter(function(x) x$getSymbol() == "Abf", mc_building_data)[[1]]$getDataFrameOfProperties()$constant,
      ach = mcResults$ach[[mc_i]],
      eta = mcResults$eta[[mc_i]],
      Hb = mcResults$Hb[[mc_i]],
      Lb = mcResults$Lb[[mc_i]],
      Lf = mcResults$Lf[[mc_i]],
      Qsoil_Qb = Filter(function(x) x$getSymbol() == "Qsoil_Qb", mc_building_data)[[1]]$getDataFrameOfProperties()$constant
    )

    det_source_dfx <- data.frame(
      Ls = mcResults$Ls[[mc_i]],
      Ts = mcResults$Ts[[mc_i]]
    )

    detResults <- runJE(det_contaminant_dfx, det_building_dfx, det_source_dfx, det_boring_log_dfx, settings_dfx)

    return(detResults$Cia)

  })

  expect_equal(mc_iterations, sum(detResults_Cia == mcResults$Cia))

})

####################### Validation of BioVapor approach ##############################

test_that("deterministic groundwater simulation with BioVapor correction matches the equivalent exterior soil gas simulation", {

  #Run groundwater test
  det_test_data <- get_default_deterministic_test_data()

  contam_dfx <- det_test_data[[1]]
  building_dfx <- det_test_data[[2]]
  source_dfx <- det_test_data[[3]]
  stratum_dfx <- det_test_data[[4]]
  settings_dfx <- det_test_data[[5]]

  settings_dfx$simulate_capillary_zone <- FALSE

  boring_log_dfx <- filter(stratum_dfx, LogID == 1)

  jemGWResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  #Run Exterior Subslab Soil Gas test
  settings_dfx$simulate_capillary_zone <- FALSE
  settings_dfx$source_medium <- "Exterior Soil Gas"

  #Convert GW concentration to an equivalent exterior soil gas concentration
  #Divide converted concentration by 10 to account for BioVapor correction in GW analysis
  Chem <- contam_dfx$Contaminant
  Cmedium <- contam_dfx$Cmedium
  Ts <- source_dfx$Ts
  calcHsOutput <- calcHs(Chem = Chem, Ts = Ts)
  Hs <- calcHsOutput[[1]]
  contam_dfx$Cmedium <- (Cmedium * 1000 * Hs)/10
  contam_dfx$Units <- "ug/m3"

  jemESGResults <- runJE(contam_dfx, building_dfx, source_dfx, boring_log_dfx, settings_dfx)

  expect_equal(jemGWResults$Cia, jemESGResults$Cia)
  expect_equal(jemGWResults$Cia_ppb, jemESGResults$Cia_ppb)
  expect_equal(jemGWResults$alpha, jemESGResults$alpha)

})

####################### Monte Carlo Results Match Deterministic Results with BioVapor Correction #######################

test_that("groundwater Monte Carlo concentrations match deterministic outputs with biovapor correction", {

  #Run Monte Carlo simulations
  mc_test_data <- get_default_monte_carlo_test_data()

  mc_contaminant_data <- mc_test_data[[1]]
  mc_building_data <- mc_test_data[[2]]
  mc_vadose_zone_data <- mc_test_data[[3]]
  stratum_dfx <- mc_test_data[[4]]
  settings_dfx <- mc_test_data[[5]]

  settings_dfx$simulate_capillary_zone <- FALSE

  mcResults <- runJE(mc_contaminant_data, mc_building_data, mc_vadose_zone_data, stratum_dfx, settings_dfx)
  mcResults <- unmc(mcResults)

  #Set up deterministic simulation settings
  settings_dfx$simulation_type <- "DET"

  unique_logIDs <- unique(stratum_dfx$LogID)

  mc_iterations <- settings_dfx$number_of_monte_carlo_iterations

  #Build true/false object stating whether each value of Cia in the mcResults equals the equivalent deterministic value
  detResults_Cia <- sapply(seq_len(mc_iterations), function(mc_i){

    det_boring_log_dfx <- stratum_dfx %>% filter(LogID == unique_logIDs[mcResults$sampled_log_index[mc_i]])

    det_contaminant_dfx <- data.frame(
      Contaminant = mc_contaminant_data$getDataFrameOfProperties()$name,
      Cmedium = mcResults$Cmedium[[mc_i]],
      Units = mc_contaminant_data$getDataFrameOfProperties()$units
    )

    det_building_dfx <- data.frame(
      Abf = Filter(function(x) x$getSymbol() == "Abf", mc_building_data)[[1]]$getDataFrameOfProperties()$constant,
      ach = mcResults$ach[[mc_i]],
      eta = mcResults$eta[[mc_i]],
      Hb = mcResults$Hb[[mc_i]],
      Lb = mcResults$Lb[[mc_i]],
      Lf = mcResults$Lf[[mc_i]],
      Qsoil_Qb = Filter(function(x) x$getSymbol() == "Qsoil_Qb", mc_building_data)[[1]]$getDataFrameOfProperties()$constant
    )

    det_source_dfx <- data.frame(
      Ls = mcResults$Ls[[mc_i]],
      Ts = mcResults$Ts[[mc_i]]
    )

    detResults <- runJE(det_contaminant_dfx, det_building_dfx, det_source_dfx, det_boring_log_dfx, settings_dfx)

    return(detResults$Cia)

  })

  expect_equal(mc_iterations, sum(detResults_Cia == mcResults$Cia))

})


