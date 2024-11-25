test_that("DeffCalc returns spreadsheet result", {

  Dair <- 0.0504664
  Dwater <- 9.4551E-06
  nS <- 0.385
  nwS <- 0.197
  Hs <- 0.72383798

  Deff <- DeffCalc(Dair, Dwater, nS, nwS, Hs)

  expect_equal(Deff, 0.00130364)
})

test_that("CsCalc returns spreadsheet result for groundwater", {

  Cmedium <- 10
  Hs <- 0.72383798
  source_medium <- "Groundwater"

  Cs <- CsCalc(Cmedium, Hs, source_medium)

  expect_equal(Cs, 7238.379804)

})

test_that("CsCalc returns spreadsheet result for other subslab soil gas", {

  Cmedium <- 10
  Hs <- 0.72383798
  source_medium <- "Subslab soil gas"

  Cs <- CsCalc(Cmedium, Hs, source_medium)

  expect_equal(Cs, 10)

})

test_that("CsCalc returns spreadsheet result for exterior soil gas", {

  Cmedium <- 10
  Hs <- 0.72383798
  source_medium <- "Exterior Soil Gas"

  Cs <- CsCalc(Cmedium, Hs, source_medium)

  expect_equal(Cs, 10)

})

test_that("%SatCalc returns spreadsheet result", {

  Cs <- 7238.379804
  Vc <- 165030716.1

  PercSat <- PercSatCalc(Cs, Vc)

  expect_equal(PercSat, 4.38608E-05)

})

test_that("HrCalc returns spreadsheet result", {

  Hc <- 0.0177

  Hr <- HrCalc(Hc)

  expect_equal(Hr, 0.72383798)

})

test_that("QbCalc returns spreadsheet result", {

  Abf <- 150
  Hb <- 2.44
  ach <- 0.45

  Qb <- QbCalc(Abf, Hb, ach)

  expect_equal(Qb, 164.70)

})

test_that("QSoilCalc returns spreadsheet result for a building with a slab-on-grade foundation", {

  Qsoil_Qb <- 0.0030
  Qb <- 164.70
  foundation_type <- "Slab-grade"

  Qsoil <- QsoilCalc(Qsoil_Qb, Qb, foundation_type)

  expect_equal(Qsoil, 0.4941)

})

test_that("QSoilCalc returns spreadsheet result for a building with a basement w/ dirt floor foundation", {

  Qsoil_Qb <- 0.0030
  Qb <- 247.05
  foundation_type <- "Basement-dirt"

  Qsoil <- QsoilCalc(Qsoil_Qb, Qb, foundation_type)

  expect_equal(Qsoil, NA)

})

test_that("QSoilCalc returns spreadsheet result for a building with a crawlspace w/ dirt floor foundation", {

  Qsoil_Qb <- 0.0030
  Qb <- 87.75
  foundation_type <- "Crawlspace-dirt"

  Qsoil <- QsoilCalc(Qsoil_Qb, Qb, foundation_type)

  expect_equal(Qsoil, NA)

})

test_that("AparamCalc returns spreadsheet result", {

  DeffT <- 0.000133929
  Abf <- 150.00
  Lb <- 0.10
  Qb <- 164.70
  Ls <- 9.54

  Aparam <- AparamCalc(DeffT, Abf, Lb, Qb, Ls)

  expect_equal(Aparam, 4.80354E-06)

})

test_that("BparamCalc returns spreadsheet result", {

  Qsoil_Qb <- 0.0030
  Qb <- 164.70
  Lf <- 0.10
  DeffBF <- 0.0013036404751028
  eta <- 0.001
  Abf <- 150.00
  Lb <- 0.10

  Bparam <- BparamCalc(Qsoil_Qb, Qb, Lf, DeffBF, eta, Abf, Lb)

  expect_equal(Bparam, 679.6823006)

})

test_that("CparamCalc returns spreadsheet result", {

  Qsoil_Qb <- 0.0030

  Cparam <- CparamCalc(Qsoil_Qb)

  expect_equal(Cparam, 3.0E-03)

})

test_that("alphaCalc returns spreadsheet result for groundwater and a slab-on-grade foundation", {

  Aparam <- 4.8E-6
  Bparam <- 6.8E2
  Cparam <- 3.0E-3
  Qsoil_Qb <- 0.0030
  foundation_type = "Slab-grade"
  source_medium = "Groundwater"

  alpha <- alphaCalc(Aparam, Bparam, Cparam, Qsoil_Qb, foundation_type, source_medium)

  expect_equal(alpha, 4.79586E-06)

})

test_that("alphaCalc returns spreadsheet result for subslab soil gas and a slab-on-grade foundation", {

  Aparam <- 6.67109E-05
  Bparam <- 679.6823006
  Cparam <- 3.0E-3
  Qsoil_Qb <- 0.0030
  foundation_type = "Slab-grade"
  source_medium = "Subslab Soil Gas"

  alpha <-  alphaCalc(Aparam, Bparam, Cparam, Qsoil_Qb, foundation_type, source_medium)

  expect_equal(alpha, 0.003)

})

test_that("alphaCalc returns spreadsheet result for groundwater and a basement w/ dirt foundation", {

  Aparam <- 5.23494E-06
  Bparam <- 0
  Cparam <- 3.0E-3
  Qsoil_Qb <- 0.0030
  foundation_type = "Basement-dirt"
  source_medium = "Groundwater"

  alpha <- alphaCalc(Aparam, Bparam, Cparam, Qsoil_Qb, foundation_type, source_medium)

  expect_equal(alpha, 5.23491E-06)

})

test_that("alphaCalc returns spreadsheet result for groundwater and a crawlspace w/ dirt foundation", {

  Aparam <- 1.16967E-05
  Bparam <- 0
  Cparam <- 3.0E-3
  Qsoil_Qb <- 0.0030
  foundation_type = "Crawlspace-dirt"
  source_medium = "Groundwater"

  alpha <- alphaCalc(Aparam, Bparam, Cparam, Qsoil_Qb, foundation_type, source_medium)

  expect_equal(alpha, 1.16966E-05)

})

test_that("CiaCalc returns spreadsheet result", {

  Cs <- 7238.379804
  alpha <- 4.79586E-06

  Cia <- CiaCalc(Cs, alpha)

  expect_equal(Cia, 0.034714258)

})

test_that("Cia_ppbCalc returns spreadsheet result", {

  Cs <- 7238.379804
  alpha <- 4.79586E-06
  molecWeight <- 165.83

  Cia_ppb <- Cia_ppbCalc(Cs, alpha, molecWeight)

  expect_equal(Cia_ppb, 0.005120369)

})

test_that("CssCalc returns spreadsheet result for a building with a slab-on-grade foundation", {

  Cia <- 0.034714258
  Qb <- 164.70
  Qsoil <- 0.4941
  foundation_type <- "Slab-grade"

  Css <- CssCalc(Cia, Qb, Qsoil, foundation_type)

  expect_equal(Css, 11.57141928)

})

test_that("CssCalc returns spreadsheet result for a building with a basement w/ dirt foundation", {

  Cia <- 0.037892291
  Qb <- 247.05
  Qsoil <- NA
  foundation_type <- "Basement-dirt"

  Css <- CssCalc(Cia, Qb, Qsoil, foundation_type)

  expect_equal(Css, NA)

})

test_that("CssCalc returns spreadsheet result for a building with a crawlspace w/ dirt foundation", {

  Cia <- 0.084664448
  Qb <- 87.75
  Qsoil <- NA
  foundation_type <- "Crawlspace-dirt"

  Css <- CssCalc(Cia, Qb, Qsoil, foundation_type)

  expect_equal(Css, NA)

})

test_that("Css_ppbCalc returns spreadsheet result for a building with a slab-on-grade foundation", {

  Cia <- 0.034714258
  Qb <- 164.70
  Qsoil <- 0.4941
  molecWeight <- 165.83
  foundation_type <- "Slab-grade"

  Css_ppb <- Css_ppbCalc(Cia, Qb, Qsoil, molecWeight, foundation_type)

  expect_equal(Css_ppb, 1.706789577)

})

test_that("Css_ppbCalc returns spreadsheet result for a building with a basement-dirt foundation", {

  Cia <- 0.037892291
  Qb <- 247.05
  Qsoil <- NA
  molecWeight <- 165.83
  foundation_type <- "Basement-dirt"

  Css_ppb <- Css_ppbCalc(Cia, Qb, Qsoil, molecWeight, foundation_type)

  expect_equal(Css_ppb, NA)

})

test_that("Css_ppbCalc returns spreadsheet result for a building with a crawlspace-dirt foundation", {

  Cia <- 0.084664448
  Qb <- 87.75
  Qsoil <- NA
  molecWeight <- 165.83
  foundation_type <- "Crawlspace-dirt"

  Css_ppb <- Css_ppbCalc(Cia, Qb, Qsoil, molecWeight, foundation_type)

  expect_equal(Css_ppb, NA)

})
