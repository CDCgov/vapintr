# length tests

test_that("Length conversion from ft to m", {

  output_number <- paramUnitConvs(1, "ft", "m")
  expect_equal(output_number, 1/3.28084)
})

test_that("Length conversion from m to ft", {

  output_number <- paramUnitConvs(1, "m", "ft")
  expect_equal(output_number, 3.28084)
})


# area tests

test_that("Area conversion from ft2 to m2", {

  output_number <- paramUnitConvs(1, "ft2", "m2")
  expect_equal(output_number, 1/3.28084/ 3.28084)
})

test_that("Area conversion from m2 to ft2", {

  output_number <- paramUnitConvs(1, "m2", "ft2")
  expect_equal(output_number, 3.28084* 3.28084)
})


# temperature tests

test_that("Temperature conversion from deg C to deg F", {

  output_number <- paramUnitConvs(25, "deg C", "deg F")
  expect_equal(output_number, 25*9/5 + 32)
})

test_that("Temperature conversion from deg F to deg C", {

  output_number <- paramUnitConvs(80, "deg F", "deg C")
  expect_equal(output_number, (80-32)*(5/9))
})


# catch-all test
test_that("Catch-all statement test", {
  errorMessage <- "Error: Unsupported unit conversion applied in paramUnitConvs"
  expect_error(paramUnitConvs(1, "ft", "degC"), errorMessage, fixed = TRUE)
})
